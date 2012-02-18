//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2011 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

module internal Microsoft.FSharp.Compiler.Tlr 

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 

open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Tastops.DebugPrint
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.Detuple.GlobalUsageAnalysis
open Microsoft.FSharp.Compiler.Lib


let verboseTLR = false

//-------------------------------------------------------------------------
// library helpers
//-------------------------------------------------------------------------

let internalError str = dprintf "Error: %s\n" str;raise (Failure str)  

module Zmap = 
    let force k   mp (str,soK) = try Zmap.find k mp with e -> dprintf "Zmap.force: %s %s\n" str (soK k); raise e

//-------------------------------------------------------------------------
// misc
//-------------------------------------------------------------------------

/// tree, used to store dec sequence 
type Tree<'T> =
    | TreeNode of Tree<'T> list
    | LeafNode of 'T

let fringeTR tr =
    let rec collect tr acc =
        match tr with
        | TreeNode subts -> List.foldBack collect subts acc
        | LeafNode x     -> x::acc
   
    collect tr []

let emptyTR = TreeNode[]


//-------------------------------------------------------------------------
// misc
//-------------------------------------------------------------------------

/// Collapse reclinks on app and combine apps if possible 
/// recursive ids are inside reclinks and maybe be type instanced with a Expr.App 

// CLEANUP NOTE: mkApps ensures applications are kept in a collapsed 
// and combined form, so this function should not be needed 
let destApp (f,fty,tys,args,m) =
    match stripExpr f with
    | Expr.App (f2,fty2,tys2,[]     ,_) -> (f2,fty2,tys2 @ tys,args,m)
    | Expr.App _                        -> (f,fty,tys,args,m) (* has args, so not combine ty args *)
    | f                                  -> (f,fty,tys,args,m)

#if DEBUG
let showTyparSet tps = showL (commaListL (List.map typarL (Zset.elements tps)))    
#endif

// CLEANUP NOTE: don't like the look of this function - this distinction 
// should never be needed 
let isDelayedRepr (f:Val) e = 
    let _tps,vss,_b,_rty = stripTopLambda (e,f.Type)
    List.length vss>0


let mkLocalNameTypeArity compgen m name ty topValInfo =
    NewVal(name,m,None,ty,Immutable,compgen,topValInfo,taccessPublic,ValNotInRecScope,None,NormalVal,[],OptionalInline,XmlDoc.Empty,false,false,false,false,false,None,ParentNone)

//-------------------------------------------------------------------------
// definitions: TLR, arity, arity-met, arity-short
//
// DEFN: An f is TLR with arity wf if
//         (a) it's repr is "LAM tps. lam x1...xN. body" and have N<=wf (i.e. have enough args)
//         (b) it has no free tps
//         (c) for g:freevars(repr), both
//             (1) g is TLR with arity wg, and
//             (2) g occurs in arity-met occurance.
//         (d) if N=0, then further require that body be a TLR-constant.
//
//   Conditions (a-c) are required if f is to have a static method/field represenation.
//   Condition (d) chooses which constants can be lifted. (no effects, non-trivial).
//
//   DEFN: An arity-met occurance of g is a g application with enough args supplied,
//         ie. (g tps args) where wg <= |args|.
//
//   DEFN: An arity-short occurance does not have enough args.
//
//   DEFN: A TLR-constant:
//         - can have constructors (tuples, datatype, records, exn).
//         - should be non-trivial (says, causes allocation).
//         - if calls are allowed, they must be effect free (since eval point is moving).
//-------------------------------------------------------------------------



//-------------------------------------------------------------------------
// OVERVIEW
// Overview of passes (over term) and steps (not over term):
//
//   pass1 - decide which f will be TLR and determine their arity.
//   pass2 - what closures are needed? Finds reqdTypars(f) and reqdItems(f) for TLR f.
//           Depends on the arity choice, so must follow pass1.
//   step3 - choose env packing, create fHats.
//   pass4 - rewrite term fixing up definitions and callsites.
//           Depends on closure and env packing, so must follow pass2 (and step 3).
//   pass5 - copyExpr call to topexpr to ensure all bound ids are unique.
//           For complexity reasons, better to re-recurse over expr once.
//   pass6 - sanity check, confirm that all TLR marked bindings meet DEFN.
//
//-------------------------------------------------------------------------


//-------------------------------------------------------------------------
// pass1: GetValsBoundUnderMustInline (see comment further below)
//-------------------------------------------------------------------------

let GetValsBoundUnderMustInline xinfo =
    let accRejectFrom (v:Val) repr rejectS =
      if v.InlineInfo = PseudoValue then
        Zset.union (GetValsBoundInExpr repr) rejectS
      else rejectS
    let rejectS = Zset.empty valOrder
    let rejectS = Zmap.fold accRejectFrom xinfo.Defns rejectS
    rejectS

//-------------------------------------------------------------------------
// pass1: IsRefusedTLR
//-------------------------------------------------------------------------
       
let IsRefusedTLR g (f:Val) =  
    let mutableVal = f.IsMutable
    // things marked NeverInline are special 
    let dllImportStubOrOtherNeverInline = (f.InlineInfo = NeverInline)
    // Cannot have static fields of byref type 
    let byrefVal = isByrefLikeTy g f.Type
    // Special values are instance methods etc. on .NET types.  For now leave these alone 
    let specialVal = f.MemberInfo.IsSome
    let alreadyChosen = f.ValReprInfo.IsSome
    let refuseTest = alreadyChosen || mutableVal || byrefVal || specialVal || dllImportStubOrOtherNeverInline
    refuseTest

let IsMandatoryTopLevel (f:Val) = 
    let specialVal = f.MemberInfo.IsSome
    let isModulBinding = f.IsMemberOrModuleBinding
    specialVal || isModulBinding

let IsMandatoryNonTopLevel g (f:Val) =
    let byrefVal = isByrefLikeTy g f.Type
    byrefVal


//-------------------------------------------------------------------------
// pass1: decide which f are to be TLR? and if so, arity(f)
//-------------------------------------------------------------------------

module Pass1_DetermineTLRAndArities = 

    let GetMaxNumArgsAtUses xinfo f =
       match Zmap.tryFind f xinfo.Uses with
       | None       -> 0 (* no call sites *)
       | Some sites -> 
           sites |> List.map (fun (_accessors,_tinst,args) -> List.length args) |> List.max

    let SelectTLRVals g xinfo f e =
        if IsRefusedTLR g f then None 
        // Exclude values bound in a decision tree
        else if Zset.contains f xinfo.DecisionTreeBindings then None
        else
            // Could the binding be TLR? with what arity? 
            let atTopLevel = Zset.contains f xinfo.TopLevelBindings
            let tps,vss,_b,_rty = stripTopLambda (e,f.Type)
            let nFormals    = vss.Length
            let nMaxApplied = GetMaxNumArgsAtUses xinfo f  
            let arity       = Operators.min nFormals nMaxApplied
            if atTopLevel || arity<>0 || nonNil tps then Some (f,arity)
            else None

    /// Check if f involves any value recursion (so can skip those).
    /// ValRec considered: recursive && some f in mutual binding is not bound to a lambda
    let IsValueRecursionFree xinfo f =

        let hasDelayedRepr f = isDelayedRepr f (Zmap.force f xinfo.Defns ("IsValueRecursionFree - hasDelayedRepr",nameOfVal))
        let isRecursive,mudefs = Zmap.force f xinfo.RecursiveBindings ("IsValueRecursionFree",nameOfVal)
        not isRecursive || FlatList.forall hasDelayedRepr mudefs

    let DumpArity arityM =
        let dump f n = dprintf "tlr: arity %50s = %d\n" (showL (valL f)) n
        Zmap.iter dump arityM

    let DetermineTLRAndArities g expr  =
       let xinfo = GetUsageInfoOfImplFile g expr
       let fArities = Zmap.chooseL (SelectTLRVals g xinfo) xinfo.Defns
       let fArities = List.filter (fst >> IsValueRecursionFree xinfo) fArities
       // Do not TLR v if it is bound under a mustinline defn 
       // There is simply no point - the original value will be duplicated and TLR'd anyway 
       let rejectS = GetValsBoundUnderMustInline xinfo
       let fArities = List.filter (fun (v,_) -> not (Zset.contains v rejectS)) fArities
       (*-*)
       let tlrS   = Zset.ofList valOrder (List.map fst fArities)
       let topValS   = xinfo.TopLevelBindings                     (* genuinely top level *)
       let topValS   = Zset.filter (IsMandatoryNonTopLevel g >> not) topValS     (* restrict *)
       (* REPORT MISSED CASES *)
#if DEBUG
       if verboseTLR then 
           let missed = Zset.diff  xinfo.TopLevelBindings tlrS
           missed |> Zset.iter (fun v -> dprintf "TopLevel but not TLR = %s\n" v.LogicalName) 
#endif
       (* REPORT OVER *)   
       let arityM = Zmap.ofList valOrder fArities
#if DEBUG
       if verboseTLR then DumpArity arityM;
#endif
       tlrS,topValS, arityM

     
//-------------------------------------------------------------------------
// pass2: determine reqdTypars(f) and envreq(f) - notes
//-------------------------------------------------------------------------

/// What are the closing types/values for {f1,f2...} mutally defined?
///
//   Note: arity-met g-applications (g TLR) will translated as:
//           [[g @ tps ` args]] -> gHAT @ reqdTypars(g) tps ` env(g) args
//         so they require availability of closing types/values for g.
//
//   If g is free wrt f1,f2... then g's closure must be included.
//
//   Note: mutual definitions have a common closure.
//
//   For f1,f2,... = fBody1,fbody2... mutual bindings:
//
//   DEFN: The reqdVals0 are the free-values of fBody1,fBody2...
//  
//   What are the closure equations?
//
//   reqdTypars(f1,f2..)    includes free-tps(f)
//   reqdTypars(f1,f2..)    includes reqdTypars(g) if fBody has arity-met g-occurance (g TLR).
//
//   reqdItems(f1,f2...) includes ReqdSubEnv(g) if fBody has arity-met   g-occurance (g TLR)
//   reqdItems(f1,f2...) includes ReqdVal(g)    if fBody has arity-short g-occurance (g TLR)
//   reqdItems(f1,f2...) includes ReqdVal(g)    if fBody has g-occurance (g not TLR)
//
//   and only collect requirements if g is a generator (see next notes).
//
//   Note: "env-availability"
//     In the translated code, env(h) will be defined at the h definition point.
//     So, where-ever h could be called (recursive or not),
//     the env(h) will be available (in scope).
//   
//   Note (subtle): "sub-env-requirement-only-for-reqdVals0"
//     If have an arity-met call to h inside fBody, but h is not a freevar for f,
//     then h does not contribute env(h) to env(f), the closure for f.
//     It is true that env(h) will be required at the h call-site,
//     but the env(h) will be available there (by "env-availability"),
//     since h must be bound inside the fBody since h was not a freevar for f.
//     .
//     [note, f and h may mutally recurse and formals of f may be in env(h),
//      so env(f) may be properly inside env(h),
//      so better not have env(h) in env(f)!!!].


/// The subset of ids from a mutal binding that are chosen to be TLR.
/// They share a common env.
/// [Each fclass has an env, the fclass are the handles to envs.]
type BindingGroupSharingSameReqdItems(bindings: Bindings) =
    let vals = valsOfBinds bindings
    let vset = Zset.addFlatList vals (Zset.empty valOrder)

    member fclass.Vals = vals

    member fclass.Contains (v: Val) = vset.Contains v

    member fclass.IsEmpty = FlatList.isEmpty vals

    member fclass.Pairs = vals |> FlatList.map (fun f -> (f,fclass)) 

    override fclass.ToString() = "+" + String.concat "+" (FlatList.map nameOfVal vals)

let fclassOrder = Order.orderOn (fun (b: BindingGroupSharingSameReqdItems) -> b.Vals) (FlatList.order valOrder)

/// It is required to make the TLR closed wrt it's freevars (the env reqdVals0).
/// For gv a generator,
///   An arity-met gv occurance contributes the env required for that gv call.
///   Other occurances contribute the value gv.
type ReqdItem =
    | ReqdSubEnv of Val
    | ReqdVal    of Val
    override i.ToString() = 
      match i with 
      | ReqdSubEnv f -> "&" + f.LogicalName
      | ReqdVal    f -> f.LogicalName


let reqdItemOrder =
    let rep = function
      | ReqdSubEnv v -> true ,v
      | ReqdVal    v -> false,v
   
    Order.orderOn rep (Pair.order (Bool.order,valOrder))

/// An env says what is needed to close the corresponding defn(s).
/// The reqdTypars   are the free reqdTypars of the defns, and those required by any direct TLR arity-met calls.
/// The reqdItems are the ids/subEnvs required from calls to freeVars.
type ReqdItemsForDefn =
    { reqdTypars   : Zset<Typar>;
      reqdItems : Zset<ReqdItem>;
      m      : Range.range; }
    member env.ReqdSubEnvs = [ for x in env.reqdItems do match x with | ReqdSubEnv f -> yield f | ReqdVal _ -> () ]
    member env.ReqdVals = [ for x in env.reqdItems do match x with | ReqdSubEnv _ -> () | ReqdVal v -> yield v ]

    member env.Extend (typars,items) =
        {env with
               reqdTypars   = Zset.addList typars env.reqdTypars;
               reqdItems = Zset.addList items  env.reqdItems}

    static member Initial typars m = 
        {reqdTypars   = Zset.addList typars  (Zset.empty typarOrder);
         reqdItems = Zset.empty reqdItemOrder;
         m      = m }

    override env.ToString() = 
        (showL (commaListL (List.map typarL (Zset.elements env.reqdTypars)))) + "--" +
        (String.concat "," (List.map string (Zset.elements env.reqdItems)))

//-------------------------------------------------------------------------
// pass2: collector - state
//-------------------------------------------------------------------------

type Generators = Zset<Val>

/// check a named function value applied to sufficient arguments 
let IsArityMet (vref:ValRef)  wf (tys: TypeInst) args = 
    (tys.Length = vref.Typars.Length) && (wf <= List.length args) 


module Pass2_DetermineReqdItems = 


    // IMPLEMENTATION PLAN:
    //
    // fold over expr.
    //
    // - at an instance g,
    //   - (a) g arity-met,   LogRequiredFrom g - ReqdSubEnv(g) -- direct call will require env(g) and reqdTypars(g)
    //   - (b) g arity-short, LogRequiredFrom g - ReqdVal(g)    -- remains g call
    //   - (c) g non-TLR,     LogRequiredFrom g - ReqdVal(g)    -- remains g
    //  where
    //   LogRequiredFrom g ... = logs info into (reqdVals0,env) if g in reqdVals0.
    //
    // - at some mu-bindings, f1,f2... = fBody1,fBody2,...
    //  "note reqdVals0, push (reqdVals0,env), fold-over bodies, pop, fold rest"
    //   
    //  - let fclass = ff1,... be the fi which are being made TLR.
    //  - required to find an env for these.
    //  - start a new envCollector:
    //      freetps = freetypars of (fBody1,fBody2,...)
    //      freevs  = freevars   of ..
    //      initialise:
    //        reqdTypars       = freetps
    //        reqdItems     = []      -- info collected from generator occurances in bindings
    //        reqdVals0 = freevs
    //  - fold bodies, collecting info for reqdVals0.
    //  - pop and save env.
    //    - note: - reqdTypars(fclass) are only the freetps
    //            - they need to include reqdTypars(g) for each direct call to g (g a generator for fclass)
    //            - the reqdTypars(g) may not yet be known,
    //              e.g. if we are inside the definition of g and had recursively called it.
    //            - so need to FIX up the reqdTypars(-) function when collected info for all fclass.
    //  - fold rest (after binding)
    //
    // fix up reqdTypars(-) according to direct call dependencies.
    //


    /// This state collects:
    ///   reqdItemsMap          - fclass -> env
    ///   fclassM       - f      -> fclass
    ///   declist       - fclass list
    ///   recShortCallS - the f which are "recursively-called" in arity short instance.
    ///
    /// When walking expr, at each mutual binding site,
    /// push a (generator,env) collector frame on stack.
    /// If occurances in body are relevant (for a generator) then it's contribution is logged.
    ///
    /// recShortCalls to f will require a binding for f in terms of fHat within the fHatBody.
    type state =
        { stack         : (BindingGroupSharingSameReqdItems * Generators * ReqdItemsForDefn) list;
          reqdItemsMap  : Zmap<BindingGroupSharingSameReqdItems,ReqdItemsForDefn>;
          fclassM       : Zmap<Val,BindingGroupSharingSameReqdItems>;
          revDeclist    : BindingGroupSharingSameReqdItems list;
          recShortCallS : Zset<Val>;
        }

    let state0 =
        { stack         = [];
          reqdItemsMap  = Zmap.empty fclassOrder;
          fclassM       = Zmap.empty valOrder;
          revDeclist    = [];
          recShortCallS = Zset.empty valOrder; }

    /// PUSH = start collecting for fclass 
    let PushFrame (fclass: BindingGroupSharingSameReqdItems) (reqdTypars0,reqdVals0,m) state =
        if fclass.IsEmpty then 
            state 
        else
          {state with
               revDeclist = fclass :: state.revDeclist;
               stack = (let env = ReqdItemsForDefn.Initial reqdTypars0 m in (fclass,reqdVals0,env)::state.stack); }

    /// POP & SAVE = end collecting for fclass and store 
    let SaveFrame     (fclass: BindingGroupSharingSameReqdItems) state = 
        if verboseTLR then dprintf "SaveFrame: %A\n" fclass;
        if fclass.IsEmpty then 
            state 
        else
            match state.stack with
            | []                             -> internalError "trl: popFrame has empty stack"
            | (fclass,_reqdVals0,env)::stack -> (* ASSERT: same fclass *)
                {state with
                   stack        = stack;
                   reqdItemsMap = Zmap.add  fclass env   state.reqdItemsMap;
                   fclassM      = FlatList.fold (fun mp (k,v) -> Zmap.add k v mp) state.fclassM fclass.Pairs }

    /// Log requirements for gv in the relevant stack frames 
    let LogRequiredFrom gv items state =
        let logIntoFrame (fclass, reqdVals0:Zset<Val>, env: ReqdItemsForDefn) =
           let env = 
               if reqdVals0.Contains gv then
                   env.Extend ([],items) 
               else env
         
           fclass,reqdVals0,env
       
        {state with stack = List.map logIntoFrame state.stack}

    let LogShortCall gv state =
        if state.stack  |> List.exists (fun (fclass,_reqdVals0,_env) ->  fclass.Contains gv) then
           if verboseTLR then dprintf "shortCall:     rec: %s\n" gv.LogicalName;
           // Have short call to gv within it's (mutual) definition(s) 
           {state with
               recShortCallS = Zset.add gv state.recShortCallS}
        else
          if verboseTLR then dprintf "shortCall: not-rec: %s\n" gv.LogicalName;
          state

    let FreeInBindings bs = FlatList.fold (foldOn (freeInBindingRhs CollectTyparsAndLocals) unionFreeVars) emptyFreeVars bs

    /// Intercepts selected exprs.
    ///   "letrec f1,f2,... = fBody1,fBody2,... in rest" - 
    ///   "val v"                                        - free occurance
    ///   "app (f,tps,args)"                             - occurance
    ///
    /// On intercepted nodes, must exprF fold to collect from subexpressions.
    let ExprEnvIntercept (tlrS,arityM) exprF z expr = 
         let accInstance z (fvref:ValRef,tps,args) (* f known local *) = 
             let f = fvref.Deref
             match Zmap.tryFind f arityM with
             
             | Some wf -> 
                 // f is TLR with arity wf 
                 if IsArityMet fvref wf tps args then
                     // arity-met call to a TLR g 
                     LogRequiredFrom f [ReqdSubEnv f] z                 
                 else
                     // arity-short instance 
                     let z = LogRequiredFrom f [ReqdVal f] z          
                     // LogShortCall - logs recursive short calls 
                     let z = LogShortCall f z                   
                     z
             
             | None    -> 
                 // f is non-TLR 
                 LogRequiredFrom f [ReqdVal f] z                    
        
         let accBinds m z (binds: Bindings) =
             let tlrBs,nonTlrBs = binds |> FlatList.partition (fun b -> Zset.contains b.Var tlrS) 
             // For bindings marked TLR, collect implied env 
             let fclass = BindingGroupSharingSameReqdItems tlrBs
             // what determines env? 
             let frees        = FreeInBindings tlrBs
             let reqdTypars0  = frees.FreeTyvars.FreeTypars   |> Zset.elements      (* put in env *)
             // occurances contribute to env 
             let reqdVals0 = frees.FreeLocals |> Zset.elements
             // tlrBs are not reqdVals0 for themselves 
             let reqdVals0 = reqdVals0 |> List.filter (fun gv -> not (fclass.Contains gv)) 
             let reqdVals0 = reqdVals0 |> Zset.ofList valOrder 
             // collect into env over bodies 
             let z          = PushFrame fclass (reqdTypars0,reqdVals0,m) z
             let z          = (z,tlrBs) ||> FlatList.fold (foldOn (fun b -> b.Expr) exprF) 
             let z          = SaveFrame     fclass z
             (* for bindings not marked TRL, collect *)
             let z          = (z,nonTlrBs) ||> FlatList.fold (foldOn (fun b -> b.Expr) exprF) 
             z
        
         match expr with
         | Expr.Val (v,_,_) -> 
             let z = accInstance z (v,[],[])
             Some z
         | Expr.Op (TOp.LValueOp (_,v),_tys,args,_) -> 
             let z = accInstance z (v,[],[])
             let z = List.fold exprF z args
             Some z
         | Expr.App (f,fty,tys,args,m) -> 
             let f,_fty,tys,args,_m = destApp (f,fty,tys,args,m)
             match f with
             | Expr.Val (f,_,_) ->
                 // YES: APP vspec tps args - log 
                 let z = accInstance z (f,tys,args)
                 let z = List.fold exprF z args
                 Some z
             | _ ->
                 // NO: app, but function is not val - no log
                 None
         | Expr.LetRec (binds,body,m,_) -> 
             let z = accBinds m z binds
             let z = exprF z body
             Some z
         | Expr.Let    (bind,body,m,_) -> 
             let z = accBinds m z (FlatList.one bind)
             let z = exprF z body
             Some z
         | _ -> 
             None 
        

    /// Initially, reqdTypars(fclass) = freetps(bodies).
    /// For each direct call to a gv, a generator for fclass,
    /// Required to include the reqdTypars(gv) in reqdTypars(fclass).
    let CloseReqdTypars fclassM reqdItemsMap =
        if verboseTLR then dprintf "CloseReqdTypars------\n";
       
        let closeStep reqdItemsMap changed fc (env: ReqdItemsForDefn) =
            let directCallReqdEnvs   = env.ReqdSubEnvs
            let directCallReqdTypars = directCallReqdEnvs |> List.map (fun f -> 
                                            let fc  = Zmap.force f  fclassM ("reqdTyparsFor",nameOfVal)
                                            let env = Zmap.force fc reqdItemsMap    ("reqdTyparsFor",string)       
                                            env.reqdTypars)

            let reqdTypars0 = env.reqdTypars
            let reqdTypars  = List.fold Zset.union reqdTypars0 directCallReqdTypars
            let changed = changed || (not (Zset.equal reqdTypars0 reqdTypars))
            let env   = {env with reqdTypars = reqdTypars}
#if DEBUG
            if verboseTLR then 
                dprintf "closeStep: fc=%30A nSubs=%d reqdTypars0=%s reqdTypars=%s\n" fc directCallReqdEnvs.Length (showTyparSet reqdTypars0) (showTyparSet reqdTypars);
                directCallReqdEnvs |> List.iter (fun f    -> dprintf "closeStep: dcall    f=%s\n" f.LogicalName)          
                directCallReqdEnvs |> List.iter (fun f    -> dprintf "closeStep: dcall   fc=%A\n" (Zmap.find f fclassM))
                directCallReqdTypars |> List.iter (fun _reqdTypars -> dprintf "closeStep: dcall reqdTypars=%s\n" (showTyparSet reqdTypars0)) 
#else
            ignore fc
#endif
            changed,env
       
        let rec fixpoint reqdItemsMap =
            let changed = false
            let changed,reqdItemsMap = Zmap.fmap (closeStep reqdItemsMap) changed reqdItemsMap
            if changed then
                fixpoint reqdItemsMap
            else
                reqdItemsMap
       
        fixpoint reqdItemsMap

#if DEBUG
    let DumpReqdValMap reqdItemsMap =
        for KeyValue(fc,env) in reqdItemsMap do 
            dprintf "CLASS=%A\n env=%A\n" fc  env
#endif

    let DetermineReqdItems (tlrS,arityM) expr =
        if verboseTLR then dprintf "DetermineReqdItems------\n";
        let folder = {ExprFolder0 with exprIntercept = ExprEnvIntercept (tlrS,arityM)}
        let z = state0
        // Walk the entire assembly
        let z = FoldImplFile folder z expr
        // project results from the state
        let reqdItemsMap          = z.reqdItemsMap
        let fclassM       = z.fclassM
        let declist       = List.rev z.revDeclist
        let recShortCallS = z.recShortCallS
        // diagnostic dump 
#if DEBUG
        if verboseTLR then DumpReqdValMap reqdItemsMap;
#endif
        // close the reqdTypars under the subEnv reln 
        let reqdItemsMap    = CloseReqdTypars fclassM reqdItemsMap
        // filter out trivial fclass - with no TLR defns 
        let reqdItemsMap    = Zmap.remove (BindingGroupSharingSameReqdItems FlatList.empty) reqdItemsMap
        // restrict declist to those with reqdItemsMap bindings (the non-trivial ones) 
        let declist = List.filter (Zmap.memberOf reqdItemsMap) declist
#if DEBUG
        // diagnostic dump 
        if verboseTLR then
             DumpReqdValMap reqdItemsMap;
             declist |> List.iter (fun fc -> dprintf "Declist: %A\n" fc) 
             recShortCallS |> Zset.iter (fun f -> dprintf "RecShortCall: %s\n" f.LogicalName) 
#endif

        reqdItemsMap,fclassM,declist,recShortCallS 

//-------------------------------------------------------------------------
// step3: PackedReqdItems
//-------------------------------------------------------------------------

/// Each env is represented by some carrier values, the aenvs.
/// An env packing defines these, and the pack/unpack bindings.
/// The bindings are in terms of the fvs directly.
///
/// When defining a new TLR f definition,
///   the fvs   will become bound by the unpack bindings,
///   the aenvs will become bound by the new lam, and
///   the reqdTypars  will become bound by the new LAM.
/// For uniqueness of bound ids,
///   all these ids (Typar/Val) will need to be freshened up.
/// It is OK to break the uniqueness-of-bound-ids rule during the rw,
/// provided it is fixed up via a copyExpr call on the final expr.

type PackedReqdItems =
    { /// The actual typars            
      ep_etps   : Typars; 
      /// The actual env carrier values 
      ep_aenvs  : Val   list; 
      /// Sequentially define the aenvs in terms of the fvs   
      ep_pack   : Bindings;       
      /// Sequentially define the fvs   in terms of the aenvs 
      ep_unpack : Bindings;       
    }


//-------------------------------------------------------------------------
// step3: FlatEnvPacks
//-------------------------------------------------------------------------

exception AbortTLR of Range.range

/// A naive packing of environments.
/// Chooses to pass all env values as explicit args (no tupling).
/// Note, tupling would cause an allocation,
/// so, unless arg lists get very long, this flat packing will be preferable.

/// Given (fclass,env).
/// Have env = ReqdVal vj, ReqdSubEnv subEnvk -- ranging over j,k
/// Define vals(env) = {vj}|j union vals(subEnvk)|k -- trans closure of vals of env.
/// Define <vi,aenvi> for each vi in vals(env).
///  This is the cmap for the env.

///  reqdTypars     = env.reqdTypars
///  carriers = aenvi|i
///  pack     = TBIND(aenvi = vi)            for each (aenvi,vi) in cmap
///  unpack   = TBIND(vj = aenvFor(vj))      for each vj in reqvals(env).
///         and TBIND(asubEnvi = aenvFor(v)) for each (asubEnvi,v) in cmap(subEnvk) ranging over required subEnvk.
/// where
///   aenvFor(v) = aenvi where (v,aenvi) in cmap.
let FlatEnvPacks g fclassM topValS declist (reqdItemsMap: Zmap<BindingGroupSharingSameReqdItems,ReqdItemsForDefn>) =
   let fclassOf f = Zmap.force f fclassM ("fclassM",nameOfVal)
   let packEnv carrierMaps (fc:BindingGroupSharingSameReqdItems) =
       if verboseTLR then dprintf "\ntlr: packEnv fc=%A\n" fc;
       let env = Zmap.force fc reqdItemsMap ("packEnv",string)

       // carrierMaps = (fclass,(v,aenv)map)map 
       let carrierMapFor f = Zmap.force (fclassOf f) carrierMaps ("carrierMapFor",string)
       let valsSubEnvFor f = Zmap.keys (carrierMapFor f)

       // determine vals(env) - transclosure 
       let vals = env.ReqdVals @ List.collect valsSubEnvFor env.ReqdSubEnvs  // list, with repeats 
       let vals = List.noRepeats valOrder vals                        // noRepeats 
       let vals = vals |> FlatList.ofList

       // Remove genuinely toplevel, no need to close over these
       let vals = vals |> FlatList.filter (IsMandatoryTopLevel >> not) 
       // Remove byrefs, no need to close over these, and would be invalid to do so since their values can change.
       //
       // Note that it is normally not OK to skip closing over values, since values given (method) TLR must have imlpementations
       // which are truly closed. However, byref values never escape into any lambdas, so are never used in anything
       // for which we will choose a method TLR. 
       // 
       // For example, consider this (FSharp 1.0 bug 5578):
       //
       //    let mutable a = 1 
       //
       //    let resutl1 = 
       //        let x = &a  // This is NOT given TLR, because it is byref
       //        x <- 111; 
       //        let temp = x // This is given a static field TLR, not a method TLR 
       //        // let f () = x  // This is not allowed, can't capture x
       //        x <- 999; 
       //        temp
       // 
       // Compare with this:
       //    let mutable a = 1 
       //
       //    let result2 = 
       //        let x = a  // this is given static field TLR
       //        a <- 111; 
       //        let temp = a
       //        let f () = x  // This is not allowed, and is given a method TLR
       //        a <- 999; 
       //        temp


       let vals = vals |> FlatList.filter (fun v -> not (isByrefLikeTy g v.Type))
       // Remove values which have been labelled TLR, no need to close over these
       let vals = vals |> FlatList.filter (Zset.memberOf topValS >> not) 
       
       // Carrier sets cannot include constrained polymorphic values. We can't just take such a value out, so for the moment 
       // we'll just abandon TLR altogether and give a warning about this condition. 
       match vals |> FlatList.tryFind (IsGenericValWithGenericContraints g) with 
       | None -> () 
       | Some v -> raise (AbortTLR v.Range);

       // build cmap for env 
       let cmapPairs = vals |> FlatList.map (fun v -> (v,(mkCompGenLocal env.m v.LogicalName v.Type |> fst))) 
       let cmap      = Zmap.ofFlatList valOrder cmapPairs
       let aenvFor     v = Zmap.force v cmap ("aenvFor",nameOfVal)
       let aenvExprFor v = exprForVal env.m (aenvFor v)

       // build PackedReqdItems 
       let reqdTypars   = env.reqdTypars
       let aenvs  = Zmap.values cmap
       let pack   = cmapPairs |> FlatList.map (fun (v,aenv) -> mkInvisibleBind aenv (exprForVal env.m v))
       let unpack = 
           let unpackCarrier (v,aenv) = mkInvisibleBind (setValHasNoArity v) (exprForVal env.m aenv)
           let unpackSubenv f = 
               let subCMap  = carrierMapFor f    
               let vaenvs   = Zmap.toList subCMap
               vaenvs |> List.map (fun (subv,subaenv) -> mkBind NoSequencePointAtInvisibleBinding subaenv (aenvExprFor subv))
           List.map unpackCarrier (Zmap.toList cmap) @
           List.collect unpackSubenv env.ReqdSubEnvs
      
       // extend carrierMaps 
       let carrierMaps = Zmap.add fc cmap carrierMaps

       // dump 
       if verboseTLR then
           dprintf "tlr: packEnv envVals =%s\n" (showL (listL valL  env.ReqdVals));
           dprintf "tlr: packEnv envSubs =%s\n" (showL (listL valL  env.ReqdSubEnvs));
           dprintf "tlr: packEnv vals    =%s\n" (showL (listL valL (FlatList.toList vals)));
           dprintf "tlr: packEnv aenvs   =%s\n" (showL (listL valL aenvs));
           dprintf "tlr: packEnv pack    =%s\n" (showL (listL bindingL  (FlatList.toList pack)));
           dprintf "tlr: packEnv unpack  =%s\n" (showL (listL bindingL  unpack))

       // result 
       carrierMaps,
       (fc, { ep_etps   = Zset.elements reqdTypars;
              ep_aenvs  = aenvs;
              ep_pack   = pack;
              ep_unpack = FlatList.ofList unpack})
  
   let carriedMaps = Zmap.empty fclassOrder
   let _carriedMaps,envPacks = List.fmap packEnv carriedMaps declist   (* List.fmap in dec order *)
   let envPacks = Zmap.ofList fclassOrder envPacks
   envPacks


//-------------------------------------------------------------------------
// step3: chooseEnvPacks
//-------------------------------------------------------------------------

#if DEBUG
let DumpEnvPackM envPackM =
    for KeyValue(fc,packedReqdItems) in envPackM do 
        dprintf "packedReqdItems: fc     = %A\n" fc;
        dprintf "         reqdTypars   = %s\n" (showL (commaListL (List.map typarL packedReqdItems.ep_etps)));
        dprintf "         aenvs  = %s\n" (showL (commaListL (List.map valL packedReqdItems.ep_aenvs)));
        dprintf "         pack   = %s\n" (showL (semiListL (FlatList.toList (FlatList.map bindingL packedReqdItems.ep_pack))));
        dprintf "         unpack = %s\n" (showL (semiListL (FlatList.toList (FlatList.map bindingL packedReqdItems.ep_unpack))));
        dprintf "\n"
#endif

/// For each fclass, have an env.
/// Required to choose an PackedReqdItems,
/// e.g. deciding whether to tuple up the environment or not.
/// e.g. deciding whether to use known values for required sub environments.
///
/// Scope for optimisating env packing here.
/// For now, pass all environments via arguments since aiming to eliminate allocations.
/// Later, package as tuples if arg lists get too long.
let ChooseReqdItemPackings g fclassM topValS  declist reqdItemsMap =
    if verboseTLR then dprintf "ChooseReqdItemPackings------\n";
    let envPackM = FlatEnvPacks g fclassM topValS  declist reqdItemsMap
#if DEBUG
    if verboseTLR then DumpEnvPackM envPackM;
#endif
    envPackM


//-------------------------------------------------------------------------
// step3: CreateNewValuesForTLR
//-------------------------------------------------------------------------

/// arity info where nothing is untupled 
let MakeSimpleArityInfo tps n = ValReprInfo (ValReprInfo.InferTyparInfo tps,List.replicate n ValReprInfo.unnamedTopArg,ValReprInfo.unnamedRetVal)

let CreateNewValuesForTLR g tlrS arityM fclassM envPackM = 
    if verboseTLR then dprintf "CreateNewValuesForTLR------\n";
    let createFHat (f:Val) =
        let wf     = Zmap.force f arityM ("createFHat - wf",(fun v -> showL (valL v)))
        let fc     = Zmap.force f fclassM ("createFHat - fc",nameOfVal)
        let envp   = Zmap.force fc envPackM ("CreateNewValuesForTLR - envp",string)
        let name   = f.LogicalName (* ^ "_TLR_" ^ string wf *)
        let m      = f.Range
        let tps,tau    = f.TypeScheme
        let argtys,res = stripFunTy g tau
        let newTps    = envp.ep_etps @ tps
        let fHatTy = 
            let newArgtys = List.map typeOfVal envp.ep_aenvs @ argtys
            mkLambdaTy newTps newArgtys res
        let fHatArity = MakeSimpleArityInfo newTps (envp.ep_aenvs.Length + wf)
        let fHatName =  globalNng.FreshCompilerGeneratedName(name,m)

        let fHat = mkLocalNameTypeArity f.IsCompilerGenerated m fHatName fHatTy (Some fHatArity)
        fHat
   
    let fs     = Zset.elements tlrS
    let ffHats = List.map (fun f -> f,createFHat f) fs
    let fHatM  = Zmap.ofList valOrder ffHats
    fHatM


//-------------------------------------------------------------------------
// pass4: rewrite - penv
//-------------------------------------------------------------------------

module Pass4_RewriteAssembly =
    [<NoEquality; NoComparison>]
    type RewriteContext =
       { ccu           : CcuThunk;
         g             : Env.TcGlobals;
         tlrS          : Zset<Val> ;
         topValS       : Zset<Val> ;
         arityM        : Zmap<Val,int> ;
         fclassM       : Zmap<Val,BindingGroupSharingSameReqdItems> ;
         recShortCallS : Zset<Val> ;
         envPackM      : Zmap<BindingGroupSharingSameReqdItems,PackedReqdItems>;
         /// The mapping from 'f' values to 'fHat' values
         fHatM         : Zmap<Val,Val> ;
       }


    //-------------------------------------------------------------------------
    // pass4: rwstate (z state)
    //-------------------------------------------------------------------------

    type IsRecursive   = IsRec | NotRec
    type LiftedDeclaration  = IsRecursive * Bindings (* where bool=true if letrec *)    

    /// This state is related to lifting to top-level (which is actually disabled right now)
    /// This is to ensure the TLR constants get initialised once.
    ///
    /// Top-level status ends when stepping inside a lambda, where a lambda is:
    ///   Expr.TyLambda, Expr.Lambda, Expr.Obj (and tmethods).
    ///   [... also, try_catch handlers, and switch targets...]
    ///
    /// Top* repr bindings already at top-level do not need moving...
    ///   [and should not be, since they may lift over unmoved defns on which they depend].
    /// Any TLR repr bindings under lambdas can be filtered out (and collected),
    /// giving pre-declarations to insert before the outermost lambda expr.
    type RewriteState =
        { rws_mustinline: bool;
          /// counts level of enclosing "lambdas"  
          rws_innerLevel : int;        
          /// collected preDecs (fringe is in-order) 
          rws_preDecs    : Tree<LiftedDeclaration>  
        }

    let rewriteState0 = {rws_mustinline=false;rws_innerLevel=0;rws_preDecs=emptyTR}

    // move in/out of lambdas (or lambda containing construct) 
    let EnterInner z = {z with rws_innerLevel = z.rws_innerLevel + 1}
    let ExitInner  z = {z with rws_innerLevel = z.rws_innerLevel - 1}

    let EnterMustInline b z f = 
        let orig = z.rws_mustinline
        let z',x = f (if b then {z with rws_mustinline = true } else z)
        {z' with rws_mustinline = orig },x

    /// extract PreDecs (iff at top-level) 
    let ExtractPreDecs z =
        // If level=0, so at top-level, then pop decs,
        // else keep until get back to a top-level point.
        if z.rws_innerLevel=0 then
          // at top-level, extract preDecs 
          let preDecs = fringeTR z.rws_preDecs
          {z with rws_preDecs=emptyTR}, preDecs
        else 
          // not yet top-level, keep decs 
          z,[]

    /// pop and set preDecs  as "LiftedDeclaration tree" 
    let PopPreDecs z     = {z with rws_preDecs=emptyTR},z.rws_preDecs
    let SetPreDecs z pdt = {z with rws_preDecs=pdt}

    /// collect Top* repr bindings - if needed... 
    let LiftTopBinds _isRec _penv z binds =
        z,binds 
       
    /// Wrap preDecs (in order) over an expr - use letrec/let as approp 
    let MakePreDec  m (isRec,binds) expr = 
        if isRec=IsRec then 
            mkLetRecBinds m binds expr
        else 
            mkLetsFromBindings  m binds expr

    let MakePreDecs m preDecs expr = List.foldBack (MakePreDec m) preDecs expr

    let RecursivePreDecs pdsA pdsB =
        let pds = fringeTR (TreeNode[pdsA;pdsB])
        let decs = pds |> List.collect (fun (_,x) -> FlatList.toList x)  |> FlatList.ofList
        LeafNode (IsRec,decs)


    //-------------------------------------------------------------------------
    // pass4: lowertop - convert_vterm_bind on TopLevel binds
    //-------------------------------------------------------------------------

    let ConvertBind g (TBind(v,repr,_) as bind)  =
        match v.ValReprInfo with 
        | None -> v.SetValReprInfo (Some (InferArityOfExprBinding g v repr ))
        | Some _ -> ()
        
        bind

    //-------------------------------------------------------------------------
    // pass4: transBind (translate)
    //-------------------------------------------------------------------------

    // Transform
    //   let f<tps> vss = f_body[<f_freeTypars>,f_freeVars]
    // To
    //   let f<tps> vss = fHat<f_freeTypars> f_freeVars vss
    //   let fHat<tps> f_freeVars vss = f_body[<f_freeTypars>,f_freeVars]
    let TransTLRBindings penv (binds:Bindings) = 
        if FlatList.isEmpty binds then FlatList.empty,FlatList.empty else
        let fc   = BindingGroupSharingSameReqdItems binds
        let envp = Zmap.force fc penv.envPackM ("TransTLRBindings",string)
        
        let fRebinding (TBind(fOrig,b,letSeqPtOpt)) =
            let m = fOrig.Range
            let tps,vss,_b,rty = stripTopLambda (b,fOrig.Type)
            let aenvExprs = envp.ep_aenvs |> List.map (exprForVal m) 
            let vsExprs   = vss |> List.map (mkTupledVars penv.g m) 
            let fHat      = Zmap.force fOrig penv.fHatM ("fRebinding",nameOfVal) 
            let fOrig = setValHasNoArity fOrig
            let fBind = 
               mkMultiLambdaBind fOrig letSeqPtOpt m tps vss 
                   (mkApps penv.g 
                            ((exprForVal m fHat, fHat.Type),
                             [List.map mkTyparTy (envp.ep_etps @ tps)],
                             aenvExprs @ vsExprs,m),rty)
            fBind                                

        let fHatNewBinding (shortRecBinds:Bindings) (TBind(f,b,letSeqPtOpt)) =
            let wf   = Zmap.force f penv.arityM ("fHatNewBinding - arityM",nameOfVal)
            let fHat = Zmap.force f penv.fHatM  ("fHatNewBinding - fHatM",nameOfVal)
            // Take off the variables
            let tps,vss,b,rty = stripTopLambda (b,f.Type)
            // Don't take all the variables - only up to length wf
            let vssTake,vssDrop = List.chop wf vss
            // put the variables back on
            let b,rty = mkMultiLambdasCore b.Range vssDrop (b,rty)
            // fHat, args 
            let m = fHat.Range
            // Add the type variables to the front
            let fHat_tps  = envp.ep_etps @ tps
            // Add the 'aenv' and original taken variables to the front
            let fHat_args = List.map List.singleton envp.ep_aenvs @ vssTake
            let fHat_body = mkLetsFromBindings m envp.ep_unpack b        
            let fHat_body = mkLetsFromBindings m shortRecBinds  fHat_body  // bind "f" if have short recursive calls (somewhere) 
            // fHat binding, f rebinding 
            let fHatBind   = mkMultiLambdaBind fHat letSeqPtOpt m fHat_tps fHat_args (fHat_body,rty)
            fHatBind
        let rebinds = binds |> FlatList.map fRebinding 
        let shortRecBinds = rebinds |> FlatList.filter (fun b -> penv.recShortCallS.Contains(b.Var)) 
        let newBinds      = binds |> FlatList.map (fHatNewBinding shortRecBinds) 
        newBinds,rebinds

    let GetAEnvBindings penv fc =
        match Zmap.tryFind fc penv.envPackM with
        | None      -> FlatList.empty           // no env for this mutual binding 
        | Some envp -> envp.ep_pack // environment pack bindings 

    let TransBindings xisRec penv (binds:Bindings) =
        let tlrBs,nonTlrBs = binds |> FlatList.partition (fun b -> Zset.contains b.Var penv.tlrS) 
        let fclass = BindingGroupSharingSameReqdItems tlrBs
        // Trans each TLR f binding into fHat and f rebind 
        let newTlrBinds,tlrRebinds = TransTLRBindings penv tlrBs
        let aenvBinds = GetAEnvBindings penv fclass
        // lower nonTlrBs if they are GTL 
        // QUERY: we repeat this logic in Lowertop.  Do we really need to do this here? 
        // QUERY: yes and no - if we don't, we have an unrealizable term, and many decisions must 
        // QUERY: correlate with Lowertop.  
        let forceTopBindToHaveArity (bind:Binding) = 
            if penv.topValS.Contains(bind.Var) then ConvertBind penv.g bind 
            else bind

        let nonTlrBs = nonTlrBs |> FlatList.map forceTopBindToHaveArity 
        let tlrRebinds = tlrRebinds |> FlatList.map forceTopBindToHaveArity 
        // assemble into replacement bindings 
        let bindAs,rebinds = 
            match xisRec with
            | IsRec  -> FlatList.toList newTlrBinds @ FlatList.toList tlrRebinds @ FlatList.toList nonTlrBs @ FlatList.toList aenvBinds,[]    (* note: aenv last, order matters in letrec! *)
            | NotRec -> FlatList.toList aenvBinds @ FlatList.toList newTlrBinds, FlatList.toList tlrRebinds @ FlatList.toList nonTlrBs (* note: aenv go first, they may be used *)
        FlatList.ofList bindAs, FlatList.ofList rebinds


    //-------------------------------------------------------------------------
    // pass4: TransApp (translate)
    //-------------------------------------------------------------------------

    let TransApp penv (fx,fty,tys,args,m) =
        // Is it a val app, where the val f is TLR with arity wf? 
        // CLEANUP NOTE: should be using a mkApps to make all applications 
        match fx with
        | Expr.Val (fvref:ValRef,_,m) when 
                (Zset.contains fvref.Deref penv.tlrS) &&
                (let wf = Zmap.force fvref.Deref penv.arityM ("TransApp - wf",nameOfVal)
                 IsArityMet fvref wf tys args) ->

                   let f = fvref.Deref
                   (* replace by direct call to corresponding fHat (and additional closure args) *)
                   let fc   = Zmap.force f  penv.fclassM ("TransApp - fc",nameOfVal)   
                   let envp = Zmap.force fc penv.envPackM ("TransApp - envp",string)
                   let fHat = Zmap.force f  penv.fHatM ("TransApp - fHat",nameOfVal)
                   let tys  = (List.map mkTyparTy envp.ep_etps) @ tys
                   let aenvExprs = List.map (exprForVal m) envp.ep_aenvs
                   let args = aenvExprs @ args
                   mkApps penv.g ((exprForVal m fHat, fHat.Type),[tys],args,m) (* change, direct fHat call with closure (reqdTypars,aenvs) *)
        | _ -> 
            if isNil tys && isNil args then 
                fx 
            else Expr.App (fx,fty,tys,args,m)
                              (* no change, f is expr *)

    //-------------------------------------------------------------------------
    // pass4: pass (over expr)
    //-------------------------------------------------------------------------

    /// Must WrapPreDecs around every construct that could do EnterInner (which filters TLR decs).
    /// i.e. let,letrec (bind may...), ilobj, lambda, tlambda.
    let WrapPreDecs m pds x =
        MakePreDecs m pds x

    /// At bindings, fixup any TLR bindings.
    /// At applications, fixup calls  if they are arity-met instances of TLR.
    /// At free vals,    fixup 0-call if it is an arity-met constant.
    /// Other cases rewrite structurally.
    let rec TransExpr (penv: RewriteContext) z expr =
        match expr with
        // Use TransLinearExpr with a rebuild-continuation for some forms to avoid stack overflows on large terms *)
        | Expr.LetRec _ | Expr.Let    _ | Expr.Seq _ -> 
             TransLinearExpr penv z expr (fun res -> res)

        // app - call sites may require z.
        //     - match the app (collapsing reclinks and type instances).
        //     - patch it.
        | Expr.App (f,fty,tys,args,m) ->
           // pass over f,args subexprs 
           let z,f      = TransExpr penv z f
           let z,args = List.fmap (TransExpr penv) z args
           // match app, and fixup if needed 
           let f,fty,tys,args,m = destApp (f,fty,tys,args,m)
           let expr = TransApp penv (f,fty,tys,args,m)
           z,expr

        | Expr.Val (v,_,m) ->
           // consider this a trivial app 
           let fx,fty = expr,v.Type
           let expr = TransApp penv (fx,fty,[],[],m)
           z,expr

        // reclink - suppress 
        | Expr.Link r ->
            TransExpr penv z (!r)

        // ilobj - has implicit lambda exprs and recursive/base references 
        | Expr.Obj (_,ty,basev,basecall,overrides,iimpls,m) ->
            let z,basecall  = TransExpr penv                            z basecall 
            let z,overrides = List.fmap (TransMethod penv)                  z overrides
            let z,iimpls    = List.fmap (fmap2Of2 (List.fmap (TransMethod penv))) z iimpls   
            let expr = Expr.Obj(newUnique(),ty,basev,basecall,overrides,iimpls,m)
            let z,pds = ExtractPreDecs z
            z,WrapPreDecs m pds expr (* if TopLevel, lift preDecs over the ilobj expr *)

        // lambda, tlambda - explicit lambda terms 
        | Expr.Lambda(_,ctorThisValOpt,baseValOpt,argvs,body,m,rty) ->
            let z = EnterInner z
            let z,body = TransExpr penv z body
            let z = ExitInner z
            let z,pds = ExtractPreDecs z
            z,WrapPreDecs m pds (rebuildLambda m ctorThisValOpt baseValOpt argvs (body,rty))

        | Expr.TyLambda(_,argtyvs,body,m,rty) ->
            let z = EnterInner z
            let z,body = TransExpr penv z body
            let z = ExitInner z
            let z,pds = ExtractPreDecs z
            z,WrapPreDecs m pds (mkTypeLambda m argtyvs (body,rty))

        /// Lifting TLR out over constructs (disabled)
        /// Lift minimally to ensure the defn is not lifted up and over defns on which it depends (disabled)
        | Expr.Match(spBind,exprm,dtree,targets,m,ty) ->
            let targets = Array.toList targets
            let z,dtree   = TransDecisionTree penv z dtree
            let z,targets = List.fmap (TransDecisionTreeTarget penv) z targets
            // TransDecisionTreeTarget wraps EnterInner/exitInnter, so need to collect any top decs 
            let z,pds = ExtractPreDecs z
            z,WrapPreDecs m pds (mkAndSimplifyMatch spBind exprm m ty dtree targets)

        // all others - below - rewrite structurally - so boiler plate code after this point... 
        | Expr.Const _ -> z,expr (* constant wrt Val *)
        | Expr.Quote (a,{contents=Some(argTypes,argExprs,data)},m,ty) -> 
            let z,argExprs = List.fmap (TransExpr penv) z argExprs
            z,Expr.Quote(a,{contents=Some(argTypes,argExprs,data)},m,ty)
        | Expr.Quote (a,{contents=None},m,ty) -> 
            z,Expr.Quote(a,{contents=None},m,ty)
        | Expr.Op (c,tyargs,args,m) -> 
            let z,args = List.fmap (TransExpr penv) z args
            z,Expr.Op(c,tyargs,args,m)
        | Expr.StaticOptimization (constraints,e2,e3,m) ->
            let z,e2 = TransExpr penv z e2
            let z,e3 = TransExpr penv z e3
            z,Expr.StaticOptimization(constraints,e2,e3,m)
        | Expr.TyChoose (_,_,m) -> 
            error(Error(FSComp.SR.tlrUnexpectedTExpr(),m))

    /// Walk over linear structured terms in tail-recursive loop, using a continuation 
    /// to represent the rebuild-the-term stack 
    and TransLinearExpr penv z expr contf =
        match expr with 
        | Expr.Seq (e1,e2,dir,spSeq,m) -> 
            let z,e1 = TransExpr penv z e1
            TransLinearExpr penv z e2 (contf << (fun (z,e2) ->  
                z,Expr.Seq(e1,e2,dir,spSeq,m)))

         // letrec - pass_recbinds does the work 
         | Expr.LetRec (binds,e,m,_) ->
             let z = EnterInner z
             // For letrec, preDecs from RHS must mutually recurse with those from the bindings 
             let z,pdsPrior    = PopPreDecs z
             let z,binds       = FlatList.fmap (TransBindingRhs penv) z binds
             let z,pdsRhs      = PopPreDecs z
             let binds,rebinds = TransBindings   IsRec penv binds
             let z,binds       = LiftTopBinds IsRec penv z   binds (* factor Top* repr binds *)
             let z,rebinds     = LiftTopBinds IsRec penv z rebinds
             let z,pdsBind     = PopPreDecs z
             let z             = SetPreDecs z (TreeNode [pdsPrior;RecursivePreDecs pdsBind pdsRhs])
             let z = ExitInner z
             let z,pds = ExtractPreDecs z
             TransLinearExpr penv z e (contf << (fun (z,e) -> 
                 let e = mkLetsFromBindings m rebinds e
                 z,WrapPreDecs m pds (Expr.LetRec (binds,e,m,NewFreeVarsCache()))))

         // let - can consider the mu-let bindings as mu-letrec bindings - so like as above 
         | Expr.Let    (bind,e,m,_) ->

             // For let, preDecs from RHS go before those of bindings, which is collection order 
             let z,bind       = TransBindingRhs penv z bind
             let binds,rebinds = TransBindings   NotRec penv (FlatList.one bind)
             // factor Top* repr binds 
             let z,binds       = LiftTopBinds NotRec penv z   binds  
             let z,rebinds     = LiftTopBinds NotRec penv z rebinds
             // any lifted PreDecs from binding, if so wrap them... 
             let z,pds = ExtractPreDecs z
             TransLinearExpr penv z e (contf << (fun (z,e) -> 
                 let e = mkLetsFromBindings m rebinds e
                 z,WrapPreDecs m pds (mkLetsFromBindings m binds e)))

         | _ -> 
            contf (TransExpr penv z expr)
      
    and TransMethod penv z (TObjExprMethod(slotsig,attribs,tps,vs,e,m)) =
        let z = EnterInner z 
        let z,e = TransExpr penv z e
        let z = ExitInner z 
        z,TObjExprMethod(slotsig,attribs,tps,vs,e,m)

    and TransBindingRhs penv z (TBind(v,e,letSeqPtOpt)) = 
        let mustInline = v.MustInline
        let z,e = EnterMustInline mustInline z (fun z -> TransExpr penv z e)
        z,TBind (v,e,letSeqPtOpt)

    and TransDecisionTree penv z x =
       match x with 
       | TDSuccess (es,n) -> 
           let z,es = FlatList.fmap (TransExpr penv) z es
           z,TDSuccess(es,n)
       | TDBind (bind,rest) -> 
           let z,bind       = TransBindingRhs penv z bind
           let z,rest = TransDecisionTree penv z rest
           z,TDBind(bind,rest)
       | TDSwitch (e,cases,dflt,m) ->
           let z,e = TransExpr penv z e
           let TransDecisionTreeCase penv z (TCase (discrim,dtree)) =
               let z,dtree = TransDecisionTree penv z dtree
               z,TCase(discrim,dtree)
          
           let z,cases = List.fmap (TransDecisionTreeCase penv) z cases
           let z,dflt  = Option.fmap (TransDecisionTree penv)      z dflt
           z,TDSwitch (e,cases,dflt,m)

    and TransDecisionTreeTarget penv z (TTarget(vs,e,spTarget)) =
        let z = EnterInner z 
        let z,e = TransExpr penv z e
        let z = ExitInner z
        z,TTarget(vs,e,spTarget)

    and TransValBinding penv z bind = TransBindingRhs penv z bind 
    and TransValBindings penv z binds = FlatList.fmap (TransValBinding penv) z  binds
    and TransModuleExpr penv z x = 
        match x with  
        | ModuleOrNamespaceExprWithSig(mty,def,m) ->  
            let z,def = TransModuleDef penv z def
            z,ModuleOrNamespaceExprWithSig(mty,def,m)
        
    and TransModuleDefs penv z x = List.fmap (TransModuleDef penv) z x
    and TransModuleDef penv (z: RewriteState) x = 
        match x with 
        | TMDefRec(tycons,binds,mbinds,m) -> 
            let z,binds = TransValBindings penv z binds
            let z,mbinds = TransModuleBindings penv z mbinds
            z,TMDefRec(tycons,binds,mbinds,m)
        | TMDefLet(bind,m)            -> 
            let z,bind = TransValBinding penv z bind
            z,TMDefLet(bind,m)
        | TMDefDo(e,m)            -> 
            let z,_bind = TransExpr penv z e
            z,TMDefDo(e,m)
        | TMDefs(defs)   -> 
            let z,defs = TransModuleDefs penv z defs
            z,TMDefs(defs)
        | TMAbstract(mexpr) -> 
            let z,mexpr = TransModuleExpr penv z mexpr
            z,TMAbstract(mexpr)
    and TransModuleBindings penv z binds = List.fmap (TransModuleBinding penv) z  binds
    and TransModuleBinding penv z (ModuleOrNamespaceBinding(nm, rhs)) =
        let z,rhs = TransModuleDef penv z rhs
        z,ModuleOrNamespaceBinding(nm,rhs)

    let TransImplFile penv z mv = fmapTImplFile (TransModuleExpr penv) z mv

    let TransAssembly penv z (TAssembly(mvs)) = 
        let _z,mvs = List.fmap (TransImplFile penv) z mvs 
        TAssembly(mvs)

//-------------------------------------------------------------------------
// pass5: copyExpr
//-------------------------------------------------------------------------

let RecreateUniqueBounds g expr = 
    copyImplFile g OnlyCloneExprVals expr

//-------------------------------------------------------------------------
// entry point
//-------------------------------------------------------------------------

let MakeTLRDecisions ccu g expr =
   try
      // pass1: choose the f to be TLR with arity(f) 
      let tlrS,topValS, arityM = Pass1_DetermineTLRAndArities.DetermineTLRAndArities g expr

      // pass2: determine the typar/freevar closures, f->fclass and fclass declist 
      let reqdItemsMap,fclassM,declist,recShortCallS = Pass2_DetermineReqdItems.DetermineReqdItems (tlrS,arityM) expr

      // pass3
      let envPackM = ChooseReqdItemPackings g fclassM topValS  declist reqdItemsMap
      let fHatM    = CreateNewValuesForTLR g tlrS arityM fclassM envPackM

      // pass4: rewrite 
      if verboseTLR then dprintf "TransExpr(rw)------\n";
      let _,expr = 
          let penv : Pass4_RewriteAssembly.RewriteContext = 
              {ccu=ccu; g=g; tlrS=tlrS; topValS=topValS; arityM=arityM; fclassM=fclassM; recShortCallS=recShortCallS; envPackM=envPackM; fHatM=fHatM}
          let z = Pass4_RewriteAssembly.rewriteState0
          Pass4_RewriteAssembly.TransImplFile penv z expr

      // pass5: copyExpr to restore "each bound is unique" property 
      // aka, copyExpr 
      if verboseTLR then dprintf "copyExpr------\n";
      let expr = RecreateUniqueBounds g expr 
      if verboseTLR then dprintf "TLR-done------\n";

      // DONE 
      expr
   with AbortTLR m -> 
       warning(Error(FSComp.SR.tlrLambdaLiftingOptimizationsNotApplied(),m));
       expr
