//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2012 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------
//-------------------------------------------------------------------------
// The F# expression simplifier. The main aim is to inline simple, known functions
// and constant values, and to eliminate non-side-affecting bindings that 
// are never used.
//------------------------------------------------------------------------- 


module internal Microsoft.FSharp.Compiler.Opt

open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.Internal
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX
open Microsoft.FSharp.Compiler

open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics

open Microsoft.FSharp.Compiler.Pickle
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.PrettyNaming 
open Microsoft.FSharp.Compiler.Tast 
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Tastops.DebugPrint
open Microsoft.FSharp.Compiler.TypeChecker
open Microsoft.FSharp.Compiler.Env
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.Typrelns
open Microsoft.FSharp.Compiler.Infos

open System.Collections.Generic



let verboseOptimizationInfo = 
#if BUILDING_PROTO
    try not (System.String.IsNullOrEmpty (System.Environment.GetEnvironmentVariable "FSHARP_verboseOptimizationInfo"))  with _ -> false
#else
    false
#endif
let verboseOptimizations = 
#if BUILDING_PROTO
    try not (System.String.IsNullOrEmpty (System.Environment.GetEnvironmentVariable "FSHARP_verboseOptimizations")) with _ -> false
#else
    false
#endif

let i_ldlen = [ I_ldlen; (AI_conv DT_I4)  ] 

let callSize = 1  // size of a function call 
let forAndWhileLoopSize = 5 // size of a for/while loop
let tryCatchSize = 5  // size of a try/catch 
let tryFinallySize = 5 // size of a try/finally
let closureTotalSize = 10 // Total cost of a closure. Each closure adds a class definition
let methodDefnTotalSize = 1  // Total cost of a method definition

//-------------------------------------------------------------------------
// Partial information about an expression.
//
// We store one of these for each value in the environment, including values 
// which we know little or nothing about. 
//------------------------------------------------------------------------- 

type TypeValueInfo =
  | UnknownTypeValue

type ExprValueInfo =
  | UnknownValue

  /// SizeValue(size, value)
  /// 
  /// Records size info (maxDepth) for an ExprValueInfo 
  | SizeValue   of int * ExprValueInfo        

  /// ValValue(vref, value)
  ///
  /// Records that a value is equal to another value, along with additional
  /// information.
  | ValValue    of ValRef * ExprValueInfo    

  | TupleValue  of ExprValueInfo array 
  
  /// RecdValue(tycon, values)
  ///
  /// INVARIANT: values are in field definition order .
  | RecdValue   of TyconRef * ExprValueInfo array         

  | UnionCaseValue of UnionCaseRef * ExprValueInfo array 

  | ConstValue of Const * TType

  /// CurriedLambdaValue(id,arity,sz,expr,typ)
  ///    
  ///    arities: The number of bunches of untupled args and type args, and 
  ///             the number of args in each bunch. NOTE: This include type arguments.
  ///    expr: The value, a lambda term.
  ///    typ: The type of lamba term
  | CurriedLambdaValue of  Unique * int * int * Expr * TType

  /// ConstExprValue(size, value)
  | ConstExprValue of int * Expr

type ValInfo =
    { ValMakesNoCriticalTailcalls: bool;
      ValExprInfo: ExprValueInfo }

//-------------------------------------------------------------------------
// Partial information about entire namespace fragments or modules
//
// This is a somewhat nasty data structure since 
//   (a) we need the lookups to be very efficient
//   (b) we need to be able to merge these efficiently while building up the overall data for a module
//   (c) we pickle these to the binary optimization data format
//   (d) we don't want the process of unpickling the data structure to 
//       dereference/resolve all the ValRef's in the data structure, since
//       this would be slow on startup and a potential failure point should
//       any of the destination values not dereference correctly.
//
// It doesn't yet feel like we've got this data structure as good as it could be
//------------------------------------------------------------------------- 


/// Table of the values contained in one module
type ValInfos(entries) =

    let valInfoTable = 
        lazy (let t = ValHash.Create () 
              for (vref:ValRef,x) in entries do 
                   t.Add (vref.Deref,(vref,x))
              t)
    // The compiler ValRef's into fslib stored in env.fs break certain invariants that hold elsewhere, 
    // because they dereference to point to Val's from signatures rather than Val's from implementations.
    // Thus a backup alternative resolution technique is needed for these.
    let valInfosForFslib = 
        lazy (Map.ofList [ for (vref,_x) as p in entries do yield (vref.Deref.LinkagePartialKey,p) ])
    member x.Entries = valInfoTable.Force().Values 
    member x.Map f = new ValInfos(Seq.map f x.Entries)
    member x.Filter f = new ValInfos(Seq.filter f x.Entries)
    member x.TryFind (v:ValRef) = valInfoTable.Force().TryFind v.Deref
    member x.TryFindForFslib (v:ValRef) = valInfosForFslib.Force().TryFind(v.Deref.LinkagePartialKey)

type ModuleInfo = 
    { ValInfos: ValInfos;
      ModuleOrNamespaceInfos: NameMap<LazyModuleInfo> }
          
and LazyModuleInfo = Lazy<ModuleInfo>

let braceL x = leftL "{" ^^ x ^^ rightL "}"  
let seqL xL xs = Seq.fold (fun z x -> z @@ xL x)  emptyL xs
let namemapL xL xmap = NameMap.foldBack (fun nm x z -> xL nm x @@ z)  xmap emptyL

let rec exprValueInfoL g = function
  | ConstValue (x,ty)         -> NicePrint.layoutConst g ty x
  | UnknownValue             -> wordL "?"
  | SizeValue (_,vinfo)      -> exprValueInfoL g vinfo
  | ValValue (vr,vinfo)      -> bracketL ((valRefL vr ^^ wordL "alias") --- exprValueInfoL g vinfo)
  | TupleValue vinfos        -> bracketL (exprValueInfosL g vinfos)
  | RecdValue (_,vinfos)     -> braceL   (exprValueInfosL g vinfos)
  | UnionCaseValue (ucr,vinfos) -> unionCaseRefL ucr ^^ bracketL (exprValueInfosL g vinfos)
  | CurriedLambdaValue(_lambdaId,_arities,_bsize,expr',_ety) -> wordL "lam" ++ exprL expr' (* (sprintf "lam(size=%d)" bsize) *)
  | ConstExprValue (_size,x)  -> exprL x
and exprValueInfosL g vinfos = commaListL (List.map (exprValueInfoL g) (Array.toList vinfos))
and moduleInfoL g (x:LazyModuleInfo) = 
    let x = x.Force()
    braceL ((wordL "Modules: " @@ (x.ModuleOrNamespaceInfos |> namemapL (fun nm x -> wordL nm ^^ moduleInfoL g x) ) )
            @@ (wordL "Values:" @@ (x.ValInfos.Entries |> seqL (fun (vref,x) -> valRefL vref ^^ valInfoL g x) )))

and valInfoL g (x:ValInfo) = 
    braceL ((wordL "ValExprInfo: " @@ exprValueInfoL g x.ValExprInfo) 
            @@ (wordL "ValMakesNoCriticalTailcalls:" @@ wordL (if x.ValMakesNoCriticalTailcalls then "true" else "false")))

type Summary<'Info> =
    { Info: 'Info; 
      /// What's the contribution to the size of this function?
      FunctionSize: int; 
      /// What's the total contribution to the size of the assembly, including closure classes etc.?
      TotalSize: int; 
      /// Meaning: could mutate, could non-terminate, could raise exception 
      /// One use: an effect expr can not be eliminated as dead code (e.g. sequencing)
      /// One use: an effect=false expr can not throw an exception? so try-catch is removed.
      HasEffect: bool  
      /// Indicates that a function may make a useful tailcall, hence when called should itself be tailcalled
      MightMakeCriticalTailcall: bool  
    }

//-------------------------------------------------------------------------
// BoundValueInfoBySize
// Note, this is a different notion of "size" to the one used for inlining heuristics
//------------------------------------------------------------------------- 

let rec SizeOfValueInfos (arr:_[]) = 
    let n = arr.Length
    let rec go i acc = if i >= n then acc else max acc (SizeOfValueInfo arr.[i])
    go 0 0
and SizeOfValueInfo x =
    match x with
    | SizeValue (vdepth,_v)    -> vdepth (* terminate recursion at CACHED size nodes *)
    | ConstValue (_x,_)        -> 1
    | UnknownValue             -> 1
    | ValValue (_vr,vinfo)     -> SizeOfValueInfo vinfo + 1
    | TupleValue vinfos        
    | RecdValue (_,vinfos)
    | UnionCaseValue (_,vinfos)-> 1 + SizeOfValueInfos vinfos
    | CurriedLambdaValue(_lambdaId,_arities,_bsize,_expr',_ety) -> 1
    | ConstExprValue (_size,_) -> 1

let rec MakeValueInfoWithCachedSize vdepth v =
    match v with
      | SizeValue(_,v) -> MakeValueInfoWithCachedSize vdepth v
      | _ -> let minDepthForASizeNode = 5 in (* for small vinfos do not record size info, save space *)
             if vdepth > minDepthForASizeNode then SizeValue(vdepth,v) else v (* add nodes to stop recursion *)
    
let MakeSizedValueInfo v =
    let vdepth = SizeOfValueInfo v
    MakeValueInfoWithCachedSize vdepth v

let BoundValueInfoBySize vinfo =
    let rec bound depth x =
        if depth<0 then UnknownValue else
        match x with
        | SizeValue (vdepth,vinfo) -> if vdepth < depth then x else MakeSizedValueInfo (bound depth vinfo)
        | ValValue (vr,vinfo)      -> ValValue (vr,bound (depth-1) vinfo)
        | TupleValue vinfos        -> TupleValue (Array.map (bound (depth-1)) vinfos)
        | RecdValue (tcref,vinfos) -> RecdValue  (tcref,Array.map (bound (depth-1)) vinfos)
        | UnionCaseValue (ucr,vinfos) -> UnionCaseValue (ucr,Array.map (bound (depth-1)) vinfos)
        | ConstValue _             -> x
        | UnknownValue             -> x
        | CurriedLambdaValue(_lambdaId,_arities,_bsize,_expr',_ety) -> x
        | ConstExprValue (_size,_) -> x
    let maxDepth  = 6   (* beware huge constants! *)
    let trimDepth = 3
    let vdepth = SizeOfValueInfo vinfo
    if vdepth > maxDepth 
    then MakeSizedValueInfo (bound trimDepth vinfo)
    else MakeValueInfoWithCachedSize vdepth vinfo

//-------------------------------------------------------------------------
// What we know about the world 
//------------------------------------------------------------------------- 

let jitOptDefault = true
let localOptDefault = true
let crossModuleOptDefault = true

type OptimizationSettings = 
    { abstractBigTargets : bool;
      jitOptUser : bool option;
      localOptUser : bool option;
      crossModuleOptUser : bool option; 
      /// size after which we start chopping methods in two, though only at match targets 
      bigTargetSize : int   
      /// size after which we start enforcing splitting sub-expressions to new methods, to avoid hitting .NET IL limitations 
      veryBigExprSize : int 
      /// The size after which we don't inline
      lambdaInlineThreshold : int;  
      /// For unit testing
      reportingPhase : bool
      reportNoNeedToTailcall: bool;
      reportFunctionSizes : bool 
      reportHasEffect : bool 
      reportTotalSizes : bool }

    static member Defaults = 
        { abstractBigTargets = false;
          jitOptUser = None;
          localOptUser = None
          /// size after which we start chopping methods in two, though only at match targets 
          bigTargetSize = 100  
          /// size after which we start enforcing splitting sub-expressions to new methods, to avoid hitting .NET IL limitations 
          veryBigExprSize = 3000 
          crossModuleOptUser = None;
          /// The size after which we don't inline
          lambdaInlineThreshold = 6;
          reportingPhase = false;
          reportNoNeedToTailcall = false;
          reportFunctionSizes = false
          reportHasEffect = false
          reportTotalSizes = false
        }

    member x.jitOpt() = (match x.jitOptUser with Some f -> f | None -> jitOptDefault)
    member x.localOpt () = (match x.localOptUser with Some f -> f | None -> localOptDefault)
    member x.crossModuleOpt () = x.localOpt () && (match x.crossModuleOptUser with Some f -> f | None -> crossModuleOptDefault)

    member x.KeepOptimizationValues() = x.crossModuleOpt ()
    /// inline calls *
    member x.InlineLambdas () = x.localOpt ()  
    /// eliminate unused bindings with no effect 
    member x.EliminateUnusedBindings () = x.localOpt () 
    /// eliminate try around expr with no effect 
    member x.EliminateTryCatchAndTryFinally () = x.localOpt () 
    /// eliminate first part of seq if no effect 
    member x.EliminateSequential () = x.localOpt () 
    /// determine branches in pattern matching
    member x.EliminateSwitch () = x.localOpt () 
    member x.EliminateRecdFieldGet () = x.localOpt () 
    member x.EliminateTupleFieldGet () = x.localOpt () 
    member x.EliminatUnionCaseFieldGet () = x.localOpt () 
    /// eliminate non-copiler generated immediate bindings 
    member x.EliminateImmediatelyConsumedLocals() = x.localOpt () 
    /// expand "let x = (exp1,exp2,...)" bind fields as prior tmps 
    member x.ExpandStructrualValues() = x.localOpt () 

#if NO_COMPILER_BACKEND
#else

type cenv =
    { g: Env.TcGlobals;
      TcVal : ConstraintSolver.TcValF
      amap: Import.ImportMap;
      optimizing: bool;
      scope: CcuThunk; 
      localInternalVals: System.Collections.Generic.Dictionary<Stamp,ValInfo> 
      settings: OptimizationSettings;
      emitTailcalls: bool;
      // cache methods with SecurityAttribute applied to them, to prevent unnecessary calls to ExistsInEntireHierarchyOfType
      casApplied : Dictionary<Stamp,bool>;}



type IncrementalOptimizationEnv =
    { // An identifier to help with name generation
      latestBoundId: Ident option;
      // The set of lambda IDs we've inlined to reach this point. Helps to prevent recursive inlining 
      dontInline: Zset<Unique>;  
      // Recursively bound vars. If an sub-expression that is a candidate for method splitting
      // contains any of these variables then don't split it, for fear of mucking up tailcalls.
      // See FSharp 1.0 bug 2892
      dontSplitVars: ValMap<unit>;  
      /// The Val for the function binding being generated, if any. 
      functionVal: (Val * Tast.ValReprInfo) option;
      typarInfos: (Typar * TypeValueInfo) list; 
      localExternalVals: LayeredMap<Stamp,ValInfo>;
      globalModuleInfos: LayeredMap<string,LazyModuleInfo>;   }

    static member Empty = 
        { latestBoundId = None; 
          dontInline = Zset.empty Int64.order;
          typarInfos = [];
          functionVal = None; 
          dontSplitVars = ValMap.Empty;
          localExternalVals = LayeredMap.Empty; 
          globalModuleInfos = LayeredMap.Empty }

//-------------------------------------------------------------------------
// IsPartialExprVal - is the expr fully known?
//------------------------------------------------------------------------- 

let rec IsPartialExprVal x = (* IsPartialExprVal can not rebuild to an expr *)
    match x with
    | UnknownValue -> true
    | TupleValue args | RecdValue (_,args) | UnionCaseValue (_,args) -> Array.exists IsPartialExprVal args
    | ConstValue _ | CurriedLambdaValue _ | ConstExprValue _ -> false
    | ValValue (_,a) 
    | SizeValue(_,a) -> IsPartialExprVal a

let rec IsPartialModuleOrNamespaceVal (ss:ModuleInfo) =
    (ss.ModuleOrNamespaceInfos  |> Map.exists (fun _ x -> IsPartialModuleOrNamespaceVal (x.Force()))) ||
    (ss.ValInfos.Entries |> Seq.exists (fun (_,x) -> IsPartialExprVal x.ValExprInfo)) 

let CheckInlineValueIsComplete (v:Val) res =
    if v.MustInline && IsPartialExprVal res then
        errorR(Error(FSComp.SR.optValueMarkedInlineButIncomplete(v.DisplayName), v.Range))
        //System.Diagnostics.Debug.Assert(false,sprintf "Break for incomplete inline value %s" v.DisplayName)

let check (vref: ValRef) (res:ValInfo)  =
    CheckInlineValueIsComplete vref.Deref res.ValExprInfo;
    (vref,res)

//-------------------------------------------------------------------------
// Bind information about values 
//------------------------------------------------------------------------- 

let EmptyModuleInfo = notlazy { ValInfos = ValInfos([]); ModuleOrNamespaceInfos = Map.empty }

let rec UnionModuleInfos (minfos : seq<LazyModuleInfo>) = 
    notlazy
       { ValInfos =  ValInfos(seq { for minfo in minfos do yield! minfo.Force().ValInfos.Entries })
         ModuleOrNamespaceInfos = minfos |> Seq.map (fun m -> m.Force().ModuleOrNamespaceInfos) |> NameMap.union UnionModuleInfos  }

let FindOrCreateModuleInfo n (ss: Map<_,_>) = 
    match ss.TryFind n with 
    | Some res -> res
    | None -> EmptyModuleInfo

let FindOrCreateGlobalModuleInfo n (ss: LayeredMap<_,_>) = 
    match ss.TryFind n with 
    | Some res -> res
    | None -> EmptyModuleInfo

let rec BindValueInSubModuleFSharpCore (mp:string[]) i (v:Val) vval ss =
    if i < mp.Length  then 
        {ss with ModuleOrNamespaceInfos = BindValueInModuleForFslib mp.[i] mp (i+1) v vval ss.ModuleOrNamespaceInfos }
    else 
        // REVIEW: this line looks quadratic for performance when compiling FSharp.Core
        {ss with ValInfos = ValInfos(Seq.append ss.ValInfos.Entries (Seq.singleton (mkLocalValRef v,vval))) }

and BindValueInModuleForFslib n mp i v vval (ss: NameMap<_>) =
    let old =  FindOrCreateModuleInfo n ss
    Map.add n (notlazy (BindValueInSubModuleFSharpCore mp i v vval (old.Force()))) ss

and BindValueInGlobalModuleForFslib n mp i v vval (ss: LayeredMap<_,_>) =
    let old =  FindOrCreateGlobalModuleInfo n ss
    ss.Add(n, notlazy (BindValueInSubModuleFSharpCore mp i v vval (old.Force())))

let BindValueForFslib (nlvref : NonLocalValOrMemberRef) v vval env =
    {env with globalModuleInfos = BindValueInGlobalModuleForFslib nlvref.AssemblyName nlvref.EnclosingEntity.nlr.Path 0 v vval env.globalModuleInfos }

let UnknownValInfo = { ValExprInfo=UnknownValue; ValMakesNoCriticalTailcalls=false }

let mkValInfo info (v:Val)  = { ValExprInfo=info.Info; ValMakesNoCriticalTailcalls= v.MakesNoCriticalTailcalls }

(* Bind a value *)
let BindInternalLocalVal cenv (v:Val) vval env = 
    let vval = if v.IsMutable then UnknownValInfo else vval
#if CHECKED
#else
    match vval.ValExprInfo with 
    | UnknownValue -> env
    | _ -> 
#endif
        cenv.localInternalVals.[v.Stamp] <- vval;
        env
        
let BindExternalLocalVal cenv (v:Val) vval env = 
#if CHECKED
    CheckInlineValueIsComplete v vval;
#endif

    if verboseOptimizationInfo then dprintn ("*** Binding "^v.LogicalName); 
    let vval = if v.IsMutable then {vval with ValExprInfo=UnknownValue } else vval
    let env = 
#if CHECKED
#else
        match vval.ValExprInfo with 
        | UnknownValue -> env  
        | _ -> 
#endif
            { env with localExternalVals=env.localExternalVals.Add (v.Stamp, vval)  }
    // If we're compiling fslib then also bind the value as a non-local path to 
    // allow us to resolve the compiler-non-local-references that arise from env.fs
    //
    // Do this by generating a fake "looking from the outside in" non-local value reference for
    // v, dereferencing it to find the corresponding signature Val, and adding an entry for the signature val.
    //
    // A similar code path exists in ilxgen.fs for the tables of "representations" for values
    let env = 
        if cenv.g.compilingFslib then 
            // Passing an empty remap is sufficient for FSharp.Core.dll because it turns out the remapped type signature can
            // still be resolved.
            match tryRescopeVal cenv.g.fslibCcu Remap.Empty v with 
            | Some vref -> BindValueForFslib vref.nlr v vval env 
            | None -> env
        else env
    env

let rec BindValsInModuleOrNamespace cenv (mval:LazyModuleInfo) env =
    let mval = mval.Force()
    // do all the sub modules
    let env = (mval.ModuleOrNamespaceInfos,env) ||> NameMap.foldBackRange  (BindValsInModuleOrNamespace cenv) 
    let env = (env, mval.ValInfos.Entries) ||> Seq.fold (fun env (v:ValRef, vval) -> BindExternalLocalVal cenv v.Deref vval env) 
    env

let BindInternalValToUnknown cenv v env = 
#if CHECKED
    BindInternalLocalVal cenv v UnknownValue env
#else
    ignore (cenv,v)
    env
#endif
let BindInternalValsToUnknown cenv vs env = 
#if CHECKED
    List.foldBack (BindInternalValToUnknown cenv) vs env
#else
    ignore (cenv,vs)
    env
#endif

let BindTypeVar tyv typeinfo env = { env with typarInfos= (tyv,typeinfo)::env.typarInfos } 

let BindTypeVarsToUnknown (tps:Typar list) env = 
    if isNil tps then env else
    // The optimizer doesn't use the type values it could track. 
    // However here we mutate to provide better names for generalized type parameters 
    // The names chosen are 'a', 'b' etc. These are also the compiled names in the IL code
    let nms = PrettyTypes.PrettyTyparNames (fun _ -> true) (env.typarInfos |> List.map (fun (tp,_) -> tp.Name) ) tps
    (tps,nms) ||> List.iter2 (fun tp nm -> 
            if PrettyTypes.NeedsPrettyTyparName tp  then 
                tp.Data.typar_id <- ident (nm,tp.Range));      
    List.fold (fun sofar arg -> BindTypeVar arg UnknownTypeValue sofar) env tps 

let BindCcu (ccu:Tast.CcuThunk) mval env cenv = 
    if verboseOptimizationInfo then 
        dprintf "*** Reloading optimization data for assembly %s, info = \n%s\n" ccu.AssemblyName (showL (Layout.squashTo 192 (moduleInfoL cenv mval)));  

    { env with globalModuleInfos=env.globalModuleInfos.Add(ccu.AssemblyName,mval)  }


    
//-------------------------------------------------------------------------
// Lookup information about values 
//------------------------------------------------------------------------- 


let GetInfoForLocalValue cenv env (v:Val) m = 
    (* Abstract slots do not have values *)
    if v.IsDispatchSlot then UnknownValInfo 
    else
        let mutable res = Unchecked.defaultof<_> 
        let ok = cenv.localInternalVals.TryGetValue(v.Stamp, &res)
        if ok then res else
        match env.localExternalVals.TryFind v.Stamp with 
        | Some vval -> vval
        | None -> 
            if v.MustInline then
                errorR(Error(FSComp.SR.optValueMarkedInlineButWasNotBoundInTheOptEnv(fullDisplayTextOfValRef (mkLocalValRef v)), m));
#if CHECKED
            warning(Error(FSComp.SR.optLocalValueNotFoundDuringOptimization(v.DisplayName),m)); 
#endif
            UnknownValInfo 

let TryGetInfoForCcu env (ccu:CcuThunk) = env.globalModuleInfos.TryFind(ccu.AssemblyName)

let TryGetInfoForEntity sv n = 
    match sv.ModuleOrNamespaceInfos.TryFind n with 
    | Some info -> Some (info.Force())
    | None -> 
        if verboseOptimizationInfo then 
            dprintn ("\n\n*** Optimization info for submodule "^n^" not found in parent module which contains submodules: "^String.concat "," (NameMap.domainL sv.ModuleOrNamespaceInfos)); 
        None

let rec TryGetInfoForPath sv (p:_[]) i = 
    if i >= p.Length then Some sv else 
    match TryGetInfoForEntity sv p.[i] with 
    | Some info -> TryGetInfoForPath info p (i+1)
    | None -> None

let TryGetInfoForNonLocalEntityRef env (nleref: NonLocalEntityRef) = 
    match TryGetInfoForCcu env nleref.Ccu with 
    | Some ccuinfo -> TryGetInfoForPath (ccuinfo.Force()) nleref.Path 0
    | None -> None
              
let GetInfoForNonLocalVal cenv env (vref:ValRef) =
    if vref.IsDispatchSlot then 
        UnknownValInfo
    // REVIEW: optionally turn x-module on/off on per-module basis  or  
    elif  cenv.settings.crossModuleOpt () || vref.MustInline then 
        match TryGetInfoForNonLocalEntityRef env vref.nlr.EnclosingEntity.nlr with
        | Some(structInfo) ->
            match structInfo.ValInfos.TryFind(vref) with 
            | Some ninfo -> snd ninfo
            | None -> 
                  //dprintn ("\n\n*** Optimization info for value "^n^" from module "^(full_name_of_nlpath smv)^" not found, module contains values: "^String.concat "," (NameMap.domainL structInfo.ValInfos));  
                  //System.Diagnostics.Debug.Assert(false,sprintf "Break for module %s, value %s" (full_name_of_nlpath smv) n)
                  if cenv.g.compilingFslib then 
                      match structInfo.ValInfos.TryFindForFslib(vref) with 
                      | Some ninfo -> snd ninfo
                      | None ->  
                          UnknownValInfo
                  else
                      UnknownValInfo
        | None -> 
            //dprintf "\n\n*** Optimization info for module %s from ccu %s not found." (full_name_of_nlpath smv) (ccu_of_nlpath smv).AssemblyName;  
            //System.Diagnostics.Debug.Assert(false,sprintf "Break for module %s, ccu %s" (full_name_of_nlpath smv) (ccu_of_nlpath smv).AssemblyName)
            UnknownValInfo
    else 
        UnknownValInfo

let GetInfoForVal cenv env m (vref:ValRef) =  
    let res = 
        match vref.IsLocalRef with 
        | true -> GetInfoForLocalValue cenv env vref.binding m
        | false -> GetInfoForNonLocalVal cenv env vref
    check (* "its stored value was incomplete" m *) vref res |> ignore;
    res

//-------------------------------------------------------------------------
// Try to get information about values of particular types
//------------------------------------------------------------------------- 

let rec stripValue = function
  | ValValue(_,details) -> stripValue details (* step through ValValue "aliases" *) 
  | SizeValue(_,details) -> stripValue details (* step through SizeValue "aliases" *) 
  | vinfo               -> vinfo

let (|StripConstValue|_|) ev = 
  match stripValue ev with
  | ConstValue(c,_) -> Some c
  | _ -> None

let (|StripLambdaValue|_|) ev = 
  match stripValue ev with 
  | CurriedLambdaValue (id,arity,sz,expr,typ) -> Some (id,arity,sz,expr,typ)
  | _ -> None

let destTupleValue ev = 
  match stripValue ev with 
  | TupleValue info -> Some info
  | _ -> None

let destRecdValue ev = 
  match stripValue ev with 
  | RecdValue (_tcref,info) -> Some info
  | _ -> None

let (|StripUnionCaseValue|_|) ev = 
  match stripValue ev with 
  | UnionCaseValue (c,info) -> Some (c,info)
  | _ -> None

let mkBoolVal g n = ConstValue(Const.Bool n, g.bool_ty)
let mkInt8Val g n = ConstValue(Const.SByte n, g.sbyte_ty)
let mkInt16Val g n = ConstValue(Const.Int16 n, g.int16_ty)
let mkInt32Val g n = ConstValue(Const.Int32 n, g.int32_ty)
let mkInt64Val g n = ConstValue(Const.Int64 n, g.int64_ty)
let mkUInt8Val g n = ConstValue(Const.Byte n, g.byte_ty)
let mkUInt16Val g n = ConstValue(Const.UInt16 n, g.uint16_ty)
let mkUInt32Val g n = ConstValue(Const.UInt32 n, g.uint32_ty)
let mkUInt64Val g n = ConstValue(Const.UInt64 n, g.uint64_ty)

let (|StripInt32Value|_|) = function StripConstValue(Const.Int32 n) -> Some n | _ -> None
      
//-------------------------------------------------------------------------
// mk value_infos
//------------------------------------------------------------------------- 

let MakeValueInfoForValue g m vref vinfo            = 
    let rec check x = 
        match x with 
        | ValValue (vref2,detail)  -> if valRefEq g vref vref2 then error(Error(FSComp.SR.optRecursiveValValue(showL(exprValueInfoL g vinfo)),m)) else check detail
        | SizeValue (_n,detail) -> check detail
        | _ -> ()
    check vinfo;
    ValValue (vref,vinfo)       |> BoundValueInfoBySize

let MakeValueInfoForRecord tcref argvals        = RecdValue (tcref,argvals)   |> BoundValueInfoBySize
let MakeValueInfoForTuple argvals               = TupleValue argvals          |> BoundValueInfoBySize
let MakeValueInfoForUnionCase cspec argvals     = UnionCaseValue (cspec,argvals) |> BoundValueInfoBySize
let MakeValueInfoForConst c ty                  = ConstValue(c,ty)

// Helper to evaluate a unary integer operation over known values
let inline IntegerUnaryOp g f8 f16 f32 f64 fu8 fu16 fu32 fu64 a = 
     match a with
     | StripConstValue(c) -> 
         match c with 
         | Const.Bool  a -> Some(mkBoolVal g (f32 (if a then 1 else 0) <> 0))
         | Const.Int32  a -> Some(mkInt32Val g (f32 a))
         | Const.Int64  a -> Some(mkInt64Val g (f64 a))
         | Const.Int16  a -> Some(mkInt16Val g (f16 a))
         | Const.SByte   a  -> Some(mkInt8Val g (f8 a))
         | Const.Byte  a  -> Some(mkUInt8Val g (fu8 a))
         | Const.UInt32 a -> Some(mkUInt32Val g (fu32 a))
         | Const.UInt64 a -> Some(mkUInt64Val g (fu64 a))
         | Const.UInt16 a -> Some(mkUInt16Val g (fu16 a))
         | _ -> None
     | _ -> None

// Helper to evaluate a unary signed integer operation over known values
let inline SignedIntegerUnaryOp g f8 f16 f32 f64 a = 
     match a with
     | StripConstValue(c) -> 
         match c with 
         | Const.Int32 a -> Some(mkInt32Val g (f32 a))
         | Const.Int64 a -> Some(mkInt64Val g (f64 a))
         | Const.Int16 a -> Some(mkInt16Val g (f16 a))
         | Const.SByte  a -> Some(mkInt8Val g (f8 a))
         | _ -> None
     | _ -> None
         
// Helper to evaluate a binary integer operation over known values
let inline IntegerBinaryOp g f8 f16 f32 f64 fu8 fu16 fu32 fu64 a b = 
     match a,b with
     | StripConstValue(c1),StripConstValue(c2) -> 
         match c1,c2 with 
         | (Const.Bool a),(Const.Bool  b) -> Some(mkBoolVal  g (f32  (if a then 1 else 0) (if b then 1 else 0) <> 0))
         | (Const.Int32  a),(Const.Int32  b) -> Some(mkInt32Val  g (f32  a b))
         | (Const.Int64  a),(Const.Int64  b) -> Some(mkInt64Val  g (f64  a b))
         | (Const.Int16  a),(Const.Int16  b) -> Some(mkInt16Val  g (f16  a b))
         | (Const.SByte   a),(Const.SByte   b) -> Some(mkInt8Val   g (f8   a b))
         | (Const.Byte  a),(Const.Byte  b) -> Some(mkUInt8Val  g (fu8  a b))
         | (Const.UInt16 a),(Const.UInt16 b) -> Some(mkUInt16Val g (fu16 a b))
         | (Const.UInt32 a),(Const.UInt32 b) -> Some(mkUInt32Val g (fu32 a b))
         | (Const.UInt64 a),(Const.UInt64 b) -> Some(mkUInt64Val g (fu64 a b))
         | _ -> None
     | _ -> None

module Unchecked = Microsoft.FSharp.Core.Operators
         
/// Evaluate primitives based on interpretation of IL instructions. 
//
// The implementation
// utilizes F# arithmetic extensively, so a mistake in the implementation of F# arithmetic  
// in the core library used by the F# compiler will propagate to be a mistake in optimization. 
// The IL instructions appear in the tree through inlining.
let mkAssemblyCodeValueInfo g instrs argvals tys =
  match instrs,argvals,tys with
    | [ AI_add ],[t1;t2],_ -> 
         // Note: each use of Unchecked.(+) gets instantiated at a different type and inlined
         match IntegerBinaryOp g Unchecked.(+) Unchecked.(+) Unchecked.(+) Unchecked.(+) Unchecked.(+) Unchecked.(+) Unchecked.(+) Unchecked.(+) t1 t2 with 
         | Some res -> res
         | _ -> UnknownValue
    | [ AI_sub ],[t1;t2],_ -> 
         // Note: each use of Unchecked.(+) gets instantiated at a different type and inlined
         match IntegerBinaryOp g Unchecked.(-) Unchecked.(-) Unchecked.(-) Unchecked.(-) Unchecked.(-) Unchecked.(-) Unchecked.(-) Unchecked.(-) t1 t2 with 
         | Some res -> res
         | _ -> UnknownValue
    | [ AI_mul ],[a;b],_ -> (match IntegerBinaryOp g Unchecked.( * )  Unchecked.( * ) Unchecked.( * )  Unchecked.( * ) Unchecked.( * )  Unchecked.( * ) Unchecked.( * )  Unchecked.( * ) a b with Some res -> res | None -> UnknownValue)
    | [ AI_and ],[a;b],_ -> (match IntegerBinaryOp g (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) (&&&) a b  with Some res -> res | None -> UnknownValue)
    | [ AI_or  ],[a;b],_ -> (match IntegerBinaryOp g (|||) (|||) (|||) (|||) (|||) (|||) (|||) (|||) a b  with Some res -> res | None -> UnknownValue)
    | [ AI_xor ],[a;b],_ -> (match IntegerBinaryOp g (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) (^^^) a b  with Some res -> res | None -> UnknownValue)
    | [ AI_not ],[a],_ -> (match IntegerUnaryOp g (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) (~~~) a with Some res -> res | None -> UnknownValue)
    | [ AI_neg ],[a],_ -> (match SignedIntegerUnaryOp g (~-) (~-) (~-) (~-) a with Some res -> res | None -> UnknownValue)

    | [ AI_ceq ],[a;b],_ -> 
       match stripValue a, stripValue b with
       | ConstValue(Const.Bool   a1,_),ConstValue(Const.Bool   a2,_)  -> mkBoolVal g (a1 = a2)
       | ConstValue(Const.SByte  a1,_),ConstValue(Const.SByte  a2,_) -> mkBoolVal g (a1 = a2)
       | ConstValue(Const.Int16  a1,_),ConstValue(Const.Int16  a2,_)  -> mkBoolVal g (a1 = a2)
       | ConstValue(Const.Int32  a1,_),ConstValue(Const.Int32  a2,_)  -> mkBoolVal g (a1 = a2)
       | ConstValue(Const.Int64  a1,_),ConstValue(Const.Int64  a2,_)  -> mkBoolVal g (a1 = a2)
       | ConstValue(Const.Char   a1,_),ConstValue(Const.Char   a2,_)  -> mkBoolVal g (a1 = a2)
       | ConstValue(Const.Byte   a1,_),ConstValue(Const.Byte   a2,_)   -> mkBoolVal g (a1 = a2)
       | ConstValue(Const.UInt16 a1,_),ConstValue(Const.UInt16 a2,_)  -> mkBoolVal g (a1 = a2)
       | ConstValue(Const.UInt32 a1,_),ConstValue(Const.UInt32 a2,_)  -> mkBoolVal g (a1 = a2)
       | ConstValue(Const.UInt64 a1,_),ConstValue(Const.UInt64 a2,_)  -> mkBoolVal g (a1 = a2)
       | _ -> UnknownValue
    | [ AI_clt ],[a;b],_ -> 
       match stripValue a,stripValue b with
       | ConstValue(Const.Bool  a1,_),ConstValue(Const.Bool  a2,_) -> mkBoolVal g (a1 < a2)
       | ConstValue(Const.Int32 a1,_),ConstValue(Const.Int32 a2,_) -> mkBoolVal g (a1 < a2)
       | ConstValue(Const.Int64 a1,_),ConstValue(Const.Int64 a2,_) -> mkBoolVal g (a1 < a2)
       | ConstValue(Const.SByte a1,_),ConstValue(Const.SByte a2,_) -> mkBoolVal g (a1 < a2)
       | ConstValue(Const.Int16 a1,_),ConstValue(Const.Int16 a2,_) -> mkBoolVal g (a1 < a2)
       | _ -> UnknownValue
    | [ (AI_conv(DT_U1))],[a],[ty] when typeEquiv g ty g.byte_ty -> 
       match stripValue a with
       | ConstValue(Const.SByte  a,_) -> mkUInt8Val g (Unchecked.byte a)
       | ConstValue(Const.Int16  a,_) -> mkUInt8Val g (Unchecked.byte a)
       | ConstValue(Const.Int32  a,_) -> mkUInt8Val g (Unchecked.byte a)
       | ConstValue(Const.Int64  a,_) -> mkUInt8Val g (Unchecked.byte a)
       | ConstValue(Const.Byte   a,_) -> mkUInt8Val g (Unchecked.byte a)
       | ConstValue(Const.UInt16 a,_) -> mkUInt8Val g (Unchecked.byte a)
       | ConstValue(Const.UInt32 a,_) -> mkUInt8Val g (Unchecked.byte a)
       | ConstValue(Const.UInt64 a,_) -> mkUInt8Val g (Unchecked.byte a)
       | _ -> UnknownValue
    | [ (AI_conv(DT_U2))],[a],[ty] when typeEquiv g ty g.uint16_ty -> 
       match stripValue a with
       | ConstValue(Const.SByte   a,_) -> mkUInt16Val g (Unchecked.uint16 a)
       | ConstValue(Const.Int16  a,_) -> mkUInt16Val g (Unchecked.uint16 a)
       | ConstValue(Const.Int32  a,_) -> mkUInt16Val g (Unchecked.uint16 a)
       | ConstValue(Const.Int64  a,_) -> mkUInt16Val g (Unchecked.uint16 a)
       | ConstValue(Const.Byte  a,_) -> mkUInt16Val g (Unchecked.uint16 a)
       | ConstValue(Const.UInt16 a,_) -> mkUInt16Val g (Unchecked.uint16 a)
       | ConstValue(Const.UInt32 a,_) -> mkUInt16Val g (Unchecked.uint16 a)
       | ConstValue(Const.UInt64 a,_) -> mkUInt16Val g (Unchecked.uint16 a)
       | _ -> UnknownValue
    | [ (AI_conv(DT_U4))],[a],[ty] when typeEquiv g ty g.uint32_ty -> 
       match stripValue a with
       | ConstValue(Const.SByte   a,_) -> mkUInt32Val g (Unchecked.uint32 a)
       | ConstValue(Const.Int16  a,_) -> mkUInt32Val g (Unchecked.uint32 a)
       | ConstValue(Const.Int32  a,_) -> mkUInt32Val g (Unchecked.uint32 a)
       | ConstValue(Const.Int64  a,_) -> mkUInt32Val g (Unchecked.uint32 a)
       | ConstValue(Const.Byte  a,_) -> mkUInt32Val g (Unchecked.uint32 a)
       | ConstValue(Const.UInt16 a,_) -> mkUInt32Val g (Unchecked.uint32 a)
       | ConstValue(Const.UInt32 a,_) -> mkUInt32Val g (Unchecked.uint32 a)
       | ConstValue(Const.UInt64 a,_) -> mkUInt32Val g (Unchecked.uint32 a)
       | _ -> UnknownValue
    | [ (AI_conv(DT_U8))],[a],[ty] when typeEquiv g ty g.uint64_ty  -> 
       match stripValue a with
       | ConstValue(Const.SByte   a,_) -> mkUInt64Val g (Unchecked.uint64 a)
       | ConstValue(Const.Int16  a,_) -> mkUInt64Val g (Unchecked.uint64 a)
       | ConstValue(Const.Int32  a,_) -> mkUInt64Val g (Unchecked.uint64 a)
       | ConstValue(Const.Int64  a,_) -> mkUInt64Val g (Unchecked.uint64 a)
       | ConstValue(Const.Byte  a,_) -> mkUInt64Val g (Unchecked.uint64 a)
       | ConstValue(Const.UInt16 a,_) -> mkUInt64Val g (Unchecked.uint64 a)
       | ConstValue(Const.UInt32 a,_) -> mkUInt64Val g (Unchecked.uint64 a)
       | ConstValue(Const.UInt64 a,_) -> mkUInt64Val g (Unchecked.uint64 a)
       | _ -> UnknownValue
    | [ (AI_conv(DT_I1))],[a],[ty] when typeEquiv g ty g.sbyte_ty  -> 
       match stripValue a with
       | ConstValue(Const.SByte   a,_) -> mkInt8Val g (Unchecked.sbyte a)
       | ConstValue(Const.Int16  a,_) -> mkInt8Val g (Unchecked.sbyte a)
       | ConstValue(Const.Int32  a,_) -> mkInt8Val g (Unchecked.sbyte a)
       | ConstValue(Const.Int64  a,_) -> mkInt8Val g (Unchecked.sbyte a)
       | ConstValue(Const.Byte  a,_) -> mkInt8Val g (Unchecked.sbyte a)
       | ConstValue(Const.UInt16 a,_) -> mkInt8Val g (Unchecked.sbyte a)
       | ConstValue(Const.UInt32 a,_) -> mkInt8Val g (Unchecked.sbyte a)
       | ConstValue(Const.UInt64 a,_) -> mkInt8Val g (Unchecked.sbyte a)
       | _ -> UnknownValue
    | [ (AI_conv(DT_I2))],[a],[ty] when typeEquiv g ty g.int16_ty  -> 
       match stripValue a with
       | ConstValue(Const.Int32  a,_) -> mkInt16Val g (Unchecked.int16 a)
       | ConstValue(Const.Int16  a,_) -> mkInt16Val g (Unchecked.int16 a)
       | ConstValue(Const.SByte   a,_) -> mkInt16Val g (Unchecked.int16 a)
       | ConstValue(Const.Int64  a,_) -> mkInt16Val g (Unchecked.int16 a)
       | ConstValue(Const.UInt32 a,_) -> mkInt16Val g (Unchecked.int16 a)
       | ConstValue(Const.UInt16 a,_) -> mkInt16Val g (Unchecked.int16 a)
       | ConstValue(Const.Byte  a,_) -> mkInt16Val g (Unchecked.int16 a)
       | ConstValue(Const.UInt64 a,_) -> mkInt16Val g (Unchecked.int16 a)
       | _ -> UnknownValue
    | [ (AI_conv(DT_I4))],[a],[ty] when typeEquiv g ty g.int32_ty -> 
       match stripValue a with
       | ConstValue(Const.Int32  a,_) -> mkInt32Val g (Unchecked.int32 a)
       | ConstValue(Const.Int16  a,_) -> mkInt32Val g (Unchecked.int32 a)
       | ConstValue(Const.SByte   a,_) -> mkInt32Val g (Unchecked.int32 a)
       | ConstValue(Const.Int64  a,_) -> mkInt32Val g (Unchecked.int32 a)
       | ConstValue(Const.UInt32 a,_) -> mkInt32Val g (Unchecked.int32 a)
       | ConstValue(Const.UInt16 a,_) -> mkInt32Val g (Unchecked.int32 a)
       | ConstValue(Const.Byte  a,_) -> mkInt32Val g (Unchecked.int32 a)
       | ConstValue(Const.UInt64 a,_) -> mkInt32Val g (Unchecked.int32 a)
       | _ -> UnknownValue
    | [ (AI_conv(DT_I8))],[a],[ty] when typeEquiv g ty g.int64_ty  -> 
       match stripValue a with
       | ConstValue(Const.Int32  a,_) -> mkInt64Val g (Unchecked.int64 a)
       | ConstValue(Const.Int16  a,_) -> mkInt64Val g (Unchecked.int64 a)
       | ConstValue(Const.SByte  a,_) -> mkInt64Val g (Unchecked.int64 a)
       | ConstValue(Const.Int64  a,_) -> mkInt64Val g (Unchecked.int64 a)
       | ConstValue(Const.UInt32 a,_) -> mkInt64Val g (Unchecked.int64 a)
       | ConstValue(Const.UInt16 a,_) -> mkInt64Val g (Unchecked.int64 a)
       | ConstValue(Const.Byte   a,_) -> mkInt64Val g (Unchecked.int64 a)
       | ConstValue(Const.UInt64 a,_) -> mkInt64Val g (Unchecked.int64 a)
       | _ -> UnknownValue
    | [ AI_clt_un ],[a;b],[ty] when typeEquiv g ty g.bool_ty  -> 
       match stripValue a,stripValue b with
       | ConstValue(Const.Char   a1,_),ConstValue(Const.Char   a2,_) -> mkBoolVal g (a1 < a2)
       | ConstValue(Const.Byte   a1,_),ConstValue(Const.Byte  a2,_)  -> mkBoolVal g (a1 < a2)
       | ConstValue(Const.UInt16 a1,_),ConstValue(Const.UInt16 a2,_) -> mkBoolVal g (a1 < a2)
       | ConstValue(Const.UInt32 a1,_),ConstValue(Const.UInt32 a2,_) -> mkBoolVal g (a1 < a2)
       | ConstValue(Const.UInt64 a1,_),ConstValue(Const.UInt64 a2,_) -> mkBoolVal g (a1 < a2)
       | _ -> UnknownValue
    | [ AI_cgt ],[a;b],[ty] when typeEquiv g ty g.bool_ty  -> 
       match stripValue a,stripValue b with
       | ConstValue(Const.SByte a1,_),ConstValue(Const.SByte  a2,_) -> mkBoolVal g (a1 > a2)
       | ConstValue(Const.Int16 a1,_),ConstValue(Const.Int16 a2,_)  -> mkBoolVal g (a1 > a2)
       | ConstValue(Const.Int32 a1,_),ConstValue(Const.Int32 a2,_)  -> mkBoolVal g (a1 > a2)
       | ConstValue(Const.Int64 a1,_),ConstValue(Const.Int64 a2,_)  -> mkBoolVal g (a1 > a2)
       | _ -> UnknownValue
    | [ AI_cgt_un ],[a;b],[ty] when typeEquiv g ty g.bool_ty   -> 
       match stripValue a,stripValue b with
       | ConstValue(Const.Char   a1,_),ConstValue(Const.Char   a2,_) -> mkBoolVal g (a1 > a2)
       | ConstValue(Const.Byte   a1,_),ConstValue(Const.Byte  a2,_)  -> mkBoolVal g (a1 > a2)
       | ConstValue(Const.UInt16 a1,_),ConstValue(Const.UInt16 a2,_) -> mkBoolVal g (a1 > a2)
       | ConstValue(Const.UInt32 a1,_),ConstValue(Const.UInt32 a2,_) -> mkBoolVal g (a1 > a2)
       | ConstValue(Const.UInt64 a1,_),ConstValue(Const.UInt64 a2,_) -> mkBoolVal g (a1 > a2)
       | _ -> UnknownValue
    | [ AI_shl ],[a;n],_ -> 
       match stripValue a,stripValue n with
       | ConstValue(Const.Int64  a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 63 -> (mkInt64Val  g (a <<< n))
       | ConstValue(Const.Int32  a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 31 -> (mkInt32Val  g (a <<< n))
       | ConstValue(Const.Int16  a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 15 -> (mkInt16Val  g (a <<< n))
       | ConstValue(Const.SByte  a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 7  -> (mkInt8Val   g (a <<< n))
       | ConstValue(Const.UInt64 a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 63 -> (mkUInt64Val g (a <<< n))
       | ConstValue(Const.UInt32 a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 31 -> (mkUInt32Val g (a <<< n))
       | ConstValue(Const.UInt16 a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 15 -> (mkUInt16Val g (a <<< n))
       | ConstValue(Const.Byte   a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 7  -> (mkUInt8Val  g (a <<< n))
       | _ -> UnknownValue

    | [ AI_shr ],[a;n],_ -> 
       match stripValue a,stripValue n with
       | ConstValue(Const.SByte a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 7  -> (mkInt8Val  g (a >>> n))
       | ConstValue(Const.Int16 a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 15 -> (mkInt16Val g (a >>> n))
       | ConstValue(Const.Int32 a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 31 -> (mkInt32Val g (a >>> n))
       | ConstValue(Const.Int64 a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 63 -> (mkInt64Val g (a >>> n))
       | _ -> UnknownValue
    | [ AI_shr_un ],[a;n],_ -> 
       match stripValue a,stripValue n with
       | ConstValue(Const.Byte   a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 7  -> (mkUInt8Val g  (a >>> n))
       | ConstValue(Const.UInt16 a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 15 -> (mkUInt16Val g (a >>> n))
       | ConstValue(Const.UInt32 a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 31 -> (mkUInt32Val g (a >>> n))
       | ConstValue(Const.UInt64 a,_),ConstValue(Const.Int32 n,_) when n >= 0 && n <= 63 -> (mkUInt64Val g (a >>> n))
       | _ -> UnknownValue
       
    // Retypings using IL asm "" are quite common in prim-types.fs
    // Sometimes these are only to get the primitives to pass the type checker.
    // Here we check for retypings from know values to known types.
    // We're conservative not to apply any actual data-changing conversions here.
    | [ ],[v],[ty] -> 
       match stripValue v with
       | ConstValue(Const.Bool   a,_) ->
            if typeEquiv g ty g.bool_ty then v
            elif typeEquiv g ty g.sbyte_ty then mkInt8Val g (if a then 1y else 0y)
            elif typeEquiv g ty g.int16_ty then mkInt16Val g (if a then 1s else 0s)
            elif typeEquiv g ty g.int32_ty then mkInt32Val g (if a then 1 else 0)
            elif typeEquiv g ty g.byte_ty then mkUInt8Val g (if a then 1uy else 0uy)
            elif typeEquiv g ty g.uint16_ty then mkUInt16Val g (if a then 1us else 0us)
            elif typeEquiv g ty g.uint32_ty then mkUInt32Val g (if a then 1u else 0u)
            else UnknownValue
       | ConstValue(Const.SByte   a,_) ->
            if typeEquiv g ty g.sbyte_ty then v
            elif typeEquiv g ty g.int16_ty then mkInt16Val g (Unchecked.int16 a)
            elif typeEquiv g ty g.int32_ty then mkInt32Val g (Unchecked.int32 a)
            else UnknownValue
       | ConstValue(Const.Byte   a,_) ->
            if typeEquiv g ty g.byte_ty then v
            elif typeEquiv g ty g.uint16_ty then mkUInt16Val g (Unchecked.uint16 a)
            elif typeEquiv g ty g.uint32_ty then mkUInt32Val g (Unchecked.uint32 a)
            else UnknownValue
       | ConstValue(Const.Int16   a,_) ->
            if typeEquiv g ty g.int16_ty then v
            elif typeEquiv g ty g.int32_ty then mkInt32Val g (Unchecked.int32 a)
            else UnknownValue
       | ConstValue(Const.UInt16   a,_) ->
            if typeEquiv g ty g.uint16_ty then v
            elif typeEquiv g ty g.uint32_ty then mkUInt32Val g (Unchecked.uint32 a)
            else UnknownValue
       | ConstValue(Const.Int32   a,_) ->
            if typeEquiv g ty g.int32_ty then v
            elif typeEquiv g ty g.uint32_ty then mkUInt32Val g (Unchecked.uint32 a)
            else UnknownValue
       | ConstValue(Const.UInt32   a,_) ->
            if typeEquiv g ty g.uint32_ty then v
            elif typeEquiv g ty g.int32_ty then mkInt32Val g (Unchecked.int32 a)
            else UnknownValue
       | ConstValue(Const.Int64   a,_) ->
            if typeEquiv g ty g.int64_ty then v
            elif typeEquiv g ty g.uint64_ty then mkUInt64Val g (Unchecked.uint64 a)
            else UnknownValue
       | ConstValue(Const.UInt64   a,_) ->
            if typeEquiv g ty g.uint64_ty then v
            elif typeEquiv g ty g.int64_ty then mkInt64Val g (Unchecked.int64 a)
            else UnknownValue
       | _ -> UnknownValue
    | _ -> UnknownValue


//-------------------------------------------------------------------------
// Size constants and combinators
//------------------------------------------------------------------------- 

let localVarSize = 1

let rec AddTotalSizesAux acc l = match l with [] -> acc | h::t -> AddTotalSizesAux (h.TotalSize + acc) t
let AddTotalSizes l = AddTotalSizesAux 0 l

let rec AddFunctionSizesAux acc l = match l with [] -> acc | h::t -> AddFunctionSizesAux (h.FunctionSize + acc) t
let AddFunctionSizes l = AddFunctionSizesAux 0 l

let AddTotalSizesFlat l = l |> FlatList.sumBy (fun x -> x.TotalSize) 
let AddFunctionSizesFlat l = l |> FlatList.sumBy (fun x -> x.FunctionSize) 

//-------------------------------------------------------------------------
// opt list/array combinators - zipping (_,_) return type
//------------------------------------------------------------------------- 
let rec OrEffects l = match l with [] -> false | h::t -> h.HasEffect || OrEffects t
let OrEffectsFlat l = FlatList.exists (fun x -> x.HasEffect) l

let rec OrTailcalls l = match l with [] -> false | h::t -> h.MightMakeCriticalTailcall || OrTailcalls t
let OrTailcallsFlat l = FlatList.exists (fun x -> x.MightMakeCriticalTailcall) l
        
let rec OptimizeListAux f l acc1 acc2 = 
    match l with 
    | [] -> List.rev acc1, List.rev acc2
    | (h ::t) -> 
        let (x1,x2)  = f h
        OptimizeListAux f t (x1::acc1) (x2::acc2) 

let OptimizeList f l = OptimizeListAux f l [] [] 

let OptimizeFlatList f l = l |> FlatList.map f |> FlatList.unzip 

let NoExprs : (Expr list * list<Summary<ExprValueInfo>>)= [],[]
let NoFlatExprs : (FlatExprs * FlatList<Summary<ExprValueInfo>>) = FlatList.empty, FlatList.empty

//-------------------------------------------------------------------------
// Common ways of building new value infos
//------------------------------------------------------------------------- 

let CombineValueInfos einfos res = 
      { TotalSize  = AddTotalSizes einfos;
        FunctionSize  = AddFunctionSizes einfos;
        HasEffect = OrEffects einfos; 
        MightMakeCriticalTailcall = OrTailcalls einfos; 
        Info = res }

let CombineFlatValueInfos einfos res = 
      { TotalSize  = AddTotalSizesFlat einfos;
        FunctionSize  = AddFunctionSizesFlat einfos;
        HasEffect = OrEffectsFlat einfos; 
        MightMakeCriticalTailcall = OrTailcallsFlat einfos; 
        Info = res }

let CombineValueInfosUnknown einfos = CombineValueInfos einfos UnknownValue
let CombineFlatValueInfosUnknown einfos = CombineFlatValueInfos einfos UnknownValue

//-------------------------------------------------------------------------
// Hide information because of a signature
//------------------------------------------------------------------------- 

let AbstractLazyModulInfoByHiding isAssemblyBoundary mhi =

    // The freevars and FreeTyvars can indicate if the non-public (hidden) items have been used.
    // Under those checks, the further hidden* checks may be subsumed (meaning, not required anymore).

    let hiddenTycon,hiddenTyconRepr,hiddenVal, hiddenRecdField, hiddenUnionCase = 
        Zset.memberOf mhi.mhiTycons, 
        Zset.memberOf mhi.mhiTyconReprs, 
        Zset.memberOf mhi.mhiVals, 
        Zset.memberOf mhi.mhiRecdFields, 
        Zset.memberOf mhi.mhiUnionCases

    let rec abstractExprInfo ivalue = 
        if verboseOptimizationInfo then dprintf "abstractExprInfo\n"; 
        match ivalue with 
        (* Check for escaping value. Revert to old info if possible  *)
        | ValValue (vref2,detail) ->
            let detail' = abstractExprInfo detail 
            let v2 = vref2.Deref 
            let tyvars = freeInVal CollectAll v2 
            if  
                (isAssemblyBoundary && not (freeTyvarsAllPublic tyvars)) || 
                Zset.exists hiddenTycon tyvars.FreeTycons || 
                hiddenVal v2
            then detail'
            else ValValue (vref2,detail')
        // Check for escape in lambda 
        | CurriedLambdaValue (_,_,_,expr,_) | ConstExprValue(_,expr) when            
            (let fvs = freeInExpr CollectAll expr
             (isAssemblyBoundary && not (freeVarsAllPublic fvs)) || 
             Zset.exists hiddenVal       fvs.FreeLocals            ||
             Zset.exists hiddenTycon     fvs.FreeTyvars.FreeTycons ||
             Zset.exists hiddenTyconRepr fvs.FreeLocalTyconReprs   ||
             Zset.exists hiddenRecdField fvs.FreeRecdFields        ||
             Zset.exists hiddenUnionCase fvs.FreeUnionCases ) ->
                UnknownValue
        // Check for escape in constant 
        | ConstValue(_,ty) when 
            (let ftyvs = freeInType CollectAll ty
             (isAssemblyBoundary && not (freeTyvarsAllPublic ftyvs)) || 
             Zset.exists hiddenTycon ftyvs.FreeTycons) ->
                UnknownValue
        | TupleValue vinfos         -> 
            TupleValue (Array.map abstractExprInfo vinfos)
        | RecdValue (tcref,vinfos)  -> 
            if hiddenTyconRepr tcref.Deref || Array.exists (mkNestedRecdFieldRef tcref >> hiddenRecdField) tcref.AllFieldsArray
            then UnknownValue 
            else RecdValue (tcref,Array.map abstractExprInfo vinfos)
        | UnionCaseValue(ucref,vinfos) -> 
            let tcref = ucref.TyconRef
            if hiddenTyconRepr ucref.Tycon || tcref.UnionCasesArray |> Array.exists (mkNestedUnionCaseRef tcref >> hiddenUnionCase) 
            then UnknownValue 
            else UnionCaseValue (ucref,Array.map abstractExprInfo vinfos)
        | SizeValue(_vdepth,vinfo)   -> MakeSizedValueInfo (abstractExprInfo vinfo)
        | UnknownValue  
        | ConstExprValue _   
        | CurriedLambdaValue _ 
        | ConstValue _  -> ivalue
    and abstractValInfo v = 
        { ValExprInfo=abstractExprInfo v.ValExprInfo; 
          ValMakesNoCriticalTailcalls=v.ValMakesNoCriticalTailcalls }
    and abstractModulInfo ss =
         if verboseOptimizationInfo then dprintf "abstractModulInfo\n"; 
         { ModuleOrNamespaceInfos = NameMap.map abstractLazyModulInfo ss.ModuleOrNamespaceInfos;
           ValInfos = 
               ValInfos(ss.ValInfos.Entries 
                         |> Seq.filter (fun (vref,_) -> not (hiddenVal vref.Deref))
                         |> Seq.map (fun (vref,e) -> check (* "its implementation uses a binding hidden by a signature" m *)  vref (abstractValInfo e) )) } 
    and abstractLazyModulInfo (ss:LazyModuleInfo) = 
          ss.Force() |> abstractModulInfo |> notlazy

    abstractLazyModulInfo

/// Hide all information except what we need for "must inline". We always save this optimization information
let AbstractLazyModulInfoToEssentials =

    let rec abstractModulInfo (ss:ModuleInfo) =
         { ModuleOrNamespaceInfos = NameMap.map (Lazy.force >> abstractModulInfo >> notlazy) ss.ModuleOrNamespaceInfos;
           ValInfos =  ss.ValInfos.Filter (fun (v,_) -> v.MustInline) }
    and abstractLazyModulInfo ss = ss |> Lazy.force |> abstractModulInfo |> notlazy
      
    abstractLazyModulInfo


//-------------------------------------------------------------------------
// Hide information because of a "let ... in ..." or "let rec  ... in ... "
//------------------------------------------------------------------------- 

let AbstractExprInfoByVars (boundVars:Val list,boundTyVars) ivalue =
  // Module and member bindings can be skipped when checking abstraction, since abstraction of these values has already been done when 
  // we hit the end of the module and called AbstractLazyModulInfoByHiding. If we don't skip these then we end up quadtratically retraversing  
  // the inferred optimization data, i.e. at each binding all the way up a sequences of 'lets' in a module. 
  let boundVars = boundVars |> List.filter (fun v -> not v.IsMemberOrModuleBinding)

  match boundVars,boundTyVars with 
  | [],[] -> ivalue
  | _ -> 

      let rec abstractExprInfo ivalue =
          match ivalue with 
          // Check for escaping value. Revert to old info if possible  
          | ValValue (VRefLocal v2,detail) when  
            (nonNil boundVars && List.exists (valEq v2) boundVars) || 
            (nonNil boundTyVars &&
             let ftyvs = freeInVal CollectTypars v2
             List.exists (Zset.memberOf ftyvs.FreeTypars) boundTyVars) -> 

              if verboseOptimizationInfo then 
                  dprintf "hiding value '%s' when used in expression (see %a)\n" v2.LogicalName outputRange v2.Range;
                  let ftyvs = freeInVal CollectTypars v2
                  ftyvs.FreeTypars |> Zset.iter (fun v -> dprintf "  -- ftyv  %s @ %a\n" v.Name outputRange v.Range);
                  boundVars |> List.iter (fun v -> dprintf "  -- bv  %s @ %a\n" v.LogicalName outputRange v.Range);
                  boundTyVars |> List.iter (fun v -> dprintf "  -- btyv  %s @ %a\n" v.Name outputRange v.Range)

              abstractExprInfo detail
          | ValValue (v2,detail) -> 
              let detail' = abstractExprInfo detail
              ValValue (v2,detail')
        
          // Check for escape in lambda 
          | CurriedLambdaValue (_,_,_,expr,_) | ConstExprValue(_,expr)  when 
            (let fvs = freeInExpr (if isNil boundTyVars then CollectLocals else CollectTyparsAndLocals) expr
             (nonNil boundVars   && List.exists (Zset.memberOf fvs.FreeLocals) boundVars) ||
             (nonNil boundTyVars && List.exists (Zset.memberOf fvs.FreeTyvars.FreeTypars) boundTyVars) ||
             (fvs.UsesMethodLocalConstructs )) ->
              if verboseOptimizationInfo then 
                  let fvs = freeInExpr (if isNil boundTyVars then CollectLocals else CollectTyparsAndLocals) expr
                  dprintf "Trimming lambda @ %a, UsesMethodLocalConstructs = %b, exprL = %s\n"  outputRange expr.Range fvs.UsesMethodLocalConstructs (showL (exprL expr));
                  fvs.FreeLocals |> Zset.iter (fun v -> dprintf "fv  %s @ %a\n" v.LogicalName outputRange v.Range);
                  fvs.FreeTyvars.FreeTypars |> Zset.iter (fun v -> dprintf "ftyv  %s @ %a\n" v.Name outputRange v.Range);
                  boundVars |> List.iter (fun v -> dprintf "bv  %s @ %a\n" v.LogicalName outputRange v.Range);
                  boundTyVars |> List.iter (fun v -> dprintf "btyv  %s @ %a\n" v.Name outputRange v.Range)

              UnknownValue

          // Check for escape in generic constant
          | ConstValue(_,ty) when 
            (nonNil boundTyVars && 
             (let ftyvs = freeInType CollectTypars ty
              List.exists (Zset.memberOf ftyvs.FreeTypars) boundTyVars)) ->
              UnknownValue

          // Otherwise check all sub-values 
          | TupleValue vinfos -> TupleValue (Array.map (abstractExprInfo) vinfos)
          | RecdValue (tcref,vinfos) -> RecdValue (tcref,Array.map (abstractExprInfo) vinfos)
          | UnionCaseValue (cspec,vinfos) -> UnionCaseValue(cspec,Array.map (abstractExprInfo) vinfos)
          | CurriedLambdaValue _ 
          | ConstValue _ 
          | ConstExprValue _ 
          | UnknownValue -> ivalue
          | SizeValue (_vdepth,vinfo) -> MakeSizedValueInfo (abstractExprInfo vinfo)

      and abstractValInfo v = 
          { ValExprInfo=abstractExprInfo v.ValExprInfo; 
            ValMakesNoCriticalTailcalls=v.ValMakesNoCriticalTailcalls }

      and abstractModulInfo ss =
         { ModuleOrNamespaceInfos  = ss.ModuleOrNamespaceInfos  |> NameMap.map (Lazy.force >> abstractModulInfo >> notlazy) ;
           ValInfos = ss.ValInfos.Map (fun (vref,e) -> 
               if verboseOptimizationInfo then dprintf "checking %s @ %a\n" vref.LogicalName outputRange (vref.Range); 
               check (* "its implementation uses a private binding" m *) vref (abstractValInfo e) ) }

      abstractExprInfo ivalue

//-------------------------------------------------------------------------
// Remap optimization information, e.g. to use public stable references so we can pickle it
// to disk.
//------------------------------------------------------------------------- 
let RemapLazyModulInfo g tmenv =

    let rec remapExprInfo ivalue = 
        if verboseOptimizationInfo then dprintf "remapExprInfo\n"; 
        match ivalue with 
        | ValValue (v,detail)      -> ValValue (remapValRef tmenv v,remapExprInfo detail)
        | TupleValue vinfos         -> TupleValue (Array.map remapExprInfo vinfos)
        | RecdValue (tcref,vinfos)  -> RecdValue (remapTyconRef tmenv.tyconRefRemap tcref, Array.map remapExprInfo vinfos)
        | UnionCaseValue(cspec,vinfos) -> UnionCaseValue (remapUnionCaseRef tmenv.tyconRefRemap cspec,Array.map remapExprInfo vinfos)
        | SizeValue(_vdepth,vinfo) -> MakeSizedValueInfo (remapExprInfo vinfo)
        | UnknownValue              -> UnknownValue
        | CurriedLambdaValue (uniq,arity,sz,expr,typ)  -> CurriedLambdaValue (uniq,arity,sz,remapExpr g CloneAll tmenv expr,remapPossibleForallTy g tmenv typ)  
        | ConstValue (c,ty)  -> ConstValue (c,remapPossibleForallTy g tmenv ty)
        | ConstExprValue (sz,expr)  -> ConstExprValue (sz,remapExpr g CloneAll tmenv expr)

    let remapValInfo v = { ValExprInfo=remapExprInfo v.ValExprInfo; ValMakesNoCriticalTailcalls=v.ValMakesNoCriticalTailcalls }
    let rec remapModulInfo ss =
         if verboseOptimizationInfo then dprintf "remapModulInfo\n"; 
         { ModuleOrNamespaceInfos = ss.ModuleOrNamespaceInfos |> NameMap.map RemapLazyModulInfo;
           ValInfos = ss.ValInfos.Map (fun (vref,vinfo) -> 
                                                     let vref' = remapValRef tmenv vref 
                                                     let vinfo = remapValInfo vinfo
                                                     // Propogate any inferred ValMakesNoCriticalTailcalls flag from implementation to signature information
                                                     if vinfo.ValMakesNoCriticalTailcalls then vref'.Deref.SetMakesNoCriticalTailcalls() 
                                                     (vref',vinfo)) } 

    and RemapLazyModulInfo ss =
         ss |> Lazy.force |> remapModulInfo |> notlazy
           
    RemapLazyModulInfo

//-------------------------------------------------------------------------
// Hide information when a value is no longer visible
//------------------------------------------------------------------------- 

let AbstractAndRemapModulInfo msg g m (repackage,hidden) info =
    let mrpi = mkRepackageRemapping repackage
    if verboseOptimizationInfo then dprintf "%s - %a - Optimization data prior to trim: \n%s\n" msg outputRange m (Layout.showL (Layout.squashTo 192 (moduleInfoL g info)));
    let info = info |> AbstractLazyModulInfoByHiding false hidden
    if verboseOptimizationInfo then dprintf "%s - %a - Optimization data after trim:\n%s\n" msg outputRange m (Layout.showL (Layout.squashTo 192 (moduleInfoL g info)));
    let info = info |> RemapLazyModulInfo g mrpi
    if verboseOptimizationInfo then dprintf "%s - %a - Optimization data after remap:\n%s\n" msg outputRange m (Layout.showL (Layout.squashTo 192 (moduleInfoL g info)));
    info

//-------------------------------------------------------------------------
// Misc helerps
//------------------------------------------------------------------------- 

// Mark some variables (the ones we introduce via abstractBigTargets) as don't-eliminate 
let suffixForVariablesThatMayNotBeEliminated = "$cont"

/// Type applications of F# "type functions" may cause side effects, e.g. 
/// let x<'a> = printfn "hello"; typeof<'a> 
/// In this case do not treat them as constants. 
let IsTyFuncValRefExpr = function 
    | Expr.Val (fv,_,_) -> fv.IsTypeFunction
    | _ -> false

/// Type applications of existing functions are always simple constants, with the exception of F# 'type functions' 
/// REVIEW: we could also include any under-applied application here. 
let rec IsSmallConstExpr x =
    match x with
    | Expr.Val (v,_,_m) -> not v.IsMutable
    | Expr.App(fe,_,_tyargs,args,_) -> isNil(args) && not (IsTyFuncValRefExpr fe) && IsSmallConstExpr fe
    | _ -> false

let ValueOfExpr expr = 
    if IsSmallConstExpr expr then 
      ConstExprValue(0,expr)
    else UnknownValue

//-------------------------------------------------------------------------
// Dead binding elimination 
//------------------------------------------------------------------------- 
 
let ValueIsUsedOrHasEffect cenv fvs (b:Binding,binfo) =
    let v = b.Var
    not (cenv.settings.EliminateUnusedBindings()) ||
    isSome v.MemberInfo ||
    binfo.HasEffect || 
    Zset.contains v (fvs())

let rec SplitValuesByIsUsedOrHasEffect cenv fvs x = 
    x |> FlatList.filter (ValueIsUsedOrHasEffect cenv fvs) |> FlatList.unzip

//-------------------------------------------------------------------------
// 
//------------------------------------------------------------------------- 

let IlAssemblyCodeInstrHasEffect i = 
    match i with 
    | (  AI_nop | AI_ldc _ | AI_add | AI_sub | AI_mul | AI_xor | AI_and | AI_or 
               | AI_ceq | AI_cgt | AI_cgt_un | AI_clt | AI_clt_un | AI_conv _ | AI_shl 
               | AI_shr | AI_shr_un | AI_neg | AI_not | AI_ldnull )
    | I_ldstr _ | I_ldtoken _  -> false
    | _ -> true
  
let IlAssemblyCodeHasEffect instrs = List.exists IlAssemblyCodeInstrHasEffect instrs

//-------------------------------------------------------------------------
// Effects
//
// note: allocating an object with observable identity (i.e. a name) 
// or reading from a mutable field counts as an 'effect', i.e.
// this context 'effect' has it's usual meaning in the effect analysis literature of 
//   read-from-mutable 
//   write-to-mutable 
//   name-generation
//   arbitrary-side-effect (e.g. 'non-termination' or 'fire the missiles')
//------------------------------------------------------------------------- 

let rec ExprHasEffect g expr = 
    match expr with 
    | Expr.Val (vref,_,_) -> vref.IsTypeFunction || (vref.IsMutable)
    | Expr.Quote _ 
    | Expr.Lambda _
    | Expr.TyLambda _ 
    | Expr.Const _ -> false
    /// type applications do not have effects, with the exception of type functions
    | Expr.App(f0,_,_,[],_) -> (IsTyFuncValRefExpr f0) || ExprHasEffect g f0
    | Expr.Op(op,_,args,_) -> ExprsHaveEffect g args || OpHasEffect g op
    | Expr.LetRec(binds,body,_,_) -> BindingsHaveEffect g binds || ExprHasEffect g body
    | Expr.Let(bind,body,_,_) -> BindingHasEffect g bind || ExprHasEffect g body
    // REVIEW: could add Expr.Obj on an interface type - these are similar to records of lambda expressions 
    | _ -> true
and ExprsHaveEffect g exprs = List.exists (ExprHasEffect g) exprs
and BindingsHaveEffect g binds = FlatList.exists (BindingHasEffect g) binds
and BindingHasEffect g bind = bind.Expr |> ExprHasEffect g
and OpHasEffect g op = 
    match op with 
    | TOp.Tuple -> false
    | TOp.Recd (ctor,tcref) -> 
        match ctor with 
        | RecdExprIsObjInit -> true
        | RecdExpr -> isRecdOrUnionOrStructTyconRefAllocObservable g tcref
    | TOp.UnionCase ucref -> isRecdOrUnionOrStructTyconRefAllocObservable g ucref.TyconRef
    | TOp.ExnConstr ecref -> isExnAllocObservable ecref
    | TOp.Bytes _ | TOp.UInt16s _ | TOp.Array -> true (* alloc observable *)
    | TOp.UnionCaseTagGet _ -> false
    | TOp.UnionCaseProof _ -> false
    | TOp.UnionCaseFieldGet (ucref,n) -> isUnionCaseFieldMutable g ucref n 
    | TOp.ILAsm(instrs,_) -> IlAssemblyCodeHasEffect instrs
    | TOp.TupleFieldGet(_) -> false
    | TOp.ExnFieldGet(ecref,n) -> isExnFieldMutable ecref n 
    | TOp.RefAddrGet -> false
    | TOp.ValFieldGet rfref  -> rfref.RecdField.IsMutable
    | TOp.ValFieldGetAddr _rfref  -> true (* check *)
    | TOp.UnionCaseFieldSet _
    | TOp.ExnFieldSet _
    | TOp.Coerce
    | TOp.Reraise
    | TOp.For _ 
    | TOp.While  _
    | TOp.TryCatch _
    | TOp.TryFinally _ (* note: these really go through a different path anyway *)
    | TOp.TraitCall _
    | TOp.Goto _
    | TOp.Label _
    | TOp.Return
    | TOp.ILCall _ (* conservative *)
    | TOp.LValueOp _  (* conservative *)
    | TOp.ValFieldSet _ -> true


let TryEliminateBinding cenv _env (TBind(vspec1,e1,spBind)) e2 _m  =
    // don't eliminate bindings if we're not optimizing AND the binding is not a compiler generated variable
    if not (cenv.optimizing && cenv.settings.EliminateImmediatelyConsumedLocals()) && 
       not vspec1.IsCompilerGenerated then 
       None 
    else
        // Peephole on immediate consumption of single bindings, e.g. "let x = e in x" --> "e" 
        // REVIEW: enhance this by general elimination of bindings to 
        // non-side-effecting expressions that are used only once. 
        // But note the cases below cover some instances of side-effecting expressions as well.... 
        let IsUniqueUse vspec2 args = 
              valEq vspec1 vspec2  
           && (not (vspec2.LogicalName.Contains(suffixForVariablesThatMayNotBeEliminated)))
           // REVIEW: this looks slow. Look only for one variable instead 
           && (let fvs = accFreeInExprs CollectLocals args emptyFreeVars
               not (Zset.contains vspec1 fvs.FreeLocals))

        // Immediate consumption of value as 2nd or subsequent argument to a construction or projection operation 
        let rec GetImmediateUseContext rargsl argsr = 
              match argsr with 
              | (Expr.Val(VRefLocal vspec2,_,_)) :: argsr2
                 when valEq vspec1 vspec2 && IsUniqueUse vspec2 (List.rev rargsl@argsr2) -> Some(List.rev rargsl,argsr2)
              | argsrh :: argsrt when not (ExprHasEffect cenv.g argsrh) -> GetImmediateUseContext (argsrh::rargsl) argsrt 
              | _ -> None

        match stripExpr e2 with 

         // Immediate consumption of value as itself 'let x = e in x'
         | Expr.Val(VRefLocal vspec2,_,_) 
             when IsUniqueUse vspec2 [] -> 
               Some e1

         // Immediate consumption of value by a pattern match 'let x = e in match x with ...'
         | Expr.Match(spMatch,_exprm,TDSwitch(Expr.Val(VRefLocal vspec2,_,_),cases,dflt,_),targets,m,ty2)
             when (valEq vspec1 vspec2 && 
                   let fvs = accFreeInTargets CollectLocals targets (accFreeInSwitchCases CollectLocals cases dflt emptyFreeVars)
                   not (Zset.contains vspec1 fvs.FreeLocals)) -> 

              let spMatch = spBind.Combine(spMatch)
              Some (Expr.Match(spMatch,e1.Range,TDSwitch(e1,cases,dflt,m),targets,m,ty2))
               
         // Immediate consumption of value as a function 'let f = e in f ...' and 'let x = e in f ... x ...'
         // Note functions are evaluated before args 
         // Note: do not include functions with a single arg of unit type, introduced by abstractBigTargets 
         | Expr.App(f,f0ty,tyargs,args,m) 
               when not (vspec1.LogicalName.Contains(suffixForVariablesThatMayNotBeEliminated)) ->
             match GetImmediateUseContext [] (f::args) with 
             | Some([],rargs) -> Some (MakeApplicationAndBetaReduce cenv.g (e1,f0ty,[tyargs],rargs ,m))
             | Some(f::largs,rargs) -> Some (MakeApplicationAndBetaReduce cenv.g (f,f0ty,[tyargs],largs @ (e1::rargs),m))
             | None -> None

         // Bug 6311: a special case of nested elimination of locals (which really should be handled more generally)
         // 'let x = e in op[op[x;arg2];arg3]' --> op[op[e;arg2];arg3]
         // 'let x = e in op[op[arg1;x];arg3]' --> op[op[arg1;e];arg3] when arg1 has no side effects etc.
         // 'let x = e in op[op[arg1;arg2];x]' --> op[op[arg1;arg2];e] when arg1, arg2 have no side effects etc.
         | Expr.Op (c1,tyargs1,[Expr.Op (c2,tyargs2,[arg1;arg2],m2);arg3],m1) -> 
             match GetImmediateUseContext [] [arg1;arg2;arg3] with 
             | Some([],[arg2;arg3]) -> Some (Expr.Op (c1,tyargs1,[Expr.Op (c2,tyargs2,[e1;arg2],m2);arg3],m1))
             | Some([arg1],[arg3])  -> Some (Expr.Op (c1,tyargs1,[Expr.Op (c2,tyargs2,[arg1;e1],m2);arg3],m1))
             | Some([arg1;arg2],[]) -> Some (Expr.Op (c1,tyargs1,[Expr.Op (c2,tyargs2,[arg1;arg2],m2);e1],m1))
             | Some _ -> error(InternalError("unexpected return pattern from GetImmediateUseContext",m1))
             | None -> None

         // Immediate consumption of value as first non-effectful argument to a construction or projection operation 
         // 'let x = e in op[x;....]'
         | Expr.Op (c,tyargs,args,m) -> 
             match GetImmediateUseContext [] args with 
             | Some(largs,rargs) -> Some (Expr.Op (c,tyargs,(largs @ (e1:: rargs)),m))
             | None -> None

         | _ ->  
            None

let TryEliminateLet cenv env bind e2 m = 
    match TryEliminateBinding cenv env bind e2 m with 
    | Some e2' -> e2',-localVarSize  (* eliminated a let, hence reduce size estimate *)
    | None -> mkLetBind m bind e2 ,0

//-------------------------------------------------------------------------

/// Detect the application of a value to an arbitrary number of arguments
let rec (|KnownValApp|_|) expr = 
    match stripExpr expr with
    | Expr.Val(vref,_,_) -> Some(vref,[],[])
    | Expr.App(KnownValApp(vref,typeArgs1,otherArgs1),_,typeArgs2,otherArgs2,_) -> Some(vref,typeArgs1@typeArgs2,otherArgs1@otherArgs2)
    | _ -> None

//-------------------------------------------------------------------------
// ExpandStructuralBinding
//
// Expand bindings to tuple expressions by factoring sub-expression out as prior bindings.
// Similarly for other structural constructions, like records...
// If the item is only projected from then the construction (allocation) can be eliminated.
// This transform encourages that by allowing projections to be simplified.
//------------------------------------------------------------------------- 

let ExprIsValue = function Expr.Val _ -> true | _ -> false
let ExpandStructuralBinding cenv expr =
    match expr with
    | Expr.Let (TBind(v,rhs,tgtSeqPtOpt),body,m,_) 
        when (isTupleExpr rhs &&  
              not v.IsCompiledAsTopLevel &&  
              not v.IsMember && 
              not v.IsTypeFunction &&
              not v.IsMutable) ->
          let args   = tryDestTuple rhs
          if List.forall ExprIsValue args then
              expr (* avoid re-expanding when recursion hits original binding *)
          else
              let argTys = destTupleTy cenv.g v.Type
              let argBind i (arg:Expr) argTy =
                  let name = v.LogicalName ^ "_" ^ string i
                  let v,ve = mkCompGenLocal arg.Range name argTy
                  ve,mkCompGenBind v arg
           
              let ves,binds = List.mapi2 argBind args argTys |> List.unzip
              let tuple = mkTupled cenv.g m ves argTys
              mkLetsBind m binds (mkLet tgtSeqPtOpt m v tuple body)
              (* REVIEW: other cases - records, explicit lists etc. *)
    | expr -> expr
//-------------------------------------------------------------------------
// QueryBuilder.Run elimination helpers
//------------------------------------------------------------------------- 

/// Detect a query { ... }
let (|QueryRun|_|) g expr = 
//#if DEBUG
//    g.query_run_value_vref.Deref |> ignore
//    g.query_run_enumerable_vref.Deref |> ignore
//#endif
    match expr with
    | Expr.App(Expr.Val (vref,_,_),_,_,[_builder; arg],_) when valRefEq g vref g.query_run_value_vref ->  
        Some (arg, None)
    | Expr.App(Expr.Val (vref,_,_),_,[ elemTy ] ,[_builder; arg],_) when valRefEq g vref g.query_run_enumerable_vref ->  
        Some (arg, Some elemTy)
    | _ -> 
        None

let (|MaybeTupled|) e = tryDestTuple e 
let (|AnyInstanceMethodApp|_|) e = 
    match e with 
    | Expr.App(Expr.Val (vref,_,_),_,tyargs,[obj; MaybeTupled args],_) -> Some (vref,tyargs,obj,args)
    | _ -> None

let (|InstanceMethodApp|_|) g (expectedValRef:ValRef) e = 
//#if DEBUG
//    expectedValRef.Deref |> ignore
//#endif
    //printfn "for vref = %A" (expectedValRef.TryDeref  |> Option.map (fun x -> x.DisplayName))
    match e with 
    | AnyInstanceMethodApp (vref,tyargs,obj,args) when valRefEq g vref expectedValRef -> Some (tyargs,obj,args)
    | _ ->  None

let (|QuerySourceEnumerable|_|) g = function
    | InstanceMethodApp g g.query_source_vref ([resTy],_builder, [res]) ->  Some (resTy,res)
    | _ ->  None

let (|QueryFor|_|) g = function
    | InstanceMethodApp g g.query_for_vref  ([srcTy;qTy;resTy;_qInnerTy],_builder, [src;selector]) ->  Some (qTy,srcTy,resTy,src,selector)
    | _ ->  None


let (|QueryYield|_|) g = function
    | InstanceMethodApp g g.query_yield_vref  ([resTy;qTy],_builder, [res]) ->  Some (qTy,resTy,res)
    | _ ->  None

let (|QueryYieldFrom|_|) g = function
    | InstanceMethodApp g g.query_yield_from_vref  ([resTy;qTy],_builder, [res]) ->  Some (qTy,resTy,res)
    | _ ->  None

let (|QuerySelect|_|) g = function
    | InstanceMethodApp g g.query_select_vref  ([srcTy;qTy;resTy],_builder, [src;selector]) ->  Some (qTy,srcTy,resTy,src,selector)
    | _ ->  None

let (|QueryZero|_|) g = function
    | InstanceMethodApp g g.query_zero_vref  ([resTy;qTy],_builder, _) ->  Some (qTy, resTy)
    | _ ->  None

/// Look for a possible tuple and transform
let (|AnyTupleTrans|) e = 
    match e with 
    | Expr.Op (TOp.Tuple,tys,es,m) -> (es, (fun es -> Expr.Op (TOp.Tuple,tys,es,m)))  
    | _ -> [e], (function [e] -> e | _ -> assert false; failwith "unreachable")

/// Look for any QueryBuilder.* operation and transform
let (|AnyQueryBuilderOpTrans|_|) g = function
    | Expr.App((Expr.Val (vref,_,_) as v),vty,tyargs,[builder; AnyTupleTrans( (src::rest), replaceArgs) ],m) when 
          (match vref.ApparentParent with Parent tcref -> tyconRefEq g tcref g.query_builder_tcref | ParentNone -> false) ->  
         Some (src,(fun newSource -> Expr.App(v,vty,tyargs,[builder; replaceArgs(newSource::rest)],m)))
    | _ ->  None

let mkUnitDelayLambda g m e =
    let uv,_ = mkCompGenLocal m "unitVar" g.unit_ty
    mkLambda m uv (e,tyOfExpr g e) 


//  <qexprInner> := 
//     | query.Select(<qexprInner>, <other-arguments>) --> Seq.map(qexprInner', ...)
//     | query.For(<qexprInner>, <other-arguments>)  --> IQueryable if qexprInner is IQueryable, otherwise Seq.collect(qexprInner', ...)
//     | query.Yield <expr>                          --> not IQueryable
//     | query.YieldFrom <qexpr>                     --> not IQueryable
//     | query.Op(<qexprInner>, <other-arguments>)   --> IQueryable if qexprInner is IQueryable, otherwise query.Op(qexprInner', <other-arguments>)   
//     | <qexprInner> :> seq<_>                      --> IQueryable if qexprInner is IQueryable
//
//  <qexprOuter> := 
//     | query.Select(<qexprInner>, <other-arguments>) --> IQueryable if qexprInner is IQueryable, otherwise seq { qexprInner' } 
//     | query.For(<qexprInner>, <other-arguments>)    --> IQueryable if qexprInner is IQueryable, otherwise seq { qexprInner' } 
//     | query.Yield <expr>                            --> not IQueryable, seq { <expr> } 
//     | query.YieldFrom <expr>                        --> not IQueryable, seq { yield! <expr> } 
//     | query.Op(<qexprOuter>, <other-arguments>)     --> IQueryable if qexprOuter is IQueryable, otherwise query.Op(qexpOuter', <other-arguments>)   

//printfn "found Query.Quote"
// If this returns "Some" then the source is not IQueryable.
let rec tryRewriteToSeqCombinators g (e: Expr) = 
    let m = e.Range
    match e with 
    //  query.Yield  --> Seq.singleton
    | QueryYield g (_, resultElemTy, vExpr) ->  Some (mkCallSeqSingleton g m resultElemTy vExpr)

    //  query.YieldFrom (query.Source s) --> s
    | QueryYieldFrom g (_, _, QuerySourceEnumerable g (_, resExpr)) -> Some resExpr

    //  query.Select --> Seq.map
    | QuerySelect g (_qTy, sourceElemTy, resultElemTy, source, resultSelector) -> 
   
        match tryRewriteToSeqCombinators g source with 
        | Some newSource -> Some (mkCallSeqMap g m sourceElemTy resultElemTy resultSelector newSource)
        | None -> None

    //  query.Zero -> Seq.empty
    | QueryZero g (_qTy, sourceElemTy) -> 
        Some (mkCallSeqEmpty g m sourceElemTy)

    //  query.For --> Seq.collect
    | QueryFor g (_qTy, sourceElemTy, resultElemTy, QuerySourceEnumerable g (_, source), Expr.Lambda(_,_,_,[resultSelectorVar],resultSelector,mLambda,_)) -> 
        match tryRewriteToSeqCombinators g resultSelector with
        | Some newResultSelector ->
            Some (mkCallSeqCollect g m sourceElemTy resultElemTy (mkLambda mLambda resultSelectorVar (newResultSelector, tyOfExpr g newResultSelector)) source)
        | _ -> None


    //  let --> let
    | Expr.Let(bind,bodyExpr,m,_) -> 
        match tryRewriteToSeqCombinators g bodyExpr with 
        | Some newBodyExpr ->    
            Some (Expr.Let(bind,newBodyExpr,m,newCache()))
        | None -> None

    // match --> match
    | Expr.Match (spBind,exprm,pt,targets,m,_ty) ->
        let targets = targets |> Array.map (fun (TTarget(vs,e,spTarget)) -> match tryRewriteToSeqCombinators g e with None -> None | Some e -> Some(TTarget(vs,e,spTarget)))
        if targets |> Array.forall isSome then 
            let targets = targets |> Array.map Option.get
            let ty = targets |> Array.pick (fun (TTarget(_,e,_)) -> Some(tyOfExpr g e))
            Some (Expr.Match (spBind,exprm,pt,targets,m,ty))
        else
            None

    | _ -> 
        None

    
// This detects forms arising from query expressions, i.e.
//    query.Run <@ query.Op(<seqSource>, <other-arguments>) @>  
//
// We check if the combinators are marked with tag IEnumerable - if do, we optimize the "Run" and quotation away, since RunQueryAsEnumerable simply performs
// an eval.


let TryDetectQueryQuoteAndRun cenv (expr:Expr) = 
    let g = cenv.g
    match expr with
    | QueryRun g (bodyOfRun, reqdResultInfo) -> 
        //printfn "found Query.Run"
        match bodyOfRun with 
        | Expr.Quote(quotedExpr,_,true,_,_) ->  // true = isFromQueryExpression


            // This traverses uses of query operators like query.Where and query.AverageBy until we're left with something familiar.
            // All these operators take the input IEnumerable 'seqSource' as the first argument.
            //
            // When we find the 'core' of the query expression, then if that is using IEnumerable execution, 
            // try to rewrite the core into combinators approximating the compiled form of seq { ... }, which in turn
            // are eligible for state-machine representation. If that fails, we still rewrite to combinator form.
            let rec loopOuter (e:Expr) = 
                match e with 

                | QueryFor g (qTy,_,resultElemTy,_,_)  
                | QuerySelect g (qTy,_,resultElemTy,_,_) 
                | QueryYield g (qTy,resultElemTy,_) 
                | QueryYieldFrom g (qTy,resultElemTy,_) 
                     when typeEquiv cenv.g qTy (mkAppTy cenv.g.tcref_System_Collections_IEnumerable []) -> 

                    match tryRewriteToSeqCombinators cenv.g e with 
                    | Some newSource -> 
                        //printfn "Eliminating because source is not IQueryable"
                        Some (mkCallSeq cenv.g newSource.Range resultElemTy (mkCallSeqDelay cenv.g newSource.Range resultElemTy (mkUnitDelayLambda cenv.g newSource.Range newSource) ), 
                              Some(resultElemTy, qTy) )
                    | None -> 
                        //printfn "Not compiling to state machines, but still optimizaing the use of quotations away"
                        Some (e, None)

                | AnyQueryBuilderOpTrans g (seqSource,replace) -> 
                    match loopOuter seqSource with
                    | Some (newSeqSource, newSeqSourceIsEnumerableInfo) -> 
                        let newSeqSourceAsQuerySource = 
                            match newSeqSourceIsEnumerableInfo with 
                            | Some (resultElemTy,qTy) -> mkCallNewQuerySource cenv.g newSeqSource.Range resultElemTy qTy newSeqSource 
                            | None -> newSeqSource
                        Some (replace newSeqSourceAsQuerySource, None)
                    | None -> None

                | _ -> 
                    None

            let resultExprInfo = loopOuter quotedExpr

            match resultExprInfo with
            | Some (resultExpr, exprIsEnumerableInfo) ->
                let resultExprAfterConvertToResultTy = 
                    match reqdResultInfo, exprIsEnumerableInfo with 
                    | Some _, Some _ | None, None -> resultExpr // the expression is a QuerySource, the result is a QuerySource, nothing to do
                    | Some resultElemTy, None -> mkCallGetQuerySourceAsEnumerable cenv.g expr.Range resultElemTy (TType_app(cenv.g.tcref_System_Collections_IEnumerable,[])) resultExpr
                    | None, Some (resultElemTy, qTy)  ->  mkCallNewQuerySource cenv.g expr.Range resultElemTy qTy resultExpr 
                Some resultExprAfterConvertToResultTy
            | None -> 
                None
                
                
        | _ -> 
            //printfn "Not eliminating because no Quote found"
            None
    | _ -> 
        //printfn "Not eliminating because no Run found"
        None
                

    
//-------------------------------------------------------------------------
// The traversal
//------------------------------------------------------------------------- 

let rec OptimizeExpr cenv (env:IncrementalOptimizationEnv) expr =

    // foreach --> fast integer for loops
    let expr = DetectFastIntegerForLoops cenv.g expr

    // Eliminate subsumption coercions for functions. This must be done post-typechecking because we need
    // complete inference types.
    let expr = NormalizeAndAdjustPossibleSubsumptionExprs cenv.g expr

    let expr = stripExpr expr

    match expr with
    // treat the common linear cases to avoid stack overflows, using an explicit continutation 
    | Expr.Sequential _ | Expr.Let _ ->  OptimizeLinearExpr cenv env expr (fun x -> x)

    | Expr.Const (c,m,ty) -> OptimizeConst cenv env expr (c,m,ty)
    | Expr.Val (v,_vFlags,m) -> OptimizeVal cenv env expr (v,m)
    | Expr.Quote(ast,splices,isFromQueryExpression,m,ty) -> 
          let splices = ref (splices.Value |> Option.map (map2Of3 (List.map (OptimizeExpr cenv env >> fst))))
          Expr.Quote(ast,splices,isFromQueryExpression,m,ty),
          { TotalSize = 10;
            FunctionSize = 1;
            HasEffect = false;  
            MightMakeCriticalTailcall=false;
            Info=UnknownValue }
    | Expr.Obj (_,typ,basev,expr,overrides,iimpls,m) -> OptimizeObjectExpr cenv env (typ,basev,expr,overrides,iimpls,m)
    | Expr.Op (c,tyargs,args,m) -> OptimizeExprOp cenv env (c,tyargs,args,m)
    | Expr.App(f,fty,tyargs,argsl,m) -> 
        // eliminate uses of query
        match TryDetectQueryQuoteAndRun cenv expr with 
        | Some newExpr -> OptimizeExpr cenv env newExpr
        | None -> OptimizeApplication cenv env (f,fty,tyargs,argsl,m) 
    (* REVIEW: fold the next two cases together *)
    | Expr.Lambda(_lambdaId,_,_,argvs,_body,m,rty) -> 
        let topValInfo = ValReprInfo ([],[argvs |> List.map (fun _ -> ValReprInfo.unnamedTopArg1)],ValReprInfo.unnamedRetVal)
        let ty = mkMultiLambdaTy m argvs rty
        OptimizeLambdas None cenv env topValInfo expr ty
    | Expr.TyLambda(_lambdaId,tps,_body,_m,rty)  -> 
        let topValInfo = ValReprInfo (ValReprInfo.InferTyparInfo tps,[],ValReprInfo.unnamedRetVal)
        let ty = tryMkForallTy tps rty
        OptimizeLambdas None cenv env topValInfo expr ty
    | Expr.TyChoose _  -> OptimizeExpr cenv env (Typrelns.ChooseTyparSolutionsForFreeChoiceTypars cenv.g cenv.amap expr)
    | Expr.Match(spMatch,exprm,dtree,targets,m,ty) -> OptimizeMatch cenv env (spMatch,exprm,dtree,targets,m,ty)
    | Expr.LetRec (binds,e,m,_) ->  OptimizeLetRec cenv env (binds,e,m)
    | Expr.StaticOptimization (constraints,e2,e3,m) ->
        let e2',e2info = OptimizeExpr cenv env e2
        let e3',e3info = OptimizeExpr cenv env e3
        Expr.StaticOptimization(constraints,e2',e3',m), 
        { TotalSize = min e2info.TotalSize e3info.TotalSize;
          FunctionSize = min e2info.FunctionSize e3info.FunctionSize;
          HasEffect = e2info.HasEffect || e3info.HasEffect;
          MightMakeCriticalTailcall=e2info.MightMakeCriticalTailcall || e3info.MightMakeCriticalTailcall // seems conservative
          Info= UnknownValue }
    | Expr.Link _eref -> 
        assert ("unexpected reclink" = "");
        failwith "Unexpected reclink"


//-------------------------------------------------------------------------
// Optimize/analyze an object expression
//------------------------------------------------------------------------- 

and OptimizeObjectExpr cenv env (typ,baseValOpt,basecall,overrides,iimpls,m) =
    if verboseOptimizations then dprintf "OptimizeObjectExpr\n";
    let basecall',basecallinfo = OptimizeExpr cenv env basecall
    let overrides',overrideinfos = OptimizeMethods cenv env baseValOpt overrides
    let iimpls',iimplsinfos = OptimizeInterfaceImpls cenv env baseValOpt iimpls
    let expr'=mkObjExpr(typ,baseValOpt,basecall',overrides',iimpls',m)
    expr', { TotalSize=closureTotalSize + basecallinfo.TotalSize + AddTotalSizes overrideinfos + AddTotalSizes iimplsinfos;
             FunctionSize=1 (* a newobj *) ;
             HasEffect=true;
             MightMakeCriticalTailcall=false; // creating an object is not a useful tailcall
             Info=UnknownValue}

//-------------------------------------------------------------------------
// Optimize/analyze the methods that make up an object expression
//------------------------------------------------------------------------- 

and OptimizeMethods cenv env baseValOpt l = OptimizeList (OptimizeMethod cenv env baseValOpt) l
and OptimizeMethod cenv env baseValOpt (TObjExprMethod(slotsig, attribs, tps, vs, e, m) as tmethod) = 
    if verboseOptimizations then dprintf "OptimizeMethod\n";
    let env = {env with latestBoundId=Some tmethod.Id; functionVal = None}
    let env = BindTypeVarsToUnknown tps env
    let env = BindInternalValsToUnknown cenv vs env
    let env = Option.foldBack (BindInternalValToUnknown cenv) baseValOpt env
    let e',einfo = OptimizeExpr cenv env e
    (* REVIEW: if we ever change this from being UnknownValue then we should call AbstractExprInfoByVars *)
    TObjExprMethod(slotsig,attribs,tps,vs,e',m),
    { TotalSize = einfo.TotalSize;
      FunctionSize = 0;
      HasEffect = false;
      MightMakeCriticalTailcall=false;
      Info=UnknownValue}

//-------------------------------------------------------------------------
// Optimize/analyze the interface implementations that form part of an object expression
//------------------------------------------------------------------------- 

and OptimizeInterfaceImpls cenv env baseValOpt l = OptimizeList (OptimizeInterfaceImpl cenv env baseValOpt) l
and OptimizeInterfaceImpl cenv env baseValOpt (ty,overrides) = 
    if verboseOptimizations then dprintf "OptimizeInterfaceImpl\n";
    let overrides',overridesinfos = OptimizeMethods cenv env baseValOpt overrides
    (ty, overrides'), 
    { TotalSize = AddTotalSizes overridesinfos;
      FunctionSize = 1;
      HasEffect = false;
      MightMakeCriticalTailcall=false;
      Info=UnknownValue}

//-------------------------------------------------------------------------
// Optimize/analyze an application of an intrinsic operator to arguments
//------------------------------------------------------------------------- 

and OptimizeExprOp cenv env (op,tyargs,args,m) =

    if verboseOptimizations then dprintf "OptimizeExprOp\n";
    (* Special cases *)
    match op,tyargs,args with 
    | TOp.Coerce,[toty;fromty],[e] -> 
        let e',einfo = OptimizeExpr cenv env e
        if typeEquiv cenv.g toty fromty then e',einfo 
        else 
          mkCoerceExpr(e',toty,m,fromty), 
          { TotalSize=einfo.TotalSize + 1;
            FunctionSize=einfo.FunctionSize + 1;
            HasEffect = true;  
            MightMakeCriticalTailcall=false;
            Info=UnknownValue }
    (* Handle these as special cases since mutables are allowed inside their bodies *)
    | TOp.While (spWhile,marker),_,[Expr.Lambda(_,_,_,[_],e1,_,_);Expr.Lambda(_,_,_,[_],e2,_,_)]  -> OptimizeWhileLoop cenv env (spWhile,marker,e1,e2,m) 
    | TOp.For(spStart,dir),_,[Expr.Lambda(_,_,_,[_],e1,_,_);Expr.Lambda(_,_,_,[_],e2,_,_);Expr.Lambda(_,_,_,[v],e3,_,_)]  -> OptimizeFastIntegerForLoop cenv env (spStart,v,e1,dir,e2,e3,m) 
    | TOp.TryFinally(spTry,spFinally),[resty],[Expr.Lambda(_,_,_,[_],e1,_,_); Expr.Lambda(_,_,_,[_],e2,_,_)] -> OptimizeTryFinally cenv env (spTry,spFinally,e1,e2,m,resty)
    | TOp.TryCatch(spTry,spWith),[resty],[Expr.Lambda(_,_,_,[_],e1,_,_); Expr.Lambda(_,_,_,[vf],ef,_,_); Expr.Lambda(_,_,_,[vh],eh,_,_)] -> OptimizeTryCatch cenv env (e1,vf,ef,vh,eh,m,resty,spTry,spWith)
    | TOp.TraitCall(traitInfo),[],args -> OptimizeTraitCall cenv env (traitInfo, args, m) 

   // This code hooks arr.Length. The idea is to ensure loops end up in the "same shape"as the forms of loops that the .NET JIT
   // guarantees to optimize.
  
    | TOp.ILCall (_,_,_,_,_,_,_,mref,_enclTypeArgs,_methTypeArgs,_tys),_,[arg]
        when (mref.EnclosingTypeRef.Scope.IsAssemblyRef &&
              mref.EnclosingTypeRef.Scope.AssemblyRef.Name = cenv.g.sysCcu.AssemblyName &&
              mref.EnclosingTypeRef.Name = "System.Array" &&
              mref.Name = "get_Length" &&
              isArray1DTy cenv.g (tyOfExpr cenv.g arg)) -> 
         OptimizeExpr cenv env (Expr.Op(TOp.ILAsm(i_ldlen,[cenv.g.int_ty]),[],[arg],m))


    // Empty IL instruction lists are used as casts in prim-types.fs. But we can get rid of them 
    // if the types match up. 
    | TOp.ILAsm([],[ty]),_,[a] when typeEquiv cenv.g (tyOfExpr cenv.g a) ty -> OptimizeExpr cenv env a

    // Handle multiplication and division with one and zero values.
    | TOp.ILAsm([AI_mul],_),_,[a;Expr.Const(c,_,_)]
    | TOp.ILAsm([AI_mul],_),_,[Expr.Const(c,_,_);a]
    | TOp.ILAsm([AI_div],_),_,[a;Expr.Const(c,_,_)] when equalsValueOne c -> OptimizeExpr cenv env a
    | TOp.ILAsm([AI_mul],_),_,[_;Expr.Const(c,_,_)] when equalsValueZero c -> OptimizeExpr cenv env args.[1]
    | TOp.ILAsm([AI_mul],_),_,[Expr.Const(c,_,_);_]
    | TOp.ILAsm([AI_div],_),_,[Expr.Const(c,_,_);_] when equalsValueZero c -> OptimizeExpr cenv env args.[0]

    | _ -> 
    (* Reductions *)
    let args',arginfos = OptimizeExprsThenConsiderSplits cenv env args
    let knownValue = 
        match op,arginfos with 
        | TOp.ValFieldGet (rf),[e1info] -> TryOptimizeRecordFieldGet cenv env (e1info,rf,tyargs,m) 
        | TOp.TupleFieldGet n,[e1info] -> TryOptimizeTupleFieldGet cenv env (e1info,tyargs,n,m)
        | TOp.UnionCaseFieldGet (cspec,n),[e1info] -> TryOptimizeUnionCaseGet cenv env (e1info,cspec,tyargs,n,m)
        | _ -> None
    match knownValue with 
    | Some valu -> 
        match TryOptimizeVal cenv env (false,valu,m)  with 
        | Some res -> OptimizeExpr cenv env res  (* discard e1 since guard ensures it has no effects *)
        | None -> OptimizeExprOpFallback cenv env (op,tyargs,args',m) arginfos valu
    | None -> OptimizeExprOpFallback cenv env (op,tyargs,args',m) arginfos UnknownValue


and OptimizeExprOpFallback cenv env (op,tyargs,args',m) arginfos valu =
    // The generic case - we may collect information, but the construction/projection doesn't disappear 
    let argsTSize = AddTotalSizes arginfos
    let argsFSize = AddFunctionSizes arginfos
    let argEffects = OrEffects arginfos
    let argValues = List.map (fun x -> x.Info) arginfos
    let effect = OpHasEffect cenv.g op
    let cost,valu = 
      match op with
      | TOp.UnionCase c -> 2,MakeValueInfoForUnionCase c (Array.ofList argValues)
      | TOp.ExnConstr _ -> 2,valu (* REVIEW: information collection possilbe here *)
      | TOp.Tuple       -> 1, MakeValueInfoForTuple (Array.ofList argValues)
      | TOp.ValFieldGet _     
      | TOp.TupleFieldGet _    
      | TOp.UnionCaseFieldGet _   
      | TOp.ExnFieldGet _
      | TOp.UnionCaseTagGet _      -> 
          // REVIEW: reduction possible here, and may be very effective
          1,valu 
      | TOp.UnionCaseProof _        -> 
          // We count the proof as size 0
          // We maintain the value of the source of the proof-cast if it is known to be a UnionCaseValue
          let valu = 
              match argValues.[0] with 
              | StripUnionCaseValue (uc,info) -> UnionCaseValue(uc,info) 
              | _ ->  valu
          0,valu
      | TOp.ILAsm(instrs,tys)         -> 
          min instrs.Length 1, 
          mkAssemblyCodeValueInfo cenv.g instrs argValues tys
      | TOp.Bytes bytes -> bytes.Length/10 , valu
      | TOp.UInt16s bytes -> bytes.Length/10 , valu
      | TOp.ValFieldGetAddr _     
      | TOp.Array | TOp.For _ | TOp.While _ | TOp.TryCatch _ | TOp.TryFinally _
      | TOp.ILCall _ | TOp.TraitCall _ | TOp.LValueOp _ | TOp.ValFieldSet _
      | TOp.UnionCaseFieldSet _ | TOp.RefAddrGet | TOp.Coerce | TOp.Reraise
      | TOp.ExnFieldSet _ -> 1,valu
      | TOp.Recd (ctorInfo,tcref) ->
          let finfos = tcref.AllInstanceFieldsAsList
          // REVIEW: this seems a little conservative: Allocating a record with a mutable field 
          // is not an effect - only reading or writing the field is. 
          let valu = 
              match ctorInfo with 
              | RecdExprIsObjInit -> UnknownValue
              | RecdExpr -> 
                   if argValues.Length <> finfos.Length then valu 
                   else MakeValueInfoForRecord tcref (Array.ofList ((argValues,finfos) ||> List.map2 (fun x f -> if f.IsMutable then UnknownValue else x) ))
          2,valu  
      | TOp.Goto _ | TOp.Label _ | TOp.Return -> assert false; error(InternalError("unexpected goto/label/return in optimization",m))

    // Indirect calls to IL code are always taken as tailcalls
    let mayBeCriticalTailcall = 
        match op with
        | TOp.ILCall (virt,_,newobj,_,_,_,_,_,_,_,_) -> not newobj && virt
        | _ -> false
    
    let vinfo = { TotalSize=argsTSize + cost;
                  FunctionSize=argsFSize + cost;
                  HasEffect=argEffects || effect;                  
                  MightMakeCriticalTailcall= mayBeCriticalTailcall; // discard tailcall info for args - these are not in tailcall position
                  Info=valu } 

    // Replace entire expression with known value? 
    match TryOptimizeValInfo cenv env m vinfo with 
    | Some res -> res,vinfo
    | None ->
          Expr.Op(op,tyargs,args',m),
          { TotalSize=argsTSize + cost;
            FunctionSize=argsFSize + cost;
            HasEffect=argEffects || effect;
            MightMakeCriticalTailcall= mayBeCriticalTailcall; // discard tailcall info for args - these are not in tailcall position
            Info=valu }

//-------------------------------------------------------------------------
// Optimize/analyze a constant node
//------------------------------------------------------------------------- 

and OptimizeConst cenv env expr (c,m,ty) = 
    match TryEliminateDesugaredConstants cenv.g m c with 
    | Some(e) -> 
        OptimizeExpr cenv env e
    | None ->
        if verboseOptimizations then dprintf "OptimizeConst\n";
        expr, { TotalSize=(match c with 
                           | Const.String b -> b.Length/10 
                           | _ -> 0);
                FunctionSize=0;
                HasEffect=false;
                MightMakeCriticalTailcall=false;
                Info=MakeValueInfoForConst c ty}

//-------------------------------------------------------------------------
// Optimize/analyze a record lookup. 
//------------------------------------------------------------------------- 

and TryOptimizeRecordFieldGet cenv _env (e1info,r:RecdFieldRef,_tinst,m) =
    match destRecdValue e1info.Info with
    | Some finfos when cenv.settings.EliminateRecdFieldGet() && not e1info.HasEffect ->
        let n = r.Index
        if n >= finfos.Length then errorR(InternalError( "TryOptimizeRecordFieldGet: term argument out of range",m));
        Some finfos.[n]   (* Uses INVARIANT on record ValInfos that exprs are in defn order *)
    | _ -> None
  
and TryOptimizeTupleFieldGet cenv _env (e1info,tys,n,m) =
    match destTupleValue e1info.Info with
    | Some tups when cenv.settings.EliminateTupleFieldGet() && not e1info.HasEffect ->
        let len = tups.Length 
        if len <> tys.Length then errorR(InternalError("error: tuple lengths don't match",m));
        if n >= len then errorR(InternalError("TryOptimizeTupleFieldGet: tuple index out of range",m));
        Some tups.[n]
    | _ -> None
      
and TryOptimizeUnionCaseGet cenv _env (e1info,cspec,_tys,n,m) =
    match e1info.Info with
    | StripUnionCaseValue(cspec2,args) when cenv.settings.EliminatUnionCaseFieldGet() && not e1info.HasEffect && cenv.g.unionCaseRefEq cspec cspec2 ->
        if n >= args.Length then errorR(InternalError( "TryOptimizeUnionCaseGet: term argument out of range",m));
        Some args.[n]
    | _ -> None

//-------------------------------------------------------------------------
// Optimize/analyze a for-loop
//------------------------------------------------------------------------- 

and OptimizeFastIntegerForLoop cenv env (spStart,v,e1,dir,e2,e3,m) =
    if verboseOptimizations then dprintf "OptimizeFastIntegerForLoop\n";
    let e1',e1info = OptimizeExpr cenv env e1 
    let e2',e2info = OptimizeExpr cenv env e2 
    let env = BindInternalValToUnknown cenv v env 
    let e3', e3info = OptimizeExpr cenv env e3 
    // Try to replace F#-style loops with C# style loops that recompute their bounds but which are compiled more efficiently by the JITs, e.g.
    //  F#  "for x = 0 to arre.Length - 1 do ..." --> C# "for (int x = 0; x < arre.Length; x++) { ... }"
    //  F#  "for x = 0 to 10 do ..." --> C# "for (int x = 0; x < 11; x++) { ... }"
    let e2', dir = 
        match dir, e2' with 
        // detect upwards for loops with bounds of the form "arr.Length - 1" and convert them to a C#-style for loop
        | FSharpForLoopUp, Expr.Op(TOp.ILAsm([ (AI_sub | AI_sub_ovf)],_),_,[Expr.Op(TOp.ILAsm([ I_ldlen; (AI_conv DT_I4)],_),_,[arre],_);
                                                                                   Expr.Const(Const.Int32 1,_,_)],_) 
                  when not (snd(OptimizeExpr cenv env arre)).HasEffect -> 

            mkLdlen cenv.g (e2'.Range) arre, CSharpForLoopUp

        // detect upwards for loops with constant bounds, but not MaxValue!
        | FSharpForLoopUp, Expr.Const(Const.Int32 n,_,_) 
                  when n < System.Int32.MaxValue -> 
            mkIncr cenv.g (e2'.Range) e2', CSharpForLoopUp

        | _ ->
            e2', dir
 
    let einfos = [e1info;e2info;e3info] 
    let eff = OrEffects einfos 
    (* neither bounds nor body has an effect, and loops always terminate, hence eliminate the loop *)
    if not eff then 
        mkUnit cenv.g m , { TotalSize=0; FunctionSize=0; HasEffect=false; MightMakeCriticalTailcall=false; Info=UnknownValue }
    else
        let expr' = mkFor cenv.g (spStart,v,e1',dir,e2',e3',m) 
        expr', { TotalSize=AddTotalSizes einfos + forAndWhileLoopSize;
                 FunctionSize=AddFunctionSizes einfos + forAndWhileLoopSize;
                 HasEffect=eff;
                 MightMakeCriticalTailcall=false;
                 Info=UnknownValue }

//-------------------------------------------------------------------------
// Optimize/analyze a set of recursive bindings
//------------------------------------------------------------------------- 

and OptimizeLetRec cenv env (binds,bodyExpr,m) =
    if verboseOptimizations then dprintf "OptimizeLetRec\n";
    let vs = binds |> FlatList.map (fun v -> v.Var) in 
    let env = BindInternalValsToUnknown cenv vs env 
    let binds',env = OptimizeBindings cenv true env binds 
    let bodyExpr',einfo = OptimizeExpr cenv env bodyExpr 
    // REVIEW: graph analysis to determine which items are unused 
    // Eliminate any unused bindings, as in let case 
    let binds'',bindinfos = 
        let fvs0 = freeInExpr CollectLocals bodyExpr' 
        let fvsN = FlatList.map (fst >> freeInBindingRhs CollectLocals) binds' 
        let fvs  = FlatList.fold unionFreeVars fvs0 fvsN 
        SplitValuesByIsUsedOrHasEffect cenv (fun () -> fvs.FreeLocals) binds'
    // Trim out any optimization info that involves escaping values 
    let evalue' = AbstractExprInfoByVars (FlatList.toList vs,[]) einfo.Info 
    // REVIEW: size of constructing new closures - should probably add #freevars + #recfixups here 
    let bodyExpr' = Expr.LetRec(binds'',bodyExpr',m,NewFreeVarsCache()) 
    let info = CombineValueInfos (einfo :: FlatList.toList bindinfos) evalue' 
    bodyExpr', info

//-------------------------------------------------------------------------
// Optimize/analyze a linear sequence of sequentioanl execution or 'let' bindings.
//------------------------------------------------------------------------- 

and OptimizeLinearExpr cenv env expr contf =
    if verboseOptimizations then dprintf "OptimizeLinearExpr\n";
    let expr = if cenv.settings.ExpandStructrualValues() then ExpandStructuralBinding cenv expr else expr 
    match expr with 
    | Expr.Sequential (e1,e2,flag,spSeq,m) -> 
      if verboseOptimizations then dprintf "OptimizeLinearExpr: seq\n";
      let e1',e1info = OptimizeExpr cenv env e1 
      OptimizeLinearExpr cenv env e2 (contf << (fun (e2',e2info) -> 
        if (flag = NormalSeq) && 
           // Always eliminate '(); expr' sequences, even in debug code, to ensure that 
           // conditional method calls don't leave a dangling breakpoint (see FSharp 1.0 bug 6034)
           (cenv.settings.EliminateSequential () || (match e1' with Expr.Const(Const.Unit,_,_) -> true | _ -> false)) && 
           not e1info.HasEffect then 
            e2', e2info
        else 
            Expr.Sequential(e1',e2',flag,spSeq,m),
            { TotalSize = e1info.TotalSize + e2info.TotalSize;
              FunctionSize = e1info.FunctionSize + e2info.FunctionSize;
              HasEffect = flag <> NormalSeq || e1info.HasEffect || e2info.HasEffect;
              MightMakeCriticalTailcall = (if flag = NormalSeq then e2info.MightMakeCriticalTailcall else  e1info.MightMakeCriticalTailcall || e2info.MightMakeCriticalTailcall)
              Info = UnknownValue (* can't propagate value: must access result of computation for its effects *) }))

    | Expr.Let (bind,body,m,_) ->  
      if verboseOptimizations then dprintf "OptimizeLinearExpr: let\n";
      let (bind',bindingInfo),env = OptimizeBinding cenv false env bind 
      OptimizeLinearExpr cenv env body (contf << (fun (body',bodyInfo) ->  
        // PERF: This call to ValueIsUsedOrHasEffect/freeInExpr amounts to 9% of all optimization time.
        // Is it quadratic or quasi-quadtratic?
        if ValueIsUsedOrHasEffect cenv (fun () -> (freeInExpr CollectLocals body').FreeLocals) (bind',bindingInfo) then
            (* Eliminate let bindings on the way back up *)
            let expr',adjust = TryEliminateLet cenv env  bind' body' m 
            expr',
            { TotalSize = bindingInfo.TotalSize + bodyInfo.TotalSize + adjust; 
              FunctionSize = bindingInfo.FunctionSize + bodyInfo.FunctionSize + adjust; 
              HasEffect=bindingInfo.HasEffect || bodyInfo.HasEffect;
              MightMakeCriticalTailcall = bodyInfo.MightMakeCriticalTailcall; // discard tailcall info from binding - not in tailcall position
              Info = UnknownValue }
        else 
            (* On the way back up: Trim out any optimization info that involves escaping values on the way back up *)
            let evalue' = AbstractExprInfoByVars ([bind'.Var],[]) bodyInfo.Info 
            body',
            { TotalSize = bindingInfo.TotalSize + bodyInfo.TotalSize - localVarSize (* eliminated a local var *); 
              FunctionSize = bindingInfo.FunctionSize + bodyInfo.FunctionSize - localVarSize (* eliminated a local var *); 
              HasEffect=bindingInfo.HasEffect || bodyInfo.HasEffect;
              MightMakeCriticalTailcall = bodyInfo.MightMakeCriticalTailcall; // discard tailcall info from binding - not in tailcall position
              Info = evalue' } ))

    | LinearMatchExpr (spMatch,exprm,dtree,tg1,e2,spTarget2,m,ty) ->
         let dtree,dinfo = OptimizeDecisionTree cenv env m dtree
         let tg1,tg1info = OptimizeDecisionTreeTarget cenv env m tg1
         // tailcall
         OptimizeLinearExpr cenv env e2 (contf << (fun (e2,e2info) ->
             let e2,e2info = ConsiderSplitToMethod cenv.settings.abstractBigTargets cenv.settings.bigTargetSize cenv env (e2,e2info) 
             let tinfos = [tg1info; e2info]
             let tgs = [tg1; TTarget([],e2,spTarget2)]
             RebuildOptimizedMatch (spMatch,exprm,m,ty,dtree,tgs,dinfo,tinfos)))

    | _ -> contf (OptimizeExpr cenv env expr)

//-------------------------------------------------------------------------
// Optimize/analyze a try/finally construct.
//------------------------------------------------------------------------- 
  
and OptimizeTryFinally cenv env (spTry,spFinally,e1,e2,m,ty) =
    if verboseOptimizations then dprintf "OptimizeTryFinally\n";
    let e1',e1info = OptimizeExpr cenv env e1 
    let e2',e2info = OptimizeExpr cenv env e2 
    let info = 
        { TotalSize = e1info.TotalSize + e2info.TotalSize + tryFinallySize;
          FunctionSize = e1info.FunctionSize + e2info.FunctionSize + tryFinallySize;
          HasEffect = e1info.HasEffect || e2info.HasEffect;
          MightMakeCriticalTailcall = false; // no tailcalls from inside in try/finally
          Info = UnknownValue } 
    (* try-finally, so no effect means no exception can be raised, so just sequence the finally *)
    if cenv.settings.EliminateTryCatchAndTryFinally () && not e1info.HasEffect then 
        let sp = 
            match spTry with 
            | SequencePointAtTry _ -> SequencePointsAtSeq 
            | SequencePointInBodyOfTry -> SequencePointsAtSeq 
            | NoSequencePointAtTry -> SuppressSequencePointOnExprOfSequential
        Expr.Sequential(e1',e2',ThenDoSeq,sp,m),info 
    else
        mkTryFinally cenv.g (e1',e2',m,ty,spTry,spFinally), 
        info

//-------------------------------------------------------------------------
// Optimize/analyze a try/catch construct.
//------------------------------------------------------------------------- 
  
and OptimizeTryCatch cenv env (e1,vf,ef,vh,eh,m,ty,spTry,spWith) =
    if verboseOptimizations then dprintf "OptimizeTryCatch\n";
    let e1',e1info = OptimizeExpr cenv env e1    
    // try-catch, so no effect means no exception can be raised, so discard the catch 
    if cenv.settings.EliminateTryCatchAndTryFinally () && not e1info.HasEffect then 
        e1',e1info 
    else
        let envinner = BindInternalValToUnknown cenv vf (BindInternalValToUnknown cenv vh env)
        let ef',efinfo = OptimizeExpr cenv envinner ef 
        let eh',ehinfo = OptimizeExpr cenv envinner eh 
        let info = 
            { TotalSize = e1info.TotalSize + efinfo.TotalSize+ ehinfo.TotalSize  + tryCatchSize;
              FunctionSize = e1info.FunctionSize + efinfo.FunctionSize+ ehinfo.FunctionSize  + tryCatchSize;
              HasEffect = e1info.HasEffect || efinfo.HasEffect || ehinfo.HasEffect;
              MightMakeCriticalTailcall = false;
              Info = UnknownValue } 
        mkTryWith cenv.g (e1',vf,ef',vh,eh',m,ty,spTry,spWith), 
        info

//-------------------------------------------------------------------------
// Optimize/analyze a while loop
//------------------------------------------------------------------------- 
  
and OptimizeWhileLoop cenv env  (spWhile,marker,e1,e2,m) =
    if verboseOptimizations then dprintf "OptimizeWhileLoop\n";
    let e1',e1info = OptimizeExpr cenv env e1 
    let e2',e2info = OptimizeExpr cenv env e2 
    mkWhile cenv.g (spWhile,marker,e1',e2',m), 
    { TotalSize = e1info.TotalSize + e2info.TotalSize + forAndWhileLoopSize;
      FunctionSize = e1info.FunctionSize + e2info.FunctionSize + forAndWhileLoopSize;
      HasEffect = true; (* may not terminate *)
      MightMakeCriticalTailcall = false;
      Info = UnknownValue }

//-------------------------------------------------------------------------
// Optimize/analyze a call to a 'member' constraint. Try to resolve the call to 
// a witness (should always be possible due to compulsory inlining of any
// code that contains calls to member constraints, except when analyzing 
// not-yet-inlined generic code)
//------------------------------------------------------------------------- 
 

and OptimizeTraitCall cenv env   (traitInfo, args, m) =

    // Resolve the static overloading early (during the compulsory rewrite phase) so we can inline. 
    match ConstraintSolver.CodegenWitnessThatTypSupportsTraitConstraint cenv.TcVal cenv.g cenv.amap m traitInfo args with

    | OkResult (_,Some expr) -> OptimizeExpr cenv env expr

    // Resolution fails when optimizing generic code, ignore the failure
    |  _ -> 
        let args',arginfos = OptimizeExprsThenConsiderSplits cenv env args 
        OptimizeExprOpFallback cenv env (TOp.TraitCall(traitInfo),[],args',m) arginfos UnknownValue 

//-------------------------------------------------------------------------
// Make optimization decisions once we know the optimization information
// for a value
//------------------------------------------------------------------------- 

and TryOptimizeVal cenv env (mustInline,valInfoForVal,m) = 
    match valInfoForVal with 
    // Inline all constants immediately 
    | ConstValue (c,ty) -> Some (Expr.Const (c,m,ty))
    | SizeValue (_,detail) -> TryOptimizeVal cenv env (mustInline,detail,m) 
    | ValValue (v',detail) -> 
         // Inline values bound to other values immediately 
         match  TryOptimizeVal cenv env (mustInline,detail,m) with 
          // Prefer to inline using the more specific info if possible 
          | Some e -> Some e
          //If the more specific info didn't reveal an inline then use the value 
          | None ->  Some(exprForValRef m v')
    | ConstExprValue(_size,expr) ->
        if verboseOptimizations then dprintf "Inlining constant expression value at %a\n"  outputRange m;
        Some (remarkExpr m (copyExpr cenv.g CloneAllAndMarkExprValsAsCompilerGenerated expr))
    | CurriedLambdaValue (_,_,_,expr,_) when mustInline ->
        if verboseOptimizations then dprintf "Inlining mustinline-lambda at %a\n"  outputRange m;
        Some (remarkExpr m (copyExpr cenv.g CloneAllAndMarkExprValsAsCompilerGenerated expr))
    | TupleValue _ | UnionCaseValue _ | RecdValue _ when mustInline -> failwith "tuple, union and record values cannot be marked 'inline'"
    | UnknownValue when mustInline -> warning(Error(FSComp.SR.optValueMarkedInlineHasUnexpectedValue(),m)); None
    | _ when mustInline -> warning(Error(FSComp.SR.optValueMarkedInlineCouldNotBeInlined(),m)); None
    | _ -> None 
  
and TryOptimizeValInfo cenv env m vinfo = 
    if vinfo.HasEffect then None else TryOptimizeVal cenv env (false,vinfo.Info ,m)

//-------------------------------------------------------------------------
// Add 'v1 = v2' information into the information stored about a value
//------------------------------------------------------------------------- 
  
and AddValEqualityInfo g m (v:ValRef) info =
    if v.IsMutable then 
        /// the env assumes known-values do not change 
        info 
    else 
        {info with Info= MakeValueInfoForValue g m v info.Info}

//-------------------------------------------------------------------------
// Optimize/analyze a use of a value
//------------------------------------------------------------------------- 

and OptimizeVal cenv env expr (v:ValRef,m) =
    let valInfoForVal = GetInfoForVal cenv env m v 

    match TryOptimizeVal cenv env (v.MustInline,valInfoForVal.ValExprInfo,m) with
    | Some e -> 
       // don't reoptimize inlined lambdas until they get applied to something
       match e with 
       | Expr.TyLambda _ 
       | Expr.Lambda _ ->
           e, (AddValEqualityInfo cenv.g m v 
                    { Info=valInfoForVal.ValExprInfo; 
                      HasEffect=false; 
                      MightMakeCriticalTailcall = false;
                      FunctionSize=10; 
                      TotalSize=10})
       | _ -> 
           let e,einfo = OptimizeExpr cenv env e 
           e,AddValEqualityInfo cenv.g m v einfo 

    | None -> 
       if v.MustInline  then error(Error(FSComp.SR.optFailedToInlineValue(v.DisplayName),m));
       expr,(AddValEqualityInfo cenv.g m v 
                    { Info=valInfoForVal.ValExprInfo; 
                      HasEffect=false; 
                      MightMakeCriticalTailcall = false;
                      FunctionSize=1; 
                      TotalSize=1})

//-------------------------------------------------------------------------
// Attempt to replace an application of a value by an alternative value.
//------------------------------------------------------------------------- 

and StripToNominalTyconRef cenv ty = 
    if isAppTy cenv.g ty then destAppTy cenv.g ty 
    elif isTupleTy cenv.g ty then 
        let tyargs = destTupleTy cenv.g ty
        mkCompiledTupleTyconRef cenv.g tyargs, tyargs 
    else failwith "StripToNominalTyconRef: unreachable" 
      

and CanDevirtualizeApplication cenv v vref ty args  = 
     valRefEq cenv.g v vref
     && not (isUnitTy cenv.g ty)
     && isAppTy cenv.g ty 
     // Exclusion: Some unions have null as representations 
     && not (IsUnionTypeWithNullAsTrueValue cenv.g (fst(StripToNominalTyconRef cenv ty)).Deref)  
     // If we de-virtualize an operation on structs then we have to take the address of the object argument
     // Hence we have to actually have the object argument available to us,
     && (not (isStructTy cenv.g ty) || nonNil args) 

and TakeAddressOfStructArgumentIfNeeded cenv (vref:ValRef) ty args m =
    if vref.IsInstanceMember && isStructTy cenv.g ty then 
        match args with 
        | objArg::rest -> 
            // REVIEW: we set NeverMutates. This is valid because we only ever use DevirtualizeApplication to transform 
            // known calls to known generated F# code for CompareTo, Equals and GetHashCode.
            // If we ever reuse DevirtualizeApplication to transform an arbitrary virtual call into a 
            // direct call then this assumption is not valid.
            let wrap,objArgAddress = mkExprAddrOfExpr cenv.g true false NeverMutates objArg None m
            wrap, (objArgAddress::rest)
        | _ -> 
            // no wrapper, args stay the same 
            (fun x -> x), args
    else
        (fun x -> x), args

and DevirtualizeApplication cenv env (vref:ValRef) ty tyargs args m =
    let wrap,args = TakeAddressOfStructArgumentIfNeeded cenv vref ty args m
    let transformedExpr = wrap (MakeApplicationAndBetaReduce cenv.g (exprForValRef m vref,vref.Type,(if isNil tyargs then [] else [tyargs]),args,m))
    OptimizeExpr cenv env transformedExpr

    
  
and TryDevirtualizeApplication cenv env (f,tyargs,args,m) =
    match f,tyargs,args with 

    // Optimize/analyze calls to LanguagePrimitives.HashCompare.GenericComparisonIntrinsic when type is known 
    // to be augmented with a visible comparison value. 
    //
    // e.g rewrite 
    //      'LanguagePrimitives.HashCompare.GenericComparisonIntrinsic (x:C) (y:C)' 
    //  --> 'x.CompareTo(y:C)' where this is a direct call to the implementation of CompareTo, i.e.
    //        C::CompareTo(C)
    //    not C::CompareTo(obj)
    //
    // If C is a struct type then we have to take the address of 'c'
    
    | Expr.Val(v,_,_),[ty],_ when CanDevirtualizeApplication cenv v cenv.g.generic_comparison_inner_vref ty args ->
         
        let tcref,tyargs = StripToNominalTyconRef cenv ty
        match tcref.GeneratedCompareToValues with 
        | Some (_,vref)  -> Some (DevirtualizeApplication cenv env vref ty tyargs args m)
        | _ -> None
        
    | Expr.Val(v,_,_),[ty],_ when CanDevirtualizeApplication cenv v cenv.g.generic_comparison_withc_inner_vref ty args ->
         
        let tcref,tyargs = StripToNominalTyconRef cenv ty
        match tcref.GeneratedCompareToWithComparerValues, args with 
        | Some vref, [comp; x; y]  -> 
            // the target takes a tupled argument, so we need to reorder the arg expressions in the
            // arg list, and create a tuple of y & comp
            // push the comparer to the end and box the argument
            let args2 = [x; mkTupledNoTypes cenv.g m [mkCoerceExpr(y,cenv.g.obj_ty,m,ty) ; comp]]
            Some (DevirtualizeApplication cenv env vref ty tyargs args2 m)
        | _ -> None
        
    // Optimize/analyze calls to LanguagePrimitives.HashCompare.GenericEqualityIntrinsic when type is known 
    // to be augmented with a visible equality-without-comparer value. 
    //   REVIEW: GenericEqualityIntrinsic (which has no comparer) implements PER semantics (5537: this should be ER semantics)
    //           We are devirtualizing to a Equals(T) method which also implements PER semantics (5537: this should be ER semantics)
    | Expr.Val(v,_,_),[ty],_ when CanDevirtualizeApplication cenv v cenv.g.generic_equality_er_inner_vref ty args ->
         
        let tcref,tyargs = StripToNominalTyconRef cenv ty 
        match tcref.GeneratedHashAndEqualsValues with 
        | Some (_,vref)  -> Some (DevirtualizeApplication cenv env vref ty tyargs args m)
        | _ -> None
        

    // Optimize/analyze calls to LanguagePrimitives.HashCompare.GenericEqualityWithComparerFast
    | Expr.Val(v,_,_),[ty],_ when CanDevirtualizeApplication cenv v cenv.g.generic_equality_withc_inner_vref ty args ->
        let tcref,tyargs = StripToNominalTyconRef cenv ty
        match tcref.GeneratedHashAndEqualsWithComparerValues, args with
        | Some (_,_,withcEqualsVal), [comp; x; y]  -> 
            // push the comparer to the end and box the argument
            let args2 = [x; mkTupledNoTypes cenv.g m [mkCoerceExpr(y,cenv.g.obj_ty,m,ty) ; comp]]
            Some (DevirtualizeApplication cenv env withcEqualsVal ty tyargs args2 m)
        | _ -> None 
      
    // Optimize/analyze calls to LanguagePrimitives.HashCompare.GenericEqualityWithComparer
    | Expr.Val(v,_,_),[ty],_ when CanDevirtualizeApplication cenv v cenv.g.generic_equality_per_inner_vref ty args  && not(isTupleTy cenv.g ty) ->
       let tcref,tyargs = StripToNominalTyconRef cenv ty
       match tcref.GeneratedHashAndEqualsWithComparerValues, args with
       | Some (_,_,withcEqualsVal), [x; y] -> 
           let args2 = [x; mkTupledNoTypes cenv.g m [mkCoerceExpr(y,cenv.g.obj_ty,m,ty); (mkCallGetGenericPEREqualityComparer cenv.g m)]]
           Some (DevirtualizeApplication cenv env withcEqualsVal ty tyargs args2 m)
       | _ -> None     
    
    // Optimize/analyze calls to LanguagePrimitives.HashCompare.GenericHashIntrinsic
    | Expr.Val(v,_,_),[ty],_ when CanDevirtualizeApplication cenv v cenv.g.generic_hash_inner_vref ty args ->
        let tcref,tyargs = StripToNominalTyconRef cenv ty
        match tcref.GeneratedHashAndEqualsWithComparerValues, args with
        | Some (_,withcGetHashCodeVal,_), [x] -> 
            let args2 = [x; mkCallGetGenericEREqualityComparer cenv.g m]
            Some (DevirtualizeApplication cenv env withcGetHashCodeVal ty tyargs args2 m)
        | _ -> None 
        
    // Optimize/analyze calls to LanguagePrimitives.HashCompare.GenericHashWithComparerIntrinsic
    | Expr.Val(v,_,_),[ty],_ when  CanDevirtualizeApplication cenv v cenv.g.generic_hash_withc_inner_vref ty args ->
        let tcref,tyargs = StripToNominalTyconRef cenv ty
        match tcref.GeneratedHashAndEqualsWithComparerValues, args with
        | Some (_,withcGetHashCodeVal,_), [comp; x]  -> 
            let args2 = [x; comp]
            Some (DevirtualizeApplication cenv env withcGetHashCodeVal ty tyargs args2 m)
        | _ -> None 

    // Optimize/analyze calls to LanguagePrimitives.HashCompare.GenericComparisonWithComparerIntrinsic for tuple types
    | Expr.Val(v,_,_),[ty],_ when  valRefEq cenv.g v cenv.g.generic_comparison_inner_vref && isTupleTy cenv.g ty ->
        let tyargs = destTupleTy cenv.g ty 
        let vref = 
            match tyargs.Length with 
            | 2 -> Some cenv.g.generic_compare_withc_tuple2_vref 
            | 3 -> Some cenv.g.generic_compare_withc_tuple3_vref 
            | 4 -> Some cenv.g.generic_compare_withc_tuple4_vref 
            | 5 -> Some cenv.g.generic_compare_withc_tuple5_vref 
            | _ -> None
        match vref with 
        | Some vref -> Some (DevirtualizeApplication cenv env vref ty tyargs (mkCallGetGenericComparer cenv.g m :: args) m)            
        | None -> None
        
    // Optimize/analyze calls to LanguagePrimitives.HashCompare.GenericHashWithComparerIntrinsic for tuple types
    | Expr.Val(v,_,_),[ty],_ when  valRefEq cenv.g v cenv.g.generic_hash_inner_vref && isTupleTy cenv.g ty ->
        let tyargs = destTupleTy cenv.g ty 
        let vref = 
            match tyargs.Length with 
            | 2 -> Some cenv.g.generic_hash_withc_tuple2_vref 
            | 3 -> Some cenv.g.generic_hash_withc_tuple3_vref 
            | 4 -> Some cenv.g.generic_hash_withc_tuple4_vref 
            | 5 -> Some cenv.g.generic_hash_withc_tuple5_vref 
            | _ -> None
        match vref with 
        | Some vref -> Some (DevirtualizeApplication cenv env vref ty tyargs (mkCallGetGenericEREqualityComparer cenv.g m :: args) m)            
        | None -> None

        
    // Optimize/analyze calls to LanguagePrimitives.HashCompare.GenericEqualityIntrinsic for tuple types
    //  REVIEW (5537): GenericEqualityIntrinsic implements PER semantics, and we are replacing it to something also
    //                 implementing PER semantics. However GenericEqualityIntrinsic should implement ER semantics.
    | Expr.Val(v,_,_),[ty],_ when  valRefEq cenv.g v cenv.g.generic_equality_per_inner_vref && isTupleTy cenv.g ty ->
        let tyargs = destTupleTy cenv.g ty 
        let vref = 
            match tyargs.Length with 
            | 2 -> Some cenv.g.generic_equals_withc_tuple2_vref 
            | 3 -> Some cenv.g.generic_equals_withc_tuple3_vref 
            | 4 -> Some cenv.g.generic_equals_withc_tuple4_vref 
            | 5 -> Some cenv.g.generic_equals_withc_tuple5_vref 
            | _ -> None
        match vref with 
        | Some vref -> Some (DevirtualizeApplication cenv env vref ty tyargs (mkCallGetGenericPEREqualityComparer cenv.g m :: args) m)            
        | None -> None
        
    // Optimize/analyze calls to LanguagePrimitives.HashCompare.GenericComparisonWithComparerIntrinsic for tuple types
    | Expr.Val(v,_,_),[ty],_ when  valRefEq cenv.g v cenv.g.generic_comparison_withc_inner_vref && isTupleTy cenv.g ty ->
        let tyargs = destTupleTy cenv.g ty 
        let vref = 
            match tyargs.Length with 
            | 2 -> Some cenv.g.generic_compare_withc_tuple2_vref 
            | 3 -> Some cenv.g.generic_compare_withc_tuple3_vref 
            | 4 -> Some cenv.g.generic_compare_withc_tuple4_vref 
            | 5 -> Some cenv.g.generic_compare_withc_tuple5_vref 
            | _ -> None
        match vref with 
        | Some vref -> Some (DevirtualizeApplication cenv env vref ty tyargs args m)            
        | None -> None
        
    // Optimize/analyze calls to LanguagePrimitives.HashCompare.GenericHashWithComparerIntrinsic for tuple types
    | Expr.Val(v,_,_),[ty],_ when  valRefEq cenv.g v cenv.g.generic_hash_withc_inner_vref && isTupleTy cenv.g ty ->
        let tyargs = destTupleTy cenv.g ty 
        let vref = 
            match tyargs.Length with 
            | 2 -> Some cenv.g.generic_hash_withc_tuple2_vref 
            | 3 -> Some cenv.g.generic_hash_withc_tuple3_vref 
            | 4 -> Some cenv.g.generic_hash_withc_tuple4_vref 
            | 5 -> Some cenv.g.generic_hash_withc_tuple5_vref 
            | _ -> None
        match vref with 
        | Some vref -> Some (DevirtualizeApplication cenv env vref ty tyargs args m)            
        | None -> None
        
    // Optimize/analyze calls to LanguagePrimitives.HashCompare.GenericEqualityWithComparerIntrinsic for tuple types
    | Expr.Val(v,_,_),[ty],_ when  valRefEq cenv.g v cenv.g.generic_equality_withc_inner_vref && isTupleTy cenv.g ty ->
        let tyargs = destTupleTy cenv.g ty 
        let vref = 
            match tyargs.Length with 
            | 2 -> Some cenv.g.generic_equals_withc_tuple2_vref 
            | 3 -> Some cenv.g.generic_equals_withc_tuple3_vref 
            | 4 -> Some cenv.g.generic_equals_withc_tuple4_vref 
            | 5 -> Some cenv.g.generic_equals_withc_tuple5_vref 
            | _ -> None
        match vref with 
        | Some vref -> Some (DevirtualizeApplication cenv env vref ty tyargs args m)            
        | None -> None
        

    // Calls to LanguagePrimitives.IntrinsicFunctions.UnboxGeneric can be optimized to calls to UnboxFast when we know that the 
    // target type isn't 'NullNotLiked', i.e. that the target type is not an F# union, record etc. 
    // Note UnboxFast is just the .NET IL 'unbox.any' instruction. 
    | Expr.Val(v,_,_),[ty],_ when valRefEq cenv.g v cenv.g.unbox_vref && 
                                   canUseUnboxFast cenv.g ty ->

        Some(DevirtualizeApplication cenv env cenv.g.unbox_fast_vref ty tyargs args m)
        
    // Calls to LanguagePrimitives.IntrinsicFunctions.TypeTestGeneric can be optimized to calls to TypeTestFast when we know that the 
    // target type isn't 'NullNotTrueValue', i.e. that the target type is not an F# union, record etc. 
    // Note TypeTestFast is just the .NET IL 'isinst' instruction followed by a non-null comparison 
    | Expr.Val(v,_,_),[ty],_ when valRefEq cenv.g v cenv.g.istype_vref && 
                                   canUseTypeTestFast cenv.g ty ->

        Some(DevirtualizeApplication cenv env cenv.g.istype_fast_vref ty tyargs args m)
        
    // Don't fiddle with 'methodhandleof' calls - just remake the application
    | Expr.Val(vref,_,_),_,_ when valRefEq cenv.g vref cenv.g.methodhandleof_vref ->
        Some( MakeApplicationAndBetaReduce cenv.g (exprForValRef m vref,vref.Type,(if isNil tyargs then [] else [tyargs]),args,m),
              { TotalSize=1;
                FunctionSize=1
                HasEffect=false;
                MightMakeCriticalTailcall = false;
                Info=UnknownValue})

    | _ -> None

/// Attempt to inline an application of a known value at callsites
and TryInlineApplication cenv env (_f0',finfo) (tyargs: TType list,args: Expr list,m) =
    if verboseOptimizations then dprintf "Considering inlining app near %a\n"  outputRange m; 
    match finfo.Info with 
    | StripLambdaValue (lambdaId,arities,size,f2,f2ty) when

       (if verboseOptimizations then dprintf "Considering inlining lambda near %a, size = %d, finfo.HasEffect = %b\n"  outputRange m size finfo.HasEffect;
        cenv.optimizing &&
        cenv.settings.InlineLambdas () &&
        not finfo.HasEffect &&
        // Don't inline recursively! 
        not (Zset.contains lambdaId env.dontInline) &&
        (// Check the number of argument groups is enough to saturate the lambdas of the target. 
         (if tyargs |> List.filter (fun t -> match t with TType_measure _ -> false | _ -> true) |> isNil then 0 else 1) + args.Length = arities &&
          (if verboseOptimizations then dprintn "Enough args"; 
           (if size > cenv.settings.lambdaInlineThreshold + args.Length then
              if verboseOptimizations then dprintf "Not inlining lambda near %a because size = %d\n" outputRange m size; 
              false
            else true)))) ->
            
        let isBaseCall =  args.Length > 0 &&          
                              match args.[0] with
                              | Expr.Val(vref,_,_) when vref.BaseOrThisInfo = BaseVal -> true
                              | _ -> false

        // Since Lazy`1 moved from FSharp.Core to mscorlib on .NET 4.0, inlining Lazy values from 2.0 will
        // confuse the optimizer if the assembly is referenced on 4.0, since there will be no value to tie back
        // to FSharp.Core                              
        let isValFromLazyExtensions =
            if cenv.g.compilingFslib then
                false
            else
                match finfo.Info with
                | ValValue(vref,_) ->
                    match vref.ApparentParent with
                    | Parent(tcr) when (tyconRefEq cenv.g cenv.g.lazy_tcr_canon tcr) ->
                            match tcr.CompiledRepresentation with
                            | CompiledTypeRepr.ILAsmNamed(iltr,_,_) -> iltr.Scope.AssemblyRef.Name = "FSharp.Core"
                            | _ -> false
                    | _ -> false
                | _ -> false                                          
                              
        let isSecureMethod =
          match finfo.Info with
          |  ValValue(vref,_) ->
                vref.Attribs |> List.exists (fun a -> (IsSecurityAttribute cenv.g cenv.amap cenv.casApplied a m) || (IsSecurityCriticalAttribute cenv.g a))
          | _ -> false                              

        if isBaseCall || isSecureMethod || isValFromLazyExtensions then None
        else

        if verboseOptimizations then dprintf "Inlining lambda near %a\n"  outputRange m;
  (* ----------       printf "Inlining lambda near %a = %s\n"  outputRange m (showL (exprL f2));  (* JAMES: *) ----------*)
        let f2' = remarkExpr m (copyExpr cenv.g CloneAllAndMarkExprValsAsCompilerGenerated f2)
        if verboseOptimizations then dprintf "--- TryInlineApplication, optimizing arguments\n";

        // REVIEW: this is a cheapshot way of optimizing the arg expressions as well without the restriction of recursive  
        // inlining kicking into effect 
        let args' = args |> List.map (fun e -> let e',_einfo = OptimizeExpr cenv env e in e') 
        // Beta reduce. MakeApplicationAndBetaReduce cenv.g does all the hard work. 
        if verboseOptimizations then dprintf "--- TryInlineApplication, beta reducing \n";
        let expr' = MakeApplicationAndBetaReduce cenv.g (f2',f2ty,[tyargs],args',m)
        if verboseOptimizations then dprintf "--- TryInlineApplication, reoptimizing\n";
        Some (OptimizeExpr cenv {env with dontInline= Zset.add lambdaId env.dontInline} expr')
          
    | _ -> None

//-------------------------------------------------------------------------
// Optimize/analyze an application of a function to type and term arguments
//------------------------------------------------------------------------- 

and OptimizeApplication cenv env (f0,f0ty,tyargs,args,m) =
    if verboseOptimizations then dprintf "--> OptimizeApplication\n";
    let f0',finfo = OptimizeExpr cenv env f0 
    if verboseOptimizations then dprintf "--- OptimizeApplication, trying to devirtualize\n";
    match TryDevirtualizeApplication cenv env (f0,tyargs,args,m) with 
    | Some res -> 
        if verboseOptimizations then dprintf "<-- OptimizeApplication, devirtualized\n";
        res
    | None -> 

    match TryInlineApplication cenv env (f0',finfo) (tyargs,args,m) with 
    | Some res -> 
        if verboseOptimizations then dprintf "<-- OptimizeApplication, inlined\n";
        res
    | None -> 

    let shapes = 
        match f0' with 
        | Expr.Val(vref,_,_) when isSome vref.ValReprInfo -> 
            let (ValReprInfo(_kinds,detupArgsL,_)) = Option.get vref.ValReprInfo 
            let nargs = (args.Length) 
            let nDetupArgsL = detupArgsL.Length
            let nShapes = min nargs nDetupArgsL 
            let detupArgsShapesL = 
                List.take  nShapes detupArgsL |> List.map (fun detupArgs -> 
                    match detupArgs with 
                    | [] | [_] -> UnknownValue
                    | _ -> TupleValue(Array.ofList (List.map (fun _ -> UnknownValue) detupArgs))) 
            detupArgsShapesL @ List.replicate (nargs - nShapes) UnknownValue
            
        | _ -> args |> List.map (fun _ -> UnknownValue) 

    let args',arginfos = OptimizeExprsThenReshapeAndConsiderSplits cenv env (List.zip shapes args) 
    if verboseOptimizations then dprintf "<-- OptimizeApplication, beta reducing\n";
    let expr' = MakeApplicationAndBetaReduce cenv.g (f0',f0ty, [tyargs],args',m) 
    
    match f0', expr' with 
    | (Expr.Lambda _ | Expr.TyLambda _), Expr.Let _ -> 
       // we beta-reduced, hence reoptimize 
        if verboseOptimizations then dprintf "<-- OptimizeApplication, beta reduced\n";
        OptimizeExpr cenv env expr'
    | _ -> 
        if verboseOptimizations then dprintf "<-- OptimizeApplication, regular\n";

        // Determine if this application is a critical tailcall
        let mayBeCriticalTailcall = 
            match f0' with 
            | KnownValApp(vref,_typeArgs,otherArgs)   ->

                 // Check if this is a call to a function of known arity that has been inferred to not be a critical tailcall when used as a direct call
                 // This includes recursive calls to the function being defined (in which case we get a non-critical, closed-world tailcall).
                 // Note we also have to check the argument count to ensure this is a direct call (or a partial application).
                 let doesNotMakeCriticalTailcall = 
                     vref.MakesNoCriticalTailcalls || 
                     (let valInfoForVal = GetInfoForVal cenv env m vref  in valInfoForVal.ValMakesNoCriticalTailcalls) ||
                     (match env.functionVal with | None -> false | Some (v,_) -> valEq vref.Deref v)
                 if doesNotMakeCriticalTailcall then
                    let numArgs = otherArgs.Length + args'.Length
                    match vref.ValReprInfo with 
                    | Some i -> numArgs > i.NumCurriedArgs 
                    | None -> 
                    match env.functionVal with 
                    | Some (_v,i) ->  numArgs > i.NumCurriedArgs
                    | None -> true // over-applicaiton of a known function, which presumably returns a function. This counts as an indirect call
                 else
                    true // application of a function that may make a critical tailcall
                
            | _ -> 
                // All indirect calls (calls to unknown functions) are assumed to be critical tailcalls 
                true

        expr', { TotalSize=finfo.TotalSize + AddTotalSizes arginfos;
                 FunctionSize=finfo.FunctionSize + AddFunctionSizes arginfos;
                 HasEffect=true;
                 MightMakeCriticalTailcall = mayBeCriticalTailcall;
                 Info=ValueOfExpr expr' }

//-------------------------------------------------------------------------
// Optimize/analyze a lambda expression
//------------------------------------------------------------------------- 
        
and OptimizeLambdas (vspec: Val option) cenv env topValInfo e ety = 
    if verboseOptimizations then dprintf "OptimizeLambdas, #argsl = %d, %a\n" topValInfo.NumCurriedArgs outputRange (e.Range) ;
    match e with 
    | Expr.Lambda (lambdaId,_,_,_,_,m,_)  
    | Expr.TyLambda(lambdaId,_,_,m,_) ->
        let isTopLevel = isSome vspec && vspec.Value.IsCompiledAsTopLevel
        let tps,ctorThisValOpt,baseValOpt,vsl,body,bodyty = IteratedAdjustArityOfLambda cenv.g cenv.amap topValInfo e
        let env = { env with functionVal = (match vspec with None -> None | Some v -> Some (v,topValInfo)) }
        let env = Option.foldBack (BindInternalValToUnknown cenv) ctorThisValOpt env
        let env = Option.foldBack (BindInternalValToUnknown cenv) baseValOpt env
        let env = BindTypeVarsToUnknown tps env
        let env = List.foldBack (BindInternalValsToUnknown cenv) vsl env
        let env = BindInternalValsToUnknown cenv (Option.toList baseValOpt) env
        let body',bodyinfo = OptimizeExpr cenv env body
        let expr' = mkMemberLambdas m tps ctorThisValOpt baseValOpt vsl (body',bodyty)
        let arities = vsl.Length
        let arities = if isNil tps then arities else 1+arities
        let bsize = bodyinfo.TotalSize
        if verboseOptimizations then dprintf "lambda @ %a, bsize = %d\n" outputRange m bsize;

        
        /// Set the flag on the value indicating that direct calls can avoid a tailcall (which are expensive on .NET x86)
        /// MightMakeCriticalTailcall is true whenever the body of the method may itself do a useful tailcall, e.g. has
        /// an application in the last position.
        match vspec with 
        | Some v -> 
            if not bodyinfo.MightMakeCriticalTailcall then 
                v.SetMakesNoCriticalTailcalls() 
            
            // UNIT TEST HOOK: report analysis results for the first optimization phase 
            if cenv.settings.reportingPhase && not v.IsCompilerGenerated then 
                if cenv.settings.reportNoNeedToTailcall then 
                    if bodyinfo.MightMakeCriticalTailcall then
                        printfn "value %s at line %d may make a critical tailcall" v.DisplayName v.Range.StartLine 
                    else 
                        printfn "value %s at line %d does not make a critical tailcall" v.DisplayName v.Range.StartLine 
                if cenv.settings.reportTotalSizes then 
                    printfn "value %s at line %d has total size %d" v.DisplayName v.Range.StartLine  bodyinfo.TotalSize 
                if cenv.settings.reportFunctionSizes then 
                    printfn "value %s at line %d has method size %d" v.DisplayName v.Range.StartLine  bodyinfo.FunctionSize
                if cenv.settings.reportHasEffect then 
                    if bodyinfo.HasEffect then
                        printfn "function %s at line %d causes side effects or may not terminate" v.DisplayName v.Range.StartLine 
                    else 
                        printfn "function %s at line %d causes no side effects" v.DisplayName v.Range.StartLine 
        | _ -> 
            () 

        // can't inline any values with semi-recursive object references to self or base 
        let valu =   
          match baseValOpt with 
          | None -> CurriedLambdaValue (lambdaId,arities,bsize,expr',ety) 
          | _ -> UnknownValue

        expr', { TotalSize=bsize + (if isTopLevel then methodDefnTotalSize else closureTotalSize); (* estimate size of new syntactic closure - expensive, in contrast to a method *)
                 FunctionSize=1; 
                 HasEffect=false;
                 MightMakeCriticalTailcall = false;
                 Info= valu; }
    | _ -> OptimizeExpr cenv env e 
      


//-------------------------------------------------------------------------
// Recursive calls that first try to make an expression "fit" the a shape
// where it is about to be consumed.
//------------------------------------------------------------------------- 

and OptimizeExprsThenReshapeAndConsiderSplits cenv env exprs = 
    match exprs with 
    | [] -> NoExprs 
    | _ -> OptimizeList (OptimizeExprThenReshapeAndConsiderSplit cenv env) exprs

and OptimizeExprsThenConsiderSplits cenv env exprs = 
    match exprs with 
    | [] -> NoExprs 
    | _ -> OptimizeList (OptimizeExprThenConsiderSplit cenv env) exprs

and OptimizeFlatExprsThenConsiderSplits cenv env (exprs:FlatExprs) = 
    if FlatList.isEmpty exprs then NoFlatExprs
    else OptimizeFlatList (OptimizeExprThenConsiderSplit cenv env) exprs

and OptimizeExprThenReshapeAndConsiderSplit cenv env (shape,e) = 
    OptimizeExprThenConsiderSplit cenv env (ReshapeExpr cenv (shape,e))

and OptimizeDecisionTreeTargets cenv env m targets = 
    OptimizeList (OptimizeDecisionTreeTarget cenv env m) (Array.toList targets)

and ReshapeExpr cenv (shape,e) = 
  match shape,e with 
  | TupleValue(subshapes), Expr.Val(_vref,_vFlags,m) ->
      let tinst = destTupleTy cenv.g (tyOfExpr cenv.g e)
      mkTupled cenv.g m (List.mapi (fun i subshape -> ReshapeExpr cenv (subshape,mkTupleFieldGet(e,tinst,i,m))) (Array.toList subshapes)) tinst
  | _ ->  
      e

and OptimizeExprThenConsiderSplit cenv env e = 
  let e',einfo = OptimizeExpr cenv env e
  // ALWAYS consider splits for enormous sub terms here - otherwise we will create invalid .NET programs  
  ConsiderSplitToMethod true cenv.settings.veryBigExprSize cenv env (e',einfo) 

//-------------------------------------------------------------------------
// Decide whether to List.unzip a sub-expression into a new method
//------------------------------------------------------------------------- 

and ComputeSplitToMethodCondition flag threshold cenv env (e,einfo) = 
    flag &&
    // REVIEW: The method splitting optimization is compeltely disabled if we are not taking tailcalls.
    // REVIEW: This should only apply to methods that actually make self-tailcalls (tested further below).
    // Old comment "don't mess with taking guaranteed tailcalls if used with --no-tailcalls!" 
    cenv.emitTailcalls &&
    einfo.FunctionSize >= threshold &&

     // We can only split an expression out as a method if certain conditions are met. 
     // It can't use any protected or base calls, rethrow(), byrefs etc.
    (let fvs = freeInExpr CollectLocals e
     not fvs.UsesUnboundRethrow  &&
     not fvs.UsesMethodLocalConstructs &&
     fvs.FreeLocals |> Zset.forall (fun v -> 
          // no direct-self-recursive refrences
          not (env.dontSplitVars.ContainsVal v) &&
          (v.ValReprInfo.IsSome ||
            // All the free variables (apart from things with an arity, i.e. compiled as methods) should be normal, i.e. not base/this etc. 
            (v.BaseOrThisInfo = NormalVal && 
             // None of them should be byrefs 
             not (isByrefLikeTy cenv.g v.Type) && 
             //  None of them should be local polymorphic constrained values 
             not (IsGenericValWithGenericContraints cenv.g v) &&
             // None of them should be mutable 
             not v.IsMutable))))

and ConsiderSplitToMethod flag threshold cenv env (e,einfo) = 
    if ComputeSplitToMethodCondition flag threshold cenv env (e,einfo) then
        let m = (e.Range)
        let uv,_ue = mkCompGenLocal m "unitVar" cenv.g.unit_ty
        let ty = tyOfExpr cenv.g e
        let nm = 
            match env.latestBoundId with 
            | Some id -> id.idText^suffixForVariablesThatMayNotBeEliminated 
            | None -> suffixForVariablesThatMayNotBeEliminated 
        let fv,fe = mkCompGenLocal m nm (cenv.g.unit_ty --> ty)
        mkInvisibleLet m fv (mkLambda m uv (e,ty)) 
          (primMkApp (fe,(cenv.g.unit_ty --> ty)) [] [mkUnit cenv.g m] m),
        {einfo with FunctionSize=callSize }
    else
        e,einfo 

//-------------------------------------------------------------------------
// Optimize/analyze a pattern matching expression
//------------------------------------------------------------------------- 

and OptimizeMatch cenv env (spMatch,exprm,dtree,targets,m, ty) =
    if verboseOptimizations then dprintf "OptimizeMatch\n";
    // REVIEW: consider collecting, merging and using information flowing through each line of the decision tree to each target 
    let dtree',dinfo = OptimizeDecisionTree cenv env m dtree 
    let targets',tinfos = OptimizeDecisionTreeTargets cenv env m targets 
    RebuildOptimizedMatch (spMatch,exprm,m,ty,dtree',targets',dinfo,tinfos)

and CombineMatchInfos dinfo tinfo = 
    { TotalSize = dinfo.TotalSize + tinfo.TotalSize;
      FunctionSize = dinfo.FunctionSize + tinfo.FunctionSize;
      HasEffect = dinfo.HasEffect || tinfo.HasEffect;
      MightMakeCriticalTailcall=tinfo.MightMakeCriticalTailcall; // discard tailcall info from decision tree since it's not in tailcall position
      Info= UnknownValue }

and RebuildOptimizedMatch (spMatch,exprm,m,ty,dtree,tgs,dinfo,tinfos) = 
     let tinfo = CombineValueInfosUnknown tinfos
     let expr = mkAndSimplifyMatch spMatch exprm m ty dtree tgs
     let einfo = CombineMatchInfos dinfo tinfo
     expr, einfo

//-------------------------------------------------------------------------
// Optimize/analyze a target of a decision tree
//------------------------------------------------------------------------- 

and OptimizeDecisionTreeTarget cenv env _m (TTarget(vs,e,spTarget)) = 
    if verboseOptimizations then dprintf "OptimizeDecisionTreeTarget\n";
    (* REVIEW: this is where we should be using information collected for each target *)
    let env = BindInternalValsToUnknown cenv vs env 
    let e',einfo = OptimizeExpr cenv env e 
    let e',einfo = ConsiderSplitToMethod cenv.settings.abstractBigTargets cenv.settings.bigTargetSize cenv env (e',einfo) 
    let evalue' = AbstractExprInfoByVars (FlatList.toList vs,[]) einfo.Info 
    TTarget(vs,e',spTarget),
    { TotalSize=einfo.TotalSize; 
      FunctionSize=einfo.FunctionSize;
      HasEffect=einfo.HasEffect;
      MightMakeCriticalTailcall = einfo.MightMakeCriticalTailcall; 
      Info=evalue' }

//-------------------------------------------------------------------------
// Optimize/analyze a decision tree
//------------------------------------------------------------------------- 

and OptimizeDecisionTree cenv env m x =
    match x with 
    | TDSuccess (es,n) -> 
        let es', einfos = OptimizeFlatExprsThenConsiderSplits cenv env es 
        TDSuccess(es',n),CombineFlatValueInfosUnknown einfos
    | TDBind(bind,rest) -> 
        let (bind,binfo),envinner = OptimizeBinding cenv false env bind 
        let rest,rinfo = OptimizeDecisionTree cenv envinner m rest 

        if ValueIsUsedOrHasEffect cenv (fun () -> (accFreeInDecisionTree CollectLocals rest emptyFreeVars).FreeLocals) (bind,binfo) then

            let info = CombineValueInfosUnknown [rinfo;binfo]
            // try to fold the let-binding into a single result expression
            match rest with 
            | TDSuccess(es,n) when es.Length = 1 -> 
                let e = es.[0]
                let e,_adjust = TryEliminateLet cenv env bind e m 
                TDSuccess(FlatList.one e,n),info
            | _ -> 
                TDBind(bind,rest),info

        else 
            rest,rinfo

    | TDSwitch (e,cases,dflt,m) -> 
        // We always duplicate boolean-typed guards prior to optimizing. This is work which really should be done in patcompile.fs
        // where we must duplicate "when" expressions to ensure uniqueness of bound variables.
        //
        // However, we are not allowed to copy expressions in patcompile.fs because type checking is not complete (see FSharp 1.0 bug 4821).
        // Hence we do it here. There is no doubt a better way to do this.
        let e = if typeEquiv cenv.g (tyOfExpr cenv.g e) cenv.g.bool_ty then copyExpr cenv.g CloneAll e else e

        OptimizeSwitch cenv env (e,cases,dflt,m)

and TryOptimizeDecisionTreeTest cenv test vinfo = 
    match test,vinfo with 
    | Test.UnionCase (c1,_), StripUnionCaseValue(c2,_) ->  Some(cenv.g.unionCaseRefEq c1 c2)
    | Test.ArrayLength (_,_),  _ -> None
    | Test.Const c1,StripConstValue(c2) -> if c1 = Const.Zero || c2 = Const.Zero then None else Some(c1=c2)
    | Test.IsNull,StripConstValue(c2) -> Some(c2=Const.Zero)
    | Test.IsInst (_srcty1,_tgty1), _ -> None
    // These should not occur in optimization
    | Test.ActivePatternCase (_,_,_vrefOpt1,_,_),_ -> None
    | _ -> None

/// Optimize/analyze a switch construct from pattern matching 
and OptimizeSwitch cenv env (e,cases,dflt,m) =
    if verboseOptimizations then dprintf "OptimizeSwitch\n";
    let e', einfo = OptimizeExpr cenv env e 

    let cases,dflt = 
        if cenv.settings.EliminateSwitch() && not einfo.HasEffect then
            // Attempt to find a definite success, i.e. the first case where there is definite success
            match (List.tryFind (function (TCase(d2,_)) when TryOptimizeDecisionTreeTest cenv d2 einfo.Info  = Some(true) -> true | _ -> false) cases) with 
            | Some(TCase(_,case)) -> [],Some(case)
            | _ -> 
                // Filter definite failures
                cases |> List.filter (function (TCase(d2,_)) when TryOptimizeDecisionTreeTest cenv d2 einfo.Info = Some(false) -> false | _ -> true), 
                dflt
        else
            cases,dflt 
    // OK, see what we're left with and continue
    match cases,dflt with 
    | [],Some case -> OptimizeDecisionTree cenv env m case
    | _ -> OptimizeSwitchFallback cenv env (e', einfo, cases,dflt,m)

and OptimizeSwitchFallback cenv env (e', einfo, cases,dflt,m) =
    let cases',cinfos = List.unzip (List.map (fun (TCase(discrim,e)) -> let e',einfo = OptimizeDecisionTree cenv env m e in TCase(discrim,e'),einfo) cases) 
    let dflt',dinfos = match dflt with None -> None,[] | Some df -> let df',einfo = OptimizeDecisionTree cenv env m df in Some df',[einfo] 
    let size = (dinfos.Length + cinfos.Length) * 2
    let info = CombineValueInfosUnknown (einfo :: cinfos @ dinfos)
    let info = { info with TotalSize = info.TotalSize + size; FunctionSize = info.FunctionSize + size;  }
    TDSwitch (e',cases',dflt',m),info

and OptimizeBinding cenv isRec env (TBind(v,e,spBind)) =
    try 
        if verboseOptimizations then dprintf "OptimizeBinding\n";
        
        // The aim here is to stop method splitting for direct-self-tailcalls. We do more than that: if an expression
        // occurs in the body of recursively defined values RVS, then we refuse to split
        // any expression that contains a reference to any value in RVS.
        // This doesn't prevent splitting for mutually recursive references. See FSharp 1.0 bug 2892.
        let env = 
            if isRec then { env with dontSplitVars = env.dontSplitVars.Add v ()  } 
            else env
        
        let repr',einfo = 
            let env = if v.IsCompilerGenerated && isSome env.latestBoundId then env else {env with latestBoundId=Some v.Id} 
            let cenv = if v.InlineInfo = ValInline.PseudoVal then { cenv with optimizing=false} else cenv 
            let e',einfo = OptimizeLambdas (Some v) cenv env (InferArityOfExprBinding cenv.g v e)  e v.Type 
            let size = localVarSize 
            e',{einfo with FunctionSize=einfo.FunctionSize+size; TotalSize = einfo.TotalSize+size} 

        // Trim out optimization information for large lambdas we'll never inline
        // Trim out optimization information for expressions that call protected members 
        let rec cut ivalue = 
            match ivalue with
            | CurriedLambdaValue (_, arities, size, body,_) -> 
                if size > (cenv.settings.lambdaInlineThreshold + arities + 2) then 
                    if verboseOptimizations then dprintf "Discarding lambda for binding %s, size = %d, m = %a\n"  v.LogicalName size outputRange body.Range;
                    UnknownValue (* trim large *)
                else
                    let fvs = freeInExpr CollectLocals body
                    if fvs.UsesMethodLocalConstructs then
                        if verboseOptimizations then dprintf "Discarding lambda for binding %s because uses protected members, m = %a\n"  v.LogicalName outputRange body.Range;
                        UnknownValue (* trim protected *)
                    else
                        ivalue

            | ValValue(v,x) -> ValValue(v,cut x)
            | TupleValue a -> TupleValue(Array.map cut a)
            | RecdValue (tcref,a) -> RecdValue(tcref,Array.map cut a)       
            | UnionCaseValue (a,b) -> UnionCaseValue (a,Array.map cut b)
            | UnknownValue | ConstValue _  | ConstExprValue _ -> ivalue
            | SizeValue(_,a) -> MakeSizedValueInfo (cut a) 
        let einfo = if v.MustInline  then einfo else {einfo with Info = cut einfo.Info } 
        let einfo = 
            if (not v.MustInline && not (cenv.settings.KeepOptimizationValues())) ||
               
               // Bug 4916: do not record inline data for initialization trigger expressions
               // Note: we can't eliminate these value infos at the file boundaries because that would change initialization
               // order
               IsCompiledAsStaticPropertyWithField cenv.g v ||
               
               (v.InlineInfo = ValInline.Never) ||
               // MarshalByRef methods may not be inlined
               (match v.ActualParent with 
                | Parent tcref -> 
                    match cenv.g.system_MarshalByRefObject_tcref with
                    | None -> false
                    | Some mbrTyconRef ->
                    // Check we can deref system_MarshalByRefObject_tcref. When compiling against the Silverlight mscorlib we can't
                    if mbrTyconRef.TryDeref.IsSome then
                        // Check if this is a subtype of MarshalByRefObject
                        assert (cenv.g.system_MarshalByRefObject_typ.IsSome)
                        ExistsSameHeadTypeInHierarchy cenv.g cenv.amap v.Range (generalizedTyconRef tcref) cenv.g.system_MarshalByRefObject_typ.Value
                    else 
                        false
                | ParentNone -> false) ||

               // These values are given a special going-over by the optimizer and 
               // ilxgen.fs, hence treat them as if no-inline (when preparing the inline information for 
               // FSharp.Core).
               (let nvref = mkLocalValRef v 
                cenv.g.compilingFslib &&
                   (valRefEq cenv.g nvref cenv.g.seq_vref ||
                    valRefEq cenv.g nvref cenv.g.seq_generated_vref ||
                    valRefEq cenv.g nvref cenv.g.seq_finally_vref ||
                    valRefEq cenv.g nvref cenv.g.seq_using_vref ||
                    valRefEq cenv.g nvref cenv.g.seq_append_vref ||
                    valRefEq cenv.g nvref cenv.g.seq_empty_vref ||
                    valRefEq cenv.g nvref cenv.g.seq_delay_vref ||
                    valRefEq cenv.g nvref cenv.g.seq_singleton_vref ||
                    valRefEq cenv.g nvref cenv.g.seq_map_vref ||
                    valRefEq cenv.g nvref cenv.g.seq_collect_vref ||
                    valRefEq cenv.g nvref cenv.g.reference_equality_inner_vref ||
                    valRefEq cenv.g nvref cenv.g.generic_comparison_inner_vref ||
                    valRefEq cenv.g nvref cenv.g.generic_comparison_withc_inner_vref ||
                    valRefEq cenv.g nvref cenv.g.generic_equality_er_inner_vref ||
                    valRefEq cenv.g nvref cenv.g.generic_equality_per_inner_vref ||
                    valRefEq cenv.g nvref cenv.g.generic_equality_withc_inner_vref ||
                    valRefEq cenv.g nvref cenv.g.generic_hash_inner_vref))
            then {einfo with Info=UnknownValue} 
            else einfo 
        if v.MustInline  && IsPartialExprVal einfo.Info then 
            errorR(InternalError("the mustinline value '"^v.LogicalName^"' was not inferred to have a known value",v.Range));
        if verboseOptimizations then dprintf "val %s gets opt info %s\n" (showL(valL v)) (showL(exprValueInfoL cenv.g einfo.Info));
        
        let env = BindInternalLocalVal cenv v (mkValInfo einfo v) env 
        (TBind(v,repr',spBind), einfo), env
    with exn -> 
        errorRecovery exn v.Range; 
        raise (ReportedError (Some exn))
          
and OptimizeBindings cenv isRec env xs = FlatList.mapFold (OptimizeBinding cenv isRec) env xs
    
and OptimizeModuleExpr cenv env x = 
    match x with   
    | ModuleOrNamespaceExprWithSig(mty,def,m) -> 
        // Optimize the module implementation
        let (def,info),(_env,bindInfosColl) = OptimizeModuleDef cenv (env,[]) def  
        let bindInfosColl = List.concat bindInfosColl 
        
        // Compute the elements truly hidden by the module signature.
        // The hidden set here must contain NOT MORE THAN the set of values made inaccessible by 
        // the application of the signature. If it contains extra elements we'll accidentally eliminate
        // bindings.
         
        let (_renaming, hidden) as rpi = ComputeRemappingFromImplementationToSignature cenv.g def mty 
        let def = 
            if not (cenv.settings.localOpt()) then def else 

            let fvs = freeInModuleOrNamespace CollectLocals def 
            let dead = 
                bindInfosColl |> List.filter (fun (bind,binfo) -> 

                    // Check the expression has no side effect, e.g. is a lambda expression (a function definition)
                    not (ValueIsUsedOrHasEffect cenv (fun () -> fvs.FreeLocals) (bind,binfo)) &&

                    // Check the thing is hidden by the signature (if any)
                    hidden.mhiVals.Contains bind.Var && 

                    // Check the thing is not compiled as a static field or property, since reflected definitions and other reflective stuff might need it
                    not (IsCompiledAsStaticProperty cenv.g bind.Var))
            if verboseOptimizations then dead |> List.iter (fun (bind,_) -> dprintf "dead, hidden, buried, gone: %s\n" (showL (vspecAtBindL bind.Var)));
            let deadSet = Zset.addList (dead |> List.map (fun (bind,_) -> bind.Var)) (Zset.empty valOrder)

            // Eliminate dead private bindings from a module type by mutation. Note that the optimizer doesn't
            // actually copy the entire term - it copies the expression portions of the term and leaves the 
            // value_spec and entity_specs in place. However this means that the value_specs and entity specs 
            // need to be updated when a change is made that affects them, e.g. when a binding is eliminated. 
            // We'd have to do similar tricks if the type of variable is changed (as happens in TLR, which also
            // uses mutation), or if we eliminated a type constructor.
            //
            // It may be wise to move to a non-mutating implementation at some point here. Copying expressions is
            // probably more costly than copying specs anyway.
            let rec elimModTy (mtyp:ModuleOrNamespaceType) =                  
                let mty = 
                    new ModuleOrNamespaceType(kind=mtyp.ModuleOrNamespaceKind, 
                                              vals= (mtyp.AllValsAndMembers |> QueueList.filter (Zset.memberOf deadSet >> not)),
                                              entities= mtyp.AllEntities)
                mtyp.ModuleAndNamespaceDefinitions |> List.iter (fun mspec -> elimModSpec  mspec)
                mty;
            and elimModSpec (mspec:ModuleOrNamespace) = 
                let mtyp = elimModTy mspec.ModuleOrNamespaceType 
                mspec.Data.entity_modul_contents <- notlazy mtyp

            let rec elimModDef x =                  
                match x with 
                | TMDefRec(tycons,vbinds,mbinds,m) -> 
                    let vbinds = vbinds |> FlatList.filter (fun b -> b.Var |> Zset.memberOf deadSet |> not) 
                    let mbinds = mbinds |> List.map elim_mbind
                    TMDefRec(tycons,vbinds,mbinds,m)
                | TMDefLet(bind,m)  -> 
                    if Zset.contains bind.Var deadSet then TMDefRec([],FlatList.empty,[],m) else x
                | TMDefDo _  -> x
                | TMDefs(defs) -> TMDefs(List.map elimModDef defs) 
                | TMAbstract _ ->  x 
            and elim_mbind (ModuleOrNamespaceBinding(mspec, d)) =
                // Clean up the ModuleOrNamespaceType by mutation
                elimModSpec mspec;
                ModuleOrNamespaceBinding(mspec,elimModDef d) 
            
            elimModDef def 

        let info = AbstractAndRemapModulInfo "defs" cenv.g m rpi info

        ModuleOrNamespaceExprWithSig(mty,def,m),info 

and mkValBind (bind:Binding) info =
    (mkLocalValRef bind.Var, info)

and OptimizeModuleDef cenv (env,bindInfosColl) x = 
    match x with 
    | TMDefRec(tycons,binds,mbinds,m) -> 
        let env = BindInternalValsToUnknown cenv (valsOfBinds binds) env
        let bindInfos,env = OptimizeBindings cenv true env binds
        let binds', binfos = FlatList.unzip bindInfos
        let mbindInfos,(env,bindInfosColl) = OptimizeModuleBindings cenv (env,bindInfosColl) mbinds
        let mbinds,minfos = List.unzip mbindInfos
        
          (* REVIEW: Eliminate let bindings on the way back up *)
        (TMDefRec(tycons,binds',mbinds,m),
         notlazy { ValInfos= ValInfos(FlatList.map2 (fun bind binfo -> mkValBind bind (mkValInfo binfo bind.Var)) binds binfos); 
                   ModuleOrNamespaceInfos = NameMap.ofList minfos}),
         (env,(FlatList.toList bindInfos :: bindInfosColl))
    | TMAbstract(mexpr) -> 
        let mexpr,info = OptimizeModuleExpr cenv env mexpr
        let env = BindValsInModuleOrNamespace cenv info env
        (TMAbstract(mexpr),info),(env,bindInfosColl)
    | TMDefLet(bind,m)  ->
        let ((bind',binfo) as bindInfo),env = OptimizeBinding cenv false env bind
          (* REVIEW: Eliminate unused let bindings from modules *)
        (TMDefLet(bind',m),
         notlazy { ValInfos=ValInfos [mkValBind bind (mkValInfo binfo bind.Var)]; 
                   ModuleOrNamespaceInfos = NameMap.ofList []}),
        (env ,([bindInfo]::bindInfosColl))

    | TMDefDo(e,m)  ->
        let (e,_einfo) = OptimizeExpr cenv env e
        (TMDefDo(e,m),EmptyModuleInfo),
        (env ,bindInfosColl)
    | TMDefs(defs) -> 
        let (defs,info),(env,bindInfosColl) = OptimizeModuleDefs cenv (env,bindInfosColl) defs 
        (TMDefs(defs), info), (env,bindInfosColl)

and OptimizeModuleBindings cenv (env,bindInfosColl) xs = List.mapFold (OptimizeModuleBinding cenv) (env,bindInfosColl) xs

and OptimizeModuleBinding cenv (env,bindInfosColl) (ModuleOrNamespaceBinding(mspec, def)) = 
    let id = mspec.Id
    let (def,info),(_,bindInfosColl) = OptimizeModuleDef cenv (env,bindInfosColl) def 
    let env = BindValsInModuleOrNamespace cenv info env
    (ModuleOrNamespaceBinding(mspec,def),(id.idText, info)), 
    (env,bindInfosColl)

and OptimizeModuleDefs cenv (env,bindInfosColl) defs = 
    if verboseOptimizations then dprintf "OptimizeModuleDefs\n";
    let defs,(env,bindInfosColl) = List.mapFold (OptimizeModuleDef cenv) (env,bindInfosColl) defs
    let defs,minfos = List.unzip defs
    (defs,UnionModuleInfos minfos),(env,bindInfosColl)
   
and OptimizeImplFileInternal cenv env isIncrementalFragment (TImplFile(qname, pragmas, (ModuleOrNamespaceExprWithSig(mty,_,_) as mexpr), hasExplicitEntryPoint,isScript)) =
    let env,mexpr',minfo  = 
        match mexpr with 
        // FSI: FSI compiles everything as if you're typing incrementally into one module 
        // This means the fragment is not truly a constrained module as later fragments will be typechecked 
        // against the internals of the module rather than the externals. Furthermore it would be wrong to apply 
        // optimizations that did lots of reorganizing stuff to the internals of a module should we ever implement that. 
        | ModuleOrNamespaceExprWithSig(mty,def,m) when isIncrementalFragment -> 
            let (def,minfo),(env,_bindInfosColl) = OptimizeModuleDef cenv (env,[]) def 
            env, ModuleOrNamespaceExprWithSig(mty, def,m), minfo
        |  _ -> 
            let mexpr', minfo = OptimizeModuleExpr cenv env mexpr
            let env = BindValsInModuleOrNamespace cenv minfo env
            let env = { env with localExternalVals=env.localExternalVals.MarkAsCollapsible() } // take the chance to flatten to a dictionary
            env, mexpr', minfo

    let hidden = ComputeHidingInfoAtAssemblyBoundary mty

    let minfo = AbstractLazyModulInfoByHiding true hidden minfo
    env, TImplFile(qname,pragmas,mexpr',hasExplicitEntryPoint,isScript), minfo

//-------------------------------------------------------------------------
// Entry point
//------------------------------------------------------------------------- 

let OptimizeImplFile(settings,ccu,tcGlobals,tcVal, importMap,optEnv,isIncrementalFragment,emitTailcalls,mimpls) =
    let cenv = 
        { settings=settings;
          scope=ccu; 
          TcVal = tcVal
          g=tcGlobals; 
          amap=importMap;
          optimizing=true;
          localInternalVals=new System.Collections.Generic.Dictionary<Stamp,ValInfo>(10000);
          emitTailcalls=emitTailcalls;
          casApplied=new Dictionary<Stamp,bool>() }
    OptimizeImplFileInternal cenv optEnv isIncrementalFragment mimpls 


//-------------------------------------------------------------------------
// Pickle to stable format for cross-module optimization data
//------------------------------------------------------------------------- 


let rec p_ExprValueInfo x st =
    match x with 
    | ConstValue (c,ty)              -> p_byte 0 st; p_tup2 p_const p_typ (c,ty) st 
    | UnknownValue                   -> p_byte 1 st
    | ValValue (a,b)                 -> p_byte 2 st; p_tup2 (p_vref "optval") p_ExprValueInfo (a,b) st
    | TupleValue a                   -> p_byte 3 st; p_array p_ExprValueInfo a st
    | UnionCaseValue (a,b)           -> p_byte 4 st; p_tup2 p_ucref (p_array p_ExprValueInfo) (a,b) st
    | CurriedLambdaValue (_,b,c,d,e) -> p_byte 5 st; p_tup4 p_int p_int p_expr p_typ (b,c,d,e) st
    | ConstExprValue (a,b)           -> p_byte 6 st; p_tup2 p_int p_expr (a,b) st
    | RecdValue (tcref,a)            -> p_byte 7 st; p_tup2 (p_tcref "opt data") (p_array p_ExprValueInfo) (tcref,a) st
    | SizeValue (_adepth,a)          -> p_ExprValueInfo a st

and p_ValInfo (v:ValInfo) st = 
    p_tup2 p_ExprValueInfo p_bool (v.ValExprInfo, v.ValMakesNoCriticalTailcalls) st

and p_ModuleInfo x st = 
    p_tup2 
        (p_array (p_tup2 (p_vref "opttab") p_ValInfo)) 
        (p_namemap p_LazyModuleInfo) 
        ((x.ValInfos.Entries |> Seq.toArray) , x.ModuleOrNamespaceInfos) 
        st

and p_LazyModuleInfo x st = 
    p_lazy p_ModuleInfo x st

#endif // !NO_COMPILER_BACKEND

let rec u_ExprInfo st =
    let rec loop st =
        let tag = u_byte st
        match tag with
        | 0 -> u_tup2 u_const u_typ               st |> (fun (c,ty) -> ConstValue(c,ty))
        | 1 -> UnknownValue
        | 2 -> u_tup2 u_vref loop                 st |> (fun (a,b) -> ValValue (a,b))
        | 3 -> u_array loop                       st |> (fun a -> TupleValue a)
        | 4 -> u_tup2 u_ucref (u_array loop)      st |> (fun (a,b) -> UnionCaseValue (a,b))
        | 5 -> u_tup4 u_int u_int u_expr u_typ    st |> (fun (b,c,d,e) -> CurriedLambdaValue (newUnique(),b,c,d,e))
        | 6 -> u_tup2 u_int u_expr                st |> (fun (a,b) -> ConstExprValue (a,b))
        | 7 -> u_tup2 u_tcref (u_array loop)      st |> (fun (a,b) -> RecdValue (a,b))
        | _ -> failwith "loop"
    MakeSizedValueInfo (loop st) (* calc size of unpicked ExprValueInfo *)

and u_ValInfo st = 
    let a,b = u_tup2 u_ExprInfo u_bool st
    { ValExprInfo=a; ValMakesNoCriticalTailcalls = b } 

and u_ModuleInfo st = 
    let a,b = u_tup2 (u_array (u_tup2 u_vref u_ValInfo)) (u_namemap u_LazyModuleInfo) st
    { ValInfos= ValInfos a; ModuleOrNamespaceInfos=b}

and u_LazyModuleInfo st = u_lazy u_ModuleInfo st

