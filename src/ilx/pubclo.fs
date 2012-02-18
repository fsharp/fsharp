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


module internal Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.EraseIlxFuncs

open Internal.Utilities

open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.Types 
open Microsoft.FSharp.Compiler.AbstractIL.Extensions.ILX.IlxSettings 
open Microsoft.FSharp.Compiler.AbstractIL.Morphs 
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics 
open Microsoft.FSharp.Compiler.AbstractIL.IL 

open Microsoft.FSharp.Compiler.PrettyNaming


let addMethodGeneratedAttrsToTypeDef ilg tdef = 
    { tdef with Methods = tdef.Methods.AsList |> List.map (fun md -> md |> addMethodGeneratedAttrs ilg) |> mkILMethods }

// -------------------------------------------------------------------- 
// Erase closures and function types
// by compiling down to code pointers, classes etc.
// -------------------------------------------------------------------- 

let notlazy v = Lazy.CreateFromValue v
let logging = false 
let _ = if logging then dprintn "*** warning: Clo2_erase.logging is on"

let rec stripUpTo n test dest x =
    if n = 0 then ([],x) else 
    if test x then 
        let l,r = dest x
        let ls,res = stripUpTo (n-1) test dest r
        (l::ls),res
    else ([],x)

// -------------------------------------------------------------------- 
// Flags.  These need to match the various classes etc. in the 
// ILX standard library, and the parts 
// of the makefile that select the right standard library for a given
// combination of flags.
//
// Beyond this, the translation inserts classes or value classes for 
// the closure environment.  
// -------------------------------------------------------------------- 

let destTyLambda = function Lambdas_forall(l,r) -> (l,r) | _ -> failwith "no"
let isTyLambda   = function Lambdas_forall(_l,_r) -> true | _ -> false
let isTyApp    = function Apps_tyapp (_b,_c) ->true | _ -> false

let stripTyLambdasUpTo n lambdas = stripUpTo n isTyLambda destTyLambda lambdas

// -------------------------------------------------------------------- 
// Three tables related to indirect calling
// -------------------------------------------------------------------- *)

// Supported indirect calling conventions: 
// 1 
// 1_1 
// 1_1_1 
// 1_1_1_1 
// 1_1_1_1_1 
// plus type applications - up to 7 in one step 
// Nb. later code currently takes advantage of the fact that term 
// and type applications are never mixed in a single step. 
let stripSupportedIndirectCall apps =
    match apps with 
    | Apps_app(x,Apps_app(y,Apps_app(z,Apps_app(w,Apps_app(v,rest))))) -> [],[x;y;z;w;v],rest
    | Apps_app(x,Apps_app(y,Apps_app(z,Apps_app(w,rest))))             -> [],[x;y;z;w],rest
    | Apps_app(x,Apps_app(y,Apps_app(z,rest)))                         -> [],[x;y;z],rest
    | Apps_app(x,Apps_app(y,rest))                                     -> [],[x;y],rest
    | Apps_app(x,rest) -> [],[x],rest
    | Apps_tyapp _  -> 
        let maxTyApps =  1
        let tys,rest =  stripUpTo maxTyApps isTyApp destTyFuncApp apps
        tys,[],rest
    | rest -> [],[],rest

// Supported conventions for baking closures: 
// 0 
// 1 
// 1_1 
// 1_1_1 
// 1_1_1_1 
// 1_1_1_1_1 
// plus type applications - up to 7 in one step 
// Nb. later code currently takes advantage of the fact that term 
// and type applications are never mixed in a single step. 
let stripSupportedAbstraction lambdas =
    match lambdas with 
    | Lambdas_lambda(x,Lambdas_lambda(y,Lambdas_lambda(z,Lambdas_lambda(w,Lambdas_lambda(v,rest))))) -> [],[x;y;z;w;v],rest
    | Lambdas_lambda(x,Lambdas_lambda(y,Lambdas_lambda(z,Lambdas_lambda(w,rest))))                   -> [],[x;y;z;w],rest
    | Lambdas_lambda(x,Lambdas_lambda(y,Lambdas_lambda(z,rest)))                                     -> [],[x;y;z],rest
    | Lambdas_lambda(x,Lambdas_lambda(y,rest))                                                       -> [],[x;y],rest
    | Lambdas_lambda(x,rest) -> [],[x],rest
    | Lambdas_forall _ -> 
        let maxTyApps =  1
        let tys,rest = stripTyLambdasUpTo maxTyApps lambdas
        tys,[],rest
    | rest -> [],[],rest

// This must correspond to stripSupportedAbstraction
let isSupportedDirectCall apps = 
    match apps with 
    | Apps_app (_,Apps_done _)                                          -> true
    | Apps_app (_,Apps_app (_, Apps_done _))                            -> true
    | Apps_app (_,Apps_app (_,Apps_app (_, Apps_done _)))               -> true
    | Apps_app (_,Apps_app (_,Apps_app (_, Apps_app (_, Apps_done _)))) -> true
    | Apps_tyapp _ -> false
    | _ -> false

// -------------------------------------------------------------------- 
// Prelude for function types.  Only use System.Func for now, prepare
// for more refined types later.
// -------------------------------------------------------------------- 

let mkFuncTypeRef n = 
    if n = 1 then mkILTyRef (IlxSettings.ilxFsharpCoreLibScopeRef (),IlxSettings.ilxNamespace () ^ ".FSharpFunc`2")
    else mkILNestedTyRef (IlxSettings.ilxFsharpCoreLibScopeRef (),
                         [IlxSettings.ilxNamespace () ^ ".OptimizedClosures"],
                         "FSharpFunc`"^ string (n + 1))
type cenv = 
    { ilg:ILGlobals;
      tref_Func: ILTypeRef array;
      mkILTyFuncTy: ILType }
  
let new_cenv(ilg) =
    { ilg=ilg;
      tref_Func= Array.init 10 (fun i -> mkFuncTypeRef(i+1));
      mkILTyFuncTy=ILType.Boxed (mkILNonGenericTySpec (mkILTyRef (IlxSettings.ilxFsharpCoreLibScopeRef (), IlxSettings.ilxNamespace () ^ ".FSharpTypeFunc"))) }

let mkILTyFuncTy cenv = cenv.mkILTyFuncTy
  
let mkILFuncTy cenv dty rty = mkILBoxedTy cenv.tref_Func.[0] [dty;rty]
let mkILCurriedFuncTy cenv dtys rty = List.foldBack (mkILFuncTy cenv) dtys rty

let typ_Func cenv (dtys: ILType list) rty = 
    let n = dtys.Length
    let tref = if n <= 10 then cenv.tref_Func.[n-1] else mkFuncTypeRef n   
    mkILBoxedTy tref (dtys @ [rty])

let rec mkTyOfApps cenv apps =
    match apps with 
    | Apps_tyapp _ -> cenv.mkILTyFuncTy
    | Apps_app (dty,rest) -> mkILFuncTy cenv dty (mkTyOfApps cenv rest)
    | Apps_done rty -> rty

let rec mkTyOfLambdas cenv lam = 
    match lam with 
    | Lambdas_return rty -> rty
    | Lambdas_lambda (d,r) -> mkILFuncTy cenv d.Type (mkTyOfLambdas cenv r)
    | Lambdas_forall _ -> cenv.mkILTyFuncTy

// -------------------------------------------------------------------- 
// Fields for free variables in closures
// -------------------------------------------------------------------- 

let mkCloFieldName (clospec:IlxClosureSpec) n = 
      (List.nth clospec.FormalFreeVars n).fvName

// type reference for the function value itself 
let tyForCloSpec (clospec: IlxClosureSpec) = ILType.Boxed (mkILTySpec(clospec.TypeRef,  clospec.GenericArgs))

let mkCloFieldSpec (clospec:IlxClosureSpec) n =
    let cloTy = tyForCloSpec clospec
    match clospec.FormalLambdas with 
    | Lambdas_forall _  | Lambdas_return _ ->  
        let ty = clospec.FormalFreeVarType n
        mkILFieldSpecInTy (cloTy,mkCloFieldName clospec n,ty)
    | Lambdas_lambda (_d,_) ->
        let ty = clospec.FormalFreeVarType n
        mkILFieldSpecInTy (cloTy,mkCloFieldName clospec n,ty)

// -------------------------------------------------------------------- 
// Method to call for a particular multi-application
// -------------------------------------------------------------------- 
    
let mkMethSpecForMultiApp cenv (argtys': ILType list,rty) =  
    let n = argtys'.Length
    let formalArgTys = List.mapi (fun i _ ->  ILType.TypeVar (uint16 i)) argtys'
    let formalRetTy = ILType.TypeVar (uint16 n)
    let inst = argtys'@[rty]
    if n = 1  then 
      true, 
       (mkILNonGenericInstanceMethSpecInTy (mkILBoxedTy cenv.tref_Func.[0] inst,"Invoke",formalArgTys, formalRetTy))
    else 
       false, 
       (mkILStaticMethSpecInTy
          (mkILFuncTy cenv inst.[0] inst.[1],
           "InvokeFast",
           [mkILCurriedFuncTy cenv formalArgTys formalRetTy]@formalArgTys,
           formalRetTy,
           inst.Tail.Tail))

let mkCallBlockForMultiValueApp cenv doTailCall (args',rty') inplab outlab =
    let callvirt,mr = mkMethSpecForMultiApp cenv (args',rty')
    let instrs = [ ( if callvirt then I_callvirt (doTailCall,mr, None) else I_call (doTailCall,mr, None) ) ]
    if doTailCall = Tailcall then mkNonBranchingInstrs inplab instrs 
    else mkNonBranchingInstrsThenBr inplab instrs outlab

let mkMethSpecForClosureCall cenv (clospec: IlxClosureSpec) = 
    let tyargsl,argtys,rstruct = stripSupportedAbstraction clospec.FormalLambdas
    if nonNil tyargsl then failwith "mkMethSpecForClosureCall: internal error";
    let rty' = mkTyOfLambdas cenv rstruct
    let argtys' = typesOfILParams argtys
    let minst' = clospec.GenericArgs
    (mkILInstanceMethSpecInTy(tyForCloSpec clospec,"Invoke",argtys',rty',minst'))


// -------------------------------------------------------------------- 
// Translate instructions....
// -------------------------------------------------------------------- 

let mkLdfldGen lda x = (if lda then mkNormalLdflda x else mkNormalLdfld x)

let convStclofldInstr _tmps (clospec:IlxClosureSpec) n =
    match clospec.FormalLambdas with 
    | Lambdas_forall _ | Lambdas_return _ -> 
        [ mkNormalStfld (mkCloFieldSpec clospec n) ]
    | Lambdas_lambda _ -> 
        [ mkNormalStfld (mkCloFieldSpec clospec n) ]

let convLdenvInstr lda (clospec:IlxClosureSpec) n = 
    match clospec.FormalLambdas with 
    | Lambdas_forall _ | Lambdas_return _ -> 
        [ mkLdarg0;
          mkLdfldGen lda (mkCloFieldSpec clospec n) ]
    | Lambdas_lambda _ -> 
            [ mkLdarg0;
              mkLdfldGen lda (mkCloFieldSpec clospec n) ]

let rec convInstr cenv (tmps: ILLocalsAllocator, thisGenParams: ILGenericParameterDefs,thisClo) inplab outlab instr = 

    match instr with 
    | EI_ilzero typ -> 
          let n = tmps.AllocLocal (mkILLocal  typ)
          Choice1Of2 [ I_ldloca n;  I_initobj typ; I_ldloc n ]

    | I_other e when isIlxExtInstr e -> 
        match (destIlxExtInstr e) with 
        |  (EI_castclo clospec) -> Choice1Of2 [ I_castclass (tyForCloSpec clospec) ]
              
        |  (EI_isclo clospec)   -> Choice1Of2 [ I_isinst (tyForCloSpec clospec) ]

        |  (EI_newclo clospec) ->

            let cspec = tyForCloSpec clospec

            match clospec.FormalLambdas with 
            | Lambdas_forall _ | Lambdas_return _ -> 
                let fields = clospec.FormalFreeVars
                let makerMethSpec = (mkILCtorMethSpecForTy (cspec,fields |> List.map (fun fv -> fv.fvType) ))
                Choice1Of2 [ I_newobj (makerMethSpec,None) ]

            | Lambdas_lambda _ ->
                let fields = clospec.FormalFreeVars
                let makerMethSpec = (mkILCtorMethSpecForTy (cspec,fields |> List.map (fun fv -> fv.fvType)))
                Choice1Of2 [ I_newobj (makerMethSpec,None) ]
              
        |  EI_stclofld (clospec,n) -> 
            Choice1Of2 (convStclofldInstr tmps clospec n)
              
        |  EI_ldenv n -> 
            match thisClo with 
            | None -> failwith "ldenv in non-closure"
            | Some (_,clospec) -> Choice1Of2 (convLdenvInstr false clospec n)
            
        |  EI_stenv n -> 
            match thisClo with 
            | None -> failwith "stenv in non-closure"
            | Some (_,clospec) -> Choice1Of2 (convStclofldInstr tmps clospec n)
            
        |  EI_ldenva n -> 
            match thisClo with 
            | None -> failwith "ldenv in non-closure"
            | Some (_,clospec) -> Choice1Of2 (convLdenvInstr true clospec n)
            
        | i when (match i with  EI_callfunc _ -> true | EI_callclo _ -> true | _ -> false) ->
            // "callfunc" and "callclo" instructions become a series of indirect 
            // calls or a single direct call.   
            let varCount = thisGenParams.Length
            let tl,apps,directClo = 
              match i with 
              | EI_callfunc (tl,apps) -> tl,apps,None
              | EI_callclo (tl,_clospec,apps) when not (isSupportedDirectCall apps) -> tl,apps,None 
              | EI_callclo (tl,clospec,apps) -> tl,apps,Some clospec 
              | _ -> failwith "Unexpected call instruction"

            match directClo with 
            | Some clospec ->  Choice1Of2 [I_call (tl, mkMethSpecForClosureCall cenv clospec,None) ]
            | None -> 
                
              // Unwind the stack until the arguments given in the apps have 
              // all been popped off.  The apps given to this function is 
              // what remains after the first "strip" of suitable arguments for the 
              // first call. 
              // Loaders and storers are returned in groups.  Storers are used to pop 
              // the arguments off the stack that correspond to all the arguments in 
              // the apps, and the loaders are used to load them back on.  
                let rec unwind apps = 
                    match apps with 
                    | Apps_tyapp (actual,rest) -> 
                        let rest = instAppsAux varCount [actual] rest
                        let storers,loaders = unwind rest
                        [] :: storers, [] :: loaders 
                    | Apps_app (arg,rest) -> 
                        let storers, loaders = unwind rest
                        let argStorers,argLoaders = 
                              let locn = tmps.AllocLocal (mkILLocal arg)
                              [I_stloc locn], [I_ldloc locn]
                        argStorers :: storers, argLoaders :: loaders  
                    | Apps_done _ -> 
                        [],[]
                
                let rec computePreCall fst n rest (loaders: ILInstr list) = 
                    if fst then 
                      let storers,(loaders2 : ILInstr list list) =  unwind rest
                      (List.rev (List.concat storers) : ILInstr list) , List.concat loaders2
                    else 
                      stripUpTo n (function (_x::_y) -> true | _ -> false) (function (x::y) -> (x,y) | _ -> failwith "no!") loaders
                
                let rec buildApp fst loaders apps inplab outlab =
                    // Strip off one valid indirect call.  [fst] indicates if this is the 
                    // first indirect call we're making. The code below makes use of the 
                    // fact that term and type applications are never currently mixed for 
                    // direct calls. 
                    match stripSupportedIndirectCall apps with 
                    // Type applications: 
                    | tyargs,[],_ when nonNil tyargs ->
                        // strip again, instantiating as we go.  we could do this while we count. 
                        let (revInstTyArgs, rest') = 
                            (([],apps), tyargs) ||> List.fold (fun (revArgsSoFar,cs) _  -> 
                                  let actual,rest' = destTyFuncApp cs
                                  let rest'' = instAppsAux varCount [actual] rest'
                                  ((actual :: revArgsSoFar),rest''))
                        let instTyargs = List.rev revInstTyArgs
                        let precall,loaders' = computePreCall fst 0 rest' loaders
                        let doTailCall = andTailness tl false
                        let instrs1 = 
                            precall @
                            [ I_callvirt (doTailCall, 
                                          
                                          (mkILInstanceMethSpecInTy (cenv.mkILTyFuncTy,"Specialize",[],cenv.ilg.typ_Object, instTyargs)), None) ]
                        let instrs1 =                        
                            // TyFunc are represented as Specialize<_> methods returning an object.                            
                            // For value types, recover result via unbox and load.
                            // For reference types, recover via cast.
                            let rtnTy = mkTyOfApps cenv rest'
                            instrs1 @ [ I_unbox_any rtnTy]
                        if doTailCall = Tailcall then mkNonBranchingInstrs inplab instrs1 
                        else 
                            let endOfCallBlock = generateCodeLabel ()
                            let block1 = mkNonBranchingInstrsThenBr inplab instrs1 endOfCallBlock
                            let block2 = buildApp false loaders' rest' endOfCallBlock outlab
                            mkGroupBlock ([endOfCallBlock],[ block1; block2 ])

                  // Term applications 
                    | [],args,rest when nonNil args -> 
                        let precall,loaders' = computePreCall fst args.Length rest loaders
                        let isLast = (match rest with Apps_done _ -> true | _ -> false)
                        let rty  = mkTyOfApps cenv rest
                        let doTailCall = andTailness tl isLast

                        let startOfCallBlock = generateCodeLabel ()
                        let preCallBlock = mkNonBranchingInstrsThenBr inplab precall startOfCallBlock

                        if doTailCall = Tailcall then 
                            let callBlock =  mkCallBlockForMultiValueApp cenv doTailCall (args,rty) startOfCallBlock outlab
                            mkGroupBlock ([startOfCallBlock],[ preCallBlock; callBlock ])
                        else
                            let endOfCallBlock = generateCodeLabel ()
                            let callBlock =  mkCallBlockForMultiValueApp cenv doTailCall (args,rty) startOfCallBlock endOfCallBlock
                            let restBlock = buildApp false loaders' rest endOfCallBlock outlab
                            mkGroupBlock ([startOfCallBlock; endOfCallBlock],[ preCallBlock; callBlock; restBlock ])

                    | [],[],Apps_done _rty -> 
                        // "void" return values are allowed in function types 
                        // but are translated to empty value classes.  These 
                        // values need to be popped. 
                        mkNonBranchingInstrsThen inplab ([]) (if tl = Tailcall then I_ret else I_br outlab)
                    | _ -> failwith "*** Error: internal error: unknown indirect calling convention returned by stripSupportedIndirectCall"
                 
                Choice2Of2 (buildApp true [] apps inplab outlab)
        | _ ->  Choice1Of2 [instr]  
          
    | _ ->  Choice1Of2 [instr] 

// Fix up I_ret instruction. Generalise to selected instr.
let convReturnInstr ty _inplab _outlab instr = 
    match instr with 
    | I_ret -> Choice1Of2 [I_box ty;I_ret]
    | _     -> Choice1Of2 [instr]
        
let convILMethodBody cenv (thisGenParams,thisClo,boxReturnTy) il = 
    let tmps = ILLocalsAllocator il.Locals.Length
    let locals = il.Locals
    // Add a local to keep the result value of a thunk while storing it 
    // into the result field and returning it. 
    // Record the local slot number in the environment passed in thisClo 
    let thisClo, newMax = 
        match thisClo with 
        | Some clospec -> Some(locals.Length, clospec),il.MaxStack+2 (* for calls *)
        | None -> None,il.MaxStack
    let code' = morphExpandILInstrsInILCode (convInstr cenv (tmps, thisGenParams,thisClo)) il.Code
    let code' = match boxReturnTy with
                | None    -> code'
                | Some ty -> (* box before returning? e.g. in the case of a TyFunc returning a struct, which compiles to a Specialise<_> method returning an object *)
                             morphExpandILInstrsInILCode (convReturnInstr ty) code'
    {il with MaxStack=newMax;  
             IsZeroInit=true;
             Code= code' ;
             Locals = locals @ tmps.Close() }

let convMethodBody cenv (thisGenParams,thisClo) = function
    | MethodBody.IL il -> MethodBody.IL (convILMethodBody cenv (thisGenParams,thisClo,None) il)
    | x -> x

let convMethodDef cenv (thisGenParams,thisClo) (md: ILMethodDef)  =
    let b' = convMethodBody cenv ((thisGenParams @ md.GenericParams) ,thisClo) (md.mdBody.Contents)
    {md with mdBody=mkMethBodyAux b'}

// -------------------------------------------------------------------- 
// Make fields for free variables of a type abstraction.
// -------------------------------------------------------------------- 

let mkILFreeVarForParam (p : ILParameter) = 
    let nm = (match p.Name with Some x -> x | None -> failwith "closure parameters must be given names")
    mkILFreeVar(nm, false,p.Type)

let mkILLocalForParam (p: ILParameter) = mkILLocal p.Type

let mkILCloFldSpecs _cenv flds = 
    flds |> List.map (fun fv -> (fv.fvName,fv.fvType)) 

let mkILCloFldDefs cenv flds = 
    flds 
    |> List.map (fun fv -> 
         let fdef = mkILInstanceField (fv.fvName,fv.fvType,None,ILMemberAccess.Public)
         if fv.fvCompilerGenerated then 
             fdef |> addFieldNeverAttrs cenv.ilg
                  |> addFieldGeneratedAttrs cenv.ilg
         else
             fdef)

// -------------------------------------------------------------------- 
// Convert a closure.  Split and chop if there are too many arguments,
// otherwise build the appropriate kind of thing depending on whether
// it's a type abstraction or a term abstraction.
// -------------------------------------------------------------------- 

let rec convIlxClosureDef cenv mdefGen encl (td: ILTypeDef) clo = 
    let newTypeDefs,newMethodDefs = 

      // the following are shared between cases 1 && 2 
      let nowFields = clo.cloFreeVars
      let nowTypeRef =  mkILNestedTyRef (ILScopeRef.Local, encl, td.Name)
      let nowTy = mkILFormalBoxedTy nowTypeRef td.GenericParams
      let nowCloRef = IlxClosureRef(nowTypeRef,clo.cloStructure,nowFields)
      let nowCloSpec = mkILFormalCloRef td.GenericParams nowCloRef
      let tagClo = clo.cloSource
      let tagApp = (Lazy.force clo.cloCode).SourceMarker
      
      let tyargsl,tmargsl,laterStruct = stripSupportedAbstraction clo.cloStructure
      let laterAccess = td.Access (* (if td.Access = ILTypeDefAccess.Public then ILTypeDefAccess.Nested ILMemberAccess.Public else ILTypeDefAccess.Nested ILMemberAccess.Assembly) in*)

      // Adjust all the argument and environment accesses 
      let fixCode (savedArgs: (int * ILParameter) list)  = 
          let nenv = nowFields.Length
          let il = Lazy.force clo.cloCode
          let numLocals = il.Locals.Length
          let fixupInstr instr =
              let fixupArg mkEnv mkArg n = 
                  let rec findMatchingArg l c = 
                      match l with 
                      | ((m,_)::t) -> 
                          if n = m then mkEnv c
                          else findMatchingArg t (c+1)
                      | [] -> mkArg (n - savedArgs.Length + 1)
                  findMatchingArg savedArgs 0
              match instr with 
              | I_ldarg n -> 
                  fixupArg 
                    (fun x -> [ I_ldloc (uint16 (x+numLocals)) ]) 
                    (fun x -> [ I_ldarg (uint16 x )])
                    (int n)
              | I_starg n -> 
                  fixupArg 
                    (fun x -> [ I_stloc (uint16 (x+numLocals)) ]) 
                    (fun x -> [ I_starg (uint16 x) ])
                    (int n)
              | I_ldarga n ->  
                  fixupArg 
                    (fun x -> [ I_ldloca (uint16 (x+numLocals)) ]) 
                    (fun x -> [ I_ldarga (uint16 x) ]) 
                    (int n)
              | i ->  [i]
          let mainCode = morphILInstrsInILCode fixupInstr il.Code
          let ldenvCode = 
             savedArgs |> List.mapi (fun n _ -> [ (mkIlxInstr (EI_ldenv (n + nenv))); I_stloc (uint16 (n+numLocals)) ]) |> List.concat 
          let code = prependInstrsToCode ldenvCode mainCode
          
          {il with 
               Code=code;
               Locals=il.Locals @ List.map (snd >> mkILLocalForParam) savedArgs; 
                            (* maxstack may increase by 1 due to environment loads *)
               MaxStack=il.MaxStack+1 }


      match tyargsl,tmargsl,laterStruct with 
      // CASE 1 - Type abstraction 
      | (_ :: _), [],_ ->
          let addedGenParams = tyargsl
          let nowReturnTy = (mkTyOfLambdas cenv laterStruct)
          
        // CASE 1a. Split a type abstraction. 
        // Adjust all the argument and environment accesses 
        // Actually that special to do here in the type abstraction case 
        // nb. should combine the term and type abstraction cases for  
        // to allow for term and type variables to be mixed in a single 
        // application. 
          if (match laterStruct with Lambdas_return _ -> false | _ -> true) then 
            
              let nowStruct = List.foldBack (fun x y -> Lambdas_forall(x,y)) tyargsl (Lambdas_return nowReturnTy)
              let laterTypeName = td.Name^"T"
              let laterTypeRef = mkILNestedTyRef (ILScopeRef.Local,encl,laterTypeName)
              let laterGenericParams = td.GenericParams @ addedGenParams
              let nowSelfTy = mkTyOfLambdas cenv nowStruct
              let laterFields =  nowFields @ [mkILFreeVar(CompilerGeneratedName ("self"^string nowFields.Length),true,nowSelfTy)]
              let laterCloRef = IlxClosureRef(laterTypeRef,laterStruct,laterFields)
              let laterCloSpec = mkILFormalCloRef laterGenericParams laterCloRef
              
              let laterCode = fixCode [(0, mkILParamNamed (CompilerGeneratedName "self", nowSelfTy))]
              let laterTypeDefs = 
                convIlxClosureDef cenv mdefGen encl
                  {td with GenericParams=laterGenericParams;
                            Access=laterAccess;
                            Name=laterTypeName} 
                  {clo with cloStructure=laterStruct;
                            cloFreeVars=laterFields;
                            cloCode=notlazy laterCode}
              
              let laterTypeDefs = laterTypeDefs |>  List.map (addMethodGeneratedAttrsToTypeDef cenv.ilg)

            // This is the code which will get called when then "now" 
            // arguments get applied. Convert it with the information 
            // that it is the code for a closure... 
              let nowCode = 
                mkILMethodBody
                  (false,[],nowFields.Length + 1,
                   nonBranchingInstrsToCode
                     begin 
                       // Load up the environment, including self... 
                       List.mapi (fun n _ -> (mkIlxInstr (EI_ldenv  n))) nowFields @
                       [ mkLdarg0 ] @
                       // Make the instance of the delegated closure && return it. 
                       // This passes the method type params. as class type params. 
                       [ (mkIlxInstr (EI_newclo laterCloSpec)) ] 
                     end,
                   tagApp)

              let nowTypeDefs = 
                convIlxClosureDef cenv mdefGen encl
                  td {clo with cloStructure=nowStruct; 
                               cloCode=notlazy nowCode}
              nowTypeDefs @ laterTypeDefs, []
          else 
              // CASE 1b. Build a type application. 
              // Currently the sole mbody defines a class and uses 
              // virtual methods. 
              let boxReturnTy = Some nowReturnTy (* box prior to all I_ret *)
              let nowApplyMethDef =
                mkILGenericVirtualMethod
                  ("Specialize",
                   ILMemberAccess.Public,
                   addedGenParams,  (* method is generic over added ILGenericParameterDefs *)
                   [],
                   mkILReturn(cenv.ilg.typ_Object),
                   MethodBody.IL (convILMethodBody cenv (td.GenericParams@addedGenParams,Some nowCloSpec,boxReturnTy)
                                    (Lazy.force clo.cloCode)))
              let ctorMethodDef = 
                  mkILStorageCtor 
                    (tagClo,
                     [ mkLdarg0; mkNormalCall (mkILCtorMethSpecForTy (cenv.mkILTyFuncTy, [])) ],
                     nowTy,
                     mkILCloFldSpecs cenv nowFields,
                     ILMemberAccess.Assembly)

              let cloTypeDef = 
                { Name = td.Name;
                  GenericParams= td.GenericParams;
                  Access=td.Access;
                  Implements = [];
                  IsAbstract = false;
                  NestedTypes = emptyILTypeDefs;
                  IsSealed = false;
                  IsSerializable=td.IsSerializable; 
                  IsComInterop=false;
                  IsSpecialName=false;
                  Layout=ILTypeDefLayout.Auto;
                  Encoding=ILDefaultPInvokeEncoding.Ansi;
                  InitSemantics=ILTypeInit.BeforeField;
                  Extends= Some cenv.mkILTyFuncTy;
                  Methods= mkILMethods ([ctorMethodDef] @ [nowApplyMethDef]); 
                  Fields= mkILFields (mkILCloFldDefs cenv nowFields);
                  CustomAttrs=emptyILCustomAttrs;
                  MethodImpls=emptyILMethodImpls;
                  Properties=emptyILProperties;
                  Events=emptyILEvents;
                  HasSecurity=false; 
                  SecurityDecls=emptyILSecurityDecls; 
                  tdKind = ILTypeDefKind.Class;}
              [ cloTypeDef], []

    // CASE 2 - Term Application 
      |  [], (_ :: _ as nowParams),_ ->
          let nowReturnTy = mkTyOfLambdas cenv laterStruct
          
         // CASE 2a - Too Many Term Arguments or Remaining Type arguments - Split the Closure Class in Two 
          if (match laterStruct with Lambdas_return _ -> false | _ -> true) then 
              let nowStruct = List.foldBack (fun l r -> Lambdas_lambda(l,r)) nowParams (Lambdas_return nowReturnTy)
              let laterTypeName = td.Name^"D"
              let laterTypeRef = mkILNestedTyRef (ILScopeRef.Local,encl,laterTypeName)
              let laterGenericParams = td.GenericParams
            // Number each argument left-to-right, adding one to account for the "this" pointer
              let self = mkILParamNamed (CompilerGeneratedName "self", tyForCloSpec nowCloSpec)
              let nowParamNums = (0, self) :: List.mapi (fun i x -> i+1, x) nowParams
              let savedArgs =  nowParamNums
              let laterFields = nowFields@(List.map (fun (_n,p) -> mkILFreeVarForParam p) savedArgs)
              let laterCloRef = IlxClosureRef(laterTypeRef,laterStruct,laterFields)
              let laterCloSpec = mkILFormalCloRef laterGenericParams laterCloRef
              // This is the code which will first get called. 
              let nowCode = 
                  mkILMethodBody
                    (false,[],savedArgs.Length + nowFields.Length,
                     nonBranchingInstrsToCode
                       begin 
                         // Load up the environment 
                         List.mapi (fun n _ -> (mkIlxInstr (EI_ldenv n))) nowFields @
                         // Load up all the arguments (including self), which become free variables in the delegated closure 
                         List.map (fun (n,_ty) -> I_ldarg (uint16 n)) savedArgs @
                         // Make the instance of the delegated closure && return it. 
                         [ (mkIlxInstr (EI_newclo laterCloSpec)) ] 
                       end,
                     tagApp)
              let nowTypeDefs = 
                convIlxClosureDef cenv mdefGen encl
                  td
                  {clo with cloStructure=nowStruct;
                            cloCode=notlazy nowCode}
              let laterCode = fixCode savedArgs
              let laterTypeDefs = 
                convIlxClosureDef cenv mdefGen encl
                  {td with GenericParams=laterGenericParams;
                             Access=laterAccess;
                             Name=laterTypeName} 
                  {clo with cloStructure=laterStruct;
                        cloFreeVars=laterFields;
                        cloCode=notlazy laterCode}
              // add 'compiler generated' to all the methods in the 'now' classes
              let laterTypeDefs = laterTypeDefs |>  List.map (addMethodGeneratedAttrsToTypeDef cenv.ilg)
              nowTypeDefs @ laterTypeDefs, []
                        
          else 
                // CASE 2b - Build an Term Application Apply method 
                // CASE 2b2. Build a term application as a virtual method. 
                
                let nowEnvParentClass = typ_Func cenv (typesOfILParams nowParams) nowReturnTy 
                let cloTypeDef = 
                    let nowApplyMethDef =
                        mkILNonGenericVirtualMethod
                          ("Invoke",ILMemberAccess.Public,
                           nowParams, 
                           mkILReturn nowReturnTy,
                           MethodBody.IL (convILMethodBody cenv (td.GenericParams,Some nowCloSpec,None)  (Lazy.force clo.cloCode)))
                    let ctorMethodDef = 
                        mkILStorageCtor 
                           (tagClo,
                            [ mkLdarg0; mkNormalCall (mkILCtorMethSpecForTy (nowEnvParentClass,[])) ],
                            nowTy,
                            mkILCloFldSpecs cenv nowFields,
                            ILMemberAccess.Assembly)
                    { Name = td.Name;
                      GenericParams= td.GenericParams;
                      Access = td.Access;
                      Implements = [];
                      IsAbstract = false;
                      IsSealed = false;
                      IsSerializable=td.IsSerializable; 
                      IsComInterop=false;
                      IsSpecialName=false;
                      Layout=ILTypeDefLayout.Auto;
                      Encoding=ILDefaultPInvokeEncoding.Ansi;
                      InitSemantics=ILTypeInit.BeforeField;
                      NestedTypes = emptyILTypeDefs; 
                      Extends= Some nowEnvParentClass;
                      Methods= mkILMethods ([ctorMethodDef] @ [nowApplyMethDef]); 
                      Fields= mkILFields (mkILCloFldDefs cenv nowFields);
                      CustomAttrs=emptyILCustomAttrs;
                      MethodImpls=emptyILMethodImpls;
                      Properties=emptyILProperties;
                      Events=emptyILEvents;
                      HasSecurity=false; 
                      SecurityDecls=emptyILSecurityDecls; 
                      tdKind = ILTypeDefKind.Class; } 
                [cloTypeDef],[]
      |  [],[],Lambdas_return _ -> 
          // No code is being declared: just bake a (mutable) environment 
          let cloCode' = 
            match td.Extends with 
            | None ->  (mkILNonGenericEmptyCtor tagClo cenv.ilg.typ_Object).MethodBody 
            | Some  _ -> convILMethodBody cenv (td.GenericParams,Some nowCloSpec,None)  (Lazy.force clo.cloCode)

          let ctorMethodDef = 
            let flds = (mkILCloFldSpecs cenv nowFields)
            mkILCtor(ILMemberAccess.Public,
                    List.map mkILParamNamed flds,
                    mkMethodBody
                      (cloCode'.IsZeroInit,
                       cloCode'.Locals,
                       cloCode'.MaxStack,
                       prependInstrsToCode
                          (List.concat (List.mapi (fun n (nm,ty) -> 
                               [ mkLdarg0;
                                 I_ldarg (uint16 (n+1));
                                 mkNormalStfld (mkILFieldSpecInTy (nowTy,nm,ty));
                               ])  flds))
                         cloCode'.Code,
                       tagClo))
          
          let cloTypeDef = 
            { td with
                  Implements= td.Implements;
                  Extends= (match td.Extends with None -> Some cenv.ilg.typ_Object | Some x -> Some(x));
                  Name = td.Name;
                  GenericParams= td.GenericParams;
                  Methods= mkILMethods (ctorMethodDef :: List.map (convMethodDef cenv ( td.GenericParams,Some nowCloSpec)) td.Methods.AsList); 
                  Fields= mkILFields (mkILCloFldDefs cenv nowFields @ td.Fields.AsList);
                  tdKind = ILTypeDefKind.Class; } 
          [cloTypeDef],[]
      | a,b,_ ->
          failwith ("Unexpected unsupported abstraction sequence, #tyabs = "^string a.Length ^ ", #tmabs = "^string b.Length)
   
    mdefGen := !mdefGen@newMethodDefs;
    newTypeDefs

// -------------------------------------------------------------------- 
// Convert a class 
// -------------------------------------------------------------------- 

let rec convTypeDef cenv mdefGen encl td = 
  match td.tdKind with 
  | ILTypeDefKind.Other e when isIlxExtTypeDefKind e && (match destIlxExtTypeDefKind e with IlxTypeDefKind.Closure _ -> true | _ -> false) -> 
      match destIlxExtTypeDefKind e with 
      | IlxTypeDefKind.Closure cloinfo -> convIlxClosureDef cenv mdefGen encl td cloinfo
      | IlxTypeDefKind.Union _ -> failwith "classunions should have been erased by this time"
  | _ -> 
      [ {td with 
             NestedTypes = convTypeDefs cenv mdefGen (encl@[td.Name]) td.NestedTypes;
             Methods=morphILMethodDefs (convMethodDef cenv (td.GenericParams,None)) td.Methods; } ]

and convTypeDefs cenv mdefGen encl tdefs = 
  morphExpandILTypeDefs (convTypeDef cenv mdefGen encl) tdefs

let ConvModule ilg modul = 
  let cenv = new_cenv(ilg)
  let mdefGen = ref []
  let newTypes = convTypeDefs cenv mdefGen [] modul.TypeDefs
  {modul with TypeDefs=newTypes}

