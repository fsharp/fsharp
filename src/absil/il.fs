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

module internal Microsoft.FSharp.Compiler.AbstractIL.IL

#nowarn "49"
#nowarn "343" // The type 'ILAssemblyRef' implements 'System.IComparable' explicitly but provides no corresponding override for 'Object.Equals'.
#nowarn "346" // The struct, record or union type 'IlxExtensionType' has an explicit implementation of 'Object.Equals'. ...


open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.Internal
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open System.Collections.Generic
open System.Collections
 
let logging = false 

// Officially supported way to detect if we are running on Mono.
// See http://www.mono-project.com/FAQ:_Technical
// "How can I detect if am running in Mono?" section
let runningOnMono = 
    try
        System.Type.GetType("Mono.Runtime") <> null
    with e-> 
        // Must be robust in the case that someone else has installed a handler into System.AppDomain.OnTypeResolveEvent
        // that is not reliable.
        // This is related to bug 5506--the issue is actually a bug in VSTypeResolutionService.EnsurePopulated which is  
        // called by OnTypeResolveEvent. The function throws a NullReferenceException. I'm working with that team to get 
        // their issue fixed but we need to be robust here anyway.
        false        

let _ = if logging then dprintn "* warning: Il.logging is on"

let isNil x = match x with [] -> true | _ -> false
let nonNil x = match x with [] -> false | _ -> true
let int_order = LanguagePrimitives.FastGenericComparer<int>

let notlazy v = Lazy.CreateFromValue v

/// A little ugly, but the idea is that if a data structure does not 
/// contain lazy values then we don't add laziness.  So if the thing to map  
/// is already evaluated then immediately apply the function.  
let lazyMap f (x:Lazy<_>) =  
      if x.IsValueCreated then notlazy (f (x.Force())) else lazy (f (x.Force()))

// -------------------------------------------------------------------- 
// Utilities: type names
// -------------------------------------------------------------------- 

let splitNameAt nm idx = 
    if idx < 0 then failwith "splitNameAt: idx < 0";
    let last = String.length nm - 1 
    if idx > last then failwith "splitNameAt: idx > last";
    (nm.Substring(0,idx)),
    (if idx < last then nm.Substring (idx+1,last - idx) else "")

let rec splitNamespaceAux (nm:string) = 
    match nm.IndexOf '.' with 
    | -1 -> [nm]
    | idx -> 
        let s1,s2 = splitNameAt nm idx 
        s1::splitNamespaceAux s2 

/// Global State. All namespace splits ever seen
// ++GLOBAL MUTABLE STATE
let memoizeNamespaceTable = new Dictionary<string,string list>(10)

//  ++GLOBAL MUTABLE STATE
let memoizeNamespaceRightTable = new Dictionary<string,string option * string>(100)


let splitNamespace nm =
    let mutable res = Unchecked.defaultof<_>
    let ok = memoizeNamespaceTable.TryGetValue(nm,&res)
    if ok then res else
    let x = splitNamespaceAux nm
    (memoizeNamespaceTable.[nm] <- x; x)

let splitNamespaceMemoized nm = splitNamespace nm

// ++GLOBAL MUTABLE STATE
let memoizeNamespaceArrayTable = 
    Dictionary<string,_>(10)

let splitNamespaceToArray nm =
    let mutable res = Unchecked.defaultof<_>
    let ok = memoizeNamespaceArrayTable.TryGetValue(nm,&res)
    if ok then res else
    let x = Array.ofList (splitNamespace nm)
    (memoizeNamespaceArrayTable.[nm] <- x; x)


let splitTypeName (nm:string) = 
    match nm.LastIndexOf '.' with
    | -1 -> [],nm
    | idx -> 
        let s1,s2 = splitNameAt nm idx
        splitNamespace s1,s2

let emptyStringArray = ([| |] : string[])
let splitTypeNameToArray (nm:string) = 
    match nm.LastIndexOf '.' with
    | -1 -> emptyStringArray,nm
    | idx -> 
        let s1,s2 = splitNameAt nm idx
        splitNamespaceToArray s1,s2

let unsplitTypeName (ns,n) = 
    match ns with 
    | [] -> String.concat "." ns + "." + n 
    | _ -> n 

let splitTypeNameRightAux nm = 
    if String.contains nm '.' then 
      let idx = String.rindex nm '.'
      let s1,s2 = splitNameAt nm idx
      Some s1,s2 
    else None, nm

let splitTypeNameRight nm =
    let mutable res = Unchecked.defaultof<_>
    let ok = memoizeNamespaceRightTable.TryGetValue(nm,&res)
    if ok then res else
    let x = splitTypeNameRightAux nm
    (memoizeNamespaceRightTable.[nm] <- x; x)

// -------------------------------------------------------------------- 
// Ordered lists with a lookup table
// --------------------------------------------------------------------

/// This is used to store event, property and field maps.
///
/// Review: this is not such a great data structure.
type LazyOrderedMultiMap<'Key,'Data when 'Key : equality>(keyf : 'Data -> 'Key, lazyItems : Lazy<'Data list>) = 

    let quickMap= 
        lazyItems |> lazyMap (fun entries -> 
            let t = new Dictionary<_,_>(entries.Length, HashIdentity.Structural)
            do entries |> List.iter (fun y -> let key = keyf y in t.[key] <- y :: (if t.ContainsKey(key) then t.[key] else [])) 
            t)

    member self.Entries() = lazyItems.Force()

    member self.Add(y) = new LazyOrderedMultiMap<'Key,'Data>(keyf, lazyItems |> lazyMap (fun x -> y :: x))
    
    member self.Filter(f) = new LazyOrderedMultiMap<'Key,'Data>(keyf, lazyItems |> lazyMap (List.filter f))

    member self.Item with get(x) = let t = quickMap.Force() in if t.ContainsKey x then t.[x] else []


//---------------------------------------------------------------------
// SHA1 hash-signing algorithm.  Used to get the public key token from
// the public key.
//---------------------------------------------------------------------


let b0 n =  (n &&& 0xFF)
let b1 n =  ((n >>> 8) &&& 0xFF)
let b2 n =  ((n >>> 16) &&& 0xFF)
let b3 n =  ((n >>> 24) &&& 0xFF)


module SHA1 = 
    let inline (>>>&)  (x:int) (y:int)  = int32 (uint32 x >>> y)
    let f(t,b,c,d) = 
        if t < 20 then (b &&& c) ||| ((~~~b) &&& d) else
        if t < 40 then b ^^^ c ^^^ d else
        if t < 60 then (b &&& c) ||| (b &&& d) ||| (c &&& d) else
        b ^^^ c ^^^ d

    let k0to19 = 0x5A827999
    let k20to39 = 0x6ED9EBA1
    let k40to59 = 0x8F1BBCDC
    let k60to79 = 0xCA62C1D6

    let k t = 
        if t < 20 then k0to19 
        elif t < 40 then k20to39 
        elif t < 60 then k40to59 
        else k60to79 


    type chan = SHABytes of byte[] 
    type sha_instream = 
        { stream: chan;
          mutable pos: int;
          mutable eof:  bool; }

    let rot_left32 x n =  (x <<< n) ||| (x >>>& (32-n))

    let sha_eof sha = sha.eof

    (* padding and length (in bits!) recorded at end *)
    let sha_after_eof sha  = 
        let n = sha.pos
        let len = 
          (match sha.stream with
          | SHABytes s -> s.Length)
        if n = len then 0x80
        else 
          let padded_len = (((len + 9 + 63) / 64) * 64) - 8
          if n < padded_len - 8  then 0x0  
          elif (n &&& 63) = 56 then int32 ((int64 len * int64 8) >>> 56) &&& 0xff
          elif (n &&& 63) = 57 then int32 ((int64 len * int64 8) >>> 48) &&& 0xff
          elif (n &&& 63) = 58 then int32 ((int64 len * int64 8) >>> 40) &&& 0xff
          elif (n &&& 63) = 59 then int32 ((int64 len * int64 8) >>> 32) &&& 0xff
          elif (n &&& 63) = 60 then int32 ((int64 len * int64 8) >>> 24) &&& 0xff
          elif (n &&& 63) = 61 then int32 ((int64 len * int64 8) >>> 16) &&& 0xff
          elif (n &&& 63) = 62 then int32 ((int64 len * int64 8) >>> 8) &&& 0xff
          elif (n &&& 63) = 63 then (sha.eof <- true; int32 (int64 len * int64 8) &&& 0xff)
          else 0x0

    let sha_read8 sha = 
        let b = 
            match sha.stream with 
            | SHABytes s -> if sha.pos >= s.Length then sha_after_eof sha else int32 s.[sha.pos]
        sha.pos <- sha.pos + 1; 
        b
        
    let sha_read32 sha  = 
        let b0 = sha_read8 sha
        let b1 = sha_read8 sha
        let b2 = sha_read8 sha
        let b3 = sha_read8 sha
        let res = (b0 <<< 24) ||| (b1 <<< 16) ||| (b2 <<< 8) ||| b3
        res


    let sha1_hash sha = 
        let h0 = ref 0x67452301
        let h1 = ref 0xEFCDAB89
        let h2 = ref 0x98BADCFE
        let h3 = ref 0x10325476
        let h4 = ref 0xC3D2E1F0
        let a = ref 0
        let b = ref 0
        let c = ref 0
        let d = ref 0
        let e = ref 0
        let w = Array.create 80 0x00
        while (not (sha_eof sha)) do
          for i = 0 to 15 do
            w.[i] <- sha_read32 sha
          for t = 16 to 79 do
            w.[t] <- rot_left32 (w.[t-3] ^^^ w.[t-8] ^^^ w.[t-14] ^^^ w.[t-16]) 1;
          a := !h0; 
          b := !h1; 
          c := !h2; 
          d := !h3; 
          e := !h4;
          for t = 0 to 79 do
            let temp =  (rot_left32 !a 5) + f(t,!b,!c,!d) + !e + w.[t] + k(t)
            e := !d; 
            d := !c; 
            c :=  rot_left32 !b 30; 
            b := !a; 
            a := temp;
          h0 := !h0 + !a; 
          h1 := !h1 + !b; 
          h2 := !h2 + !c;  
          h3 := !h3 + !d; 
          h4 := !h4 + !e
        done;
        (!h0,!h1,!h2,!h3,!h4)

    let sha1HashBytes s = 
        let (_h0,_h1,_h2,h3,h4) = sha1_hash { stream = SHABytes s; pos = 0; eof = false }   // the result of the SHA algorithm is stored in registers 3 and 4
        Array.map byte [|  b0 h4; b1 h4; b2 h4; b3 h4; b0 h3; b1 h3; b2 h3; b3 h3; |]


let sha1HashBytes s = SHA1.sha1HashBytes s

// --------------------------------------------------------------------
// 
// -------------------------------------------------------------------- 

type ILVersionInfo = uint16 * uint16 * uint16 * uint16

type Locale = string

[<StructuralEquality; StructuralComparison>]
type PublicKey =
    | PublicKey of byte[]
    | PublicKeyToken of byte[]
    member x.IsKey=match x with PublicKey _ -> true | _ -> false
    member x.IsKeyToken=match x with PublicKeyToken _ -> true | _ -> false
    member x.Key=match x with PublicKey b -> b | _ -> invalidOp "not a key"
    member x.KeyToken=match x with PublicKeyToken b -> b | _ -> invalidOp"not a key token"

    member x.ToToken() = 
        match x with 
        | PublicKey bytes -> SHA1.sha1HashBytes bytes
        | PublicKeyToken token -> token
    static member KeyAsToken(k) = PublicKeyToken(PublicKey(k).ToToken())

[<StructuralEquality; StructuralComparison>]
type AssemblyRefData =
    { assemRefName: string;
      assemRefHash: byte[] option;
      assemRefPublicKeyInfo: PublicKey option;
      assemRefRetargetable: bool;
      assemRefVersion: ILVersionInfo option;
      assemRefLocale: Locale option; } 

/// Global state: table of all assembly references keyed by AssemblyRefData
let AssemblyRefUniqueStampGenerator = new UniqueStampGenerator<AssemblyRefData>()

[<Sealed>]
type ILAssemblyRef(data)  =  
    let uniqueStamp = AssemblyRefUniqueStampGenerator.Encode(data)
    member x.Name=data.assemRefName
    member x.Hash=data.assemRefHash
    member x.PublicKey=data.assemRefPublicKeyInfo
    member x.Retargetable=data.assemRefRetargetable  
    member x.Version=data.assemRefVersion
    member x.Locale=data.assemRefLocale
    member x.UniqueStamp=uniqueStamp
    override x.GetHashCode() = uniqueStamp
    override x.Equals(yobj) = ((yobj :?> ILAssemblyRef).UniqueStamp = uniqueStamp)
    interface System.IComparable with
        override x.CompareTo(yobj) = compare (yobj :?> ILAssemblyRef).UniqueStamp uniqueStamp
    static member Create(name,hash,publicKey,retargetable,version,locale) =
        ILAssemblyRef
            { assemRefName=name;
              assemRefHash=hash;
              assemRefPublicKeyInfo=publicKey;
              assemRefRetargetable=retargetable;
              assemRefVersion=version;
              assemRefLocale=locale; } 

    static member FromAssembly(assembly:System.Reflection.Assembly) =
// SILVERLIGHT-TODO: Needs to account for empty versions, public keys    
#if SILVERLIGHT
        let fullName = assembly.ToString()
        let components = fullName.Split([|','|])
        // name, version, culture, public key token
        let nm = components.[0]
        let v = System.Version(components.[1].Split([|'='|]).[1])
        let version : ILVersionInfo option = Some((uint16 v.Major,uint16 v.Minor,uint16 v.Build,uint16 v.Revision))
        let keyString = components.[3].Split([|'='|]).[1]
        let keyBytes = Bytes.stringAsUtf8NullTerminated keyString
        let key = Some(PublicKeyToken(keyBytes.[0..keyBytes.Length-2])) // remove null terminator
        ILAssemblyRef.Create(nm,None,key,false,version,None)
        
#else
        let aname = assembly.GetName()
        let locale = None
        //match aname.CultureInfo with 
        //   | null -> None 
        //   | x -> Some x.Name
        let publicKey = 
           match aname.GetPublicKey()  with 
           | null | [| |] -> 
               match aname.GetPublicKeyToken()  with 
               | null | [| |] -> None
               | bytes -> Some (PublicKeyToken bytes)
           | bytes -> 
               Some (PublicKey bytes)
        
        let version = 
           match aname.Version with 
           | null -> None
           | v -> Some (uint16 v.Major,uint16 v.Minor,uint16 v.Build,uint16 v.Revision)

        ILAssemblyRef.Create(aname.Name,None,publicKey,false,version,locale)
#endif

    member aref.QualifiedName = 
        let b = new System.Text.StringBuilder(100)
        let add (s:string) = (b.Append(s) |> ignore)
        let addC (s:char) = (b.Append(s) |> ignore)
        add(aref.Name);
        match aref.Version with 
        | None -> ()
        | Some (a,b,c,d) -> 
            add ", Version=";
            add (string (int a))
            add ".";
            add (string (int b))
            add ".";
            add (string (int c))
            add ".";
            add (string (int d))
            add ", Culture="
            match aref.Locale with 
            | None -> add "neutral"
            | Some b -> add b
            add ", PublicKeyToken="
            match aref.PublicKey with 
            | None -> add "null"
            | Some pki -> 
                  let pkt = pki.ToToken()
                  let convDigit(digit) = 
                      let digitc = 
                          if digit < 10 
                          then  System.Convert.ToInt32 '0' + digit 
                          else System.Convert.ToInt32 'a' + (digit - 10) 
                      System.Convert.ToChar(digitc)
                  for i = 0 to pkt.Length-1 do
                      let v = pkt.[i]
                      addC (convDigit(System.Convert.ToInt32(v)/16))
                      addC (convDigit(System.Convert.ToInt32(v)%16))
        b.ToString()


[<StructuralEquality; StructuralComparison>]
type ILModuleRef = 
    { name: string;
      hasMetadata: bool; 
      hash: byte[] option; }
    static member Create(name,hasMetadata,hash) = 
        { name=name;
          hasMetadata= hasMetadata;
          hash=hash }
    
    member x.Name=x.name
    member x.HasMetadata=x.hasMetadata
    member x.Hash=x.hash 

[<StructuralEquality; StructuralComparison>]
[<RequireQualifiedAccess>]
type ILScopeRef = 
    | Local
    | Module of ILModuleRef 
    | Assembly of ILAssemblyRef
    member x.IsLocalRef   = match x with ILScopeRef.Local      -> true | _ -> false
    member x.IsModuleRef  = match x with ILScopeRef.Module _   -> true | _ -> false
    member x.IsAssemblyRef= match x with ILScopeRef.Assembly _ -> true | _ -> false
    member x.ModuleRef    = match x with ILScopeRef.Module x   -> x | _ -> failwith "not a module reference"
    member x.AssemblyRef  = match x with ILScopeRef.Assembly x -> x | _ -> failwith "not an assembly reference"

    member scoref.QualifiedName = 
        match scoref with 
        | ILScopeRef.Local -> ""
        | ILScopeRef.Module mref -> "module "^mref.Name
        | ILScopeRef.Assembly aref when aref.Name = "mscorlib" -> ""
        | ILScopeRef.Assembly aref -> aref.QualifiedName

    member scoref.QualifiedNameWithNoShortMscorlib = 
        match scoref with 
        | ILScopeRef.Local -> ""
        | ILScopeRef.Module mref -> "module "+mref.Name
        | ILScopeRef.Assembly aref -> aref.QualifiedName

type ILArrayBound = int32 option 
type ILArrayBounds = ILArrayBound * ILArrayBound

[<StructuralEquality; StructuralComparison>]
type ILArrayShape = 
    | ILArrayShape of ILArrayBounds list (* lobound/size pairs *)
    member x.Rank = (let (ILArrayShape l) = x in l.Length)
    static member SingleDimensional = ILArrayShapeStatics.SingleDimensional
    static member FromRank n = if n = 1 then ILArrayShape.SingleDimensional else ILArrayShape(List.replicate n (None,None))


and ILArrayShapeStatics() = 
    static let singleDimensional = ILArrayShape [(Some 0, None)]    
    static member SingleDimensional = singleDimensional

/// Calling conventions.  These are used in method pointer types.
[<StructuralEquality; StructuralComparison; RequireQualifiedAccess>]
type ILArgConvention = 
    | Default
    | CDecl 
    | StdCall 
    | ThisCall 
    | FastCall 
    | VarArg
      
[<StructuralEquality; StructuralComparison; RequireQualifiedAccess>]
type ILThisConvention =
    | Instance
    | InstanceExplicit
    | Static

let mutable instance_callconv : obj = new obj()
let mutable static_callconv : obj = new obj()

[<StructuralEquality; StructuralComparison>]
type ILCallingConv =
    | Callconv of ILThisConvention * ILArgConvention
    member x.ThisConv           = let (Callconv(a,_b)) = x in a
    member x.BasicConv          = let (Callconv(_a,b)) = x in b
    member x.IsInstance         = match x.ThisConv with ILThisConvention.Instance -> true | _ -> false
    member x.IsInstanceExplicit = match x.ThisConv with ILThisConvention.InstanceExplicit -> true | _ -> false
    member x.IsStatic           = match x.ThisConv with ILThisConvention.Static -> true | _ -> false

    static member Instance : ILCallingConv = unbox(instance_callconv) 
    static member Static : ILCallingConv = unbox(static_callconv) 

do instance_callconv <- box (Callconv(ILThisConvention.Instance,ILArgConvention.Default))
do static_callconv <- box (Callconv(ILThisConvention.Static,ILArgConvention.Default))

type ILBoxity = 
  | AsObject 
  | AsValue


// IL type references have a pre-computed hash code to enable quick lookup tables during binary generation.
[<CustomEquality; CustomComparison>]
type ILTypeRef = 
    { trefScope: ILScopeRef;
      trefEnclosing: string list;
      trefName: string; 
      hashCode : int }
      
    static member Create(scope,enclosing,name) = 
        let hashCode = hash scope * 17 ^^^ (hash enclosing * 101 <<< 1) ^^^ (hash name * 47 <<< 2)
        { trefScope=scope;
          trefEnclosing= enclosing;
          trefName=name;
          hashCode=hashCode }
          
    member x.Scope= x.trefScope
    member x.Enclosing= x.trefEnclosing
    member x.Name=x.trefName
    member x.ApproxId= x.hashCode
    override x.GetHashCode() = x.hashCode
    override x.Equals(yobj) = 
         let y = (yobj :?> ILTypeRef) 
         (x.ApproxId = y.ApproxId) && 
         (x.Scope = y.Scope) && 
         (x.Name = y.Name) && 
         (x.Enclosing = y.Enclosing)
    interface System.IComparable with
        override x.CompareTo(yobj) = 
            let y = (yobj :?> ILTypeRef) 
            let c = compare x.ApproxId y.ApproxId
            if c <> 0 then c else
            let c = compare x.Scope y.Scope
            if c <> 0 then c else
            let c = compare x.Name y.Name 
            if c <> 0 then c else
            compare x.Enclosing y.Enclosing
        
    member tref.FullName = String.concat "." (tref.Enclosing @ [tref.Name])
        
    member tref.BasicQualifiedName = 
        String.concat "+" (tref.Enclosing @ [ tref.Name ])

    member tref.AddQualifiedNameExtensionWithNoShortMscorlib(basic) = 
        let sco = tref.Scope.QualifiedNameWithNoShortMscorlib
        if sco = "" then basic else String.concat ", " [basic;sco]

    member tref.QualifiedNameWithNoShortMscorlib = 
        tref.AddQualifiedNameExtensionWithNoShortMscorlib(tref.BasicQualifiedName)

    member tref.QualifiedName = 
        let basic = tref.BasicQualifiedName
        let sco = tref.Scope.QualifiedName
        if sco = "" then basic else String.concat ", " [basic;sco]


    override x.ToString() = x.FullName

        
type 
    [<StructuralEquality; StructuralComparison>]
    ILTypeSpec = 
    { tspecTypeRef: ILTypeRef;    
      /// The type instantiation if the type is generic
      tspecInst: ILGenericArgs }    
    member x.TypeRef=x.tspecTypeRef
    member x.Scope=x.TypeRef.Scope
    member x.Enclosing=x.TypeRef.Enclosing
    member x.Name=x.TypeRef.Name
    member x.GenericArgs=x.tspecInst
    static member Create(tref,inst) = { tspecTypeRef =tref; tspecInst=inst }
    override x.ToString() = x.TypeRef.ToString() + (match x.GenericArgs with [] -> "" | _ -> "<...>")
    member x.BasicQualifiedName = 
        let tc = x.TypeRef.BasicQualifiedName
        match x.GenericArgs with 
        | [] -> tc
        | args -> tc + "[" + String.concat "," (args |> List.map (fun arg -> "[" + arg.QualifiedNameWithNoShortMscorlib + "]")) + "]"

    member x.AddQualifiedNameExtensionWithNoShortMscorlib(basic) = 
        x.TypeRef.AddQualifiedNameExtensionWithNoShortMscorlib(basic)

    member x.FullName=x.TypeRef.FullName

and [<RequireQualifiedAccess; StructuralEquality; StructuralComparison>]
    ILType =
    | Void                   
    | Array    of ILArrayShape * ILType 
    | Value    of ILTypeSpec      
    | Boxed    of ILTypeSpec      
    | Ptr      of ILType             
    | Byref    of ILType           
    | FunctionPointer     of ILCallingSignature 
    | TypeVar    of uint16              
    | Modified of bool * ILTypeRef * ILType

    member x.BasicQualifiedName = 
        match x with 
        | ILType.TypeVar n -> "!" + string n
        | ILType.Modified(_,_ty1,ty2) -> ty2.BasicQualifiedName
        | ILType.Array (ILArrayShape(s),ty) -> ty.BasicQualifiedName + "[" + System.String(',',s.Length-1) + "]"
        | ILType.Value tr | ILType.Boxed tr -> tr.BasicQualifiedName
        | ILType.Void -> "void"
        | ILType.Ptr _ty -> failwith "unexpected pointer type"
        | ILType.Byref _ty -> failwith "unexpected byref type"
        | ILType.FunctionPointer _mref -> failwith "unexpected function pointer type"

    member x.AddQualifiedNameExtensionWithNoShortMscorlib(basic) = 
        match x with 
        | ILType.TypeVar _n -> basic
        | ILType.Modified(_,_ty1,ty2) -> ty2.AddQualifiedNameExtensionWithNoShortMscorlib(basic)
        | ILType.Array (ILArrayShape(_s),ty) -> ty.AddQualifiedNameExtensionWithNoShortMscorlib(basic)
        | ILType.Value tr | ILType.Boxed tr -> tr.AddQualifiedNameExtensionWithNoShortMscorlib(basic)
        | ILType.Void -> failwith "void"
        | ILType.Ptr _ty -> failwith "unexpected pointer type"
        | ILType.Byref _ty -> failwith "unexpected byref type"
        | ILType.FunctionPointer _mref -> failwith "unexpected function pointer type"
        
    member x.QualifiedNameWithNoShortMscorlib = 
        x.AddQualifiedNameExtensionWithNoShortMscorlib(x.BasicQualifiedName)

and 
    [<CustomEquality; CustomComparison>]
    IlxExtensionType = 
    | Ext_typ of obj
    member x.Value = (let (Ext_typ(v)) = x in v)
    override x.Equals(yobj) = match yobj with :? IlxExtensionType as y -> Unchecked.equals x.Value y.Value | _ -> false
    interface System.IComparable with
        override x.CompareTo(yobj) = match yobj with :? IlxExtensionType as y -> Unchecked.compare x.Value y.Value | _ -> invalidOp "bad comparison"

and [<StructuralEquality; StructuralComparison>]
    ILCallingSignature = 
    { CallingConv: ILCallingConv;
      ArgTypes: ILType list;
      ReturnType: ILType }

and ILGenericArgs = ILType list

let mkILCallSig (cc,args,ret) = { ArgTypes=args; CallingConv=cc; ReturnType=ret}


type ILMethodRef =
    { mrefParent: ILTypeRef;
      mrefCallconv: ILCallingConv;
      mrefGenericArity: int; 
      mrefName: string;
      mrefArgs: ILType list;
      mrefReturn: ILType }
    member x.EnclosingTypeRef = x.mrefParent
    member x.CallingConv = x.mrefCallconv
    member x.Name = x.mrefName
    member x.GenericArity = x.mrefGenericArity
    member x.ArgCount = x.mrefArgs.Length
    member x.ArgTypes = x.mrefArgs
    member x.ReturnType = x.mrefReturn

    member x.CallingSignature = mkILCallSig (x.CallingConv,x.ArgTypes,x.ReturnType)
    static member Create(a,b,c,d,e,f) = 
        { mrefParent= a;mrefCallconv=b;mrefName=c;mrefGenericArity=d; mrefArgs=e;mrefReturn=f }
    override x.ToString() = x.EnclosingTypeRef.ToString() + "::" + x.Name + "(...)"


[<StructuralEquality; StructuralComparison>]
type ILFieldRef = 
    { EnclosingTypeRef: ILTypeRef;
      Name: string;
      Type: ILType }
    override x.ToString() = x.EnclosingTypeRef.ToString() + "::" + x.Name

[<StructuralEquality; StructuralComparison>]
type ILMethodSpec = 
    { mspecMethodRef: ILMethodRef;
      mspecEnclosingType: ILType;          
      mspecMethodInst: ILGenericArgs; }     
    static member Create(a,b,c) = { mspecEnclosingType=a; mspecMethodRef =b; mspecMethodInst=c }
    member x.MethodRef = x.mspecMethodRef
    member x.EnclosingType=x.mspecEnclosingType
    member x.GenericArgs=x.mspecMethodInst
    member x.Name=x.MethodRef.Name
    member x.CallingConv=x.MethodRef.CallingConv
    member x.GenericArity = x.MethodRef.GenericArity
    member x.FormalArgTypes = x.MethodRef.ArgTypes
    member x.FormalReturnType = x.MethodRef.ReturnType
    override x.ToString() = x.MethodRef.ToString() + "(...)"


type ILFieldSpec =
    { FieldRef: ILFieldRef;
      EnclosingType: ILType }         
    member x.FormalType       = x.FieldRef.Type
    member x.Name             = x.FieldRef.Name
    member x.EnclosingTypeRef = x.FieldRef.EnclosingTypeRef
    override x.ToString() = x.FieldRef.ToString()


// --------------------------------------------------------------------
// Debug info.                                                     
// -------------------------------------------------------------------- 

type Guid =  byte[]

type ILPlatform = 
    | X86
    | AMD64
    | IA64

type ILSourceDocument = 
    { sourceLanguage: Guid option; 
      sourceVendor: Guid option;
      sourceDocType: Guid option;
      sourceFile: string; }
    static member Create(language,vendor,docType,file) =
        { sourceLanguage=language; 
          sourceVendor=vendor;
          sourceDocType=docType;
          sourceFile=file; }
    member x.Language=x.sourceLanguage
    member x.Vendor=x.sourceVendor
    member x.DocumentType=x.sourceDocType
    member x.File=x.sourceFile

type ILSourceMarker =
    { sourceDocument: ILSourceDocument;
      sourceLine: int;
      sourceColumn: int;
      sourceEndLine: int;
      sourceEndColumn: int }
    static member Create(document, line, column, endLine, endColumn) = 
        { sourceDocument=document;
          sourceLine=line;
          sourceColumn=column;
          sourceEndLine=endLine;
          sourceEndColumn=endColumn }
    member x.Document=x.sourceDocument
    member x.Line=x.sourceLine
    member x.Column=x.sourceColumn
    member x.EndLine=x.sourceEndLine
    member x.EndColumn=x.sourceEndColumn
    override x.ToString() = sprintf "(%d,%d)-(%d,%d)" x.Line x.Column x.EndLine x.EndColumn

// --------------------------------------------------------------------
// Custom attributes                                                     
// -------------------------------------------------------------------- 

type ILAttribElem =  
  | String of string  option
  | Bool of bool
  | Char of char
  | SByte of int8
  | Int16 of int16
  | Int32 of int32
  | Int64 of int64
  | Byte of uint8
  | UInt16 of uint16
  | UInt32 of uint32
  | UInt64 of uint64
  | Single of single
  | Double of double
  | Null 
  | Type of ILType option
  | TypeRef of ILTypeRef option
  | Array of ILType * ILAttribElem list

type ILAttributeNamedArg =  (string * ILType * bool * ILAttribElem)
type ILAttribute = 
    { Method: ILMethodSpec;
#if SILVERLIGHT
      Arguments: ILAttribElem list * ILAttributeNamedArg list
#endif
      Data: byte[] }

[<NoEquality; NoComparison>]
type ILAttributes = 
   | CustomAttrs of Lazy<ILAttribute list>
   member x.AsList = let (CustomAttrs m) = x in m.Force()

type ILCodeLabel = int

// --------------------------------------------------------------------
// Instruction set.                                                     
// -------------------------------------------------------------------- 

type ILBasicType =
  | DT_R
  | DT_I1
  | DT_U1
  | DT_I2
  | DT_U2
  | DT_I4
  | DT_U4
  | DT_I8
  | DT_U8
  | DT_R4
  | DT_R8
  | DT_I
  | DT_U
  | DT_REF

[<StructuralEquality; StructuralComparison; RequireQualifiedAccess>]
type ILToken = 
  | ILType of ILType 
  | ILMethod of ILMethodSpec 
  | ILField of ILFieldSpec

[<StructuralEquality; StructuralComparison; RequireQualifiedAccess>]
type ILConst = 
  | I4 of int32
  | I8 of int64
  | R4 of single
  | R8 of double

type ILTailcall = 
  | Tailcall
  | Normalcall

type ILAlignment =  
  | Aligned
  | Unaligned1
  | Unaligned2
  | Unaligned4

type ILVolatility =  
  | Volatile
  | Nonvolatile

type ILReadonly =  
  | ReadonlyAddress
  | NormalAddress

type ILVarArgs = ILType list option

[<StructuralEquality; StructuralComparison>]
type ILComparisonInstr = 
  | BI_beq        
  | BI_bge        
  | BI_bge_un     
  | BI_bgt        
  | BI_bgt_un        
  | BI_ble        
  | BI_ble_un        
  | BI_blt        
  | BI_blt_un 
  | BI_bne_un 
  | BI_brfalse 
  | BI_brtrue 


[<StructuralEquality; NoComparison>]
type ILInstr = 
  | AI_add    
  | AI_add_ovf
  | AI_add_ovf_un
  | AI_and    
  | AI_div   
  | AI_div_un
  | AI_ceq      
  | AI_cgt      
  | AI_cgt_un   
  | AI_clt     
  | AI_clt_un  
  | AI_conv      of ILBasicType
  | AI_conv_ovf  of ILBasicType
  | AI_conv_ovf_un  of ILBasicType
  | AI_mul       
  | AI_mul_ovf   
  | AI_mul_ovf_un
  | AI_rem       
  | AI_rem_un       
  | AI_shl       
  | AI_shr       
  | AI_shr_un
  | AI_sub       
  | AI_sub_ovf   
  | AI_sub_ovf_un   
  | AI_xor       
  | AI_or        
  | AI_neg       
  | AI_not       
  | AI_ldnull    
  | AI_dup       
  | AI_pop
  | AI_ckfinite 
  | AI_nop
  | AI_ldc of ILBasicType * ILConst 
  | I_ldarg     of uint16
  | I_ldarga    of uint16
  | I_ldind     of ILAlignment * ILVolatility * ILBasicType
  | I_ldloc     of uint16
  | I_ldloca    of uint16
  | I_starg     of uint16
  | I_stind     of  ILAlignment * ILVolatility * ILBasicType
  | I_stloc     of uint16

  | I_br    of  ILCodeLabel
  | I_jmp   of ILMethodSpec
  | I_brcmp of ILComparisonInstr * ILCodeLabel * ILCodeLabel (* second label is fall-through *)
  | I_switch    of (ILCodeLabel list * ILCodeLabel) (* last label is fallthrough *)
  | I_ret 

  | I_call     of ILTailcall * ILMethodSpec * ILVarArgs
  | I_callvirt of ILTailcall * ILMethodSpec * ILVarArgs
  | I_callconstraint of ILTailcall * ILType * ILMethodSpec * ILVarArgs
  | I_calli    of ILTailcall * ILCallingSignature * ILVarArgs
  | I_ldftn    of ILMethodSpec
  | I_newobj  of ILMethodSpec  * ILVarArgs
  
  | I_throw
  | I_endfinally
  | I_endfilter
  | I_leave     of  ILCodeLabel
  | I_rethrow

  | I_ldsfld      of ILVolatility * ILFieldSpec
  | I_ldfld       of ILAlignment * ILVolatility * ILFieldSpec
  | I_ldsflda     of ILFieldSpec
  | I_ldflda      of ILFieldSpec 
  | I_stsfld      of ILVolatility  *  ILFieldSpec
  | I_stfld       of ILAlignment * ILVolatility * ILFieldSpec
  | I_ldstr       of string
  | I_isinst      of ILType
  | I_castclass   of ILType
  | I_ldtoken     of ILToken
  | I_ldvirtftn   of ILMethodSpec

  | I_cpobj       of ILType
  | I_initobj     of ILType
  | I_ldobj       of ILAlignment * ILVolatility * ILType
  | I_stobj       of ILAlignment * ILVolatility * ILType
  | I_box         of ILType
  | I_unbox       of ILType
  | I_unbox_any   of ILType
  | I_sizeof      of ILType

  | I_ldelem      of ILBasicType
  | I_stelem      of ILBasicType
  | I_ldelema     of ILReadonly * ILArrayShape * ILType
  | I_ldelem_any  of ILArrayShape * ILType
  | I_stelem_any  of ILArrayShape * ILType
  | I_newarr      of ILArrayShape * ILType 
  | I_ldlen

  | I_mkrefany    of ILType
  | I_refanytype  
  | I_refanyval   of ILType

  | I_break 
  | I_seqpoint of ILSourceMarker

  | I_arglist  

  | I_localloc
  | I_cpblk of ILAlignment * ILVolatility
  | I_initblk of ILAlignment  * ILVolatility

  (* FOR EXTENSIONS, e.g. MS-ILX *)  
  | EI_ilzero of ILType
  | EI_ldlen_multi      of int32 * int32
  | I_other    of IlxExtensionInstr

and IlxExtensionInstr = Ext_instr of obj


// -------------------------------------------------------------------- 
// Helpers for the ILX extensions
// -------------------------------------------------------------------- 

type internal_instr_extension = 
    { internalInstrExtIs: IlxExtensionInstr -> bool; 
      internalInstrExtDests: IlxExtensionInstr -> ILCodeLabel list;
      internalInstrExtFallthrough: IlxExtensionInstr -> ILCodeLabel option;
      internalInstrExtIsTailcall: IlxExtensionInstr -> bool;
      internalInstrExtRelabel: (ILCodeLabel -> ILCodeLabel) -> IlxExtensionInstr -> IlxExtensionInstr; }

type ILInstrSetExtension<'T> = 
    { instrExtDests: 'T -> ILCodeLabel list;
      instrExtFallthrough: 'T -> ILCodeLabel option;
      instrExtIsTailcall: 'T -> bool;
      instrExtRelabel: (ILCodeLabel -> ILCodeLabel) -> 'T -> 'T; }

let instrExtensions = ref []

let RegisterInstructionSetExtension  (ext: ILInstrSetExtension<'T>) = 
    if nonNil !instrExtensions then failwith "RegisterInstructionSetExtension: only one extension currently allowed";
    let mk (x: 'T) = Ext_instr (box x)
    let test (Ext_instr _x) = true
    let dest (Ext_instr x) = (unbox x : 'T)
    instrExtensions := 
       { internalInstrExtIs=test;
         internalInstrExtDests=(fun x -> ext.instrExtDests (dest x));
         internalInstrExtFallthrough=(fun x -> ext.instrExtFallthrough (dest x));
         internalInstrExtIsTailcall=(fun x -> ext.instrExtIsTailcall (dest x));
         internalInstrExtRelabel=(fun f x -> mk (ext.instrExtRelabel f (dest x))); }
         :: !instrExtensions;
    mk,test,dest

let rec find_extension s f l = 
  let rec look l1 = 
    match l1 with
    | [] -> failwith ("extension for "+s+" not found")
    | (h::t) -> match f h with None -> look t | Some res -> res 
  look l
          

type ILDebugMapping = 
    { LocalIndex: int;
      LocalName: string; }

type ILBasicBlock = 
    { Label: ILCodeLabel;
      Instructions: ILInstr array }
    member bb.LastInstruction = 
      let n = bb.Instructions.Length
      if n = 0 then failwith "last_of_bblock: empty bblock";
      bb.Instructions.[n - 1]

    member x.Fallthrough = 
        match x.LastInstruction with 
        | I_br l | I_brcmp (_,_,l) | I_switch (_,l) -> Some l
        | I_other e -> find_extension "instr" (fun ext -> if ext.internalInstrExtIs e then Some (ext.internalInstrExtFallthrough e) else None) !instrExtensions
        | _ -> None


type ILCode = 
    | ILBasicBlock    of ILBasicBlock
    | GroupBlock    of ILDebugMapping list * ILCode list
    | RestrictBlock of ILCodeLabel list * ILCode
    | TryBlock      of ILCode * ILExceptionBlock

and ILExceptionBlock = 
    | FaultBlock       of ILCode 
    | FinallyBlock     of ILCode
    | FilterCatchBlock of (ILFilterBlock * ILCode) list

and ILFilterBlock = 
    | TypeFilter of ILType
    | CodeFilter of ILCode

type ILLocal = 
    { Type: ILType;
      IsPinned: bool }
      
type ILMethodBody = 
    { IsZeroInit: bool;
      MaxStack: int32;
      NoInlining: bool;
      Locals: ILLocal list;
      Code:  ILCode;
      SourceMarker: ILSourceMarker option }

[<RequireQualifiedAccess>]
type ILMemberAccess = 
    | Assembly
    | CompilerControlled
    | FamilyAndAssembly
    | FamilyOrAssembly
    | Family
    | Private 
    | Public 

[<StructuralEquality; StructuralComparison>]
[<RequireQualifiedAccess>]
type ILFieldInit = 
    | String of string
    | Bool of bool
    | Char of uint16
    | Int8 of int8
    | Int16 of int16
    | Int32 of int32
    | Int64 of int64
    | UInt8 of uint8
    | UInt16 of uint16
    | UInt32 of uint32
    | UInt64 of uint64
    | Single of single
    | Double of double
    | Null
  
// -------------------------------------------------------------------- 
// Native Types, for marshalling to the native C interface.
// These are taken directly from the ILASM syntax, and don't really
// correspond yet to the ECMA Spec (Partition II, 7.4).  
// -------------------------------------------------------------------- 

[<RequireQualifiedAccess>]
[<StructuralEquality; StructuralComparison>]
type ILNativeType = 
    | Empty
    | Custom of Guid * string * string * byte[] (* guid,nativeTypeName,custMarshallerName,cookieString *)
    | FixedSysString of int32
    | FixedArray of int32
    | Currency
    | LPSTR
    | LPWSTR
    | LPTSTR
    | ByValStr
    | TBSTR
    | LPSTRUCT
    | Struct
    | Void
    | Bool
    | Int8
    | Int16
    | Int32
    | Int64
    | Single
    | Double
    | Byte
    | UInt16
    | UInt32
    | UInt64
    | Array of ILNativeType option * (int32 * int32 option) option (* optional idx of parameter giving size plus optional additive i.e. num elems *)
    | Int
    | UInt
    | Method
    | AsAny
    | BSTR
    | IUnknown
    | IDispatch
    | Interface
    | Error               
    | SafeArray of ILNativeVariant * string option 
    | ANSIBSTR
    | VariantBool


and ILNativeVariant = 
    | Empty
    | Null
    | Variant
    | Currency
    | Decimal               
    | Date               
    | BSTR               
    | LPSTR               
    | LPWSTR               
    | IUnknown               
    | IDispatch               
    | SafeArray               
    | Error               
    | HRESULT               
    | CArray               
    | UserDefined               
    | Record               
    | FileTime
    | Blob               
    | Stream               
    | Storage               
    | StreamedObject               
    | StoredObject               
    | BlobObject               
    | CF                
    | CLSID
    | Void 
    | Bool
    | Int8
    | Int16                
    | Int32                
    | Int64                
    | Single                
    | Double                
    | UInt8                
    | UInt16                
    | UInt32                
    | UInt64                
    | PTR                
    | Array of ILNativeVariant                
    | Vector of ILNativeVariant                
    | Byref of ILNativeVariant                
    | Int                
    | UInt                

type ILSecurityAction = 
    | Request 
    | Demand
    | Assert
    | Deny
    | PermitOnly
    | LinkCheck 
    | InheritCheck
    | ReqMin
    | ReqOpt
    | ReqRefuse
    | PreJitGrant
    | PreJitDeny
    | NonCasDemand
    | NonCasLinkDemand
    | NonCasInheritance
    | LinkDemandChoice
    | InheritanceDemandChoice
    | DemandChoice

type ILPermission = 
    | PermissionSet of ILSecurityAction * byte[]

type ILPermissions =
    | SecurityDecls of Lazy<ILPermission list>
    member x.AsList = let (SecurityDecls m) = x in m.Force()

[<RequireQualifiedAccess>]
type PInvokeCharBestFit  = 
    | UseAssembly
    | Enabled
    | Disabled

[<RequireQualifiedAccess>]
type PInvokeThrowOnUnmappableChar =
    | UseAssembly
    | Enabled
    | Disabled

[<RequireQualifiedAccess>]
type PInvokeCallingConvention =
    | None
    | Cdecl
    | Stdcall
    | Thiscall
    | Fastcall
    | WinApi

[<RequireQualifiedAccess>]
type PInvokeCharEncoding =
    | None
    | Ansi
    | Unicode
    | Auto

type PInvokeMethod =
    { Where: ILModuleRef;
      Name: string;
      CallingConv: PInvokeCallingConvention;
      CharEncoding: PInvokeCharEncoding;
      NoMangle: bool;
      LastError: bool;
      ThrowOnUnmappableChar: PInvokeThrowOnUnmappableChar;
      CharBestFit: PInvokeCharBestFit }

type ILParameter =
    { Name: string option;
      Type: ILType;
      Default: ILFieldInit option;  
      Marshal: ILNativeType option; 
      IsIn: bool;
      IsOut: bool;
      IsOptional: bool;
      CustomAttrs: ILAttributes }


type ILReturn = 
    { Marshal: ILNativeType option;
      Type: ILType; 
      CustomAttrs: ILAttributes }

type ILOverridesSpec = 
    | OverridesSpec of ILMethodRef * ILType
    member x.MethodRef = let (OverridesSpec(mr,_ty)) = x in mr
    member x.EnclosingType = let (OverridesSpec(_mr,ty)) = x in ty

type ILMethodVirtualInfo = 
    { IsFinal: bool
      IsNewSlot: bool 
      IsCheckAccessOnOverride: bool
      IsAbstract: bool }

type MethodKind =
    | Static 
    | Cctor 
    | Ctor 
    | NonVirtual 
    | Virtual of ILMethodVirtualInfo

[<RequireQualifiedAccess>]
type MethodBody =
    | IL of ILMethodBody
    | PInvoke of PInvokeMethod       (* platform invoke to native  *)
    | Abstract
    | Native

type ILLazyMethodBody = 
    | ILLazyMethodBody of Lazy<MethodBody >
    member x.Contents = let (ILLazyMethodBody mb) = x in mb.Force()

[<RequireQualifiedAccess>]
type MethodCodeKind =
    | IL
    | Native
    | Runtime

let mkMethBodyAux mb = ILLazyMethodBody (Lazy.CreateFromValue mb)
let mkMethBodyLazyAux mb = ILLazyMethodBody mb

let typesOfILParams (ps:ILParameter list) = ps |> List.map (fun p -> p.Type) 

[<StructuralEquality; StructuralComparison>]
type ILGenericVariance = 
    | NonVariant            
    | CoVariant             
    | ContraVariant         

type ILGenericParameterDef =
    { Name: string;
      Constraints: ILType list;
      Variance: ILGenericVariance; 
      HasReferenceTypeConstraint: bool;     
      CustomAttrs : ILAttributes;
      HasNotNullableValueTypeConstraint: bool;
      HasDefaultConstructorConstraint: bool; }

    override x.ToString() = x.Name 

type ILGenericParameterDefs = ILGenericParameterDef list

type ILMethodDef = 
    { Name: string;
      mdKind: MethodKind;
      CallingConv: ILCallingConv;
      Parameters: ILParameter list;
      Return: ILReturn;
      Access: ILMemberAccess;
      mdBody: ILLazyMethodBody;   
      mdCodeKind: MethodCodeKind;   
      IsInternalCall: bool;
      IsManaged: bool;
      IsForwardRef: bool;
      SecurityDecls: ILPermissions;
      HasSecurity: bool;
      IsEntryPoint:bool;
      IsReqSecObj: bool;
      IsHideBySig: bool;
      IsSpecialName: bool;
      IsUnmanagedExport: bool;
      IsSynchronized: bool;
      IsPreserveSig: bool;
      IsMustRun: bool; 
      GenericParams: ILGenericParameterDefs;
      CustomAttrs: ILAttributes; }
    member x.ParameterTypes = typesOfILParams x.Parameters
    // Whidbey feature: SafeHandle finalizer must be run 
    member md.Code = 
          match md.mdBody.Contents with 
          | MethodBody.IL il-> Some il.Code
          | _ -> None
    member x.IsIL = match x.mdBody.Contents with | MethodBody.IL _ -> true | _ -> false
    member x.Locals = match x.mdBody.Contents with | MethodBody.IL il -> il.Locals | _ -> []

    member x.MethodBody = match x.mdBody.Contents with MethodBody.IL il -> il | _ -> failwith "not IL"

    member x.IsNoInline   = x.MethodBody.NoInlining  
    member x.SourceMarker = x.MethodBody.SourceMarker
    member x.MaxStack     = x.MethodBody.MaxStack  
    member x.IsZeroInit   = x.MethodBody.IsZeroInit

    member x.IsClassInitializer   = match x.mdKind with | MethodKind.Cctor      -> true | _ -> false
    member x.IsConstructor        = match x.mdKind with | MethodKind.Ctor       -> true | _ -> false
    member x.IsStatic             = match x.mdKind with | MethodKind.Static     -> true | _ -> false
    member x.IsNonVirtualInstance = match x.mdKind with | MethodKind.NonVirtual -> true | _ -> false
    member x.IsVirtual            = match x.mdKind with | MethodKind.Virtual _  -> true | _ -> false

    member x.IsFinal                = match x.mdKind with | MethodKind.Virtual v -> v.IsFinal    | _ -> invalidOp "not virtual"
    member x.IsNewSlot              = match x.mdKind with | MethodKind.Virtual v -> v.IsNewSlot  | _ -> invalidOp "not virtual"
    member x.IsCheckAccessOnOverride= match x.mdKind with | MethodKind.Virtual v -> v.IsCheckAccessOnOverride   | _ -> invalidOp "not virtual"
    member x.IsAbstract             = match x.mdKind with | MethodKind.Virtual v -> v.IsAbstract | _ -> invalidOp "not virtual"

    member md.CallingSignature =  mkILCallSig (md.CallingConv,md.ParameterTypes,md.Return.Type)


/// Index table by name and arity. 
type MethodDefMap = Map<string, ILMethodDef list>

[<NoEquality; NoComparison>]
type ILMethodDefs = 
    | Methods of Lazy<ILMethodDef list * MethodDefMap>
    interface IEnumerable with 
        member x.GetEnumerator() = ((x :> IEnumerable<ILMethodDef>).GetEnumerator() :> IEnumerator)
    interface IEnumerable<ILMethodDef> with 
        member x.GetEnumerator() = 
            let (Methods(lms)) = x
            let ms,_ = lms.Force()
            (ms :> IEnumerable<ILMethodDef>).GetEnumerator()
    member x.AsList = Seq.toList x

    member x.FindByName nm  = 
        let (Methods lpmap) = x 
        let t = snd (Lazy.force lpmap)
        Map.tryFindMulti nm t 

    member x.FindByNameAndArity (nm,arity) = 
        x.FindByName nm |> List.filter (fun x -> x.Parameters.Length = arity) 


type ILEventDef =
    { Type: ILType option; 
      Name: string;
      IsRTSpecialName: bool;
      IsSpecialName: bool;
      AddMethod: ILMethodRef; 
      RemoveMethod: ILMethodRef;
      FireMethod: ILMethodRef option;
      OtherMethods: ILMethodRef list;
      CustomAttrs: ILAttributes; }

(* Index table by name. *)
[<NoEquality; NoComparison>]
type ILEventDefs = 
    | Events of LazyOrderedMultiMap<string, ILEventDef>
    member x.AsList = let (Events t) = x in t.Entries()
    member x.LookupByName s = let (Events t) = x in t.[s]

type ILPropertyDef = 
    { Name: string;
      IsRTSpecialName: bool;
      IsSpecialName: bool;
      SetMethod: ILMethodRef option;
      GetMethod: ILMethodRef option;
      CallingConv: ILThisConvention;
      Type: ILType;
      Init: ILFieldInit option;
      Args: ILType list;
      CustomAttrs: ILAttributes; }
    
// Index table by name.
[<NoEquality; NoComparison>]
type ILPropertyDefs = 
    | Properties of LazyOrderedMultiMap<string, ILPropertyDef>
    member x.AsList = let (Properties t) = x in t.Entries()
    member x.LookupByName s = let (Properties t) = x in t.[s]

type ILFieldDef = 
    { Name: string;
      Type: ILType;
      IsStatic: bool;
      Access: ILMemberAccess;
      Data:  byte[] option;
      LiteralValue:  ILFieldInit option;
      Offset:  int32 option; 
      IsSpecialName: bool;
      Marshal: ILNativeType option; 
      NotSerialized: bool;
      IsLiteral: bool ;
      IsInitOnly: bool;
      CustomAttrs: ILAttributes; }


// Index table by name.  Keep a canonical list to make sure field order is not disturbed for binary manipulation.
type ILFieldDefs = 
    | Fields of LazyOrderedMultiMap<string, ILFieldDef>
    member x.AsList = let (Fields t) = x in t.Entries()
    member x.LookupByName s = let (Fields t) = x in t.[s]

type ILMethodImplDef =
    { Overrides: ILOverridesSpec;
      OverrideBy: ILMethodSpec }

// Index table by name and arity. 
type ILMethodImplDefs = 
    | MethodImpls of Lazy<MethodImplsMap>
    member x.AsList = let (MethodImpls ltab) = x in Map.foldBack (fun _x y r -> y@r) (ltab.Force()) []

and MethodImplsMap = Map<string * int, ILMethodImplDef list>

[<RequireQualifiedAccess>]
type ILTypeDefLayout =
    | Auto
    | Sequential of ILTypeDefLayoutInfo
    | Explicit of ILTypeDefLayoutInfo 

and ILTypeDefLayoutInfo =
    { Size: int32 option;
      Pack: uint16 option } 

[<RequireQualifiedAccess>]
type ILTypeInit =
    | BeforeField
    | OnAny

[<RequireQualifiedAccess>]
type ILDefaultPInvokeEncoding =
    | Ansi
    | Auto
    | Unicode

type ILTypeDefAccess =
    | Public 
    | Private
    | Nested of ILMemberAccess 

[<RequireQualifiedAccess>]
type ILTypeDefKind =
    | Class
    | ValueType
    | Interface
    | Enum 
    | Delegate
    | Other of IlxExtensionTypeKind

and IlxExtensionTypeKind = Ext_type_def_kind of obj

type internal_type_def_kind_extension = 
    { internalTypeDefKindExtIs: IlxExtensionTypeKind -> bool; }


type ILTypeDef =  
    { tdKind: ILTypeDefKind;
      Name: string;  
      GenericParams: ILGenericParameterDefs;   (* class is generic *)
      Access: ILTypeDefAccess;  
      IsAbstract: bool;
      IsSealed: bool; 
      IsSerializable: bool; 
      IsComInterop: bool; (* Class or interface generated for COM interop *) 
      Layout: ILTypeDefLayout;
      IsSpecialName: bool;
      Encoding: ILDefaultPInvokeEncoding;
      NestedTypes: ILTypeDefs;
      Implements: ILType list;  
      Extends: ILType option; 
      Methods: ILMethodDefs;
      SecurityDecls: ILPermissions;
      HasSecurity: bool;
      Fields: ILFieldDefs;
      MethodImpls: ILMethodImplDefs;
      InitSemantics: ILTypeInit;
      Events: ILEventDefs;
      Properties: ILPropertyDefs;
      CustomAttrs: ILAttributes; }
    member x.IsClass =     (match x.tdKind with ILTypeDefKind.Class -> true | _ -> false)
    member x.IsInterface = (match x.tdKind with ILTypeDefKind.Interface -> true | _ -> false)
    member x.IsEnum =      (match x.tdKind with ILTypeDefKind.Enum -> true | _ -> false)
    member x.IsDelegate =  (match x.tdKind with ILTypeDefKind.Delegate -> true | _ -> false)

    member tdef.IsStructOrEnum = 
        match tdef.tdKind with
        | ILTypeDefKind.ValueType | ILTypeDefKind.Enum -> true
        | _ -> false


and ILTypeDefs = 
    | TypeDefTable of Lazy<(string list * string * ILAttributes * Lazy<ILTypeDef>) array> * Lazy<ILTypeDefsMap>
    interface IEnumerable with 
        member x.GetEnumerator() = ((x :> IEnumerable<ILTypeDef>).GetEnumerator() :> IEnumerator)
    interface IEnumerable<ILTypeDef> with 
        member x.GetEnumerator() = 
            let (TypeDefTable (larr,_tab)) = x
            let tds = seq { for (_,_,_,td) in larr.Force() -> td.Force() }
            tds.GetEnumerator()
    member x.AsList = Seq.toList x
    
    member x.AsListOfLazyTypeDefs = let (TypeDefTable (larr,_tab)) = x in larr.Force() |> Array.toList

    member x.FindByName nm  = 
        let (TypeDefTable (_,m)) = x 
        let ns,n = splitTypeName nm
        m.Force().[ns].[n].Force()

        
/// keyed first on namespace then on type name.  The namespace is often a unique key for a given type map.
and ILTypeDefsMap = 
     Map<string list,Dictionary<string,Lazy<ILTypeDef>>>

type ILNestedExportedType =
    { Name: string;
      Access: ILMemberAccess;
      Nested: ILNestedExportedTypes;
      CustomAttrs: ILAttributes } 

and ILNestedExportedTypes = 
    | ILNestedExportedTypes of Lazy<Map<string,ILNestedExportedType>>
    member x.AsList = let (ILNestedExportedTypes ltab) = x in Map.foldBack (fun _x y r -> y::r) (ltab.Force()) []

and ILExportedTypeOrForwarder =
    { ScopeRef: ILScopeRef;
      Name: string;
      IsForwarder: bool;
      Access: ILTypeDefAccess;
      Nested: ILNestedExportedTypes;
      CustomAttrs: ILAttributes } 

and ILExportedTypesAndForwarders = 
    | ILExportedTypesAndForwarders of Lazy<Map<string,ILExportedTypeOrForwarder>>
    member x.AsList = let (ILExportedTypesAndForwarders ltab) = x in Map.foldBack (fun _x y r -> y::r) (ltab.Force()) []

[<RequireQualifiedAccess>]
type ILResourceAccess = 
    | Public 
    | Private 

[<RequireQualifiedAccess>]
type ILResourceLocation =
    | Local of (unit -> byte[])
    | File of ILModuleRef * int32
    | Assembly of ILAssemblyRef

type ILResource =
    { Name: string;
      Location: ILResourceLocation;
      Access: ILResourceAccess;
      CustomAttrs: ILAttributes }

type ILResources = 
    | ILResources of Lazy<ILResource list>
    member x.AsList = let (ILResources ltab) = x in (ltab.Force())

// -------------------------------------------------------------------- 
// One module in the "current" assembly
// -------------------------------------------------------------------- 

[<RequireQualifiedAccess>]
type ILAssemblyLongevity =
    | Unspecified
    | Library
    | PlatformAppDomain
    | PlatformProcess
    | PlatformSystem


type ILAssemblyManifest = 
    { Name: string;
      AuxModuleHashAlgorithm: int32;
      SecurityDecls: ILPermissions;
      PublicKey: byte[] option;
      Version: ILVersionInfo option;
      Locale: Locale option;
      CustomAttrs: ILAttributes;

      AssemblyLongevity: ILAssemblyLongevity; 
      DisableJitOptimizations: bool;
      JitTracking: bool;
      Retargetable: bool;

      /// Records the types impemented by other modules. 
      ExportedTypes: ILExportedTypesAndForwarders;
      /// Records whether the entrypoint resides in another module. 
      EntrypointElsewhere: ILModuleRef option; 
               
    } 

type ILModuleDef = 
    { Manifest: ILAssemblyManifest option;
      CustomAttrs: ILAttributes;
      Name: string;
      TypeDefs: ILTypeDefs;
      (* Random bits of relatively uninteresting data *)
      SubSystemFlags: int32;
      IsDLL: bool;
      IsILOnly: bool;
      Platform: ILPlatform option; 
      StackReserveSize: int32 option;
      Is32Bit: bool;
      Is64Bit: bool;
      VirtualAlignment: int32;
      PhysicalAlignment: int32;
      ImageBase: int32;
      MetadataVersion: string;
      Resources: ILResources;
      NativeResources: list<Lazy<byte[]>>; (* e.g. win32 resources *)
    }
    member x.ManifestOfAssembly = 
        match x.Manifest with 
        | Some m -> m
        | None -> failwith "no manifest.  It is possible you are using an auxiliary module of an assembly in a context where the main module of an assembly is expected.  Typically the main module of an assembly must be specified first within a list of the modules in an assembly."

    member m.HasManifest = 
        match m.Manifest with None -> false | _ -> true


// -------------------------------------------------------------------- 
// Add fields and types to tables, with decent error messages
// when clashes occur...
// -------------------------------------------------------------------- 



let mkILEmptyGenericParams = ([]: ILGenericParameterDefs)
let emptyILGenericArgs = ([]: ILGenericArgs)


type ILType with
    member x.TypeSpec =
      match x with 
      | ILType.Boxed tr | ILType.Value tr -> tr
      | _ -> invalidOp "not a nominal type"
    member x.Boxity =
      match x with 
      | ILType.Boxed _ -> AsObject
      | ILType.Value _ -> AsValue
      | _ -> invalidOp "not a nominal type"
    member x.TypeRef = 
      match x with 
      | ILType.Boxed tspec | ILType.Value tspec -> tspec.TypeRef
      | _ -> invalidOp "not a nominal type"
    member x.IsNominal = 
      match x with 
      | ILType.Boxed _ | ILType.Value _ -> true
      | _ -> false
    member x.GenericArgs =
      match x with 
      | ILType.Boxed tspec | ILType.Value tspec -> tspec.GenericArgs
      | _ -> emptyILGenericArgs
    member x.IsTyvar =
      match x with 
      | ILType.TypeVar _ -> true | _ -> false



// --------------------------------------------------------------------
// Make ILTypeRefs etc.
// -------------------------------------------------------------------- 

let mkILNestedTyRef (scope,l,nm) =  ILTypeRef.Create(scope,l,nm)
let mkILTyRef (scope,nm) =  mkILNestedTyRef (scope,[],nm)
let mkILTySpec (tref,inst) =  ILTypeSpec.Create(tref, inst)
let mkILNonGenericTySpec tref =  mkILTySpec (tref,[])

let mkILTyRefInTyRef (tref:ILTypeRef,nm) = 
    mkILNestedTyRef (tref.Scope,tref.Enclosing@[tref.Name],nm)

let mkILTy boxed tspec = 
  match boxed with AsObject -> ILType.Boxed tspec | _ -> ILType.Value tspec

let mkILNamedTy vc tref tinst = mkILTy vc (ILTypeSpec.Create(tref, tinst))

let mkILValueTy tref tinst = mkILNamedTy AsValue tref tinst
let mkILBoxedTy tref tinst = mkILNamedTy AsObject tref tinst

let mkILNonGenericValueTy tref = mkILNamedTy AsValue tref []
let mkILNonGenericBoxedTy tref = mkILNamedTy AsObject tref []


type ILTypeDefKindExtension<'T> = 
    | TypeDefKindExtension

let type_def_kind_extensions = ref []

let RegisterTypeDefKindExtension (TypeDefKindExtension : ILTypeDefKindExtension<'T>) = 
    if nonNil !type_def_kind_extensions then failwith "define_type_extension: only one extension currently allowed";
    let mk (x:'T) = Ext_type_def_kind (box x)
    let test (Ext_type_def_kind _x) = true
    let dest (Ext_type_def_kind x) = (unbox x: 'T)
    type_def_kind_extensions := 
       { internalTypeDefKindExtIs=test;}
         :: !type_def_kind_extensions;
    mk,test,dest

// -------------------------------------------------------------------- 
// Making assembly, module and file references
// -------------------------------------------------------------------- 

let mkSimpleAssRef n = 
  ILAssemblyRef.Create(n, None, None, false, None, None)

let mkSimpleModRef n = 
    ILModuleRef.Create(n, true, None)

let module_name_of_scoref = function 
    | ILScopeRef.Module(mref) -> mref.Name
    | _ -> failwith "module_name_of_scoref"

// --------------------------------------------------------------------
// The toplevel class of a module is called "<Module>"
//
// The following comments come from the ECMA Spec (Parition II, Section 9.8)
//
// "For an ordinary type, if the metadata merges two definitions 
// of the same type, it simply discards one definition on the 
// assumption they are equivalent and that any anomaly will be 
// discovered when the type is used.  For the special class that 
// holds global members, however, members are unioned across all 
// modules at merge time. If the same name appears to be defined 
// for cross-module use in multiple modules then there is an 
// error.  In detail:
//  - If no member of the same kind (field or method), name, and 
//    signature exists, then add this member to the output class.
//  - If there are duplicates and no more than one has an 
//    accessibility other than compilercontrolled, then add them 
//    all in the output class.
//  - If there are duplicates and two or more have an accessibility 
//    other than compilercontrolled an error has occurred."
// -------------------------------------------------------------------- 

let typeNameForGlobalFunctions = "<Module>"

let tref_for_toplevel scoref = ILTypeRef.Create(scoref,[],typeNameForGlobalFunctions)

let tspec_for_toplevel scoref = mkILNonGenericTySpec (tref_for_toplevel scoref)

let mkILTypeForGlobalFunctions scorefs = ILType.Boxed (tspec_for_toplevel scorefs)

let isTypeNameForGlobalFunctions d = (d = typeNameForGlobalFunctions)

let mkILMethRef (tref,callconv,nm,gparams,args,rty) =
  { mrefParent=tref; 
    mrefCallconv=callconv;
    mrefGenericArity=gparams;
    mrefName=nm;
    mrefArgs=args;
    mrefReturn=rty}

let mkILMethSpecForMethRefInTy (mref,typ,minst) = 
  { mspecMethodRef=mref;
    mspecEnclosingType=typ;
    mspecMethodInst=minst }

let mkILMethSpec (mref, vc, tinst, minst) =mkILMethSpecForMethRefInTy (mref,mkILNamedTy vc mref.EnclosingTypeRef tinst,minst)

let mk_mspec_in_tref (tref,vc,cc,nm,args,rty,tinst,minst) =
  mkILMethSpec (mkILMethRef ( tref,cc,nm,List.length minst,args,rty),vc,tinst,minst)

let mkILMethSpecInTy (typ:ILType, cc, nm, args, rty, minst) =
  mkILMethSpecForMethRefInTy (mkILMethRef (typ.TypeRef,cc,nm,List.length minst,args,rty),typ,minst)

let mkILNonGenericMethSpecInTy (typ,cc,nm,args,rty) = 
  mkILMethSpecInTy (typ,cc,nm,args,rty,emptyILGenericArgs)

let mkILInstanceMethSpecInTy (typ:ILType,nm,args,rty,minst) =
  mkILMethSpecInTy (typ, ILCallingConv.Instance, nm, args, rty, minst)

let mkILNonGenericInstanceMethSpecInTy (typ:ILType,nm,args,rty) =
  mkILInstanceMethSpecInTy (typ,nm,args,rty,[])

let mkILStaticMethSpecInTy (typ,nm,args,rty,minst) =
  mkILMethSpecInTy (typ,ILCallingConv.Static,nm,args,rty,minst)

let mkILNonGenericStaticMethSpecInTy (typ,nm,args,rty) =
  mkILStaticMethSpecInTy (typ,nm,args,rty,emptyILGenericArgs)

let mkILCtorMethSpec (tref,args,cinst) = 
  mk_mspec_in_tref(tref,AsObject,ILCallingConv.Instance,".ctor",args,ILType.Void,cinst, emptyILGenericArgs)

let mkILCtorMethSpecForTy (ty,args) = 
  mkILMethSpecInTy(ty,ILCallingConv.Instance,".ctor",args,ILType.Void, emptyILGenericArgs)

let mkILNonGenericCtorMethSpec (tref,args) = 
  mkILCtorMethSpec (tref,args,emptyILGenericArgs)

// --------------------------------------------------------------------
// Make references to fields
// -------------------------------------------------------------------- 

let mkILFieldRef(tref,nm,ty) = { EnclosingTypeRef=tref; Name=nm; Type=ty}

let mkILFieldSpec (tref,ty) = { FieldRef= tref; EnclosingType=ty }

let mkILFieldSpecInTy (typ:ILType,nm,fty) = 
  mkILFieldSpec (mkILFieldRef (typ.TypeRef,nm,fty), typ)
    
let add_custom_attr_to_tab ca tab =  ca::tab
let mkILCustomAttrs l = CustomAttrs (Lazy.CreateFromValue(List.foldBack add_custom_attr_to_tab l []))
let mkILComputedCustomAttrs l = CustomAttrs (Lazy.Create l)
let emptyILCustomAttrs = mkILCustomAttrs []

let andTailness x y = 
  match x with Tailcall when y -> Tailcall | _ -> Normalcall

// -------------------------------------------------------------------- 
// ILAttributes on code blocks (esp. debug info)
// -------------------------------------------------------------------- 

let formatCodeLabel (x:int) = "L"+string x

module CodeLabels = 
    let insert (e:ILCodeLabel) l = Zset.add e l
    let remove e l = Zset.remove e l
    let fold f s acc = Zset.fold f s acc
    let add s x = Zset.add s x
    let addList s xs = Zset.addList s xs
    let diff l1 l2 = Zset.diff l1 l2
    let union l1 l2 = Zset.union l1 l2
    let inter (l1:Zset<ILCodeLabel>) l2 = Zset.inter l1 l2
    let subset (l1:Zset<ILCodeLabel>) l2 = Zset.subset l1 l2
    let empty = Zset.empty int_order
    let is_non_empty s = not (Zset.isEmpty s)
    let ofList l = Zset.addList l empty
    let toList l = Zset.elements l

// -------------------------------------------------------------------- 
// Basic operations on code.
// -------------------------------------------------------------------- 

let destinationsOfInstr i = 
  match i with 
  | I_leave l | I_br l -> [l]
  | I_brcmp (_,l1,l2) -> [l1; l2]
  | I_switch (ls,l) -> CodeLabels.toList (CodeLabels.ofList (l::ls))
  | I_endfinally | I_endfilter | I_ret | I_throw | I_rethrow 
  | I_call (Tailcall,_,_)| I_callvirt (Tailcall,_,_)| I_callconstraint (Tailcall,_,_,_)
  | I_calli (Tailcall,_,_) -> []
  | I_other e -> find_extension "instr" (fun ext -> if ext.internalInstrExtIs e then Some (ext.internalInstrExtDests e) else None) !instrExtensions
  | _ -> []

let destinations_of_bblock (bblock:ILBasicBlock) = destinationsOfInstr bblock.LastInstruction

let instrIsTailcall i = 
  match i with 
  | I_call (Tailcall,_,_)| I_callvirt (Tailcall,_,_) | I_callconstraint (Tailcall,_,_,_) | I_calli (Tailcall,_,_) -> true
  | I_other e -> find_extension "instr" (fun ext -> if ext.internalInstrExtIs e then Some (ext.internalInstrExtIsTailcall e) else None) !instrExtensions
  | _ -> false

let instrIsBascBlockEnd i = 
  instrIsTailcall i ||
  match i with 
  | I_leave _ | I_br _ | I_brcmp _ | I_switch _ | I_endfinally
  | I_endfilter | I_ret | I_throw | I_rethrow  ->  true
  | I_other e -> find_extension "instr" (fun ext -> if ext.internalInstrExtIs e then Some (nonNil (ext.internalInstrExtDests e)) else None) !instrExtensions
  | _ -> false

let checks = false 
let _ = if checks then dprintn "Warning - Il.checks is on"

let rec accEntriesOfCode c acc =
  match c with
  | ILBasicBlock bb -> CodeLabels.add bb.Label acc
  | GroupBlock (_,l) -> List.foldBack accEntriesOfCode l acc
  | RestrictBlock (ls,c) -> CodeLabels.union acc (CodeLabels.diff (entriesOfCodeAsSet c) (CodeLabels.ofList ls))
  | TryBlock (l,_r) -> accEntriesOfCode l acc
and entriesOfCodeAsSet c = accEntriesOfCode c CodeLabels.empty 

let rec accExitsOfCode c acc =
  let basicOutsideLabels = 
    match c with
    | ILBasicBlock bblock -> CodeLabels.addList (destinations_of_bblock bblock) acc
    | GroupBlock (_,l) -> List.foldBack accExitsOfCode l acc
    | RestrictBlock (ls,c) ->  CodeLabels.union acc (CodeLabels.diff (exitsOfCodeAsSet c) (CodeLabels.ofList ls))
    | TryBlock (l,_r) -> accExitsOfCode l acc
  CodeLabels.diff basicOutsideLabels (entriesOfCodeAsSet c)
and exitsOfCodeAsSet c = accExitsOfCode c CodeLabels.empty

let entries_of_code c = CodeLabels.toList (entriesOfCodeAsSet c)
let exits_of_code c = CodeLabels.toList (exitsOfCodeAsSet c)

/// Finds all labels defined within this code block, seeing through restrictions.
/// This assumes that labels are unique within the code blocks, even if hidden behind restrictions.
///
// Note: Repeats in the list indicate this invariant is broken.
let rec accLabelsOfCode acc c =
  match c with
  | ILBasicBlock bb        -> bb.Label::acc
  | GroupBlock (_,l)     -> List.fold accLabelsOfCode acc l 
  | RestrictBlock (_ls,c) -> accLabelsOfCode acc c
  | TryBlock (l,r)       -> let acc = accLabelsOfCode acc l
                            let acc = accLabelsOfSEH  acc r
                            acc
and accLabelsOfSEH acc = function
  | FaultBlock       code   -> accLabelsOfCode acc code
  | FinallyBlock     code   -> accLabelsOfCode acc code
  | FilterCatchBlock fcodes -> List.fold accLabelsOfFilterCode acc fcodes
      
and accLabelsOfFilterCode acc = function
  | TypeFilter _,code    -> accLabelsOfCode acc code
  | CodeFilter test,code -> let accA = accLabelsOfCode acc code
                            let accB = accLabelsOfCode accA test
                            accB

let labelsOfCode code = accLabelsOfCode [] code

(*

From the ECMA spec:

There are only two ways to enter a try block from outside its lexical body:
 - Branching to or falling into the try block�s first instruction. The branch may be made using a 37
conditional branch, an unconditional branch, or a leave instruction. 38
 - Using a leave instruction from that try�s catch block. In this case, correct CIL code may 39
branch to any instruction within the try block, not just its first instruction, so long as that 40
branch target is not protected by yet another try, nested withing the first 
*)


let checkILCode code = 
    if checks then 
        match code with
        | RestrictBlock (ls,c') -> 
            (*
              if not (CodeLabels.subset ls (entries_of_code c')) then begin
                dprintn ("* warning: Restricting labels that are not declared in block, e.g. "+ (List.head (CodeLabels.diff ls (entries_of_code c'))));
                dprintn ("* warning: Labels in block are: "+ (String.concat "," (entries_of_code c')));
                dprintn ("* warning: Labels being restricted are: "+ (String.concat "," ls));
              end;
            *)
            let cls = (CodeLabels.inter (CodeLabels.ofList ls) (exitsOfCodeAsSet c'))
            if (CodeLabels.is_non_empty cls) then 
              dprintn ("* warning: restricting unsatisfied exits from a block, e.g. "+ formatCodeLabel (List.head (CodeLabels.toList cls)));
        | TryBlock (_l,r) -> 
            begin match r with 
            | FaultBlock b | FinallyBlock b -> 
                if (CodeLabels.is_non_empty (CodeLabels.inter (exitsOfCodeAsSet b) (entriesOfCodeAsSet b))) then 
                  dprintn "* warning: exits from fault or finally blocks must leave the block";
                let n = List.length (entries_of_code b)
                if not (n = 1) then dprintn "* warning: zero or more than one entry to a fault or finally block";
            | FilterCatchBlock r -> 
                List.iter 
                  (fun (flt,z) -> 
                    let m = List.length (entries_of_code z)
                    if not (m = 1) then dprintn "* warning: zero or more than one entry to a catch block";
                    match flt with 
                    | CodeFilter y -> 
                        if (CodeLabels.is_non_empty (exitsOfCodeAsSet y)) then dprintn "* warning: exits exist from filter block - you must always exit using endfinally";
                        let n = List.length (entries_of_code y)
                        if not (n = 1) then dprintn "* warning: zero or more than one entry to a filter block";
                    | TypeFilter _ty -> ())
                  r;
            end;
        | ILBasicBlock bb ->
            if (Array.length bb.Instructions) = 0 then dprintn ("* warning: basic block " + formatCodeLabel bb.Label + " is empty")
            elif not (instrIsBascBlockEnd (bb.Instructions.[Array.length bb.Instructions - 1])) then failwith "* warning: bblock does not end in an appropriate instruction";
            
        | _ -> ()
    match code with 
    | RestrictBlock (labs,c) when (isNil labs) -> c 
    | GroupBlock ([],[c]) -> c 
    | _ -> code


let mkBasicBlock bb = ILBasicBlock bb
let mk_scope_block (a,b) = GroupBlock (a,[checkILCode b])
let mk_group_block_from_code (internals,codes) = RestrictBlock (internals,checkILCode (GroupBlock ([],codes)))
let mkGroupBlock (internals,blocks) = mk_group_block_from_code (internals,List.map checkILCode blocks)

let mk_restrict_block lab c = RestrictBlock (CodeLabels.toList (CodeLabels.remove lab (entriesOfCodeAsSet c)),c)
let mk_try_finally_block (tryblock, enter_finally_lab, finallyblock) = 
  TryBlock(checkILCode tryblock, FinallyBlock (checkILCode (mk_restrict_block enter_finally_lab (checkILCode finallyblock))))

let mk_try_fault_block (tryblock, enter_fault_lab, faultblock) = 
  TryBlock(checkILCode tryblock, FaultBlock (checkILCode (mk_restrict_block enter_fault_lab (checkILCode faultblock))))

let mk_try_multi_filter_catch_block (tryblock, clauses) = 
    TryBlock
      (checkILCode tryblock, 
       FilterCatchBlock 
         (clauses |> List.map (fun (flt, (enter_catch_lab, catchblock)) -> 
                let fltcode = 
                  match flt with 
                  | Choice1Of2 (enter_filter_lab, filterblock) ->
                      CodeFilter (checkILCode (mk_restrict_block enter_filter_lab (checkILCode filterblock)))
                  | Choice2Of2 ty -> 
                      TypeFilter ty
                fltcode,
                checkILCode (mk_restrict_block enter_catch_lab (checkILCode catchblock)))))


let new_generator () = 
    let i = ref 0
    fun _n -> 
      incr i; !i

//  ++GLOBAL MUTABLE STATE
let code_label_generator = (new_generator () : unit -> ILCodeLabel) 
let generateCodeLabel x  = code_label_generator x

let uniqueEntryOfCode c = 
    match entries_of_code c with 
    | [] -> failwith ("uniqueEntryOfCode: no entries to code")
    | [inlab] -> inlab
    | labs -> failwith ("uniqueEntryOfCode: need one entry to code, found: "+String.concat "," (List.map formatCodeLabel labs))

let unique_exit_of_code c = 
    match exits_of_code c with 
    | [] -> failwith ("unique_exit_of_code: no exits from code")
    | [outlab] -> outlab
    | labs -> failwith ("unique_exit_of_code: need one exit from code, found: "+String.concat "," (List.map formatCodeLabel labs))

let mkNonBranchingInstrs inplab instrs = 
    checkILCode (mkBasicBlock {Label=inplab; Instructions= Array.ofList instrs})

let mkNonBranchingInstrsThen inplab instrs instr = 
    if nonNil instrs && instrIsBascBlockEnd (List.last instrs) then failwith "mkNonBranchingInstrsThen: bblock already terminates with a control flow instruction";
    mkNonBranchingInstrs inplab (instrs @ [ instr ]) 

let mk_nonbranching_instrs_then_ret inplab instrs = 
    mkNonBranchingInstrsThen inplab instrs I_ret

let mkNonBranchingInstrsThenBr inplab instrs lab = 
    mkNonBranchingInstrsThen inplab instrs (I_br lab)

let nonBranchingInstrsToCode instrs = 
    let inplab = generateCodeLabel ()
    if nonNil instrs && instrIsBascBlockEnd (List.last instrs) then 
      mkNonBranchingInstrs inplab instrs
    else
      mk_nonbranching_instrs_then_ret inplab  instrs

let join_code code1 code2 = 
    if not (unique_exit_of_code code1 = uniqueEntryOfCode code2)  then 
      dprintn "* warning: join_code: exit of code1 is not entry of code 2";
    checkILCode 
      (RestrictBlock ([unique_exit_of_code code1], 
                      (checkILCode (mkGroupBlock ([],[ code1; code2 ])))))

// -------------------------------------------------------------------- 
// Security declarations (2)
// -------------------------------------------------------------------- 

let add_security_decl_to_tab sd tab =  sd::tab
let mkILSecurityDecls l = SecurityDecls (notlazy (List.foldBack add_security_decl_to_tab l []))
let mkILLazySecurityDecls l = SecurityDecls (lazy (List.foldBack add_security_decl_to_tab (Lazy.force l) []))

let emptyILSecurityDecls = mkILSecurityDecls []

// --------------------------------------------------------------------
// ILX stuff
// -------------------------------------------------------------------- 

let mkILTyvarTy tv = ILType.TypeVar tv

let listRead l n = try List.nth l n with _ -> failwith "uninterp: read"
  
let instRead (inst:ILGenericArgs) v =
  try listRead inst  (int v)
  with _ -> failwithf "type variable no. %d needs a value" v

let mkILSimpleTypar nm =
   { Name=nm;
     Constraints=[];
     Variance=NonVariant;
     HasReferenceTypeConstraint=false;
     HasNotNullableValueTypeConstraint=false;
     HasDefaultConstructorConstraint=false; 
     CustomAttrs = emptyILCustomAttrs }

let gparam_of_gactual (_ga:ILType) = mkILSimpleTypar "T"

let mkILFormalTypars (x: ILGenericArgs) = List.map gparam_of_gactual x

let mkILFormalGenericArgs (gparams:ILGenericParameterDefs)  =
    List.mapi (fun n _gf -> mkILTyvarTy (uint16 n)) gparams
 
let mkILFormalBoxedTy tref gparams = mkILBoxedTy tref (mkILFormalGenericArgs gparams)

// -------------------------------------------------------------------- 
// Operations on class etc. defs.
// -------------------------------------------------------------------- 

let mkRefForNestedILTypeDef scope (enc:ILTypeDef list,td:ILTypeDef)  = 
    mkILNestedTyRef(scope, (enc |> List.map (fun etd -> etd.Name)), td.Name)

// -------------------------------------------------------------------- 
// Operations on type tables.
// -------------------------------------------------------------------- 

let getName (ltd: Lazy<ILTypeDef>) = 
    let td = Lazy.force ltd
    let ns,n = splitTypeName td.Name
    (ns,n,td.CustomAttrs,ltd)

let addILTypeDefToTable (ns,n,_cas,ltd) tab = 
    let prev = 
       (match Map.tryFind ns tab with 
        | None -> Dictionary<_,_>(1, HashIdentity.Structural) 
        | Some prev -> prev)
    if prev.ContainsKey n then  
        let msg = sprintf "not unique type %s" (unsplitTypeName (ns,n));
        System.Diagnostics.Debug.Assert(false,msg)
        failwith msg
    prev.[n] <- ltd;
    Map.add ns prev tab

let addLazyTypeDefToTable ltd larr = lazyMap (fun arr -> Array.ofList (getName ltd :: Array.toList arr)) larr

let buildTable larr = lazyMap (fun arr -> Array.foldBack addILTypeDefToTable arr Map.empty) larr
let buildTypes larr = TypeDefTable (larr, buildTable larr)

(* this is not performance critical *)
let addILTypeDef td (TypeDefTable (larr,_ltab)) = buildTypes (addLazyTypeDefToTable (notlazy td) larr)       
let mkILTypeDefs l =  buildTypes (List.map (notlazy >> getName) l |> Array.ofList |> notlazy )
let mkILTypeDefsLazy llist = buildTypes (lazyMap Array.ofList llist)
let emptyILTypeDefs = mkILTypeDefs []

// -------------------------------------------------------------------- 
// Operations on method tables.
// -------------------------------------------------------------------- 

let addILMethodToTable (y: ILMethodDef) tab =
  let key = y.Name
  let prev = Map.tryFindMulti key tab
  Map.add key (y::prev) tab

let addILMethod_to_pmap y (mds,tab) = y::mds,addILMethodToTable y tab
let addILMethod y (Methods lpmap) = Methods (lazyMap (addILMethod_to_pmap y) lpmap)

let mkILMethods l =  Methods (notlazy (List.foldBack addILMethod_to_pmap l ([],Map.empty)))
let mkILMethodsLazy l =  Methods (lazy (List.foldBack addILMethod_to_pmap (Lazy.force l) ([],Map.empty)))
let emptyILMethods = mkILMethods []

let filterILMethodDefs f (Methods lpmap) = 
    Methods (lazyMap (fun (fs,_) -> 
        let l = List.filter f fs
        (l, List.foldBack addILMethodToTable l Map.empty)) lpmap)


// -------------------------------------------------------------------- 
// Operations and defaults for modules, assemblies etc.
// -------------------------------------------------------------------- 

let defaultSubSystem = 3 (* this is what comes out of ILDASM on 30/04/2001 *)
let defaultPhysAlignment = 512 (* this is what comes out of ILDASM on 30/04/2001 *)
let defaultVirtAlignment = 0x2000 (* this is what comes out of ILDASM on 30/04/2001 *)
let defaultImageBase = 0x034f0000 (* this is what comes out of ILDASM on 30/04/2001 *)

// -------------------------------------------------------------------- 
// Array types
// -------------------------------------------------------------------- 

let mkILArrTy (ty, shape) = ILType.Array(shape,ty)
let mkILArr1DTy ty = mkILArrTy (ty,ILArrayShape.SingleDimensional)
let isILArrTy ty = match ty with ILType.Array _ -> true| _ -> false
let destILArrTy ty = match ty with ILType.Array(shape,ty) -> (shape,ty) | _ -> failwith "destILArrTy"

// -------------------------------------------------------------------- 
// Sigs of special types built-in
// -------------------------------------------------------------------- 

let tname_Object = "System.Object"
let tname_String = "System.String"
let tname_StringBuilder = "System.Text.StringBuilder"
let tname_AsyncCallback = "System.AsyncCallback"
let tname_IAsyncResult = "System.IAsyncResult"
let tname_IComparable = "System.IComparable"
let tname_Exception = "System.Exception"
let tname_Type = "System.Type"
let tname_Missing = "System.Reflection.Missing"
let tname_Activator = "System.Activator"
let tname_SerializationInfo = "System.Runtime.Serialization.SerializationInfo"
let tname_StreamingContext = "System.Runtime.Serialization.StreamingContext"
let tname_SecurityPermissionAttribute = "System.Security.Permissions.SecurityPermissionAttribute"
let tname_Delegate = "System.Delegate"
let tname_ValueType = "System.ValueType"
let tname_TypedReference = "System.TypedReference"
let tname_Enum = "System.Enum"
let tname_MulticastDelegate = "System.MulticastDelegate"
let tname_Array = "System.Array"

let tname_Int64 = "System.Int64"
let tname_UInt64 = "System.UInt64"
let tname_Int32 = "System.Int32"
let tname_UInt32 = "System.UInt32"
let tname_Int16 = "System.Int16"
let tname_UInt16 = "System.UInt16"
let tname_SByte = "System.SByte"
let tname_Byte = "System.Byte"
let tname_Single = "System.Single"
let tname_Double = "System.Double"
let tname_Bool = "System.Boolean"
let tname_Char = "System.Char"
let tname_IntPtr = "System.IntPtr"
let tname_UIntPtr = "System.UIntPtr"
let tname_RuntimeArgumentHandle = "System.RuntimeArgumentHandle"
let tname_RuntimeTypeHandle = "System.RuntimeTypeHandle"
let tname_RuntimeMethodHandle = "System.RuntimeMethodHandle"
let tname_RuntimeFieldHandle = "System.RuntimeFieldHandle"

[<NoEquality; NoComparison>]
type ILGlobals = 
    { mscorlibScopeRef: ILScopeRef;
      mscorlibAssemblyName: string;
      noDebugData: bool;
      tref_Object: ILTypeRef 
      ; tspec_Object: ILTypeSpec
      ; typ_Object: ILType
      ; tref_String: ILTypeRef
      ; typ_String: ILType
      ; typ_StringBuilder: ILType
      ; typ_AsyncCallback: ILType
      ; typ_IAsyncResult: ILType
      ; typ_IComparable: ILType
      ; tref_Type: ILTypeRef
      ; typ_Type: ILType
      ; typ_Missing: ILType
      ; typ_Activator: ILType
      ; typ_Delegate: ILType
      ; typ_ValueType: ILType
      ; typ_Enum: ILType
      ; tspec_TypedReference: ILTypeSpec
      ; typ_TypedReference: ILType
      ; typ_MulticastDelegate: ILType
      ; typ_Array: ILType
      ; tspec_Int64: ILTypeSpec
      ; tspec_UInt64: ILTypeSpec
      ; tspec_Int32: ILTypeSpec
      ; tspec_UInt32: ILTypeSpec
      ; tspec_Int16: ILTypeSpec
      ; tspec_UInt16: ILTypeSpec
      ; tspec_SByte: ILTypeSpec
      ; tspec_Byte: ILTypeSpec
      ; tspec_Single: ILTypeSpec
      ; tspec_Double: ILTypeSpec
      ; tspec_IntPtr: ILTypeSpec
      ; tspec_UIntPtr: ILTypeSpec
      ; tspec_Char: ILTypeSpec
      ; tspec_Bool: ILTypeSpec
      ; typ_int8: ILType
      ; typ_int16: ILType
      ; typ_int32: ILType
      ; typ_int64: ILType
      ; typ_uint8: ILType
      ; typ_uint16: ILType
      ; typ_uint32: ILType
      ; typ_uint64: ILType
      ; typ_float32: ILType
      ; typ_float64: ILType
      ; typ_bool: ILType
      ; typ_char: ILType
      ; typ_IntPtr: ILType
      ; typ_UIntPtr: ILType
      ; typ_RuntimeArgumentHandle: ILType
      ; typ_RuntimeTypeHandle: ILType
      ; typ_RuntimeMethodHandle: ILType
      ; typ_RuntimeFieldHandle: ILType
      ; typ_Byte: ILType
      ; typ_Int16: ILType
      ; typ_Int32: ILType
      ; typ_Int64: ILType
      ; typ_SByte: ILType
      ; typ_UInt16: ILType
      ; typ_UInt32: ILType
      ; typ_UInt64: ILType
      ; typ_Single: ILType
      ; typ_Double: ILType
      ; typ_Bool: ILType
      ; typ_Char: ILType
      ; typ_SerializationInfo: ILType
      ; typ_StreamingContext: ILType
      ; tref_SecurityPermissionAttribute: ILTypeRef
      ; tspec_Exception: ILTypeSpec
      ; typ_Exception: ILType
      ; mutable generatedAttribsCache: ILAttribute list 
      ; mutable debuggerBrowsableNeverAttributeCache : ILAttribute option
      ; mutable debuggerTypeProxyAttributeCache : ILAttribute option }
    override x.ToString() = "<ILGlobals>"

let mkNormalCall mspec = I_call (Normalcall, mspec, None)
let mkNormalCallvirt mspec = I_callvirt (Normalcall, mspec, None)
let mkNormalCallconstraint (ty,mspec) = I_callconstraint (Normalcall, ty, mspec, None)
let mkNormalNewobj mspec =  I_newobj (mspec, None)
let mkLdarg0 = I_ldarg 0us
let ldarg_1 = I_ldarg 1us
let tname_CompilerGeneratedAttribute = "System.Runtime.CompilerServices.CompilerGeneratedAttribute"
let tname_DebuggableAttribute = "System.Diagnostics.DebuggableAttribute"

let mkILGlobals mscorlibScopeRef mscorlib_assembly_name_option noDebugData =
  let mscorlibAssemblyName =
    match mscorlib_assembly_name_option with
      | Some name -> name 
      | None      -> (match mscorlibScopeRef with
                        | ILScopeRef.Assembly assref -> assref.Name
                        | _ -> failwith "mkILGlobals: mscorlib ILScopeRef is not an assembly ref")
  let tref_Object = mkILTyRef (mscorlibScopeRef,tname_Object)
  let tspec_Object = mkILNonGenericTySpec tref_Object
  let typ_Object = ILType.Boxed tspec_Object

  let tref_String = mkILTyRef (mscorlibScopeRef,tname_String)
  let tspec_String = mkILTySpec(tref_String,emptyILGenericArgs)
  let typ_String = ILType.Boxed tspec_String

  let tref_StringBuilder = mkILTyRef (mscorlibScopeRef,tname_StringBuilder)
  let tspec_StringBuilder = mkILTySpec(tref_StringBuilder,emptyILGenericArgs)
  let typ_StringBuilder = ILType.Boxed tspec_StringBuilder

  let tref_AsyncCallback = mkILTyRef (mscorlibScopeRef,tname_AsyncCallback)
  let tspec_AsyncCallback = mkILTySpec(tref_AsyncCallback,emptyILGenericArgs)
  let typ_AsyncCallback = ILType.Boxed tspec_AsyncCallback

  let tref_IAsyncResult = mkILTyRef (mscorlibScopeRef,tname_IAsyncResult)
  let tspec_IAsyncResult = mkILTySpec(tref_IAsyncResult,emptyILGenericArgs)
  let typ_IAsyncResult = ILType.Boxed tspec_IAsyncResult

  let tref_IComparable = mkILTyRef (mscorlibScopeRef,tname_IComparable)
  let tspec_IComparable = mkILTySpec(tref_IComparable,emptyILGenericArgs)
  let typ_IComparable = ILType.Boxed tspec_IComparable

  let tref_Exception = mkILTyRef (mscorlibScopeRef,tname_Exception)
  let tspec_Exception = mkILTySpec(tref_Exception,emptyILGenericArgs)
  let typ_Exception = ILType.Boxed tspec_Exception

  let tref_Type = mkILTyRef(mscorlibScopeRef,tname_Type)
  let tspec_Type = mkILTySpec(tref_Type,emptyILGenericArgs)
  let typ_Type = ILType.Boxed tspec_Type

  let tref_Missing = mkILTyRef(mscorlibScopeRef,tname_Missing)
  let tspec_Missing = mkILTySpec(tref_Missing,emptyILGenericArgs)
  let typ_Missing = ILType.Boxed tspec_Missing


  let tref_Activator = mkILTyRef(mscorlibScopeRef,tname_Activator)
  let tspec_Activator = mkILTySpec(tref_Activator,emptyILGenericArgs)
  let typ_Activator = ILType.Boxed tspec_Activator

  let tref_SerializationInfo = mkILTyRef(mscorlibScopeRef,tname_SerializationInfo)
  let tspec_SerializationInfo = mkILTySpec(tref_SerializationInfo,emptyILGenericArgs)
  let typ_SerializationInfo = ILType.Boxed tspec_SerializationInfo

  let tref_StreamingContext = mkILTyRef(mscorlibScopeRef,tname_StreamingContext)
  let tspec_StreamingContext = mkILTySpec(tref_StreamingContext,emptyILGenericArgs)
  let typ_StreamingContext = ILType.Value tspec_StreamingContext

  let tref_SecurityPermissionAttribute = mkILTyRef(mscorlibScopeRef,tname_SecurityPermissionAttribute)

  let tref_Delegate = mkILTyRef(mscorlibScopeRef,tname_Delegate)
  let tspec_Delegate = mkILTySpec(tref_Delegate,emptyILGenericArgs)
  let typ_Delegate = ILType.Boxed tspec_Delegate

  let tref_ValueType = mkILTyRef (mscorlibScopeRef,tname_ValueType)
  let tspec_ValueType = mkILTySpec(tref_ValueType,emptyILGenericArgs)
  let typ_ValueType = ILType.Boxed tspec_ValueType

  let tref_TypedReference = mkILTyRef (mscorlibScopeRef,tname_TypedReference)
  let tspec_TypedReference = mkILTySpec(tref_TypedReference,emptyILGenericArgs)
  let typ_TypedReference = ILType.Value tspec_TypedReference

  let tref_Enum = mkILTyRef (mscorlibScopeRef,tname_Enum)
  let tspec_Enum = mkILTySpec(tref_Enum,emptyILGenericArgs)
  let typ_Enum = ILType.Boxed tspec_Enum

  let tref_MulticastDelegate = mkILTyRef (mscorlibScopeRef,tname_MulticastDelegate)
  let tspec_MulticastDelegate = mkILTySpec(tref_MulticastDelegate,emptyILGenericArgs)
  let typ_MulticastDelegate = ILType.Boxed tspec_MulticastDelegate

  let typ_Array = ILType.Boxed (mkILTySpec(mkILTyRef (mscorlibScopeRef,tname_Array),emptyILGenericArgs))

  let tref_Int64 = mkILTyRef (mscorlibScopeRef,tname_Int64)
  let tref_UInt64 = mkILTyRef (mscorlibScopeRef,tname_UInt64)
  let tref_Int32 = mkILTyRef (mscorlibScopeRef,tname_Int32)
  let tref_UInt32 = mkILTyRef (mscorlibScopeRef,tname_UInt32)
  let tref_Int16 = mkILTyRef (mscorlibScopeRef,tname_Int16)
  let tref_UInt16 = mkILTyRef (mscorlibScopeRef,tname_UInt16)
  let tref_SByte = mkILTyRef (mscorlibScopeRef,tname_SByte)
  let tref_Byte = mkILTyRef (mscorlibScopeRef,tname_Byte)
  let tref_Single = mkILTyRef (mscorlibScopeRef,tname_Single)
  let tref_Double = mkILTyRef (mscorlibScopeRef,tname_Double)
  let tref_Bool = mkILTyRef (mscorlibScopeRef,tname_Bool)
  let tref_Char = mkILTyRef (mscorlibScopeRef,tname_Char)
  let tref_IntPtr = mkILTyRef (mscorlibScopeRef,tname_IntPtr)
  let tref_UIntPtr = mkILTyRef (mscorlibScopeRef,tname_UIntPtr)

  let tspec_Int64 = mkILTySpec(tref_Int64,emptyILGenericArgs)
  let tspec_UInt64 = mkILTySpec(tref_UInt64,emptyILGenericArgs)
  let tspec_Int32 = mkILTySpec(tref_Int32,emptyILGenericArgs)
  let tspec_UInt32 = mkILTySpec(tref_UInt32,emptyILGenericArgs)
  let tspec_Int16 = mkILTySpec(tref_Int16,emptyILGenericArgs)
  let tspec_UInt16 = mkILTySpec(tref_UInt16,emptyILGenericArgs)
  let tspec_SByte = mkILTySpec(tref_SByte,emptyILGenericArgs)
  let tspec_Byte = mkILTySpec(tref_Byte,emptyILGenericArgs)
  let tspec_Single = mkILTySpec(tref_Single,emptyILGenericArgs)
  let tspec_Double = mkILTySpec(tref_Double,emptyILGenericArgs)
  let tspec_IntPtr = mkILTySpec(tref_IntPtr,emptyILGenericArgs)
  let tspec_UIntPtr = mkILTySpec(tref_UIntPtr,emptyILGenericArgs)
  let tspec_Char = mkILTySpec(tref_Char,emptyILGenericArgs)
  let tspec_Bool = mkILTySpec(tref_Bool,emptyILGenericArgs)

  let typ_int8 = ILType.Value tspec_SByte 
  let typ_int16 = ILType.Value tspec_Int16
  let typ_int32 = ILType.Value tspec_Int32
  let typ_int64 = ILType.Value tspec_Int64
  let typ_uint8 = ILType.Value tspec_Byte
  let typ_uint16 = ILType.Value tspec_UInt16
  let typ_uint32 = ILType.Value tspec_UInt32
  let typ_uint64 = ILType.Value tspec_UInt64
  let typ_float32 = ILType.Value tspec_Single
  let typ_float64 = ILType.Value tspec_Double
  let typ_bool = ILType.Value tspec_Bool
  let typ_char = ILType.Value tspec_Char
  let typ_IntPtr = ILType.Value tspec_IntPtr
  let typ_UIntPtr = ILType.Value tspec_UIntPtr

  let typ_SByte = ILType.Value tspec_SByte
  let typ_Int16 = ILType.Value tspec_Int16
  let typ_Int32 = ILType.Value tspec_Int32
  let typ_Int64 = ILType.Value tspec_Int64
  let typ_Byte = ILType.Value tspec_Byte
  let typ_UInt16 = ILType.Value tspec_UInt16
  let typ_UInt32 = ILType.Value tspec_UInt32
  let typ_UInt64 = ILType.Value tspec_UInt64
  let typ_Single = ILType.Value tspec_Single
  let typ_Double = ILType.Value tspec_Double
  let typ_Bool = ILType.Value tspec_Bool
  let typ_Char = ILType.Value tspec_Char

  let tref_RuntimeArgumentHandle = mkILTyRef (mscorlibScopeRef,tname_RuntimeArgumentHandle)
  let tspec_RuntimeArgumentHandle = mkILTySpec(tref_RuntimeArgumentHandle,emptyILGenericArgs)
  let typ_RuntimeArgumentHandle = ILType.Value tspec_RuntimeArgumentHandle
  let tref_RuntimeTypeHandle = mkILTyRef (mscorlibScopeRef,tname_RuntimeTypeHandle)
  let tspec_RuntimeTypeHandle = mkILTySpec(tref_RuntimeTypeHandle,emptyILGenericArgs)
  let typ_RuntimeTypeHandle = ILType.Value tspec_RuntimeTypeHandle
  let tref_RuntimeMethodHandle = mkILTyRef (mscorlibScopeRef,tname_RuntimeMethodHandle)
  let tspec_RuntimeMethodHandle = mkILTySpec(tref_RuntimeMethodHandle,emptyILGenericArgs)
  let typ_RuntimeMethodHandle = ILType.Value tspec_RuntimeMethodHandle
  let tref_RuntimeFieldHandle = mkILTyRef (mscorlibScopeRef,tname_RuntimeFieldHandle)
  let tspec_RuntimeFieldHandle = mkILTySpec(tref_RuntimeFieldHandle,emptyILGenericArgs)
  let typ_RuntimeFieldHandle = ILType.Value tspec_RuntimeFieldHandle
  {   mscorlibScopeRef           =mscorlibScopeRef
    ; mscorlibAssemblyName       =mscorlibAssemblyName
    ; noDebugData                =noDebugData 
    ; tref_Object                =tref_Object                  
    ; tspec_Object               =tspec_Object                 
    ; typ_Object                 =typ_Object                   
    ; tref_String                =tref_String                  
    ; typ_String                 =typ_String                   
    ; typ_StringBuilder          =typ_StringBuilder                   
    ; typ_AsyncCallback          =typ_AsyncCallback            
    ; typ_IAsyncResult           =typ_IAsyncResult             
    ; typ_IComparable            =typ_IComparable              
    ; typ_Activator              =typ_Activator                     
    ; tref_Type                  =tref_Type                    
    ; typ_Type                   =typ_Type                     
    ; typ_Missing                =typ_Missing                     
    ; typ_Delegate               =typ_Delegate                 
    ; typ_ValueType              =typ_ValueType                
    ; typ_Enum                   =typ_Enum                     
    ; tspec_TypedReference       =tspec_TypedReference         
    ; typ_TypedReference         =typ_TypedReference           
    ; typ_MulticastDelegate      =typ_MulticastDelegate        
    ; typ_Array                  =typ_Array                    
    ; tspec_Int64                =tspec_Int64                  
    ; tspec_UInt64               =tspec_UInt64                 
    ; tspec_Int32                =tspec_Int32                  
    ; tspec_UInt32               =tspec_UInt32                 
    ; tspec_Int16                =tspec_Int16                  
    ; tspec_UInt16               =tspec_UInt16                 
    ; tspec_SByte                =tspec_SByte                  
    ; tspec_Byte                 =tspec_Byte                   
    ; tspec_Single               =tspec_Single                 
    ; tspec_Double               =tspec_Double                 
    ; tspec_IntPtr               =tspec_IntPtr                 
    ; tspec_UIntPtr              =tspec_UIntPtr                
    ; tspec_Char                 =tspec_Char                   
    ; tspec_Bool                 =tspec_Bool                   
    ; typ_int8                   =typ_int8                     
    ; typ_int16                  =typ_int16                    
    ; typ_int32                  =typ_int32                    
    ; typ_int64                  =typ_int64                    
    ; typ_uint8                  =typ_uint8                    
    ; typ_uint16                 =typ_uint16                   
    ; typ_uint32                 =typ_uint32                   
    ; typ_uint64                 =typ_uint64                   
    ; typ_float32                =typ_float32                  
    ; typ_float64                =typ_float64                  
    ; typ_bool                   =typ_bool                     
    ; typ_char                   =typ_char                     
    ; typ_IntPtr                    =typ_IntPtr                      
    ; typ_UIntPtr                   =typ_UIntPtr                     
    ; typ_RuntimeArgumentHandle  =typ_RuntimeArgumentHandle    
    ; typ_RuntimeTypeHandle      =typ_RuntimeTypeHandle        
    ; typ_RuntimeMethodHandle    =typ_RuntimeMethodHandle      
    ; typ_RuntimeFieldHandle     =typ_RuntimeFieldHandle       
                                                                               
    ; typ_Byte                   =typ_Byte                     
    ; typ_Int16                  =typ_Int16                    
    ; typ_Int32                  =typ_Int32                    
    ; typ_Int64                  =typ_Int64                    
    ; typ_SByte                  =typ_SByte                    
    ; typ_UInt16                 =typ_UInt16                   
    ; typ_UInt32                 =typ_UInt32                   
    ; typ_UInt64                 =typ_UInt64                   
    ; typ_Single                 =typ_Single                   
    ; typ_Double                 =typ_Double                   
    ; typ_Bool                   =typ_Bool                     
    ; typ_Char                   =typ_Char                     
    ; typ_SerializationInfo=typ_SerializationInfo
    ; typ_StreamingContext=typ_StreamingContext
    ; tref_SecurityPermissionAttribute=tref_SecurityPermissionAttribute
    ; tspec_Exception            =tspec_Exception              
    ; typ_Exception              =typ_Exception
    ; generatedAttribsCache = []
    ; debuggerBrowsableNeverAttributeCache = None                 
    ; debuggerTypeProxyAttributeCache = None                 }

        
(* NOTE: ecma_ prefix refers to the standard "mscorlib" *)
let ecmaPublicKey = PublicKeyToken (Bytes.ofInt32Array [|0xde; 0xad; 0xbe; 0xef; 0xca; 0xfe; 0xfa; 0xce |]) 

let ecmaMscorlibScopeRef = ILScopeRef.Assembly (ILAssemblyRef.Create("mscorlib", None, Some ecmaPublicKey, true, None, None))

let ecmaILGlobals = mkILGlobals ecmaMscorlibScopeRef None false
   
let mkInitializeArrayMethSpec ilg = 
  mkILNonGenericStaticMethSpecInTy(mkILNonGenericBoxedTy(mkILTyRef(ilg.mscorlibScopeRef,"System.Runtime.CompilerServices.RuntimeHelpers")),"InitializeArray", [ilg.typ_Array;ilg.typ_RuntimeFieldHandle], ILType.Void)
(* e.ilg. [mkMscorlibExnNewobj "System.InvalidCastException"] *)
let mkMscorlibExnNewobj ilg eclass = 
  mkNormalNewobj (mkILNonGenericCtorMethSpec (mkILTyRef(ilg.mscorlibScopeRef,eclass),[]))

let typ_is_boxed = function ILType.Boxed _ -> true | _ -> false
let typ_is_value = function ILType.Value _ -> true | _ -> false


let tspec_is_mscorlib ilg (tspec:ILTypeSpec) n = 
  let tref = tspec.TypeRef
  let scoref = tref.Scope
  (tref.Name = n) &&
  match scoref with
  | ILScopeRef.Assembly n -> n.Name = ilg.mscorlibAssemblyName 
  | ILScopeRef.Module _ -> false
  | ILScopeRef.Local -> true

let typ_is_boxed_mscorlib_typ ilg (ty:ILType) n = 
  typ_is_boxed ty && tspec_is_mscorlib ilg ty.TypeSpec n

let typ_is_value_mscorlib_typ ilg (ty:ILType) n = 
  typ_is_value ty && tspec_is_mscorlib ilg ty.TypeSpec n
      
let isILObjectTy            ilg ty = typ_is_boxed_mscorlib_typ ilg ty tname_Object
let isILStringTy            ilg ty = typ_is_boxed_mscorlib_typ ilg ty tname_String
let typ_is_AsyncCallback     ilg ty = typ_is_boxed_mscorlib_typ ilg ty tname_AsyncCallback
let isILTypedReferenceTy    ilg ty = typ_is_value_mscorlib_typ ilg ty tname_TypedReference
let typ_is_IAsyncResult ilg ty = typ_is_boxed_mscorlib_typ ilg ty tname_IAsyncResult
let typ_is_IComparable  ilg ty = typ_is_boxed_mscorlib_typ ilg ty tname_IComparable
let isILSByteTy        ilg ty = typ_is_value_mscorlib_typ ilg ty tname_SByte
let isILByteTy         ilg ty = typ_is_value_mscorlib_typ ilg ty tname_Byte
let isILInt16Ty        ilg ty = typ_is_value_mscorlib_typ ilg ty tname_Int16
let isILUInt16Ty       ilg ty = typ_is_value_mscorlib_typ ilg ty tname_UInt16
let isILInt32Ty        ilg ty = typ_is_value_mscorlib_typ ilg ty tname_Int32
let isILUInt32Ty       ilg ty = typ_is_value_mscorlib_typ ilg ty tname_UInt32
let isILInt64Ty        ilg ty = typ_is_value_mscorlib_typ ilg ty tname_Int64
let isILUInt64Ty       ilg ty = typ_is_value_mscorlib_typ ilg ty tname_UInt64
let isILIntPtrTy       ilg ty = typ_is_value_mscorlib_typ ilg ty tname_IntPtr
let isILUIntPtrTy      ilg ty = typ_is_value_mscorlib_typ ilg ty tname_UIntPtr
let isILBoolTy         ilg ty = typ_is_value_mscorlib_typ ilg ty tname_Bool
let isILCharTy         ilg ty = typ_is_value_mscorlib_typ ilg ty tname_Char
let isILSingleTy       ilg ty = typ_is_value_mscorlib_typ ilg ty tname_Single
let isILDoubleTy       ilg ty = typ_is_value_mscorlib_typ ilg ty tname_Double

// -------------------------------------------------------------------- 
// Rescoping
// -------------------------------------------------------------------- 


let qrescope_scoref scoref scoref_old = 
    match scoref,scoref_old with 
    | _,ILScopeRef.Local -> Some scoref
    | ILScopeRef.Local,_ -> None
    | _,ILScopeRef.Module _ -> Some scoref
    | ILScopeRef.Module _,_ -> None
    | _ -> None
let qrescope_tref scoref (x:ILTypeRef) = 
    match qrescope_scoref scoref x.Scope with 
    | None -> None
    | Some s -> Some (ILTypeRef.Create(s,x.Enclosing,x.Name))

let rescopeILScopeRef x y = match qrescope_scoref x y with Some x -> x | None -> y
let rescope_tref x y = match qrescope_tref x y with Some x -> x | None -> y

// ORIGINAL IMPLEMENTATION (too many allocations
//         { tspecTypeRef=rescope_tref scoref tref;
//           tspecInst=rescope_inst scoref tinst } 
let rec qrescope_tspec scoref (tspec:ILTypeSpec) = 
  let tref = tspec.TypeRef
  let tinst = tspec.GenericArgs
  let qtref = qrescope_tref scoref tref
  match tinst,qtref with 
  | [],None -> None  (* avoid reallocation in the common case *)
  | _,None -> 
      Some (ILTypeSpec.Create (tref, rescope_inst scoref tinst))
  | _,Some tref -> 
      Some (ILTypeSpec.Create (tref, rescope_inst scoref tinst))
and rescopeILTypeSpec x y = match qrescope_tspec x y with Some x -> x | None -> y
and rescopeILType scoref typ = 
    match typ with 
    | ILType.Ptr t -> ILType.Ptr (rescopeILType scoref t)
    | ILType.FunctionPointer t -> ILType.FunctionPointer (rescope_callsig scoref t)
    | ILType.Byref t -> ILType.Byref (rescopeILType scoref t)
    | ILType.Boxed cr -> 
        match qrescope_tspec scoref cr with 
        | Some res -> ILType.Boxed res
        | None -> typ  // avoid reallocation in the common case 
    | ILType.Array (s,ty) -> ILType.Array (s,rescopeILType scoref ty)
    | ILType.Value cr -> 
        match qrescope_tspec scoref cr with 
        | Some res -> ILType.Value res
        | None -> typ  // avoid reallocation in the common case 
    | ILType.Modified(b,tref,ty) -> ILType.Modified(b,rescope_tref scoref tref, rescopeILType scoref ty)
    | x -> x
and rescope_inst scoref i = List.map (rescopeILType scoref) i
and rescope_callsig scoref  csig = 
  mkILCallSig
    (csig.CallingConv,List.map (rescopeILType scoref) csig.ArgTypes,rescopeILType scoref csig.ReturnType)

let rescopeILMethodRef scoref (x:ILMethodRef) =
  { mrefParent = rescope_tref scoref x.EnclosingTypeRef;
    mrefCallconv = x.mrefCallconv;
    mrefGenericArity=x.mrefGenericArity;
    mrefName=x.mrefName;
    mrefArgs = List.map (rescopeILType scoref) x.mrefArgs;
    mrefReturn= rescopeILType scoref x.mrefReturn }

let rescopeILFieldRef scoref x = 
  { EnclosingTypeRef = rescope_tref scoref x.EnclosingTypeRef;
    Name= x.Name;
    Type= rescopeILType scoref x.Type }

// -------------------------------------------------------------------- 
// Instantiate polymorphism in types
// -------------------------------------------------------------------- 

let rec inst_tspec_aux num_free inst (tspec:ILTypeSpec) = 
  ILTypeSpec.Create(tspec.TypeRef,inst_inst_aux num_free inst tspec.GenericArgs) 
  
and instILTypeAux num_free inst typ = 
  match typ with 
  | ILType.Ptr t       -> ILType.Ptr (instILTypeAux num_free inst t)
  | ILType.FunctionPointer t      -> ILType.FunctionPointer (inst_callsig_aux num_free inst t)
  | ILType.Array (a,t) -> ILType.Array (a,instILTypeAux num_free inst t)
  | ILType.Byref t     -> ILType.Byref (instILTypeAux num_free inst t)
  | ILType.Boxed cr    -> ILType.Boxed (inst_tspec_aux num_free inst cr)
  | ILType.Value cr    -> ILType.Value (inst_tspec_aux num_free inst cr)
  | ILType.TypeVar  v -> 
      let v = int v
      let top = List.length inst
      if v < num_free then typ else
      if v - num_free >= top then ILType.TypeVar (uint16 (v - top)) else 
      instRead inst (uint16 (v - num_free)) 
  | x -> x
    
and inst_inst_aux num_free inst i = List.map (instILTypeAux num_free inst) i
and inst_callsig_aux num_free inst  csig = 
  mkILCallSig 
    (csig.CallingConv,List.map (instILTypeAux num_free inst) csig.ArgTypes,instILTypeAux num_free inst csig.ReturnType)

let instILType     i t = instILTypeAux 0 i t

// --------------------------------------------------------------------
// MS-IL: Parameters, Return types and Locals
// -------------------------------------------------------------------- 

let mkILParam (name,ty) =
    { Name=name;
      Default=None;
      Marshal=None;
      IsIn=false;
      IsOut=false;
      IsOptional=false;
      Type=ty;
      CustomAttrs=emptyILCustomAttrs }
let mkILParamNamed (s,ty) = mkILParam (Some s,ty)
let mkILParamAnon ty = mkILParam (None,ty)

let mkILReturn ty : ILReturn = 
    { Marshal=None;
      Type=ty;
      CustomAttrs=emptyILCustomAttrs  }

let mkILLocal ty = 
    { IsPinned=false;
      Type=ty; }

type ILFieldSpec with
  member fr.ActualType = 
      let env = fr.EnclosingType.GenericArgs
      instILType env fr.FormalType

// -------------------------------------------------------------------- 
// Make a method mbody
// -------------------------------------------------------------------- 

let mkLdcInt32 i = AI_ldc (DT_I4,ILConst.I4 i)


let mkILMethodBody (zeroinit,locals,maxstack,code,tag) = 
  { IsZeroInit=zeroinit;
    MaxStack=maxstack;
    NoInlining=false;
    Locals=locals;
    Code= code;
    SourceMarker=tag }

let mkMethodBody info = MethodBody.IL (mkILMethodBody info)

// -------------------------------------------------------------------- 
// Make a constructor
// -------------------------------------------------------------------- 

let mk_void_return = mkILReturn ILType.Void

let mkILCtor (access,args,impl) = 
    { Name=".ctor";
      mdKind=MethodKind.Ctor;
      CallingConv=ILCallingConv.Instance;
      Parameters=args;
      Return= mk_void_return;
      Access=access;
      mdBody= mkMethBodyAux impl;
      mdCodeKind=MethodCodeKind.IL;
      IsInternalCall=false;
      IsManaged=true;
      IsForwardRef=false;
      SecurityDecls=emptyILSecurityDecls;
      HasSecurity=false;
      IsEntryPoint=false;
      GenericParams=mkILEmptyGenericParams;
      IsReqSecObj=false;
      IsHideBySig=false;
      IsSpecialName=true;
      IsUnmanagedExport=false;
      IsSynchronized=false;
      IsMustRun=false;
      IsPreserveSig=false;
      CustomAttrs = emptyILCustomAttrs; }

// -------------------------------------------------------------------- 
// Do-nothing ctor, just pass on to monomorphic superclass
// -------------------------------------------------------------------- 

let mkCallBaseConstructor (typ,args: ILType list) =
    [ mkLdarg0; ] @
    List.mapi (fun i _ -> I_ldarg (uint16 (i+1))) args @
    [ mkNormalCall (mkILCtorMethSpecForTy (typ,[])) ]

let mkNormalStfld fspec = I_stfld (Aligned,Nonvolatile,fspec)
let mkNormalStsfld fspec = I_stsfld (Nonvolatile,fspec)
let mkNormalLdsfld fspec = I_ldsfld (Nonvolatile,fspec)
let mkNormalLdfld fspec = I_ldfld (Aligned,Nonvolatile,fspec)
let mkNormalLdflda fspec = I_ldflda fspec
let mkNormalLdobj dt = I_ldobj(Aligned,Nonvolatile,dt)
let mkNormalStobj dt = I_stobj(Aligned,Nonvolatile,dt)

let mkILNonGenericEmptyCtor tag superTy = 
    let ctor = mkCallBaseConstructor (superTy,[])
    mkILCtor(ILMemberAccess.Public,[],mkMethodBody(false,[],8, nonBranchingInstrsToCode ctor,tag))

// -------------------------------------------------------------------- 
// Make a static, top level monomophic method - very useful for
// creating helper ILMethodDefs for internal use.
// -------------------------------------------------------------------- 
let mkILStaticMethod (genparams,nm,access,args,ret,impl) = 
    { GenericParams=genparams;
      Name=nm;
      CallingConv = ILCallingConv.Static;
      mdKind=MethodKind.Static;
      Parameters=  args;
      Return= ret;
      Access=access;
      HasSecurity=false;
      SecurityDecls=emptyILSecurityDecls;
      IsEntryPoint=false;
      CustomAttrs = emptyILCustomAttrs;
      mdBody= mkMethBodyAux impl;
      mdCodeKind=MethodCodeKind.IL;
      IsInternalCall=false;
      IsManaged=true;
      IsForwardRef=false;
      IsReqSecObj=false;
      IsHideBySig=false;
      IsSpecialName=false;
      IsUnmanagedExport=false;
      IsSynchronized=false;
      IsMustRun=false;
      IsPreserveSig=false; }

let mkILNonGenericStaticMethod (nm,access,args,ret,impl) = 
    mkILStaticMethod (mkILEmptyGenericParams,nm,access,args,ret,impl)

let mkILClassCtor impl = 
    { Name=".cctor";
      CallingConv=ILCallingConv.Static;
      GenericParams=mkILEmptyGenericParams;
      mdKind=MethodKind.Cctor;
      Parameters=[];
      Return=mk_void_return;
      Access=ILMemberAccess.Private; 
      IsEntryPoint=false;
      HasSecurity=false;
      SecurityDecls=emptyILSecurityDecls;
      CustomAttrs = emptyILCustomAttrs;
      mdBody= mkMethBodyAux impl; 
      mdCodeKind=MethodCodeKind.IL;
      IsInternalCall=false;
      IsManaged=true;
      IsForwardRef=false;
      IsReqSecObj=false;
      IsHideBySig=false;
      IsSpecialName=true;
      IsUnmanagedExport=false; 
      IsSynchronized=false;
      IsMustRun=false;
      IsPreserveSig=false;  } 

// -------------------------------------------------------------------- 
// Make a virtual method, where the overriding is simply the default
// (i.e. overrides by name/signature)
// -------------------------------------------------------------------- 

let mk_ospec (typ:ILType,callconv,nm,genparams,formal_args,formal_ret) =
  OverridesSpec (mkILMethRef (typ.TypeRef, callconv, nm, genparams, formal_args,formal_ret), typ)

let mkILGenericVirtualMethod (nm,access,genparams,actual_args,actual_ret,impl) = 
  { Name=nm;
    GenericParams=genparams;
    CallingConv=ILCallingConv.Instance;
    mdKind=
      MethodKind.Virtual 
        { IsFinal=false; 
          IsNewSlot = false;
          IsCheckAccessOnOverride=true;
          IsAbstract=(match impl with MethodBody.Abstract -> true | _ -> false) ; };
    Parameters= actual_args;
    Return=actual_ret;
    Access=access;
    IsEntryPoint=false;
    HasSecurity=false;
    SecurityDecls=emptyILSecurityDecls;
    CustomAttrs = emptyILCustomAttrs;
    mdBody= mkMethBodyAux impl;
    mdCodeKind=MethodCodeKind.IL;
    IsInternalCall=false;
    IsManaged=true;
    IsForwardRef=false;
    IsReqSecObj=false;
    IsHideBySig=false;
    IsSpecialName=false;
    IsUnmanagedExport=false; 
    IsSynchronized=false;
    IsMustRun=false;
    IsPreserveSig=false; }
    
let mkILNonGenericVirtualMethod (nm,access,args,ret,impl) =  
  mkILGenericVirtualMethod (nm,access,mkILEmptyGenericParams,args,ret,impl)

let mkILGenericNonVirtualMethod (nm,access,genparams, actual_args,actual_ret, impl) = 
  { Name=nm;
    GenericParams=genparams;
    CallingConv=ILCallingConv.Instance;
    mdKind=MethodKind.NonVirtual;
    Parameters= actual_args;
    Return=actual_ret;
    Access=access;
    IsEntryPoint=false;
    HasSecurity=false;
    SecurityDecls=emptyILSecurityDecls;
    CustomAttrs = emptyILCustomAttrs;
    mdBody= mkMethBodyAux impl;
    mdCodeKind=MethodCodeKind.IL;
    IsInternalCall=false;
    IsManaged=true;
    IsForwardRef=false;
    IsReqSecObj=false;
    IsHideBySig=false;
    IsSpecialName=false;
    IsUnmanagedExport=false; 
    IsSynchronized=false;
    IsMustRun=false;
    IsPreserveSig=false; }
    
let mkILNonGenericInstanceMethod (nm,access,args,ret,impl) =  
  mkILGenericNonVirtualMethod (nm,access,mkILEmptyGenericParams,args,ret,impl)


// -------------------------------------------------------------------- 
// Add some code to the end of the .cctor for a type.  Create a .cctor
// if one doesn't exist already.
// -------------------------------------------------------------------- 

let ilmbody_code2code f il  =
  {il with Code = f il.Code}

let mdef_code2code f md  =
    let il = 
        match md.mdBody.Contents with 
        | MethodBody.IL il-> il 
        | _ -> failwith "mdef_code2code - method not IL"
    let b = MethodBody.IL (ilmbody_code2code f il)
    {md with mdBody= mkMethBodyAux b }  

let prependInstrsToCode c1 c2 = 
    let internalLab = generateCodeLabel ()
    join_code (checkILCode (mkBasicBlock {Label=internalLab; 
                                       Instructions=Array.ofList (c1 @ [ I_br (uniqueEntryOfCode c2)])})) c2

let prependInstrsToMethod new_code md  = 
    mdef_code2code (prependInstrsToCode new_code) md

(* Creates cctor if needed *)
let cdef_cctorCode2CodeOrCreate tag f cd = 
    let mdefs = cd.Methods
    let md,mdefs = 
        match mdefs.FindByName ".cctor" with 
        | [mdef] -> mdef,filterILMethodDefs (fun md -> md.Name <> ".cctor") mdefs
        | [] -> mkILClassCtor (mkMethodBody (false,[],1,nonBranchingInstrsToCode [ ],tag)), mdefs
        | _ -> failwith "bad method table: more than one .cctor found"
    let md' = f md
    {cd with Methods = addILMethod md' mdefs}


let code_of_mdef (md:ILMethodDef) = 
    match md.Code with 
    | Some x -> x
    | None -> failwith "code_of_mdef: not IL" 

let mkRefToILMethod (tref, md: ILMethodDef) =
    mkILMethRef (tref, md.CallingConv, md.Name, md.GenericParams.Length, md.ParameterTypes, md.Return.Type)

let mkRefToILField (tref,fdef:ILFieldDef) =   mkILFieldRef (tref, fdef.Name, fdef.Type)

let mkRefForILMethod scope (tdefs,tdef) mdef = mkRefToILMethod (mkRefForNestedILTypeDef scope (tdefs,tdef), mdef)
let mkRefForILField scope (tdefs,tdef) (fdef:ILFieldDef) = mkILFieldRef (mkRefForNestedILTypeDef scope (tdefs,tdef), fdef.Name, fdef.Type)


(* Creates cctor if needed *)
let prependInstrsToClassCtor instrs tag cd = 
    cdef_cctorCode2CodeOrCreate tag (prependInstrsToMethod instrs) cd
    

let mkILField (isStatic,nm,ty,init,at,access,isLiteral) =
   { Name=nm;
     Type=ty;
     IsStatic = isStatic; 
     LiteralValue = init;
     Data=at;
     Offset=None;
     IsSpecialName = false;
     Marshal=None; 
     NotSerialized=false;
     IsInitOnly = false;
     IsLiteral = isLiteral; 
     Access = access; 
     CustomAttrs=emptyILCustomAttrs }

let mkILInstanceField (nm,ty,init,access) = mkILField (false,nm,ty,init,None,access,false)
let mkILStaticField (nm,ty,init,at,access) = mkILField (true,nm,ty,init,at,access,false)
let mkILLiteralField (nm,ty,init,at,access) = mkILField (true, nm, ty, Some init, at, access, true)

// -------------------------------------------------------------------- 
// Scopes for allocating new temporary variables.
// -------------------------------------------------------------------- 

type ILLocalsAllocator(numPrealloc:int) = 
    let newLocals = ResizeArray<ILLocal>()
    member tmps.AllocLocal loc = 
        let locn = uint16(numPrealloc + newLocals.Count)
        newLocals.Add loc;
        locn

    member tmps.Close() = ResizeArray.toList newLocals


let mkILFieldsLazy l =  Fields (LazyOrderedMultiMap((fun (f:ILFieldDef) -> f.Name),l))
let mkILFields l =  mkILFieldsLazy (notlazy l)
let emptyILFields = mkILFields []

let mkILEventsLazy l =  Events (LazyOrderedMultiMap((fun (e: ILEventDef) -> e.Name),l))
let mkILEvents l =  mkILEventsLazy (notlazy l)
let emptyILEvents =  mkILEvents []

let mkILPropertiesLazy l =  Properties (LazyOrderedMultiMap((fun (p: ILPropertyDef) -> p.Name),l) )
let mkILProperties l =  mkILPropertiesLazy (notlazy l)
let emptyILProperties =  mkILProperties []

let addExportedTypeToTable (y: ILExportedTypeOrForwarder) tab = Map.add y.Name y tab
let mkILExportedTypes l =  ILExportedTypesAndForwarders (notlazy (List.foldBack addExportedTypeToTable l Map.empty))
let mkILExportedTypesLazy (l:Lazy<_>) =   ILExportedTypesAndForwarders (lazy (List.foldBack addExportedTypeToTable (l.Force()) Map.empty))

let addNestedExportedTypeToTable (y: ILNestedExportedType) tab =
    Map.add y.Name y tab

let mkILNestedExportedTypes l =  
    ILNestedExportedTypes (notlazy (List.foldBack addNestedExportedTypeToTable l Map.empty))

let mkILNestedExportedTypesLazy (l:Lazy<_>) =  
    ILNestedExportedTypes (lazy (List.foldBack addNestedExportedTypeToTable (l.Force()) Map.empty))

let mkILResources l =  ILResources (notlazy l)
let mkILResourcesLazy l =  ILResources l

let addMethodImplToTable y tab =
    let key = (y.Overrides.MethodRef.Name,y.Overrides.MethodRef.ArgTypes.Length)
    let prev = Map.tryFindMulti key tab
    Map.add key (y::prev) tab

let mkILMethodImpls l =  MethodImpls (notlazy (List.foldBack addMethodImplToTable l Map.empty))
let mkILMethodImplsLazy l =  MethodImpls (lazy (List.foldBack addMethodImplToTable (Lazy.force l) Map.empty))
let emptyILMethodImpls =  mkILMethodImpls []


// -------------------------------------------------------------------- 
// Make a constructor that simply takes its arguments and stuffs
// them in fields.  preblock is how to call the superclass constructor....
// -------------------------------------------------------------------- 

let mkILStorageCtorWithParamNames(tag,preblock,typ,flds,access) = 
    mkILCtor(access,
            flds |> List.map (fun (pnm,_,ty) -> mkILParamNamed (pnm,ty)),
            mkMethodBody
              (false,[],2,
               nonBranchingInstrsToCode
                 begin 
                   (match tag with Some x -> [I_seqpoint x] | None -> []) @ 
                   preblock @
                   List.concat (List.mapi (fun n (_pnm,nm,ty) -> 
                     [ mkLdarg0;
                       I_ldarg (uint16 (n+1));
                       mkNormalStfld (mkILFieldSpecInTy (typ,nm,ty));
                     ])  flds)
                 end,tag))
    
let mkILSimpleStorageCtorWithParamNames(tag,base_tspec,typ,flds,access) = 
    let preblock = 
      match base_tspec with 
        None -> []
      | Some tspec -> 
          ([ mkLdarg0; 
             mkNormalCall (mkILCtorMethSpecForTy (ILType.Boxed tspec,[])) ])
    mkILStorageCtorWithParamNames(tag,preblock,typ,flds,access)

let addParamNames flds = 
    flds |> List.map (fun (nm,ty) -> (nm,nm,ty))

let mkILSimpleStorageCtor(tag,base_tspec,typ,flds,access) = 
    mkILSimpleStorageCtorWithParamNames(tag,base_tspec,typ, addParamNames flds, access)

let mkILStorageCtor(tag,preblock,typ,flds,access) = mkILStorageCtorWithParamNames(tag,preblock,typ, addParamNames flds, access)


let mkILGenericClass (nm, access, genparams, extends, impl, methods, fields, nestedTypes, props, events, attrs, init) =
  { tdKind=ILTypeDefKind.Class;
    Name=nm;
    GenericParams= genparams;
    Access = access;
    Implements = impl;
    IsAbstract = false;
    IsSealed = false;
    IsSerializable = false;
    IsComInterop=false;
    IsSpecialName=false;
    Layout=ILTypeDefLayout.Auto;
    Encoding=ILDefaultPInvokeEncoding.Ansi;
    InitSemantics=init;
    Extends = Some extends;
    Methods= methods; 
    Fields= fields;
    NestedTypes=nestedTypes;
    CustomAttrs=attrs;
    MethodImpls=emptyILMethodImpls;
    Properties=props;
    Events=events;
    SecurityDecls=emptyILSecurityDecls; 
    HasSecurity=false;
} 
    
let mkRawDataValueTypeDef ilg (nm,size,pack) =
  { tdKind=ILTypeDefKind.ValueType;
    Name = nm;
    GenericParams= [];
    Access = ILTypeDefAccess.Private;
    Implements = [];
    IsAbstract = false;
    IsSealed = true;
    Extends = Some ilg.typ_ValueType;
    IsComInterop=false;    
    IsSerializable = false;
    IsSpecialName=false;
    Layout=ILTypeDefLayout.Explicit { Size=Some size; Pack=Some pack };
    Encoding=ILDefaultPInvokeEncoding.Ansi;
    InitSemantics=ILTypeInit.BeforeField;
    Methods= emptyILMethods; 
    Fields= emptyILFields;
    NestedTypes=emptyILTypeDefs;
    CustomAttrs=emptyILCustomAttrs;
    MethodImpls=emptyILMethodImpls;
    Properties=emptyILProperties;
    Events=emptyILEvents;
    SecurityDecls=emptyILSecurityDecls; 
    HasSecurity=false;  }


let mkILSimpleClass ilg (nm, access, methods, fields, nestedTypes, props, events, attrs, init) =
  mkILGenericClass (nm,access, mkILEmptyGenericParams, ilg.typ_Object, [], methods, fields, nestedTypes, props, events, attrs, init)

let mkILTypeDefForGlobalFunctions ilg (methods,fields) = mkILSimpleClass ilg (typeNameForGlobalFunctions, ILTypeDefAccess.Public, methods, fields, emptyILTypeDefs, emptyILProperties, emptyILEvents, emptyILCustomAttrs,ILTypeInit.BeforeField)

let destTypeDefsWithGlobalFunctionsFirst ilg (tdefs: ILTypeDefs) = 
  let l = tdefs.AsList
  let top,nontop = l |> List.partition (fun td -> td.Name = typeNameForGlobalFunctions)
  let top2 = if isNil top then [mkILTypeDefForGlobalFunctions ilg (emptyILMethods, emptyILFields)] else top
  top2@nontop

let mkILSimpleModule assname modname dll tdefs hashalg locale flags exportedTypes metadataVersion = 
    { Manifest= 
        Some { Name=assname;
               AuxModuleHashAlgorithm= match hashalg with | Some(alg) -> alg | _ -> 0x8004; // SHA1
               SecurityDecls=emptyILSecurityDecls;
               PublicKey= None;
               Version= None;
               Locale=locale
               CustomAttrs=emptyILCustomAttrs;
               AssemblyLongevity=ILAssemblyLongevity.Unspecified;
               DisableJitOptimizations= 0 <> (flags &&& 0x4000);
               JitTracking=0 <> (flags &&& 0x8000); // always turn these on
               Retargetable= 0 <> (flags &&& 0xff);
               ExportedTypes=exportedTypes;
               EntrypointElsewhere=None
             };
      CustomAttrs=emptyILCustomAttrs;
      Name=modname;
      NativeResources=[];
      TypeDefs=tdefs;
      SubSystemFlags=defaultSubSystem;
      IsDLL=dll;
      IsILOnly=true;
      Platform=None;
      StackReserveSize=None;
      Is32Bit=false;
      Is64Bit=false;
      PhysicalAlignment=defaultPhysAlignment;
      VirtualAlignment=defaultVirtAlignment;
      ImageBase=defaultImageBase;
      MetadataVersion=metadataVersion;
      Resources=mkILResources [];
    }


//-----------------------------------------------------------------------
// Intermediate parsing structure for exception tables....
//-----------------------------------------------------------------------

type ILExceptionClause = 
    | Finally of (ILCodeLabel * ILCodeLabel)
    | Fault  of (ILCodeLabel * ILCodeLabel)
    | FilterCatch of (ILCodeLabel * ILCodeLabel) * (ILCodeLabel * ILCodeLabel)
    | TypeCatch of ILType * (ILCodeLabel * ILCodeLabel)

type ILExceptionSpec = 
    { exnRange: (ILCodeLabel * ILCodeLabel);
      exnClauses: ILExceptionClause list }

type exceptions = ILExceptionSpec list

//-----------------------------------------------------------------------
// [instructions_to_code] makes the basic block structure of code from
// a primitive array of instructions.  We
// do this be iterating over the instructions, pushing new basic blocks 
// everytime we encounter an address that has been recorded
// [bbstartToCodeLabelMap].
//-----------------------------------------------------------------------

type ILLocalSpec = 
    { locRange: (ILCodeLabel * ILCodeLabel);
      locInfos: ILDebugMapping list }

type structspec = SEH of ILExceptionSpec | LOCAL of ILLocalSpec 

let delayInsertedToWorkaroundKnownNgenBug _s f = 
    (* Some random code to prevent inlining of this function *)
    let mutable res = 10
    for i = 0 to 2 do 
       res <- res + 1;
    //printf "------------------------executing NGEN bug delay '%s', calling 'f' --------------\n" s;
    let res = f()
    //printf "------------------------exiting NGEN bug delay '%s' --------------\n" s;
    res


let popRangeM lo hi (m:Zmap<'Key,'U>) =
    let collect k v (rvs,m) = (v :: rvs) , Zmap.remove k m
    let rvs,m = Zmap.foldSection lo hi collect m ([],m)
    List.rev rvs,m

type BasicBlockStartsToCodeLabelsMap(instrs,tryspecs,localspecs,lab2pc) = 

    // Find all the interesting looking labels that form the boundaries of basic blocks. 
    // These are the destinations of branches and the boundaries of both exceptions and 
    // those blocks where locals are live. 
    let bbstartToCodeLabelMap = 
        let res = ref CodeLabels.empty
        let add_range (a,b) = res := CodeLabels.insert a (CodeLabels.insert b !res)
        instrs |> Array.iter (fun i -> res := CodeLabels.addList (destinationsOfInstr i) !res);

        tryspecs |> List.iter (fun espec -> 
          add_range espec.exnRange;
          List.iter (function 
            | Finally r1 | Fault r1 | TypeCatch (_,r1)-> add_range r1
            | FilterCatch (r1,r2) -> add_range r1; add_range r2) espec.exnClauses);

        localspecs |> List.iter (fun l -> add_range l.locRange) ;

        !res 

    // Construct a map that gives a unique ILCodeLabel for each label that 
    // might be a boundary of a basic block.  These will be the labels 
    // for the basic blocks we end up creating. 
    let lab2clMap = Dictionary<_,_>(10, HashIdentity.Structural) 
    let pc2clMap = Dictionary<_,_>(10, HashIdentity.Structural) 
    let addBBstartPc pc pcs cls = 
        if pc2clMap.ContainsKey pc then 
            pc2clMap.[pc], pcs, cls
        else 
            let cl = generateCodeLabel ()
            pc2clMap.[pc] <- cl;
            cl, pc::pcs, CodeLabels.insert cl cls 

    let bbstartPcs, bbstart_code_labs  = 
      CodeLabels.fold
        (fun bbstart_lab (pcs, cls) -> 
          let pc = lab2pc bbstart_lab
          if logging then dprintf "bblock starts with label %s at pc %d\n" (formatCodeLabel bbstart_lab) pc;
          let cl,pcs',cls' = addBBstartPc pc pcs cls
          lab2clMap.[bbstart_lab] <- cl;
          pcs',
          cls')
        bbstartToCodeLabelMap 
        ([], CodeLabels.empty) 
    let cl0,bbstartPcs, bbstart_code_labs = addBBstartPc 0 bbstartPcs bbstart_code_labs 
    
    
    member c.InitialCodeLabel = cl0
    member c.BasicBlockStartPositions = bbstartPcs
    member c.BasicBlockStartCodeLabels = bbstart_code_labs

    member c.lab2cl bbLab = 
        try 
            lab2clMap.[bbLab]
        with :? KeyNotFoundException -> failwith ("basic block label "+formatCodeLabel bbLab+" not declared")  

    member c.pc2cl pc = 
        try 
            pc2clMap.[pc] 
        with :? KeyNotFoundException -> 
            failwith ("internal error while mapping pc "+string pc+" to code label")  

    member c.remapLabels i =
        match i with 
        | I_leave l -> I_leave(c.lab2cl l)
        | I_br l -> I_br (c.lab2cl l)
        | I_other e -> I_other (find_extension "instr" (fun ext -> if ext.internalInstrExtIs e then Some (ext.internalInstrExtRelabel c.lab2cl e) else None) !instrExtensions)
        | I_brcmp (x,l1,l2) -> I_brcmp(x,c.lab2cl l1, c.lab2cl l2)
        | I_switch (ls,l) -> I_switch(List.map c.lab2cl ls, c.lab2cl l)
        | _ -> i 

let disjoint_range (start_pc1,end_pc1) (start_pc2,end_pc2) =
  ((start_pc1 : int) < start_pc2 && end_pc1 <= start_pc2) ||
  (start_pc1 >= end_pc2 && end_pc1 > end_pc2) 

let merge_ranges (start_pc1,end_pc1) (start_pc2,end_pc2) =
  (min (start_pc1:int) start_pc2, max (end_pc1:int) end_pc2) 

let rangeInsideRange (start_pc1,end_pc1) (start_pc2,end_pc2)  =
  (start_pc1:int) >= start_pc2 && start_pc1 < end_pc2 &&
  (end_pc1:int) > start_pc2 && end_pc1 <= end_pc2 

let lranges_of_clause cl = 
  match cl with 
  | Finally r1 -> [r1]
  | Fault r1 -> [r1]
  | FilterCatch (r1,r2) -> [r1;r2]
  | TypeCatch (_ty,r1) -> [r1]  

  
type CodeOffsetViewOfLabelledItems(lab2pc) =
    member x.labelsToRange p = let (l1,l2) = p in lab2pc l1, lab2pc l2 

    member x.lrange_inside_lrange ls1 ls2 = 
      rangeInsideRange (x.labelsToRange ls1) (x.labelsToRange ls2) 
      
    member x.disjoint_lranges ls1 ls2 = 
      disjoint_range (x.labelsToRange ls1) (x.labelsToRange ls2) 

    member x.clause_inside_lrange cl lr =
      List.forall (fun lr1 -> x.lrange_inside_lrange lr1 lr) (lranges_of_clause cl) 

    member x.clauses_inside_lrange cls lr = 
      List.forall 
        (fun cl -> x.clause_inside_lrange cl lr)
        cls 
    
    member x.tryspec_inside_lrange tryspec1 lr =
      (x.lrange_inside_lrange tryspec1.exnRange lr &&
       x.clauses_inside_lrange tryspec1.exnClauses lr) 

    member x.tryspec_inside_clause tryspec1 cl =
      List.exists (fun lr -> x.tryspec_inside_lrange tryspec1 lr) (lranges_of_clause cl) 

    member x.locspec_inside_clause locspec1 cl =
      List.exists (fun lr -> x.lrange_inside_lrange locspec1.locRange lr) (lranges_of_clause cl) 

    member x.tryspec_inside_tryspec tryspec1 tryspec2 =
      x.tryspec_inside_lrange tryspec1 tryspec2.exnRange ||
      List.exists (fun c2 -> x.tryspec_inside_clause tryspec1 c2) tryspec2.exnClauses 
    
    member x.locspec_inside_tryspec locspec1 tryspec2 =
      x.lrange_inside_lrange locspec1.locRange tryspec2.exnRange ||
      List.exists (fun c2 -> x.locspec_inside_clause locspec1 c2) tryspec2.exnClauses 
    
    member x.tryspec_inside_locspec tryspec1 locspec2 =
      x.tryspec_inside_lrange tryspec1 locspec2.locRange 
    
    member x.disjoint_clause_and_lrange cl lr =
      List.forall (fun lr1 -> x.disjoint_lranges lr1 lr) (lranges_of_clause cl) 
    
    member x.disjoint_clauses_and_lrange cls lr = 
      List.forall (fun cl -> x.disjoint_clause_and_lrange cl lr) cls 
    
    member x.disjoint_tryspec_and_lrange tryspec1 lr =
      (x.disjoint_lranges tryspec1.exnRange lr &&
       x.disjoint_clauses_and_lrange tryspec1.exnClauses lr) 
    
    member x.disjoint_tryspec_and_clause tryspec1 cl =
      List.forall (fun lr -> x.disjoint_tryspec_and_lrange tryspec1 lr) (lranges_of_clause cl) 

    member x.tryspec_disjoint_from_tryspec tryspec1 tryspec2 =
      x.disjoint_tryspec_and_lrange tryspec1 tryspec2.exnRange &&
      List.forall (fun c2 -> x.disjoint_tryspec_and_clause tryspec1 c2) tryspec2.exnClauses 
    
    member x.tryspec_disjoint_from_locspec tryspec1 locspec2 =
      x.disjoint_tryspec_and_lrange tryspec1 locspec2.locRange 
    
    member x.locspec_disjoint_from_locspec locspec1 locspec2 =
      x.disjoint_lranges locspec1.locRange locspec2.locRange 
    
    member x.locspec_inside_locspec locspec1 locspec2 =
      x.lrange_inside_lrange locspec1.locRange locspec2.locRange 
    
    member x.structspec_inside_structspec specA specB = (* only for sanity checks, then can be removed *)
        match specA,specB with
          | SEH   tryspecA,SEH   tryspecB -> x.tryspec_inside_tryspec tryspecA tryspecB
          | SEH   tryspecA,LOCAL locspecB -> x.tryspec_inside_locspec tryspecA locspecB
          | LOCAL locspecA,SEH   tryspecB -> x.locspec_inside_tryspec locspecA tryspecB
          | LOCAL locspecA,LOCAL locspecB -> x.locspec_inside_locspec locspecA locspecB
    
    // extent (or size) is the sum of range extents 
    // We want to build in increasing containment-order, that's a partial order. 
    // Size-order implies containment-order, and size-order is a total order. 
    member x.extent_structspec ss =  
        let extent_range (start_pc,end_pc) = end_pc - start_pc 
        let extent_lrange lrange = extent_range (x.labelsToRange lrange)  
        let extent_locspec locspec = extent_lrange locspec.locRange 
        let extent_list  extent_item items = List.fold (fun acc item -> acc + extent_item item) 0 items 
        let extent_list2 extent_item items = List.fold (fun acc item -> acc + extent_item item) 0 items 
        let extent_clause cl = extent_list extent_lrange (lranges_of_clause cl) 
        let extent_tryspec tryspec = extent_lrange tryspec.exnRange + (extent_list2 extent_clause tryspec.exnClauses) 
        
        match ss with 
        | LOCAL locspec -> extent_locspec locspec 
        | SEH tryspec -> extent_tryspec tryspec 

    (* DIAGNOSTICS: START ------------------------------ *)
    member x.string_of_structspec ss = 
        let stringOfRange (l1,l2) = 
          let pc1,pc2 = x.labelsToRange ((l1,l2))
          formatCodeLabel l1+"("+string pc1+")-"+ formatCodeLabel l2+"("+string pc2+")" 
        let string_of_clause cl = String.concat "+" (List.map stringOfRange (lranges_of_clause cl)) 
        let string_of_tryspec tryspec = "tryspec"+ stringOfRange tryspec.exnRange + "--" + String.concat " / " (List.map string_of_clause tryspec.exnClauses) 
        let string_of_locspec locspec = "local "+(String.concat ";" (locspec.locInfos |> List.map (fun l -> l.LocalName)))+": "+ stringOfRange locspec.locRange 
        match ss with 
        | SEH tryspec -> string_of_tryspec tryspec 
        | LOCAL locspec -> string_of_locspec locspec 
            


// Stage 2b - Given an innermost tryspec, collect together the 
// blocks covered by it. Preserve the essential ordering of blocks. 
let blockForInnerTrySpec (codeOffsetView:CodeOffsetViewOfLabelledItems,
                          coverageOfCodes,
                          addBlocks,
                          computeCoveredBlocks,
                          bbstartToCodeLabelMap:BasicBlockStartsToCodeLabelsMap) tryspec state0 = 

    let (blocks, remainingBasicBlockStarts) = state0
    let tryBlocks, otherBlocks = computeCoveredBlocks (codeOffsetView.labelsToRange tryspec.exnRange) blocks
    if isNil tryBlocks then (dprintn "try block specification covers no real code"; state0) else
    let getClause r otherBlocks = 
        let clauseBlocks, otherBlocks = computeCoveredBlocks (codeOffsetView.labelsToRange r) otherBlocks
        if isNil clauseBlocks then 
          failwith "clause block specification covers no real code";
        (* The next line computes the code label for the entry to the clause *)
        let clauseEntryLabel = bbstartToCodeLabelMap.lab2cl (fst r)
        // Now compute the overall clause, with labels still visible. 
        let clauseBlock = mkGroupBlock ([],List.map snd clauseBlocks)
        (* if logging then dprintf "-- clause entry label is %s" clauseEntryLabel; *)
        (clauseEntryLabel, clauseBlocks, clauseBlock), otherBlocks
    let tryCodeBlocks = List.map snd tryBlocks
    let tryEntryLabel = bbstartToCodeLabelMap.lab2cl (fst tryspec.exnRange)
    let tryHiddn = CodeLabels.remove tryEntryLabel (List.foldBack (entriesOfCodeAsSet >> CodeLabels.union) tryCodeBlocks CodeLabels.empty) 
    let tryBlock =  mkGroupBlock (CodeLabels.toList tryHiddn,tryCodeBlocks)

    match tryspec.exnClauses with 
    |  Finally _ :: _ :: _ -> failwith "finally clause combined with others"
    | [ Finally r ] | [ Fault r ] -> 

        let maker =       
          match tryspec.exnClauses with
            [ Finally _ ] -> mk_try_finally_block 
          | [ Fault _ ] -> mk_try_fault_block 
          | _ -> failwith ""

        let (clauseEntryLabel, clauseBlocks, clauseBlock), otherBlocks = getClause r otherBlocks
        let newBlockRange = coverageOfCodes (tryBlocks@clauseBlocks)
        // The next construction joins the blocks together. 
        // It automatically hides any internal labels used in the 
        // clause blocks. Only the entry to the clause is kept visible. 
        // We hide the entries to the try block up above. 
        let newBlock =  maker (tryBlock,clauseEntryLabel,clauseBlock)
        // None of the entries to the clause block are visible outside the 
        // entire try-clause construct, nor the other entries to the try block 
        // apart from the one at the. top 
        let newStarts = CodeLabels.diff remainingBasicBlockStarts (CodeLabels.union tryHiddn (entriesOfCodeAsSet clauseBlock))
        // Now return the new block, the remaining blocks and the new set 
        // of entries. 
        addBlocks otherBlocks [(newBlockRange, newBlock)], newStarts

    | clauses when clauses |> List.forall (function | FilterCatch _ -> true | TypeCatch _ -> true | _ -> false) -> 
          
          let clause_infos, otherBlocks (*(prior,posterior)*) = 
            List.fold 
              (fun (sofar,otherBlocks) cl -> 
                match cl with 
                | FilterCatch(r1,r2) -> 
                    let ((lab1,_,bl1) as _info1),otherBlocks =  getClause r1 otherBlocks
                    let info2,otherBlocks =  getClause r2 otherBlocks
                    (sofar@[(Choice1Of2 (lab1,bl1),info2)]), otherBlocks
                | TypeCatch(typ,r2) -> 
                    let info2,otherBlocks = getClause r2 otherBlocks
                    (sofar@[(Choice2Of2 typ,info2)]), otherBlocks
                | _ -> failwith "internal error")
              ([],otherBlocks)
              clauses
          let newBlockRange = 
            // Ignore filter blocks when computing this range 
            coverageOfCodes 
              (tryBlocks@
               ((List.collect (fun (_,(_,blocks2,_)) -> blocks2) clause_infos)))
          
          // The next construction joins the blocks together. 
          // It automatically hides any internal labels used in the 
          // clause blocks. Only the entry to the clause is kept visible. 
          let newBlock = 
            mk_try_multi_filter_catch_block 
              (tryBlock,
               List.map 
                 (fun (choice,(lab2,_,bl2)) -> choice, (lab2,bl2)) 
                 clause_infos)
          // None of the entries to the filter or catch blocks are 
          // visible outside the entire exception construct. 
          let newStarts =
            CodeLabels.diff remainingBasicBlockStarts 
              (CodeLabels.union tryHiddn
                 (List.foldBack 
                    (fun (flt,(_,_,ctch_blck)) acc -> 
                      CodeLabels.union
                        (match flt with 
                         | Choice1Of2 (_,flt_block) -> entriesOfCodeAsSet flt_block
                         | Choice2Of2 _ -> CodeLabels.empty)
                        (CodeLabels.union (entriesOfCodeAsSet ctch_blck) acc)) 
                    clause_infos
                    CodeLabels.empty))
          // Now return the new block, the remaining blocks and the new set 
          // of entries. 
          addBlocks otherBlocks [ (newBlockRange, newBlock)], newStarts
    | _ -> failwith "invalid pattern of exception constructs" 



let doStructure' (codeOffsetView:CodeOffsetViewOfLabelledItems,
                   computeCoveredBlocks,
                   coverageOfCodes,
                   addBlocks,
                   bbstartToCodeLabelMap:BasicBlockStartsToCodeLabelsMap)
                 structspecs 
                 blockState =

    (* Stage 2b - Given an innermost tryspec, collect together the *)
    (* blocks covered by it. Preserve the essential ordering of blocks. *)
    let blockForInnerLocSpec locspec ((blocks, remainingBasicBlockStarts) as state0) =
        let scope_blocks, otherBlocks (*(prior,posterior)*) = computeCoveredBlocks (codeOffsetView.labelsToRange locspec.locRange) blocks
        if isNil scope_blocks then (dprintn "scope block specification covers no real code"; state0) else
        let newBlock =  mk_scope_block (locspec.locInfos,mkGroupBlock ([],List.map snd scope_blocks))
        let newBlockRange = coverageOfCodes scope_blocks
        addBlocks otherBlocks [ (newBlockRange, newBlock)], remainingBasicBlockStarts

    // Require items by increasing inclusion-order.
    // Order by size/extent.
    // a) size-ordering implies containment-ordering.
    // b) size-ordering is total, so works with List.sort
    let buildOrder = Order.orderOn codeOffsetView.extent_structspec int_order

    (* checkOrder: checking is O(n^2) *)
(*
    let rec checkOrder = function
      | []      -> ()
      | sA::sBs -> List.iter (fun sB ->
                                if codeOffsetView.structspec_inside_structspec sB sA && not (codeOffsetView.structspec_inside_structspec sA sB) then (
                                  dprintf "sA = %s\n" (codeOffsetView.string_of_structspec sA);
                                  dprintf "sB = %s\n" (codeOffsetView.string_of_structspec sB);
                                  assert false
                                )) sBs;
                   checkOrder sBs
*)

    let structspecs = List.sortWithOrder buildOrder structspecs

    (* if sanity_check_order then checkOrder structspecs; *) (* note: this check is n^2 *)
    let buildBlock blockState = function
      | SEH   tryspec ->  blockForInnerTrySpec (codeOffsetView,coverageOfCodes,addBlocks,computeCoveredBlocks,bbstartToCodeLabelMap) tryspec blockState
      | LOCAL locspec ->  blockForInnerLocSpec locspec blockState
    List.fold buildBlock blockState structspecs 

            
// This function shows up on performance traces. If we eliminated the last ILX->IL rewrites from the
// F# compiler we could get rid of this structured code representation from Abstract IL altogether, and 
// never convert F# code into this form.
let buildILCode methName lab2pc instrs tryspecs localspecs =

    let bbstartToCodeLabelMap = BasicBlockStartsToCodeLabelsMap(instrs,tryspecs,localspecs,lab2pc)
    let codeOffsetView = CodeOffsetViewOfLabelledItems(lab2pc)

    let basicInstructions = Array.map bbstartToCodeLabelMap.remapLabels instrs
    
    (* DIAGNOSTICS: END -------------------------------- *)

    let buildCodeFromInstructionArray instrs =

        // Consume instructions until we hit the end of the basic block, either 
        // by hitting a control-flow instruction or by hitting the start of the 
        // next basic block by fall-through. 
        let rec consumeBBlockInstrs instrs rinstrs (pc:int) nextBBstartPc =
          (* rinstrs = accumulates instructions in reverse order *)
          if pc = (Array.length instrs) then 
              dprintn "* WARNING: basic block at end of method ends without a leave, branch, return or throw. Adding throw\n";
              pc,List.rev (I_throw :: rinstrs)
          // The next test is for drop-through at end of bblock, when we just insert 
          // a branch to the next bblock. 
          elif (match nextBBstartPc with Some pc' -> pc = pc' | _ -> false) then 
              if logging then dprintf "-- pushing br, pc = nextBBstartPc = %d\n" pc;
              pc,List.rev (I_br (bbstartToCodeLabelMap.pc2cl pc) :: rinstrs)
          else
              // Otherwise bblocks end with control-flow. 
              let i = instrs.[pc]
              let pc' = pc + 1
              if instrIsBascBlockEnd i then 
                  if instrIsTailcall i then 
                      if pc' = instrs.Length || (match instrs.[pc'] with I_ret -> false | _ -> true) then 
                          failwithf "a tailcall must be followed by a return, instrs = %A" instrs
                      elif (match nextBBstartPc with Some pc'' -> pc' = pc'' | _ -> false) then
                          // In this obscure case, someone branches to the return instruction 
                          // following the tailcall, so we'd better build a basic block 
                          // containing just that return instruction. 
                          pc', List.rev (i :: rinstrs)
                      else 
                          // Otherwise skip the return instruction, but keep the tailcall. 
                          pc'+1, List.rev (i :: rinstrs)
                  else 
                    pc', List.rev (i :: rinstrs)
              else
                  // recursive case 
                  consumeBBlockInstrs instrs (i::rinstrs) pc' nextBBstartPc

        (* type block = (int * int) * Code // a local type (alias) would be good, good for intelisense too *)
        let rec consumeOneBlock bbstartPc nextBBstartPc currentPc =
          if currentPc = (Array.length instrs) then None
          elif bbstartPc < currentPc then failwith "internal error: bad basic block structure (missing bblock start marker?)"
          elif bbstartPc > currentPc then
            (* dprintn ("* ignoring unreachable instruction in method: "^ methName); *)
              consumeOneBlock bbstartPc nextBBstartPc (currentPc + 1)
          else
              let pc', bblockInstrs = consumeBBlockInstrs instrs [] bbstartPc nextBBstartPc
              if logging then dprintf "-- making bblock, entry label is %s, length = %d, bbstartPc = %d\n" (formatCodeLabel (bbstartToCodeLabelMap.pc2cl bbstartPc)) (List.length bblockInstrs) bbstartPc;
              let bblock = mkBasicBlock {Label= bbstartToCodeLabelMap.pc2cl bbstartPc; Instructions=Array.ofList bblockInstrs}
              
              let bblockRange = (bbstartPc, pc')
              // Return the bblock and the range of instructions that the bblock covered. 
              // Also return any remaining instructions and the pc' for the first 
              // such instruction. 
              Some ((bblockRange, bblock), pc')

        let rec fetchBasicBlocks bbstartToCodeLabelMap currentPc = 
            match bbstartToCodeLabelMap with 
            | [] -> 
                (* if currentPc <> Array.length instrs then 
                   dprintn ("* ignoring instructions at end of method: "+ methName); *)
                []
            | h::t -> 
                let h2 = match t with [] -> None | h2:: _ -> assert (not (h = h2)); Some h2
                match consumeOneBlock h h2 currentPc with
                | None -> []
                | Some (bblock, currentPc') -> bblock :: fetchBasicBlocks t currentPc'

        let inside range (brange,_) =
            if rangeInsideRange brange range then true else
            if disjoint_range brange range then false else
            failwith "exception block specification overlaps the range of a basic block"

        (* A "blocks" contain blocks, ordered on startPC.
         * Recall, a block is (range,code) where range=(pcStart,pcLast+1). *)
        let addBlock m (((startPC,_endPC),_code) as block) =
            match Zmap.tryFind startPC m with
            | None        -> Zmap.add startPC [block] m
            | Some blocks -> Zmap.add startPC (block :: blocks) m in  (* NOTE: may reverse block *)

        let addBlocks m blocks = List.fold addBlock m blocks
              
        let mkBlocks blocks =
            let emptyBlocks = (Zmap.empty int_order :  Zmap<int,((int*int) * ILCode) list>)
            List.fold addBlock emptyBlocks blocks

        let sanityCheck = false (* linear check  *)

        let computeCoveredBlocks ((start_pc,end_pc) as range) (blocks: Zmap<int,((int*int) * ILCode) list> ) =
            // It is assumed that scopes never overlap.
            // locinfo scopes could overlap if there is a bug elsewhere.
            // If overlaps are discovered, an exception is raised. see NOTE#overlap.
            let pcCovered,blocks = popRangeM start_pc (end_pc - 1) blocks
            let coveredBlocks = pcCovered |> List.concat
            // Look for bad input, e.g. overlapping locinfo scopes. 
            let overlapBlocks = List.filter (inside range >> not) coveredBlocks
            if not (isNil overlapBlocks) then notFound(); (* see NOTE#overlap *)
            if sanityCheck then (
              let assertIn  block = assert (inside range block)
              let assertOut block = assert (not (inside range block))
              List.iter assertIn coveredBlocks;
              Zmap.iter (fun _ bs -> List.iter assertOut bs) blocks
            );
            coveredBlocks,blocks

        let rec coverageOfCodes blocks = 
            match blocks with 
            | [] -> failwith "start_of_codes"
            | [(r,_)] -> r 
            | ((r,_)::t) -> merge_ranges r (coverageOfCodes t)
        
        delayInsertedToWorkaroundKnownNgenBug "Delay4i3" <| fun () ->

        let doStructure = doStructure' (codeOffsetView, computeCoveredBlocks,coverageOfCodes,addBlocks,bbstartToCodeLabelMap)
        
        (* Apply stage 1. Compute the blocks not taking exceptions into account. *)
        let bblocks = 
            fetchBasicBlocks (List.sort bbstartToCodeLabelMap.BasicBlockStartPositions) 0

        let bblocks = mkBlocks bblocks
        (* Apply stage 2. Compute the overall morphed blocks. *)
        let morphed_blocks,remaining_entries = 
            let specs1 = List.map (fun x -> SEH x) tryspecs
            let specs2 = List.map (fun x -> LOCAL x) localspecs

            try 
                doStructure (specs1 @ specs2) (bblocks,bbstartToCodeLabelMap.BasicBlockStartCodeLabels) 
            with :? KeyNotFoundException->
                // NOTE#overlap.
                // Here, "Not_found" indicates overlapping scopes were found.
                // Maybe the calling code got the locspecs scopes wrong.
                // Try recovery by discarding locspec info...
                let string_of_tryspec _tryspec = "tryspec"
                let stringOfRange (l1,l2) = 
                  let pc1,pc2 = codeOffsetView.labelsToRange ((l1,l2))
                  formatCodeLabel l1+"("+string pc1+")-"+ formatCodeLabel l2+"("+string pc2+")"
                let string_of_locspec locspec = "local "+(String.concat ";" (locspec.locInfos |> List.map (fun l -> l.LocalName)))+": "+ stringOfRange locspec.locRange
                
                dprintf "\nERROR: could not find an innermost exception block or local scope, specs = \n%s\nTrying again without locals."
                  (String.concat "\n" (List.map string_of_tryspec tryspecs @ List.map string_of_locspec localspecs));
                doStructure specs1 (bblocks,bbstartToCodeLabelMap.BasicBlockStartCodeLabels) 

        delayInsertedToWorkaroundKnownNgenBug "Delay4k" <| fun () ->

        let morphed_blocks = Zmap.values morphed_blocks |> List.concat in (* NOTE: may mixup order *)
        (* Now join up all the remaining blocks into one block with one entry. *)
        if logging then dprintn "-- computing entry label";
        if logging then dprintn ("-- entry label is "+formatCodeLabel bbstartToCodeLabelMap.InitialCodeLabel);
        mkGroupBlock 
          (CodeLabels.toList (CodeLabels.remove bbstartToCodeLabelMap.InitialCodeLabel remaining_entries),List.map snd morphed_blocks)


    try buildCodeFromInstructionArray basicInstructions
    with e -> 
      dprintn ("* error while converting instructions to code for method: " + methName);
      reraise()

// -------------------------------------------------------------------- 
// Detecting Delegates
// -------------------------------------------------------------------- 

let mkILDelegateMethods ilg (parms,rtv:ILReturn) = 
    let rty = rtv.Type
    let one nm args ret =
        let mdef = mkILNonGenericVirtualMethod (nm,ILMemberAccess.Public,args,mkILReturn ret,MethodBody.Abstract)
        {mdef with 
                   mdKind=
                      match mdef.mdKind with 
                      | MethodKind.Virtual vinfo -> MethodKind.Virtual {vinfo with IsAbstract=false; } 
                      | k -> k 
                   mdCodeKind=MethodCodeKind.Runtime;
                   IsHideBySig=true; }
    let ctor = mkILCtor(ILMemberAccess.Public, [ mkILParamNamed("object",ilg.typ_Object); mkILParamNamed("method",ilg.typ_IntPtr) ], MethodBody.Abstract)
    let ctor = { ctor with  mdCodeKind=MethodCodeKind.Runtime; IsHideBySig=true }
    [ ctor;
      one "Invoke" parms rty;
      one "BeginInvoke" (parms @ [mkILParamNamed("callback",ilg.typ_AsyncCallback); mkILParamNamed("objects",ilg.typ_Object) ] ) ilg.typ_IAsyncResult;
      one "EndInvoke" [mkILParamNamed("result",ilg.typ_IAsyncResult)] rty; ]
    

let mkCtorMethSpecForDelegate ilg (typ:ILType,useUIntPtr) =
    let scoref = typ.TypeRef.Scope 
    mkILInstanceMethSpecInTy (typ,".ctor",[rescopeILType scoref ilg.typ_Object; rescopeILType scoref (if useUIntPtr then ilg.typ_UIntPtr else ilg.typ_IntPtr)],ILType.Void,emptyILGenericArgs)

type ILEnumInfo =
    { enumValues: (string * ILFieldInit) list;  
      enumType: ILType }

let getTyOfILEnumInfo info = info.enumType

let computeILEnumInfo (mdName,mdFields: ILFieldDefs) = 
    match (List.partition (fun fd -> fd.IsStatic) mdFields.AsList) with 
    | staticFields,[vfd] -> 
        { enumType = vfd.Type; 
          enumValues = staticFields |> List.map (fun fd -> (fd.Name, match fd.LiteralValue with Some i -> i | None -> failwith ("info_of_enum_tdef: badly formed enum "+mdName+": static field does not have an default value")))  }
    | _,[] -> failwith ("info_of_enum_tdef: badly formed enum "+mdName+": no non-static field found")
    | _,_ -> failwith ("info_of_enum_tdef: badly formed enum "+mdName+": more than one non-static field found")

 

//---------------------------------------------------------------------
// Primitives to help read signatures.  These do not use the file cursor, but
// pass around an int index
//---------------------------------------------------------------------

let sigptr_get_byte bytes sigptr = 
    Bytes.get bytes sigptr, sigptr + 1

let sigptr_get_bool bytes sigptr = 
    let b0,sigptr = sigptr_get_byte bytes sigptr
    (b0 = 0x01) ,sigptr

let sigptr_get_u8 bytes sigptr = 
    let b0,sigptr = sigptr_get_byte bytes sigptr
    byte b0,sigptr

let sigptr_get_i8 bytes sigptr = 
    let i,sigptr = sigptr_get_u8 bytes sigptr
    sbyte i,sigptr

let sigptr_get_u16 bytes sigptr = 
    let b0,sigptr = sigptr_get_byte bytes sigptr
    let b1,sigptr = sigptr_get_byte bytes sigptr
    uint16 (b0 ||| (b1 <<< 8)),sigptr

let sigptr_get_i16 bytes sigptr = 
    let u,sigptr = sigptr_get_u16 bytes sigptr
    int16 u,sigptr

let sigptr_get_i32 bytes sigptr = 
    let b0,sigptr = sigptr_get_byte bytes sigptr
    let b1,sigptr = sigptr_get_byte bytes sigptr
    let b2,sigptr = sigptr_get_byte bytes sigptr
    let b3,sigptr = sigptr_get_byte bytes sigptr
    b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24),sigptr

let sigptr_get_u32 bytes sigptr = 
    let u,sigptr = sigptr_get_i32 bytes sigptr
    uint32 u,sigptr

let sigptr_get_i64 bytes sigptr = 
    let b0,sigptr = sigptr_get_byte bytes sigptr
    let b1,sigptr = sigptr_get_byte bytes sigptr
    let b2,sigptr = sigptr_get_byte bytes sigptr
    let b3,sigptr = sigptr_get_byte bytes sigptr
    let b4,sigptr = sigptr_get_byte bytes sigptr
    let b5,sigptr = sigptr_get_byte bytes sigptr
    let b6,sigptr = sigptr_get_byte bytes sigptr
    let b7,sigptr = sigptr_get_byte bytes sigptr
    int64 b0 ||| (int64 b1 <<< 8) ||| (int64 b2 <<< 16) ||| (int64 b3 <<< 24) |||
    (int64 b4 <<< 32) ||| (int64 b5 <<< 40) ||| (int64 b6 <<< 48) ||| (int64 b7 <<< 56),
    sigptr

let sigptr_get_u64 bytes sigptr = 
    let u,sigptr = sigptr_get_i64 bytes sigptr
    uint64 u,sigptr

let float32_of_bits (x:int32) = System.BitConverter.ToSingle(System.BitConverter.GetBytes(x),0)
let float_of_bits (x:int64) = System.BitConverter.Int64BitsToDouble(x)

let sigptr_get_ieee32 bytes sigptr = 
    let u,sigptr = sigptr_get_i32 bytes sigptr
    float32_of_bits u,sigptr

let sigptr_get_ieee64 bytes sigptr = 
    let u,sigptr = sigptr_get_i64 bytes sigptr
    float_of_bits u,sigptr

let sigptr_get_intarray n (bytes:byte[]) sigptr = 
    let res = Bytes.zeroCreate n
    for i = 0 to n - 1 do 
        res.[i] <- bytes.[sigptr + i]
    res, sigptr + n

let sigptr_get_string n bytes sigptr = 
    let intarray,sigptr = sigptr_get_intarray n bytes sigptr
#if SILVERLIGHT
    System.Text.Encoding.UTF8.GetString(intarray , 0, intarray.Length), sigptr
#else    
    System.Text.Encoding.UTF8.GetString intarray , sigptr
#endif    
   
let sigptr_get_z_i32 bytes sigptr = 
    let b0,sigptr = sigptr_get_byte bytes sigptr
    if b0 <= 0x7F then b0, sigptr
    elif b0 <= 0xbf then 
        let b0 = b0 &&& 0x7f
        let b1,sigptr = sigptr_get_byte bytes sigptr
        (b0 <<< 8) ||| b1, sigptr
    else 
        let b0 = b0 &&& 0x3f
        let b1,sigptr = sigptr_get_byte bytes sigptr
        let b2,sigptr = sigptr_get_byte bytes sigptr
        let b3,sigptr = sigptr_get_byte bytes sigptr
        (b0 <<< 24) ||| (b1 <<< 16) ||| (b2 <<< 8) ||| b3, sigptr

let sigptr_get_serstring  bytes sigptr = 
    let len,sigptr = sigptr_get_z_i32 bytes sigptr 
    sigptr_get_string ( len) bytes sigptr 
  
let sigptr_get_serstring_possibly_null  bytes sigptr = 
    let b0,_ = sigptr_get_byte bytes sigptr   (* throw away sigptr *)
    if b0 = 0xFF then
        None,sigptr
    else  
        let len,sigptr = sigptr_get_z_i32 bytes sigptr 
        let s, sigptr = sigptr_get_string len bytes sigptr
        Some(s),sigptr
  
//---------------------------------------------------------------------
// Get the public key token from the public key.
//---------------------------------------------------------------------


let mkRefToILAssembly (m: ILAssemblyManifest) = 
    ILAssemblyRef.Create(m.Name, None, (match m.PublicKey with Some k -> Some (PublicKey.KeyAsToken(k)) | None -> None), false, m.Version, m.Locale)

let z_unsigned_int_size n = 
    if n <= 0x7F then 1
    elif n <= 0x3FFF then 2
    else 3

let z_unsigned_int n = 
    if n >= 0 &&  n <= 0x7F then [| byte n |] 
    elif n >= 0x80 && n <= 0x3FFF then [| byte (0x80 ||| (n >>>& 8)); byte (n &&& 0xFF) |] 
    else [| byte (0xc0 ||| (n >>>& 24)); 
            byte ((n >>>& 16) &&& 0xFF); 
            byte ((n >>>& 8) &&& 0xFF); 
            byte (n &&& 0xFF) |]

let string_as_utf8_bytes (s:string) = System.Text.Encoding.UTF8.GetBytes s

(* Little-endian encoding of int64 *)
let dw7 n = byte ((n >>> 56) &&& 0xFFL)
let dw6 n = byte ((n >>> 48) &&& 0xFFL)
let dw5 n = byte ((n >>> 40) &&& 0xFFL)
let dw4 n = byte ((n >>> 32) &&& 0xFFL)
let dw3 n = byte ((n >>> 24) &&& 0xFFL)
let dw2 n = byte ((n >>> 16) &&& 0xFFL)
let dw1 n = byte ((n >>> 8)  &&& 0xFFL)
let dw0 n = byte (n          &&& 0xFFL)

let u8AsBytes (i:byte) = [| i |]
let u16AsBytes x =  let n = (int x) in [| byte (b0 n); byte (b1 n) |]
let i32AsBytes i = [| byte (b0 i); byte (b1 i); byte (b2 i); byte (b3 i) |]
let i64AsBytes i = [| dw0 i; dw1 i; dw2 i; dw3 i; dw4 i; dw5 i; dw6 i; dw7 i |]

let i8AsBytes (i:sbyte) = u8AsBytes (byte i)
let i16AsBytes (i:int16) = u16AsBytes (uint16 i)
let u32AsBytes (i:uint32) = i32AsBytes (int32 i)
let u64AsBytes (i:uint64) = i64AsBytes (int64 i)

let bits_of_float32 (x:float32) = System.BitConverter.ToInt32(System.BitConverter.GetBytes(x),0)
let bits_of_float (x:float) = System.BitConverter.DoubleToInt64Bits(x)

let ieee32AsBytes i = i32AsBytes (bits_of_float32 i)
let ieee64AsBytes i = i64AsBytes (bits_of_float i)

let et_END = 0x00uy
let et_VOID = 0x01uy
let et_BOOLEAN = 0x02uy
let et_CHAR = 0x03uy
let et_I1 = 0x04uy
let et_U1 = 0x05uy
let et_I2 = 0x06uy
let et_U2 = 0x07uy
let et_I4 = 0x08uy
let et_U4 = 0x09uy
let et_I8 = 0x0Auy
let et_U8 = 0x0Buy
let et_R4 = 0x0Cuy
let et_R8 = 0x0Duy
let et_STRING = 0x0Euy
let et_PTR = 0x0Fuy
let et_BYREF = 0x10uy
let et_VALUETYPE      = 0x11uy
let et_CLASS          = 0x12uy
let et_VAR            = 0x13uy
let et_ARRAY          = 0x14uy
let et_WITH           = 0x15uy
let et_TYPEDBYREF     = 0x16uy
let et_I              = 0x18uy
let et_U              = 0x19uy
let et_FNPTR          = 0x1Buy
let et_OBJECT         = 0x1Cuy
let et_SZARRAY        = 0x1Duy
let et_MVAR           = 0x1Euy
let et_CMOD_REQD      = 0x1Fuy
let et_CMOD_OPT       = 0x20uy

let formatILVersion ((a,b,c,d):ILVersionInfo) = sprintf "%d.%d.%d.%d" (int a) (int b) (int c) (int d)

let encodeCustomAttrString s = 
    let arr = string_as_utf8_bytes s
    Array.concat [ z_unsigned_int arr.Length; arr ]      

let rec encodeCustomAttrElemType x = 
    match x with
    | ILType.Value tspec when tspec.Name = "System.SByte" ->  [| et_I1 |]
    | ILType.Value tspec when tspec.Name = "System.Byte" ->  [| et_U1 |]
    | ILType.Value tspec when tspec.Name = "System.Int16" ->  [| et_I2 |]
    | ILType.Value tspec when tspec.Name = "System.UInt16" ->  [| et_U2 |]
    | ILType.Value tspec when tspec.Name = "System.Int32" ->  [| et_I4 |]
    | ILType.Value tspec when tspec.Name = "System.UInt32" ->  [| et_U4 |]
    | ILType.Value tspec when tspec.Name = "System.Int64" ->  [| et_I8 |]
    | ILType.Value tspec when tspec.Name = "System.UInt64" ->  [| et_U8 |]
    | ILType.Value tspec when tspec.Name = "System.Double" ->  [| et_R8 |]
    | ILType.Value tspec when tspec.Name = "System.Single" ->  [| et_R4 |]
    | ILType.Value tspec when tspec.Name = "System.Char" ->  [| et_CHAR |]
    | ILType.Value tspec when tspec.Name = "System.Boolean" ->  [| et_BOOLEAN |]
    | ILType.Boxed tspec when tspec.Name = "System.String" ->  [| et_STRING |]
    | ILType.Boxed tspec when tspec.Name = "System.Object" ->  [| 0x51uy |] 
    | ILType.Boxed tspec when tspec.Name = "System.Type" ->  [| 0x50uy |]
    | ILType.Value tspec ->  Array.append [| 0x55uy |] (encodeCustomAttrString tspec.TypeRef.QualifiedNameWithNoShortMscorlib)
    | ILType.Array (shape, elemType) when shape = ILArrayShape.SingleDimensional -> 
          Array.append [| et_SZARRAY |] (encodeCustomAttrElemType elemType)
    | _ ->  failwith "encodeCustomAttrElemType: unrecognized custom element type"

/// Given a custom attribute element, work out the type of the .NET argument for that element
let rec encodeCustomAttrElemTypeForObject x = 
    match x with
    | ILAttribElem.String _  -> [| et_STRING |]
    | ILAttribElem.Bool _    -> [| et_BOOLEAN |]
    | ILAttribElem.Char _    -> [| et_CHAR |]
    | ILAttribElem.SByte _   -> [| et_I1 |]
    | ILAttribElem.Int16 _   -> [| et_I2 |]
    | ILAttribElem.Int32 _   -> [| et_I4 |]
    | ILAttribElem.Int64 _   -> [| et_I8 |]
    | ILAttribElem.Byte _    -> [| et_U1 |]
    | ILAttribElem.UInt16 _  -> [| et_U2 |]
    | ILAttribElem.UInt32 _  -> [| et_U4 |]
    | ILAttribElem.UInt64 _  -> [| et_U8 |]
    | ILAttribElem.Type _    -> [| 0x50uy |]
    | ILAttribElem.TypeRef _ -> [| 0x50uy |]
    | ILAttribElem.Null _    -> [| et_STRING  |]// yes, the 0xe prefix is used when passing a "null" to a property or argument of type "object" here
    | ILAttribElem.Single _  -> [| et_R4 |]
    | ILAttribElem.Double _  -> [| et_R8 |]
    | ILAttribElem.Array (elemTy,_) -> [| yield et_SZARRAY; yield! encodeCustomAttrElemType elemTy |]


let rec decodeCustomAttrElemType ilg bytes sigptr x = 
    match x with
    | x when x =  et_I1 -> ilg.typ_SByte, sigptr
    | x when x = et_U1 -> ilg.typ_Byte, sigptr
    | x when x =  et_I2 -> ilg.typ_Int16, sigptr
    | x when x =  et_U2 -> ilg.typ_UInt16, sigptr
    | x when x =  et_I4 -> ilg.typ_Int32, sigptr
    | x when x =  et_U4 -> ilg.typ_UInt32, sigptr
    | x when x =  et_I8 -> ilg.typ_Int64, sigptr
    | x when x =  et_U8 -> ilg.typ_UInt64, sigptr
    | x when x =  et_R8 -> ilg.typ_Double, sigptr
    | x when x =  et_R4 -> ilg.typ_Single, sigptr
    | x when x = et_CHAR -> ilg.typ_Char, sigptr
    | x when x =  et_BOOLEAN -> ilg.typ_Bool, sigptr
    | x when x =  et_STRING -> ilg.typ_String, sigptr
    | x when x =  et_OBJECT -> ilg.typ_Object, sigptr
    | x when x =  et_SZARRAY -> 
         let et,sigptr = sigptr_get_u8 bytes sigptr 
         let elemTy,sigptr = decodeCustomAttrElemType ilg bytes sigptr et
         mkILArr1DTy elemTy, sigptr
    | x when x = 0x50uy -> ilg.typ_Type, sigptr
    | _ ->  failwithf "decodeCustomAttrElemType ilg: unrecognized custom element type: %A" x


/// Given a custom attribute element, encode it to a binary representation according to the rules in Ecma 335 Partition II.
let rec encodeCustomAttrPrimValue ilg c = 
    match c with 
    | ILAttribElem.Bool b -> [| (if b then 0x01uy else 0x00uy) |]
    | ILAttribElem.String None 
    | ILAttribElem.Type None 
    | ILAttribElem.TypeRef None
    | ILAttribElem.Null -> [| 0xFFuy |]
    | ILAttribElem.String (Some s) -> encodeCustomAttrString s
    | ILAttribElem.Char x -> u16AsBytes (uint16 x)
    | ILAttribElem.SByte x -> i8AsBytes x
    | ILAttribElem.Int16 x -> i16AsBytes x
    | ILAttribElem.Int32 x -> i32AsBytes x
    | ILAttribElem.Int64 x -> i64AsBytes x
    | ILAttribElem.Byte x -> u8AsBytes x
    | ILAttribElem.UInt16 x -> u16AsBytes x
    | ILAttribElem.UInt32 x -> u32AsBytes x
    | ILAttribElem.UInt64 x -> u64AsBytes x
    | ILAttribElem.Single x -> ieee32AsBytes x
    | ILAttribElem.Double x -> ieee64AsBytes x
    | ILAttribElem.Type (Some ty) -> encodeCustomAttrString ty.QualifiedNameWithNoShortMscorlib 
    | ILAttribElem.TypeRef (Some tref) -> encodeCustomAttrString tref.QualifiedNameWithNoShortMscorlib 
    | ILAttribElem.Array (_,elems) ->  
         [| yield! i32AsBytes elems.Length; for elem in elems do yield! encodeCustomAttrPrimValue ilg elem |]

and encodeCustomAttrValue ilg ty c = 
    match ty, c with 
    | ILType.Boxed tspec, _ when tspec.Name = "System.Object" ->  
       [| yield! encodeCustomAttrElemTypeForObject c; yield! encodeCustomAttrPrimValue ilg c |]
    | ILType.Array (shape, elemType), ILAttribElem.Array (_,elems) when shape = ILArrayShape.SingleDimensional ->  
       [| yield! i32AsBytes elems.Length; for elem in elems do yield! encodeCustomAttrValue ilg elemType elem |]
    | _ -> 
       encodeCustomAttrPrimValue ilg c

let encodeCustomAttrNamedArg ilg (nm, ty, prop, elem) = 
   [| yield (if prop then 0x54uy else 0x53uy) 
      yield! encodeCustomAttrElemType ty;
      yield! encodeCustomAttrString nm;
      yield! encodeCustomAttrValue ilg ty elem |]

let mkILCustomAttribMethRef (ilg: ILGlobals) (mspec:ILMethodSpec, fixedArgs: list<_>, namedArgs: list<_>) = 
#if SILVERLIGHT
    { Method = mspec;
      Arguments = fixedArgs, namedArgs
      Data = [||] }
#else
    let argtys = mspec.MethodRef.ArgTypes
    let args = 
      [| yield! [| 0x01uy; 0x00uy; |]
         for (argty,fixedArg) in Seq.zip argtys fixedArgs do
            yield! encodeCustomAttrValue ilg argty fixedArg
         yield! u16AsBytes (uint16 namedArgs.Length) 
         for namedArg in namedArgs do 
             yield! encodeCustomAttrNamedArg ilg namedArg |]

    { Method = mspec;
      Data = args }
#endif

let mkILCustomAttribute ilg (tref,argtys,argvs,propvs) = 
    mkILCustomAttribMethRef ilg (mkILNonGenericCtorMethSpec (tref,argtys),argvs,propvs)

(* Q: CompilerGeneratedAttribute is new in 2.0. Unconditional generation of this attribute prevents running on 1.1 Framework. (discovered running on early mono version). *)
let tref_CompilerGeneratedAttribute   ilg = mkILTyRef (ilg.mscorlibScopeRef,tname_CompilerGeneratedAttribute)

let tname_DebuggerNonUserCodeAttribute = "System.Diagnostics.DebuggerNonUserCodeAttribute"
let tname_DebuggableAttribute_DebuggingModes = "DebuggingModes"
let tname_DebuggerHiddenAttribute = "System.Diagnostics.DebuggerHiddenAttribute"
let tname_DebuggerDisplayAttribute = "System.Diagnostics.DebuggerDisplayAttribute"
let tname_DebuggerTypeProxyAttribute = "System.Diagnostics.DebuggerTypeProxyAttribute"
let tname_DebuggerStepThroughAttribute = "System.Diagnostics.DebuggerStepThroughAttribute"
let tname_DebuggerBrowsableAttribute = "System.Diagnostics.DebuggerBrowsableAttribute"
let tname_DebuggerBrowsableState = "System.Diagnostics.DebuggerBrowsableState"

let tref_DebuggerNonUserCodeAttribute ilg = mkILTyRef (ilg.mscorlibScopeRef,tname_DebuggerNonUserCodeAttribute)
let tref_DebuggerStepThroughAttribute ilg = mkILTyRef (ilg.mscorlibScopeRef,tname_DebuggerStepThroughAttribute)
let tref_DebuggerHiddenAttribute ilg = mkILTyRef (ilg.mscorlibScopeRef,tname_DebuggerHiddenAttribute)
let tref_DebuggerDisplayAttribute ilg = mkILTyRef (ilg.mscorlibScopeRef,tname_DebuggerDisplayAttribute)
let tref_DebuggerTypeProxyAttribute ilg = mkILTyRef (ilg.mscorlibScopeRef,tname_DebuggerTypeProxyAttribute)
let tref_DebuggerBrowsableAttribute ilg = mkILTyRef (ilg.mscorlibScopeRef,tname_DebuggerBrowsableAttribute)
let tref_DebuggableAttribute          ilg = mkILTyRef (ilg.mscorlibScopeRef,tname_DebuggableAttribute)
let tref_DebuggableAttribute_DebuggingModes ilg = mkILNestedTyRef (ilg.mscorlibScopeRef,[tname_DebuggableAttribute],tname_DebuggableAttribute_DebuggingModes)

let typ_DebuggerBrowsableState ilg = 
    let tref_DebuggerBrowsableState = mkILTyRef(ilg.mscorlibScopeRef,tname_DebuggerBrowsableState)
    ILType.Value (mkILTySpec(tref_DebuggerBrowsableState,emptyILGenericArgs))

let mkCompilerGeneratedAttribute   ilg = mkILCustomAttribute ilg (tref_CompilerGeneratedAttribute ilg,[],[],[])
let mkDebuggerNonUserCodeAttribute ilg = mkILCustomAttribute ilg (tref_DebuggerNonUserCodeAttribute ilg,[],[],[])
let mkDebuggerHiddenAttribute ilg = mkILCustomAttribute ilg (tref_DebuggerHiddenAttribute ilg,[],[],[])
let mkDebuggerDisplayAttribute ilg s = mkILCustomAttribute ilg (tref_DebuggerDisplayAttribute ilg,[ilg.typ_String],[ILAttribElem.String (Some s)],[])
let mkDebuggerTypeProxyAttribute ilg (ty:ILType) = 
    match ilg.debuggerTypeProxyAttributeCache with 
    | None -> 
        let res = mkILCustomAttribute ilg (tref_DebuggerTypeProxyAttribute ilg,[ilg.typ_Type],[ILAttribElem.TypeRef (Some ty.TypeRef)],[])
        ilg.debuggerTypeProxyAttributeCache <- Some res
        res
    | Some res -> res

let mkDebuggerBrowsableAttribute ilg n = mkILCustomAttribute ilg (tref_DebuggerBrowsableAttribute ilg,[typ_DebuggerBrowsableState ilg],[ILAttribElem.Int32 n],[])
let mkDebuggerBrowsableNeverAttribute ilg = 
    match ilg.debuggerBrowsableNeverAttributeCache with 
    | None -> 
        let res = mkDebuggerBrowsableAttribute  ilg 0
        ilg.debuggerBrowsableNeverAttributeCache <- Some res
        res
    | Some res -> res
let mkDebuggerStepThroughAttribute ilg = mkILCustomAttribute ilg (tref_DebuggerStepThroughAttribute ilg,[],[],[])
let mkDebuggableAttribute ilg (jitTracking, jitOptimizerDisabled) = 
    mkILCustomAttribute ilg (tref_DebuggableAttribute ilg,[ilg.typ_Bool;ilg.typ_Bool], [ILAttribElem.Bool jitTracking; ILAttribElem.Bool jitOptimizerDisabled],[])


// Bug 2129. Requests attributes to be added to compiler generated methods 
let add_generated_attrs ilg (attrs: ILAttributes) = 
    let attribs = 
       match ilg.generatedAttribsCache with 
       | [] -> 
           let res = [ if not ilg.noDebugData then 
                          yield mkCompilerGeneratedAttribute ilg
                          yield mkDebuggerNonUserCodeAttribute ilg]
           ilg.generatedAttribsCache <- res
           res
       | res -> res
    mkILCustomAttrs (attrs.AsList @ attribs)

let addMethodGeneratedAttrs ilg (mdef:ILMethodDef)   = {mdef with CustomAttrs   = add_generated_attrs ilg mdef.CustomAttrs}
let addPropertyGeneratedAttrs ilg (pdef:ILPropertyDef) = {pdef with CustomAttrs = add_generated_attrs ilg pdef.CustomAttrs}
let addFieldGeneratedAttrs ilg (fdef:ILFieldDef) = {fdef with CustomAttrs = add_generated_attrs ilg fdef.CustomAttrs}

let add_never_attrs ilg (attrs: ILAttributes) = mkILCustomAttrs (attrs.AsList @ [mkDebuggerBrowsableNeverAttribute ilg])
let addPropertyNeverAttrs ilg (pdef:ILPropertyDef) = {pdef with CustomAttrs = add_never_attrs ilg pdef.CustomAttrs}
let addFieldNeverAttrs ilg (fdef:ILFieldDef) = {fdef with CustomAttrs = add_never_attrs ilg fdef.CustomAttrs}


// PermissionSet is a 'blob' having the following format:
// � A byte containing a period (.).
// � A compressed int32 containing the number of attributes encoded in the blob.
// � An array of attributes each containing the following:
// o A String, which is the fully-qualified type name of the attribute. (Strings are encoded
// as a compressed int to indicate the size followed by an array of UTF8 characters.)
// o A set of properties, encoded as the named arguments to a custom attribute would be (as
// in �23.3, beginning with NumNamed).
let mkPermissionSet (ilg: ILGlobals) (action,attributes: list<(ILTypeRef * (string * ILType * ILAttribElem) list)>) = 
    let bytes = 
        [| yield (byte '.');
           yield! z_unsigned_int attributes.Length;
           for (tref:ILTypeRef,props) in attributes do 
              yield! encodeCustomAttrString tref.QualifiedNameWithNoShortMscorlib 
              let bytes = 
                  [| yield! z_unsigned_int props.Length;
                      for (nm,typ,value) in props do 
                          yield! encodeCustomAttrNamedArg ilg (nm,typ,true,value)|]
              yield! z_unsigned_int bytes.Length;
              yield! bytes |]
              
    PermissionSet(action,bytes)


let decodeILAttribData ilg (ca: ILAttribute) scope = 
    let bytes = ca.Data
    let sigptr = 0
    let bb0,sigptr = sigptr_get_byte bytes sigptr
    let bb1,sigptr = sigptr_get_byte bytes sigptr
    if not (bb0 = 0x01 && bb1 = 0x00) then failwith "decodeILAttribData: invalid data";
    let rec parseVal argty sigptr = 
      match argty with 
      | ILType.Value tspec when tspec.Name = "System.SByte" ->  
          let n,sigptr = sigptr_get_i8 bytes sigptr
          ILAttribElem.SByte n, sigptr
      | ILType.Value tspec when tspec.Name = "System.Byte" ->  
          let n,sigptr = sigptr_get_u8 bytes sigptr
          ILAttribElem.Byte n, sigptr
      | ILType.Value tspec when tspec.Name = "System.Int16" ->  
          let n,sigptr = sigptr_get_i16 bytes sigptr
          ILAttribElem.Int16 n, sigptr
      | ILType.Value tspec when tspec.Name = "System.UInt16" ->  
          let n,sigptr = sigptr_get_u16 bytes sigptr
          ILAttribElem.UInt16 n, sigptr
      | ILType.Value tspec when tspec.Name = "System.Int32" ->  
          let n,sigptr = sigptr_get_i32 bytes sigptr
          ILAttribElem.Int32 n, sigptr
      | ILType.Value tspec when tspec.Name = "System.UInt32" ->  
          let n,sigptr = sigptr_get_u32 bytes sigptr
          ILAttribElem.UInt32 n, sigptr
      | ILType.Value tspec when tspec.Name = "System.Int64" ->  
          let n,sigptr = sigptr_get_i64 bytes sigptr
          ILAttribElem.Int64 n, sigptr
      | ILType.Value tspec when tspec.Name = "System.UInt64" ->  
          let n,sigptr = sigptr_get_u64 bytes sigptr
          ILAttribElem.UInt64 n, sigptr
      | ILType.Value tspec when tspec.Name = "System.Double" ->  
          let n,sigptr = sigptr_get_ieee64 bytes sigptr
          ILAttribElem.Double n, sigptr
      | ILType.Value tspec when tspec.Name = "System.Single" ->  
          let n,sigptr = sigptr_get_ieee32 bytes sigptr
          ILAttribElem.Single n, sigptr
      | ILType.Value tspec when tspec.Name = "System.Char" ->  
          let n,sigptr = sigptr_get_u16 bytes sigptr
          ILAttribElem.Char (char (int32 n)), sigptr
      | ILType.Value tspec when tspec.Name = "System.Boolean" ->  
          let n,sigptr = sigptr_get_byte bytes sigptr
          ILAttribElem.Bool (not (n = 0)), sigptr
      | ILType.Boxed tspec when tspec.Name = "System.String" ->  
          let n,sigptr = sigptr_get_serstring_possibly_null bytes sigptr
          ILAttribElem.String n, sigptr
      | ILType.Boxed tspec when tspec.Name = "System.Type" ->  
          let nOpt,sigptr = sigptr_get_serstring_possibly_null bytes sigptr
          ILAttribElem.TypeRef (match nOpt with None -> None | Some n -> Some (mkILTyRef(ilg.mscorlibScopeRef,n))), sigptr
      | ILType.Boxed tspec when tspec.Name = "System.Object" ->  
          let et,sigptr = sigptr_get_u8 bytes sigptr
          if et = 0xFFuy then 
              ILAttribElem.Null, sigptr
          else
              let ty,sigptr = decodeCustomAttrElemType ilg bytes sigptr et 
              parseVal ty sigptr 
      | ILType.Array(shape,elemTy) when shape = ILArrayShape.SingleDimensional ->  
          let n,sigptr = sigptr_get_i32 bytes sigptr
          let rec parseElems acc n sigptr = 
            if n = 0 then List.rev acc else
            let v,sigptr = parseVal elemTy sigptr
            parseElems (v ::acc) (n-1) sigptr
          let elems = parseElems [] n sigptr
          ILAttribElem.Array(elemTy,elems), sigptr
      | ILType.Value _ ->  (* assume it is an enumeration *)
          let n,sigptr = sigptr_get_i32 bytes sigptr
          ILAttribElem.Int32 n, sigptr
      | _ ->  failwith "decodeILAttribData: attribute data involves an enum or System.Type value"
    let rec parseFixed argtys sigptr = 
      match argtys with 
        [] -> [],sigptr
      | h::t -> 
          let nh,sigptr = parseVal h sigptr
          let nt,sigptr = parseFixed t sigptr
          nh ::nt, sigptr
    let fixedArgs,sigptr = parseFixed (ca.Method.FormalArgTypes) sigptr
    let nnamed,sigptr = sigptr_get_u16 bytes sigptr
    let rec parseNamed acc n sigptr = 
      if n = 0 then List.rev acc else
      let isPropByte,sigptr = sigptr_get_u8 bytes sigptr
      let isProp = (int isPropByte = 0x54)
      let et,sigptr = sigptr_get_u8 bytes sigptr
      // We have a named value type
      // We'll only run into this situation when attempting to decode a named value type
      // property for a SecurityAttribute ILType
      let ty,sigptr = 
        if 0x55 = (int et) && Option.isSome scope then
            let qualified_tname,sigptr = sigptr_get_serstring bytes sigptr
            let scoref = the scope                    
            let tref = mkILTyRef (scoref,qualified_tname)
            let tspec = mkILNonGenericTySpec tref
            ILType.Value(tspec),sigptr            
        else
            decodeCustomAttrElemType ilg bytes sigptr et
      let nm,sigptr = sigptr_get_serstring bytes sigptr
      let v,sigptr = parseVal ty sigptr
      parseNamed ((nm,ty,isProp,v) :: acc) (n-1) sigptr
    let named = parseNamed [] (int nnamed) sigptr
    fixedArgs,named
      

let mkDebuggableAttributeV2 ilg (jitTracking, ignoreSymbolStoreSequencePoints, jitOptimizerDisabled,enableEnC) = 
    mkILCustomAttribute ilg 
      (tref_DebuggableAttribute ilg,[mkILNonGenericValueTy (tref_DebuggableAttribute_DebuggingModes ilg)],
       [ILAttribElem.Int32( 
                        (* See System.Diagnostics.DebuggableAttribute.DebuggingModes *)
                          (if jitTracking then 1 else 0) |||  
                          (if jitOptimizerDisabled then 256 else 0) |||  
                          (if ignoreSymbolStoreSequencePoints then 2 else 0) |||
                          (if enableEnC then 4 else 0))],[])

// -------------------------------------------------------------------- 
// Functions to collect up all the references in a full module or
// asssembly manifest.  The process also allocates
// a unique name to each unique internal assembly reference.
// -------------------------------------------------------------------- 

type ILReferences = 
    { AssemblyReferences: ILAssemblyRef list; 
      ModuleReferences: ILModuleRef list; }

type ILReferencesAccumulator = 
    { refsA: Hashset<ILAssemblyRef>; 
      refsM: Hashset<ILModuleRef>; }

let emptyILRefs = 
  { AssemblyReferences=[];
    ModuleReferences = []; }

let iter_pair f1 f2 (x,y) = f1 x; f2 y 

(* Now find references. *)
let refs_of_assref s x = Hashset.add s.refsA x
let refs_of_modref s x = Hashset.add s.refsM x
    
let refs_of_scoref s x = 
    match x with 
    | ILScopeRef.Local -> () 
    | ILScopeRef.Assembly assref -> refs_of_assref s assref
    | ILScopeRef.Module modref -> refs_of_modref s modref  

let refs_of_tref s (x:ILTypeRef) = refs_of_scoref s x.Scope
  
let rec refs_of_typ s x = 
    match x with
    | ILType.Void |  ILType.TypeVar _ -> ()
    | ILType.Modified(_,ty1,ty2) -> refs_of_tref s ty1; refs_of_typ s ty2
    | ILType.Array (_,ty)
    | ILType.Ptr ty | ILType.Byref ty -> refs_of_typ s ty 
    | ILType.Value tr | ILType.Boxed tr -> refs_of_tspec s tr
    | ILType.FunctionPointer mref -> refs_of_callsig s mref 

and refs_of_inst s i = List.iter (refs_of_typ s) i
and refs_of_tspec s (x:ILTypeSpec) = refs_of_tref s x.TypeRef;  refs_of_inst s x.GenericArgs
and refs_of_callsig s csig  = refs_of_typs s csig.ArgTypes; refs_of_typ s csig.ReturnType
and refs_of_genparam s x = refs_of_typs s x.Constraints
and refs_of_genparams s b = List.iter (refs_of_genparam s) b
    
and refs_of_dloc s ts = refs_of_tref s ts
   
and refs_of_mref s (x:ILMethodRef) = 
    refs_of_dloc s x.EnclosingTypeRef  ;
    List.iter (refs_of_typ s) x.mrefArgs;
    refs_of_typ s x.mrefReturn
    
and refs_of_fref s x = refs_of_tref s x.EnclosingTypeRef; refs_of_typ s x.Type
and refs_of_ospec s (OverridesSpec(mref,ty)) = refs_of_mref s mref; refs_of_typ s ty 
and refs_of_mspec s (x: ILMethodSpec) = 
    refs_of_mref s x.MethodRef;
    refs_of_typ s x.EnclosingType;
    refs_of_inst s x.GenericArgs

and refs_of_fspec s x =
    refs_of_fref s x.FieldRef;
    refs_of_typ s x.EnclosingType

and refs_of_typs s l = List.iter (refs_of_typ s) l
  
and refs_of_token s x = 
    match x with
    | ILToken.ILType ty -> refs_of_typ s ty
    | ILToken.ILMethod mr -> refs_of_mspec s mr
    | ILToken.ILField fr -> refs_of_fspec s fr

and refs_of_custom_attr s x = refs_of_mspec s x.Method
    
and refs_of_custom_attrs s (cas : ILAttributes) = List.iter (refs_of_custom_attr s) cas.AsList
and refs_of_varargs s tyso = Option.iter (refs_of_typs s) tyso 
and refs_of_instr s x = 
    match x with
    | I_call (_,mr,varargs) | I_newobj (mr,varargs) | I_callvirt (_,mr,varargs) ->
        refs_of_mspec s mr;
        refs_of_varargs s varargs
    | I_callconstraint (_,tr,mr,varargs) -> 
        refs_of_typ s tr;
        refs_of_mspec s mr;
        refs_of_varargs s varargs
    | I_calli (_,callsig,varargs) ->  
        refs_of_callsig s callsig;  refs_of_varargs s varargs 
    | I_jmp mr | I_ldftn mr | I_ldvirtftn mr -> 
        refs_of_mspec s mr
    | I_ldsfld (_,fr) | I_ldfld (_,_,fr) | I_ldsflda fr | I_ldflda fr | I_stsfld (_,fr) | I_stfld (_,_,fr) -> 
        refs_of_fspec s fr
    | I_isinst ty | I_castclass ty | I_cpobj ty | I_initobj ty | I_ldobj (_,_,ty) 
    | I_stobj (_,_,ty) | I_box ty |I_unbox ty | I_unbox_any ty | I_sizeof ty
    | I_ldelem_any (_,ty) | I_ldelema (_,_,ty) |I_stelem_any (_,ty) | I_newarr (_,ty)
    | I_mkrefany ty | I_refanyval ty 
    | EI_ilzero ty ->   refs_of_typ s ty 
    | I_ldtoken token -> refs_of_token s token 
    | I_stelem _|I_ldelem _|I_ldstr _|I_switch _|I_stloc _|I_stind _
    | I_starg _|I_ldloca _|I_ldloc _|I_ldind _
    | I_ldarga _|I_ldarg _|I_leave _|I_br _
    | I_brcmp _|I_rethrow|I_refanytype|I_ldlen|I_throw|I_initblk _ |I_cpblk _ 
    | I_localloc|I_ret |I_endfilter|I_endfinally|I_arglist
    | I_other _ | I_break
    | AI_add    | AI_add_ovf | AI_add_ovf_un | AI_and    | AI_div    | AI_div_un | AI_ceq      | AI_cgt      | AI_cgt_un   | AI_clt     
    | AI_clt_un  | AI_conv      _ | AI_conv_ovf  _ | AI_conv_ovf_un  _ | AI_mul       | AI_mul_ovf    | AI_mul_ovf_un | AI_rem       | AI_rem_un       
    | AI_shl       | AI_shr       | AI_shr_un | AI_sub       | AI_sub_ovf   | AI_sub_ovf_un   | AI_xor       | AI_or        | AI_neg       | AI_not       
    | AI_ldnull    | AI_dup       | AI_pop | AI_ckfinite | AI_nop | AI_ldc       _
    | I_seqpoint _ | EI_ldlen_multi _ ->  ()
      
  
and refs_of_il_block s c  = 
    match c with 
    | ILBasicBlock bb -> Array.iter (refs_of_instr s) bb.Instructions 
    | GroupBlock (_,l) -> List.iter (refs_of_il_code s) l 
    | RestrictBlock (_nms,c) -> refs_of_il_code s c 
    | TryBlock (l,r) -> 
       refs_of_il_code s l;
       match r with 
       | FaultBlock flt -> refs_of_il_code s flt 
       | FinallyBlock flt -> refs_of_il_code s flt 
       | FilterCatchBlock clauses -> 
           clauses |> List.iter (fun (flt,ctch)  -> 
               refs_of_il_code s ctch;
               match flt with 
               | CodeFilter fltcode -> refs_of_il_code s fltcode 
               |  TypeFilter ty -> refs_of_typ s ty)

and refs_of_il_code s c  = refs_of_il_block s c 
    
and refs_of_ilmbody s il = 
    List.iter (refs_of_local s) il.Locals;
    refs_of_il_code s il.Code 
    
and refs_of_local s loc = refs_of_typ s loc.Type
    
and refs_of_mbody s x = 
    match x with 
    | MethodBody.IL il -> refs_of_ilmbody s il
    | MethodBody.PInvoke (attr) -> refs_of_modref s attr.Where
    | _ -> ()

and refs_of_mdef s md = 
    List.iter (refs_of_param s) md.Parameters;
    refs_of_return s md.Return;
    refs_of_mbody s  md.mdBody.Contents;
    refs_of_custom_attrs s  md.CustomAttrs;
    refs_of_genparams s  md.GenericParams
    
and refs_of_param s p = refs_of_typ s p.Type 
and refs_of_return s (rt:ILReturn) = refs_of_typ s rt.Type
and refs_of_mdefs s x =  Seq.iter (refs_of_mdef s) x
    
and refs_of_event_def s (ed: ILEventDef) = 
    Option.iter (refs_of_typ s)  ed.Type ;
    refs_of_mref  s ed.AddMethod ;
    refs_of_mref  s ed.RemoveMethod;
    Option.iter (refs_of_mref s) ed.FireMethod ;
    List.iter (refs_of_mref s)  ed.OtherMethods ;
    refs_of_custom_attrs  s ed.CustomAttrs
    
and refs_of_events s (x: ILEventDefs) =  List.iter (refs_of_event_def s) x.AsList
    
and refs_of_property_def s pd = 
    Option.iter (refs_of_mref s)  pd.SetMethod ;
    Option.iter (refs_of_mref s)  pd.GetMethod ;
    refs_of_typ  s pd.Type ;
    refs_of_typs  s pd.Args ;
    refs_of_custom_attrs  s pd.CustomAttrs
    
and refs_of_properties s (x: ILPropertyDefs) = List.iter (refs_of_property_def s) x.AsList
    
and refs_of_fdef s fd = 
    refs_of_typ  s fd.Type;
    refs_of_custom_attrs  s fd.CustomAttrs

and refs_of_fields s fields = List.iter (refs_of_fdef s) fields
    
and refs_of_method_impls s mimpls =  List.iter (refs_of_method_impl s) mimpls
    
and refs_of_method_impl s m = 
    refs_of_ospec s m.Overrides;
    refs_of_mspec s m.OverrideBy

and refs_of_tdef_kind _s _k =  ()
  
and refs_of_tdef s (td : ILTypeDef)  =  
    refs_of_types s td.NestedTypes;
    refs_of_genparams s  td.GenericParams;
    refs_of_typs  s td.Implements;
    Option.iter (refs_of_typ s) td.Extends;
    refs_of_mdefs        s td.Methods;
    refs_of_fields       s td.Fields.AsList;
    refs_of_method_impls s td.MethodImpls.AsList;
    refs_of_events       s td.Events;
    refs_of_tdef_kind    s td.tdKind;
    refs_of_custom_attrs s td.CustomAttrs;
    refs_of_properties   s td.Properties

and refs_of_string _s _ = ()
and refs_of_types s (types: ILTypeDefs) = Seq.iter  (refs_of_tdef s) types
    
and refs_of_exported_type s (c: ILExportedTypeOrForwarder) = 
    refs_of_custom_attrs s c.CustomAttrs
    
and refs_of_exported_types s (tab: ILExportedTypesAndForwarders) = List.iter (refs_of_exported_type s) tab.AsList
    
and refs_of_resource_where s x = 
    match x with 
    | ILResourceLocation.Local _ -> ()
    | ILResourceLocation.File (mref,_) -> refs_of_modref s mref
    | ILResourceLocation.Assembly aref -> refs_of_assref s aref

and refs_of_resource s x = 
    refs_of_resource_where s x.Location;
    refs_of_custom_attrs s x.CustomAttrs
    
and refs_of_resources s (tab: ILResources) = List.iter (refs_of_resource s) tab.AsList
    
and refs_of_modul s m = 
    refs_of_types s m.TypeDefs;
    refs_of_resources s m.Resources;
    Option.iter (refs_of_manifest s) m.Manifest
    
and refs_of_manifest s m = 
    refs_of_custom_attrs s m.CustomAttrs;
    refs_of_exported_types s m.ExportedTypes

let computeILRefs modul = 
    let s = 
      { refsA = Hashset.create 10; 
        refsM = Hashset.create 5; }
    refs_of_modul s modul;
    { AssemblyReferences = Hashset.fold (fun x acc -> x::acc) s.refsA [];
      ModuleReferences =  Hashset.fold (fun x acc -> x::acc) s.refsM [] }

let tspan = System.TimeSpan(System.DateTime.Now.Ticks - System.DateTime(2000,1,1).Ticks)

let parseILVersion (vstr : string) = 
    // matches "v1.2.3.4" or "1.2.3.4". Note, if numbers are missing, returns -1 (not 0).
    let mutable vstr = vstr.TrimStart [|'v'|] 
    // if the version string contains wildcards, replace them
    let versionComponents = vstr.Split([|'.'|])
    
    // account for wildcards
    if versionComponents.Length > 2 then
      let defaultBuild = (uint16)tspan.Days % System.UInt16.MaxValue - 1us
      let defaultRevision = (uint16)(System.DateTime.Now.TimeOfDay.TotalSeconds / 2.0) % System.UInt16.MaxValue - 1us
      if versionComponents.[2] = "*" then
        if versionComponents.Length > 3 then
          failwith "Invalid version format"
        else
          // set the build number to the number of days since Jan 1, 2000
          versionComponents.[2] <- defaultBuild.ToString() ;
          // Set the revision number to number of seconds today / 2
          vstr <- System.String.Join(".",versionComponents) + "." + defaultRevision.ToString() ;
      elif versionComponents.Length > 3 && versionComponents.[3] = "*" then
        // Set the revision number to number of seconds today / 2
        versionComponents.[3] <- defaultRevision.ToString() ;
        vstr <- System.String.Join(".",versionComponents) ;
        
    let version = System.Version(vstr)
    let zero32 n = if n < 0 then 0us else uint16(n)
    // since the minor revision will be -1 if none is specified, we need to truncate to 0 to not break existing code
#if SILVERLIGHT
    let minorRevision = if versionComponents.Length < 4 then 0us else uint16(version.Revision)
#else
    let minorRevision = if versionComponents.Length < 4 then 0us else uint16(version.MinorRevision)
#endif    
    (zero32 version.Major, zero32 version.Minor, zero32 version.Build, minorRevision);;


let compareILVersions (a1,a2,a3,a4) ((b1,b2,b3,b4) : ILVersionInfo) = 
    let c = compare a1 b1
    if c <> 0 then c else
    let c = compare a2 b2
    if c <> 0 then c else
    let c = compare a3 b3
    if c <> 0 then c else
    let c = compare a4 b4
    if c <> 0 then c else
    0


let resolveILMethodRef td (mref:ILMethodRef) = 
    let args = mref.ArgTypes
    let nargs = List.length args
    let nm = mref.Name
    let possibles = td.Methods.FindByNameAndArity (nm,nargs)
    if isNil possibles then failwith ("no method named "+nm+" found in type "+td.Name);
    match 
      possibles |> List.filter (fun md -> 
          mref.CallingConv = md.CallingConv &&
          (md.Parameters,mref.ArgTypes) ||>  List.lengthsEqAndForall2 (fun p1 p2 -> p1.Type = p2) &&
          md.Return.Type = mref.ReturnType)  with 
    | [] -> 
        failwith ("no method named "+nm+" with appropriate argument types found in type "+td.Name);
    | [mdef] ->  mdef
    | _ -> 
        failwith ("multiple methods named "+nm+" appear with identical argument types in type "+td.Name)
        
let mkRefToILModule m =
  ILModuleRef.Create(m.Name, true, None)


let ungenericizeTypeName n = 
  let sym = '`'
  if 
    String.contains n sym && 
      (* check what comes after the symbol is a number *)
    (let m = String.rindex n sym
     let res = ref (m < n.Length - 1)
     for i = m + 1 to n.Length - 1 do
       res := !res && n.[i] >= '0' && n.[i] <= '9';
     !res)
  then 
      let pos = String.rindex n sym
      String.sub n 0 pos
  else n

type ILEventRef = 
    { erA: ILTypeRef; erB: string }
    static member Create(a,b) = {erA=a;erB=b}
    member x.EnclosingTypeRef = x.erA
    member x.Name = x.erB

type ILPropertyRef = 
    { prA: ILTypeRef; prB: string }
    static member Create (a,b) = {prA=a;prB=b}
    member x.EnclosingTypeRef = x.prA
    member x.Name = x.prB



