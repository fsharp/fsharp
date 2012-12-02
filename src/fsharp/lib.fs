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


module internal Microsoft.FSharp.Compiler.Lib

open System.IO
open System.Collections.Generic
open Internal.Utilities
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library


/// is this the developer-debug build? 
let debug = false 
let verbose = false
let progress = ref false 
let tracking = ref false // intended to be a general hook to control diagnostic output when tracking down bugs

let condition _s = 
#if SILVERLIGHT
    false
#else    
    try (System.Environment.GetEnvironmentVariable(_s) <> null) with _ -> false
#endif    

let dispose (x:System.IDisposable) = match x with null -> () | x -> x.Dispose()

//-------------------------------------------------------------------------
// Library: bits
//------------------------------------------------------------------------

module Bits = 
    let b0 n =  (n          &&& 0xFF)  
    let b1 n =  ((n >>> 8)  &&& 0xFF) 
    let b2 n =  ((n >>> 16) &&& 0xFF) 
    let b3 n =  ((n >>> 24) &&& 0xFF) 

    let rec pown32 n = if n = 0 then 0  else (pown32 (n-1) ||| (1  <<<  (n-1)))
    let rec pown64 n = if n = 0 then 0L else (pown64 (n-1) ||| (1L <<< (n-1)))
    let mask32 m n = (pown32 n) <<< m
    let mask64 m n = (pown64 n) <<< m


//-------------------------------------------------------------------------
// Library: files
//------------------------------------------------------------------------

module Filename = 
    let fullpath cwd nm = 
        let p = if FileSystem.IsPathRootedShim(nm) then nm else Path.Combine(cwd,nm)
        try FileSystem.GetFullPathShim(p) with 
        | :? System.ArgumentException 
        | :? System.ArgumentNullException 
        | :? System.NotSupportedException 
        | :? System.IO.PathTooLongException 
        | :? System.Security.SecurityException -> p

    let hasSuffixCaseInsensitive suffix filename = (* case-insensitive *)
      Filename.checkSuffix (String.lowercase filename) (String.lowercase suffix)

    let isDll file = hasSuffixCaseInsensitive ".dll" file 

//-------------------------------------------------------------------------
// Library: Orders
//------------------------------------------------------------------------

module Bool = 
    let order = LanguagePrimitives.FastGenericComparer<bool>

module Int32 = 
    let order = LanguagePrimitives.FastGenericComparer<int>

module Int64 = 
    let order = LanguagePrimitives.FastGenericComparer<int64>

module Pair = 
    let order (compare1: IComparer<'T1>, compare2: IComparer<'T2>) =
        { new IComparer<'T1 * 'T2> with 
             member __.Compare((a1,a2),(aa1,aa2)) =
                  let res1 = compare1.Compare (a1, aa1)
                  if res1 <> 0 then res1 else compare2.Compare (a2, aa2) }


type NameSet =  Zset<string>
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NameSet =
    let ofList l : NameSet = List.foldBack Zset.add l (Zset.empty String.order)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NameMap = 
    let domain m = Map.foldBack (fun x _ acc -> Zset.add x acc) m (Zset.empty String.order)
    let domainL m = Zset.elements (domain m)


(*

//-------------------------------------------------------------------------
// Library: Atoms
//------------------------------------------------------------------------

type AtomTable = 
    { LookupTable : Dictionary<int32,string>
      EncodeTable : Dictionary<string,int32> }
    member at.Encode(name:string) = 
        let mutable res = 0 
        let idx = 
            if at.EncodeTable.TryGetValue(name, &res) then 
                res
            else
                let idx = at.EncodeTable.Count
                at.LookupTable.[idx] <- name
                at.EncodeTable.[name] <- idx
                idx
        Atom(res
#if DEBUG
             ,at
#endif
            )


and Atom internal (idx:int32
#if DEBUG
                   ,_provider:AtomTable
#endif
                   ) =
    member __.Index = idx
    member __.Deref(provider: AtomTable) = 
       
#if DEBUG
        assert (provider = _provider)
        assert (provider.LookupTable.ContainsKey idx)
#endif
        provider.LookupTable.[idx]
*)            

    

//---------------------------------------------------------------------------
// Library: Pre\Post checks
//------------------------------------------------------------------------- 
module Check = 
    
    /// Throw System.InvalidOperationException() if argument is None.
    /// If there is a value (e.g. Some(value)) then value is returned.
    let NotNone argname (arg:'T option) : 'T = 
        match arg with 
        | None -> raise (new System.InvalidOperationException(argname))
        | Some x -> x

    /// Throw System.ArgumentNullException() if argument is null.
    let ArgumentNotNull arg argname = 
        match box(arg) with 
        | null -> raise (new System.ArgumentNullException(argname))
        | _ -> ()
       
        
    /// Throw System.ArgumentNullException() if array argument is null.
    /// Throw System.ArgumentOutOfRangeException() is array argument is empty.
    let ArrayArgumentNotNullOrEmpty (arr:'T[]) argname = 
        ArgumentNotNull arr argname
        if (0 = arr.Length) then
            raise (new System.ArgumentOutOfRangeException(argname))

    /// Throw System.ArgumentNullException() if string argument is null.
    /// Throw System.ArgumentOutOfRangeException() is string argument is empty.
    let StringArgumentNotNullOrEmpty (s:string) argname = 
        ArgumentNotNull s argname
        if s.Length = 0 then
            raise (new System.ArgumentNullException(argname))

//-------------------------------------------------------------------------
// Library 
//------------------------------------------------------------------------

type IntMap<'T> = Zmap<int,'T>
module IntMap = 
    let empty () = Zmap.empty Int32.order

    let add k v (t:IntMap<'T>) = Zmap.add k v t
    let find k (t:IntMap<'T>) = Zmap.find k t
    let tryFind k (t:IntMap<'T>) = Zmap.tryFind k t
    let remove  k (t:IntMap<'T>) = Zmap.remove k t
    let mem     k (t:IntMap<'T>)  = Zmap.mem k t
    let iter    f (t:IntMap<'T>)  = Zmap.iter f t
    let map     f (t:IntMap<'T>)  = Zmap.map f t 
    let fold    f (t:IntMap<'T>)  z = Zmap.fold f t z


//-------------------------------------------------------------------------
// Library: generalized association lists
//------------------------------------------------------------------------

module ListAssoc = 

    /// Treat a list of key-value pairs as a lookup collection.
    /// This function looks up a value based on a match from the supplied
    /// predicate function.
    let rec find f x l = 
      match l with 
      | [] -> notFound()
      | (x',y)::t -> if f x x' then y else find f x t

    /// Treat a list of key-value pairs as a lookup collection.
    /// This function returns true if two keys are the same according to the predicate
    /// function passed in.
    let rec containsKey (f:'key->'key->bool) (x:'key) (l:('key*'value) list) : bool = 
      match l with 
      | [] -> false
      | (x',_y)::t -> f x x' || containsKey f x t

//-------------------------------------------------------------------------
// Library: lists as generalized sets
//------------------------------------------------------------------------

module ListSet = 
    (* NOTE: O(n)! *)
    let rec contains f x l = 
        match l with 
        | [] -> false
        | x'::t -> f x x' || contains f x t

    (* NOTE: O(n)! *)
    let insert f x l = if contains f x l then l else x::l
    let unionFavourRight f l1 l2 = 
        match l1, l2 with 
        | _, [] -> l1
        | [], _ -> l2 
        | _ -> List.foldBack (insert f) l1 l2 (* nb. foldBack to preserve natural orders *)

    (* NOTE: O(n)! *)
    let rec private findIndexAux eq x l n =
        match l with
        | [] -> notFound()
        | (h::t) -> if eq h x then n else findIndexAux eq x t (n+1)

    let findIndex eq x l = findIndexAux eq x l 0

    let rec remove f x l = 
        match l with 
        | (h::t) -> if f x h then t else h:: remove f x t
        | [] -> []

    (* NOTE: quadratic! *)
    let rec subtract f l1 l2 = 
      match l2 with 
      | (h::t) -> subtract f (remove (fun y2 y1 ->  f y1 y2) h l1) t
      | [] -> l1

    let isSubsetOf f l1 l2 = List.forall (fun x1 -> contains f x1 l2) l1
    (* nb. preserve orders here: f must be applied to elements of l1 then elements of l2*)
    let isSupersetOf f l1 l2 = List.forall (fun x2 -> contains (fun y2 y1 ->  f y1 y2) x2 l1) l2
    let equals f l1 l2 = isSubsetOf f l1 l2 && isSupersetOf f l1 l2

    let unionFavourLeft f l1 l2 = 
        match l1,l2 with 
        | _,[] -> l1 
        | [],_ -> l2 
        | _ -> l1 @ (subtract f l2 l1)


    (* NOTE: not tail recursive! *)
    let rec intersect f l1 l2 = 
        match l2 with 
        | (h::t) -> if contains f h l1 then h::intersect f l1 t else intersect f l1 t
        | [] -> []

    (* NOTE: quadratic! *)
    // Note: if duplicates appear, keep the ones toward the _front_ of the list
    let setify f l = List.foldBack (insert f) (List.rev l) [] |> List.rev


module FlatListSet = 
    let remove f x l = FlatList.filter (fun y -> not (f x y)) l

//-------------------------------------------------------------------------
// Library: pairs
//------------------------------------------------------------------------

let mapFoldFst f s (x,y) =  let x',s = f s x in  (x',y),s
let mapFoldSnd f s (x,y) =  let y',s = f s y in  (x,y'),s
let pair a b = a,b      

let p13 (x,_y,_z) = x
let p23 (_x,y,_z) = y
let p33 (_x,_y,z) = z

let map1Of2 f (a1,a2)       = (f a1,a2)
let map2Of2 f (a1,a2)       = (a1,f a2)
let map1Of3 f (a1,a2,a3)     = (f a1,a2,a3)
let map2Of3 f (a1,a2,a3)     = (a1,f a2,a3)
let map3Of3 f (a1,a2,a3)     = (a1,a2,f a3)
let map3Of4 f (a1,a2,a3,a4)     = (a1,a2,f a3,a4)
let map4Of4 f (a1,a2,a3,a4)   = (a1,a2,a3,f a4)
let map5Of5 f (a1,a2,a3,a4,a5) = (a1,a2,a3,a4,f a5)
let map6Of6 f (a1,a2,a3,a4,a5,a6) = (a1,a2,a3,a4,a5,f a6)
let foldPair (f1,f2)    acc (a1,a2)         = f2 (f1 acc a1) a2
let fold1Of2 f1    acc (a1,_a2)         = f1 acc a1
let foldTriple (f1,f2,f3) acc (a1,a2,a3)      = f3 (f2 (f1 acc a1) a2) a3
let mapPair (f1,f2)    (a1,a2)     = (f1 a1, f2 a2)
let mapTriple (f1,f2,f3) (a1,a2,a3)  = (f1 a1, f2 a2, f3 a3)
let fmap2Of2 f z (a1,a2)       = let z,a2 = f z a2 in z,(a1,a2)

module List = 
    let noRepeats xOrder xs =
        let s = Zset.addList   xs (Zset.empty xOrder) // build set 
        Zset.elements s          // get elements... no repeats 

    let groupBy f (xs:list<'T>) =  xs |> Seq.groupBy f |> Seq.map (map2Of2 Seq.toList) |> Seq.toList

//---------------------------------------------------------------------------
// Zmap rebinds
//------------------------------------------------------------------------- 

module Zmap = 
    let force  k   mp           = match Zmap.tryFind k mp with Some x -> x | None -> failwith "Zmap.force: lookup failed"

    let mapKey key f mp =
      match f (Zmap.tryFind key mp) with
      | Some fx -> Zmap.add key fx mp       
      | None    -> Zmap.remove key mp

//---------------------------------------------------------------------------
// Zset
//------------------------------------------------------------------------- 

module Zset =
    let ofList order xs = Zset.addList   xs (Zset.empty order)

    // CLEANUP NOTE: move to Zset?
    let rec fixpoint f (s as s0) =
        let s = f s
        if Zset.equal s s0 then s0           (* fixed *)
                           else fixpoint f s (* iterate *)

//---------------------------------------------------------------------------
// Misc
//------------------------------------------------------------------------- 

let equalOn f x y = (f x) = (f y)


//---------------------------------------------------------------------------
// Buffer printing utilities
//---------------------------------------------------------------------------

let bufs f = 
    let buf = System.Text.StringBuilder 100 
    f buf; 
    buf.ToString()

let buff (os: TextWriter) f x = 
    let buf = System.Text.StringBuilder 100 
    f buf x; 
    os.Write(buf.ToString())

// Converts "\n" into System.Environment.NewLine before writing to os. See lib.fs:buff
let writeViaBufferWithEnvironmentNewLines (os: TextWriter) f x = 
    let buf = System.Text.StringBuilder 100 
    f buf x;
    let text = buf.ToString()
    let text = text.Replace("\n",System.Environment.NewLine)
    os.Write text
        
//---------------------------------------------------------------------------
// Imperative Graphs 
//---------------------------------------------------------------------------

type GraphNode<'Data, 'Id> = { nodeId: 'Id; nodeData: 'Data; mutable nodeNeighbours: GraphNode<'Data, 'Id> list }

type Graph<'Data, 'Id when 'Id : comparison and 'Id : equality>
         (nodeIdentity: ('Data -> 'Id),
          nodes: 'Data list,
          edges: ('Data * 'Data) list) =

    let edges = edges |> List.map (fun (v1,v2) -> nodeIdentity v1, nodeIdentity v2)
    let nodes = nodes |> List.map (fun d -> nodeIdentity d, { nodeId = nodeIdentity d; nodeData=d; nodeNeighbours=[] }) 
    let tab = Map.ofList nodes 
    let nodes = List.map snd nodes
    do for node in nodes do 
        node.nodeNeighbours <- edges |>  List.filter (fun (x,_y) -> x = node.nodeId) |> List.map (fun (_,nodeId) -> tab.[nodeId])

    member g.GetNodeData nodeId = tab.[nodeId].nodeData

    member g.IterateCycles f = 
        let rec trace path node = 
            if List.exists (nodeIdentity >> (=) node.nodeId) path then f (List.rev path)
            else List.iter (trace (node.nodeData::path)) node.nodeNeighbours
        List.iter (fun node -> trace [] node) nodes 

#if OLDCODE

    member g.DepthFirstSearch() = 
        let grey = ref Set.empty 
        let time = ref 0
        let forest = ref []
        let backEdges = ref []
        let discoveryTimes = ref Map.empty 
        let finishingTimes = ref Map.empty 
        nodes |> List.iter (fun n ->  
            // build a dfsTree for each node in turn 
            let treeEdges = ref []
            let rec visit n1 = 
                incr time;
                grey := Set.add n1.nodeId !grey;
                discoveryTimes := Map.add n1.nodeId !time !discoveryTimes;
                for n2 in n1.nodeNeighbours do
                    if not ((!grey).Contains n2.nodeId) then 
                        treeEdges := (n1.nodeId,n2.nodeId) :: !treeEdges;
                        visit(n2)
                    else 
                        backEdges := (n1.nodeId,n2.nodeId) :: !backEdges
                incr time;
                finishingTimes := Map.add n1.nodeId !time !finishingTimes;
                ()
            if not ((!grey).Contains n.nodeId) then 
                visit(n);
                forest := (n.nodeId,!treeEdges) :: !forest);

        !forest, !backEdges,  (fun n -> (!discoveryTimes).[n]), (fun n -> (!finishingTimes).[n])
 

    // Present strongly connected components, in dependency order 
    // Each node is assumed to have a self-edge 
    member g.GetTopologicalSortStronglyConnectedComponents() = 
        let forest, backEdges, discoveryTimes, finishingTimes = g.DepthFirstSearch()
        let nodeIds = List.map (fun n -> n.nodeId) nodes
        let nodesInDecreasingFinishingOrder = 
          List.sortWith (fun n1 n2 -> -(compare (finishingTimes n1) (finishingTimes n2))) nodeIds
        let gT = Graph (nodeIdentity, List.map g.GetNodeData nodesInDecreasingFinishingOrder, List.map (fun (x,y) -> (g.GetNodeData y, g.GetNodeData x)) edges)
        let forest, backEdges, discoveryTimes, finishingTimes = gT.DepthFirstSearch()
        let scc (root,tree) = Set.add root (List.foldBack (fun (n1,n2) acc -> Set.add n1 (Set.add n2 acc)) tree Set.empty)
        let sccs = List.rev (List.map scc forest)
        List.map (Set.toList >> List.map g.GetNodeData) sccs
#endif


//---------------------------------------------------------------------------
// In some cases we play games where we use 'null' as a more efficient representation
// in F#. The functions below are used to give initial values to mutable fields.
// This is an unsafe trick, as it relies on the fact that the type of values
// being placed into the slot never utilizes "null" as a representation. To be used with
// with care.
//----------------------------------------------------------------------------

// The following DEBUG code does not currently compile.
//#if DEBUG
//type 'T NonNullSlot = 'T option 
//let nullableSlotEmpty() = None 
//let nullableSlotFull(x) = Some x
//#else
type NonNullSlot<'T> = 'T
let nullableSlotEmpty() = Unchecked.defaultof<'T>
let nullableSlotFull x = x
//#endif    

//---------------------------------------------------------------------------
// Caches, mainly for free variables
//---------------------------------------------------------------------------

type cache<'T> = { mutable cacheVal: 'T NonNullSlot; }
let newCache() = { cacheVal = nullableSlotEmpty() }

let inline cached cache resf = 
    match box cache.cacheVal with 
    | null -> 
        let res = resf() 
        cache.cacheVal <- nullableSlotFull res; 
        res
    | _ -> 
        cache.cacheVal

let inline cacheOptRef cache f = 
    match !cache with 
    | Some v -> v
    | None -> 
       let res = f()
       cache := Some res;
       res 


// There is a bug in .NET Framework v2.0.52727 DD#153959 that very occasionally hits F# code.
// It is related to recursive class loading in multi-assembly NGEN scenarios. The bug has been fixed but
// not yet deployed.
// The bug manifests itself as an ExecutionEngine failure or fast-fail process exit which comes
// and goes depending on whether components are NGEN'd or not, e.g. 'ngen install FSharp.COmpiler.dll'
// One workaround for the bug is to break NGEN loading and fixups into smaller fragments. Roughly speaking, the NGEN
// loading process works by doing delayed fixups of references in NGEN code. This happens on a per-method
// basis. For example, one manifestation is that a "print" before calling a method like Lexfilter.create gets
// displayed but the corresponding "print" in the body of that function doesn't get displayed. In between, the NGEN
// image loader is performing a whole bunch of fixups of the NGEN code for the body of that method, and also for
// bodies of methods referred to by that method. That second bit is very important: the fixup causing the crash may
// be a couple of steps down the dependency chain.
//
// One way to break work into smaller chunks is to put delays in the call chains, i.e. insert extra stack frames. That's
// what the function 'delayInsertedToWorkaroundKnownNgenBug' is for. If you get this problem, try inserting 
//    delayInsertedToWorkaroundKnownNgenBug "Delay1" (fun () -> ...)
// at the top of the function that doesn't seem to be being called correctly. This will help you isolate out the problem
// and may make the problem go away altogher. Enable the 'print' commands in that function too.

let delayInsertedToWorkaroundKnownNgenBug s f = 
    (* Some random code to prevent inlining of this function *)
    let res = ref 10
    for i = 0 to 2 do 
       res := !res + String.length s;
    done;
    if verbose then printf "------------------------executing NGEN bug delay '%s', calling 'f' --------------\n" s;
    let res = f()
    if verbose then printf "------------------------exiting NGEN bug delay '%s' --------------\n" s;
    res
    

#if DUMPER
type Dumper(x:obj) =
     [<DebuggerBrowsable(DebuggerBrowsableState.Collapsed)>]
     member self.Dump = sprintf "%A" x 
#endif

//---------------------------------------------------------------------------
// AsyncUtil
//---------------------------------------------------------------------------

module internal AsyncUtil =
    open System
    open System.Threading
    open Microsoft.FSharp.Control

    /// Represents the reified result of an asynchronous computation
    [<NoEquality; NoComparison>]
    type AsyncResult<'T>  =
        |   AsyncOk of 'T
        |   AsyncException of exn
        |   AsyncCanceled of OperationCanceledException

        static member Commit(res:AsyncResult<'T>) =
            Async.FromContinuations (fun (cont,econt,ccont) ->
                    match res with
                    | AsyncOk v -> cont v
                    | AsyncException exn -> econt exn
                    | AsyncCanceled exn -> ccont exn)

    /// When using .NET 4.0 you can replace this type by Task<'T>
    [<Sealed>]
    type AsyncResultCell<'T>() =
        let mutable result = None
        // The continuation for the result, if any
        let mutable savedConts = []
       
        let syncRoot = new obj()
               

        // Record the result in the AsyncResultCell.
        // Ignore subsequent sets of the result. This can happen, e.g. for a race between
        // a cancellation and a success.
        member x.RegisterResult (res:AsyncResult<'T>) =
            let grabbedConts =
                lock syncRoot (fun () ->
                    if result.IsSome then  
                        []
                    else
                        result <- Some res;
                        // Invoke continuations in FIFO order
                        // Continuations that Async.FromContinuations provide do QUWI/SynchContext.Post,
                        // so the order is not overly relevant but still.                        
                        List.rev savedConts)
            let postOrQueue (sc:SynchronizationContext,cont) =
                match sc with
                |   null -> ThreadPool.QueueUserWorkItem(fun _ -> cont res) |> ignore
                |   sc -> sc.Post((fun _ -> cont res), state=null)

            // Run continuations outside the lock
            match grabbedConts with
            |   [] -> ()
            |   [(sc,cont) as c] -> 
                    if SynchronizationContext.Current = sc then
                        cont res
                    else
                        postOrQueue c
            |   _ ->
                    grabbedConts |> List.iter postOrQueue

        /// Get the reified result
        member private x.AsyncPrimitiveResult =
            Async.FromContinuations(fun (cont,_,_) ->
                let grabbedResult =
                    lock syncRoot (fun () ->
                        match result with
                        | Some _ ->
                            result
                        | None ->
                            // Otherwise save the continuation and call it in RegisterResult
                            let sc = SynchronizationContext.Current
                            savedConts <- (sc,cont)::savedConts
                            None)
                // Run the action outside the lock
                match grabbedResult with
                | None -> ()
                | Some res -> cont res)
                          

        /// Get the result and commit it
        member x.AsyncResult =
            async { let! res = x.AsyncPrimitiveResult
                    return! AsyncResult.Commit(res) }

//---------------------------------------------------------------------------
// EnableHeapTerminationOnCorruption()
//---------------------------------------------------------------------------

// USAGE: call UnmanagedProcessExecutionOptions.EnableHeapTerminationOnCorruption() from "main()".
// Note: This is not SDL required but recommended.
module UnmanagedProcessExecutionOptions =
    open System
    open System.Runtime.InteropServices

    [<DllImport("kernel32.dll")>] 
    extern UIntPtr private GetProcessHeap()

    [<DllImport("kernel32.dll")>]
    extern bool private HeapSetInformation( 
        UIntPtr _HeapHandle, 
        UInt32  _HeapInformationClass, 
        UIntPtr _HeapInformation, 
        UIntPtr _HeapInformationLength)

    [<DllImport("kernel32.dll")>]
    extern UInt32 private GetLastError()

    // Translation of C# from http://swikb/v1/DisplayOnlineDoc.aspx?entryID=826 and copy in bug://5018
    [<System.Security.Permissions.SecurityPermission(System.Security.Permissions.SecurityAction.Assert,UnmanagedCode = true)>] 
    let EnableHeapTerminationOnCorruption() =        
        if (System.Environment.OSVersion.Version.Major >= 6 && // If OS is Vista or higher
            System.Environment.Version.Major < 3) then         // and CLR not 3.0 or higher 
            // "The flag HeapSetInformation sets is available in Windows XP SP3 and later.
            //  The data structure used for heap information is available on earlier versions of Windows.
            //  The call will either return TRUE (found and set the flag) or false (flag not found).
            //  Not a problem in native code, so the program will merrily continue running.
            //  In managed code, the call to HeapSetInformation is a p/invoke.
            //  If HeapSetInformation returns FALSE then an exception will be thrown.
            //  If we are not running an OS which supports this (XP SP3, Vista, Server 2008, and Win7)
            //  then the call should not be made." -- see bug://5018.
            // See also:
            //  http://blogs.msdn.com/michael_howard/archive/2008/02/18/faq-about-heapsetinformation-in-windows-vista-and-heap-based-buffer-overruns.aspx
            let HeapEnableTerminationOnCorruption = 1u : uint32
            if not (HeapSetInformation(GetProcessHeap(),HeapEnableTerminationOnCorruption,UIntPtr.Zero,UIntPtr.Zero)) then
                  raise (System.Security.SecurityException( 
                            "Unable to enable unmanaged process execution option TerminationOnCorruption. " + 
                            "HeapSetInformation() returned FALSE; LastError = 0x" + 
                            GetLastError().ToString("X").PadLeft(8,'0') + "."))

