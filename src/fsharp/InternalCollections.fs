//----------------------------------------------------------------------------
//
// Copyright (c) 2002-2010 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

namespace Internal.Utilities.Collections
open System
open System.Collections.Generic

#nowarn "44" // This construct is deprecated. This F# library function has been renamed. Use 'isSome' instead

[<StructuralEquality; NoComparison>]
type internal ValueStrength<'T> =
   | Strong of 'T
   | Weak of WeakReference

type internal AgedLookup<'TKey,'TValue>(keepStrongly:int, areSame) =
    /// The list of items stored. Youngest is at the end of the list.
    /// The choice of order is somewhat aribtrary. If the other way then adding
    /// items would be O(1) and removing O(N).
    let mutable refs:('TKey*ValueStrength<'TValue>) list = [] 
    
    // The 75 here determines how long the list should be passed the end of strongly held
    // references. Some operations are O(N) and we don't want to let things get out of
    // hand.
    let keepTotal : int = max keepStrongly 75 
    
    /// Look up a the given key, return None if not found.
    let TryPeekKeyValueImpl(data,key) = 
        let rec Lookup key = function 
            // Treat a list of key-value pairs as a lookup collection.
            // This function returns true if two keys are the same according to the predicate
            // function passed in.
            | []->None
            | (key',value)::t->
                if areSame(key,key') then Some(key',value) 
                else Lookup key t      
        Lookup key data    
        
    /// Determines whether a particular key exists.
    let Exists(data,key) = 
        match TryPeekKeyValueImpl(data,key) with
        | Some _->true
        | None->false    
        
    /// Set a particular key's value.
    let Add(data,key,value) = 
        data @ [key,value]   
        
    /// Remove a particular key value 
    let RemoveImpl(data, key) = 
        data |> List.filter (fun (key',_)->not (areSame(key,key')))          
        
    let TryGetKeyValueImpl(data,key) = 
        match TryPeekKeyValueImpl(data,key) with 
        | Some(_, value) as result ->
            // If the result existed, move it to the top of the list.
            let data = RemoveImpl(data,key)
            let data = Add(data,key,value)
            result,data
        | None -> None,data          
       
    let FilterAndHold() =
        [ for (key,value) in refs do
            match value with
            | Strong(value) -> yield (key,value)
            | Weak(weakReference) ->
                match weakReference.Target with 
                | null -> ()
                | value -> yield key,(value:?>'TValue) ]

        
    let AssignWithStrength(newdata) = 
        let actualLength = List.length newdata
        let tossThreshold = max 0 (actualLength - keepTotal) // Delete everything less than this threshold
        let weakThreshhold = max 0 (actualLength - keepStrongly) // Weaken everything less than this threshhold
        
        refs<-
            newdata  
            |> List.mapi( fun n kv -> n,kv ) // Place the index.
            |> List.filter (fun (n:int,_) -> n >= tossThreshold) // Delete everything below the toss threshhold
            |> List.map( fun (n:int,(k,v)) -> k,if n<weakThreshhold then Weak(WeakReference(v)) else Strong(v) )
        
    member al.TryPeekKeyValue(key) = 
        // Returns the original key value as well since it may be different depending on equality test.
        let data = FilterAndHold()
        TryPeekKeyValueImpl(data,key)
        
    member al.TryGetKeyValue(key) = 
        let data = FilterAndHold()
        let result,newdata = TryGetKeyValueImpl(data,key)
        AssignWithStrength(newdata)
        result
    member al.TryGet(key) = 
        let data = FilterAndHold()
        let result,newdata = TryGetKeyValueImpl(data,key)
        AssignWithStrength(newdata)
        match result with
        | Some(_,value) -> Some(value)
        | None -> None
    member al.Put(key,value) = 
        let data = FilterAndHold()
        let data = if Exists(data,key) then RemoveImpl(data,key) else data
        let data = Add(data,key,value)
        AssignWithStrength(data) // This will remove extras 
    member al.Remove(key) = 
        let data = FilterAndHold()
        let newdata = RemoveImpl(data,key)
        AssignWithStrength(newdata)
    member al.MostRecent : ('TKey*'TValue) option=  
        let data = FilterAndHold()
        if not data.IsEmpty then 
           // Non-optimal reverse list to get most recent. Consider an array of option for the data structure.
           Some(data |> List.rev |> List.head)
        else None        
    member al.Clear() =
       refs <- []
    member al.ToSeq() =
        FilterAndHold()
        |> List.toSeq
           
        

type internal MruCache<'TKey,'TValue>(n,compute, areSame, ?isStillValid : 'TKey*'TValue->bool, ?areSameForSubsumption, ?logComputedNewValue, ?logUsedCachedValue) =
        
    /// Default behavior of areSame function is structural equality (=)
    let AreSameForSubsumption(key,key') : bool = 
        match areSameForSubsumption with 
        | Some(f)->f(key,key')
        | None->areSame(key,key')   
        
    /// The list of items in the cache. Youngest is at the end of the list.
    /// The choice of order is somewhat aribtrary. If the other way then adding
    /// items would be O(1) and removing O(N).
    let cache = AgedLookup<'TKey,'TValue>(n,AreSameForSubsumption)
        
    /// Whether or not this result value is still valid.
    let IsStillValid(key,value) = 
        match isStillValid with 
        | Some(f)->f(key,value) 
        | None-> true        
        
    let Log = function
        | Some(f) -> f
        | None-> ignore

    /// Log a message when a new value is computed.        
    let LogComputedNewValue = Log logComputedNewValue
        
    /// Log a message when an existing value was retrieved from cache.
    let LogUsedCachedValue =  Log logUsedCachedValue
                
    member bc.GetAvailable(key) = 
        match cache.TryPeekKeyValue(key) with
        | Some(key', value)->
            if areSame(key',key) then Some(value)
            else None
        | None -> None
       
    member bc.Get(key) = 
        let Compute() = 
            let value = compute key
            cache.Put(key, value)
            LogComputedNewValue(key)
            value        
        match cache.TryGetKeyValue(key) with
        | Some(key', value) -> 
            if areSame(key', key) && IsStillValid(key,value) then
                LogUsedCachedValue(key)
                value
            else Compute()
        | None -> Compute()
           
    member bc.MostRecent = 
        cache.MostRecent
       
    member bc.SetAlternate(key:'TKey,value:'TValue) = 
        cache.Put(key,value)
       
    member bc.Remove(key) = 
        cache.Remove(key)
       
    member bc.Clear() =
        cache.Clear()
        
/// List helpers
[<Sealed>]
type internal List = 
    /// Return a new list with one element for each unique 'TKey. Multiple 'TValues are flattened. The original order of the first instance of 'TKey is preserved.
    static member GroupByFirst( l : ('TKey * 'TValue) list) : ('TKey * 'TValue list) list =
        let nextIndex = ref 0
        let result = System.Collections.Generic.List<'TKey * System.Collections.Generic.List<'TValue>>()
        let keyToIndex = Dictionary<'TKey,int>(HashIdentity.Structural)
        let IndexOfKey(key) =
            match keyToIndex.TryGetValue(key) with
            | true, v -> v
            | false, _ -> 
                keyToIndex.Add(key,!nextIndex)
                nextIndex := !nextIndex + 1
                !nextIndex - 1
            
        for kv in l do 
            let index = IndexOfKey(fst kv)
            if index>= result.Count then 
                let k,vs = fst kv,System.Collections.Generic.List<'TValue>()
                vs.Add(snd kv)
                result.Add(k,vs)
            else
                let _,vs = result.[index]
                vs.Add(snd kv)
        
        result |> Seq.map(fun (k,vs) -> k,vs |> List.ofSeq ) |> List.ofSeq
