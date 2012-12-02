//----------------------------------------------------------------------------
// Copyright (c) 2002-2012 Microsoft Corporation. 
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

type internal AgedLookup<'TKey,'TValue>(keepStrongly:int, areSame, ?onStrongDiscard : ('TValue -> unit), ?keepMax: int) =
    /// The list of items stored. Youngest is at the end of the list.
    /// The choice of order is somewhat aribtrary. If the other way then adding
    /// items would be O(1) and removing O(N).
    let mutable refs:('TKey*ValueStrength<'TValue>) list = [] 

    // Only set a strong discard function if keepMax is expliclty set to keepStrongly, i.e. there are no weak entries in this lookup.
    do assert (onStrongDiscard.IsNone || Some keepStrongly = keepMax)
       
    let strongDiscard x = match onStrongDiscard with None -> () | Some f -> f x
        
    // The 75 here determines how long the list should be passed the end of strongly held
    // references. Some operations are O(N) and we don't want to let things get out of
    // hand.
    let keepMax = defaultArg keepMax 75 
    let keepMax = max keepStrongly keepMax 
    
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
    let Exists(data,key) = TryPeekKeyValueImpl(data,key).IsSome
        
    /// Set a particular key's value.
    let Add(data,key,value) = 
        data @ [key,value]   
        
    /// Promote a particular key value 
    let Promote (data, key, value) = 
        (data |> List.filter (fun (key',_)-> not (areSame(key,key')))) @ [ (key, value) ] 

    /// Remove a particular key value 
    let RemoveImpl (data, key) = 
        let discard,keep = data |> List.partition (fun (key',_)-> areSame(key,key'))
        keep, discard
        
    let TryGetKeyValueImpl(data,key) = 
        match TryPeekKeyValueImpl(data,key) with 
        | Some(_, value) as result ->
            // If the result existed, move it to the top of the list.
            result,Promote (data,key,value)
        | None -> None,data          
       
    /// Remove weak entries from the list that have been collected
    let FilterAndHold() =
        [ for (key,value) in refs do
            match value with
            | Strong(value) -> yield (key,value)
            | Weak(weakReference) ->
                match weakReference.Target with 
                | null -> assert onStrongDiscard.IsNone; ()
                | value -> yield key,(value:?>'TValue) ]

        
    let AssignWithStrength(newdata,discard1) = 
        let actualLength = List.length newdata
        let tossThreshold = max 0 (actualLength - keepMax) // Delete everything less than this threshold
        let weakThreshhold = max 0 (actualLength - keepStrongly) // Weaken everything less than this threshhold
        
        let newdata = newdata|> List.mapi( fun n kv -> n,kv ) // Place the index.
        let newdata,discard2 = newdata |> List.partition (fun (n:int,_) -> n >= tossThreshold)
        let newdata = 
            newdata 
            |> List.map( fun (n:int,(k,v)) -> 
                let handle = 
                    if n<weakThreshhold then 
                        assert onStrongDiscard.IsNone; // it disappeared, we can't dispose 
                        Weak(WeakReference(v)) 
                    else 
                        Strong(v)
                k,handle )
        refs<- newdata
        discard1 |> List.iter (snd >> strongDiscard)
        discard2 |> List.iter (snd >> snd >> strongDiscard)
        
    member al.TryPeekKeyValue(key) = 
        // Returns the original key value as well since it may be different depending on equality test.
        let data = FilterAndHold()
        TryPeekKeyValueImpl(data,key)
        
    member al.TryGetKeyValue(key) = 
        let data = FilterAndHold()
        let result,newdata = TryGetKeyValueImpl(data,key)
        AssignWithStrength(newdata,[])
        result
    member al.TryGet(key) = 
        let data = FilterAndHold()
        let result,newdata = TryGetKeyValueImpl(data,key)
        AssignWithStrength(newdata,[])
        match result with
        | Some(_,value) -> Some(value)
        | None -> None
    member al.Put(key,value) = 
        let data = FilterAndHold()
        let data,discard = if Exists(data,key) then RemoveImpl (data,key) else data,[]
        let data = Add(data,key,value)
        AssignWithStrength(data,discard) // This will remove extras 

    member al.Remove(key) = 
        let data = FilterAndHold()
        let newdata,discard = RemoveImpl (data,key)
        AssignWithStrength(newdata,discard)

    member al.MostRecent : ('TKey*'TValue) option=  
        let data = FilterAndHold()
        if not data.IsEmpty then 
           // Non-optimal reverse list to get most recent. Consider an array of option for the data structure.
           Some(data |> List.rev |> List.head)
        else None        

    member al.Clear() =
       let discards = FilterAndHold()
       AssignWithStrength([], discards)

        

type internal MruCache<'TKey,'TValue>(keepStrongly,compute, areSame, ?isStillValid : 'TKey*'TValue->bool, ?areSameForSubsumption, ?logComputedNewValue, ?logUsedCachedValue, ?onStrongDiscard, ?keepMax) =
        
    /// Default behavior of areSameForSubsumption function is areSame
    let areSameForSubsumption = defaultArg areSameForSubsumption areSame
        
    /// The list of items in the cache. Youngest is at the end of the list.
    /// The choice of order is somewhat aribtrary. If the other way then adding
    /// items would be O(1) and removing O(N).
    let cache = AgedLookup<'TKey,'TValue>(keepStrongly=keepStrongly,areSame=areSameForSubsumption,?onStrongDiscard=onStrongDiscard,?keepMax=keepMax)
        
    /// Whether or not this result value is still valid.
    let isStillValid = defaultArg isStillValid (fun _ -> true)
        
    /// Log a message when a new value is computed.        
    let logComputedNewValue = defaultArg logComputedNewValue ignore
        
    /// Log a message when an existing value was retrieved from cache.
    let logUsedCachedValue =  defaultArg logUsedCachedValue ignore
                
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
            logComputedNewValue(key)
            value        
        match cache.TryGetKeyValue(key) with
        | Some(key', value) -> 
            if areSame(key', key) && isStillValid(key,value) then
                logUsedCachedValue(key)
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
    static member groupByFirst( l : ('TKey * 'TValue) list) : ('TKey * 'TValue list) list =
        let nextIndex = ref 0
        let result = System.Collections.Generic.List<'TKey * System.Collections.Generic.List<'TValue>>()
        let keyToIndex = Dictionary<'TKey,int>(HashIdentity.Structural)
        let indexOfKey(key) =
            match keyToIndex.TryGetValue(key) with
            | true, v -> v
            | false, _ -> 
                keyToIndex.Add(key,!nextIndex)
                nextIndex := !nextIndex + 1
                !nextIndex - 1
            
        for kv in l do 
            let index = indexOfKey(fst kv)
            if index>= result.Count then 
                let k,vs = fst kv,System.Collections.Generic.List<'TValue>()
                vs.Add(snd kv)
                result.Add(k,vs)
            else
                let _,vs = result.[index]
                vs.Add(snd kv)
        
        result |> Seq.map(fun (k,vs) -> k,vs |> List.ofSeq ) |> List.ofSeq

    /// Return each distinct item in the list using reference equality.
    static member referenceDistinct( l : 'T list) : 'T list when 'T : not struct =
        let set = System.Collections.Generic.Dictionary<'T,bool>(HashIdentity.Reference)
        l |> List.iter(fun i->set.Add(i,true))
        set |> Seq.map(fun kv->kv.Key) |> List.ofSeq
