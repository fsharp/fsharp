// #Conformance #Multitargeting 


#if FX_AT_LEAST_2_0
let something_in_2_0 : System.Collections.Generic.List<int> = Unchecked.defaultof<_>
#endif

#if FX_AT_LEAST_3_5
let something_in_3_5_not_in_2_0 : System.GCCollectionMode = Unchecked.defaultof<_>
#endif


#if SILVERLIGHT_AT_LEAST_2_0
let something_in_silverlight_2_0 = typeof<System.Collections.Generic.List<int>>
#else
let something_not_in_silverlight_2_0 = typeof<System.Diagnostics.DebuggerTypeProxyAttribute>
#endif
