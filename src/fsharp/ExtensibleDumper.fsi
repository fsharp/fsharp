namespace Internal.Utilities.Diagnostic
open System
open System.Diagnostics
open System.Reflection
open System.Collections.Generic

#if EXTENSIBLE_DUMPER
#if DEBUG

type internal ExtensibleDumper =
  class
    new : x:obj -> ExtensibleDumper
    member Debug : string
    static member Dump : o:obj -> string
  end
  
#endif  
#endif
