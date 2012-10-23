
namespace Microsoft.FSharp.Compiler 
    
    module internal SR =
        val GetString : string -> string
        val GetObject : string -> System.Object
            
        
    module internal DiagnosticMessage =
        type ResourceString<'T> =
          new : string * Printf.StringFormat<'T> -> ResourceString<'T>
          member Format : 'T

        val DeclareResourceString : string * Printf.StringFormat<'T> -> ResourceString<'T>