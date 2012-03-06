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

module internal Microsoft.FSharp.Compiler.UnicodeLexing

//------------------------------------------------------------------
// Functions for Unicode char-based lexing (new code).
//

open Internal.Utilities
open System.IO 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 

open Internal.Utilities.Text.Lexing

type Lexbuf =  LexBuffer<char>

let StringAsLexbuf (s:string) : Lexbuf =
    LexBuffer<char>.FromChars [| for c in s -> c |] 
  
let FunctionAsLexbuf (bufferFiller: char[] * int * int -> int) : Lexbuf =
    LexBuffer<char>.FromFunction bufferFiller 
     
// The choice of 60 retries times 50 ms is not arbitrary. The NTFS FILETIME structure 
// uses 2 second resolution for LastWriteTime. We retry long enough to surpass this threshold 
// plus 1 second. Once past the threshold the incremental builder will be able to retry asynchronously based
// on plain old timestamp checking.
//
// The sleep time of 50ms is chosen so that we can respond to the user more quickly for Intellisense operations.
//
// This is not run on the UI thread for VS but it is on a thread that must be stopped before Intellisense
// can return any result except for pending.
let retryDelayMilliseconds = 50
let numRetries = 60

/// Standard utility to create a Unicode LexBuffer
///
/// One small annoyance is that LexBuffers and not IDisposable. This means 
/// we can't just return the LexBuffer object, since the file it wraps wouldn't
/// get closed when we're finished with the LexBuffer. Hence we return the stream,
/// the reader and the LexBuffer. The caller should dispose the first two when done.
let UnicodeFileAsLexbuf (filename,codePage : int option, retryLocked:bool) :  Lexbuf =
    // Retry multiple times since other processes may be writing to this file.
    let rec getSource retryNumber =
      try 
        // Use the .NET functionality to auto-detect the unicode encoding
        use stream = System.IO.File.NewFileStreamShim(filename) 
        use reader = 
            match codePage with 
            | None -> new  StreamReader(stream,true)
// SILVERLIGHT-TODO: Bug?
#if SILVERLIGHT
            | Some n -> new  StreamReader(stream,System.Text.Encoding.GetEncoding(n.ToString()))             
#else            
            | Some n -> new  StreamReader(stream,System.Text.Encoding.GetEncoding(n)) 
#endif            
        reader.ReadToEnd()
      with 
          // We can get here if the file is locked--like when VS is saving a file--we don't have direct
          // access to the HRESULT to see that this is EONOACCESS.
          | :? System.IO.IOException as err when retryLocked && err.GetType() = typeof<System.IO.IOException> -> 
               // This second check is to make sure the exception is exactly IOException and none of these for example:
               //   DirectoryNotFoundException 
               //   EndOfStreamException 
               //   FileNotFoundException 
               //   FileLoadException 
               //   PathTooLongException
               if retryNumber < numRetries then 
                   System.Threading.Thread.Sleep (retryDelayMilliseconds)
                   getSource (retryNumber + 1)
               else 
                   reraise()
    let source = getSource 0
    let lexbuf = LexBuffer<char>.FromChars(source.ToCharArray())  
    lexbuf
