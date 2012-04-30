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

namespace Samples.ConsoleApp

open System
open System.Collections.Generic
open System.IO

/// <summary>
/// Defines a read-only input stream used to feed content to the
/// hosted F# Interactive dynamic compiler.
/// </summary>
/// <remarks>
/// The hosted compiler is handed an instance of this stream and
/// continuously tries to read from it. Content entered by users
/// through the user interface is fed into the stream via its
/// <see cref="Add(string)"/> method.
/// </remarks>
[<AllowNullLiteral>]
type CompilerInputStream() =
    inherit Stream()
        // Duration (in milliseconds) of the pause in the loop of waitForAtLeastOneByte.
    let pauseDuration = 100

        // Queue of characters waiting to be read.
    let readQueue = new Queue<byte>()

    let  waitForAtLeastOneByte(count : int) =
        let rec loop() =
            let attempt =
                lock readQueue (fun () ->
                    let n = readQueue.Count
                    if (n >= 1) then
                        let lengthToRead = if (n < count) then n else count
                        let ret = Array.zeroCreate lengthToRead
                        for i in 0 .. lengthToRead - 1 do
                            ret.[i] <- readQueue.Dequeue()

                        Some ret
                    else
                        None)
            match attempt with
            | None -> System.Threading.Thread.Sleep(pauseDuration); loop()
            | Some res -> res
        loop()

    override x.CanRead = true
    override x.CanWrite = false
    override x.CanSeek = false
    override x.Position with get() = raise (NotSupportedException()) and set v = raise (NotSupportedException())
    override x.Length = raise (NotSupportedException())
    override x.Flush() = ()
    override x.Seek(offset, origin) = raise (NotSupportedException())
    override x.SetLength(value) = raise (NotSupportedException())
    override x.Write(buffer, offset, count) = raise (NotSupportedException("Cannot write to input stream"))
    override x.Read(buffer, offset, count) =
        let bytes = waitForAtLeastOneByte count
        Array.Copy(bytes, 0, buffer, offset, bytes.Length)
        bytes.Length

    /// <summary>
    /// Feeds content into the stream.
    /// </summary>
    /// <remarks>
    /// The content will become available to the client reading from the stream.
    /// </remarks>
    /// <param name="str">The string to append to the end of the stream.</param>
    member x.Add(str:string) =
        if (System.String.IsNullOrEmpty(str)) then () else

        lock readQueue (fun () ->
            let bytes = System.Text.Encoding.UTF8.GetBytes(str)
            for i in 0 .. bytes.Length - 1 do
                readQueue.Enqueue(bytes.[i]))



/// <summary>
/// Defines a write-only stream used to capture output of the hosted F# Interactive dynamic compiler.
/// </summary>
/// <remarks>
/// The hosted compiler is handed an instance of this stream and writes to it.
/// The user interface polls the same instance at regular interval to transfer
/// the stream's accumulated content to the output window of the user interface.
/// </remarks>
[<AllowNullLiteral>]
type CompilerOutputStream()  =
    inherit Stream()
    // Queue of characters waiting to be read.
    let contentQueue = new Queue<byte>()
    let nyi() = raise (NotSupportedException())

    override x.CanRead = false
    override x.CanWrite = true
    override x.CanSeek = false
    override x.Position with get() = nyi() and set v = nyi()
    override x.Length = nyi()
    override x.Flush() = ()
    override x.Seek(offset, origin) = nyi()
    override x.SetLength(value) = nyi()
    override x.Read(buffer, offset, count) = raise (NotSupportedException("Cannot write to input stream"))
    override x.Write(buffer, offset, count) =
        if (buffer = null) then raise (ArgumentNullException("buffer"))

        if (offset < 0) then raise (ArgumentOutOfRangeException("offset"))
        if (count < 0) then raise (ArgumentOutOfRangeException("count"))

        let stop = offset + count
        if (stop > buffer.Length) then raise (ArgumentException("offset,count"))

        lock contentQueue (fun () ->
            for i in offset .. stop - 1 do
                contentQueue.Enqueue(buffer.[i]))

    member x.Read() =
        lock contentQueue (fun () ->
            let n = contentQueue.Count
            if (n > 0) then
                let bytes = Array.zeroCreate n
                for i in 0 .. n-1 do
                    bytes.[i] <- contentQueue.Dequeue()

                System.Text.Encoding.UTF8.GetString(bytes, 0, n)
            else
                "")
