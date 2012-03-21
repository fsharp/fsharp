namespace System.Windows.Controls
 
open System
open System.IO
open System.Windows
open System.Windows.Controls
open System.Windows.Input
//open SilverlightContrib.Utilities.ClipboardHelper
open System.Text
 
// A shared base implementation of Stream for 
// use by the console input and output streams
[<AbstractClass>]
type private ConsoleStream(isRead) = 
    inherit Stream() 
    override this.CanRead = isRead
    override this.CanWrite = not isRead
    override this.CanSeek = false
    override this.Position 
        with get() = raise (new NotSupportedException("Console stream does not have a position"))
        and  set(v) = raise (new NotSupportedException("Console stream does not have a position"))
    override this.Length = raise (new NotSupportedException("Console stream does not have a length"))
    override this.Flush() = ()
    override this.Seek(offset, origin) = raise (new NotSupportedException("Console stream cannot seek")) 
    override this.SetLength(v) = raise (new NotSupportedException("Console stream does not have a length")) 
 
/// A control representing a Console window
/// Provides an InputStream and OutputStream
/// for reading an writing character input.
/// Also supports copy/paste on some browsers
type SilverlightConsole() as self = 
    inherit TextBox()
    
    // The queue of user input which has been collected by the 
    // console, but not yet read from the input stream
    let readQueue = new System.Collections.Generic.Queue<int>()
    
    // A stream that reads characters from user input
    let inputStream = 
        { new ConsoleStream(true) with
            override this.Write(buffer,offset,count) = 
                raise (new NotSupportedException("Cannot write from Console input stream")) 
            override this.Read(buffer,offset,count) = 
                do System.Diagnostics.Debug.WriteLine("Starting to read {0} bytes", count)
                let rec waitForAtLeastOneByte() = 
                    let shouldSleep = ref true
                    let ret = ref [||]
                    lock readQueue  (fun () ->
                        shouldSleep := readQueue.Count < 1
                        if not !shouldSleep then 
                            let lengthToRead = min readQueue.Count count
                            ret := Array.init lengthToRead (fun i -> byte (readQueue.Dequeue())))
                    if !shouldSleep
                    then System.Threading.Thread.Sleep(100); waitForAtLeastOneByte()
                    else !ret
                let bytes = waitForAtLeastOneByte()
                System.Array.Copy(bytes, 0, buffer, offset, bytes.Length)
                do System.Diagnostics.Debug.WriteLine("Finished reading {0} bytes", bytes.Length)
                bytes.Length
        }
    
    // A stream that sends character output onto the console screen
    let outputStream = 
        { new ConsoleStream(false) with
            override this.Read(buffer,offset,count) = 
                raise (new NotSupportedException("Cannot read from Console output stream")) 
            override this.Write(buffer,offset,count) = 
                let isDelete = offset < 0
                let newText = 
                    if isDelete 
                    then ""
                    else UnicodeEncoding.UTF8.GetString(buffer, offset, count)
                let _ = self.Dispatcher.BeginInvoke(fun () ->
                    if isDelete then 
                        if self.Text.Length >= count then 
                            self.Text <- self.Text.Substring(0, self.Text.Length - count)
                    else
                        do self.Text <- self.Text + newText
                    do self.SelectionStart <- self.Text.Length
                    do self.SelectionLength <- 0)
                ()
        }
   
    let shiftNumbers = [|')';'!';'@';'#';'$';'%';'^';'&';'*';'('|]
    let currentInputLine = new System.Collections.Generic.List<int>()
    
    // Handles key down events
    // Processes the pressed key and turns it into console input
    // Also echos the pressed key to the console 
    let keyDownHandler(keyArgs : KeyEventArgs) = 
        try
            do keyArgs.Handled <- true
            let shiftDown = Keyboard.Modifiers &&& ModifierKeys.Shift <> (enum 0)
            let ctrlDown = Keyboard.Modifiers &&& ModifierKeys.Control <> (enum 0)
            let p = keyArgs.PlatformKeyCode
            if ctrlDown || keyArgs.Key = Key.Ctrl then
                if keyArgs.Key = Key.V then
                    lock currentInputLine (fun () ->
                        let fromClipboard = Clipboard.GetText()
                        for c in fromClipboard do
                            do currentInputLine.Add(int c)
                            outputStream.WriteByte(byte c)
                            if c = '\n' then
                                for i in currentInputLine do 
                                    do System.Diagnostics.Debug.WriteLine("Enqueued {0}", char i)
                                    do readQueue.Enqueue(i)
                                do currentInputLine.Clear()
                    )
                elif keyArgs.Key = Key.C then
                    let text = self.SelectedText
                    Clipboard.SetText(text)
            else
                System.Diagnostics.Debug.WriteLine("Got key {0} {1} {2}", p, char p, keyArgs.Key)
                let ascii = 
                    match p with
                    | n when n >= 65 && n <= 90 -> if shiftDown then p else p+32
                    | n when n >= 48 && n <= 57 -> if shiftDown then int shiftNumbers.[p-48] else p
                    | 8 -> 8 // backspace
                    | 13 -> int '\n'
                    | 32 -> int ' '
                    | 186 -> if shiftDown then int ':' else int ';'
                    | 187 -> if shiftDown then int '+' else int '='
                    | 188 -> if shiftDown then int '<' else int ','
                    | 189 -> if shiftDown then int '_' else int '-'
                    | 190 -> if shiftDown then int '>' else int '.'
                    | 191 -> if shiftDown then int '?' else int '/'
                    | 192 -> if shiftDown then int '~' else int '`'
                    | 219 -> if shiftDown then int '{' else int '['
                    | 220 -> if shiftDown then int '|' else int '\\'
                    | 221 -> if shiftDown then int '}' else int ']'
                    | 222 -> if shiftDown then int '\"' else int '\''
                    | _ -> -1
                if ascii = 8 then
                    lock currentInputLine (fun () ->
                        if currentInputLine.Count > 0 then currentInputLine.RemoveAt(currentInputLine.Count - 1)
                        outputStream.Write([||], -1, 1)
                    )
                elif ascii > 0 then
                    lock currentInputLine (fun () ->
                        do currentInputLine.Add(ascii)
                        outputStream.WriteByte(byte ascii)
                    )
                if ascii = int '\n' then 
                    lock currentInputLine (fun () ->
                        for i in currentInputLine do 
                            do System.Diagnostics.Debug.WriteLine("Enqueued {0}", char i)
                            if i = 10 then
                                do readQueue.Enqueue(13)
                            do readQueue.Enqueue(i)
                        do currentInputLine.Clear())
                do self.SelectionStart <- self.Text.Length
                do self.SelectionLength <- 0
        with 
        | e -> System.Diagnostics.Debug.WriteLine(e)
    
    // Lazily initialized StreamReader/StreamWriter
    let outReader = lazy (new System.IO.StreamWriter(outputStream, Encoding.UTF8, 256, AutoFlush=true))
    let inReader = lazy (new System.IO.StreamReader(inputStream, Encoding.UTF8, false, 256))
 
    // Manually handle the Return key so we can accept newlines
    do self.AcceptsReturn <- true
    // Make sure a vertical scrollbar appears when needed
    do self.VerticalScrollBarVisibility <- ScrollBarVisibility.Auto
    // Make the control read-only so that users cannot move the cusor or change the contents
    // Unfortunatley, this also greys it out - ideally we could seperate theese two.
    do self.IsReadOnly <- true
    // Hookup the keyDownHandler
    do self.KeyDown.Add(keyDownHandler)
    
    /// The raw input stream for the Console
    member this.InputStream = inputStream :> Stream
    /// The raw ouput stream for the Console
    member this.OutputStream = outputStream :> Stream
    
    /// A StreamWriter for writing to the Console
    member this.Out = outReader.Value
    /// A StreamReader for reading from the Console
    member this.In = inReader.Value