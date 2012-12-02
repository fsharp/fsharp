/// Some filename operations.    
module internal Internal.Utilities.Filename

exception IllegalFileNameChar of string * char

/// "checkSuffix f s" returns true if filename "f" ends in suffix "s",
/// e.g. checkSuffix "abc.fs" ".fs" returns true.
val checkSuffix: string -> string -> bool

/// "chopExtension f" removes the extension from the given
/// filename. Raises ArgumentException if no extension is present.
val chopExtension: string -> string

/// "directoryName" " decomposes a filename into a directory name
val directoryName: string -> string

/// Return true if the filename has a "." extension
val hasExtension: string -> bool

/// Get the filename of the given path
val fileNameOfPath: string -> string

/// Get the filename without extenstion of the given path
val fileNameWithoutExtension: string -> string


