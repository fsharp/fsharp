/// Some filename operations.    
module internal Internal.Utilities.Filename

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

