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

// API to the compiler as an incremental service for lexing, parsing,
// type checking and intellisense-like environment-reporting.

namespace Microsoft.FSharp.Compiler.SourceCodeServices

open Microsoft.FSharp.Compiler 
open Microsoft.FSharp.Compiler.Range
open System.Collections.Generic

/// Represents encoded information for the end-of-line continutation of lexing
type (* internal *) LexState = int64

/// A line/column pair
type (* internal *) Position = int * int
/// A start-position/end-position pair
type (* internal *) Range = Position * Position

type TokenColorKind =
    | Default = 0
    | Text = 0
    | Keyword = 1
    | Comment = 2
    | Identifier = 3
    | String = 4
    | UpperIdentifier = 5
    | InactiveCode = 7
    | PreprocessorKeyword = 8
    | Number = 9
    | Operator = 10
    
type TriggerClass =
    | None         = 0x00000000
    | MemberSelect = 0x00000001
    | MatchBraces  = 0x00000002
    | ChoiceSelect = 0x00000004
    | MethodTip    = 0x000000F0
    | ParamStart   = 0x00000010
    | ParamNext    = 0x00000020
    | ParamEnd     = 0x00000040    
    
type TokenCharKind = 
    | Default     = 0x00000000
    | Text        = 0x00000000
    | Keyword     = 0x00000001
    | Identifier  = 0x00000002
    | String      = 0x00000003
    | Literal     = 0x00000004
    | Operator    = 0x00000005
    | Delimiter   = 0x00000006
    | WhiteSpace  = 0x00000008
    | LineComment = 0x00000009
    | Comment     = 0x0000000A    
    
/// Information about a particular token from the tokenizer
type TokenInformation = {
    /// Left column of the token.
    LeftColumn:int;
    /// Right column of the token.
    RightColumn:int;
    ColorClass:TokenColorKind;
    CharClass:TokenCharKind;
    TriggerClass:TriggerClass;
    /// The tag is an integer identifier for the token
    Tag:int
    /// Provides additional information about the token
    TokenName:string }

type Severity = Warning | Error

type ErrorInfo = 
  { StartLine:int
    EndLine:int
    StartColumn:int
    EndColumn:int
    Severity:Severity
    Message:string
    Subcategory:string }
  static member internal CreateFromException : ErrorLogger.PhasedError * bool * bool * range -> ErrorInfo

/// Describe a comment as either a block of text or a file+signature reference into an intellidoc file.
type (* internal *) XmlComment =
    | XmlCommentNone
    | XmlCommentText of string
    | XmlCommentSignature of (*File and Signature*) string * string

/// A single data tip display element
type (* internal *) DataTipElement = 
    | DataTipElementNone
    /// A single type, method, etc with comment.
    | DataTipElement of (* text *) string * XmlComment
    /// For example, a method overload group.
    | DataTipElementGroup of ((* text *) string * XmlComment) list
    /// An error occurred formatting this element
    | DataTipElementCompositionError of string

/// Information for building a data tip box.
type (* internal *) DataTipText = 
    /// A list of data tip elements to display.
    | DataTipText of DataTipElement list  
    
[<Sealed>]
type (* internal *) Declaration =
    member Name : string
    member DescriptionText : DataTipText
    member Glyph : int
    
[<Sealed>]
type (* internal *) DeclarationSet =
    member Items : Declaration array
    
type (* internal *) Param = 
    { Name: string;
      Display: string;
      Description: string }

[<NoEquality; NoComparison>]
type (* internal *) Method = 
    { Description : DataTipText;
      Type: string;
      Parameters: Param array }

[<Sealed>]
type (* internal *) MethodOverloads = 
    member Name: string;
    member Methods: Method array 



type (* internal *) DeclarationItemKind =
|   NamespaceDecl
|   ModuleFileDecl
|   ExnDecl
|   ModuleDecl
|   TypeDecl
|   MethodDecl
|   PropertyDecl
|   FieldDecl
|   OtherDecl
    

/// Represents an item to be displayed in the navigation bar
[<Sealed>]
type (* internal *) DeclarationItem = 
    member Name : string
    member UniqueName : string
    member Glyph : int
    member Kind : DeclarationItemKind
    member Range : Range
    member BodyRange : Range
    member IsSingleTopLevel : bool

/// Represents top-level declarations (that should be in the type drop-down)
/// with nested declarations (that can be shown in the member drop-down)
[<NoEquality; NoComparison>]
type (* internal *) TopLevelDeclaration = 
    { Declaration : DeclarationItem
      Nested : DeclarationItem[] }
      
/// Represents result of 'GetNavigationItems' operation - this contains
/// all the members and currently selected indices. First level correspond to
/// types & modules and second level are methods etc.
[<Sealed>]
type (* internal *) NavigationItems =
    member Declarations : TopLevelDeclaration[]
    
[<NoEquality; NoComparison>]
type (* internal *) FindDeclResult = 
    ///  no identifier at this locn 
    | IdNotFound    
    /// no decl info in this buffer at the moment 
    | NoDeclInfo   
    /// found declaration; return (position-in-file, name-of-file, names-of-referenced-assemblies)
    | DeclFound      of Position * string * (string list)
    /// found declaration but source file doesn't exist; try to generate an .fsi
    | NeedToGenerate of (*filename of .dll*) string * (*name-fixing function*)(string -> string) * (*fully-qualified identifier to goto*)(string list)
     
type (* internal *) Names = string list 
type (* internal *) NamesWithResidue = Names * string 

[<Sealed>]
/// A handle to type information gleaned from typechecking the file. 
type (* internal *) TypeCheckInfo  =
    /// Resolve the names at the given location to a set of declarations
    member GetDeclarations                : Position * string * NamesWithResidue * (*tokentag:*)int -> DeclarationSet
    /// Resolve the names at the given location to give a data tip 
    member GetDataTipText                 : Position * string * Names * (*tokentag:*)int -> DataTipText
    /// Resolve the names at the given location to give F1 keyword
    member GetF1Keyword                   : Position * string * Names -> string option
    // Resolve the names at the given location to a set of methods
    member GetMethods                     : Position * string * Names option * (*tokentag:*)int -> MethodOverloads
    /// Resolve the names at the given location to the declaration location of the corresponding construct
    member GetDeclarationLocation         : Position * string * Names * (*tokentag:*)int * bool -> FindDeclResult
    /// A version of `GetDeclarationLocation` augmented with the option (via the `bool`) parameter to force .fsi generation (even if source exists); this is primarily for testing
    member internal GetDeclarationLocationInternal : bool -> Position * string * Names * (*tokentag:*)int * bool -> FindDeclResult

[<Sealed>]
/// A handle to the results of TypeCheckSource
type (* internal *) TypeCheckResults =
    /// The errors returned by parsing a source file
    member Errors : ErrorInfo array
    /// A handle to type information gleaned from typechecking the file. 
    member TypeCheckInfo: TypeCheckInfo option

[<Sealed>]
type (* internal *) UntypedParseInfo = 
    /// Name of the file for which this information were created
    member FileName                       : string
    /// Get declaraed items and the selected item at the specified location
    member GetNavigationItems             : unit -> NavigationItems
    /// Return the inner-most range associated with a possible breakpoint location
    member ValidateBreakpointLocation : Position -> Range option
    /// When these files change then the build is invalid
    member DependencyFiles : unit -> string list
    
/// This type represents results obtained from parsing, before the type checking is performed
/// It can be used for populating navigation information and for running the 
/// 'TypeCheckSource' method to get the full information.
type (* internal *) UntypedParseResults

type (* internal *) CheckOptions = 
    { 
      ProjectFileName: string;  // JAF: Can this reduce to just project directory? No, because there may be two projects in the same directory.
      ProjectFileNames: string array;
      ProjectOptions: string array;
      /// When true, the typechecking environment is known a priori to be incomplete. 
      /// This can happen, for example, when a .fs file is opened outside of a project.
      /// It may be appropriate, then, to not show error messages related to type checking
      /// since they will just be noise.
      IsIncompleteTypeCheckEnvironment : bool;
      /// When true, use the reference resolution rules for scripts rather than the rules for compiler.
      UseScriptResolutionRules : bool;
    }
         
          
/// Tokenizer for a source file. Holds some expensive-to-compute resources at the scope of the file.
[<Sealed>]
type (* internal *) SourceTokenizer =
    new : string list * string -> SourceTokenizer
    member CreateLineTokenizer : string -> LineTokenizer
    
/// Object to tokenize a line of F# source code, starting with the given lexState.  The lexState should be 0 for
/// the first line of text. Returns an array of ranges of the text and two enumerations categorizing the
/// tokens and characters covered by that range, i.e. TokenColorKind and TokenCharKind.  The enumerations
/// are somewhat adhoc but useful enough to give good colorization options to the user in an IDE.
///
/// A new lexState is also returned.  An IDE-plugin should in general cache the lexState 
/// values for each line of the edited code.
and [<Sealed>] (* internal *) LineTokenizer =
    member StartNewLine : unit -> unit 
    member ScanToken : LexState -> Option<TokenInformation> * LexState
    
/// Information about the compilation environment    
module (* internal *) CompilerEnvironment =
    /// These are the names of assemblies that should be referenced for .fs, .ml, .fsi, .mli files that
    /// are not asscociated with a project.
    val DefaultReferencesForOrphanSources : string list
    /// Return the compilation defines that should be used when editing the given file.
    val GetCompilationDefinesForEditing : filename : string * compilerFlags : string list -> string list
    /// Return true if this is a subcategory of error or warning message that the language service can emit
    val IsCheckerSupportedSubcategory : string -> bool

/// Information about the debugging environment
module (* internal *) DebuggerEnvironment =
    /// Return the language ID, which is the expression evaluator id that the
    /// debugger will use.
    val GetLanguageID : unit -> System.Guid
    
/// This file has become eligible to be re-typechecked.
type (* internal *) FileTypeCheckStateIsDirty = string -> unit
        
/// Identical to _VSFILECHANGEFLAGS in vsshell.idl
type (* internal *) DependencyChangeCode =
    | NoChange = 0x0
    | FileChanged = 0x00000001
    | TimeChanged = 0x00000002
    | Deleted = 0x00000008
    | Added = 0x00000010   
    
/// Callback that indicates whether a requested result has become obsolete.    
[<NoComparison;NoEquality>]
type (* internal *) IsResultObsolete = 
    | IsResultObsolete of (unit->bool)

/// The result of calling TypeCheckResult including the possibility of abort and background compiler not caught up.
[<NoComparison>]
type (* internal *) TypeCheckAnswer =
    | NoAntecedant
    | Aborted // because result was obsolete
    | TypeCheckSucceeded of TypeCheckResults    

[<Sealed>]
[<AutoSerializable(false)>]      
type (* internal *) InteractiveChecker =
    /// Create an instance of an InteractiveChecker.  Currently resources are not reclaimed.
    static member Create : FileTypeCheckStateIsDirty -> InteractiveChecker
    /// Parse a source code file, returning information about brace matching in the file
    /// Return an enumeration of the matching parethetical tokens in the file
    ///
    /// Used to get partial parse information for a file, e.g. for navigation bars. Fairly quick.
    /// Is quick enough to call from the UI thread (?)
    ///
    member MatchBraces : filename : string * source: string * options: CheckOptions -> (Range * Range) array
    /// Parse a source code file, returning a handle that can be used for obtaining navigation bar information
    /// To get the full information, call 'TypeCheckSource' method on the result
    ///
    /// Used to get partial parse information for a file, e.g. for navigation bars. Fairly quick.
    /// Not quite quick enough to call from UI thread (as with all operations in this API), so best to
    /// call this asynchronously via a background thread.
    ///
    member UntypedParse : filename: string *  source: string * options: CheckOptions -> UntypedParseInfo        

    /// Typecheck a source code file, returning a handle to the results of the parse including
    /// the reconstructed types in the file.
    ///
    /// Return None if the background builder is not yet done prepring the type check results for the antecedent to the 
    /// file.
    /// Used to build the CheckOptions to provide for a script. The CheckOptions describes a "project".
    /// The #load, #r settings etc. are an implicit description of a project.
    ///
    /// Used for intellisense if TryGetRecentTypeCheckResultsForFile 
    /// fails and information is essential to complete the operation.
    /// e.g. F1 help, or "exact" intellisense.
    ///
    /// Also used to get the set of errors for red squigglies for a file.
    member TypeCheckSource : parsed: UntypedParseInfo * filename : string * fileversion : int * source: string * options: CheckOptions * isResultObsolete:IsResultObsolete  -> TypeCheckAnswer
    
    /// For a given script file, get the CheckOptions implied by the #load closure
    ///
    /// Used to build the CheckOptions to provide for a script. The CheckOptions describes a "project".
    /// The #load, #r settings etc. are an implicit description of a project.
    member GetCheckOptionsFromScriptRoot : filename: string * source: string -> CheckOptions
        
    /// For QuickSearch index - not used by VS2008/VS2010
    member GetSlotsCount : options : CheckOptions -> int
    /// For QuickSearch index - not used by VS2008/VS2010
    member UntypedParseForSlot : slot:int * options : CheckOptions -> UntypedParseInfo

    /// Try to get recent type check results for a file. This may arbitrarily refuse to return any
    /// results if the InteractiveChecker would like a chance to recheck the file, in which case
    /// UntypedParse and TypeCheckSource should be called. If the source of the file
    /// has changed the results returned by this function may be out of date, though may
    /// still be usable for generating intellsense menus and information.
    ///
    /// Best practice: call from background thread in response to "."
    /// To prototype: call from UI thread in synchronous handling of "."
    member TryGetRecentTypeCheckResultsForFile : filename: string * options:CheckOptions -> (UntypedParseInfo * TypeCheckResults * (*version*)int) option
        
    /// This function is called when the configuration is known to have changed for reasons not encoded in the CheckOptions.
    /// For example, dependent references may have been deleted or created.
    ///
    /// Call when the project settings change. Stops needless work and restarts the background checker.
    member InvalidateConfiguration : options : CheckOptions -> unit    

    /// Begin background compile of antecedent information for the given project.
    member StartBackgroundCompile : CheckOptions -> unit

    /// Stop the background compile of antecedent information. Used on exit.
    member StopBackgroundCompile : unit -> unit

    /// Block until the background compile of antecedent information finishes. For unit testing only!
    member WaitForBackgroundCompile : unit -> unit
    
    /// Report a statistic for testability
    static member GlobalForegroundParseCountStatistic : int

    /// Report a statistic for testability
    static member GlobalForegroundTypeCheckCountStatistic : int

// Note: multiple used of the word "background":
// 
//     VS/MD                         |    FSharp.Compiler.dll
// |---------------------------------|------------------------
// | UI    |  -->  BackgroundThread  |  -->   BackgroundReactor
//
// Note, "background compile of antecedent information" means "the background reactor builds
// an object for each file representing the typecheck environment at the top of that
// file".



// These functions determine all declarations, called by fsi.fs for fsi-server requests.
module internal FsiIntelisense =
    val getDeclarations : Build.TcConfig * Env.TcGlobals * Build.TcImports * Build.TcState -> string -> string array -> (string * string * string * int) array

module internal TestHooks =
    val FormatOverloadsToListScope                   : (DataTipElement->DataTipElement) -> System.IDisposable
    val EnableFsiGenerationScope                     : unit -> System.IDisposable
    
module internal TestExpose =     
    val TokenInfo                                    : Parser.token -> (TokenColorKind * TokenCharKind * TriggerClass) 

module (* internal *) PrettyNaming =
    val IsIdentifierPartCharacter     : (char -> bool)
    val IsLongIdentifierPartCharacter : (char -> bool)
    val GetLongNameFromString         : (string -> Names)
    // Temporary workaround for no localized resources in FSharp.LanguageService.dll
    val FormatAndOtherOverloadsString : (int -> string)

/// Information about F# source file names
module (* internal *) SourceFile =
   /// Whether or not this file is compilable
   val IsCompilable : string -> bool
   /// Whether or not this file should be a single-file project
   val MustBeSingleFileProject : string -> bool
   /// Is a file generated by the language service?
   val IsFSharpLanguageServiceGenerated : string -> bool

module (* internal *) Flags =
    val init : unit -> unit