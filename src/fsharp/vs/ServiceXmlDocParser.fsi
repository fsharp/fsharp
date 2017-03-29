// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Compiler.SourceCodeServices

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Ast

/// Represent an Xml documentation block in source code
type internal XmlDocable =
    | XmlDocable of line:int * indent:int * paramNames:string list

module internal XmlDocComment =
    
    /// if it's a blank XML comment with trailing "<", returns Some (index of the "<"), otherwise returns None
    val isBlank : string -> int option

module internal XmlDocParser =
    /// Get the list of Xml documentation from current source code
    val getXmlDocables : sourceCodeOfTheFile : string * input : Ast.ParsedInput option -> XmlDocable list
    