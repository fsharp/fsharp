// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Compiler.SourceCodeServices

[<RequireQualifiedAccess>]
type FSharpGlyph =
    | Class
    | Constant
    | Delegate
    | Enum
    | EnumMember
    | Event
    | Exception
    | Field
    | Interface
    | Method
    | OverridenMethod
    | Module
    | NameSpace
    | Property
    | Struct
    | Typedef
    | Type
    | Union
    | Variable
    | ExtensionMethod
    | Error