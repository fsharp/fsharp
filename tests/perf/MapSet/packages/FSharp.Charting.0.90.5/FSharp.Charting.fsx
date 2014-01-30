//----------------------------------------------------------------------------
// Copyright (c) 2002-2013 Microsoft Corporation
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//
// Originally based on https://fsharppowerpack.codeplex.com/SourceControl/changeset/71707
//
// Modified by Tomas Petricek and other contributors under the Apache 2.0 License
//----------------------------------------------------------------------------

#r "System.Windows.Forms.DataVisualization.dll"
#nowarn "211"
#I "../bin"
#I "../../../packages/FSharp.Charting.0.90.5/lib/net40"
#I "../../../packages/FSharp.Charting/lib/net40"
#I "../../packages/FSharp.Charting.0.90.5/lib/net40"
#I "../../packages/FSharp.Charting/lib/net40"
#r "FSharp.Charting.dll"

open FSharp.Charting
module FsiAutoShow = 
    fsi.AddPrinter(fun (ch:FSharp.Charting.ChartTypes.GenericChart) -> ch.ShowChart(); "(Chart)")

