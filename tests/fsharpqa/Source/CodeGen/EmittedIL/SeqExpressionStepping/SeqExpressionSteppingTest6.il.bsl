
//  Microsoft (R) .NET Framework IL Disassembler.  Version 4.6.1055.0
//  Copyright (c) Microsoft Corporation.  All rights reserved.



// Metadata version: v4.0.30319
.assembly extern mscorlib
{
  .publickeytoken = (B7 7A 5C 56 19 34 E0 89 )                         // .z\V.4..
  .ver 4:0:0:0
}
.assembly extern FSharp.Core
{
  .publickeytoken = (B0 3F 5F 7F 11 D5 0A 3A )                         // .?_....:
  .ver 4:4:3:0
}
.assembly SeqExpressionSteppingTest6
{
  .custom instance void [FSharp.Core]Microsoft.FSharp.Core.FSharpInterfaceDataVersionAttribute::.ctor(int32,
                                                                                                      int32,
                                                                                                      int32) = ( 01 00 02 00 00 00 00 00 00 00 00 00 00 00 00 00 ) 

  // --- The following custom attribute is added automatically, do not uncomment -------
  //  .custom instance void [mscorlib]System.Diagnostics.DebuggableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggableAttribute/DebuggingModes) = ( 01 00 01 01 00 00 00 00 ) 

  .hash algorithm 0x00008004
  .ver 0:0:0:0
}
.mresource public FSharpSignatureData.SeqExpressionSteppingTest6
{
  // Offset: 0x00000000 Length: 0x000002A4
}
.mresource public FSharpOptimizationData.SeqExpressionSteppingTest6
{
  // Offset: 0x000002A8 Length: 0x000000BA
}
.module SeqExpressionSteppingTest6.exe
// MVID: {5B17FC50-2432-94A2-A745-038350FC175B}
.imagebase 0x00400000
.file alignment 0x00000200
.stackreserve 0x00100000
.subsystem 0x0003       // WINDOWS_CUI
.corflags 0x00000001    //  ILONLY
// Image base: 0x023E0000


// =============== CLASS MEMBERS DECLARATION ===================

.class public abstract auto ansi sealed SeqExpressionSteppingTest6
       extends [mscorlib]System.Object
{
  .custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = ( 01 00 07 00 00 00 00 00 ) 
  .class abstract auto ansi sealed nested public SeqExpressionSteppingTest6
         extends [mscorlib]System.Object
  {
    .custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = ( 01 00 07 00 00 00 00 00 ) 
    .class auto autochar serializable sealed nested assembly beforefieldinit specialname f7@6
           extends class [FSharp.Core]Microsoft.FSharp.Core.CompilerServices.GeneratedSequenceBase`1<int32>
    {
      .custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = ( 01 00 06 00 00 00 00 00 ) 
      .field public int32 x
      .field public int32 x0
      .field public class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> 'enum'
      .custom instance void [mscorlib]System.Diagnostics.DebuggerBrowsableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggerBrowsableState) = ( 01 00 00 00 00 00 00 00 ) 
      .custom instance void [mscorlib]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
      .custom instance void [mscorlib]System.Diagnostics.DebuggerNonUserCodeAttribute::.ctor() = ( 01 00 00 00 ) 
      .field public class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> enum0
      .custom instance void [mscorlib]System.Diagnostics.DebuggerBrowsableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggerBrowsableState) = ( 01 00 00 00 00 00 00 00 ) 
      .custom instance void [mscorlib]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
      .custom instance void [mscorlib]System.Diagnostics.DebuggerNonUserCodeAttribute::.ctor() = ( 01 00 00 00 ) 
      .field public int32 pc
      .custom instance void [mscorlib]System.Diagnostics.DebuggerBrowsableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggerBrowsableState) = ( 01 00 00 00 00 00 00 00 ) 
      .custom instance void [mscorlib]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
      .custom instance void [mscorlib]System.Diagnostics.DebuggerNonUserCodeAttribute::.ctor() = ( 01 00 00 00 ) 
      .field public int32 current
      .custom instance void [mscorlib]System.Diagnostics.DebuggerBrowsableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggerBrowsableState) = ( 01 00 00 00 00 00 00 00 ) 
      .custom instance void [mscorlib]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
      .custom instance void [mscorlib]System.Diagnostics.DebuggerNonUserCodeAttribute::.ctor() = ( 01 00 00 00 ) 
      .method public specialname rtspecialname 
              instance void  .ctor(int32 x,
                                   int32 x0,
                                   class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> 'enum',
                                   class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> enum0,
                                   int32 pc,
                                   int32 current) cil managed
      {
        // Code size       52 (0x34)
        .maxstack  8
        IL_0000:  ldarg.0
        IL_0001:  ldarg.1
        IL_0002:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::x
        IL_0007:  ldarg.0
        IL_0008:  ldarg.2
        IL_0009:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::x0
        IL_000e:  ldarg.0
        IL_000f:  ldarg.3
        IL_0010:  stfld      class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::'enum'
        IL_0015:  ldarg.0
        IL_0016:  ldarg.s    enum0
        IL_0018:  stfld      class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::enum0
        IL_001d:  ldarg.0
        IL_001e:  ldarg.s    pc
        IL_0020:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
        IL_0025:  ldarg.0
        IL_0026:  ldarg.s    current
        IL_0028:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::current
        IL_002d:  ldarg.0
        IL_002e:  call       instance void class [FSharp.Core]Microsoft.FSharp.Core.CompilerServices.GeneratedSequenceBase`1<int32>::.ctor()
        IL_0033:  ret
      } // end of method f7@6::.ctor

      .method public strict virtual instance int32 
              GenerateNext(class [mscorlib]System.Collections.Generic.IEnumerable`1<int32>& next) cil managed
      {
        // Code size       341 (0x155)
        .maxstack  6
        .language '{AB4F38C9-B6E6-43BA-BE3B-58080B2CCCE3}', '{994B45C4-E6E9-11D2-903F-00C04FA302A1}', '{5A869D0B-6611-11D3-BD2A-0000F80849BD}'
        .line 100001,100001 : 0,0 'C:\\GitHub\\dsyme\\visualfsharp\\tests\\fsharpqa\\Source\\CodeGen\\EmittedIL\\SeqExpressionStepping\\SeqExpressionSteppingTest6.fs'
        IL_0000:  ldarg.0
        IL_0001:  ldfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
        IL_0006:  ldc.i4.1
        IL_0007:  sub
        IL_0008:  switch     ( 
                              IL_0023,
                              IL_0025,
                              IL_0027,
                              IL_0029,
                              IL_002b)
        IL_0021:  br.s       IL_0048

        IL_0023:  br.s       IL_002d

        IL_0025:  br.s       IL_0033

        IL_0027:  br.s       IL_0036

        IL_0029:  br.s       IL_003c

        IL_002b:  br.s       IL_0042

        .line 100001,100001 : 0,0 ''
        IL_002d:  nop
        IL_002e:  br         IL_00ad

        .line 100001,100001 : 0,0 ''
        IL_0033:  nop
        IL_0034:  br.s       IL_00a3

        .line 100001,100001 : 0,0 ''
        IL_0036:  nop
        IL_0037:  br         IL_012b

        .line 100001,100001 : 0,0 ''
        IL_003c:  nop
        IL_003d:  br         IL_0121

        .line 100001,100001 : 0,0 ''
        IL_0042:  nop
        IL_0043:  br         IL_014c

        .line 100001,100001 : 0,0 ''
        IL_0048:  nop
        .line 6,8 : 15,25 ''
        IL_0049:  ldarg.0
        IL_004a:  call       class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6::get_es()
        IL_004f:  callvirt   instance class [mscorlib]System.Collections.Generic.IEnumerator`1<!0> class [mscorlib]System.Collections.Generic.IEnumerable`1<int32>::GetEnumerator()
        IL_0054:  stfld      class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::'enum'
        IL_0059:  ldarg.0
        IL_005a:  ldc.i4.1
        IL_005b:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
        .line 6,8 : 15,25 ''
        IL_0060:  ldarg.0
        IL_0061:  ldfld      class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::'enum'
        IL_0066:  callvirt   instance bool [mscorlib]System.Collections.IEnumerator::MoveNext()
        IL_006b:  brfalse.s  IL_00ad

        IL_006d:  ldarg.0
        IL_006e:  ldarg.0
        IL_006f:  ldfld      class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::'enum'
        IL_0074:  callvirt   instance !0 class [mscorlib]System.Collections.Generic.IEnumerator`1<int32>::get_Current()
        IL_0079:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::x
        .line 7,7 : 18,33 ''
        IL_007e:  ldstr      "hello"
        IL_0083:  newobj     instance void class [FSharp.Core]Microsoft.FSharp.Core.PrintfFormat`5<class [FSharp.Core]Microsoft.FSharp.Core.Unit,class [mscorlib]System.IO.TextWriter,class [FSharp.Core]Microsoft.FSharp.Core.Unit,class [FSharp.Core]Microsoft.FSharp.Core.Unit,class [FSharp.Core]Microsoft.FSharp.Core.Unit>::.ctor(string)
        IL_0088:  call       !!0 [FSharp.Core]Microsoft.FSharp.Core.ExtraTopLevelOperators::PrintFormatLine<class [FSharp.Core]Microsoft.FSharp.Core.Unit>(class [FSharp.Core]Microsoft.FSharp.Core.PrintfFormat`4<!!0,class [mscorlib]System.IO.TextWriter,class [FSharp.Core]Microsoft.FSharp.Core.Unit,class [FSharp.Core]Microsoft.FSharp.Core.Unit>)
        IL_008d:  pop
        IL_008e:  ldarg.0
        IL_008f:  ldc.i4.2
        IL_0090:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
        .line 8,8 : 18,25 ''
        IL_0095:  ldarg.0
        IL_0096:  ldarg.0
        IL_0097:  ldfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::x
        IL_009c:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::current
        IL_00a1:  ldc.i4.1
        IL_00a2:  ret

        .line 6,8 : 15,25 ''
        IL_00a3:  ldarg.0
        IL_00a4:  ldc.i4.0
        IL_00a5:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::x
        .line 100001,100001 : 0,0 ''
        IL_00aa:  nop
        IL_00ab:  br.s       IL_0060

        IL_00ad:  ldarg.0
        IL_00ae:  ldc.i4.5
        IL_00af:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
        .line 6,8 : 15,25 ''
        IL_00b4:  ldarg.0
        IL_00b5:  ldfld      class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::'enum'
        IL_00ba:  call       void [FSharp.Core]Microsoft.FSharp.Core.LanguagePrimitives/IntrinsicFunctions::Dispose<class [mscorlib]System.Collections.Generic.IEnumerator`1<int32>>(!!0)
        IL_00bf:  nop
        IL_00c0:  ldarg.0
        IL_00c1:  ldnull
        IL_00c2:  stfld      class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::'enum'
        .line 9,11 : 15,25 ''
        IL_00c7:  ldarg.0
        IL_00c8:  call       class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6::get_es()
        IL_00cd:  callvirt   instance class [mscorlib]System.Collections.Generic.IEnumerator`1<!0> class [mscorlib]System.Collections.Generic.IEnumerable`1<int32>::GetEnumerator()
        IL_00d2:  stfld      class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::enum0
        IL_00d7:  ldarg.0
        IL_00d8:  ldc.i4.3
        IL_00d9:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
        .line 9,11 : 15,25 ''
        IL_00de:  ldarg.0
        IL_00df:  ldfld      class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::enum0
        IL_00e4:  callvirt   instance bool [mscorlib]System.Collections.IEnumerator::MoveNext()
        IL_00e9:  brfalse.s  IL_012b

        IL_00eb:  ldarg.0
        IL_00ec:  ldarg.0
        IL_00ed:  ldfld      class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::enum0
        IL_00f2:  callvirt   instance !0 class [mscorlib]System.Collections.Generic.IEnumerator`1<int32>::get_Current()
        IL_00f7:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::x0
        .line 10,10 : 18,35 ''
        IL_00fc:  ldstr      "goodbye"
        IL_0101:  newobj     instance void class [FSharp.Core]Microsoft.FSharp.Core.PrintfFormat`5<class [FSharp.Core]Microsoft.FSharp.Core.Unit,class [mscorlib]System.IO.TextWriter,class [FSharp.Core]Microsoft.FSharp.Core.Unit,class [FSharp.Core]Microsoft.FSharp.Core.Unit,class [FSharp.Core]Microsoft.FSharp.Core.Unit>::.ctor(string)
        IL_0106:  call       !!0 [FSharp.Core]Microsoft.FSharp.Core.ExtraTopLevelOperators::PrintFormatLine<class [FSharp.Core]Microsoft.FSharp.Core.Unit>(class [FSharp.Core]Microsoft.FSharp.Core.PrintfFormat`4<!!0,class [mscorlib]System.IO.TextWriter,class [FSharp.Core]Microsoft.FSharp.Core.Unit,class [FSharp.Core]Microsoft.FSharp.Core.Unit>)
        IL_010b:  pop
        IL_010c:  ldarg.0
        IL_010d:  ldc.i4.4
        IL_010e:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
        .line 11,11 : 18,25 ''
        IL_0113:  ldarg.0
        IL_0114:  ldarg.0
        IL_0115:  ldfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::x0
        IL_011a:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::current
        IL_011f:  ldc.i4.1
        IL_0120:  ret

        .line 9,11 : 15,25 ''
        IL_0121:  ldarg.0
        IL_0122:  ldc.i4.0
        IL_0123:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::x0
        .line 100001,100001 : 0,0 ''
        IL_0128:  nop
        IL_0129:  br.s       IL_00de

        IL_012b:  ldarg.0
        IL_012c:  ldc.i4.5
        IL_012d:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
        .line 9,11 : 15,25 ''
        IL_0132:  ldarg.0
        IL_0133:  ldfld      class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::enum0
        IL_0138:  call       void [FSharp.Core]Microsoft.FSharp.Core.LanguagePrimitives/IntrinsicFunctions::Dispose<class [mscorlib]System.Collections.Generic.IEnumerator`1<int32>>(!!0)
        IL_013d:  nop
        IL_013e:  ldarg.0
        IL_013f:  ldnull
        IL_0140:  stfld      class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::enum0
        IL_0145:  ldarg.0
        IL_0146:  ldc.i4.5
        IL_0147:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
        IL_014c:  ldarg.0
        IL_014d:  ldc.i4.0
        IL_014e:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::current
        IL_0153:  ldc.i4.0
        IL_0154:  ret
      } // end of method f7@6::GenerateNext

      .method public strict virtual instance void 
              Close() cil managed
      {
        // Code size       189 (0xbd)
        .maxstack  6
        .locals init ([0] class [mscorlib]System.Exception V_0,
                 [1] class [FSharp.Core]Microsoft.FSharp.Core.Unit V_1,
                 [2] class [mscorlib]System.Exception e)
        .line 100001,100001 : 0,0 ''
        IL_0000:  ldarg.0
        IL_0001:  ldfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
        IL_0006:  ldc.i4.5
        IL_0007:  sub
        IL_0008:  switch     ( 
                              IL_0013)
        IL_0011:  br.s       IL_0019

        .line 100001,100001 : 0,0 ''
        IL_0013:  nop
        IL_0014:  br         IL_00b0

        .line 100001,100001 : 0,0 ''
        IL_0019:  nop
        .try
        {
          IL_001a:  ldarg.0
          IL_001b:  ldfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
          IL_0020:  switch     ( 
                                IL_003f,
                                IL_0041,
                                IL_0043,
                                IL_0045,
                                IL_0047,
                                IL_0049)
          IL_003d:  br.s       IL_005d

          IL_003f:  br.s       IL_004b

          IL_0041:  br.s       IL_004e

          IL_0043:  br.s       IL_0051

          IL_0045:  br.s       IL_0054

          IL_0047:  br.s       IL_0057

          IL_0049:  br.s       IL_005a

          .line 100001,100001 : 0,0 ''
          IL_004b:  nop
          IL_004c:  br.s       IL_008a

          .line 100001,100001 : 0,0 ''
          IL_004e:  nop
          IL_004f:  br.s       IL_0076

          .line 100001,100001 : 0,0 ''
          IL_0051:  nop
          IL_0052:  br.s       IL_0075

          .line 100001,100001 : 0,0 ''
          IL_0054:  nop
          IL_0055:  br.s       IL_005f

          .line 100001,100001 : 0,0 ''
          IL_0057:  nop
          IL_0058:  br.s       IL_005e

          .line 100001,100001 : 0,0 ''
          IL_005a:  nop
          IL_005b:  br.s       IL_008a

          .line 100001,100001 : 0,0 ''
          IL_005d:  nop
          .line 100001,100001 : 0,0 ''
          IL_005e:  nop
          IL_005f:  ldarg.0
          IL_0060:  ldc.i4.5
          IL_0061:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
          IL_0066:  ldarg.0
          IL_0067:  ldfld      class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::enum0
          IL_006c:  call       void [FSharp.Core]Microsoft.FSharp.Core.LanguagePrimitives/IntrinsicFunctions::Dispose<class [mscorlib]System.Collections.Generic.IEnumerator`1<int32>>(!!0)
          IL_0071:  nop
          .line 100001,100001 : 0,0 ''
          IL_0072:  nop
          IL_0073:  br.s       IL_008a

          .line 100001,100001 : 0,0 ''
          IL_0075:  nop
          IL_0076:  ldarg.0
          IL_0077:  ldc.i4.5
          IL_0078:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
          IL_007d:  ldarg.0
          IL_007e:  ldfld      class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::'enum'
          IL_0083:  call       void [FSharp.Core]Microsoft.FSharp.Core.LanguagePrimitives/IntrinsicFunctions::Dispose<class [mscorlib]System.Collections.Generic.IEnumerator`1<int32>>(!!0)
          IL_0088:  nop
          .line 100001,100001 : 0,0 ''
          IL_0089:  nop
          IL_008a:  ldarg.0
          IL_008b:  ldc.i4.5
          IL_008c:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
          IL_0091:  ldarg.0
          IL_0092:  ldc.i4.0
          IL_0093:  stfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::current
          IL_0098:  ldnull
          IL_0099:  stloc.1
          IL_009a:  leave.s    IL_00a8

        }  // end .try
        catch [mscorlib]System.Object 
        {
          IL_009c:  castclass  [mscorlib]System.Exception
          IL_00a1:  stloc.2
          .line 6,8 : 15,25 ''
          IL_00a2:  ldloc.2
          IL_00a3:  stloc.0
          IL_00a4:  ldnull
          IL_00a5:  stloc.1
          IL_00a6:  leave.s    IL_00a8

          .line 100001,100001 : 0,0 ''
        }  // end handler
        IL_00a8:  ldloc.1
        IL_00a9:  pop
        .line 100001,100001 : 0,0 ''
        IL_00aa:  nop
        IL_00ab:  br         IL_0000

        IL_00b0:  ldloc.0
        IL_00b1:  ldnull
        IL_00b2:  cgt.un
        IL_00b4:  brfalse.s  IL_00b8

        IL_00b6:  br.s       IL_00ba

        IL_00b8:  br.s       IL_00bc

        .line 100001,100001 : 0,0 ''
        IL_00ba:  ldloc.0
        IL_00bb:  throw

        .line 100001,100001 : 0,0 ''
        IL_00bc:  ret
      } // end of method f7@6::Close

      .method public strict virtual instance bool 
              get_CheckClose() cil managed
      {
        // Code size       78 (0x4e)
        .maxstack  5
        .line 100001,100001 : 0,0 ''
        IL_0000:  ldarg.0
        IL_0001:  ldfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::pc
        IL_0006:  switch     ( 
                              IL_0025,
                              IL_0027,
                              IL_0029,
                              IL_002b,
                              IL_002d,
                              IL_002f)
        IL_0023:  br.s       IL_0043

        IL_0025:  br.s       IL_0031

        IL_0027:  br.s       IL_0034

        IL_0029:  br.s       IL_0037

        IL_002b:  br.s       IL_003a

        IL_002d:  br.s       IL_003d

        IL_002f:  br.s       IL_0040

        .line 100001,100001 : 0,0 ''
        IL_0031:  nop
        IL_0032:  br.s       IL_004c

        .line 100001,100001 : 0,0 ''
        IL_0034:  nop
        IL_0035:  br.s       IL_004a

        .line 100001,100001 : 0,0 ''
        IL_0037:  nop
        IL_0038:  br.s       IL_0048

        .line 100001,100001 : 0,0 ''
        IL_003a:  nop
        IL_003b:  br.s       IL_0046

        .line 100001,100001 : 0,0 ''
        IL_003d:  nop
        IL_003e:  br.s       IL_0044

        .line 100001,100001 : 0,0 ''
        IL_0040:  nop
        IL_0041:  br.s       IL_004c

        .line 100001,100001 : 0,0 ''
        IL_0043:  nop
        IL_0044:  ldc.i4.1
        IL_0045:  ret

        IL_0046:  ldc.i4.1
        IL_0047:  ret

        IL_0048:  ldc.i4.1
        IL_0049:  ret

        IL_004a:  ldc.i4.1
        IL_004b:  ret

        IL_004c:  ldc.i4.0
        IL_004d:  ret
      } // end of method f7@6::get_CheckClose

      .method public strict virtual instance int32 
              get_LastGenerated() cil managed
      {
        .custom instance void [mscorlib]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
        .custom instance void [mscorlib]System.Diagnostics.DebuggerNonUserCodeAttribute::.ctor() = ( 01 00 00 00 ) 
        // Code size       7 (0x7)
        .maxstack  8
        IL_0000:  ldarg.0
        IL_0001:  ldfld      int32 SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::current
        IL_0006:  ret
      } // end of method f7@6::get_LastGenerated

      .method public strict virtual instance class [mscorlib]System.Collections.Generic.IEnumerator`1<int32> 
              GetFreshEnumerator() cil managed
      {
        .custom instance void [mscorlib]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
        .custom instance void [mscorlib]System.Diagnostics.DebuggerNonUserCodeAttribute::.ctor() = ( 01 00 00 00 ) 
        // Code size       12 (0xc)
        .maxstack  10
        IL_0000:  ldc.i4.0
        IL_0001:  ldc.i4.0
        IL_0002:  ldnull
        IL_0003:  ldnull
        IL_0004:  ldc.i4.0
        IL_0005:  ldc.i4.0
        IL_0006:  newobj     instance void SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::.ctor(int32,
                                                                                                             int32,
                                                                                                             class [mscorlib]System.Collections.Generic.IEnumerator`1<int32>,
                                                                                                             class [mscorlib]System.Collections.Generic.IEnumerator`1<int32>,
                                                                                                             int32,
                                                                                                             int32)
        IL_000b:  ret
      } // end of method f7@6::GetFreshEnumerator

    } // end of class f7@6

    .method public specialname static class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> 
            get_es() cil managed
    {
      // Code size       6 (0x6)
      .maxstack  8
      IL_0000:  ldsfld     class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> '<StartupCode$SeqExpressionSteppingTest6>'.$SeqExpressionSteppingTest6::es@4
      IL_0005:  ret
    } // end of method SeqExpressionSteppingTest6::get_es

    .method public static class [mscorlib]System.Collections.Generic.IEnumerable`1<int32> 
            f7() cil managed
    {
      // Code size       12 (0xc)
      .maxstack  8
      .line 6,11 : 9,27 ''
      IL_0000:  ldc.i4.0
      IL_0001:  ldc.i4.0
      IL_0002:  ldnull
      IL_0003:  ldnull
      IL_0004:  ldc.i4.0
      IL_0005:  ldc.i4.0
      IL_0006:  newobj     instance void SeqExpressionSteppingTest6/SeqExpressionSteppingTest6/f7@6::.ctor(int32,
                                                                                                           int32,
                                                                                                           class [mscorlib]System.Collections.Generic.IEnumerator`1<int32>,
                                                                                                           class [mscorlib]System.Collections.Generic.IEnumerator`1<int32>,
                                                                                                           int32,
                                                                                                           int32)
      IL_000b:  ret
    } // end of method SeqExpressionSteppingTest6::f7

    .property class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>
            es()
    {
      .custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = ( 01 00 09 00 00 00 00 00 ) 
      .get class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6::get_es()
    } // end of property SeqExpressionSteppingTest6::es
  } // end of class SeqExpressionSteppingTest6

} // end of class SeqExpressionSteppingTest6

.class private abstract auto ansi sealed '<StartupCode$SeqExpressionSteppingTest6>'.$SeqExpressionSteppingTest6
       extends [mscorlib]System.Object
{
  .field static assembly class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> es@4
  .custom instance void [mscorlib]System.Diagnostics.DebuggerBrowsableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggerBrowsableState) = ( 01 00 00 00 00 00 00 00 ) 
  .field static assembly int32 init@
  .custom instance void [mscorlib]System.Diagnostics.DebuggerBrowsableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggerBrowsableState) = ( 01 00 00 00 00 00 00 00 ) 
  .custom instance void [mscorlib]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
  .custom instance void [mscorlib]System.Diagnostics.DebuggerNonUserCodeAttribute::.ctor() = ( 01 00 00 00 ) 
  .method public static void  main@() cil managed
  {
    .entrypoint
    // Code size       42 (0x2a)
    .maxstack  6
    .locals init ([0] class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> es)
    .line 4,4 : 5,21 ''
    IL_0000:  ldc.i4.1
    IL_0001:  ldc.i4.2
    IL_0002:  ldc.i4.3
    IL_0003:  call       class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0> class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::get_Empty()
    IL_0008:  call       class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0> class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::Cons(!0,
                                                                                                                                                                    class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0>)
    IL_000d:  call       class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0> class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::Cons(!0,
                                                                                                                                                                    class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0>)
    IL_0012:  call       class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0> class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32>::Cons(!0,
                                                                                                                                                                    class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<!0>)
    IL_0017:  dup
    IL_0018:  stsfld     class [FSharp.Core]Microsoft.FSharp.Collections.FSharpList`1<int32> '<StartupCode$SeqExpressionSteppingTest6>'.$SeqExpressionSteppingTest6::es@4
    IL_001d:  stloc.0
    .line 13,13 : 13,31 ''
    IL_001e:  call       class [mscorlib]System.Collections.Generic.IEnumerable`1<int32> SeqExpressionSteppingTest6/SeqExpressionSteppingTest6::f7()
    IL_0023:  call       int32 [FSharp.Core]Microsoft.FSharp.Collections.SeqModule::Length<int32>(class [mscorlib]System.Collections.Generic.IEnumerable`1<!!0>)
    IL_0028:  pop
    IL_0029:  ret
  } // end of method $SeqExpressionSteppingTest6::main@

} // end of class '<StartupCode$SeqExpressionSteppingTest6>'.$SeqExpressionSteppingTest6


// =============================================================

// *********** DISASSEMBLY COMPLETE ***********************