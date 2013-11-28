
using System; using System.Diagnostics;


//****************************************************************************
// user defined operators
//****************************************************************************

namespace UserDefinedOperators
{
        class B
        {
            public int OpIncrement;
            public int OpDecrement;
            public int OpUnaryPlus;
            public int OpUnaryNegation;
            public int OpOnesComplement;
            public int OpAddition;
            public int OpSubtraction;
            public int OpMultiply;
            public int OpDivision;
            public int OpModulus;
            public int OpExclusiveOr;
            public int OpBitwiseAnd;
            public int OpBitwiseOr;
            public int OpLeftShift;
            public int OpRightShift;
            public int OpUnsignedRightShift;
            public int OpEquals;
            public int OpCompare;
        }

        class C : B
        {
                // valid declarations
                public static C operator ++ (C c) { return null; }
                public static C operator -- (C c) { return null; }
                public static int operator + (C c) { return 0; }
                public static int operator - (C c) { return 0; }
                public static int operator ~ (C c) { return 0; }
                public static int operator + (C c, int i) { return 0; }
                public static int operator - (C c, int i) { return 0; }
                public static int operator * (C c, int i) { return 0; }
                public static int operator / (C c, int i) { return 0; }
                public static int operator % (C c, int i) { return 0; }
                public static int operator ^ (C c, int i) { return 0; }
                public static int operator & (C c, int i) { return 0; }
                public static int operator | (C c, int i) { return 0; }
                public static long operator <<  (C c, int i) { return 0; }
                public static long operator >>  (C c, int i) { return 0; }
                
                public static int  operator < (C c, int i) { return 0; }
                public static int  operator > (C c, int i) { return 0; }
                public static int  operator <= (C c, int i) { return 0; }
                public static int  operator >= (C c, int i) { return 0; }
                public static int  operator == (C c, int i) { return 0; }
                public static int  operator != (C c, int i) { return 0; }
        }
}

//****************************************************************************
// readonly modifiers
//****************************************************************************

namespace ReadOnly
{
        struct S
        {
                public int i;
                public int Property {
                        get {
                                return i;
                        }
                        set {
                                i = value;
                        }
                }
        }

        struct S2
        {
                readonly public S readonlyS;
                         public S readwriteS;

                readonly public int readonlyI;

                public int i;
                public S2(int value) {

                                readonlyS.i = value;
                                readonlyS.Property = value;
                                readonlyI = value;
                i = value;
                readwriteS.i = value;
                                int j = readonlyI;
                                j = j + readonlyS.i;
                                j = j + readonlyS.Property;
                                

                }
        }

        class Base
        {
                protected readonly int i;
                public    readonly S s;
                public             S2 s2;

                Base(int q) {
                        i = 5;
                        s.i = 3;
                        s.Property = 2;

                        S2 localS2 = s2;
                        i = i + s2.readonlyS.i;
                        i = i + s2.readonlyS.Property;
                }

                class Nested 
                {
                        Nested(Base b) {
                                // b.i = 5;
                                // b.s.i = 3;
                                // b.s.Property = 2;

                                S2 localS2 = b.s2;
                                // b.i = b.i + b.s2.readonlyS.i;
                                // b.i = b.i + b.s2.readonlyS.Property;
                        }
                }
        }

        class Class
        {
                static void m()
                {
                        S2 s2 = new S2();

                        int i;
                        i = s2.i;

                        i = i + s2.readonlyI;
                        i = i + s2.readonlyS.i;
                        i = i + s2.readonlyS.Property;
                }
        }

}

//****************************************************************************
// user defined conversion operators
//****************************************************************************

namespace Conversions
{
        enum E {}
        delegate void D();
        struct Struct {}
        class Class { public void m() { return; } }

        struct StructConvert
        {
                // to/from another struct
                public static implicit operator Struct (StructConvert s) { return new Struct(); }
                public static explicit operator StructConvert (Struct s) { return new StructConvert(); }

                // to/from a class
                public static implicit operator Class (StructConvert s) { return new Class(); }
                public static explicit operator StructConvert (Class c) { return new StructConvert(); }

                // to/from an enum
                // UNDONE: public static implicit operator E (StructConvert s) { return new E(); }
                public static explicit operator StructConvert (E e) { return new StructConvert(); }

                // to/from a delegate
                public static implicit operator D (StructConvert s) { return new D((new Class()).m); }
                public static explicit operator StructConvert (D d) { return new StructConvert(); }

                // to/from a built in type
                public static implicit operator int (StructConvert s) { return 0; }
                public static explicit operator StructConvert (int i) { return new StructConvert(); }

                // to/from an array of basic type
                public static implicit operator int[] (StructConvert s) { return null; }
                public static explicit operator StructConvert (int[] a) { return new StructConvert(); }

                // to/from an array of reference type
        public static implicit operator Class[,] (StructConvert s) { return null; }
        public static explicit operator StructConvert (Class[,] a) { return new StructConvert(); }

                // UNDONE: to/from a pointer type
        }

        struct ClassConvert
        {
                // to/from another struct
                public static implicit operator Struct (ClassConvert s) { return new Struct(); }
                public static explicit operator ClassConvert (Struct s) { return new ClassConvert(); }

                // to/from a class
                public static implicit operator Class (ClassConvert s) { return new Class(); }
                public static explicit operator ClassConvert (Class c) { return new ClassConvert(); }

                // to/from an enum
                // UNDONE: public static implicit operator E (ClassConvert s) { return new E(); }
                public static explicit operator ClassConvert (E e) { return new ClassConvert(); }

                // to/from a delegate
                public static implicit operator D (ClassConvert s) { return new D((new Class()).m); }
                public static explicit operator ClassConvert (D d) { return new ClassConvert(); }

                // to/from a built in type
                public static implicit operator int (ClassConvert s) { return 0; }
                public static explicit operator ClassConvert (int i) { return new ClassConvert(); }

                // to/from an array of basic type
                public static implicit operator int[] (ClassConvert s) { return null; }
                public static explicit operator ClassConvert (int[] a) { return new ClassConvert(); }

                // to/from an array of reference type
        public static implicit operator Class[,] (ClassConvert s) { return null; }
        public static explicit operator ClassConvert (Class[,] a) { return new ClassConvert(); }

                // UNDONE: to/from a pointer type
        }

        //
        // check hiding, and conversions between class types
        //
        class Base
        {
                public static implicit operator int (Base b) { return 0; }
                public static implicit operator Base (int i) { return null; }

                public static explicit operator Class (Base b) { return null; }
                public static explicit operator Base (Class i) { return null; }
        }

        class Derived1 : Base
        {
                // check hiding
                public static implicit operator int (Derived1 b) { return 0; }
                public static implicit operator Derived1 (int i) { return null; }

                public static explicit operator Class (Derived1 b) { return null; }
                public static explicit operator Derived1 (Class i) { return null; }

                // conversions between classes
                public static implicit operator Derived2 (Derived1 b) { return null; }
                public static explicit operator Derived1 (Derived2 i) { return null; }
        }

        class Derived2 : Base
        {
                // implicit operators hide inherited explicit operators
                public static implicit operator Class (Derived2 b) { return null; }
                public static implicit operator Derived2 (Class i) { return null; }

                // conversions between classes
                public static implicit operator Derived2 (Derived1 b) { return null; }
                public static explicit operator Derived1 (Derived2 i) { return null; }
        }
}

//****************************************************************************
// Lookup in nested classes
//****************************************************************************

namespace Lookup
{
        class Outer
        {
                private class NestedClass
                {
                        private class NestedNestedClass
                        {
                                private class DerivedClass : Outer.NestedClass
                                {
                                }
                        }
                }

                class AnotherNestedClass
                {
                        NestedClass field;

                        object m() { return field; }
                }
        }
}


//****************************************************************************
// Delegates
//****************************************************************************

namespace Delegates
{
                                        delegate void namespaceMemberDelegate();


        public class Base
        {
                public          delegate void publicDelegate();
                private         delegate void privateDelegate();
                protected       delegate void protectedDelegate();
                internal        delegate void internalDelegate();
        internal protected delegate void internalprotectedDelegate();

                public          delegate void newDelegate();
        }

        class Derived : Base
        {
                new                     delegate int newDelegate(int i);

                delegate void MulticastDelegateWithManyArgs(byte a1, char a2, short a3, int a4, long a5, float a6, double a7, bool a8, decimal a9,
                                    ref byte b1, ref char b2, ref short b3, ref int b4, ref long b5, ref float b6, ref double b7, ref bool b8, ref decimal b9,
                                    out byte c1, out char c2, out short c3, out int c4, out long c5, out float c6, out double c7, out bool c8, out decimal c9);

                delegate int DelegateWithManyArgs(byte a1, char a2, short a3, int a4, long a5, float a6, double a7, bool a8, decimal a9,
                                    ref byte b1, ref char b2, ref short b3, ref int b4, ref long b5, ref float b6, ref double b7, ref bool b8, ref decimal b9,
                                    out byte c1, out char c2, out short c3, out int c4, out long c5, out float c6, out double c7, out bool c8, out decimal c9);
        }

        public class DelegateTest
        {
                public void m()
                {
                        Console.WriteLine("DelegateTest.m");
                }
        }
}


//****************************************************************************
// properties
//****************************************************************************

namespace Properties
{
        class Class
        {
                public int property {
                        get {
                                return propertyValue;
                        }
                        set {
                                propertyValue = value;
                        }
                }

                private int propertyValue;
        }

        interface Interface
        {
                Object objectProperty {
                        get; 
                }

                String stringProperty {
                        get;
                }
        }

        interface Interface2
        {
                Object objectProperty {
                        get; 
                }

                String stringProperty {
                        get;
                }
        }

        abstract class AbstractBase
        {
                public abstract int splitProperty {
                        get;
                        set;
                }

                public String stringProperty {
                        get {
                                return String.Copy("foo");
                        }
                }
        }

        abstract class DerivedAbstractClass : AbstractBase
        {
                public override int splitProperty {
                        set {
                                myValue = value;
                        }
                }

                protected int myValue;

                public Object objectProperty {
                        get {
                                return null;
                        }
                }

                new public String stringProperty() {
                        return null;
                }
        }

        class DerivedClass : DerivedAbstractClass, Interface {
                public override int splitProperty {
                        get {
                                return myValue;
                        }
                }

                new private Object objectProperty {
                        get {
                                return null;
                        }
                }

                new public int stringProperty {
                        get {
                                return 0;
                        }
                }
        }

        class ExplicitInterfaceClass : Interface, Interface2
        {
                public Object objectProperty {
                        get {
                                return null;
                        }
                }

                Object Interface2.objectProperty {
                        get {
                                return null;
                        }
                }

                public String stringProperty {
                        get {
                                return String.Copy("foo");
                        }
                }

                String Interface.stringProperty {
                        get {
                                return null;
                        }
                }
        }

}

struct Struct
{
        // fields
        public int field;
        public static int staticField;
        // public int fieldWithInitializer = 3;
        public static int staticFieldWithInitializer = 3;
        private int privateField;
        public static Struct staticStructWithNoInitializer;

        // constructors
        public Struct(int i) { field = 4; privateField = 2; }
        static Struct() { staticField = 1; }

        // methods
        void ImplicitPrivateMethod() { return; }
        public void PublicMethod() { ImplicitPrivateMethod(); return; }
}

struct EmptyStruct
{
}

struct StructImplicitFieldInitializers
{
        public int                      intField;
        public Object           objectField;
        public int[]            arrayField;
        public EmptyStruct      structField;
}


//****************************************************************************
// interfaces
//****************************************************************************
interface EmptyInterface
{
}

interface BaseInterface
{
        void method();
        char bMethod();
}

interface BaseInterface2
{
        int method();
}

interface DerivedInterface : BaseInterface
{
        void method(int i);
        int dMethod();
}

interface MultiplyDerivedInterface : DerivedInterface, EmptyInterface, BaseInterface
{
        int mMethod();
}

class InterfaceClass : Object, MultiplyDerivedInterface
{
        public interface NestedInterface : BaseInterface
        {
        }

        public void method()            { return; }
        public char bMethod()           { return 'a'; }
        public void method(int i)       { return; }
        public int dMethod()            { return 0; }
        public int mMethod()            { return 0; }
}

class DerivedInterfaceClass : InterfaceClass, DerivedInterface, BaseInterface2
{
        void DerivedInterface.method(int i) { return; }
        public new int method() { return 0; }

}

struct InterfaceStruct : DerivedInterface
{
        public interface NestedInterface : InterfaceClass.NestedInterface, MultiplyDerivedInterface
        {
                void method(Object o);
        }

        public void method() { return; }
        public char bMethod() { return 'a'; }
        public void method(int i) { return; }
        public int dMethod() { return 0; }
}

namespace InterfaceHiding
{
        interface Interface
        {
                void method();
        }

        class Base
        {
                public void method() { return; }
        }

        class Derived1 : Base, Interface
        {
                new private void method() { return; }
        }

        class Derived2 : Base, Interface
        {
                new public int method;
        }

        class Derived3 : Base, Interface
        {
                public void method(int i) { return; }
        }

        class Derived4 : Base, Interface
        {
                new public int method() { return 0; }
        }
}

namespace AbstractMethods
{
        abstract class Base
        {
                public abstract void method();
        }

        abstract class Derived2 : Base
        {
                // this is NOT an error since classes outside
                // this assembly could derive from Derived2
                // in which case they would only see Base.method()
                new internal int method;
        }

        abstract class Foo
        {
                public abstract void m();
        }

        abstract class Bar : Foo
        {
                public override void m() { return; }
        }

        class Gar : Bar
        {
        }
}

namespace MemberSameNameAsParent
{
        namespace MemberSameNameAsParent
        {
                struct MemberSameNameAsParent
                {
                }
        }

        namespace Foo
        {
                class Foo
                {
                }
        }
}

//****************************************************************************
// declare
//****************************************************************************
namespace EmptyNamespace
{
}

namespace Foo
{
    class Bar
        {
        }
}

namespace Bart
{
    internal class Bart : Foo.Bar
        {
        }

        public class PublicClass
        {
        }

        abstract class AbstractClass
        {
        }

        sealed class SealedClass
        {
        }
}

namespace Really.Really.Really.Really.Really.Really.Really.Really.Really.Really.Really.Really.LongDottedNamespace
{
    using DuplicateNamespace;

        class Bar : Foo
        {
        }
}

namespace DuplicateNamespace
{
    class Foo
        {
        }
}

namespace NotSoLong.DottedNamespace
{
    using Foo = DuplicateNamespace.Foo;

    namespace NestedNamespace
        {
                class Bar : Foo
                {
                }
        }

        namespace AnotherNestedNamespace
        {
        }
}

namespace DuplicateNamespace
{
}

namespace Bar
{
    class Foo
        {
                public class PublicClass
                {
                }

                protected class ProtectedClass
                {
                }

        protected internal class ProtectedInternalClass
        {
        }

                private class PrivateClass
                {
                }
        }
}

namespace Baz
{
    using Foo = Bar.Foo;

        class Car : Foo
        {
        }

        class Lar : Bar.Foo
        {
        }
}

class EmptyClass
{
}


class AClass
{
    internal class NestedClass
        {
             internal class NestedNestedClass
                 {
                 }
        }

        public    Object newMember;
        public int NewMethodHidesField;
        public int NewClassHidesField;
        internal  void meth3() {}

        protected void MemberFunc() { return; }
        protected void NewFieldHidesMethod() { return; }
}

//****************************************************************************
// define
//****************************************************************************

abstract class SimpleDerivedClass : AClass, BaseInterface
{
                  int i;
        static    int staticMember;
        public    int publicMember;
        protected int protectedMember;
    internal protected int internalprotectedMember;
        internal  int internalMember;
        private   int privateMember;
    new       int newMember;

        new class NestedClass
        {
        }

        new abstract public int NewMethodHidesField();
        new protected int NewFieldHidesMethod;
        new abstract class NewClassHidesField
        {
        }
//      PEVerify does not allow overloading on ref/out until we can start using CMOD_OPT
//      public    SimpleDerivedClass(ref int i, float j) {  }
        public    SimpleDerivedClass(out int i, float j) { i = 3; }
        public    SimpleDerivedClass(    int i, float j) {  }
        protected SimpleDerivedClass(int i, byte j) { }
        private   SimpleDerivedClass(int i, Object j) {  }
                  SimpleDerivedClass(int i) { }
        protected internal SimpleDerivedClass(byte i, int j) { }

    static    SimpleDerivedClass() { }

    // dotted member function
        void BaseInterface.method()             { }
        char BaseInterface.bMethod()    { return 'a'; }

        void MemberFuncWithArgs(int i, int j, Object k) { }
        void MemberFuncWithManyArgs(byte a1, char a2, short a3, int a4, long a5, float a6, double a7, bool a8, decimal a9,
                                    byte b1, char b2, short b3, int b4, long b5, float b6, double b7, bool b8, decimal b9,
                                    byte c1, char c2, short c3, int c4, long c5, float c6, double c7, bool c8, decimal c9)
                                                                { }

        private   int    OverloadedFunction(int i) { return privateMember; }
        protected void   OverloadedFunction(int i, int j) { }
        public    int    OverloadedFunction() { return 0; }
                  int    OverloadedFunction(float i) { return 1; }
        static           Object OverloadedFunction(Object i) { return null; }

        static new void meth3() {}

        const int ANOTHER_CONSTANT = CONSTANT_VALUE + 1;
    const int CONSTANT_VALUE = 5;

    new void MemberFunc() { return; }

}

class DerivedFromDotted : AClass.NestedClass.NestedNestedClass
{
}


abstract class AbstractClass
{
        protected abstract void AnotherAbstractFunction();

    public abstract int AbstractFunction();
        public virtual int  VirtualFunction() { return 0; }
        protected virtual int  AnotherVirtualFunction() { AnotherAbstractFunction(); return 0; }

        protected int hiddenField;
}

abstract class DerivedClass : AbstractClass
{
    public override int AbstractFunction() { AnotherAbstractFunction(); return 0; }
        public override int VirtualFunction() { return 1; }
        new      void AnotherVirtualFunction() {}
}


