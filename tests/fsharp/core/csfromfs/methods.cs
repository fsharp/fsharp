namespace Methods
{
    public class StaticMethodsWithRefParams
    {
        static public void IntegerOutParam(out int x) { x = 3; }
    }

    public class VirtualMethodsWithRefParams
    {
        virtual public void IntegerOutParam(out int x) { x = 3; }
    }

    public enum E { Red, Green, Blue, Black }
    public struct OneString { public string x; public OneString(string s) { x = s; } }
    public struct OneDouble { public double x; public OneDouble(double s) { x = s; } }

    public class TestMethods<T, U>
    {
        T x;
        public T m0()
        {
            return x;
        }

        public T withlocal_m0()
        {
            T local = x;
            return local;
        }

        public T m1(T x1)
        {
            System.Console.WriteLine(x1);
            return x;
        }

        public T withlocal_m1(T x1)
        {
            T local = x1;
            return local;
        }

        public T inline_m1(T x1)
        {
            return x;
        }

        public T m2(T x1, U x2)
        {
            System.Console.WriteLine(x1);
            System.Console.WriteLine(x2);
            return x;
        }

        public T withlocal_m2(T x1, U x2)
        {
            T local1 = x1;
            U local2 = x2;
            System.Console.WriteLine(local1);
            System.Console.WriteLine(local2);
            return local1;
        }
        public T inline_m2(T x1, U x2)
        {
            return x;
        }
        public T m3<V>(T x1, U x2, V x3)
        {
            System.Console.WriteLine(x1);
            System.Console.WriteLine(x2);
            System.Console.WriteLine(x3);
            return x;
        }
        public T withlocal_m3<V>(T x1, U x2, V x3)
        {
            T local1 = x1;
            U local2 = x2;
            V local3 = x3;
            System.Console.WriteLine(local1);
            System.Console.WriteLine(local2);
            System.Console.WriteLine(local3);
            return local1;
        }

        public T inline_m3<V>(T x1, U x2, V x3)
        {
            return x;
        }
        public T m4<V, W>(T x1, U x2, V x3, W x4)
        {
            System.Console.WriteLine(x1);
            System.Console.WriteLine(x2);
            System.Console.WriteLine(x3);
            System.Console.WriteLine(x4);
            return x;
        }
        public T withlocal_m4<V, W>(T x1, U x2, V x3, W x4)
        {
            T local1 = x1;
            U local2 = x2;
            V local3 = x3;
            W local4 = x4;
            System.Console.WriteLine(local1);
            System.Console.WriteLine(local2);
            System.Console.WriteLine(local3);
            System.Console.WriteLine(local4);
            return local1;
        }
        public T inline_m4<V, W>(T x1, U x2, V x3, W x4)
        {
            return x;
        }
        public T r1(ref T x1)
        {
            System.Console.WriteLine(x1);
            return x;
        }

        public T withlocal_r1(ref T x1)
        {
            T local = x1;
            return local;
        }

        public T inline_r1(ref T x1)
        {
            return x;
        }

        public T r2(ref T x1, ref U x2)
        {
            System.Console.WriteLine(x1);
            System.Console.WriteLine(x2);
            return x;
        }

        public T withlocal_r2(ref T x1, ref U x2)
        {
            T local1 = x1;
            U local2 = x2;
            System.Console.WriteLine(local1);
            System.Console.WriteLine(local2);
            return local1;
        }
        public T inline_r2(ref T x1, ref U x2)
        {
            return x;
        }
        public T r3<V>(ref T x1, ref U x2, ref V x3)
        {
            System.Console.WriteLine(x1);
            System.Console.WriteLine(x2);
            System.Console.WriteLine(x3);
            return x;
        }
        public T withlocal_r3<V>(ref T x1, ref U x2, ref V x3)
        {
            T local1 = x1;
            U local2 = x2;
            V local3 = x3;
            System.Console.WriteLine(local1);
            System.Console.WriteLine(local2);
            System.Console.WriteLine(local3);
            return local1;
        }

        public T inline_r3<V>(ref T x1, ref U x2, ref V x3)
        {
            return x;
        }
        public T r4<V, W>(ref T x1, ref U x2, ref V x3, ref W x4)
        {
            System.Console.WriteLine(x1);
            System.Console.WriteLine(x2);
            System.Console.WriteLine(x3);
            System.Console.WriteLine(x4);
            return x;
        }
        public T withlocal_r4<V, W>(ref T x1, ref U x2, ref V x3, ref W x4)
        {
            T local1 = x1;
            U local2 = x2;
            V local3 = x3;
            W local4 = x4;
            System.Console.WriteLine(local1);
            System.Console.WriteLine(local2);
            System.Console.WriteLine(local3);
            System.Console.WriteLine(local4);
            return local1;
        }
        public T inline_r4<V, W>(ref T x1, ref U x2, ref V x3, ref W x4)
        {
            return x;
        }
    }


    namespace TestLibrary
    {
        public struct TestStruct
        {
            float value;
            public float Value { get { return value; } }

            public TestStruct(float value)
            {
                this.value = value;
            }

            public static TestStruct operator *(float scalar, TestStruct testStruct)
            {
                return new TestStruct(scalar * testStruct.Value);
            }

            public static TestStruct Add(TestStruct a, TestStruct b)
            {
                System.Console.WriteLine("Add by val");
                return new TestStruct(a.Value + b.Value);
            }

            public static void Add(ref TestStruct a, ref TestStruct b, out TestStruct result)
            {
                System.Console.WriteLine("Add by ref");
                result = new TestStruct(a.Value + b.Value);
            }
        }

        public struct JustByVal
        {
            float value;
            public float Value { get { return value; } }

            public JustByVal(float value)
            {
                this.value = value;
            }

            public static JustByVal operator *(float scalar, JustByVal testStruct)
            {
                return new JustByVal(scalar * testStruct.Value);
            }

            public static JustByVal Add(JustByVal a, JustByVal b)
            {
                System.Console.WriteLine("Add by val");
                return new JustByVal(a.Value + b.Value);
            }
        }

        public struct JustByRef
        {
            float value;
            public float Value { get { return value; } }

            public JustByRef(float value)
            {
                this.value = value;
            }

            public static JustByRef operator *(float scalar, JustByRef testStruct)
            {
                return new JustByRef(scalar * testStruct.Value);
            }

            public static void Add(ref JustByRef a, ref JustByRef b, out JustByRef result)
            {
                System.Console.WriteLine("Add by ref");
                result = new JustByRef(a.Value + b.Value);
            }
        }
    }

    public struct TestStruct
    {
        float value;
        public float Value { get { return value; } }

        public TestStruct(float value)
        {
            this.value = value;
        }

        public static TestStruct operator *(float scalar, TestStruct testStruct)
        {
            return new TestStruct(scalar * testStruct.Value);
        }

        public static TestStruct Add(TestStruct a, TestStruct b)
        {
            System.Console.WriteLine("Add by val");
            return new TestStruct(a.Value + b.Value);
        }

        public static void Add(ref TestStruct a, ref TestStruct b, out TestStruct result)
        {
            System.Console.WriteLine("Add by ref");
            result = new TestStruct(a.Value + b.Value);
        }
    }

    public struct JustByVal
    {
        float value;
        public float Value { get { return value; } }

        public JustByVal(float value)
        {
            this.value = value;
        }

        public static JustByVal operator *(float scalar, JustByVal testStruct)
        {
            return new JustByVal(scalar * testStruct.Value);
        }

        public static JustByVal Add(JustByVal a, JustByVal b)
        {
            System.Console.WriteLine("Add by val");
            return new JustByVal(a.Value + b.Value);
        }
    }

    public struct JustByRef
    {
        float value;
        public float Value { get { return value; } }

        public JustByRef(float value)
        {
            this.value = value;
        }

        public static JustByRef operator *(float scalar, JustByRef testStruct)
        {
            return new JustByRef(scalar * testStruct.Value);
        }

        public static void Add(ref JustByRef a, ref JustByRef b, out JustByRef result)
        {
            System.Console.WriteLine("Add by ref");
            result = new JustByRef(a.Value + b.Value);
        }
    }

}



