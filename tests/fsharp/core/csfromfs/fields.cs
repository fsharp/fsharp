
using System; using System.Diagnostics;

namespace CSharpFields
{
    public abstract class ByrefCallback { public abstract void Callback(ref S arg); }

    public class Helpers { static public void CallByrefCallback(ref S arg, ByrefCallback cb) { cb.Callback(ref arg); } }


    public struct EmptyStruct { }

    public struct S
    {
        private int			intPropertyField;
        public int			intProperty { get { return intPropertyField; } 
                                                      set { intPropertyField = value; } }

        public int			intField;
        public Object		objectField;
        public int[]		arrayField;
        public EmptyStruct	structField;
        static public int			intFieldStatic;
        static public Object		objectFieldStatic;
        static public int[]		arrayFieldStatic;
        static public EmptyStruct	structFieldStatic;
        public const int			intFieldConst = 3;
        public const byte			byteFieldConst = 4;
        public const char			charFieldConst = 'a';
        public const string			stringFieldConst = "help";
        public const float			singleFieldConst = 3.14F;
        public const double			doubleFieldConst = 3.14;
        /*	static public Object		objectFieldStatic;
                static public int[]		arrayFieldStatic;
                static public EmptyStruct	structFieldStatic; */
    }
    
    public class C
    {

	public int			intField;
	public Object		objectField;
	public int[]		arrayField;
	public EmptyStruct	structField;
	static public int			intFieldStatic;
	static public Object		objectFieldStatic;
	static public int[]		arrayFieldStatic;
	static public EmptyStruct	structFieldStatic;
    }
}
