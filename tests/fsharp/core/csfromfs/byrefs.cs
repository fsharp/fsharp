



namespace Byrefs
{
   public class Simple
   { 
      public static System.DateTime DateTimeChoice(System.DateTime x, System.DateTime y, bool z) 
       { 
           if (z) return x; else return y; 
       }
      public static System.DateTime DateTimeChoiceRef(ref System.DateTime x, ref System.DateTime y, bool z) 
       { 
           if (z) return x; else return y; 
       }
      public static void DateTimeChoiceOut(ref System.DateTime x, out System.DateTime y) 
       { 
           y = x; 
       }
   }
   public class Generic<T>
   { 

      public T Choice(T x, T y, bool z) 
       { 
           if (z) return x; else return y; 
       }
      public T ChoiceRef(ref T x, ref T y, bool z) 
       { 
           if (z) return x; else return y; 
       }
      public void ChoiceOut(ref T x, out T y) 
       { 
           y = x; 
       }
   }
   public class MakeGenerics
   { 

      public static Generic<int> MakeGenericInt()
       { 
           return new Generic<int>();
       }
   }
	public class ClassWithStaticMethods
	{
            static public void OneIntegerOutParam(out int x) { x = 3; }
	}

	public class ClassWithVirtualMethods
	{
            virtual public void OneIntegerOutParam(out int x) { x = 3; }
            virtual public void TwoIntegerOutParams(out int x, out int y) { x = -1; y = -1; }
	}


   public delegate void DelegateWithOneOutParam(int x, out int y);
   public delegate void DelegateWithTwoOutParams(int x, out int y, out int z);

}



