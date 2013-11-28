
//****************************************************************************
// properties
//****************************************************************************

namespace TypesWithProperties
{
	public interface Interface
	{
		object objectProperty {
			get; 
		}

		string stringProperty {
			get;
		}
	}

	public interface Interface2
	{
		object objectProperty {
			get; 
		}

		string stringProperty {
			get;
		}
	}


	public abstract class AbstractBase
	{
		public abstract int splitProperty {
			get;
			set;
		}

		public string stringProperty {
			get {
				return string.Copy("foo");
			}
		}
	}

	public abstract class DerivedAbstractClass : AbstractBase
	{
		public override int splitProperty {
			set {
				myValue = value;
			}
		}

		protected int myValue;

		public object objectProperty {
			get {
				return null;
			}
		}

		new public string stringProperty() {
			return null;
		}
	}

	public class DerivedClass : DerivedAbstractClass, Interface {
		public override int splitProperty {
			get {
				return myValue;
			}
		}

		new private object objectProperty {
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

	public class ExplicitInterfaceClass : Interface, Interface2
	{
		public object objectProperty {
			get {
				return null;
			}
		}


		public string stringProperty {
			get {
				return string.Copy("foo");
			}
		}

		string Interface.stringProperty {
			get {
				return null;
			}
		}
	}

    public abstract class PartialExplicitInterfaceClass1 : Interface, Interface2
    {

        public abstract object objectProperty { get; }

        string Interface.stringProperty
        {
            get
            {
                return null;
            }
        }
        
        object Interface2.objectProperty
        {
            get
            {
                return null;
            }
        }

        string Interface2.stringProperty
        {
            get
            {
                return null;
            }
        }


    }

}
public struct MyStruct
{
    int x;
    public MyStruct(int x) { this.x = x; }
}


public struct Nested
{
    public int v;
    public void setV(int V) { v = V; }
}

public struct NestedNested
{
    public Nested v;
    public void setV(int V) { v.setV(V); }
}

public class Container
{
    public Nested nested;
    public readonly Nested nested2;
    public NestedNested nestedNested;
    public readonly NestedNested nestedNested2;
    public Container() { }
}


public struct MyStructGeneric<T>
{
    T x;
    public MyStructGeneric(T x) { this.x = x; }
}


public struct NestedGeneric<T>
{
    public T v;
    public void setV(T V) { v = V; }
}

public struct NestedNestedGeneric<T>
{
    public NestedGeneric<T> v;
    public void setV(T V) { v.setV(V); }
}

public class ContainerGeneric<T>
{
    public NestedGeneric<T> nested;
    public readonly NestedGeneric<T> nested2;
    public NestedNestedGeneric<T> nestedNested;
    public readonly NestedNestedGeneric<T> nestedNested2;
    public ContainerGeneric() { }
}


