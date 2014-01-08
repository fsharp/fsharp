namespace CSLib
{
    public class Outer
    {
        // protected void OuterProtectedMethod()
        // {
        // }

        // protected int OuterProtectedProperty { get; set; }
        
        public class PublicInner1
        {
            public class ProtectedInnerInner1
            {
            }

            public void PublicMethod()
            {
            }

            protected void ProtectedMethod()
            {
            }

            public int PublicProperty { get; set; }

            protected int ProtectedProperty { get; set; }
        
        }

        protected class ProtectedInner1
        {
            public class ProtectedInnerInner1
            {
            }

            public ProtectedInner1()
            {
            }

            public void PublicMethod()
            {
            }

            protected void ProtectedMethod()
            {
            }

            public int PublicProperty { get; set; }

            protected int ProtectedProperty { get; set; }
        }

        protected class ProtectedInner2 : Outer
        {
            public ProtectedInner2()
            {
            }

            public void PublicMethod()
            {
            }

            protected void ProtectedMethod()
            {
            }

            public int PublicProperty { get; set; }

            protected int ProtectedProperty { get; set; }
        }

        protected class ProtectedInner3
        {
            protected class ProtectedInner
            {
            }
        }
    }
}