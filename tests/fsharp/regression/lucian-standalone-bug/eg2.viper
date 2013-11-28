// This code exercises "multiwire-out" for services

class Success {string s;}
class Failure {string f;}
extern int rand(int min,int max);
extern void print(string);

extern activity Test : void
{
}

activity Multiwire
{
  on Test(void m)
  { t = {m} call Multiwire.Spurious(void) |
    if (t:Success) then b1=t.s
    else if (t:Failure) then b2=t.f |
    dummy1 = {b1} external print("hello") |
    dummy2 = {b2} external print("there")
  }
  
  on Spurious(void *m):Success+Failure
  { i = {m} external rand(0,2) |
    if (i==0) then b1=void
    else b2=void |
    return {b1} new Success(s="hello") |
    return {b2} new Failure(f="world")
  }

}
