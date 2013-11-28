// This code exercises the reader/writer locking primitive

extern activity TestR1 : void {}
extern activity TestR2 : void {}
extern activity TestW3 : void {}

activity Sync : int
{ readwrite rw = new readwrite(rcount=0, wcount=0);
  int x=0;

  on TestR1(void m)
  { t = {m} begin_read(rw) |
    gx1 = {t} x |
    gx2 = {t} x |
    y = gx1+gx2 |
    t2 = {gx1,gx2} end_read(rw) |
    signal y
  }

  on TestR2(void m)
  { t = {m} begin_read(rw) |
    gx1 = {t} x |
    gx2 = {t} x |
    y = gx1*gx2 |
    t2 = {y} end_read(rw) |
    signal y
  }
  
  on TestW3(void m)
  { t = {m} begin_write(rw) |
    done =  {t} x := x+1 |
    t3 = {done} end_write(rw) |
    signal done
  }

}