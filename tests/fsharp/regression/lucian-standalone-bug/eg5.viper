// This is like eg4.viper, but the race condition
// has been removed through the use of a read-writer.

extern activity Tick : void {}
extern activity Reset : important void {}

activity Race : int
{ int count = 0;
  readwrite rw = new readwrite(rcount=0,wcount=0);

  on Tick(void m)
  { lock = {m} begin_write(rw) |
    t = {lock} count := count+1 |
    e = {t} end_write(rw)
  }

  on Reset(important void *m)
  { lock = {m} begin_write(rw) |
    t = {lock} count := 0 |
    e = {t} end_write(rw)
  }

}