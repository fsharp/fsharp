// This code has cyclic dependency between activities

activity One : int
{ readwrite rw = new readwrite(rcount=0,wcount=0);

  on Test(void *t1):int
  { t2 = {t1} begin_read(rw) |
    t3 = {t2} call Two.Handle(void) |
    t4 = {t3} end_read(rw) |
    return {t4} 0
  }

  on Back(void *b1):int
  { b2 = {b1} begin_write(rw) |
    b3 = {b2} end_write(rw) |
    return {b3} 0
  }

}


activity Two : int
{ readwrite rw = new readwrite(rcount=0,wcount=0);

  on Handle(void *h1):int
  { h2 = call One.Back(void) |
    return {h2} 0
  }
}