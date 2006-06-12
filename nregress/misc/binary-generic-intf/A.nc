interface A<t> {
  command int request();
  event void done(t val);
}
