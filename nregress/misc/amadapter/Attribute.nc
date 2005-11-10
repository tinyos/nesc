interface Attribute<val_t> {
  command val_t get();
  command int set(val_t val);
}
