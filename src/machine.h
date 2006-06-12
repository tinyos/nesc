#ifndef MACHINE_H
#define MACHINE_H

typedef struct {
  size_t size, align;
} machine_type_spec;

typedef struct {
  const char *machine_name;

  void (*handle_option)(const char *opt);

  bool pcc_bitfield_type_matters;
  size_t empty_field_boundary, structure_size_boundary;

  machine_type_spec tptr, tfloat, tdouble, tlong_double, tshort, tint,
    tlong, tlong_long;
  size_t int1_align, int2_align, int4_align, int8_align;
  size_t wchar_t_size, size_t_size;
  bool char_signed, wchar_t_signed;

  cval (*adjust_field_align)(field_declaration fdecl, cval alignment);

  bool (*decl_attribute)(gcc_attribute attr, data_declaration ddecl);
  bool (*tag_attribute)(gcc_attribute attr, tag_declaration tdecl);
  bool (*field_attribute)(gcc_attribute attr, field_declaration fdecl);
  bool (*type_attribute)(gcc_attribute attr, type *t);
  
} machine_spec;

extern machine_spec *target;
extern const char *target_compiler;

bool select_target(const char *targetname);

#endif
