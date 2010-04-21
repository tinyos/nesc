#ifndef MACHINE_H
#define MACHINE_H

typedef struct {
  size_t size, align;
} machine_type_spec;

struct yystype;

typedef struct {
  const char *machine_name;

  void (*handle_option)(const char *opt);

  bool big_endian, pcc_bitfield_type_matters;
  size_t empty_field_boundary, structure_size_boundary;

  size_t word_size;
  machine_type_spec tptr, tfloat, tdouble, tlong_double, tshort, tint,
    tlong, tlong_long;
  size_t int1_align, int2_align, int4_align, int8_align;
  size_t wchar_t_size, size_t_size;
  bool char_signed, wchar_t_signed;
  char *async_functions_atribute;

  cval (*adjust_field_align)(field_declaration fdecl, cval alignment);

  bool (*decl_attribute)(gcc_attribute attr, data_declaration ddecl);
  bool (*tag_attribute)(gcc_attribute attr, tag_declaration tdecl);
  bool (*field_attribute)(gcc_attribute attr, field_declaration fdecl);
  bool (*type_attribute)(gcc_attribute attr, type *t);

  void (*preinit)(void); /* Immediately after target selection */
  void (*init)(void);    /* After everything else is setup */
  int (*token)(const char *word, int length, struct yystype *lvalp);

  /* A Keil C for 8051 special... */
  declaration (*keilc_definition)(location loc, cstring keyword, cstring name,
				  expression address);

  /* Called once when compilation starts. Should:
     - setup system-specific include paths
     - return name of a file definining system-specific macros
     Targets using gcc can set this field to gcc_global_cpp_init
  */
  const char *(*global_cpp_init)(void);

  /* Called just before preprocessing each file. Modify current.lex.finput
     as needed (eg, add pragma handlers). Can be NULL. */
  void (*file_cpp_init)(void); 
  
} machine_spec;

extern machine_spec *target;
extern const char *target_compiler;

bool select_target(const char *targetname);

#endif
