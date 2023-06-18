#include "foreign.h"
#include <libguile.h>
#include <tree_sitter/api.h>

SCM type_table;
SCM make_foreign_object(SCM type, void *o) {
  SCM p = scm_from_pointer(o, NULL);
  SCM d = scm_hash_ref(type_table, p, SCM_BOOL_F);
  if (scm_is_false(d)) {
    SCM obj = scm_make_foreign_object_1(type, o);
    scm_hash_set_x(type_table, p, obj);
    return obj;
  }
  return d;
}

SCM make_foreign_object_type(char* cname, scm_t_struct_finalize finalizer) {
  SCM name, slots;
  name = scm_from_utf8_symbol(cname);
  slots = finalizer ? scm_list_1(scm_from_utf8_symbol("%data"))
  : scm_list_2(scm_from_utf8_symbol("%data"),scm_from_utf8_symbol("%freed"));
  return scm_make_foreign_object_type(name, slots, finalizer);
}
