#ifndef _GTS_FOREIGN_H
#define _GTS_FOREIGN_H
#include <libguile.h>

#define DEFINE_FOREGE_TYPE(c_name, scheme_name, display_name, finalizer)       \
  SCM_SNARF_HERE(SCM c_name)                                            \
  SCM_SNARF_INIT({                                                             \
    SCM _v = make_foreign_object_type(                                         \
        display_name ? display_name : scheme_name, finalizer);                 \
    c_name = _v;                                                               \
    scm_c_define(scheme_name, _v);                                             \
  })
#define DEFINE_ENUM(n) \
  SCM_SNARF_INIT(scm_c_define(#n, scm_from_uint32(n)); scm_c_export(#n,NULL));
#define foreign_object_ref(o) scm_foreign_object_ref(o, 0)

SCM make_foreign_object(SCM type, void *o);
SCM make_foreign_object_type(char* cname, scm_t_struct_finalize finalizer);
#endif
