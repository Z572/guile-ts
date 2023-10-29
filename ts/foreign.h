#ifndef _GTS_FOREIGN_H
#define _GTS_FOREIGN_H
#include <libguile.h>

#define DEFINE_ENUM(n) \
  SCM_SNARF_INIT(scm_c_define(#n, scm_from_uint32(n)); scm_c_export(#n,NULL));
#define foreign_object_ref(o) scm_foreign_object_ref(o, 0)

inline SCM make_foreign_object(SCM type, void *o){
  return scm_call_2(scm_c_private_ref("ts init", "find-or-create-fobject"),
                    type, scm_from_pointer(o, NULL));
};
#endif
