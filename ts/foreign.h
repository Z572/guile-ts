#ifndef _GTS_FOREIGN_H
#define _GTS_FOREIGN_H
#include <libguile.h>

#define foreign_object_ref(o) scm_foreign_object_ref(o, 0)

#define foreign_object_freed_p(o) \
  (scm_foreign_object_unsigned_ref(o, 1))

#define foreign_object_set_freed(o,b) \
  (scm_foreign_object_unsigned_set_x(o, 1, b))

SCM make_foreign_object(SCM type, void *o);
SCM make_foreign_object_type(char* cname, scm_t_struct_finalize finalizer);
#endif
