#ifndef _GTS_FOREIGN_H
#define _GTS_FOREIGN_H
#include <libguile.h>

#define foreign_object_ref(o) scm_foreign_object_ref(o, 0)

SCM make_foreign_object(SCM type, void *o);
SCM make_foreign_object_type(char* cname, scm_t_struct_finalize finalizer);
#endif
