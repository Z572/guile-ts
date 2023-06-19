#ifndef _GTS_INIT_H
#define _GTS_INIT_H

#include <libguile.h>
#define GTS_GC 0
#if GTS_GC
#define gts_free(mem) scm_gc_free(mem,sizeof(mem),"ts");
#define gts_malloc(size) scm_gc_malloc(size, "ts")
#define gts_calloc(num,size) scm_gc_calloc(num*size, "ts")
#define gts_realloc(mem,size) scm_gc_realloc(mem,sizeof(mem),size, "ts")
#else
#define gts_free free
#define gts_malloc scm_malloc
#define gts_calloc(num,size) scm_calloc(num*size)
#define gts_realloc scm_realloc
#endif

#endif
