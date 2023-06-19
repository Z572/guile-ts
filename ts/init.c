#include <libguile.h>
#include <tree_sitter/api.h>
#include "init.h"

#if GTS_GC
static inline void* _scm_gc_realloc(void *mem, size_t size) {
  return gts_realloc(mem,size);
}

static inline void* _scm_gc_calloc(size_t num, size_t size) {
  return gts_calloc(num,size);
}

static inline void* _scm_gc_malloc(size_t size) {
  return gts_malloc(size);
}
static inline void _scm_gc_free(void *mem) {
  gts_free(mem);
}
#else

static inline void* _scm_calloc(size_t num, size_t size) {
  return gts_calloc(num,size);
}
#endif


void init_ts() {
#if GTS_GC
  ts_set_allocator(_scm_gc_malloc, _scm_gc_calloc, _scm_gc_realloc, _scm_gc_free);
#else
  ts_set_allocator(gts_malloc, _scm_calloc, gts_realloc, gts_free);
#endif
}
