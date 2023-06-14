#include <libguile.h>
#include <tree_sitter/api.h>

static inline void* _scm_gc_realloc(void *mem, size_t size) {
  return scm_gc_realloc(mem, sizeof(mem),size,"ts");
}

static inline void* _scm_gc_calloc(size_t num, size_t size) {
  return scm_gc_calloc(num*size,"ts");
}

static inline void* _scm_gc_malloc(size_t size) {
  return scm_gc_malloc(size, "ts");
}
static inline void _scm_gc_free(void *mem) {
  scm_gc_free(mem,sizeof(mem),"ts");
}

/* void init_ts() { */
/*   ts_set_allocator(scm_malloc, scm_calloc_, scm_realloc, free); */
/* } */

void init_ts() {
  ts_set_allocator(_scm_gc_malloc, _scm_gc_calloc, _scm_gc_realloc, _scm_gc_free);
}
