#ifndef _GTS_API_H
#define _GTS_API_H
#include <libguile.h>
#include <tree_sitter/api.h>
#define ASSERT_TSL(o) scm_assert_foreign_object_type(tsl_type, o)
#define ASSERT_TSN(o)                                                   \
  scm_assert_foreign_object_type(tsn_type, o);
#define FR(o) foreign_object_ref(o)
#define ASSERT_TST(o) scm_assert_foreign_object_type(tst_type, o); \
    scm_remember_upto_here_1(o)
extern SCM tsp_type;
extern SCM tsl_type;
extern SCM tst_type;
extern SCM tsq_type;
extern SCM tsqc_type;
extern SCM tsn_type;
extern SCM tsr_type;
inline SCM point_to_cons(TSPoint p) {
  return scm_cons(scm_from_uint32(p.row),scm_from_uint32(p.column));
}
inline TSPoint cons_to_point(SCM cons) {
  TSPoint point = {
    .row= scm_to_uint32(scm_car(cons)),
    .column= scm_to_uint32(scm_cdr(cons))
  };
  return point;
}
void value_range_error (const char* subr,SCM bad_val, SCM min, SCM max) SCM_NORETURN;
typedef struct Node Node;
TSNode node_ref(Node *node);
SCM node_tree(TSNode tsn);
SCM make_node(TSNode tsn);
#endif
