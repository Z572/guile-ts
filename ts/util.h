#ifndef _GTS_API_H
#define _GTS_API_H
#include "foreign.h"
#include <libguile.h>
#include <tree_sitter/api.h>
#define ASSERT_TSL(o) scm_assert_foreign_object_type(scm_c_private_ref("ts language", "<ts-language>"), o)
#define ASSERT_TSN(o) scm_assert_foreign_object_type(scm_c_private_ref("ts tree", "<ts-node>"), o);
#define FR(o) foreign_object_ref(o)
#define ASSERT_TST(o)                                                          \
  scm_assert_foreign_object_type(scm_c_private_ref("ts tree", "<ts-tree>"), o); \
  scm_remember_upto_here_1(o)
#define ASSERT_TSP(o)                                                          \
  scm_assert_foreign_object_type(                                              \
      (scm_c_private_ref("ts parser", "<ts-parser>")), o);                     \
  scm_remember_upto_here_1(o)
#define ASSERT_TSR(o)                                                          \
  scm_assert_foreign_object_type(scm_c_private_ref("ts util", "<ts-range>"), o); \
  scm_remember_upto_here_1(o)

inline SCM point_to_cons(TSPoint p) {
  return scm_cons(scm_from_uint32(p.row), scm_from_uint32(p.column));
}
inline TSPoint cons_to_point(SCM cons) {
  TSPoint point = {.row = scm_to_uint32(scm_car(cons)),
                   .column = scm_to_uint32(scm_cdr(cons))};
  return point;
}
inline SCM make_range(TSRange *range) {
  SCM range_type = scm_c_private_ref("ts util", "<ts-range>");

  return make_foreign_object(range_type, range);
};

void value_range_error(const char *subr, SCM bad_val, SCM min,
                       SCM max) SCM_NORETURN;
typedef struct Node Node;
TSNode node_ref(Node *node);
SCM node_tree(TSNode tsn);
SCM make_node(TSNode tsn);
#endif
