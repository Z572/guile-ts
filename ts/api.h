#ifndef _GTS_API_H
#define _GTS_API_H
#include <libguile.h>
#include <tree_sitter/api.h>
#define ASSERT_TSL(o) scm_assert_foreign_object_type(tsl_type, o)
#define ASSERT_TSN(o)                                                   \
  scm_assert_foreign_object_type(tsn_type, o);                          \
  SCM_ASSERT_TYPE(                                                      \
      !(foreign_object_freed_p(node_tree(node_ref((Node*)(foreign_object_ref(o)))))), o,   \
      0, NULL, "node have no delteed tree")
extern SCM tsp_type;
extern SCM tsl_type;
extern SCM tst_type;
extern SCM tsq_type;
extern SCM tsqc_type;
extern SCM tsn_type;
extern SCM tsr_type;
void value_range_error (const char* subr,SCM bad_val, SCM min, SCM max) SCM_NORETURN;
typedef struct Node Node;
TSNode node_ref(Node *node);
SCM node_tree(TSNode tsn);
#endif
