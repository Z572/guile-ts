#ifndef _GTS_API_H
#define _GTS_API_H
#include <libguile.h>
#include <tree_sitter/api.h>
#define ASSERT_TSL(o) scm_assert_foreign_object_type(tsl_type, o)
#define ASSERT_TSN(o)                                                   \
  scm_assert_foreign_object_type(tsn_type, o);
  
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
SCM make_node(TSNode tsn);
#endif
