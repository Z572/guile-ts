#include <libguile.h>
#include <tree_sitter/api.h>
#include <string.h>

static SCM tsp_type;
static SCM tsl_type;
static SCM tst_type;
static SCM tsq_type;
static SCM tsqc_type;
static SCM tsn_type;
#define ASSERT_TSP(o) scm_assert_foreign_object_type(tsp_type, o)
#define ASSERT_TSL(o) scm_assert_foreign_object_type(tsl_type, o)
#define ASSERT_TST(o) scm_assert_foreign_object_type(tst_type, o)
#define ASSERT_TSN(o) scm_assert_foreign_object_type(tsn_type, o)
#define ASSERT_TSQ(o) scm_assert_foreign_object_type(tsq_type, o)
#define ASSERT_TSQC(o) scm_assert_foreign_object_type(tsqc_type, o)
#define FR(o) scm_foreign_object_ref(o, 0)

SCM type_table ;
static inline SCM make_foreign_object(SCM type, void *o) {
  SCM p=scm_from_pointer(o, NULL);
  SCM d=scm_hash_ref(type_table, p, SCM_BOOL_F);
  if (scm_is_false(d)) {
    SCM obj=scm_make_foreign_object_1(type, o);
    scm_hash_set_x(type_table, p, obj);
  return obj;
}
  return d;
}

static void ts_parser_finalizer(SCM scm) { ts_parser_delete(FR(scm)); }

static void ts_tree_finalizer(SCM scm) { ts_tree_delete(FR(scm)); }

void init_ts_parser_type(void) {
  SCM name, slots;
  scm_t_struct_finalize finalizer;
  name = scm_from_utf8_symbol("<%ts-parser>");
  slots = scm_list_1(scm_from_utf8_symbol("%data"));
  finalizer = ts_parser_finalizer;
  tsp_type = scm_make_foreign_object_type(name, slots, finalizer);
}

void init_ts_language_type(void) {
  SCM name, slots;
  scm_t_struct_finalize finalizer;
  name = scm_from_utf8_symbol("<ts-language>");
  slots = scm_list_1(scm_from_utf8_symbol("%data"));
  finalizer = NULL;
  tsl_type = scm_make_foreign_object_type(name, slots, finalizer);
}

void init_ts_tree_type(void) {
  SCM name, slots;
  scm_t_struct_finalize finalizer;
  name = scm_from_utf8_symbol("<ts-tree>");
  slots = scm_list_1(scm_from_utf8_symbol("%data"));
  finalizer = ts_tree_finalizer;
  tst_type = scm_make_foreign_object_type(name, slots, finalizer);
}


typedef struct {
  TSNode node;
} Node;

static SCM make_node(TSNode tsn) {
  Node *node=scm_malloc(sizeof(Node));
  node->node=tsn;
  return make_foreign_object(tsn_type,node);
}

static void node_finalizer(SCM o) { free(FR(o));}
static void init_ts_node_type(void) {
  SCM name, slots;
  scm_t_struct_finalize finalizer;
  name = scm_from_utf8_symbol("<ts-node>");
  slots = scm_list_1(scm_from_utf8_symbol("%data"));
  finalizer = node_finalizer;
  tsn_type = scm_make_foreign_object_type(name, slots, finalizer);
}

SCM_DEFINE_PUBLIC(tsp_new, "%tsp-new", 0, 0, 0, (), "") {
  return scm_from_pointer(ts_parser_new(), NULL);
}

SCM_DEFINE(tsp_set_language, "%tsp-set-language!", 2, 0, 0, (SCM p, SCM l),
           "") {
  ASSERT_TSP(p);
  ASSERT_TSL(l);
  if (!ts_parser_set_language(FR(p), FR(l))) {
    scm_misc_error("ts-language", "set ~a language failed! ~a",
                   scm_list_2(p, l));
  };
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(tsp_language, "%tsp-language", 1, 0, 0, (SCM o), "") {
  ASSERT_TSP(o);
  TSParser *tsp = FR(o);
  const TSLanguage *tsl = ts_parser_language(tsp);
  return tsl ? make_foreign_object(tsl_type, tsl) : SCM_BOOL_F;
}

SCM_DEFINE(tsp_set_timeout, "%tsp-set-timeout!", 2, 0, 0, (SCM p, SCM timeout),
           "") {
  ASSERT_TSP(p);
  ts_parser_set_timeout_micros(FR(p), scm_to_uint64(timeout));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(tsp_timeout_micros, "%tsp-timeout", 1, 0, 0, (SCM o), "") {
  ASSERT_TSP(o);
  TSParser *tsp = FR(o);
  return scm_from_uint64(ts_parser_timeout_micros(tsp));
}

SCM_DEFINE(tsp_reset, "ts-parser-reset", 1, 0, 0, (SCM p), "") {
  ASSERT_TSP(p);
  ts_parser_reset(FR(p));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(tsp_parse_string, "ts-parser-parse-string", 3, 1, 0,
           (SCM p, SCM tree, SCM string, SCM length), "") {
  ASSERT_TSP(p);
  if (scm_to_bool(tree)) {
    (ASSERT_TST(tree));
  };
  char* cstring=scm_to_utf8_string(string);
  TSTree *tst =
      ts_parser_parse_string(FR(p), (scm_to_bool(tree)) ? (FR(tree)) : NULL,
                             cstring,
                             SCM_UNBNDP(length) ? strlen(cstring)
                                                 : scm_to_uint32(length));
  return make_foreign_object(tst_type, tst);
}

/// Tree

SCM_DEFINE(tst_copy, "ts-tree-copy", 1, 0, 0, (SCM o), "") {
  ASSERT_TST(o);
  TSTree *tst = FR(o);
  return make_foreign_object(tst_type, ts_tree_copy(tst));
}

SCM_DEFINE(tst_language, "ts-tree-language", 1, 0, 0, (SCM o), "") {
  ASSERT_TST(o);
  TSTree *tst = FR(o);
  return make_foreign_object(tsl_type, ts_tree_language(tst)) ;
}

SCM_DEFINE(tst_root_node, "ts-tree-root-node", 1, 0, 0, (SCM o), "") {
  ASSERT_TST(o);
  TSTree *tst = FR(o);
  return make_node(ts_tree_root_node(tst));
}

SCM_DEFINE(tsn_type_, "ts-node-type", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_utf8_string(ts_node_type(node->node));
}

void init_ts_api() {
  type_table=scm_make_weak_value_hash_table(scm_from_int(3000));
  init_ts_parser_type();
  init_ts_language_type();
  init_ts_tree_type();
  init_ts_node_type();
  scm_c_define("<ts-language>", tsl_type);
  scm_c_define("<%ts-parser>", tsp_type);
#include "api.x"
}
