#include <libguile.h>
#include <tree_sitter/api.h>
#include <string.h>

static SCM tsp_type;
static SCM tsl_type;
static SCM tst_type;
static SCM tsq_type;
static SCM tsqc_type;
static SCM tsn_type;
static SCM tsr_type;
#define ASSERT_TSP(o) scm_assert_foreign_object_type(tsp_type, o)
#define ASSERT_TSL(o) scm_assert_foreign_object_type(tsl_type, o)
#define ASSERT_TST(o) scm_assert_foreign_object_type(tst_type, o)
#define ASSERT_TSN(o) scm_assert_foreign_object_type(tsn_type, o)
#define ASSERT_TSQ(o) scm_assert_foreign_object_type(tsq_type, o)
#define ASSERT_TSQC(o) scm_assert_foreign_object_type(tsqc_type, o)
#define ASSERT_TSR(o) scm_assert_foreign_object_type(tsr_type, o)
#define FR(o) scm_foreign_object_ref(o, 0)

static void value_range_error (const char* subr,SCM bad_val, SCM min, SCM max) SCM_NORETURN;
static void
value_range_error (const char* subr, SCM bad_val, SCM min, SCM max)
{
  scm_error (scm_out_of_range_key,
	     subr,
	     "Value out of range ~S to< ~S: ~S",
             scm_list_3 (min, max, bad_val),
             scm_list_1 (bad_val));
}

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

SCM_DEFINE(ref_or_set, "%rf", 2, 0, 0, (SCM type,SCM point),
           "") {
  return make_foreign_object(type,scm_to_pointer(point));
}

static inline SCM make_range(TSRange *range) {
  return make_foreign_object(scm_c_private_ref("ts api", "<ts-range>"), range);
}

static inline TSPoint cons_to_point(SCM cons) {
  TSPoint point = {
    .row= scm_to_uint32(scm_car(cons)),
    .column= scm_to_uint32(scm_cdr(cons))
  };
  return point;
}

static inline SCM point_to_cons(TSPoint p) {
  return scm_cons(scm_from_uint32(p.row),scm_from_uint32(p.column));
}
static void ts_parser_finalizer(SCM scm) { ts_parser_delete(FR(scm)); }

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

void init_ts_range_type(void) {
  SCM name, slots;
  scm_t_struct_finalize finalizer;
  name = scm_from_utf8_symbol("<%ts-range>");
  slots = scm_list_1(scm_from_utf8_symbol("%data"));
  finalizer = NULL;
  tsr_type = scm_make_foreign_object_type(name, slots, finalizer);
  scm_c_define("<%ts-range>", tsr_type);
}

SCM_DEFINE(tsr_start_point, "%tsr-start-point", 1, 0, 0, (SCM o),
           "") {
  ASSERT_TSR(o);
  TSRange *range=FR(o);
  return point_to_cons(range->start_point);
}
SCM_DEFINE(tsr_end_point, "%tsr-end-point", 1, 0, 0, (SCM o),
           "") {
  ASSERT_TSR(o);
  TSRange *range=FR(o);
  return point_to_cons(range->end_point);
}

SCM_DEFINE(tsr_make, "%make-tsr", 0, 0, 0, (),
           "") {
  return scm_from_pointer(scm_calloc(sizeof(TSRange *)),NULL);
}

SCM_DEFINE(tsr_start_byte, "%tsr-start-byte", 1, 0, 0, (SCM o),
           "") {
  TSRange *range=FR(o);
  return scm_from_uint32(range->start_byte);
}
SCM_DEFINE(tsr_end_byte, "%tsr-end-byte", 1, 0, 0, (SCM o),
           "") {
  TSRange *range=FR(o);
  return scm_from_uint32(range->end_byte);
}

SCM_DEFINE(tsr_set_end_point, "%tsr-set-end-point!", 2, 0, 0, (SCM r,SCM o),
           "") {
  ASSERT_TSR(r);
  TSRange *range=FR(r);
  range->end_point=cons_to_point(o);
  return SCM_UNSPECIFIED;
}
SCM_DEFINE(tsr_set_start_point, "%tsr-set-start-point!", 2, 0, 0, (SCM r,SCM o),
           "") {
  ASSERT_TSR(r);
  TSRange *range=FR(r);
  range->start_point=cons_to_point(o);
  return SCM_UNSPECIFIED;
}
SCM_DEFINE(tsr_set_start_byte, "%tsr-set-start-byte!", 2, 0, 0, (SCM r,SCM o),
           "") {
  TSRange *range=FR(r);
  range->start_byte=scm_to_uint32(o);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(tsr_set_end_byte, "%tsr-set-end-byte!", 2, 0, 0, (SCM r,SCM o),
           "") {
  TSRange *range=FR(r);
  range->end_byte=scm_to_uint32(o);
  return SCM_UNSPECIFIED;
}

void init_ts_tree_type(void) {
  SCM name, slots;
  name = scm_from_utf8_symbol("<ts-tree>");
  slots = scm_list_1(scm_from_utf8_symbol("%data"));
  tst_type = scm_make_foreign_object_type(name, slots, NULL);
  scm_c_define("<ts-tree>",tst_type);
}


typedef struct {
  TSNode node;
} Node;


static SCM make_node(TSNode tsn) {
  if (ts_node_is_null(tsn)) {
    return SCM_BOOL_F;
}
  Node *node=scm_malloc(sizeof(Node));
  node->node=tsn;
  return make_foreign_object(tsn_type,node);
}

static void node_finalizer(SCM o) {
  Node *node=FR(o);
  free(node);
}
static void init_ts_node_type(void) {
  SCM name, slots;
  scm_t_struct_finalize finalizer;
  name = scm_from_utf8_symbol("<ts-node>");
  slots = scm_list_1(scm_from_utf8_symbol("%data"));
  finalizer = node_finalizer;
  tsn_type = scm_make_foreign_object_type(name, slots, finalizer);
}
static SCM tstc_type;
typedef struct {
  TSTreeCursor cursor;
} Tcursor;

static void init_ts_tcursor_type(void) {
  SCM name, slots;
  name = scm_from_utf8_symbol("<ts-tree-cursor>");
  slots = scm_list_2(scm_from_utf8_symbol("%data"),
                     scm_from_utf8_symbol("%freed?"));
  tstc_type = scm_make_foreign_object_type(name, slots, NULL);
}
#define ASSERT_TSTC(o, arg, func_name, string)                                 \
  scm_assert_foreign_object_type(tstc_type, o);                                \
  SCM_ASSERT_TYPE(                                                             \
      !(scm_foreign_object_unsigned_ref(cursor, 1)), o,   \
      arg, func_name, string)

static SCM make_tcursor(TSTreeCursor tstc) {
  Tcursor *t=scm_malloc(sizeof(Tcursor));
  t->cursor=tstc;
  SCM ts=make_foreign_object(tstc_type,t);
  scm_foreign_object_unsigned_set_x(ts, 1, false);
  return ts;
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

SCM_DEFINE(tsp_included_ranges, "%tsp-included-ranges", 1, 0, 0, (SCM o),
           "") {
  ASSERT_TSP(o);
  TSParser *tsp = FR(o);
  uint32_t *length=scm_gc_malloc_pointerless(sizeof(uint32_t *), "p");
  TSRange *range=ts_parser_included_ranges(tsp, length);
  SCM list=scm_make_list(scm_from_uint8(0), SCM_UNSPECIFIED);
  for (unsigned i = 0; i < *length; i++) {
    TSRange *r=&range[i];
    list=scm_cons(make_range(r),list);
  }
  return list;
}

SCM_DEFINE(tsp_set_included_ranges, "%tsp-set-included-ranges!", 2, 0, 0, (SCM o,SCM list),
           "")
#define FUNC_NAME s_tsp_set_included_ranges
{
  ASSERT_TSP(o);
  SCM_ASSERT(scm_to_bool(scm_list_p(list)) ,list,SCM_ARG2,FUNC_NAME);
  TSParser *tsp = FR(o);
  uint32_t length=scm_to_uint32(scm_length(list));
  TSRange *range[length];
  for (unsigned i = 0; i < length; i++) {
    SCM n=scm_list_ref(list, scm_from_unsigned_integer(i));
    ASSERT_TSR(n);
    range[i]=FR(n);
  }
  return scm_from_bool(ts_parser_set_included_ranges(FR(o),*range,length));
}
#undef FUNC_NAME

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

SCM_DEFINE(tsp_reset, "ts-parser-reset!", 1, 0, 0, (SCM p), "") {
  ASSERT_TSP(p);
  ts_parser_reset(FR(p));
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(tsp_parse_string, "ts-parser-parse-string", 3, 1, 0,
           (SCM p, SCM tree, SCM string, SCM length), "")
#define FUNC_NAME s_tsp_parse_string
{
  ASSERT_TSP(p);
  if (scm_is_true(tree)) {
    ASSERT_TST(tree);
  };
  char* cstring=scm_to_utf8_string(string);
  uint32_t clength=SCM_UNBNDP(length) ? strlen(cstring) : scm_to_uint32(length);
  if (clength > strlen(cstring)) {
    value_range_error(FUNC_NAME, length,
                      scm_from_uint32(0),
                      scm_from_uint32(strlen(cstring)));
  }
  TSTree *tst =
      ts_parser_parse_string(FR(p), (scm_is_true(tree)) ? (FR(tree)) : NULL,
                             cstring,
                             clength);
  return tst ? make_foreign_object(tst_type, tst) : SCM_BOOL_F;
}
#undef FUNC_NAME

/// Tree

SCM_DEFINE(tst_delete, "ts-tree-delete", 1, 0, 0, (SCM o), "") {
  ASSERT_TST(o);
  TSTree *tst = FR(o);
  ts_tree_delete(tst);
  return SCM_UNSPECIFIED;
}

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

SCM_DEFINE(tsn_string, "ts-node-string", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  char *string=ts_node_string(node->node);
  SCM s_string=scm_from_utf8_string(string);
  free(string);
  return s_string;
}

SCM_DEFINE(tsn_null_p, "ts-node-null?", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_bool(ts_node_is_null(node->node));
}

SCM_DEFINE(tsn_named_p, "ts-node-named?", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_bool(ts_node_is_named(node->node));
}

SCM_DEFINE(tsn_missing_p, "ts-node-missing?", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_bool(ts_node_is_missing(node->node));
}

SCM_DEFINE(tsn_extra_p, "ts-node-extra?", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_bool(ts_node_is_extra(node->node));
}

SCM_DEFINE(tsn_has_changes_p, "ts-node-has-changes?", 1, 0, 0,
           (SCM o), "Check if a syntax node has been edited.") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_bool(ts_node_has_changes(node->node));
}

SCM_DEFINE(tsn_has_error_p, "ts-node-has-error?", 1, 0, 0,
           (SCM o), "Check if a syntax node has been edited.") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_bool(ts_node_has_error(node->node));
}

SCM_DEFINE(tsn_type_, "ts-node-type", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node = FR(o);
  // some node is unamed, e.g. it type is "[" it symbol is #{\x5b;}#
  // so cann't return a symbol.
  return scm_from_utf8_string(ts_node_type(node->node));
}

SCM_DEFINE(tsn_symbol, "ts-node-symbol", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_uint16(ts_node_symbol(node->node));
}

SCM_DEFINE(tsn_start_byte, "ts-node-start-byte", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_uint32(ts_node_start_byte(node->node));
}

SCM_DEFINE(tsn_start_point, "ts-node-start-point", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  TSPoint point=ts_node_start_point(node->node);
  return point_to_cons(point);
}

SCM_DEFINE(tsn_end_byte, "ts-node-end-byte", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_uint32(ts_node_end_byte(node->node));
}

SCM_DEFINE(tsn_end_point, "ts-node-end-point", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return point_to_cons(ts_node_end_point(node->node));
}

SCM_DEFINE(tsn_child_count, "ts-node-child-count", 1, 1, 0,
           (SCM o,SCM named), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_uint32(scm_is_true(named)
                         ? ts_node_named_child_count(node->node)
                         : ts_node_child_count(node->node));
}

SCM_DEFINE(tsn_parent, "ts-node-parent", 1, 0, 0, (SCM o), "")
#define FUNC_NAME s_tsn_child
{
  ASSERT_TSN(o);
  Node *node=FR(o);
  TSNode tsn=node->node;
  TSNode root_node=ts_tree_root_node(node->node.tree);
  return make_node(ts_node_parent(tsn));
}
#undef FUNC_NAME


SCM_DEFINE(tsn_child, "ts-node-child", 2, 1, 0, (SCM o,SCM n,SCM named), "")
#define FUNC_NAME s_tsn_child
{
  ASSERT_TSN(o);
  Node *node=FR(o);
  TSNode t_node=node->node;
  bool is_named=scm_is_true(named);
  uint32_t c_n=scm_to_uint32(n);
  {
    uint32_t count =is_named
      ? ts_node_named_child_count(t_node)
      : ts_node_child_count(t_node);

    if (c_n > count) {
      value_range_error(FUNC_NAME, n, scm_from_uint32(0), scm_from_uint32(count));
    }
  }
  return make_node(is_named
                   ? ts_node_named_child(t_node,c_n)
                   : ts_node_child(t_node,c_n));
}
#undef FUNC_NAME

SCM_DEFINE(tsn_field_name_for_child, "ts-node-field-name-for-child", 2, 0, 0,
           (SCM o,SCM n), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  const char *c=ts_node_field_name_for_child(node->node,scm_to_uint32(n));
  return c ? scm_from_utf8_string(c) : SCM_BOOL_F;
}

SCM_DEFINE(tsn_child_by_field_name, "ts-node-child-by-field-name", 2, 1, 0,
           (SCM o ,SCM name,SCM length), "")
#define FUNC_NAME s_tsn_child_by_field_name
{
  ASSERT_TSN(o);
  if (!SCM_UNBNDP(length)) {
    SCM_ASSERT((scm_c_string_length(name) < scm_to_uint32(length)), length, SCM_ARG3, FUNC_NAME);
  }
  Node *node=FR(o);
  return make_node(ts_node_child_by_field_name
                   (node->node,
                    scm_to_utf8_string(name),
                    SCM_UNBNDP(length)
                    ? scm_c_string_length(name)
                    : scm_to_uint32(length)));
}
#undef FUNC_NAME

SCM_DEFINE_PUBLIC(tsn_child_by_field_id, "ts-node-child-by-field-id", 2, 0, 0,
           (SCM o,SCM n), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  TSNode tsn=ts_node_child_by_field_id(node->node,scm_to_uint16(n));
  return make_node(tsn);
}

SCM_DEFINE(tsn_next_sibling, "ts-node-next-sibling", 1, 1, 0,
           (SCM o,SCM named), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);

  TSNode tsn= scm_is_true(named)
    ? ts_node_next_named_sibling(node->node)
    : ts_node_next_sibling(node->node);
  return make_node(tsn);
}
SCM_DEFINE(tsn_prev_sibling, "ts-node-prev-sibling", 2, 0, 0,
           (SCM o,SCM named), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  TSNode tsn= scm_is_true(named)
    ? ts_node_prev_named_sibling(node->node)
    : ts_node_prev_sibling(node->node);
  return make_node(tsn);
}

SCM_DEFINE(tsn_first_child_for_byte, "ts-node-first-child-for-byte", 2, 1, 0,
           (SCM o,SCM n,SCM named), "")
#define FUNC_NAME s_tsn_first_child_for_byte
{
  ASSERT_TSN(o);
  Node *node=FR(o);
  TSNode t_node=node->node;
  uint32_t c_n =scm_to_uint32(n);
  return make_node(scm_is_true(named)
                   ? ts_node_first_named_child_for_byte(t_node,c_n)
                   : ts_node_first_child_for_byte(t_node,c_n));
}
#undef FUNC_NAME

SCM_DEFINE(tsn_eq, "%ts-node-eq?", 2, 0, 0,
           (SCM o,SCM o2), "") {
  ASSERT_TSN(o);
  ASSERT_TSN(o2);
  Node *node=FR(o);
  Node *node2=FR(o2);
  return scm_from_bool(ts_node_eq(node->node, node2->node));
}

SCM_DEFINE(tsl_field_count, "ts-language-field-count", 1, 0, 0,
           (SCM o), "") {
  ASSERT_TSL(o);
  return scm_from_uint32(ts_language_field_count(FR(o)));
}

SCM_DEFINE(tsl_field_name_for_id, "ts-language-field-name-for-id", 2, 0, 0,
           (SCM o,SCM fieldid), "")
#define FUNC_NAME s_tsl_field_name_for_id
{
  ASSERT_TSL(o);
  const TSLanguage *tsl=FR(o);
  uint16_t c_fieldid = scm_to_uint16(fieldid);
  SCM_ASSERT((c_fieldid <= ts_language_field_count(tsl)),
             fieldid, SCM_ARG2, FUNC_NAME);
  SCM_ASSERT((0 < scm_to_uint16(fieldid)),fieldid, SCM_ARG2, FUNC_NAME);
  const char* string=ts_language_field_name_for_id(tsl,c_fieldid);
  return string ? scm_from_utf8_string(string) : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE(tsl_symbol_type, "ts-language-symbol-type", 2, 0, 0,
           (SCM o,SCM n), "") {
  ASSERT_TSL(o);
  return scm_from_uint32(ts_language_symbol_type(FR(o),scm_to_uint16(n)));
}

SCM_DEFINE(tsl_version, "ts-language-version", 1, 0, 0,
           (SCM o), "") {
  ASSERT_TSL(o);
  return scm_from_uint32(ts_language_version(FR(o)));
}

SCM_DEFINE(tstc_cursor_new, "ts-tree-cursor-new", 1, 0, 0,
           (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return make_tcursor(ts_tree_cursor_new(node->node));
}

SCM_DEFINE(tstc_cursor_delete, "ts-tree-cursor-delete", 1, 0, 0, (SCM cursor),
           "")
#define FUNC_NAME s_tstc_cursor_delete
{
  ASSERT_TSTC(cursor, SCM_ARG1, FUNC_NAME, "no deleted <ts-tree-cursor>");
  Tcursor *tc = FR(cursor);
  scm_foreign_object_unsigned_set_x(cursor, 1, true);
  ts_tree_cursor_delete(&tc->cursor);
  free(tc);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(tstc_cursor_copy, "ts-tree-cursor-copy", 1, 0, 0, (SCM cursor),
           "")
#define FUNC_NAME s_tstc_cursor_copy
{
  ASSERT_TSTC(cursor, SCM_ARG1, FUNC_NAME, "no deleted <ts-tree-cursor>");
  Tcursor *tc = FR(cursor);
  return make_tcursor(ts_tree_cursor_copy(&tc->cursor));
}
#undef FUNC_NAME

SCM_DEFINE(tstc_cursor_reset, "ts-tree-cursor-reset!", 2, 0, 0,
           (SCM cursor,SCM node),
           "")
#define FUNC_NAME s_tstc_cursor_reset
{
  ASSERT_TSTC(cursor, SCM_ARG1, FUNC_NAME, "freed cursor!");
  ASSERT_TSN(node);
  Tcursor *tc = FR(cursor);
  Node *c_node=FR(node);
  TSNode t_node=c_node->node;
  ts_tree_cursor_reset(&tc->cursor, t_node);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(tstc_current_node, "ts-tree-cursor-current-node", 1, 0, 0,
           (SCM cursor),
           "")
#define FUNC_NAME s_tstc_current_node
{
  ASSERT_TSTC(cursor, SCM_ARG1, FUNC_NAME, "freed cursor!");
  Tcursor *tc = FR(cursor);
  return make_node(ts_tree_cursor_current_node(&tc->cursor));
}
#undef FUNC_NAME

SCM_DEFINE(tstc_current_field_name, "ts-tree-cursor-current-field-name", 1, 0, 0,
           (SCM cursor),
           "")
#define FUNC_NAME s_tstc_current_node
{
  ASSERT_TSTC(cursor, SCM_ARG1, FUNC_NAME, "freed cursor!");
  Tcursor *tc = FR(cursor);
  const char *string=ts_tree_cursor_current_field_name(&tc->cursor);;
  return string ? scm_from_utf8_string(string) : SCM_BOOL_F ;
}
#undef FUNC_NAME

SCM_DEFINE(tstc_goto_parent, "ts-tree-cursor-goto-parent", 1, 0, 0,
           (SCM cursor),
           "")
#define FUNC_NAME s_tstc_goto_parent
{
  ASSERT_TSTC(cursor, SCM_ARG1, FUNC_NAME, "freed cursor!");
  Tcursor *tc = FR(cursor);
  return scm_from_bool(ts_tree_cursor_goto_parent(&tc->cursor));
}
#undef FUNC_NAME
SCM_DEFINE(tstc_goto_first_child, "ts-tree-cursor-goto-first-child", 1, 0, 0,
           (SCM cursor),
           "")
#define FUNC_NAME s_tstc_goto_first_child
{
  ASSERT_TSTC(cursor, SCM_ARG1, FUNC_NAME, "freed cursor!");
  Tcursor *tc = FR(cursor);
  return scm_from_bool(ts_tree_cursor_goto_first_child(&tc->cursor));
}
#undef FUNC_NAME

void init_ts_api_enum() {
#define DEFINE_ENUM(n)   scm_c_define(#n, scm_from_uint32(n)); scm_c_export(#n,NULL)
  DEFINE_ENUM(TSSymbolTypeRegular);
  DEFINE_ENUM(TSSymbolTypeAnonymous);
  DEFINE_ENUM(TSSymbolTypeAuxiliary);
#undef DEFINE_ENUM
}
void init_ts_api() {
  type_table=scm_make_weak_value_hash_table(scm_from_int(3000));
  init_ts_parser_type();
  init_ts_language_type();
  init_ts_tree_type();
  init_ts_node_type();
  init_ts_api_enum();
  init_ts_range_type();
  init_ts_tcursor_type();
  scm_c_define("<ts-language>", tsl_type);
  scm_c_define("<%ts-parser>", tsp_type);
  scm_c_define("<ts-node>",tsn_type);
#ifndef SCM_MAGIC_SNARFER
#include "api.x"
#endif
}
