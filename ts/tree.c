#include <libguile.h>
#include <tree_sitter/api.h>
#include <string.h>
#include "init.h"
#include "foreign.h"
#include "util.h"

struct Node {
  TSNode node;
};

TSNode node_ref(Node *node) {
  return node->node;
}

SCM make_node(TSNode tsn) {
  if (ts_node_is_null(tsn)) {
    return SCM_BOOL_F;
}
  Node *node=gts_malloc(sizeof(Node));
  node->node=tsn;
  return make_foreign_object(tsn_type,node);
}

SCM node_tree(TSNode tsn) {
  return make_foreign_object(tst_type, tsn.tree);
}

static void node_finalizer(SCM o) {
  Node *node=FR(o);
  gts_free(node);
}

DEFINE_FOREGE_TYPE(tsn_type,"<ts-node>",NULL,node_finalizer);

typedef struct {
  TSTreeCursor cursor;
} Tcursor;

static void ts_tree_finalizer(SCM scm) {
  ts_tree_delete(FR(scm));
}
DEFINE_FOREGE_TYPE(tst_type,"<ts-tree>",NULL,ts_tree_finalizer);

static void ts_tcursor_finalizer(SCM cursor) {
  Tcursor *tc = FR(cursor);
  ts_tree_cursor_delete(&tc->cursor);
  gts_free(tc);
}
DEFINE_FOREGE_TYPE(tstc_type,"<ts-tree-cursor>",NULL,ts_tcursor_finalizer);
#define ASSERT_TSTC(o)                                 \
  scm_assert_foreign_object_type(tstc_type, o)

static SCM make_tcursor(TSTreeCursor tstc) {
  Tcursor *t=gts_malloc(sizeof(Tcursor));
  t->cursor=tstc;
  SCM ts=make_foreign_object(tstc_type,t);
  return ts;
}

SCM_DEFINE(tst_copy, "ts-tree-copy", 1, 0, 0, (SCM o), "")
#define FUNC_NAME s_tst_copy
{
  ASSERT_TST(o);
  TSTree *tst = FR(o);
  scm_remember_upto_here_1(o);
  return make_foreign_object(tst_type, ts_tree_copy(tst));
}
#undef FUNC_NAME

SCM_DEFINE(tst_language, "ts-tree-language", 1, 0, 0, (SCM o), "")
#define FUNC_NAME s_tst_language
{
  ASSERT_TST(o);
  TSTree *tst = FR(o);
  SCM l=make_foreign_object(tsl_type, ts_tree_language(tst));
  scm_remember_upto_here_1(o);
  return l;
}
#undef FUNC_NAME

SCM_DEFINE(tst_root_node, "%ts-tree-root-node", 1, 0, 0, (SCM o), "")
#define FUNC_NAME s_tst_root_node
{
  ASSERT_TST(o);
  TSTree *tst = FR(o);
  SCM node=make_node(ts_tree_root_node(tst));
  scm_remember_upto_here_1(o);
  return node;
}
#undef FUNC_NAME

SCM_DEFINE(tst_root_node_with_offset, "%ts-tree-root-node-with-offset", 3, 0, 0,
           (SCM o, SCM offset,SCM point), "")
#define FUNC_NAME s_tst_root_node_with_offset
{
  ASSERT_TST(o);
  TSTree *tst = FR(o);
  SCM node;
  scm_remember_upto_here_1(o);
  return make_node(ts_tree_root_node_with_offset(tst, scm_to_uint32(offset),
                                                 cons_to_point(point)));
}
#undef FUNC_NAME

SCM_DEFINE(tst_edit, "%ts-tree-edit", 2, 0, 0,
           (SCM o, SCM edit), "")
#define FUNC_NAME s_tst_edit
{
  ASSERT_TST(o);
  TSTree *tst = FR(o);
  SCM node;
  const TSInputEdit tsie = {
    .start_byte=scm_to_uint32(scm_list_ref(edit, scm_from_uint(0))),
    .old_end_byte=scm_to_uint32(scm_list_ref(edit, scm_from_uint(1))),
    .new_end_byte=scm_to_uint32(scm_list_ref(edit, scm_from_uint(2))),
    .start_point=cons_to_point((scm_list_ref(edit, scm_from_uint(3)))),
    .old_end_point=cons_to_point(scm_list_ref(edit, scm_from_uint(4))),
    .new_end_point=cons_to_point(scm_list_ref(edit, scm_from_uint(5)))

  };
  scm_remember_upto_here_2(o,edit);
  ts_tree_edit(tst, &tsie);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(tst_get_changed_ranges, "ts-tree-get-changed-ranges", 2, 0, 0,
           (SCM tree1, SCM tree2), "")
#define FUNC_NAME s_tst_get_changed_ranges
{
  ASSERT_TST(tree1);
  ASSERT_TST(tree2);
  TSTree *tst1 = FR(tree1);
  TSTree *tst2 = FR(tree2);
  uint32_t length;
  TSRange *ranges=ts_tree_get_changed_ranges(tst1, tst2, &length);
  SCM list=scm_make_list(scm_from_uint32(length), SCM_BOOL_F);
  for (unsigned i = 0; i < length; i++) {
    TSRange *r=&ranges[i];
    scm_list_set_x(list, scm_from_unsigned_integer(i), make_range(r));
  }
  scm_remember_upto_here_2(tree1,tree2);
  return list;
}
#undef FUNC_NAME


SCM_DEFINE(tsn_string, "ts-node-string", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  char *string=ts_node_string(node_ref(node));
  SCM s_string=scm_from_utf8_string(string);
  gts_free(string);
  return s_string;
}

SCM_DEFINE(tsn_null_p, "ts-node-null?", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_bool(ts_node_is_null(node_ref(node)));
}

SCM_DEFINE(tsn_named_p, "ts-node-named?", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_bool(ts_node_is_named(node_ref(node)));
}

SCM_DEFINE(tsn_missing_p, "ts-node-missing?", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_bool(ts_node_is_missing(node_ref(node)));
}

SCM_DEFINE(tsn_extra_p, "ts-node-extra?", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_bool(ts_node_is_extra(node_ref(node)));
}

SCM_DEFINE(tsn_has_changes_p, "ts-node-has-changes?", 1, 0, 0,
           (SCM o), "Check if a syntax node has been edited.") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_bool(ts_node_has_changes(node_ref(node)));
}

SCM_DEFINE(tsn_has_error_p, "ts-node-has-error?", 1, 0, 0,
           (SCM o), "Check if a syntax node has been edited.") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  SCM tree=node_tree(node->node);
  SCM has_error_p=scm_from_bool(ts_node_has_error(node_ref(node)));
  scm_remember_upto_here_2(node,tree);
  return has_error_p;
}

SCM_DEFINE(tsn_type_, "ts-node-type", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node = FR(o);
  // some node is unamed, e.g. it type is "[" it symbol is #{\x5b;}#
  // so cann't return a symbol.
  return scm_from_utf8_string(ts_node_type(node_ref(node)));
}

SCM_DEFINE(tsn_symbol, "ts-node-symbol", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_uint16(ts_node_symbol(node_ref(node)));
}

SCM_DEFINE(tsn_start_byte, "ts-node-start-byte", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_uint32(ts_node_start_byte(node_ref(node)));
}

SCM_DEFINE(tsn_start_point, "ts-node-start-point", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  TSPoint point=ts_node_start_point(node_ref(node));
  return point_to_cons(point);
}

SCM_DEFINE(tsn_end_byte, "ts-node-end-byte", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return scm_from_uint32(ts_node_end_byte(node_ref(node)));
}

SCM_DEFINE(tsn_end_point, "ts-node-end-point", 1, 0, 0, (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return point_to_cons(ts_node_end_point(node_ref(node)));
}

SCM_DEFINE(tsn_child_count, "ts-node-child-count", 1, 1, 0,
           (SCM o,SCM named), "") {
  ASSERT_TSN(o);
  bool c_named=SCM_UNBNDP(named) ? false : scm_to_bool(named);
  Node *node=FR(o);
  return scm_from_uint32(c_named
                         ? ts_node_named_child_count(node_ref(node))
                         : ts_node_child_count(node_ref(node)));
}

SCM_DEFINE(tsn_parent, "ts-node-parent", 1, 0, 0, (SCM o), "")
#define FUNC_NAME s_tsn_child
{
  ASSERT_TSN(o);
  Node *node=FR(o);
  TSNode tsn=node_ref(node);
  TSNode root_node=ts_tree_root_node(node_ref(node).tree);
  return make_node(ts_node_parent(tsn));
}
#undef FUNC_NAME


SCM_DEFINE(tsn_child, "ts-node-child", 2, 1, 0, (SCM o,SCM n,SCM named), "")
#define FUNC_NAME s_tsn_child
{
  ASSERT_TSN(o);
  Node *node=FR(o);
  TSNode t_node=node_ref(node);
  bool is_named=SCM_UNBNDP(named) ? false : scm_to_bool(named);
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
  const char *c=ts_node_field_name_for_child(node_ref(node),scm_to_uint32(n));
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
                   (node_ref(node),
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
  TSNode tsn=ts_node_child_by_field_id(node_ref(node),scm_to_uint16(n));
  return make_node(tsn);
}

SCM_DEFINE(tsn_next_sibling, "ts-node-next-sibling", 1, 1, 0,
           (SCM o,SCM named), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);

  TSNode tsn= scm_is_true(named)
    ? ts_node_next_named_sibling(node_ref(node))
    : ts_node_next_sibling(node_ref(node));
  return make_node(tsn);
}
SCM_DEFINE(tsn_prev_sibling, "ts-node-prev-sibling", 2, 0, 0,
           (SCM o,SCM named), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  TSNode tsn= scm_is_true(named)
    ? ts_node_prev_named_sibling(node_ref(node))
    : ts_node_prev_sibling(node_ref(node));
  return make_node(tsn);
}

SCM_DEFINE(tsn_first_child_for_byte, "ts-node-first-child-for-byte", 2, 1, 0,
           (SCM o,SCM n,SCM named), "")
#define FUNC_NAME s_tsn_first_child_for_byte
{
  ASSERT_TSN(o);
  Node *node=FR(o);
  TSNode t_node=node_ref(node);
  uint32_t c_n =scm_to_uint32(n);
  return make_node(scm_is_true(named)
                   ? ts_node_first_named_child_for_byte(t_node,c_n)
                   : ts_node_first_child_for_byte(t_node,c_n));
}
#undef FUNC_NAME

SCM_DEFINE(tsn_descendant_for_byte_range, "ts-node-descendant-for-byte-range", 3, 1, 0,
           (SCM o,SCM start,SCM end,SCM named), "")
#define FUNC_NAME s_tsn_descendant_for_byte_range
{
  ASSERT_TSN(o);
  Node *node=FR(o);
  TSNode t_node=node_ref(node);
  uint32_t c_start =  scm_to_uint32(start);
  uint32_t c_end =  scm_to_uint32(end);
  return make_node(scm_is_true(named)
                   ? ts_node_named_descendant_for_byte_range(t_node,c_start,c_end)
                   : ts_node_descendant_for_byte_range(t_node,c_start,c_end));
}
#undef FUNC_NAME



SCM_DEFINE(tsn_eq, "%ts-node-eq?", 2, 0, 0,
           (SCM o,SCM o2), "") {
  ASSERT_TSN(o);
  ASSERT_TSN(o2);
  Node *node=FR(o);
  Node *node2=FR(o2);
  return scm_from_bool(ts_node_eq(node_ref(node), node_ref(node2)));
}

SCM_DEFINE(tstc_cursor_new, "ts-tree-cursor-new", 1, 0, 0,
           (SCM o), "") {
  ASSERT_TSN(o);
  Node *node=FR(o);
  return make_tcursor(ts_tree_cursor_new(node_ref(node)));
}

SCM_DEFINE(tstc_cursor_copy, "ts-tree-cursor-copy", 1, 0, 0, (SCM cursor),
           "")
#define FUNC_NAME s_tstc_cursor_copy
{
  ASSERT_TSTC(cursor);
  Tcursor *tc = FR(cursor);
  SCM new=make_tcursor(ts_tree_cursor_copy(&tc->cursor));
  scm_remember_upto_here_1(cursor);
  return new;
}
#undef FUNC_NAME

SCM_DEFINE(tstc_cursor_reset, "ts-tree-cursor-reset!", 2, 0, 0,
           (SCM cursor,SCM node),
           "")
#define FUNC_NAME s_tstc_cursor_reset
{
  ASSERT_TSTC(cursor);
  ASSERT_TSN(node);
  Tcursor *tc = FR(cursor);
  Node *c_node=FR(node);
  TSNode t_node=node_ref(c_node);
  ts_tree_cursor_reset(&tc->cursor, t_node);
  scm_remember_upto_here_2(cursor,node);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(tstc_current_node, "ts-tree-cursor-current-node", 1, 0, 0,
           (SCM cursor),
           "")
#define FUNC_NAME s_tstc_current_node
{
  ASSERT_TSTC(cursor);
  Tcursor *tc = FR(cursor);
  SCM node=make_node(ts_tree_cursor_current_node(&tc->cursor));
  scm_remember_upto_here_1(cursor);
  return node;
}
#undef FUNC_NAME

SCM_DEFINE(tstc_current_field_name, "ts-tree-cursor-current-field-name", 1, 0, 0,
           (SCM cursor),
           "")
#define FUNC_NAME s_tstc_current_node
{
  ASSERT_TSTC(cursor);
  Tcursor *tc = FR(cursor);
  const char *string=ts_tree_cursor_current_field_name(&tc->cursor);;
  scm_remember_upto_here_1(cursor);
  return string ? scm_from_utf8_string(string) : SCM_BOOL_F ;
}
#undef FUNC_NAME

SCM_DEFINE(tstc_goto_parent, "ts-tree-cursor-goto-parent", 1, 0, 0,
           (SCM cursor),
           "")
#define FUNC_NAME s_tstc_goto_parent
{
  ASSERT_TSTC(cursor);
  Tcursor *tc = FR(cursor);
  SCM g_p=scm_from_bool(ts_tree_cursor_goto_parent(&tc->cursor));
  scm_remember_upto_here_1(cursor);
  return g_p;
}
#undef FUNC_NAME

SCM_DEFINE(tstc_goto_first_child, "ts-tree-cursor-goto-first-child", 1, 0, 0,
           (SCM cursor),
           "")
#define FUNC_NAME s_tstc_goto_first_child
{
  ASSERT_TSTC(cursor);
  Tcursor *tc = FR(cursor);
  SCM g_p=scm_from_bool(ts_tree_cursor_goto_first_child(&tc->cursor));
  scm_remember_upto_here_1(cursor);
  return g_p;
}
#undef FUNC_NAME

SCM_DEFINE(tstc_goto_next_sibling, "ts-tree-cursor-goto-next-sibling", 1, 0, 0,
           (SCM cursor),
           "")
#define FUNC_NAME s_tstc_goto_next_sibling
{
  ASSERT_TSTC(cursor);
  Tcursor *tc = FR(cursor);
  SCM success=scm_from_bool(ts_tree_cursor_goto_next_sibling(&tc->cursor));
  scm_remember_upto_here_1(cursor);
  return success;
}
#undef FUNC_NAME
SCM_DEFINE(tstc_goto_first_child_for_byte, "ts-tree-cursor-goto-first-child-for-byte", 2, 0, 0,
           (SCM cursor, SCM byte),
           "")
#define FUNC_NAME s_tstc_goto_first_child_for_byte
{
  ASSERT_TSTC(cursor);
  Tcursor *tc = FR(cursor);
  int64_t index = ts_tree_cursor_goto_first_child_for_byte(&tc->cursor, scm_to_uint32(byte));
  SCM success;
  if (index == -1) {
    success= SCM_BOOL_F;
  } else {
    success=scm_from_int64(index);
  }
  scm_remember_upto_here_1(cursor);
  return success;
}
#undef FUNC_NAME

SCM_DEFINE(tstc_goto_first_child_for_point, "ts-tree-cursor-goto-first-child-for-point", 2, 0, 0,
           (SCM cursor, SCM point),
           "")
#define FUNC_NAME s_tstc_goto_first_child_for_point
{
  ASSERT_TSTC(cursor);
  Tcursor *tc = FR(cursor);
  int64_t index = ts_tree_cursor_goto_first_child_for_point(&tc->cursor, cons_to_point(point));
  SCM success;
  if (index == -1) {
    success= SCM_BOOL_F;
  } else {
    success=scm_from_int64(index);
  }
  scm_remember_upto_here_1(cursor);
  return success;
}
#undef FUNC_NAME

void init_ts_tree() {
#ifndef SCM_MAGIC_SNARFER
#include "tree.x"
#endif
}
