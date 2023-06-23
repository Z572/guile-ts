#include <libguile.h>
#include <tree_sitter/api.h>
#include <string.h>
#include "foreign.h"
#include "api.h"
static SCM query_type;
static SCM query_cursor_type;

#define ASSERT_QUERY(o) scm_assert_foreign_object_type(query_type, o)
#define ASSERT_QC(o) scm_assert_foreign_object_type(query_cursor_type, o)
static void query_finalizer(SCM q) {
  TSQuery *query=foreign_object_ref(q);
  ts_query_delete(query);
}
static void init_query(void) {
  SCM name, slots;
  char *cname="<ts-query>";
  query_type=make_foreign_object_type(cname,query_finalizer);
  scm_c_define(cname,query_type);
}
static void qc_finalizer(SCM qc) {
  TSQueryCursor *c=foreign_object_ref(qc);
  ts_query_cursor_delete(c);
}

static void init_query_cursor(void) {
  SCM name, slots;
  char *cname="<ts-query-cursor>";
  query_cursor_type=make_foreign_object_type(cname,qc_finalizer);
  scm_c_define(cname,query_type);
}

SCM_DEFINE(query_new, "ts-query-new", 2,0, 0,
           (SCM language,SCM source),
           "")
#define FUNC_NAME s_query_new
{
  ASSERT_TSL(language);
  uint32_t error_offset;
  TSQueryError error_type;
  char* c_string=scm_to_utf8_string(source);
  uint32_t c_length=strlen(c_string);
  TSQuery *query=ts_query_new(foreign_object_ref(language),
                              c_string,
                              c_length,
                              &error_offset,
                              &error_type);
  SCM fo= query ? make_foreign_object(query_type, query) : SCM_BOOL_F;
  return query
         ? scm_values_3(fo, SCM_BOOL_F,SCM_BOOL_F)
         : scm_values_3(SCM_BOOL_F, scm_from_uint32(error_offset),
                        scm_from_uint32(error_type));
}
#undef FUNC_NAME

SCM_DEFINE(query_pattern_count, "ts-query-pattern-count",1,0, 0,
           (SCM q),
           "")
#define FUNC_NAME s_query_pattern_count
{
  ASSERT_QUERY(q);
  TSQuery *query=foreign_object_ref(q);
  scm_remember_upto_here_1(q);
  return scm_from_uint32(ts_query_pattern_count(query));
}
#undef FUNC_NAME

SCM_DEFINE(query_capture_count, "ts-query-capture-count",1,0, 0,
           (SCM q),
           "")
#define FUNC_NAME s_query_capture_count
{
  ASSERT_QUERY(q);
  TSQuery *query=foreign_object_ref(q);
  scm_remember_upto_here_1(q);
  return scm_from_uint32(ts_query_capture_count(query));
}
#undef FUNC_NAME

SCM_DEFINE(query_capture_name_for_id, "ts-query-capture-name-for-id", 2, 0, 0,
           (SCM q, SCM id), "")
#define FUNC_NAME s_query_capture_name_for_id
{
  ASSERT_QUERY(q);
  TSQuery *query = foreign_object_ref(q);
  scm_remember_upto_here_1(q);
  uint32_t c_id = scm_to_uint32(id);
  {
    uint32_t count = ts_query_capture_count(query);
    if (c_id >= count) {
      value_range_error(FUNC_NAME, id, scm_from_uint32(0),
                        scm_from_uint32(count));
    }
  }
  uint32_t length;
  const char *string = ts_query_capture_name_for_id(query, c_id, &length);
  return scm_from_utf8_stringn(string, length);
}
#undef FUNC_NAME

SCM_DEFINE(query_string_value_for_id, "ts-query-string-value-for-id", 2, 0, 0,
           (SCM q, SCM id), "")
#define FUNC_NAME s_query_string_value_for_id
{
  ASSERT_QUERY(q);
  TSQuery *query = foreign_object_ref(q);
  scm_remember_upto_here_1(q);
  uint32_t c_id = scm_to_uint32(id);
  {
    uint32_t count = ts_query_string_count(query);
    if (c_id >= count) {
      value_range_error(FUNC_NAME, id, scm_from_uint32(0),
                        scm_from_uint32(count));
    }
  }
  uint32_t length;
  const char *string = ts_query_string_value_for_id(query, c_id, &length);
  return scm_from_utf8_stringn(string, length);
}
#undef FUNC_NAME

SCM_DEFINE(query_capture_quantifier_for_id, "ts-query-capture-quantifier-for-id", 3, 0, 0,
           (SCM q, SCM p_id,SCM id), "")
#define FUNC_NAME s_query_capture_quantifier_for_id
{
  ASSERT_QUERY(q);
  TSQuery *query = foreign_object_ref(q);
  scm_remember_upto_here_1(q);
  uint32_t pat_id = scm_to_uint32(p_id);
  {
    uint32_t count = ts_query_pattern_count(query);
    if (pat_id >= count) {
      value_range_error(FUNC_NAME, id, scm_from_uint32(0),
                        scm_from_uint32(count));
    }
  }

  uint32_t c_id = scm_to_uint32(id);
  {
    uint32_t count = ts_query_capture_count(query);
    if (c_id >= count) {
      value_range_error(FUNC_NAME, id, scm_from_uint32(0),
                        scm_from_uint32(count));
    }
  }
  TSQuantifier quantifier = ts_query_capture_quantifier_for_id(query, pat_id,c_id);
  return scm_from_uint32(quantifier);
}
#undef FUNC_NAME

SCM_DEFINE(query_string_count, "ts-query-string-count",1,0, 0,
           (SCM q),
           "")
#define FUNC_NAME s_query_string_count
{
  ASSERT_QUERY(q);
  TSQuery *query=foreign_object_ref(q);
  scm_remember_upto_here_1(q);
  return scm_from_uint32(ts_query_string_count(query));
}
#undef FUNC_NAME

SCM_DEFINE(query_start_byte_for_pattern, "ts-query-start-byte-for-pattern",2,0, 0,
           (SCM q,SCM n),
           "")
#define FUNC_NAME s_query_start_byte_for_pattern
{
  ASSERT_QUERY(q);
  TSQuery *query=foreign_object_ref(q);
  scm_remember_upto_here_1(q);
  uint32_t pat_id = scm_to_uint32(n);
  {
    uint32_t count = ts_query_pattern_count(query);
    if (pat_id >= count) {
      value_range_error(FUNC_NAME, n, scm_from_uint32(0),
                        scm_from_uint32(count));
    }
  }
  return scm_from_uint32(ts_query_start_byte_for_pattern(query,pat_id));
}

#undef FUNC_NAME

SCM_DEFINE(query_cursor_new, "ts-query-cursor-new", 0,0, 0,
           (),
           "")
#define FUNC_NAME s_query_cursor_new
{
  uint32_t error_offset;
  TSQueryError error_type;
  SCM qc=make_foreign_object(query_cursor_type, ts_query_cursor_new());
  return qc;
}
#undef FUNC_NAME

SCM_DEFINE(query_cursor_exec, "ts-query-cursor-exec", 3,0, 0,
           (SCM qc,SCM q,SCM tnode),
           "")
#define FUNC_NAME s_query_cursor_exec
{
  ASSERT_QC(qc);
  ASSERT_QUERY(q);;
  ASSERT_TSN(tnode);
  Node *node=foreign_object_ref(tnode);
  ts_query_cursor_exec(foreign_object_ref(qc),
                       foreign_object_ref(q),
                       node_ref(node));
  scm_remember_upto_here_1(qc);
  scm_remember_upto_here_2(tnode,q);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(query_is_pattern_rooted, "ts-query-pattern-rooted?", 2,0, 0,
           (SCM q,SCM index),
           "")
#define FUNC_NAME s_query_is_pattern_rooted
{
  ASSERT_QUERY(q);
  scm_remember_upto_here_1(q);
  return scm_from_bool(ts_query_is_pattern_rooted(foreign_object_ref(q),
                                                  scm_to_uint32(index)));
}
#undef FUNC_NAME

SCM_DEFINE(query_cursor_set_byte_range, "ts-query-cursor-set-byte-range!", 3, 0,
           0, (SCM qc, SCM begin, SCM end), "")
#define FUNC_NAME s_query_cursor_set_byte_range
{
  ASSERT_QC(qc);
  scm_remember_upto_here_1(qc);
  TSQueryCursor *cursor = foreign_object_ref(qc);
  ts_query_cursor_set_byte_range(cursor, scm_to_uint32(begin),
                                 scm_to_uint32(end));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(query_cursor_next_match, "ts-query-cursor-next-match",1,0, 0,
           (SCM cursor),
           "")
#define FUNC_NAME s_query_cursor_next_match
{
  ASSERT_QC(cursor);
  TSQueryCursor *qc=foreign_object_ref(cursor);
  scm_remember_upto_here_1(cursor);
  TSQueryMatch tsq_match;
  bool success=ts_query_cursor_next_match(qc, &tsq_match);
  if (success) {
    SCM list=scm_make_list(scm_from_uint8(0), SCM_UNSPECIFIED);
    SCM match=scm_make(scm_list_1(scm_c_private_ref("ts query","<ts-query-match>")));

    scm_slot_set_x(match, scm_from_utf8_symbol("id"), scm_from_uint32(tsq_match.id));
    scm_slot_set_x(match, scm_from_utf8_symbol("pattern-index"), scm_from_uint32(tsq_match.pattern_index));
    for (unsigned i = 0; i < tsq_match.capture_count; i++) {
      SCM node = make_node(tsq_match.captures[i].node);
      list = scm_cons(scm_cons(node,scm_from_uint32(tsq_match.captures[i].index)), list);
    }
    scm_slot_set_x(match, scm_from_utf8_symbol("captures"), list);
    return match;
  }
  return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE(query_cursor_remove_match, "ts-query-cursor-remove-match",2 ,0, 0,
           (SCM cursor,SCM id),
           "")
#define FUNC_NAME s_query_cursor_remove_match
{
  ASSERT_QC(cursor);
  TSQueryCursor *qc=foreign_object_ref(cursor);
  scm_remember_upto_here_1(cursor);
  ts_query_cursor_remove_match(qc,scm_to_uint32(id));
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(query_cursor_next_capture, "ts-query-cursor-next-capture",1,0, 0,
           (SCM cursor),
           "")
#define FUNC_NAME s_query_cursor_next_capture
{
  ASSERT_QC(cursor);
  TSQueryCursor *qc=foreign_object_ref(cursor);
  scm_remember_upto_here_1(cursor);
  TSQueryMatch tsq_match;
  uint32_t capture_index;
  bool success=ts_query_cursor_next_capture(qc, &tsq_match,&capture_index);
  if (success) {
    SCM list=scm_make_list(scm_from_uint8(0), SCM_UNSPECIFIED);
    SCM match=scm_make(scm_list_1(scm_c_private_ref("ts query","<ts-query-match>")));

    scm_slot_set_x(match, scm_from_utf8_symbol("id"), scm_from_uint32(tsq_match.id));
    scm_slot_set_x(match, scm_from_utf8_symbol("pattern-index"), scm_from_uint32(tsq_match.pattern_index));
    for (unsigned i = 0; i < tsq_match.capture_count; i++) {
      SCM node = make_node(tsq_match.captures[i].node);
      list = scm_cons(scm_cons(node,scm_from_uint32(tsq_match.captures[i].index)), list);
    }
    scm_slot_set_x(match, scm_from_utf8_symbol("captures"), list);
    return scm_values_2(match,scm_from_uint32(capture_index));
  }
  return scm_values_2( SCM_BOOL_F, SCM_BOOL_F);
}
#undef FUNC_NAME

static void init_enum() {
#define DEFINE_ENUM(n)   scm_c_define(#n, scm_from_uint32(n)); scm_c_export(#n,NULL)
  DEFINE_ENUM(TSQueryErrorNone);
  DEFINE_ENUM(TSQueryErrorSyntax);
  DEFINE_ENUM(TSQueryErrorNodeType);
  DEFINE_ENUM(TSQueryErrorField);
  DEFINE_ENUM(TSQueryErrorCapture);
  DEFINE_ENUM(TSQueryErrorStructure);
  DEFINE_ENUM(TSQueryErrorLanguage);

  DEFINE_ENUM(TSQuantifierZero);
  DEFINE_ENUM(TSQuantifierZeroOrOne);
  DEFINE_ENUM(TSQuantifierZeroOrMore);
  DEFINE_ENUM(TSQuantifierOne);
  DEFINE_ENUM(TSQuantifierOneOrMore);
#undef DEFINE_ENUM
}

void init_ts_query() {
  init_enum();
  init_query();
  init_query_cursor();
#ifndef SCM_MAGIC_SNARFER
#include "query.x"
#endif
}
