#include <libguile.h>
#include <tree_sitter/api.h>
#include <string.h>
#include "init.h"
#include "foreign.h"
#include "util.h"

static void ts_parser_finalizer(SCM scm) { ts_parser_delete(FR(scm)); }

DEFINE_FOREGE_TYPE(tsp_type,"<%ts-parser>",NULL,ts_parser_finalizer);

inline static void log_call(void *payload, TSLogType logtype, const char *string) {
  SCM proc=payload;
  scm_call_2(proc, scm_from_uint32(logtype), scm_from_utf8_string(string));
  scm_remember_upto_here_1(proc);
}

SCM_DEFINE(tsp_set_logger, "%tsp-set-logger!", 2, 0, 0, (SCM p, SCM proc), "")
#define FUNC_NAME s_tsp_set_logger
{
  ASSERT_TSP(p);
  SCM_ASSERT_TYPE((scm_to_bool(scm_procedure_p(proc)) || (scm_is_false(proc))),
                  proc, SCM_ARG2, FUNC_NAME, "procedure or #f");
  TSLogger logger = {.payload = proc, .log = log_call};
  scm_remember_upto_here_2(p,proc);
  ts_parser_set_logger(FR(p), logger);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE(tsp_logger, "%tsp-logger", 1, 0, 0, (SCM p),
                  "")
#define FUNC_NAME s_tsp_logger
{
  ASSERT_TSP(p);
  TSLogger logger=ts_parser_logger(FR(p));
  scm_remember_upto_here_1(p);

  return (logger.payload) ? (logger.payload) : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE(tsp_print_dot_graphs, "%ts-parser-print-dot-graphs", 2, 0, 0, (SCM p,SCM fd),
                  "")
#define FUNC_NAME s_tsp_print_dot_graphs
{
  ASSERT_TSP(p);
  TSParser *parser=FR(p);
  ts_parser_print_dot_graphs(parser,scm_to_int(fd));
  scm_remember_upto_here_2(p,fd);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

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
  scm_remember_upto_here_1(p);
  return SCM_UNSPECIFIED;
}

SCM_DEFINE(tsp_language, "%tsp-language", 1, 0, 0, (SCM o), "") {
  ASSERT_TSP(o);
  TSParser *tsp = FR(o);
  scm_remember_upto_here_1(o);
  const TSLanguage *tsl = ts_parser_language(tsp);
  return tsl ? make_foreign_object(tsl_type, tsl) : SCM_BOOL_F;
}

SCM_DEFINE(tsp_included_ranges, "%tsp-included-ranges", 1, 0, 0, (SCM o),
           "") {
  ASSERT_TSP(o);
  TSParser *tsp = FR(o);
  uint32_t *length=gts_malloc(sizeof(uint32_t *));
  TSRange *range=ts_parser_included_ranges(tsp, length);
  scm_remember_upto_here_1(o);
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
  scm_remember_upto_here_1(o);
  return scm_from_uint64(ts_parser_timeout_micros(tsp));
}

SCM_DEFINE(tsp_reset, "ts-parser-reset!", 1, 0, 0, (SCM p), "") {
  ASSERT_TSP(p);
  ts_parser_reset(FR(p));
  scm_remember_upto_here_1(p);
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
  SCM s_tst=tst ? make_foreign_object(tst_type, tst) : SCM_BOOL_F;
  scm_remember_upto_here_2(p,tree);
  return s_tst;
}
#undef FUNC_NAME

void init_ts_parser() {
#ifndef SCM_MAGIC_SNARFER
#include "parser.x"
#endif
}
