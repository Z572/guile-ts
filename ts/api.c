#include <libguile.h>
#include <tree_sitter/api.h>
#include <string.h>
#include "init.h"
#include "foreign.h"
#include "api.h"
SCM tsp_type;
SCM tsl_type;
SCM tst_type;
SCM tsq_type;
SCM tsqc_type;
SCM tsn_type;
SCM tsr_type;
#define ASSERT_TSP(o) scm_assert_foreign_object_type(tsp_type, o); \
  scm_remember_upto_here_1(o)
#define ASSERT_TSQ(o) scm_assert_foreign_object_type(tsq_type, o);  \
    scm_remember_upto_here_1(o)
#define ASSERT_TSQC(o) scm_assert_foreign_object_type(tsqc_type, o);\
    scm_remember_upto_here_1(o)
#define ASSERT_TSR(o) scm_assert_foreign_object_type(tsr_type, o);\
    scm_remember_upto_here_1(o)

void
value_range_error (const char* subr, SCM bad_val, SCM min, SCM max)
{
  scm_error (scm_out_of_range_key,
             subr,
             "Value out of range ~S to< ~S: ~S",
             scm_list_3 (min, max, bad_val),
             scm_list_1 (bad_val));
}

extern SCM type_table ;

static inline SCM make_range(TSRange *range) {
  return make_foreign_object(scm_c_private_ref("ts api", "<ts-range>"), range);
}

static void ts_parser_finalizer(SCM scm) { ts_parser_delete(FR(scm)); }

void init_ts_parser_type(void) {
  tsp_type = make_foreign_object_type("<%ts-parser>", ts_parser_finalizer);
}

void init_ts_range_type(void) {
  tsr_type = make_foreign_object_type("<%ts-range>",NULL);
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
  init_ts_api_enum();
  init_ts_range_type();
  scm_c_define("<%ts-parser>", tsp_type);
#ifndef SCM_MAGIC_SNARFER
#include "api.x"
#endif
}
