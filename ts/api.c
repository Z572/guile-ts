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

void init_ts_api_enum() {
#define DEFINE_ENUM(n)   scm_c_define(#n, scm_from_uint32(n)); scm_c_export(#n,NULL)
  DEFINE_ENUM(TSSymbolTypeRegular);
  DEFINE_ENUM(TSSymbolTypeAnonymous);
  DEFINE_ENUM(TSSymbolTypeAuxiliary);
#undef DEFINE_ENUM
}
void init_ts_api() {
  type_table=scm_make_weak_value_hash_table(scm_from_int(3000));
  init_ts_api_enum();
  init_ts_range_type();
#ifndef SCM_MAGIC_SNARFER
#include "api.x"
#endif
}
