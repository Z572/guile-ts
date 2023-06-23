#include <libguile.h>
#include <tree_sitter/api.h>
#include <string.h>
#include "init.h"
#include "foreign.h"
#include "api.h"

SCM_DEFINE(ref_or_set, "%rf", 2, 0, 0, (SCM type,SCM point),
           "") {
  return make_foreign_object(type,scm_to_pointer(point));
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

void init_ts_language_type(void) {
  tsl_type = make_foreign_object_type("<ts-language>", NULL);
}
void init_ts_language() {
  init_ts_language_type();
  scm_c_define("<ts-language>", tsl_type);
#ifndef SCM_MAGIC_SNARFER
#include "language.x"
#endif
}
