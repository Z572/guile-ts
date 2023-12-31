#include <libguile.h>
#include <tree_sitter/api.h>
#include <string.h>
#include "init.h"
#include "foreign.h"
#include "util.h"

DEFINE_ENUM(TSSymbolTypeRegular);
DEFINE_ENUM(TSSymbolTypeAnonymous);
DEFINE_ENUM(TSSymbolTypeAuxiliary);

SCM_DEFINE(tsl_field_count, "ts-language-field-count", 1, 0, 0,
           (SCM o), "") {
  ASSERT_TSL(o);
  const TSLanguage *tsl=FR(o);
  scm_remember_upto_here_1(tsl);
  return scm_from_uint32(ts_language_field_count(tsl));
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
  scm_remember_upto_here_2(o, fieldid);
  return string ? scm_from_utf8_string(string) : SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE(tsl_symbol_type, "ts-language-symbol-type", 2, 0, 0,
           (SCM o,SCM n), "") {
  ASSERT_TSL(o);
  TSLanguage *l=FR(o);
  TSSymbol sym=scm_to_uint16(n);
  scm_remember_upto_here_2(o,n);
  return scm_from_uint32(ts_language_symbol_type(l,sym));
}

SCM_DEFINE(tsl_symbol_count, "ts-language-symbol-count", 1, 0, 0,
           (SCM language), "")
#define FUNC_NAME s_tsl_symbol_count
{
  ASSERT_TSL(language);
  TSLanguage *l=FR(language);
  scm_remember_upto_here_1(language);
  return scm_from_uint32(ts_language_symbol_count(l));
}
#undef FUNC_NAME

SCM_DEFINE(tsl_version, "ts-language-version", 1, 0, 0,
           (SCM o), "") {
  ASSERT_TSL(o);
  TSLanguage *l=FR(o);
  scm_remember_upto_here_1(o);
  return scm_from_uint32(ts_language_version(l));
}
void init_ts_language() {
#ifndef SCM_MAGIC_SNARFER
#include "language.x"
#endif
}
