AUTOMAKE_OPTIONS = gnu
SUFFIXES = .scm .go .c .x

libguile_ts_la_SOURCES=ts/api.c \
	ts/init.c
libguile_ts_la_CFLAGS=$(TREE_SITTER_CFLAGS) $(GUILE_CFLAGS)
libguile_ts_la_LIBADD=$(TREE_SITTER_LIBS) $(GUILE_LIBS)
lib_LTLIBRARIES = libguile_ts.la
$(SOURCES):  $(lib_LTLIBRARIES)
ACLOCAL_AMFLAGS = -I m4
BUILT_SOURCES= $(libguile_ts_la_SOURCES:%.c=%.x)
moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"
.c.x:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env guile-snarf -o $@ $< $(libguile_ts_la_CFLAGS)
EXTRA_DIST = $(SOURCES) $(TESTS) build-aux/test-driver.scm m4/ChangeLog
SOURCES= ts/api.scm
GUILE_WARNINGS =                                          \
  -Wunbound-variable -Warity-mismatch -Wshadowed-toplevel \
  -Wmacro-use-before-definition                           \
  -Wunused-variable                                       \
  -Wduplicate-case-datum -Wbad-case-datum                 \
  -Wformat

GOBJECTS = $(SOURCES:%.scm=%.go)
CLEANFILES = $(GOBJECTS) $(TESTS:tests/%.scm=%.log)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(NODIST_SOURCES)
nobase_go_DATA = $(GOBJECTS)

guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA




TESTS= tests/api.scm
TEST_EXTENSIONS = .scm
AM_SCM_LOG_FLAGS = --no-auto-compile
AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"
SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main            \
  $(top_srcdir)/build-aux/test-driver.scm
