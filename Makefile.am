AUTOMAKE_OPTIONS = gnu
SUFFIXES = .scm .go .c .x

libguile_ts_la_SOURCES= \
	ts/api.h		\
	ts/api.c		\
	ts/init.c		\
	ts/init.h		\
	ts/language.c	\
	ts/foreign.h	\
	ts/foreign.c	\
	ts/query.c		\
	ts/tree.c		\
	ts/parser.c
libguile_ts_la_CFLAGS=$(TREE_SITTER_CFLAGS) $(GUILE_CFLAGS)
libguile_ts_la_LIBADD=$(TREE_SITTER_LIBS) $(GUILE_LIBS)
lib_LTLIBRARIES = libguile_ts.la
$(SOURCES):  $(lib_LTLIBRARIES)
ACLOCAL_AMFLAGS = -I m4
BUILT_SOURCES= $(filter %.x, $(libguile_ts_la_SOURCES:%.c=%.x))
moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_TARGET) $(GUILE_WARNINGS) -o "$@" "$<"
.c.x:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env guile-snarf -o $@ $< $(libguile_ts_la_CFLAGS)
EXTRA_DIST = $(SOURCES) $(TESTS) build-aux/test-driver.scm
SOURCES=ts/init.scm ts/query.scm ts.scm ts/language.scm ts/api.scm ts/tree.scm ts/parser.scm
GUILE_WARNINGS =                                          \
  -Wunbound-variable -Warity-mismatch -Wshadowed-toplevel \
  -Wmacro-use-before-definition                           \
  -Wunused-variable                                       \
  -Wduplicate-case-datum -Wbad-case-datum                 \
  -Wformat

GOBJECTS = $(SOURCES:%.scm=%.go)
CLEANFILES = $(GOBJECTS) $(TESTS:tests/%.scm=%.log) $(BUILT_SOURCES)

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES) $(NODIST_SOURCES)
nobase_go_DATA = $(GOBJECTS)

guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

TESTS= tests/api.scm tests/gc.scm tests/tcursor.scm tests/query.scm
TEST_EXTENSIONS = .scm
AM_SCM_LOG_FLAGS = --no-auto-compile
AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)" abs_top_builddir="$(abs_top_builddir)"
SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main            \
  $(top_srcdir)/build-aux/test-driver.scm

# a tree-sitter-json for test
# https://github.com/tree-sitter/tree-sitter-json
# commit 40a81c01a40ac48744e0c8ccabbaba1920441199

EXTRA_DIST += tests/tree-sitter-json.c
CLEANFILES += tests/tree-sitter-json.so
$(TESTS): tests/tree-sitter-json.so
tests/tree-sitter-json.so: tests/tree-sitter-json.c
	-$(MKDIR_P) $(top_builddir)/tests
	$(CC) $(TREE_SITTER_CFLAGS) -fPIC -shared $< -o $@
