#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([guile-ts], [0.1.0], [873216071@qq.com],
                    [guile-ts],
                    [https://github.com/Z572/guile-ts])
AC_CONFIG_SRCDIR([ts/init.c])
AC_CONFIG_HEADERS([ts/config.h])
AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIRS([m4])
AM_INIT_AUTOMAKE([color-tests subdir-objects gnu])
AC_CONFIG_FILES([Makefile doc/Makefile])
AC_CONFIG_FILES([pre-inst-env], [chmod +x pre-inst-env])
# Checks for programs.
AC_PROG_CC

# Checks for libraries.
PKG_CHECK_MODULES([GUILE], [guile-3.0])
PKG_CHECK_MODULES([TREE_SITTER], [tree-sitter])
# Checks for header files.
AC_CHECK_HEADERS([libguile.h])
AC_CHECK_HEADERS([tree_sitter/api.h])
LT_INIT([dlopen disable-static])
GUILE_PKG([3.0])

GUILE_PROGS
GUILE_SITE_DIR

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_OUTPUT
