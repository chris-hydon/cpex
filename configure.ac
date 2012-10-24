AC_INIT([cpex], [0.0.1])
AC_PROG_CXX

function which_hs {
  for i in `ghc-pkg list | grep -o '^@<:@^ @:>@@<:@^:@:>@*'`; do
    conffile=`ls -1 $i | grep "^$1"`
    if test -n "$conffile"; then
      dir=`grep '^library-dirs' "$i/$conffile" | cut -d ' ' -f 2-`
      for j in `ls -1 "$dir" | grep "libHS$2.*\.so"`; do
        echo "$dir/$j"
      done
    fi
  done
}

AC_CHECK_PROGS(cabal, cabal, none)
if test "$cabal" = "none"; then
  AC_MSG_ERROR([Could not find cabal. Please install it.])
fi

AC_MSG_CHECKING([for rts])
AC_SUBST(hs_rts, `which_hs builtin_rts 'rts_thr-'`)
if test -z "$hs_rts"; then
  AC_MSG_RESULT([no])
  AC_MSG_ERROR([Shared libraries for rts not found. Please install them. This is usually done with cabal install --enable-shared rts, but you may also have to reinstall other dependencies with the --enable-shared flag.])
else
  AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for libcspm])
AC_SUBST(hs_cspm, [`which_hs libcspm`])
if test -z "$hs_cspm"; then
  AC_MSG_RESULT([no])
  AC_MSG_ERROR([Shared libraries for libcspm not found. Please install them. This is usually done with cabal install --enable-shared libcspm, but you may also have to reinstall other dependencies with the --enable-shared flag.])
else
  AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for libHScpex])
AC_SUBST(hs_cpex)
if test -d dist/build; then
  hs_cpex=["dist/build/`ls -1 dist/build | grep '\.so'`"]
fi
if test -f "$hs_cpex"; then
  AC_MSG_RESULT([yes])
else
  AC_MSG_RESULT([no])
  (
    cd "$srcdir" && "$cabal" configure --enable-shared --builddir="$ac_pwd/dist" && "$cabal" build --builddir="$ac_pwd/dist"
  )
  if test $? -ne 0; then
    AC_MSG_ERROR([Could not build haskell component, even though cabal was found.])
  fi
  if test -d dist/build; then
    hs_cpex="dist/build/`ls -1 dist/build | grep '\.so'`"
  fi
  if test ! -f "$hs_cpex"; then
    AC_MSG_ERROR([Could not find haskell component, even after building apparently succeeded.])
  fi
fi

AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AC_SUBST(AM_LDFLAGS, ["-Wl,-rpath,`dirname "$hs_rts"`:`dirname "$hs_cspm"`:dist/build"])
AC_SUBST(AM_CPPFLAGS, ["-I`dirname "$hs_rts"`/include -Idist/build"])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
