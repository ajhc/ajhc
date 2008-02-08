dnl Borrowed from Merijn de Jonge:
dnl   AC_PACKAGE_REQUIRE
dnl   AC_PROGRAM_REQUIRE
dnl   AC_PACKAGE_REQUIRE1

dnl AC_PACKAGE_REQUIRE
dnl Add --with-<pkg> switch. If this switch was not specified try to locate
dnl it by searching for one or more programs contained in that package.
dnl Abort configuration when no program could be found. The variable PKG contains the
dnl full path to the found program on return.
dnl
dnl Usage:
dnl    AC_PACKAGE_REQUIRE(package, programs, usage )
dnl
dnl When --with-<pkg> was specified, the variable <PKG> will contain the
dnl value of the argument of the switch. If the switch was not specified,
dnl its location is determined automatically using the <programs> argument.
dnl This is a list of programs to search for to find the location of the
dnl package. If one such program is found, <PKG> is set to
dnl <path_to_program>/../..
dnl
dnl Example
dnl    AC_PACKAGE_REQUIRE(wish, wish8.0 wish8.1 wish,
dnl                             [ --with-wish   location of wish program])
dnl
dnl This would set the variable WISH to the location of the first program
dnl found or to the program as specified with the --with-wish switch.
AC_DEFUN([AC_PACKAGE_REQUIRE],
[AC_PACKAGE_REQUIRE1([$1],[$2],[$3],
    dnl Program found; evaluate <actions_of_found>
    [translit($1,-a-z,_A-Z)=`dirname \`dirname $translit($1,a-z-,A-Z_)\``])
])

dnl AC_PROGRAM_REQUIRE
dnl Add --with-<pkg> switch. If this switch was not specified try to locate
dnl it by searching for one or more programs contained in that package.
dnl Abort configuration when no program could be found. The variable PKG contains the
dnl full path to the found program on return.
dnl
dnl Usage:
dnl    AC_PACKAGE_REQUIRE(package, programs, usage )
dnl
dnl When --with-<pkg> was specified, the variable <PKG> will contain the
dnl value of the argument of the switch. If the switch was not specified,
dnl its location is determined automatically using the <programs> argument.
dnl This is a list of programs to search for to find the location of the
dnl package. If one such program is found, <PKG> is set to <path_to_program>
dnl
dnl Example
dnl    AC_PROGRAM_REQUIRE(wish, wish8.0 wish8.1 wish,
dnl                             [ --with-wish   location of wish program])
dnl
dnl This would set the variable WISH to the location of the first program
dnl found or to the program as specified with the --with-wish switch.
AC_DEFUN([AC_PROGRAM_REQUIRE],
   [AC_PACKAGE_REQUIRE1([$1],[$2],[$3],)])

dnl AC_PACKAGE_REQUIRE1
dnl Implementation for AC_PACKAGE_REQUIRE and AC_PROGRAM_REQUIRE. The
dnl commands in the fourht argument are evaluated after a successful
dnl search for a package program and is used to obtain the installation
dnl directory from a full path to a program. For example to obtain
dnl /usr/local from /usr/local/bin/wish

AC_DEFUN([AC_PACKAGE_REQUIRE1],
[
   dnl Add configuration switch
   AC_ARG_WITH($1, [$3],
      dnl switch was specified
      translit($1,a-z-,A-Z_)=${withval},
      dnl If switch not specified, try to find the program automatically
      [
         AC_PATH_PROGS(translit($1,a-z-,A-Z_),$2,
         [
            dnl Not found; abort configuration
            AC_ERROR(Required \"$1\" program not found.)
         ])
         dnl Program found; evaluate <actions_of_found>
         $4
      ])
])
