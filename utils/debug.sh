valgrind --db-command=cgdb  -x utils/debug.gdb -nw %f %p --db-attach=yes ./hs.out 2000
