m4_changequote({{,}})
m4_changecom({-,-})

m4_define(ONCE,{{m4_ifdef(done-$1,{{m4_dnl}},{{m4_define(done-$1,1)$1}})}})
