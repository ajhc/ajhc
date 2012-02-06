m4_changequote({{,}})
m4_changecom({-,-})

m4_define(ONCE,{{m4_ifdef(done-$1,{{m4_dnl}},{{m4_define(done-$1,1)$1}})}})

m4_define({{m4_for}},{{m4_ifelse($#,0,{{{{$0}}}},{{m4_ifelse(m4_eval($2<=$3),1,
{{m4_pushdef({{$1}},$2)$4{{}}m4_popdef({{$1}})$0({{$1}},m4_incr($2),$3,{{$4}})}})}})}})

m4_define({{m4_foreach}},{{m4_ifelse(m4_eval($#>2),1,
{{m4_pushdef({{$1}},{{$3}})$2{{}}m4_popdef({{$1}})m4_dnl
{{}}m4_ifelse(m4_eval($#>3),1,{{$0({{$1}},{{$2}},m4_shift(m4_shift(m4_shift($@))))}})}})}})
