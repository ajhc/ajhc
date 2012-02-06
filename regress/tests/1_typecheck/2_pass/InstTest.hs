{-# LANGUAGE M4 #-}

m4_define({{m4_for}},{{m4_ifelse($#,0,{{{{$0}}}},{{m4_ifelse(m4_eval($2<=$3),1,
{{m4_pushdef({{$1}},$2)$4{{}}m4_popdef({{$1}})$0({{$1}},m4_incr($2),$3,{{$4}})}})}})}})

m4_define({{m4_foreach}},{{m4_ifelse(m4_eval($#>2),1,
{{m4_pushdef({{$1}},{{$3}})$2{{}}m4_popdef({{$1}})m4_dnl
{{}}m4_ifelse(m4_eval($#>3),1,{{$0({{$1}},{{$2}},m4_shift(m4_shift(m4_shift($@))))}})}})}})

m4_define(checkp,{{ck_$1_$3 = (id :: $1 a => a -> a) undefined :: $2}})
m4_define(check,checkp($1,$2,$2))

m4_foreach(t,{{check(Eq,t)
}},Int,Integer,Char,Float,Double,Bool,Ordering)

m4_foreach(t,{{check(Ord,t)
}},Int,Integer,Char,Float,Double,Bool,Ordering)

m4_foreach(t,{{check(Num,t)
}},Int,Integer,Float,Double)

m4_foreach(t,{{check(Show,t)
}},Int,Integer,Char,Float,Double,Bool,Ordering)

checkp(Eq,(Char,()),Tup2)
checkp(Ord,(Char,()),Tup2)

checkp(Bounded,(),Unit)
check(Bounded,Int)
check(Bounded,Char)
check(Bounded,Bool)
check(Bounded,Ordering)

main :: IO ()
main = return ()
