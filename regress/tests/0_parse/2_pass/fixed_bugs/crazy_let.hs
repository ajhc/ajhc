
y = []
foo = [ x | let z = y, x <- z ]
bar = [ x | let z = True in z, x <- y ]
raz = let x = let z = y in z in x

baz = let x = let {z = y} in z in x
