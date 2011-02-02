{-
/* replace defaults with five random seed values in calling program */
static unsigned long x=123456789,y=362436069,z=521288629,w=88675123,v=886756453;
unsigned long
xorshift(void)
{
  unsigned long t;
  t=(x^(x>>7)); x=y; y=z; z=w; w=v;
  v=(v^(v<<6))^(t^(t<<13)); return (y+y+1)*v;
}

-}


import Data.Bits
import Data.Word
import Control.Exception

type MyWord = Word32

initialState = State { 
    x=123456789, y=362436069,
    z=521288629, w=88675123,
    v=886756453 }

data State = State {
    x,y,z,w,v :: !MyWord
    }

xorshift :: State -> (State,MyWord)
xorshift s = (ns, (y ns + y ns + 1) * v ns) where
    t = x s `xor` (x s `shiftR` 7);
    nv = (v s `xor` (v s `shiftL` 6)) `xor` (t `xor` (t `shiftL` 13))
    ns = State {
        x = y s,
        y = z s,
        z = w s,
        w = v s,
        v = nv
        }

g 0 s r = s `seq` r
g n s r = r `seq` n `seq`  case xorshift s of
    (s',w) -> g (n - 1) s' (w + r)

main = do
    let f s = case xorshift s of
            (s',w) -> w:f s'
--    let g 0 s r = s `seq` r
--        g n s r = r `seq` n `seq`  case xorshift s of
--            (s',w) -> g (n - 1) s' (w + r)
    print (g (10000000::Int) initialState 0)

--    print (sum (take 1000000 (f initialState)))
