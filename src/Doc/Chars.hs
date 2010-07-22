-- | A variety of useful constant documents representing many unicode characters.

module Doc.Chars where

import Char(chr)
import Doc.DocLike

ulCorner, llCorner, urCorner, lrCorner, rTee, lTee, bTee, tTee, hLine,
 vLine, plus, s1, s9, diamond, ckBoard, degree, plMinus, bullet, lArrow,
 rArrow, dArrow, uArrow, board, lantern, block, s3, s7, lEqual, gEqual,
 pi, nEqual, sterling, coloncolon, alpha, beta, lambda, forall, exists,
 box, bot, bottom, top, pI, lAmbda, star, elem, notElem, and, or, sqoparen, sqcparen  :: TextLike a => a

ulCorner  = char $ chr 0x250C
llCorner = char $ chr 0x2514
urCorner = char $ chr 0x2510
lrCorner = char $ chr 0x2518
rTee     = char $ chr 0x2524
lTee     = char $ chr 0x251C
bTee     = char $ chr 0x2534
tTee     = char $ chr 0x252C
hLine    = char $ chr 0x2500
vLine    = char $ chr 0x2502
plus     = char $ chr 0x253C
s1       = char $ chr 0x23BA -- was: 0xF800
s9       = char $ chr 0x23BD -- was: 0xF804
diamond  = char $ chr 0x25C6
ckBoard  = char $ chr 0x2592
degree   = char $ chr 0x00B0
plMinus  = char $ chr 0x00B1
bullet   = char $ chr 0x00B7
lArrow   = char $ chr 0x2190
rArrow   = char $ chr 0x2192
dArrow   = char $ chr 0x2193
uArrow   = char $ chr 0x2191
board    = char $ chr 0x2591
lantern  = char $ chr 0x256C
block    = char $ chr 0x2588
s3       = char $ chr 0x23BB -- was: 0xF801
s7       = char $ chr 0x23BC -- was: 0xF803
lEqual   = char $ chr 0x2264
gEqual   = char $ chr 0x2265
pi       = char $ chr 0x03C0
nEqual   = char $ chr 0x2260
sterling = char $ chr 0x00A3

coloncolon = char $ chr 0x2237  -- ∷

alpha    = char $ chr 0x03b1  -- α
beta     = char $ chr 0x03b2  -- β


lambda   = char $ chr 0x03bb  -- λ
forall   = char $ chr 0x2200  -- ∀
exists   = char $ chr 0x2203  -- ∃
box      = char $ chr 0x25a1  -- □

bot      = char $ chr 0x22a5  -- ⊥
bottom   = char $ chr 0x22a5  -- ⊥
top      = char $ chr 0x22a4  -- T
pI       = char $ chr 0x03a0
lAmbda   = char $ chr 0x039b  -- Λ  (capital λ)
and      = char $ chr 0x2227  -- ∧
or       = char $ chr 0x2228  -- ∨
star     = char $ chr 0x22c6
elem     = char $ chr 0x2208  -- ∈
notElem  = char $ chr 0x2209

sqoparen = char $ chr 0x3014  -- 〔
sqcparen = char $ chr 0x3015  --  〕

