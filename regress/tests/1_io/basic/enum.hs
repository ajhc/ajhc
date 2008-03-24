
import Data.Word

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving(Eq,Ord,Enum,Show,Bounded)

main = do
    print (False,True)
    print (fromEnum False, fromEnum True)
    print (toEnum 0 :: Bool, toEnum 1 :: Bool)
    print $ (toEnum 3 :: Day)
    print [10 :: Int, 9 .. -12 ]
    print [1, 5 :: Word8 .. 16 ]
    print [100, 93 :: Word8 .. 43  ]
    print (minBound :: Day,maxBound :: Day)
    print [Friday, Thursday  .. ]
    print [Sunday .. ]
    print [Wednesday .. ]
    print [Wednesday .. Saturday]
    print [Friday, Thursday  .. ]
    print [Monday, Tuesday  .. ]
    print [Monday, Wednesday .. ]


