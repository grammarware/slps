String transmitter example from section 7.6 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.


> import Char

Base conversion
---------------

> type Bit                      =  Int
> 
> bin2int                       :: [Bit] -> Int
> bin2int                       =  foldr (\x y -> x + 2*y) 0
> 
> int2bin                       :: Int -> [Bit]
> int2bin 0                     =  []
> int2bin n                     =  n `mod` 2 : int2bin (n `div` 2)

Transmission
------------

> make8                         :: [Bit] -> [Bit]
> make8 bits                    =  take 8 (bits ++ repeat 0)
> 
> encode                        :: String -> [Bit]
> encode                        =  concat . map (make8 . int2bin . ord)
> 
> chop8                         :: [Bit] -> [[Bit]]
> chop8 []                      =  []
> chop8 bits                    =  take 8 bits : chop8 (drop 8 bits)
> 
> decode                        :: [Bit] -> String
> decode                        =  map (chr . bin2int) . chop8
> 
> transmit                      :: String -> String
> transmit                      =  decode . channel . encode
> 
> channel                       :: [Bit] -> [Bit]
> channel                       =  id
