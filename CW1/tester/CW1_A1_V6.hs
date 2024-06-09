import Data.Char (ord,chr)

vige :: [Char] -> [Char] -> [Char]
vige [] k = []
vige p [] = []
vige (p:ps) (k:ks) = (encode p k):(vige ps (ks++[k]))
  where
    encode a b = chr $ 65 + mod (ord a + ord b) 26

vigd :: [Char] -> [Char] -> [Char]
vigd [] k = []
vigd p [] = []
vigd (p:ps) (k:ks) = (decode p k):(vigd ps (ks++[k]))
  where
    decode a b = chr $ 65 + mod (ord a - ord b) 26
