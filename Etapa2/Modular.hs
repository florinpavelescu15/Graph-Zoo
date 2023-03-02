module Modular where

import Data.List
import Data.Function (on)
import qualified Data.Set as S
import StandardGraph

type Graph a = StandardGraph a

{-
    O partiție este o mulțime de submulțimi ale unei alte mulțimi, disjuncte
    (fără elemente comune) și care împreună conțin toate elementele originale.
    
    De exemplu, pentru mulțimea [1,2,3], o posibilă partiție este [[1], [2,3]].

    Va fi folosită în etapa 3.
-}
type Partition a = S.Set (S.Set a)

{-
    *** TODO ***

    Aplică o funcție pe fiecare element al unei liste, însă doar pe unul singur
    la un moment dat, păstrându-le pe celalte nemodificate. Prin urmare, pentru
    fiecare element din lista inițială rezultă câte o listă în lista finală,
    aferentă modificării doar a acelui element.

    Exemplu:

    > mapSingle (+10) [1,2,3]
    [[11,2,3],[1,12,3],[1,2,13]]
-}
auxMapSingle :: (a -> a) -> [a] -> [a] -> [[a]] -> [[a]]
auxMapSingle f lt rt acc = if (null rt) then
                                acc
                           else auxMapSingle f (lt ++ [(head rt)]) (tail rt)
                                                    (acc ++ [lt ++ [(f (head rt))] ++ (tail rt)])

mapSingle :: (a -> a) -> [a] -> [[a]]
mapSingle f xs = auxMapSingle f [] xs []

{-
    *** TODO ***

    Determină lista tuturor partițiilor unei liste. Deși mai sus tipul
    Partition a este definit utilizând mulțimi, aici utilizăm liste,
    pentru simplitate.

    Dacă vi se pare greu de urmărit tipul întors de funcție, cu 3 niveluri
    ale constructorului de tip listă, gândiți-vă așa:
    - avem nevoie de un nivel pentru o submulțime
    - încă un nivel pentru o partiție, care este o mulțime de submulțimi
    - încă un nivel pentru mulțimea tuturor partițiilor.

    Hint: Folosiți list comprehensions pentru a răspunde la întrebarea:
    dacă am obținut o partiție a restului listei, cum obținem o partiție
    a întregii liste, care include capul? (folosiți și mapSingle)

    Exemple:

    > partitions [2,3]
    [[[2],[3]],[[2,3]]]

    > partitions [1,2,3]
    [[[1],[2],[3]],[[1,2],[3]],[[2],[1,3]],[[1],[2,3]],[[1,2,3]]]
-}
myMap :: (a -> [a]) -> [a] -> [a] -> [a]
myMap f xs acc = if (null xs) then
                    acc
                 else myMap f (tail xs) (acc ++ (f (head xs)))

auxPartitions :: [a] -> [[[a]]] -> [[[a]]]
auxPartitions xs acc = if (null xs) then
                          acc
                       else auxPartitions (tail xs) ((map (++ [[(head xs)]]) acc)
                                                  ++ (myMap (mapSingle (++ [(head xs)])) acc []))

partitions :: [a] -> [[[a]]]
partitions xs = if (null xs) then [] else auxPartitions (tail xs) [[[(head xs)]]]
