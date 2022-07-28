module Algorithms where

import qualified Data.Set as S
import StandardGraph

{-
    În etapa 1, prin graf înțelegem un graf cu reprezentare standard.
    În etapele următoare, vom experimenta și cu altă reprezentare.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Graph a = StandardGraph a

{-
    *** TODO ***

    Funcție generală care abstractizează BFS și DFS pornind dintr-un anumit nod,
    prin funcția de îmbinare a listelor care constituie primul parametru.
    
    Cele două liste primite ca parametru de funcția de îmbinare sunt lista
    elementelor deja aflate în structură (coadă/stivă), respectiv lista
    vecinilor nodului curent, proaspăt expandați.

    Căutarea ar trebui să țină cont de eventualele cicluri.

    Hint: Scrieți o funcție auxiliară care primește ca parametru suplimentar
    o mulțime (set) care reține nodurile vizitate până în momentul curent.
-}
auxSearch :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> Graph a              -- graful
       -> S.Set a              -- multimea de noduri vizitate
       -> [a]                  -- structura (coadă/stivă)
       -> [a]                  -- lista obținută în urma parcurgerii (acumulatorul din recursivitatea pe coada)
       -> [a]                  -- lista obținută în urma parcurgerii
auxSearch f graph visited str acc = if (null str) then 
                                       acc 
                                   else 
                                       if (S.member (head str) visited) then
                                           auxSearch f graph visited (tail str) acc
                                       else 
                                           let newVisited = S.insert (head str) visited
                                               newStr = f (tail str) (filter (\x -> (S.notMember x visited)) (S.toList (outNeighbors (head str) graph)))
                                               newAcc = acc ++ [(head str)]
                                           in auxSearch f graph newVisited newStr newAcc

search :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> a                    -- nodul de pornire
       -> Graph a              -- graful
       -> [a]                  -- lista obținută în urma parcurgerii
search f node graph = auxSearch f graph S.empty [node] []

{-
    *** TODO ***

    Strategia BFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > bfs 1 graph4
    [1,2,3,4]

    > bfs 4 graph4
    [4,1,2,3]
-}
bfs :: Ord a => a -> Graph a -> [a]
bfs = search (\x y -> x ++ y)

{-
    *** TODO ***

    Strategia DFS, derivată prin aplicarea parțială a funcției search.

    Exemple:

    > dfs 1 graph4 
    [1,2,4,3]
    
    > dfs 4 graph4
    [4,1,2,3]
-}
dfs :: Ord a => a -> Graph a -> [a]
dfs = search (\x y -> y ++ x)

{-
    *** TODO ***

    Funcția numără câte noduri intermediare expandează strategiile BFS,
    respectiv DFS, în încercarea de găsire a unei căi între un nod sursă
    și unul destinație, ținând cont de posibilitatea absenței acesteia din graf.
    Numărul exclude nodurile sursă și destinație.

    Modalitatea uzuală în Haskell de a preciza că o funcție poate să nu fie
    definită pentru anumite valori ale parametrului este constructorul de tip
    Maybe. Astfel, dacă o cale există, funcția întoarce
    Just (numărBFS, numărDFS), iar altfel, Nothing.

    Hint: funcția span.

    Exemple:

    > countIntermediate 1 3 graph4
    Just (1,2)

    Aici, bfs din nodul 1 întoarce [1,2,3,4], deci există un singur nod
    intermediar (2) între 1 și 3. dfs întoarce [1,2,4,3], deci sunt două noduri
    intermediare (2, 4) între 1 și 3.

    > countIntermediate 3 1 graph4
    Nothing

    Aici nu există cale între 3 și 1.
-}
myAuxSearch :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> Graph a              -- graful
       -> a                    -- destinatia
       -> S.Set a              -- multimea de noduri vizitate
       -> [a]                  -- structura (coadă/stivă)
       -> Int                  -- numărul de noduri expandate (acumulatorul)
       -> Int                  -- numărul de noduri expandate până la destinație
myAuxSearch f graph to visited str acc = if (null str) then 
                                             -1
                                         else 
                                             if ((head str) == to) then
                                                 acc - 1
                                             else
                                                 if (S.member (head str) visited) then
                                                     myAuxSearch f graph to visited (tail str) acc
                                                 else 
                                                     let newVisited = S.insert (head str) visited
                                                         newStr = f (tail str) (filter (\x -> (S.notMember x visited)) (S.toList (outNeighbors (head str) graph)))
                                                         newAcc = acc + 1
                                                     in myAuxSearch f graph to newVisited newStr newAcc

mySearch :: Ord a
       => ([a] -> [a] -> [a])  -- funcția de îmbinare a listelor de noduri
       -> a                    -- nodul de pornire
       -> a                    -- nodul detinatie
       -> Graph a              -- graful
       -> Int                  -- rezultatul
mySearch f from to graph = myAuxSearch f graph to S.empty [from] 0

countIntermediate :: Ord a
                  => a                 -- nodul sursă
                  -> a                 -- nodul destinație
                  -> StandardGraph a   -- graful
                  -> Maybe (Int, Int)  -- numărul de noduri expandate de BFS/DFS
countIntermediate from to graph = let numberBFS = mySearch (\x y -> x ++ y) from to graph
                                      numberDFS = mySearch (\x y -> y ++ x) from to graph
                                  in if (numberBFS == -1) then
                                         Nothing
                                     else Just (numberBFS, numberDFS)
