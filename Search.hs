{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import Data.Maybe

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Nod {lvl::s, act::(Maybe a), parent::(Maybe (Node s a)), depth::Int, children::[(Node s a)]}

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState n = lvl n

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent n = parent n

nodeDepth :: Node s a -> Int
nodeDepth n = depth n

nodeAction :: Node s a -> Maybe a
nodeAction n = act n

nodeChildren :: Node s a -> [Node s a]
nodeChildren n = children n

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

-- createStateSpaceChildren::(ProblemState s a, Eq s) => Node s a -> s -> a -> Node s a
-- createStateSpaceChildren child_parent cur_state current_action = child_node
--     where child_node = (Node cur_state (Just current_action) (Just child_parent) (depth child_parent + 1) (map (\current_state -> (createStateSpaceChildren child_node  (snd current_state)  (fst current_state))) (successors cur_state)))

createStateSpaceHelper :: (ProblemState s a, Eq s) => s -> (Maybe a) -> (Maybe (Node s a)) -> Int -> Node s a
createStateSpaceHelper l a p d = currNode
    where currNode = Nod l a p d (map (\nextState -> createStateSpaceHelper (snd nextState) (Just (fst nextState)) (Just currNode) (d + 1)) (successors l))

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace l = createStateSpaceHelper l Nothing Nothing 0

{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfsHelper :: Ord s => Node s a -> [Node s a] -> [([Node s a], [Node s a])]
bfsHelper n front = (c, (front ++ c)) : (bfsHelper (head (front ++ c)) (rest ++ c))
    where   
        c = (nodeChildren n)
        rest = if null front then [] else tail front

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs n = ([n], [n]) : bfsHelper n []

{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

instance Eq s => Eq (Node s a) where
    n1 == n2 = nodeState n1 == nodeState n2
    n1 /= n2 = not (n1 == n2)

bidirBFS :: (ProblemState s a, Eq s, Ord s, Eq a) => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS n1 n2 = foldr 
    (\x acc -> if [(x1, y1) | x1 <- (fst x), y1 <- (snd x), x1 == y1] /= []
        then head [(x1, y1) | x1 <- (fst x), y1 <- (snd x), x1 == y1]
        else acc)
    (n1, n2)
    (zip front1 added2)
    where
        front1 = map snd (bfs n1)
        added2 = map fst (bfs n2)

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPathHelper :: (Eq a, Eq s) => Node s a -> [(Maybe a, s)]
extractPathHelper n = if par == Nothing then [(Nothing, state)] else (action, state) : (extractPathHelper (fromJust par))
    where
        action = nodeAction n
        state = nodeState n
        par = nodeParent n

extractPath :: (Eq a, Eq s) => Node s a -> [(Maybe a, s)]
extractPath n = reverse (extractPathHelper n)


{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

extractPathHelper2 :: (ProblemState s a, Ord s, Eq a) => Node s a -> [(Maybe a, s)]
extractPathHelper2 n = if (nodeAction (fromJust (nodeParent n))) == Nothing
    then move : []
    else move : (extractPathHelper2 (fromJust par))
    where
        action = nodeAction n
        state = nodeState n
        par = nodeParent n
        move = ((Just (fst (reverseAction (fromJust action), state))), nodeState (fromJust par))

extractPath2 :: (ProblemState s a, Ord s, Eq a) => Node s a -> [(Maybe a, s)]
extractPath2 n = extractPathHelper2 n

solveHelper :: (ProblemState s a, Ord s, Eq a) => (Node s a, Node s a) -> [(Maybe a, s)]
solveHelper (n1, n2) = (extractPath n1) ++ (extractPath2 n2)

solve :: (ProblemState s a, Ord s, Eq a)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve initial final = solveHelper (bidirBFS (createStateSpace initial) (createStateSpace final))


