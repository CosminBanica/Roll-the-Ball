{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}

data Cell = Cell {pipe::Char}
    deriving (Eq, Ord)

{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level {arrMap::(A.Array Position Cell)}--TODO
    deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

instance Show Cell
    where
        show cell = [pipe cell]

instance Show Level 
    where
        show lvl = concatMap show ([Cell endl] ++ (concatMap (\x -> [(arrMap lvl) A.! (x, y) | y <- [0..yMap]] ++ [Cell endl]) [0..xMap]))
            where 
                xMap = (fst (snd (A.bounds (arrMap lvl))))
                yMap = (snd (snd (A.bounds (arrMap lvl))))

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel pos = Level (A.array ((0, 0), pos) [((x,y), (Cell emptySpace)) | x <- [0..(fst pos)], y <- [0..(snd pos)]])

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

-- functie care verifica daca o positie e in array
validPos :: Position -> Level -> Bool
validPos (x, y) lvl = x >= 0 && x <= xMap && y >= 0 && y <= yMap
    where
        xMap = (fst (snd (A.bounds (arrMap lvl))))
        yMap = (snd (snd (A.bounds (arrMap lvl))))

addCell :: (Char, Position) -> Level -> Level
addCell (cell, pos) lvl = if (validPos pos lvl) then Level ((arrMap lvl) A.// [(pos, (Cell cell))]) else lvl
{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}

createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos cells = foldr addCell (emptyLevel pos) cells

{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

-- functie care returneaza noua positie, in functie de directie
newPos :: Position -> Directions -> Position
newPos (x, y) dir
    | dir == South = (x + 1, y)
    | dir == North = (x - 1, y)
    | dir == East = (x, y + 1)
    | otherwise = (x, y - 1)

isStart :: Char -> Bool
isStart chr = chr == startDown || chr == startUp || chr == startLeft || chr == startRight

isWin :: Char -> Bool
isWin chr = chr == winDown || chr == winUp || chr == winLeft || chr == winRight

notFixed :: Char -> Bool
notFixed chr = not (isStart chr) && not (isWin chr) && chr /= emptySpace

-- functie care intoarce Char-ul de la o pozitie din array
getCell :: Position -> Level -> Char
getCell (x, y) lvl = pipe ((arrMap lvl) A.! (x, y))

moveCell :: Position -> Directions -> Level -> Level
moveCell (x, y) dir lvl = if validMove then addCell (currCell, npos) (addCell (emptySpace, (x, y)) lvl) else lvl
    where
        npos = newPos (x, y) dir
        nCell = getCell npos lvl
        currCell = getCell (x, y) lvl
        validMove = (notFixed currCell) && validPos npos lvl && nCell == emptySpace

{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection = undefined

{-
    *** TODO ***
    1 1 1
    1 1 1
    1 1 1
    1 1 1
    row * lRow + column
    pos 'div' lRow = row
    pos 'mod' lRow = column

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}

-- functie care intoarce pozitia celulei de start
getStart :: Level -> [Cell] -> Int -> Position
getStart lvl cells pos = if isStart currCell then (xRez, yRez) else getStart lvl (tail cells) (pos + 1)
    where
        yMap = (snd (snd (A.bounds (arrMap lvl))))
        currCell = pipe (head cells)
        xRez = pos `div` (yMap + 1)
        yRez = pos `mod` (yMap + 1)

-- functie care intoarce pozitiile celulelor empty
getEmptys :: Level -> [Cell] -> Int -> [Position] -> [Position]
getEmptys lvl cells pos acc
    | null cells = acc
    | currCell == emptySpace = getEmptys lvl (tail cells) (pos + 1) ((xRez, yRez) : acc)
    | pos > (yMap + 1) * (xMap + 1) = acc
    | otherwise = getEmptys lvl (tail cells) (pos + 1) acc
    where
        xMap = (fst (snd (A.bounds (arrMap lvl))))
        yMap = (snd (snd (A.bounds (arrMap lvl))))
        currCell = pipe (head cells)
        xRez = pos `div` (yMap + 1)
        yRez = pos `mod` (yMap + 1)

-- functie care verifica legatura valida la celula urmatoare
validNext :: Position -> Level -> Directions -> Bool 
validNext pos lvl fromDir
    | getCell pos lvl == winUp = if fromDir == North then True else False
    | getCell pos lvl == winLeft = if fromDir == West then True else False
    | getCell pos lvl == winRight = if fromDir == East then True else False
    | getCell pos lvl == winDown = if fromDir == South then True else False
    | isStart (getCell pos lvl) = False
	| getCell pos lvl == horPipe = if fromDir == West || fromDir == East then True else False
    | getCell pos lvl == verPipe = if fromDir == North || fromDir == South then True else False
    | getCell pos lvl == botLeft = if fromDir == East || fromDir == North then True else False
    | getCell pos lvl == botRight = if fromDir == West || fromDir == North then True else False
    | getCell pos lvl == topRight = if fromDir == West || fromDir == South then True else False
    | otherwise = if fromDir == East || fromDir == South then True else False

-- functie care intoarce pozitia urmatoare si directia din care s-a ajuns in ea
nextMove :: Position -> Level -> Directions -> (Position, Directions)
nextMove pos lvl fromDir
    | getCell pos lvl == startDown = ((x + 1, y), North)
    | getCell pos lvl == startLeft = ((x, y - 1), East)
    | getCell pos lvl == startRight = ((x, y + 1), West)
    | getCell pos lvl == startUp = ((x - 1, y), South)
	| getCell pos lvl == horPipe = if fromDir == West then ((x, y + 1), West) else ((x, y - 1), East)
    | getCell pos lvl == verPipe = if fromDir == North then ((x + 1, y), North) else ((x - 1, y), South)
    | getCell pos lvl == botLeft = if fromDir == East then ((x - 1, y), South) else ((x, y + 1), West)
    | getCell pos lvl == botRight = if fromDir == West then ((x - 1, y), South) else ((x, y - 1), East)
    | getCell pos lvl == topRight = if fromDir == West then ((x + 1, y), North) else ((x, y - 1), East)
    | otherwise = if fromDir == East then ((x + 1, y), North) else ((x, y + 1), West)
	where
        x = fst pos
        y = snd pos

wonLevelHelper :: Position -> Level -> Directions -> Bool
wonLevelHelper pos lvl fromDir
	| isWin currCell = True
	| currCell == emptyCell || currCell == emptySpace = False
    | not (validPos nPos lvl) = False
	| not (validNext nPos lvl nDir) = False
	| otherwise = wonLevelHelper nPos lvl nDir
    where
        currCell = (getCell pos lvl)
        nPos = fst (nextMove pos lvl fromDir)
        nDir = snd (nextMove pos lvl fromDir)

wonLevel :: Level -> Bool
wonLevel lvl = wonLevelHelper firstPos lvl firstDir
    where 
        firstMove = (nextMove (getStart lvl (A.elems (arrMap lvl)) 0) lvl South)
        firstPos = fst firstMove
        firstDir = snd firstMove


-- functie care intoarce miscarile valide pentru celula empty data
getNearbyMoves :: Level -> Position -> [Directions] -> [((Position, Directions), Level)]
getNearbyMoves lvl pos dirs
    | null dirs = []
    | head dirs == North = if (validPos sud lvl) && (notFixed (getCell sud lvl)) then sudLvl else getNearbyMoves lvl pos (tail dirs)
    | head dirs == East = if (validPos vest lvl) && (notFixed (getCell vest lvl)) then vestLvl else getNearbyMoves lvl pos (tail dirs)
    | head dirs == West = if (validPos est lvl) && (notFixed (getCell est lvl)) then estLvl else getNearbyMoves lvl pos (tail dirs)
    | otherwise = if (validPos nord lvl) && (notFixed (getCell nord lvl)) then nordLvl else getNearbyMoves lvl pos (tail dirs)
    where
        x = fst pos
        y = snd pos
        sud = (x - 1, y)
        nord = (x + 1, y)
        est = (x, y - 1)
        vest = (x, y + 1)
        sudLvl = ((sud, South), moveCell sud South lvl) : getNearbyMoves lvl pos (tail dirs)
        nordLvl = ((nord, North), moveCell nord North lvl) : getNearbyMoves lvl pos (tail dirs)
        estLvl = ((est, East), moveCell est East lvl) : getNearbyMoves lvl pos (tail dirs)
        vestLvl = ((vest, West), moveCell vest West lvl) : getNearbyMoves lvl pos (tail dirs)


instance ProblemState Level (Position, Directions) where
    successors lvl = concatMap (\x -> getNearbyMoves lvl x dirs) emptys
        where 
            emptys = (getEmptys lvl (A.elems (arrMap lvl)) 0 [])
            dirs = [North, South, East, West]

    isGoal lvl = wonLevel lvl

    reverseAction (((x, y), dir), lvl)
        | dir == North = (((x - 1, y), South), (moveCell (x - 1, y) South lvl))
        | dir == South = (((x + 1, y), North), (moveCell (x + 1, y) North lvl))
        | dir == East = (((x, y + 1), West), (moveCell (x, y + 1) West lvl))
        | otherwise =  (((x, y - 1), East), (moveCell (x, y - 1) East lvl))
