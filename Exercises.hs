-- Dummy Solutions to COMP2209 Coursework 1 Exercises
-- Please take these dummy functions and implement them as specified
-- To test this with the supplied test harness, rename to Exercises.hs
-- Submit your code using your tested version of this file
--
-- NOTE THAT NO EXTERNAL MODULES MAY BE USED IN SOLVING THESE EXERCISES AND
-- THAT YOU MAY NOT CHANGE THE FUNCTION SIGNATURES NOR TYPE DEFINITIONS 

-- This module statement makes public only the specified functions and types
-- please do not change these lines either
module Exercises (splitSort, longestCommonSubList, findCoords, connectLines, findLines,
    ModuleResult (ModuleResult), canProgress, DegreeClass (First, UpperSecond,
    LowerSecond, Third), classify, hillClimb, nearestRoot, Instruction (Duplicate, 
    Pop, Add, Multiply), executeInstructionSequence, optimalSequence, 
    findBusyBeavers, Rectangle (Rectangle), simplifyRectangleList, drawEllipse, 
    extractMessage, differentStream, unPairAndApply, isShellTreeSum) where
     
-- Exercise 1
-- split a given list into sub-lists 
-- each of these must be strictly ascending, descending, or equal
getDirection :: Ord a => [a] -> Int
getDirection ns
    | ns!!0 < ns!!1 = getPos ns (fromIntegral 0)
    | ns!!0 > ns!!1 = getPos ns (fromIntegral 1)
    | ns!!0 == ns!!1 = getPos ns (fromIntegral 2)

getPos :: Ord a => [a] -> Int -> Int
getPos ns p
    | tail ns == [] = 1
    | p == 0 && ns!!0 < ns!!1 = (getPos (tail ns) p) + 1
    | p == 1 && ns!!0 > ns!!1 = (getPos (tail ns) p) + 1
    | p == 2 && ns!!0 == ns!!1 = (getPos (tail ns) p) + 1
    | otherwise = 1

splitSort :: Ord a => [a] -> [[a]]
splitSort [] = []
splitSort ns
    | length ns > 1 = [take d ns] ++ splitSort (drop d ns)
    | length ns <= 1 = [ns]
    where d = getDirection ns

-- Exercise 2
-- longest common sub-list of a finite list of finite list
findSubLists [] = [[]]
findSubLists (x:xs) = [x:sublist | sublist <- findSubLists xs] ++ findSubLists xs

mergeTwo x [] = x
mergeTwo [] x = x
mergeTwo (x:xs) (y:ys) = x:(mergeTwo xs (y:ys))

merge [] = []
merge ([]:xss) = merge xss
merge ((x:xs):xss) = x : mergeTwo xs (merge xss)

count :: Eq a => [a] -> [[a]] -> Int
count _ [] = 0
count n (x:xs)
 | n == x = 1 + count n xs
 | otherwise = count n xs

longestCommonSubList :: Eq a => [[a]] -> [a]
longestCommonSubList [] = []
longestCommonSubList xs =
  foldl1 (\x y -> if length x > length y then x else y) commonSubLists
  where sublists = merge (map findSubLists xs)
        commonSubLists = filter (\x -> count x sublists == length xs) sublists

-- Exercise 3
-- check whether the given results are sufficient to pass the year 
-- and progress using the University of Southampton Calendar regulations
data ModuleResult = ModuleResult { credit :: Float, mark :: Int} deriving Show
getCredit :: ModuleResult -> Float
getCredit (ModuleResult c m)
  | m >= 40 = c
  | m >= 25 = c - (40.0 - fromIntegral m)
  | otherwise = 0

canProgress :: [ModuleResult] -> Bool
canProgress [] = False
canProgress ms
  | moduleHrSum >= 60 = True
  | otherwise = False
  where moduleHrSum = sum (map getCredit ms)

-- Exercise 4
-- compute the degree classification associate with 3 or 4 year's worth of results
-- using the regulations given in the University of Southampton Calendar
data DegreeClass = First | UpperSecond | LowerSecond | Third deriving (Eq, Show)

getCreditSum :: [ModuleResult] -> Float
getCreditSum xs = sum (map getCredit xs)

getMark :: ModuleResult -> Float
getMark (ModuleResult _ m) = fromIntegral m

getWeightedCredit :: [[ModuleResult]] -> Float
getWeightedCredit xs
  | length xs == 3 = creditSum!!1 + (2 * creditSum!!2)
  | otherwise = creditSum!!1 + (2 * creditSum!!2) + (2 * creditSum!!3)
  where creditSum = map getCreditSum xs

getMarkSum :: [ModuleResult] -> Float
getMarkSum xs = sum (map (\x -> getMark x * getCredit x) xs)

getWeightedMarks :: [[ModuleResult]] -> Float
getWeightedMarks xs
  | length xs == 3 = markSum!!1 + (2 * markSum!!2)
  | otherwise = markSum!!1 + (2 * markSum!!2) + (2 * markSum!!3)
  where markSum = map getMarkSum xs

classify :: [[ModuleResult]] -> DegreeClass
classify [] = Third
classify ms
  | weightedAvg >= 70 = First
  | weightedAvg >= 60 = UpperSecond
  | weightedAvg >= 50 = LowerSecond
  | otherwise = Third
  where weightedAvg = getWeightedMarks ms / getWeightedCredit ms

-- Exercise 5
-- search for the local maximum of f nearest x using an 
-- approximation margin delta and initial step value s
getX1 :: Float -> Float -> Float
getX1 xLow xHigh = xHigh - (0.618 * (xHigh - xLow))

getX2 :: Float -> Float -> Float
getX2 xLow xHigh = 0.618 * (xHigh - xLow) + xLow

hillClimb :: (Float -> Float) -> Float -> Float -> Float -> Float
hillClimb d x x' eps
  | abs ((d x2) - (d x1)) <= eps = (x1 + x2) / 2
  | (d x1) < (d x2) = hillClimb d x1 (max x x') eps
  | otherwise = hillClimb d (min x x') x2 eps
  where x1 = getX1 (min x x') (max x x')
        x2 = getX2 (min x x') (max x x')

-- Exercise 6
hillDescent :: (Float -> Float) -> Float -> Float -> Float -> Float
hillDescent d x x' eps
  | abs ((d x2) - (d x1)) <= eps = (x1 + x2) / 2
  | (d x1) > (d x2) = hillDescent d x1 (max x x') eps
  | otherwise = hillDescent d (min x x') x2 eps
  where x1 = getX1 (min x x') (max x x')
        x2 = getX2 (min x x') (max x x')

powerList x = map (\n -> x**n) [0..]
getFunction [] = \x -> 0.0
getFunction coefficients = \x -> (sum $ zipWith (*) coefficients (powerList x))**2
nearestRoot :: [Float] -> Float -> Float -> Float -> Float
nearestRoot xs x x' eps = hillDescent (getFunction xs) x x' eps

-- Exercise 7
data Instruction = Add | Subtract | Multiply | Duplicate | Pop deriving (Eq, Show)
popItem [] = []
popItem (x:xs) = xs

executeInstructionSequence :: [Int] -> [Instruction] -> [Int]
executeInstructionSequence [] _ = []
executeInstructionSequence ns [] = ns
executeInstructionSequence ns (i:is)
  | i == Add && length ns > 1 = executeInstructionSequence ([addition] ++ popItem(popItem(ns))) is
  | i == Multiply && length ns > 1 = executeInstructionSequence ([multiplication] ++ popItem(popItem(ns))) is
  | i == Duplicate = executeInstructionSequence ([ns!!0] ++ ns) is
  | i == Pop = executeInstructionSequence (popItem(ns)) is
  | otherwise = ns
  where addition = ns!!0 + ns!!1
        multiplication = ns!!0 * ns!!1

-- Exercise 8
examineQueue :: [Int] -> [[Instruction]] -> [Int] -> [Instruction]
examineQueue _ [] _ = []
examineQueue source (q:qs) goal
  | executeInstructionSequence source q == goal = q
  | otherwise = examineQueue source qs goal

bfs :: [Int] -> [Int] -> [[Instruction]] -> [Instruction]
bfs source goal queue
  | length workingIns /= 0 = workingIns
  | otherwise = bfs source goal newQueue
  where workingIns = examineQueue source queue goal
        newQueue = [x ++ [Multiply] | x <- queue] ++ [x ++ [Duplicate] | x <- queue]

optimalSequence :: Int -> [Instruction]
optimalSequence 0 = []
optimalSequence 1 = []
optimalSequence n = bfs [2] [2^n] [[Duplicate]]

-- Exercise 9
validInstructions = [Pop, Add, Multiply]
getValidInstructionSet :: [Int] -> [[Instruction]]
getValidInstructionSet ns = [1..z] >>= (\n -> mapM (const validInstructions) [1..z])
  where z = (length ns) - 1

getUniqueElements :: [[Instruction]] -> [[Instruction]]
getUniqueElements [] = []
getUniqueElements ns = (ns!!0) : getUniqueElements (filter (/= (ns!!0)) (drop 1 ns))

findBusyBeavers :: [Int] -> [[Instruction]]
findBusyBeavers ns = getUniqueElements (getBeaverInstructions ns (getValidInstructionSet ns) c)
  where c = executeInstructionSequence ns (maximumIns ns (getValidInstructionSet ns))

maximumIns :: [Int] -> [[Instruction]] -> [Instruction]
maximumIns _ [] = []
maximumIns zs ys
  | length ys > 1 && (executeInstructionSequence zs (ys!!0)) >= (executeInstructionSequence zs ((drop 1 ys)!!0)) = maximumIns zs ([(ys!!0)] ++ (drop 2 ys))
  | length ys > 1 && (executeInstructionSequence zs (ys!!0)) < (executeInstructionSequence zs ((drop 1 ys)!!0)) = maximumIns zs (drop 1 ys)
  | otherwise = ys!!0

getBeaverInstructions :: [Int] -> [[Instruction]] -> [Int] -> [[Instruction]]
getBeaverInstructions [] _ _ = []
getBeaverInstructions _ [[]] _ = []
getBeaverInstructions ns xs c
  | length xs > 0 && executeInstructionSequence ns (xs!!0) == c = [xs!!0] ++ getBeaverInstructions ns (drop 1 xs) c
  | length xs > 0 && executeInstructionSequence ns (xs!!0) /= c = getBeaverInstructions ns (drop 1 xs) c
  | otherwise = []

-- Exercise 10
data Rectangle = Rectangle (Int, Int) (Int, Int) deriving (Eq, Show)

botLeft :: Rectangle -> (Int, Int)
botLeft (Rectangle c _) = c

topRight :: Rectangle -> (Int, Int)
topRight (Rectangle _ c) = c

getArea :: Rectangle -> Int
getArea rect = xDiff * yDiff
  where xDiff = fst (topRight rect) - fst (botLeft rect)
        yDiff = snd (topRight rect) - snd (botLeft rect)

sortRectangles :: [Rectangle] -> [Rectangle]
sortRectangles [] = []
sortRectangles (x:xs) = (sortRectangles more) ++ [x] ++ (sortRectangles less)
    where less = filter (\r -> getArea r < getArea x) xs
          more = filter (\r -> getArea r >= getArea x) xs

getCoords :: Rectangle -> [(Int, Int)]
getCoords rect = [(x, y) | x <- (map (\i -> i + blX) xRange), y <- (map (\i -> i + blY) yRange)]
  where blX = fst (botLeft rect)
        blY = snd (botLeft rect)
        trX = fst (topRight rect)
        trY = snd (topRight rect)
        xDiff = trX - blX
        yDiff = trY - blY
        xRange = [0..xDiff]
        yRange = [0..yDiff]

noDup :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
noDup seen [] = seen
noDup seen (x:xs)
  | x `notElem` seen = noDup (seen ++ [x]) xs
  | otherwise = noDup seen xs

cleanRectangles :: [Rectangle] -> [Rectangle]
cleanRectangles xs = filter (\x -> (fst (botLeft x) <= fst (topRight x)) && (snd (botLeft x) <= snd (topRight x))) xs

rmFullContainedRect :: [Rectangle] -> [(Int, Int)] -> [Rectangle] -> [Rectangle]
rmFullContainedRect [] _ out = out
rmFullContainedRect (x:xs) seenCoords out
  | cleanRectangles [x] /= [] &&
    (bl `notElem` seenCoords ||
    tl `notElem` seenCoords ||
    tr `notElem` seenCoords ||
    br `notElem` seenCoords) = rmFullContainedRect xs (noDup [] (seenCoords ++ getCoords x)) (out ++ [x])
  | otherwise = rmFullContainedRect xs seenCoords out
  where bl = botLeft x
        tr = topRight x
        tl = (fst bl, snd tr)
        br = (fst tr, snd bl)

getAllCoords :: [Rectangle] -> [(Int, Int)] -> [(Int, Int)]
getAllCoords [] _ = []
getAllCoords (r:rs) seenCoords
  | length rs > 0 = getAllCoords rs (noDup [] (seenCoords ++ getCoords r))
  | otherwise = noDup [] (seenCoords ++ getCoords r)

sortList :: [Int] -> [Int]
sortList [] = []
sortList (x:xs) = (sortList less) ++ [x] ++ (sortList more)
  where less = filter (\r -> r < x) xs
        more = filter (\r -> r >= x) xs

findStartEdgeRangeVert :: [(Int, Int)] -> Int -> Int
findStartEdgeRangeVert seenCoords yRange
  | (yOptions!!0 + yRange) `notElem` yOptions = yOptions!!0 + yRange - 1
  | otherwise = findStartEdgeRangeVert seenCoords (yRange + 1)
  where lowestX = findExtremeX seenCoords (<)
        yOptions = sortList (map (\x -> snd x) (filter (\x -> (fst x == lowestX)) seenCoords))

findStartEdgeRangeHoriz :: [(Int, Int)] -> Int -> Int
findStartEdgeRangeHoriz seenCoords xRange
  | (xOptions!!0 + xRange) `notElem` xOptions = xOptions!!0 + xRange - 1
  | otherwise = findStartEdgeRangeHoriz seenCoords (xRange + 1)
  where highestY = findExtremeY seenCoords (>)
        xOptions = sortList (map (\x -> fst x) (filter (\x -> (snd x == highestY)) seenCoords))

findEndEdgeVert :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
findEndEdgeVert edge allCoords
  | length outOfBoundCoords /= 0 = edge
  | otherwise = findEndEdgeVert nextEdge allCoords
  where nextEdge = map (\x -> ((fst x) + 1, snd x)) edge
        outOfBoundCoords = filter (\x -> x `notElem` allCoords) nextEdge

findEndEdgeHoriz :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
findEndEdgeHoriz edge allCoords
  | length outOfBoundCoords /= 0 = edge
  | otherwise = findEndEdgeHoriz nextEdge allCoords
  where nextEdge = map (\x -> (fst x, (snd x - 1))) edge
        outOfBoundCoords = filter (\x -> x `notElem` allCoords) nextEdge

findExtremeX :: [(Int, Int)] -> (Int -> Int -> Bool) -> Int
findExtremeX coords f = foldl1 (\x y -> if f x y then x else y) allXs
  where allXs = map (\x -> fst x) coords

findExtremeY :: [(Int, Int)] -> (Int -> Int -> Bool) -> Int
findExtremeY coords f = foldl1 (\x y -> if f x y then x else y) allYs
  where allYs = map (\x -> snd x) coords

findAdjoiningCoords :: [(Int, Int)] -> Rectangle -> [(Int, Int)]
findAdjoiningCoords seenCoords rect = topAdjoining ++ botAdjoining ++ leftAdjoining ++ rightAdjoining
  where rectCoords = getCoords rect
        topEdge = filter (\x -> snd x == findExtremeY rectCoords (>)) rectCoords
        botEdge = filter (\x -> snd x == findExtremeY rectCoords (<)) rectCoords
        leftEdge = filter (\x -> fst x == findExtremeX rectCoords (<)) rectCoords
        rightEdge = filter (\x -> fst x == findExtremeX rectCoords (>)) rectCoords
        remainingCoords = filter (`notElem` rectCoords) seenCoords
        topAdjoining = filter (\x -> (fst x, snd x + 1) `elem` remainingCoords) topEdge
        botAdjoining = filter (\x -> (fst x, snd x - 1) `elem` remainingCoords) botEdge
        leftAdjoining = filter (\x -> (fst x - 1, snd x) `elem` remainingCoords) leftEdge
        rightAdjoining = filter (\x -> (fst x + 1, snd x) `elem` remainingCoords) rightEdge


findLines :: [Rectangle] -> [Rectangle]
findLines [] = []
findLines rects = filter (\x -> fst (botLeft x) == fst (topRight x) || snd (botLeft x) == snd (topRight x)) rects

connectLines :: [Rectangle] -> [Rectangle] -> [Rectangle]
connectLines [] _ = []
connectLines (x:xs) output
  | xs == [] && matchingHorizLines /= [] = output ++ [Rectangle (leftMostBotLeft) (rightMostTopRight)]
  | xs == [] && matchingVertLines /= [] = output ++ [Rectangle (lowestBotLeft) (highestTopRight)]
  | xs == [] = output ++ [x]
  | matchingHorizLines /= [] = connectLines xs output ++ [Rectangle (leftMostBotLeft) (rightMostTopRight)]
  | matchingVertLines /= [] = connectLines xs output ++ [Rectangle (lowestBotLeft) (highestTopRight)]
  | otherwise = connectLines xs output
  where matchingHorizLines = filter (\r -> fst (botLeft r) == fst (botLeft x) && fst (topRight r) == fst (topRight x)) xs
        matchingVertLines = filter (\r -> snd (botLeft r) == snd (botLeft x) && snd (topRight r) == snd (topRight x)) xs
        leftMostBotLeft = if matchingHorizLines /= []
                          then foldl1 (\y z -> if y < z then y else z) (map (\r -> botLeft r) (matchingHorizLines ++ [x]))
                          else (0,0)
        lowestBotLeft = if matchingVertLines /= []
                        then foldl1 (\y z -> if y < z then y else z) (map (\r -> botLeft r) (matchingVertLines ++ [x]))
                        else (0,0)
        rightMostTopRight = if matchingHorizLines /= []
                            then foldl1 (\y z -> if y > z then y else z) (map (\r -> topRight r) (matchingHorizLines ++ [x]))
                            else (0,0)
        highestTopRight = if matchingVertLines /= []
                          then foldl1 (\y z -> if y > z then y else z) (map (\r -> topRight r) (matchingVertLines ++ [x]))
                          else (0,0)

mergeRectsVertSweep :: [(Int, Int)] -> [(Int, Int)] -> [Rectangle] -> [Rectangle]
mergeRectsVertSweep seenCoords allCoords rectMade
  | length seenCoords == 0 = rmFullContainedRect (sortRectangles (rectMade ++ (connectLines (findLines rectMade) []))) [] []
  | otherwise = mergeRectsVertSweep (filter (`notElem` coveredCoords) seenCoords) allCoords (rectMade ++ [newRect])
  where startingEdge = filter (\x -> (fst x == findExtremeX seenCoords (<)) && (snd x <= (findStartEdgeRangeVert seenCoords 0))) seenCoords
        endEdge = findEndEdgeVert startingEdge allCoords
        newRectBl = foldl1 (\x y -> if x < y then x else y) startingEdge
        newRectTr = foldl1 (\x y -> if x > y then x else y) endEdge
        newRect = Rectangle newRectBl newRectTr
        adjoiningCoords = findAdjoiningCoords seenCoords newRect
        coveredCoords = filter (`notElem` adjoiningCoords) (getCoords newRect)

mergeRectsHorizSweep :: [(Int, Int)] -> [(Int, Int)] -> [Rectangle] -> [Rectangle]
mergeRectsHorizSweep seenCoords allCoords rectMade
  | length seenCoords == 0 = rmFullContainedRect (sortRectangles (rectMade ++ (connectLines (findLines rectMade) []))) [] []
  | otherwise = mergeRectsHorizSweep (filter (`notElem` coveredCoords) seenCoords) allCoords (rectMade ++ [newRect])
  where startingEdge = filter (\x -> (snd x == findExtremeY seenCoords (>)) && (fst x <= (findStartEdgeRangeHoriz seenCoords 0))) seenCoords
        endEdge = findEndEdgeHoriz startingEdge allCoords
        newRectBl = foldl1 (\x y -> if x < y then x else y) endEdge
        newRectTr = foldl1 (\x y -> if x > y then x else y) startingEdge
        newRect = Rectangle newRectBl newRectTr
        adjoiningCoords = findAdjoiningCoords seenCoords newRect
        coveredCoords = filter (`notElem` adjoiningCoords) (getCoords newRect)

simplifyRectangleList :: [Rectangle] -> [Rectangle]
simplifyRectangleList [] = []
simplifyRectangleList xs
  | length cleanedRects > length mergedRectsVert && length mergedRectsHoriz >= length mergedRectsVert = mergedRectsVert
  | length mergedRectsVert > length mergedRectsHoriz = mergedRectsHoriz
  | otherwise = cleanedRects
  where cleanedRects = rmFullContainedRect (sortRectangles xs) [] []
        mergedRectsVert = mergeRectsVertSweep (getAllCoords cleanedRects []) (getAllCoords cleanedRects []) []
        mergedRectsHoriz = mergeRectsHorizSweep (getAllCoords cleanedRects []) (getAllCoords cleanedRects []) []


-- Exercise 11
-- convert an ellipse into a minimal list of rectangles representing its image
drawEllipse :: Float -> Float -> Float -> Float -> [Rectangle]
drawEllipse x y a b = simplifyRectangleList(mergeRectsVertSweep (findCoords x y a b) (findCoords x y a b) [])

isInEllipse :: Float -> Float -> Float -> Float -> Float -> Float -> Bool
isInEllipse x y a b p pp
  | ((p-x)/a)^2 + ((pp-y)/b)^2 <= 1 = True
  | otherwise = False

findCoords :: Float -> Float -> Float -> Float -> [(Int, Int)]
findCoords x y a b = coords
  where coords = filter (\(p, pp) -> (isInEllipse x y a b (fromIntegral p) (fromIntegral pp))==True) rawCoords
        rawCoords = [(p, pp) | p <- [floor(x - a)..floor(x + a)], pp <- [floor(y - b)..floor(y + b)]]

-- Exercise 12
-- extract a message hidden using a simple steganography technique
extractMessage :: String -> String
extractMessage "" = ""
extractMessage s = decode ([x | x <- s, (x `elem` "01")])

decode :: String -> String
decode "" = ""
decode s
    | [s!!0] == "0" && [s!!1] == "0" = "a" ++ decode (tail (tail s))
    | [s!!0] == "0" && [s!!1] == "1" = "b" ++ decode (tail (tail s))
    | [s!!0] == "1" && [s!!1] == "0" = "c" ++ decode (tail (tail s))
    | [s!!0] == "1" && [s!!1] == "1" = "d" ++ decode (tail (tail s))

-- Exercise 13
-- return a stream which is different from all streams of the given stream
-- you may choose to use Cantor's diagonal method
differentStream :: [[Int]] -> [Int]
differentStream ss = getUniqueStream ss 0

getUniqueStream :: [[Int]] -> Int -> [Int]
getUniqueStream xs x
 | (xs!!x)!!x == 0 = [1] ++ getUniqueStream xs (x+1)
 | otherwise = [0] ++ getUniqueStream xs (x+1)

-- Exercise 14
-- extract both components from a square shell pair and apply the (curried) function
unpair :: Int -> (Int, Int)
unpair n = (x, y)
  where base = floor(sqrt (fromIntegral n))
        x = if n < base * base + base then n - base * base else base
        y = if n <= base * base + base then base else base - (n - (base * base + base))

unPairAndApply :: Int -> (Int -> Int -> a) -> a
unPairAndApply n f = f (fst coords) (snd coords)
  where coords = unpair n

-- Exercise 15
terminateTree :: Int -> Bool
terminateTree 0 = True
terminateTree 1 = True
terminateTree xs = False

exploreTrees :: [Int] -> [Int] -> Int
exploreTrees (q:qs) nodes
  | terminateTree(fst unpaired) && terminateTree(snd unpaired) && length(qs) == 0 = sum(nodes ++ unpairedList)
  | terminateTree(fst unpaired) && terminateTree(snd unpaired) = exploreTrees qs (nodes ++ unpairedList)
  | terminateTree(fst unpaired) = exploreTrees (qs ++ [snd unpaired]) (nodes ++ unpairedList)
  | terminateTree(snd unpaired) = exploreTrees (qs ++ [fst unpaired]) (nodes ++ unpairedList)
  | otherwise = exploreTrees (qs ++ unpairedList) (nodes ++ unpairedList)
  where unpaired = unpair q
        unpairedList = [fst(unpaired), snd(unpaired)]

isShellTreeSum :: Int -> Bool
isShellTreeSum n
  | actualVal == testVal = True
  | otherwise = False
  where unpaired = unpair n
        actualVal = exploreTrees [fst(unpaired)] []
        testVal = snd(unpaired)
