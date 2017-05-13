module Main

where

  import System.IO
  import qualified Data.Tuple      as Tuple
  import qualified Data.List       as List
  import qualified Data.Set        as Set
  import qualified Data.Foldable   as Foldable
  import qualified Data.Array      as Array
  import qualified Data.Monoid     as Monoid
  import qualified Data.Char       as Char
  import Data.Function (on)

  data BChar = Char0 | Char1 deriving (Eq, Ord)

  instance Show BChar where
    show Char0 = "0"
    show Char1 = "1"

  choose :: [b] -> Int -> [[b]]
  _      `choose` 0       = [[]]
  []     `choose` _       =  []
  (x:xs) `choose` k       =  (x:) `fmap` (xs `choose` (k-1)) ++ xs `choose` k

  bchars :: Int -> Int -> [[BChar]]
  bchars n k
    | k > n     = error $ show n ++ " > " ++ show n
    | odd n     = error $ "odd " ++ show n
    | odd k     = error $ "odd " ++ show k
    | otherwise = [aux pairs | let is = [1..n]
                               , js <- is `choose` k
                               , let is' = is List.\\ js
                               , let bchars1 = List.replicate (n-k) Char1
                               , let x1s = List.zip is' bchars1
                               , let bchars0 = List.replicate k Char0
                               , let x0s = List.zip js bchars0
                               , let pairs = x0s ++ x1s]
    where
      aux = List.map Tuple.snd . List.sortBy (compare `on` Tuple.fst)

  count :: Eq a => a -> [a] -> Int
  count x = List.length . List.filter (x==)

  nbOcc :: [BChar] -> Int
  nbOcc xs = nbOcc0110 xs + nbOcc1001 xs

  nbOcc0110 :: [BChar] -> Int
  nbOcc0110 = nbOcc0110Part0
    where
      nbOcc0110Part0 :: [BChar] -> Int
      nbOcc0110Part0 [] = 0
      nbOcc0110Part0 (Char0 : bs) = nbOcc0110Part01 bs + nbOcc0110Part0 bs
      nbOcc0110Part0 (Char1 : bs) = nbOcc0110Part0 bs

      nbOcc0110Part01 :: [BChar] -> Int
      nbOcc0110Part01 [] = 0
      nbOcc0110Part01 (Char0 : bs) = nbOcc0110Part01 bs
      nbOcc0110Part01 (Char1 : bs) = nbOcc0110Part011 bs + nbOcc0110Part01 bs

      nbOcc0110Part011 :: [BChar] -> Int
      nbOcc0110Part011 [] = 0
      nbOcc0110Part011 (Char0 : bs) = nbOcc0110Part011 bs
      nbOcc0110Part011 (Char1 : bs) = nbOcc0110Part0110 bs + nbOcc0110Part011 bs

      nbOcc0110Part0110 :: [BChar] -> Int
      nbOcc0110Part0110 = count Char0

  nbOcc1001 :: [BChar] -> Int
  nbOcc1001 = nbOcc1001Part1
    where
      nbOcc1001Part1 :: [BChar] -> Int
      nbOcc1001Part1 [] = 0
      nbOcc1001Part1 (Char0 : bs) = nbOcc1001Part1 bs
      nbOcc1001Part1 (Char1 : bs) = nbOcc1001Part10 bs + nbOcc1001Part1 bs

      nbOcc1001Part10 :: [BChar] -> Int
      nbOcc1001Part10 [] = 0
      nbOcc1001Part10 (Char0 : bs) = nbOcc1001Part100 bs + nbOcc1001Part10 bs
      nbOcc1001Part10 (Char1 : bs) = nbOcc1001Part10 bs

      nbOcc1001Part100 :: [BChar] -> Int
      nbOcc1001Part100 [] = 0
      nbOcc1001Part100 (Char0 : bs) = nbOcc1001Part1001 bs + nbOcc1001Part100 bs
      nbOcc1001Part100 (Char1 : bs) = nbOcc1001Part100 bs

      nbOcc1001Part1001 :: [BChar] -> Int
      nbOcc1001Part1001 = count Char1

  {-|
    The 'balancedSplit' function cut a list into two balanced parts. In case the
    input list has odd length, the first part receives the extra element.
  -}
  balancedSplit :: [BChar] -> ([BChar], [BChar])
  balancedSplit xs = List.splitAt (List.length xs `div` 2) xs

  {-|
    The 'square' function returns true if the input list is a square
    (i.e., the first half list is equal to the second half list).
  -}
  isPerfectSquare :: [BChar] -> Bool
  isPerfectSquare xs = even (List.length xs) && Tuple.fst s == Tuple.snd s
    where
      s = balancedSplit xs

  {-|
    The 'isShuffleSquare' function returns true if the input string is a square
    w.r.t. to the shufle product.
  -}
  isShuffleSquare :: [BChar] -> Bool
  isShuffleSquare = not . List.null . shuffleSquareRoots

  {-|
    The 'shuffleSquareRoots' function returns the list of the shuffle square
    roots of an input list.
  -}
  shuffleSquareRoots :: [BChar] -> [[BChar]]
  shuffleSquareRoots xs
    | odd n              = []
    | otherwise          = shuffleSquareRootsAux a n
    where
      n = List.length xs
      a = Array.array (0, n-1) $ List.zip [0..] xs

  shuffleSquareRootsAux :: Array.Array Int BChar -> Int -> [[BChar]]
  shuffleSquareRootsAux a n = shuffleSquareRootsStep 2 a n t
    where
      a' = Array.array (0, 0) [(0, a Array.! 0)]
      t  = Array.array (0, 0) [(0, Set.singleton a')]

  shuffleSquareRootsStep :: Int -> Array.Array Int BChar -> Int -> Array.Array Int (Set.Set (Array.Array Int BChar)) -> [[BChar]]
  shuffleSquareRootsStep i a n t
    | i > n     = fmap Array.elems . Set.toList $ t Array.! (n `div` 2)
    | otherwise = shuffleSquareRootsStep (i+1) a n t'
    where
      t' = mkArrayshuffleSquareRootsStep i a n t

  mkArrayshuffleSquareRootsStep :: Int -> Array.Array Int BChar -> Int -> Array.Array Int (Set.Set (Array.Array Int BChar)) -> Array.Array Int (Set.Set (Array.Array Int BChar))
  mkArrayshuffleSquareRootsStep i a n t = Array.array (lb, ub) assocs
    where
      lb     = max 0 (i - (n `div` 2))
      ub     = i `div` 2
      assocs = [(j, mkSet j) | j <- [lb..ub]]

      mkSet j = Set.union set1 set2
        where
          set1 = Set.fromList $ mkSetAux1 j
          set2 = Set.fromList $ mkSetAux2 j

      mkSetAux1 j
        | j > 0 = [a' | a' <- Set.toList $ t Array.! (j-1)
                      , a' Array.! (j-1) == a Array.! (i-1)]
        | otherwise = []

      mkSetAux2 j
        | j <= b = [Array.array (lb', ub'+1) assocs | a' <- Set.toList $ t Array.! j
                                                    , let (lb', ub') = Array.bounds a'
                                                    , let assoc  = (ub'+1, a Array.! (i-1))
                                                    , let assocs = assoc : Array.assocs a']
        | otherwise = []
        where
          b = min ((i-1) `div` 2) (n `div` 2)

  explore :: Int -> Int -> ([([BChar], Int)], [([BChar], Int)])
  explore n k = (minBound, maxBound)
    where
      bs = bchars n k
      nonSquares = List.filter (not . isShuffleSquare) bs
      annotedNonSquares = List.map (\ xs -> (xs, nbOcc xs)) nonSquares
      sortedAnnotedNonSquares = List.sortBy (compare `on` Tuple.snd) annotedNonSquares
      groupedAnnotatedNonSquares = List.groupBy cmp sortedAnnotedNonSquares
      minBound = List.head groupedAnnotatedNonSquares
      maxBound = List.last groupedAnnotatedNonSquares
      cmp x y = Tuple.snd x == Tuple.snd y

  main :: IO ()
  main = do
    putStr $ show (explore 10 2)
