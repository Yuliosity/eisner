{-# LANGUAGE ExplicitForAll, ScopedTypeVariables #-}

module Eisner
    --( runEisner
    --)
    where

import Control.Monad
import Control.Monad.Trans.State
import Data.Function (on)
import Data.List (minimumBy, partition)
import qualified Data.Map as M
import Data.Maybe

type Position = Int
class Positioned a where
    position :: a -> Position

type Weight = Double

class Weighed a where
    weight :: a -> Weight

compWeight :: Weighed a => a -> a -> Ordering
compWeight = compare `on` weight

data Direction = DirLeft | DirRight
    deriving (Eq, Ord)

data Left
data Right

data Link d n l = Link
    { linkFather, linkChild :: !n
    , linkData :: !l
    }

data PathType d n l
    = PathElementary
    | PathConcat (Path d n l) (Path d n l)
    | PathJoin (Link d n l) (Path Right n l) (Path Left n l)

data Path d n l = Path
    { headNode :: !n
    , pathType :: PathType d n l
    , pathWeight :: !Weight
    }

instance Weighed (Path d n l) where
    weight = pathWeight

instance Weighed l => Weighed (Link d n l) where
    weight = weight . linkData

elemPath :: n -> Path d n l
elemPath node = Path
    { headNode = node
    , pathType = PathElementary
    , pathWeight = 0.0
    }

-- TODO: should we need to re-check positions here?
concatPath :: Weighed l => Path d n l -> Path d n l -> Path d n l
concatPath father child = Path
    { headNode = headNode father
    , pathType = PathConcat father child
    , pathWeight = weight father + weight child
    }

-- TODO: should we need to re-check positions here?
class Joined d where
    joinPath :: Weighed l => Link d n l -> Path Right n l -> Path Left n l -> Path d n l

instance Joined Right where
    joinPath link right left = Path
        { headNode = headNode right
        , pathType = PathJoin link right left
        , pathWeight = weight right + weight left + weight link
        }

instance Joined Left where
    joinPath link right left = Path
        { headNode = headNode left
        , pathType = PathJoin link right left
        , pathWeight = weight right + weight left + weight link
        }

--data Tree n l = Branch n [Tree n l]

--pathToTree :: Path d n l -> Tree n l
--pathToTree (Path {headNode = n, pathType = ElementaryPath}) = Branch n []
--pathToTree (Path {headNode = n, pathType = PathConcat c}) =
pathToLinks :: Path d n l -> [(n, n, l)]
pathToLinks (Path {pathType = PathElementary}) = []
pathToLinks (Path {pathType = PathConcat f c}) = pathToLinks f ++ pathToLinks c
pathToLinks (Path {pathType = PathJoin (Link n1 n2 l) f c}) = (n1, n2, l):pathToLinks f ++ pathToLinks c

type LinkGraph d n l = M.Map (Int, Int) (Link d n l)
type PathGraph d n l = M.Map (Int, Int) (Path d n l)

runEisner :: forall n l . (Positioned n, Weighed l) => [n] -> [(n, n, l)] -> Maybe (Path Left n l)
runEisner sentence links = M.lookup (0, length sentence - 1) $ fst $ testEisner sentence links

testEisner :: forall n l . (Positioned n, Weighed l) => [n] -> [(n, n, l)] -> (PathGraph Left n l, PathGraph Right n l)
testEisner sentence links = finalGraphs where
    (leftLinks, rightLinks) = partition (\(n1, n2, _) -> position n1 > position n2) links
    lgraph :: M.Map (Int, Int) (Link Left n l)
    lgraph = M.fromList $ map (\(n1, n2, link) -> ((position n2, position n1), Link n1 n2 link)) leftLinks
    rgraph :: M.Map (Int, Int) (Link Right n l)
    rgraph = M.fromList $ map (\(n1, n2, link) -> ((position n1, position n2), Link n1 n2 link)) rightLinks
    indexed = zip [0 ..] sentence
    startMatrix :: M.Map (Int, Int) (Path d n l)
    startMatrix = M.fromList
        [ ((i, i), elemPath node)
        | (i, node) <- indexed
        ]
    finalGraphs = flip execState (startMatrix :: PathGraph Left n l, startMatrix :: PathGraph Right n l) $ do
        let len = length sentence
        -- TODO: check boundaries
        forM_ [ (i, j)
              | l <- [1 .. len - 1], i <- [0 .. len - 1 - l], let j = i + l] $
            \(i, j) -> do
            let key = (position (sentence !! i), position (sentence !! j))
            (lmatrix, rmatrix) <- get
            let buildConcat :: PathGraph d n l -> [Path d n l]
                buildConcat matrix = catMaybes
                    [ liftM2 concatPath path1 path2
                    | k <- [i + 1 .. j - 1]
                    , let path1 = M.lookup (i, k) matrix
                    , let path2 = M.lookup (k, j) matrix
                    ]

            let buildJoin :: Joined d => LinkGraph d n l -> PathGraph Right n l -> PathGraph Left n l -> [Path d n l]
                buildJoin links m1 m2 = case M.lookup key links of
                    Nothing -> []
                    Just link -> catMaybes
                        [ liftM2 (joinPath link) path1 path2
                        | k <- [i .. j - 1]
                        , let path1 = M.lookup (i, k) m1
                        , let path2 = M.lookup (k + 1, j) m2
                        ]

            let posLeft :: [Path Left n l]
                posLeft  = buildConcat lmatrix ++ buildJoin lgraph rmatrix lmatrix
                posRight = buildConcat rmatrix ++ buildJoin rgraph rmatrix lmatrix

            let lmatrix' = if null posLeft  then lmatrix
                               else M.insert (i, j) (minimumBy compWeight posLeft)  lmatrix
                rmatrix' = if null posRight then rmatrix
                               else M.insert (i, j) (minimumBy compWeight posRight) rmatrix
            put (lmatrix', rmatrix')
