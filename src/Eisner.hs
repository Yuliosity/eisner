{-# LANGUAGE ExplicitForAll, TypeFamilies, ScopedTypeVariables #-}

module Language.Algorithms.Eisner
    ( runEisner
    )
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

type family RevDir a :: * where
    RevDir Left = Right
    RevDir Right = Left

data Link d n l = Link
    { linkFather, linkChild :: n
    , linkData :: l
    }

data PathType d n l
    = PathElementary
    | PathConcat (Path d n l)
    | PathJoin (Link d n l) (Path (RevDir d) n l)

data Path d n l = Path
    { headNode :: n
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
    , pathType = PathConcat child
    , pathWeight = weight father + weight child
    }

-- TODO: should we need to re-check positions here?
joinPath :: Weighed l => Link d n l -> Path d n l -> Path (RevDir d) n l -> Path d n l
joinPath link father child = Path
    { headNode = headNode father
    , pathType = PathJoin link child
    , pathWeight = weight father + weight child + weight link
    }

data Tree n = Branch n [Tree n]

type LinkGraph d n l = M.Map (Int, Int) (Link d n l)
type PathGraph d n l = M.Map (Int, Int) (Path d n l)

runEisner :: forall n l . (Positioned n, Weighed l) => [n] -> [(n, n, l)] -> Maybe (Path Left n l)
runEisner sentence links = M.lookup (0, length sentence) finalGraph where
    (leftLinks, rightLinks) = partition (\(n1, n2, _) -> position n1 < position n2) links
    lgraph :: M.Map (Int, Int) (Link Left n l)
    lgraph = M.fromList $ map (\(n1, n2, link) -> ((position n1, position n2), Link n2 n1 link)) leftLinks
    rgraph :: M.Map (Int, Int) (Link Right n l)
    rgraph = M.fromList $ map (\(n1, n2, link) -> ((position n1, position n2), Link n1 n2 link)) rightLinks
    indexed = zip [0 ..] sentence
    startMatrix :: M.Map (Int, Int) (Path d n l)
    startMatrix = M.fromList
        [ ((i, i), elemPath node)
        | (i, node) <- indexed
        ]
    finalGraph = fst $ flip execState (startMatrix :: PathGraph Left n l, startMatrix :: PathGraph Right n l) $ do
        let len = length sentence
        -- TODO: check boundaries
        forM_ [ (i, j, (position node1, position node2))
              | (j, node1) <- tail indexed, (i, node2) <- drop j indexed] $
            \(i, j, key) -> do
            (lmatrix, rmatrix) <- get
            let buildConcat :: PathGraph d n l -> [Path d n l]
                buildConcat matrix = catMaybes
                    [ liftM2 concatPath path1 path2
                    | k <- [i + 1 .. j - 1]
                    , let path1 = M.lookup (i, k) matrix
                    , let path2 = M.lookup (k, j) matrix
                    ]

            let buildJoin :: LinkGraph d n l -> PathGraph d n l -> PathGraph (RevDir d) n l -> [Path d n l]
                buildJoin links m1 m2 = case M.lookup key links of
                    Nothing -> []
                    Just link -> catMaybes
                        [ liftM2 (joinPath link) path1 path2
                        | k <- [i .. j - 1]
                        , let path1 = M.lookup (i, k) m1
                        , let path2 = M.lookup (k + 1, j) m2
                        ]

            let posLeft :: [Path Left n l]
                posLeft  = buildConcat lmatrix ++ buildJoin lgraph lmatrix rmatrix
                posRight = buildConcat rmatrix ++ buildJoin rgraph rmatrix lmatrix

            let lmatrix' = if null posLeft  then lmatrix
                               else M.insert (i, j) (minimumBy compWeight posLeft)  lmatrix
                rmatrix' = if null posRight then rmatrix
                               else M.insert (i, j) (minimumBy compWeight posRight) rmatrix
            put (lmatrix', rmatrix')
