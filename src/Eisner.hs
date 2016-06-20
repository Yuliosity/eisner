module Language.Algorithms.Eisner
    ( runEisner
    )
    where

import Control.Monad
import Control.Monad.Trans.State
import Data.Function (on)
import Data.List (minimumBy)
import qualified Data.Map as M
import Data.Maybe

type Position = Int

data Direction = DirLeft | DirRight
    deriving (Eq, Ord)
revDir :: Direction -> Direction
revDir DirLeft = DirRight
revDir DirRight = DirLeft

type Weight = Double

data Node d = Node
    { nodePos :: !Position
    , nodeData :: d
    }

instance Eq (Node d) where
    (==) = (==) `on` nodePos

instance Ord (Node d) where
    compare = compare `on` nodePos

data Link n d = Link
    { linkFather, linkChild :: Node n
    , linkWeight :: !Weight
    , linkDir :: !Direction
    , linkData :: d
    }

data PathType n l = PathElementary
                  | PathConcat (Path n l) (Path n l)
                  | PathJoin (Path n l) (Path n l) (Link n l)

data Path n l = Path
    { headNode :: Node n
    , pathType :: PathType n l
    , pathWeight :: !Weight
    , pathDir :: !Direction
    }

class Weighed a where
    weight :: a -> Weight

instance Weighed (Path n l) where
    weight = pathWeight

instance Weighed (Link n l) where
    weight = linkWeight

compWeight :: Weighed a => a -> a -> Ordering
compWeight = compare `on` weight

elemPath :: Direction -> Node n -> Path n l
elemPath dir node = Path
    { headNode = node
    , pathType = PathElementary
    , pathWeight = 0.0
    , pathDir = dir
    }

concatPath :: Path n l -> Path n l -> Maybe (Path n l)
concatPath first second = res where
    res = case (pathDir first, pathDir second) of
              (DirRight, DirRight) -> concatImpl first second
              (DirLeft, DirLeft)   -> concatImpl second first
              _ -> Nothing
    concatImpl father child = Just Path
        { headNode = headNode father
        , pathType = PathConcat father child
        , pathWeight = pathWeight father + pathWeight child
        , pathDir = pathDir father
        }

joinPath :: Link n l -> Path n l -> Path n l -> Maybe (Path n l)
joinPath link first second = res where
    res = case (pathDir first, pathDir second, linkDir link) of
              (DirRight, DirLeft, DirRight) -> joinImpl first second
              (DirRight, DirLeft, DirLeft)  -> joinImpl second first
              _ -> Nothing

    joinImpl father child = Just Path
        { headNode = headNode father
        , pathType = PathJoin father child link
        , pathWeight = pathWeight father + pathWeight child + linkWeight link
        , pathDir = linkDir link
        }

data Tree n = Branch n [Tree n]

runEisner :: [Node w] -> [((Node w, Node w), Link w l)] -> Maybe (Path w l)
runEisner sentence links = M.lookup (0, length sentence - 1, DirLeft) finalGraph where
    graph = M.fromList links
    indexed = zip [0 ..] sentence
    --startMatrix :: M.Map (Position, Position, Direction) (Path w l)
    startMatrix = M.fromList
        [ ((i, i, dir), elemPath dir node)
        | dir <- [DirLeft, DirRight]
        , (i, node) <- indexed
        ]
    finalGraph = flip execState startMatrix $ do
        let len = length sentence
        -- TODO: check boundaries
        forM_ [(i, l, (node1, node2)) | (l, node1) <- tail indexed, (i, node2) <- drop l indexed] $
            \(i, l, coords) -> do
            matrix <- get
            let j = i + l
            let buildConcat dir = catMaybes
                    [ join $ liftM2 concatPath path1 path2
                    | k <- [i + 1 .. j - 1]
                    , let path1 = M.lookup (i, k, dir) matrix
                    , let path2 = M.lookup (k, j, dir) matrix
                    ]

            let buildJoin dir key = case M.lookup key graph of
                    Nothing -> []
                    Just link -> catMaybes
                        [ join $ liftM2 (joinPath link) path1 path2
                        | k <- [i .. j - 1]
                        , let path1 = M.lookup (i, k, dir) matrix
                        , let path2 = M.lookup (k + 1, j, revDir dir) matrix
                        ]

            forM_ [DirLeft, DirRight] $ \dir -> do
                let pos = buildConcat dir ++ buildJoin dir coords
                unless (null pos) $ modify (M.insert (i, j, dir) (minimumBy compWeight pos))
