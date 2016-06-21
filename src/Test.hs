{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

import Test.Hspec

import Data.List (sort)
import Eisner

import Text.Printf

--instance (Show n, Show l) => Show (Link Right n l) where
--    show (Link _ _ l) = printf "-(%s)>" (show l)

--instance (Show n, Show l) => Show (Link Left n l) where
--    show (Link _ _ l) = printf "<(%s)-" (show l)

instance (Show n, Show l) => Show (Path Right n l) where
    show (Path {headNode = n, pathType = PathElementary}) = show n
    show (Path {pathType = PathConcat f c}) = printf "%s+%s" (show f) (show c)
    show (Path {pathType = PathJoin l f c}) = printf "%s-(%s)>%s" (show f) (show $ linkData l) (show c)

instance (Show n, Show l) => Show (Path Left n l) where
    show (Path {headNode = n, pathType = PathElementary}) = show n
    show (Path {pathType = PathConcat f c}) = printf "%s+%s" (show f) (show c)
    show (Path {pathType = PathJoin l f c}) = printf "%s<(%s)-%s" (show f) (show $ linkData l) (show c)

instance Positioned Int where
    position = id

instance Weighed Double where
    weight = id

type TestLinks = [(Int, Int, Double)]

testSeq = [0 .. 4]

testGraph :: TestLinks
testGraph = [(3, 1, 1.0), (1, 2, 1.0), (1, 0, 1.0), (3, 0, 5.0), (0, 2, 5.0), (4, 3, 10.0)]

testTree :: TestLinks
testTree = [(4, 3, 10.0), (3, 1, 1.0), (1, 2, 1.0), (1, 0, 1.0)]

unorderedShouldBe l r = shouldBe (sort l) (sort r)

main :: IO ()
main = hspec $
    describe "Eisner basic" $
        it "builds a simple path" $
            fmap (sort . pathToLinks) (runEisner testSeq testGraph) `shouldBe` Just (sort testTree)
