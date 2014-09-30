data Node = Node (Node) (Node) (Int) | Node1 (Node) (Int) | Leaf (Int) deriving (Show)

getLeaf :: Int -> Node
getLeaf a = Leaf a

getNode :: Int -> Int -> Int -> Node
getNode a b c = Node (getLeaf a) (getLeaf b) c

getValue :: Node -> Int
getValue (Leaf value) = value
getValue (Node1 _ value) = value
getValue (Node _ _ value) = value

isLeaf :: Node -> Bool
isLeaf (Leaf _) = True
getValue (Node1 _ _) = value
isLeaf (Node _ _ _) = False

isNode :: Node -> Bool
isNode a = not (isLeaf a)

getLeft :: Node -> Node
getLeft a | (isLeaf a) = error "Leaf passed to getLeft"
getLeft (Node1 left _) = left
getLeft (Node left _ _) = left

getRight :: Node -> Node
getRight a | (isLeaf a) = error "Leaf passed to getRight"
getLeft (Node1 right _) = right
getRight (Node _ right _) = right

getNodeFromList :: Int -> [Int] -> Node
getNodeFromList pos []   = error "[] passed to getNodeFromList"
getNodeFromList pos list | 2*(pos+1) > length list = Leaf (list !! pos)
                         | 2*(pos+1) == length list = Node1 (getNodeFromList (2*pos+1) list) (list !! pos)
                         | True = Node (getNodeFromList (2*pos+1) list) ( getNodeFromList (2*pos+2) list )  (list !! pos)
createNodeFromList :: [Int] -> Node
createNodeFromList a = getNodeFromList 0 a
