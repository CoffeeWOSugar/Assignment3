-- DO NOT MODIFY THE FOLLOWING LINES

module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, encode, compress, decompress) where

import Table
import PriorityQueue as PQ

import Test.HUnit
import BinomialHeap (isEmpty)
import qualified GHC.Llvm as Void

{- a bit code (of a character or string) is represented by a list of Booleans
   INVARIANT:
     the bit code is a concatenation of (0 or more) valid code words for some Huffman tree
 -}
type BitCode = [Bool]

-- END OF DO NOT MODIFY ZONE

--------------------------------------------------------------------------------

{-
Authored by: 
   Anton Rodell
   Vincent Andersson
   2022-02-08
-}

pqEmpty :: PriorityQueue a
pqEmpty = PQ.empty

{- characterCounts s
   Gives a table of characters as keys and the amount of times
   they occur in the string as the associated value
   PRECONS: Any string s.
   RETURNS: A table that maps each character that occurs in s to the number of
            times the character occurs in s.
   EXAMPLE: characterCounts "Huffman trees are fun!" = 
            T [('H',1),('u',2),('f',3),('m',1),('a',2),('n',2),(' ',3),('t',1),('r',2),('e',3),('s',1),('!',1)]
 -}
characterCounts :: String -> Table Char Int
characterCounts t = characterCountsAux t Table.empty

{- characterCountsAux s t
   Auxillary function, meant to be ran from characterCounts.
   PRECONS: Any string s and an empty table t.
   RETURNS: A table that maps each character that occurs in s to the number of 
            times the character occurs in s. 
   EXAMPLE: characterCountsAux "Huffman trees are fun!" Table.empty ==
            T [('H',1),('u',2),('f',3),('m',1),('a',2),('n',2),(' ',3),('t',1),('r',2),('e',3),('s',1),('!',1)]
   VARIANT: length s
-}
characterCountsAux :: String -> Table Char Int -> Table Char Int
characterCountsAux [] table = table
characterCountsAux (x:xs) table | Table.exists table x = characterCountsAux xs (Table.insert table x (1 + frJust(Table.lookup table x)))
                                | otherwise = characterCountsAux xs (Table.insert table x 1)

{- frJust a
   Removes the 'Just' type from any type a
   PRECONS: A value of type Maybe a. Nothing will return an error.
   RETURNS: The value of a.
   EXAMPLE: frJust (Just 6) = 6
-}
frJust :: Maybe a -> a
frJust (Just a) = a
frJust Nothing = error "Nothing in frJust."

{- tableToQueue t
   Takes a table from the characterCounts function and turns it into a
   priority queue of Huffmantree leaves.
   PRECONS: A table of characters and their respective counts.
   RETURNS: A priorityQueue with leaves which contain a key - value pair each.
   EXAMPLE: tableToQueue (characterCounts "Huffman trees are fun!") =
            BinoHeap [Node 2 1 (Leaf 's' 1) [Node 1 2 (Leaf 'r' 2) [Node 0 3 (Leaf 'e' 3) []],Node 0 1 (Leaf '!' 1) []],Node 3 1 (Leaf 'H' 1) [Node 2 1 (Leaf 't' 1) [Node 1 2 (Leaf 'a' 2) [Node 0 2 (Leaf 'n' 2) []],Node 0 3 (Leaf ' ' 3) []],Node 1 1 (Leaf 'm' 1) [Node 0 3 (Leaf 'f' 3) []],Node 0 2 (Leaf 'u' 2) []]]
-}
tableToQueue :: Table Char Int -> PriorityQueue HuffmanTree
tableToQueue t = Table.iterate t tableToQueueAux PQ.empty

{- tableToQueueAux t (c, n)
   Auxillary function, meant to be ran from tableToQueue
   PRECONS: A table of characters and their respective counts and a
            pair which is to be inserted into the queue
   RETURNS: The priorityQueue with the pair inserted as a leaf.
   EXAMPLE: tableToQueueAux (characterCounts "Huffman trees are fun!") =
            BinoHeap [Node 2 1 (Leaf 's' 1) [Node 1 2 (Leaf 'r' 2) [Node 0 3 (Leaf 'e' 3) []],Node 0 1 (Leaf '!' 1) []],Node 3 1 (Leaf 'H' 1) [Node 2 1 (Leaf 't' 1) [Node 1 2 (Leaf 'a' 2) [Node 0 2 (Leaf 'n' 2) []],Node 0 3 (Leaf ' ' 3) []],Node 1 1 (Leaf 'm' 1) [Node 0 3 (Leaf 'f' 3) []],Node 0 2 (Leaf 'u' 2) []]]
-}
tableToQueueAux :: PriorityQueue HuffmanTree -> (Char, Int) -> PriorityQueue HuffmanTree
tableToQueueAux queue pair = PQ.insert queue (createLeaf pair)

{- createLeaf (c, n)
   creates a leaf from any pair (character, Int)
   PRECONS: Any tuple of a character and int
   RETURNS: A leaf based on the character and integer tupled with the integer.
   EXAMPLE: createLeaf ('a',2) = ((Leaf 'a' 2), 2)
-}
createLeaf :: (Char, Int) -> (HuffmanTree, Int)
createLeaf (x,y) = ((Leaf x y),y)

{- The Datatype HuffmanTree represents a huffman tree and the variants it can present as.
   It represents the possible structures of a huffman tree by either taking the value of
   being a leaf with it's associated values or a node with more huffman tree types
   underneath. The tree may also be empty, in which case it is Void.
   INVARIANT: 
             - Each leaf is labeled with a unique character. 
             - Each subtree is labeled with the count of all characters within that subtree
             - Subtrees with larger character counts do not appear at a lower level in the
               tree.
-}
data HuffmanTree = Void | Leaf Char Int | Node Int HuffmanTree HuffmanTree deriving Show

{- huffmanTree t
   Generates a valid huffman tree from a table of character - int pairs.
   PRECONS: t maps each key to a positive value
   RETURNS: a Huffman tree based on the character counts in the Table t
   EXAMPLE: huffmanTree (characterCounts "Huffman trees are fun!") =
            Node 22 (Node 9 (Node 4 (Node 2 (Leaf 't' 1) (Leaf 'm' 1)) (Leaf 'r' 2)) (Node 5 (Leaf 'n' 2) (Node 3 (Leaf '!' 1) (Node 2 (Leaf 'H' 1) (Leaf 's' 1))))) (Node 13 (Node 6 (Leaf ' ' 3) (Leaf 'f' 3)) (Node 7 (Leaf 'e' 3) (Node 4 (Leaf 'a' 2) (Leaf 'u' 2))))
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t
  | PQ.is_empty (tableToQueue t) = Void
  | otherwise = fst (fst (least (huffmanTreeAux (tableToQueue t))))

{- huffmanTreeAux q
   Auxillary function, meant to be ran from huffmanTree
   PRECONS: A priorityQueue of at least one HuffmanTree.
   RETURNS: A combined single Huffmantree built from the HuffmanTrees in the priorityQueue.
   EXAMPLE: huffmanTreeAux (tableToQueue (characterCounts "Huffman trees are fun!")) =
            BinoHeap [Node 0 22 (Node 22 (Node 9 (Node 4 (Node 2 (Leaf 't' 1) (Leaf 'm' 1)) (Leaf 'r' 2)) (Node 5 (Leaf 'n' 2) (Node 3 (Leaf '!' 1) (Node 2 (Leaf 'H' 1) (Leaf 's' 1))))) (Node 13 (Node 6 (Leaf ' ' 3) (Leaf 'f' 3)) (Node 7 (Leaf 'e' 3) (Node 4 (Leaf 'a' 2) (Leaf 'u' 2))))) []]
   VARIANT: length PirorityQueue
-}
huffmanTreeAux :: PriorityQueue HuffmanTree -> PriorityQueue HuffmanTree
huffmanTreeAux q
  | PQ.is_empty q    = PQ.empty
  | PQ.is_empty nxtQ = q
  | otherwise        = huffmanTreeAux (PQ.insert nxt2Q (mergeTrees nxtT nxt2T, prio + prio2))
  where nxtQ  = snd (least q)          -- Next Queue
        nxtT  = fst (fst (least q))    -- Next Tree
        prio  = snd (fst (least q))    -- Priority of next Tree
        nxt2Q = snd (least nxtQ)       -- Second to next Queue
        nxt2T = fst (fst (least nxtQ)) -- Second to next Tree
        prio2 = snd (fst (least nxtQ)) -- Priority of second to next Tree

{- mergeTrees t1 t2
   merges two huffmanTrees and returns the combined tree with an extra node.
   PRECONS: Two Huffman trees
   RETURNS: A Huffman tree with the sum of the weights and the two trees as it's children.
   EXAMPLE: mergeTrees (Leaf 'h' 1) (Leaf 'i' 2) =
            Node 3 (Leaf 'h' 1) (Leaf 'i' 2)
-}
mergeTrees :: HuffmanTree -> HuffmanTree -> HuffmanTree
mergeTrees t1 t2 = Node (weight t1 + weight t2) t1 t2

{- weight t
   Gives the weight of any HuffmanTree, 0 if Void.
   PRECONS: Any Huffman tree
   RETURNS: The value/weight of the entered node, 0 if void
   EXAMPLE: weight Node 3 (Leaf 'h' 1) (Leaf 'i' 2) == 3
-}
weight :: HuffmanTree -> Int
weight Void         = 0
weight (Leaf _ w)   = w
weight (Node w _ _) = w

{- codeTable h
   Returns the table of characters and their respective Huffman coding
   in the entered huffmantree.
   PRECONS: Any valid HuffmanTree
   RETURNS: A table that maps each character in h to its Huffman code
   EXAMPLE: codeTable (huffmanTree (characterCounts "abcdef")) =
            T [('c',[False,False]),('b',[False,True]),('a',[True,False,False]),('e',[True,False,True]),('d',[True,True,False]),('f',[True,True,True])]
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable Void               = Table.empty
codeTable (Leaf a b)         = Table.insert Table.empty a [True]
codeTable h                  = foldl (uncurryTwo Table.insert) Table.empty (codeTableAux h [])
  where uncurryTwo f x (a,b) = f x a b

{- codeTableAux h bc
   Auxillary function, meant to be ran from codeTable.
   PRECONS: h is a non empty tree
   RETURNS: the list of pairs with chars in h paired with thier bitcode under h
   EXAMPLE: codeTableAux (Node 3 (Node 2 (Leaf 'e' 1) (Leaf 'j' 1)) (Leaf 'h' 1)) [] = [('e', [False, False]), ('j', [False, True]), ('h', [True])]
   VARIANT: amount of nodes in h
-}
codeTableAux :: HuffmanTree -> BitCode -> [(Char, BitCode)]
codeTableAux (Leaf c _) s     = [(c, s)]
codeTableAux (Node _ t1 t2) s = codeTableAux t1 (s ++ [False]) ++ codeTableAux t2 (s ++ [True])

{- encode h s
   Turns a string into its code under a tree.
   PRECONS: All characters in s appear in h
   RETURNS: the concatenation of the characters of s encoded using the Huffman code table of h.
   EXAMPLE: encode Void "" = []
            encode (Leaf 'c' 4) "c" = [True]
            encode (Leaf 'e' 6) "ee" = [True, True]
            encode (Node 8 (Leaf 't' 4) (Node 4 (Leaf 'r' 2) (Leaf 'e' 2))) "te" = [False, True, True]
   VARIANT: lenght of s
 -}
encode :: HuffmanTree -> String -> BitCode
encode _ []     = []
encode h (x:xs) = frJust(Table.lookup (codeTable h) x) ++ encode h xs

{- compress s
   creates a tree and code from a string
   PRECONS: any string
   RETURNS: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLE: compress "huffman" = (Node 7 (Node 3 (Leaf 'n' 1) (Node 2 (Leaf 'h' 1) (Leaf 'a' 1))) (Node 4 (Node 2 (Leaf 'm' 1) (Leaf 'u' 1)) (Leaf 'f' 2)),[False,True,False,True,False,True,True,True,True,True,True,False,False,False,True,True,False,False])
            compress "" = (Void, [])
            compress "aaa" = ((Leaf 'a' 3), [True, True, True])
 -}
compress :: String -> (HuffmanTree, BitCode)
compress s   = (tree, encode tree s)
  where tree = huffmanTree(characterCounts s)

{- decompress h bits
   Decodes a bitcode under a huffmantree
   PRECONS: bits is a concatenation of valid Huffman code words for h
   RETURNS: the decoding of bits under h
   EXAMPLE: decompress (Node 8 (Leaf 't' 4) (Node 4 (Leaf 'r' 2) (Leaf 'e' 2))) [False, True, False, True, True] = "tre"
            decompress (Node 2 (Leaf 'x' 1) (Leaf 'y' 1)) [False, False] = "xx"
            decompress (Leaf 'w' 2) [True] = "w"
            decompress Void [] = ""
   VARIANT: length Bitcode
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress _ []              = ""
decompress (Leaf a b) (x:xs) = a : decompress (Leaf a b) xs
decompress h b               = decompressAux h h b

{- decompressAux h1 h2 bits
   Auxillary function, meant to be ran from decompress
   PRECONS: h1 is a huffmantree with atleast 2 leafs and h2 is a subtree of h1, bits is a concatenation of valid Huffman code words for h1
   RETURNS: the decoding of bits under h1 
   EXAMPLE: decompressAux (Node 2 (Leaf 'x' 1) (Leaf 'y' 1)) (Node 2 (Leaf 'x' 1) (Leaf 'y' 1)) [True, True, False] = "yyx"
            decompressAux (Node 4 (Leaf 't' 2) (Node 2 (Leaf 'r' 1) (Leaf 'e' 1))) (Node 4 (Leaf 't' 2) (Node 2 (Leaf 'r' 1) (Leaf 'e' 1))) [False, True, False, True, True, True, True] = "tree"
   VARIANT: length bitcode
 -}
decompressAux :: HuffmanTree -> HuffmanTree -> BitCode -> String
decompressAux _ (Leaf a _) []         = [a]
decompressAux h (Leaf a _) x          = a : decompressAux h h x
decompressAux h (Node _ t1 t2) (x:xs) = decompressAux h (if x then t2 else t1) xs

--------------------------------------------------------------------------------
-- Test Cases
-- You may add your own test cases here:
-- Follow the pattern and/or read about HUnit on the interwebs.
--------------------------------------------------------------------------------

-- characterCounts
test1 = TestCase $ assertEqual "characterCounts"
            (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')

-- codeTable
-- while the precise code for ' ' may vary, its length (for the given example string) should always be 3 bits
test2 = TestCase $ assertEqual "codeTable"
            3 (maybe (-1) length (Table.lookup (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '))

-- compress
-- while the precise code for the given example string may vary, its length should always be 135 bits
test3 = TestCase $ assertEqual "compress"
            135 (length (snd (compress "this is an example of a huffman tree")))

-- decompress
test4 =
    let s = "this is an example of a huffman tree"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test5 =
    let s = "xxx"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test6 =
    let s = ""
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6]
