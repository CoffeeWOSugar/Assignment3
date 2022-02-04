-- DO NOT MODIFY THE FOLLOWING LINES

module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, encode, compress, decompress) where

import Table
import PriorityQueue as PQ

import Test.HUnit
import BinomialHeap (isEmpty)

{- a bit code (of a character or string) is represented by a list of Booleans
   INVARIANT:
     the bit code is a concatenation of (0 or more) valid code words for some Huffman tree
 -}
type BitCode = [Bool]

-- END OF DO NOT MODIFY ZONE

--------------------------------------------------------------------------------

pqEmpty :: PriorityQueue a
pqEmpty = PQ.empty

{- characterCounts s
   PRECONS: A string s.
   RETURNS: A table that maps each character that occurs in s to the number of
            times the character occurs in s.
   EXAMPLE:
 -}
characterCounts :: String -> Table Char Int
characterCounts t = characterCountsAux t Table.empty

{- characterCountsAux s t
   PRECONS: A string s and an empty table t.
   RETURNS: A table that maps each character that occurs in s to the number of 
            times the character occurs in s. 
   EXAMPLE: 
   VARIANT: length s
-}
characterCountsAux :: String -> Table Char Int -> Table Char Int
characterCountsAux [] table = table
characterCountsAux (x:xs) table | Table.exists table x = characterCountsAux xs (Table.insert table x (1 + frJust(Table.lookup table x)))
                                | otherwise = characterCountsAux xs (Table.insert table x 1)

{- frJust a
   PRECONS: A value of type Maybe a. Nothing will return an error.
   RETURNS: The value of a.
   EXAMPLE: frJust (Just 6) = 6
-}
frJust :: Maybe a -> a
frJust (Just a) = a
frJust Nothing = error "Nothing in frJust."

--tableToQueue :: Table Char Int -> PriorityQueue HuffmanTree

-- Snyggt! 
--omvandlingen från char int i table till queue av träd krävde att vi skapar en aux som skapar leafs och insertar
tableToQueue :: Table Char Int -> PriorityQueue HuffmanTree
tableToQueue t = Table.iterate t tableToQueueAux PQ.empty

--
tableToQueueAux :: PriorityQueue HuffmanTree -> (Char, Int) -> PriorityQueue HuffmanTree
tableToQueueAux x y = PQ.insert x (createLeaf y)

--skapar leafs och av key - value par, och lägger sedan till i par med leaf o value så att de kan insertas i queue
createLeaf :: (Char, Int) -> (HuffmanTree, Int)
createLeaf (x,y) = ((Leaf x y),y)

-- modify and add comments as needed
data HuffmanTree = Leaf Char Int | Node Int HuffmanTree HuffmanTree deriving Show

{- huffmanTree t
   PRECONS:  t maps each key to a positive value
   RETURNS: a Huffman tree based on the character counts in t
   EXAMPLE:
   ! UNTESTED !
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t = fst (fst (least (huffmanTreeAux (tableToQueue t))))

{- huffmanTreeAux q
   PRECONS: A priorityQueue of at least one HuffmanTree.
   RETURNS: A combined Huffmantree from the HuffmanTrees in the priorityQueue
   EXAMPLE: 
   VARIANT: length PirorityQueue
   ! UNTESTED !
-}
huffmanTreeAux :: PriorityQueue HuffmanTree -> PriorityQueue HuffmanTree
huffmanTreeAux q
  | PQ.is_empty q    = error "Empty Queue"
  | PQ.is_empty nxtQ = q
  | otherwise        = huffmanTreeAux (PQ.insert (snd (least nxtQ)) (mergeTrees nxtT nxt2T, prio + prio2))
--     (The queue without the next two elements in queue) /\            /\ next two trees merged /\ the sum of respective priorities.
  where nxtQ  = snd (least q)
        nxtT  = fst (fst (least q))
        prio  = snd (fst (least q))
        nxt2Q = snd (least nxtQ)
        nxt2T = fst (fst (least nxtQ))
        prio2 = snd (fst (least nxtQ))
        

{- mergeTrees t1 t2
   PRECONS: Two Huffmantrees
   RETURNS: A Huffmantree with the sum of the weights and the two trees as it's children.
   EXAMPLE: 
   ! UNTESTED !
-}
mergeTrees :: HuffmanTree -> HuffmanTree -> HuffmanTree
mergeTrees t1 t2 = Node (weight t1 + weight t2) t1 t2

{- weight t
   PRECONS: 
   RETURNS:
   EXAMPLE: 
-}
weight :: HuffmanTree -> Int
weight (Leaf _ w)   = w
weight (Node w _ _) = w

{- codeTable h
   PRECONS: 
   RETURNS: a table that maps each character in h to its Huffman code
   EXAMPLE:
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable = undefined

{- encode h s
   PRECONS: All characters in s appear in h
   RETURNS: the concatenation of the characters of s encoded using the Huffman code table of h.
   EXAMPLE:
 -}
encode :: HuffmanTree -> String -> BitCode
encode = undefined

{- compress s
   PRECONS:
   RETURNS: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLE:
 -}
compress :: String -> (HuffmanTree, BitCode)
compress = undefined


{- decompress h bits
   PRECONS:  bits is a concatenation of valid Huffman code words for h
   RETURNS: the decoding of bits under h
   EXAMPLE:
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress = undefined


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
