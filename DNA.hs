-- Using types to solve a problem
data Nucleotide = A | G | T | C 
  deriving (Show, Eq)

data Pair a = Pair a a
  deriving (Show, Eq)

-- A possible implementation of pair 
data Pair' a b = Pair' a b
  deriving (Show, Eq)

type DNA = [Pair Nucleotide]

compliment :: Nucleotide -> Nucleotide
compliment A = T
compliment T = A
compliment C = G
compliment G = C

gene :: [Nucleotide]
gene = [C, T, A]

strand :: [Nucleotide]
strand = [A, C, C, T, A, G, T, A, A, T]

dnaComp :: [Nucleotide] -> DNA
dnaComp []     = []
dnaComp (n:ns) = Pair n (compliment n) : dnaComp ns 

hasGene :: [Nucleotide] -> [Nucleotide] -> Bool
hasGene _ [] = False
hasGene [] _ = True
hasGene g strand@(n:ns)  = if g == take (length g) strand then True else hasGene g ns
