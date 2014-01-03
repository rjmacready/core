module Core where
import Data.Char
import Data.List

type Name = String
type IsRec = Bool
recursive, nonRecursive :: IsRec
recursive = True
nonRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, _) <- defns]

rhssOf :: [(a,b)] -> [b]
rhssOf defns = [rhs | (_, rhs) <- defns]

type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

data Expr a = EVar Name
	  		 	| ENum Int
				| EConstr Int Int
				| EAp (Expr a) (Expr a)
				| ELet IsRec [(a, Expr a)] (Expr a)
				| ECase (Expr a) [Alter a]
				| ELam [a] (Expr a)
				deriving (Show)

type CoreExpr = Expr Name

isAtomic :: Expr a -> Bool
isAtomic (EVar _) = True
isAtomic (ENum _) = True
isAtomic _ = False

type Program a = [ScnDefn a]
type CoreProgram = Program Name

type ScnDefn a = (Name, [a], Expr a)
type CoreScnDefn = ScnDefn Name

preludeDefs :: CoreProgram
preludeDefs = [("I", ["x"], EVar "x"),
				  	("K", ["x", "y"], EVar "x"),
					("K1", ["x", "y"], EVar "y"),
					("S", ["f", "g", "z"], EAp (EAp (EVar "f") (EVar "z")) 
										  		  		(EAp (EVar "g") (EVar "z"))),
					("compose", ["f", "g", "x"], EAp (EVar "f") 
												  		  		(EAp (EVar "g") (EVar "x"))),
					("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f"))]

-- PRETTY PRINT

data Iseq = INil
	  		 | IStr String
			 | IAppend Iseq Iseq
			 | IIndent Iseq
			 | INewline
			 deriving (Show)

iNil :: Iseq
iNil = INil

iStr :: String -> Iseq
iStr str = IStr str

iAppend :: Iseq -> Iseq -> Iseq
iAppend a b = IAppend a b

iNewline :: Iseq
iNewline = INewline

iNum :: Int -> Iseq
iNum n = IStr $ show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item $ zip [1..] seqs)
			  	 where
				 lay_item (n, seq) = iConcat [iFWNum 4 n, iStr ") ", iIndent seq, iNewline]

iFWNum :: Int -> Int -> Iseq
iFWNum width n = 
		 iStr (space (width - length digits) ++ digits)
		 where
		 digits = show n

iIndent :: Iseq -> Iseq
iIndent seq = IIndent seq

space :: Int -> String
space n | n > 0 = ' ' : space (n-1)
space _ | otherwise = []

flatten :: Int -> [(Iseq, Int)] -> String
flatten _ [] = ""
flatten col ((INil, indent) : seqs) = flatten col seqs
flatten col ((IStr s, indent) : seqs) = s ++ flatten col seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) = flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col ((INewline, indent) : seqs) = '\n' : (space indent) ++ (flatten indent seqs)
flatten col ((IIndent seq, indent) : seqs) = flatten col ((seq, col) : seqs)

iDisplay :: Iseq -> String
iDisplay seq = flatten 0 [(seq, 0)]


-- 1.2 (a)
iConcat :: [Iseq] -> Iseq
iConcat [] = iNil
iConcat (x:xs) = iAppend x $ iConcat xs

-- 1.2 (b)
iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave _ [] = iNil
iInterleave s (x1:[]) = x1
iInterleave s (x1:x2:[]) = iAppend x1 $ iAppend s x2
iInterleave s (x1:x2:xs) = iAppend x1 $ iAppend s $ iAppend x2 $ iAppend s $ iInterleave s xs

--

pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n) = iStr $ show n
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)

pprExpr (ELet isrec defns expr) =
		  iConcat [ iStr keyword, iNewline,
		  			 	iStr "  ", iIndent (pprDefns defns), iNewline,
						iStr "in ", pprExpr expr]
		  where
		  keyword | not isrec = "let"
		  			 | isrec = "letrec"


pprExpr (ECase expr alters) = iConcat [iStr "case ", pprAExpr expr, 
		  				  			 			  iStr " of", iNewline,
												  iConcat $ map pprAlter alters ]

pprAlter :: CoreAlt -> Iseq
pprAlter (i, vars, expr) = iIndent $ iConcat [iStr "<", iStr $ show i, iStr ">", 
				 		 		 			  iInterleave (iStr " ") $ map iStr vars,
											  iStr " -> ", pprExpr expr, iNewline]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns = iInterleave sep (map pprDefn defns)
					  where
					  sep = iConcat [iStr ";", iNewline]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]

pprAExpr :: CoreExpr -> Iseq
pprAExpr e | isAtomic e = pprExpr e
pprAExpr e | otherwise = (iStr "(") `iAppend` (pprExpr e) `iAppend` (iStr ")")

pprScnDefn :: CoreScnDefn -> Iseq
pprScnDefn (name, args, expr) = iConcat [iStr name, iStr " ", 
			  							  			 iInterleave (iStr " ") $ map iStr args,
													 iStr " = ",
			  							  			 pprExpr expr]

pprProgram :: CoreProgram -> Iseq
pprProgram defns = iInterleave iNewline (map pprScnDefn defns)

-- PARSER

type Token = String

-- utils

isWhitespace, isIdChar :: Char -> Bool
isWhitespace c = c `elem` " \t\n"

isIdChar c = isAlphaOrSymbol c || isDigit c
isSymbolic c = c `elem` "_=<>!~-+"
isAlphaOrSymbol c = isAlpha c || isSymbol c 

-- ...

clex :: String -> [Token]
clex (c:cs) | isWhitespace c = clex cs
clex (c:cs) | isDigit c = num_token : clex rest_cs
	  			  			 	  where
								  num_token = c : takeWhile isDigit cs
								  rest_cs = dropWhile isDigit cs

clex (c:cs) | isAlpha c = var_tok : clex rest_cs
	  			  			 	  where
								  var_tok = c : takeWhile isIdChar cs
								  rest_cs = dropWhile isIdChar cs

clex (c:cs) | isSymbolic c = var_tok : clex rest_cs
	  			  			 	  where
								  var_tok = c : takeWhile isSymbolic cs
								  rest_cs = dropWhile isSymbolic cs


clex (c:cs) = [c] : clex cs
clex [] = []

type Parser a = [Token] -> [(a, [Token])]

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

-- 1.16
pSat :: (String -> Bool) -> Parser String
pSat pred (tok:toks) | pred tok  = [(tok, toks)]
	  		 				| otherwise = []
pSat _ [] = []

pLit :: String -> Parser String
pLit s = pSat (== s)

-- 1.17
pVar :: Parser String
pVar = pSat (\all@(x:_) -> isAlpha x && (not $ elem all keywords))

pNum :: Parser Int
pNum = pApply (pSat (\x -> all isDigit x)) (\x -> read x :: Int) 

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks = [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks,
				  	  	  		 	  			  	  				 	(v2, toks2) <- p2 toks1 ]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore a = (pOneOrMore a) `pAlt` (pEmpty [])

pEmpty :: a -> Parser a
pEmpty ret = \tokens->[(ret, tokens)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pApply :: Parser a -> (a -> b) -> Parser b
pApply p fn toks = [(fn v1, toks) | (v1, toks) <- p toks]

-- 1.15

ignore1 :: a -> b -> b
ignore1 _ b = b

pAuxSepP :: Parser a -> Parser b -> Parser a
pAuxSepP p sep = pThen ignore1 sep p

pAuxRest :: Parser [a] -> Parser b -> Parser [a]
pAuxRest p sep = (pAuxSepP p sep) `pAlt` pEmpty []

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p sep = pThen (:) p $ pAuxRest (pOneOrMoreWithSep p sep) sep

-- end of 1.15

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d

pThen3 fn a b c toks = [ (fn v1 v2 v3, toks3) | (v1, toks1) <- a toks,
		 	 	  	 		  	 		 	 	 	 	  			  (v2, toks2) <- b toks1,
																	  (v3, toks3) <- c toks2]

-- ***

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e

pThen4 fn a b c d toks = [ (fn v1 v2 v3 v4, toks4) | (v1, toks1) <- a toks,
		 	 	  	 		  	 		 	 	 	 	  			  (v2, toks2) <- b toks1,
																	  (v3, toks3) <- c toks2,
																	  (v4, toks4) <- d toks3]

-- ***

pThen5 :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f

pThen5 fn a b c d e toks = [ (fn v1 v2 v3 v4 v5, toks5) | (v1, toks1) <- a toks,
		 	 	  	 		  	 		 	 	 	 	  			  (v2, toks2) <- b toks1,
																	  (v3, toks3) <- c toks2,
																	  (v4, toks4) <- d toks3,
																	  (v5, toks5) <- e toks4]

-- ***

pThen6 :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g

pThen6 fn a b c d e f toks = [ (fn v1 v2 v3 v4 v5 v6, toks6) | (v1, toks1) <- a toks,
		 	 	  	 		  	 		 	 	 	 	  			  (v2, toks2) <- b toks1,
																	  (v3, toks3) <- c toks2,
																	  (v4, toks4) <- d toks3,
																	  (v5, toks5) <- e toks4,
																	  (v6, toks6) <- f toks5]

-- ***

pThen7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h

pThen7 fn a b c d e f g toks = [ (fn v1 v2 v3 v4 v5 v6 v7, toks7) | (v1, toks1) <- a toks,
		 	 	  	 		  	 		 	 	 	 	  			  (v2, toks2) <- b toks1,
																	  (v3, toks3) <- c toks2,
																	  (v4, toks4) <- d toks3,
																	  (v5, toks5) <- e toks4,
																	  (v6, toks6) <- f toks5,
																	  (v7, toks7) <- g toks6]

-- ***

mk_sc name args _ expr = (name, args, expr)
mk_ap expr aexpr = EAp expr aexpr 

-- 1.23, my way!
mk_app_chain :: [CoreExpr] -> CoreExpr
mk_app_chain (left:right:[]) = EAp left right
mk_app_chain (several) = EAp (mk_app_chain iseveral) lseveral
				 			  	 where
								 iseveral = init several
								 lseveral = last several

mk_app_chain_top :: CoreExpr -> [CoreExpr] -> CoreExpr
mk_app_chain_top hd rest = mk_app_chain $ hd:rest

pAp :: Parser CoreExpr
pAp = pThen mk_app_chain_top pAExpr $ pOneOrMore pAExpr

mk_abs :: a -> [Name] -> b -> CoreExpr -> CoreExpr
mk_abs _ vars _ expr = ELam vars expr

pAbs :: Parser CoreExpr
pAbs = pThen4 mk_abs (pLit "\\") (pOneOrMore pVar) (pLit ".") pExpr

pEVar :: Parser CoreExpr
pEVar = pVar `pApply` (\x -> EVar x)

pENum :: Parser CoreExpr
pENum = pNum `pApply` (\x -> ENum x)

ignore1and3 :: a -> b -> c -> b
ignore1and3 _ b _ = b

pPExpr :: Parser CoreExpr
pPExpr = pThen3 ignore1and3 (pLit "(") pExpr (pLit ")")

pAExpr :: Parser CoreExpr
pAExpr = pEVar `pAlt` pENum `pAlt` pPExpr

mk_def v _ e = (v, e)
mk_let a b _ d = ELet (if a == "let" then nonRecursive else recursive) b d

-- Definitions

pDefn :: Parser (Name, CoreExpr)
pDefn = pThen3 mk_def pVar (pLit "=") pExpr

pDefns :: Parser [(Name, CoreExpr)]
pDefns = pOneOrMoreWithSep pDefn (pLit ";")

-- 

mk_alter :: a -> Int -> b -> [Name] -> c -> CoreExpr -> CoreAlt
mk_alter _ n _ vars _ expr = (n, vars, expr)

pAlter :: Parser CoreAlt
pAlter = pThen6 mk_alter (pLit "<") 
		 			 			 pNum 
								 (pLit ">") 
								 (pZeroOrMore pVar) 
								 (pLit "->") 
								 pExpr

pAlters :: Parser [CoreAlt]
pAlters = pOneOrMoreWithSep pAlter (pLit ";")

mk_case _ e _ alts = ECase e alts

pCase :: Parser CoreExpr
pCase = pThen4 mk_case (pLit "case") pExpr (pLit "of") pAlters

pLet :: Parser CoreExpr
pLet = pThen4 mk_let ((pLit "let") `pAlt` (pLit "letrec")) pDefns (pLit "in") pExpr

pExpr :: Parser CoreExpr
pExpr = pLet `pAlt` pCase `pAlt` pExpr1

data PartialExpr = NoOp | FoundOp Name CoreExpr

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2

--pMakeExpr inner right = pThen assembleOp inner right

--pMakeExprc op rightside = (pThen FoundOp (pLit op) rightside) `pAlt` (pEmpty NoOp)

--pMakeExprSevc ops rightside = (pThen FoundOp ops rightside) `pAlt` (pEmpty NoOp)

--pExpr1 :: Parser CoreExpr
--pExpr1 = pMakeExpr pExpr2 pExpr1c

--pExpr1c :: Parser PartialExpr
--pExpr1c = pMakeExprc "|" pExpr1

pExpr1 = pThen assembleOp pExpr2 pExpr1c
pExpr1c = (pThen FoundOp (pLit "|") pExpr1) `pAlt` (pEmpty NoOp)

-- **

--pExpr2 :: Parser CoreExpr
--pExpr2 = pMakeExpr pExpr3 pExpr2c

--pExpr2c :: Parser PartialExpr
--pExpr2c = pMakeExprc "&" pExpr2

pExpr2 = pThen assembleOp pExpr3 pExpr2c
pExpr2c = (pThen FoundOp (pLit "&") pExpr2) `pAlt` (pEmpty NoOp)

-- **

relop :: Parser Name
relop = (pLit "<=") `pAlt` (pLit "<") `pAlt` (pLit ">=") `pAlt` (pLit ">") `pAlt` (pLit "==")`pAlt` (pLit "~=")

--pExpr3 :: Parser CoreExpr
--pExpr3 = pMakeExpr pExpr4 pExpr3c

--pExpr3c :: Parser PartialExpr
--pExpr3c = pMakeExprSevc relop pExpr3

pExpr3 = pThen assembleOp pExpr4 pExpr3c
pExpr3c = (pThen FoundOp relop pExpr3) `pAlt` (pEmpty NoOp)

-- **

addop :: Parser Name
addop = (pLit "-") `pAlt` (pLit "+")

--pExpr4 :: Parser CoreExpr
--pExpr4 = pMakeExpr pExpr5 pExpr4c

--pExpr4c :: Parser PartialExpr
--pExpr4c = pMakeExprSevc addop pExpr4

pExpr4 = pThen assembleOp pExpr5 pExpr4c
pExpr4c = (pThen FoundOp addop pExpr4) `pAlt` (pEmpty NoOp)


-- **

multop :: Parser Name
multop = (pLit "*") `pAlt` (pLit "/")

--pExpr5 :: Parser CoreExpr
--pExpr5 = pMakeExpr pExpr6 pExpr5c

--pExpr5c :: Parser PartialExpr
--pExpr5c = pMakeExprSevc multop pExpr5

pExpr5 = pThen assembleOp pExpr6 pExpr5c
pExpr5c = (pThen FoundOp multop pExpr5) `pAlt` (pEmpty NoOp)


-- **

pExpr6 :: Parser CoreExpr
pExpr6 = pAp `pAlt` pAExpr
-- 
-- **

pSc :: Parser CoreScnDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr

pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

syntax :: [Token] -> CoreProgram
syntax = take_first_parse . pProgram
		 	where
			take_first_parse ((prog, []) : others) = prog
			take_first_parse (parse : others) = take_first_parse others
			take_first_parse _ = error "Syntax error!"

parse :: String -> CoreProgram
parse = syntax . clex

-- 2. Template Instantiation
-- Some utils first

type Addr = Int

data Node = NAp Addr Addr
	  		 | NSupercomb Name [Name] CoreExpr
			 | NNum Int
			 | NInd Addr

type TiStack = [Addr]

data TiDump = DummyTiDump
initialTiDump = DummyTiDump

type Assoc a b = [(a, b)]

justOrFail :: Maybe a -> String -> a
justOrFail v e = case v of
			  	 	Just a -> a
					_ -> error e

aLookup :: (Eq a, Show a) => a -> Assoc a b -> b -> b
--aLookup k l d = justOrFail (lookup k l) ("Error looking for " ++ (show k))
aLookup k l d = case (lookup k l) of
		  	 	  	 		Just v -> v
					 		_ -> d

aDomain :: Assoc a b -> [a]
aDomain = map fst

aRange :: Assoc a b -> [b]
aRange = map snd

type Heap a = (Int, [Int], Assoc Int a)
type TiHeap = Heap Node

hInitial :: Heap a
hInitial = (0, [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next:free), cts) n = ((size + 1, free, (next, n) : cts), next)

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove _ [] = []
remove k ((k1, _):rest) | k == k1 = remove k rest
remove k (h:rest) = h : (remove k rest)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, cts) a n = (size, free, (a, n) : remove a cts)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, cts) a = (size - 1, a:free, remove a cts)

hLookup :: Heap a -> Addr -> a
hLookup (_, _, ls) a = case (lookup a ls) of
		  				 	  		 Just v -> v
									 _ -> error "Couldnt find ..."

hAddresses :: Heap a -> [Addr]
hAddresses (_, _, ls) = aDomain ls

hSize :: Heap a -> Int
hSize (_, _, ls) = length ls

hNull :: Addr
hNull = 0

hIsNull :: Addr -> Bool
hIsNull a = a == 0

type TiGlobals = Assoc Name Addr

type TiStats = Int

tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps s = s+1

tiStatGetSteps :: TiStats -> Int
tiStatGetSteps s = s

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats fn (stack, dump, head, globals, stats) = (stack, dump, head, globals, fn stats)

extraPreludeDefs = []

mapAccuml :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccuml _ acc [] = (acc, [])
mapAccuml f acc (h:r) = (acc'', parsed:list)
									where
									(acc', parsed) = f acc h
									(acc'', list) = (mapAccuml f acc' r)

buildInitialHeap :: [CoreScnDefn] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs = mapAccuml allocateSc hInitial sc_defs


allocateSc :: TiHeap -> CoreScnDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
			  		 		  		  		  	 where
												 (heap', addr) = hAlloc heap (NSupercomb name args body)

compile :: CoreProgram -> TiState
compile program =
		  (initialStack, initialTiDump, initialHeap, globals, tiStatInitial)
		  where
		  sc_defs = program ++ preludeDefs ++ extraPreludeDefs
		  (initialHeap, globals) = buildInitialHeap sc_defs
		  initialStack = [addressOfMain]
		  addressOfMain = aLookup "main" globals (error "Couldnt find main")


doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSteps state

tiFinal :: TiState -> Bool
tiFinal ([sole_addr], dump, heap, globals, stats) = isDataNode (hLookup heap sole_addr)
tiFinal ([], _, _, _, _) = error "Empty stack!"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _ = False

step :: TiState -> TiState
step state = dispatch (hLookup heap (head stack))
	  		  	 where
				 (stack, dump, heap, globals, stats) = state
				 dispatch (NNum n) = numStep state n
				 dispatch (NAp a1 a2) = apStep state a1 a2
				 dispatch (NSupercomb sc args body) = scStep state sc args body
				 dispatch (NInd a1) = step (a1:stack, dump, heap, globals, stats)

numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, stats) a1 a2 = (a1 : stack, dump, heap, globals, stats)

eval :: TiState -> [TiState]
eval state = state : rest_states
	  		  	 where
				 rest_states | tiFinal state = []
				 				 | otherwise = eval next_state
				 next_state = doAdmin (step state)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body = 
		 			(new_stack, dump, nnew_heap, globals, stats)
					where
					new_stack = result_addr : (drop (length arg_names + 1) stack)
					nnew_heap = hUpdate new_heap result_addr (NInd result_addr)
					(new_heap, result_addr) = instantiate body heap env
					env = arg_bindings ++ globals
					arg_values = getargs heap stack
					arg_bindings = zip arg_names arg_values 

--if (length arg_names == length arg_values) then								 		
--										else
--										  (error "Not enough arguments!")

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc:stack) =
		  map get_arg stack
		  where get_arg addr = arg where (NAp fun arg) = hLookup heap addr

instantiate :: CoreExpr -> TiHeap -> Assoc Name Addr -> (TiHeap, Addr)
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = 
				hAlloc heap2 (NAp a1 a2) 
				where 
				(heap1, a1) = instantiate e1 heap env
				(heap2, a2) = instantiate e2 heap1 env

instantiate (EVar v) heap env =
				(heap, aLookup v env (error $ "Couldn't instantiate EVar " ++ show v ++ 
						 			  				 	" with env " ++ show env))

instantiate (EConstr tag arity) heap env = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env = instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env = error "Can't instantiate case expr"

instantiateConstr tag arity heap env = error "Can't instantiate constructors yet"

instantiateLet False defs body heap env = instantiateNRLet defs body heap env
instantiateLet True  defs body heap env = instantiateRLet defs body heap env

instantiateNRLet defs body heap env = 
					  -- call instantiate passing the augmented env and the body		  
					  instantiate body nheap env2
				  	  where
					  -- instantiate the right hand side of each def
					  (nheap, instantiateddefs) = mapAccuml (\ nheap (name, expr) ->
					  			 						 				 	 let (nheap2, addr) = instantiate expr nheap env in
					  			 						 				 	 (nheap2, (name, addr))) heap defs
					  -- augment the env to bind the names in defs to the addresses of the defs
					  env2 = env ++ instantiateddefs

instantiateRLet defs body heap env = 
					  -- call instantiate passing the augmented env and the body		  
					  instantiate body nheap env2
				  	  where
					  -- instantiate the right hand side of each def
					  (nheap, instantiateddefs) = mapAccuml (\ nh (name, expr) ->
					  			 						 				 	 -- only diff here! in non-strict (functional?) langs (Haskell, Miranda, ...), is very easy
					  			 						 				 	 let (nheap2, addr) = instantiate expr nh env2 in
					  			 						 				 	 (nheap2, (name, addr))) heap defs
					  -- augment the env to bind the names in defs to the addresses of the defs
					  env2 = env ++ instantiateddefs


showResults :: [TiState] -> String
showResults states = iDisplay (iConcat [ iLayn (map showState states), showStats (last states)])

showHeap :: TiHeap -> Iseq
showHeap (size, _, heap) = iIndent $ iConcat [ iStr "Heap(", iNum size, iStr ")[", 
					 	 		 			  	 			iIndent (iInterleave iNewline (map show_heap_item heap)), iStr "]"]
			  	 where
				 show_heap_item (addr, value) = iConcat [ showFWAddr addr, 
				 					 		  					  	 iStr ": ", 
																	 showNode value]

showState :: TiState -> Iseq
showState (stack, dump, heap, globals, stats) = iConcat [ showStack heap stack, iNewline,
			 												 			  	 showHeap heap, iNewline]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack = iConcat [iStr "Stk[", 
			 				  			 iIndent $ iInterleave iNewline $ map show_stack_item stack,
										 iStr " ]"]
			 				  where
							  show_stack_item addr = iConcat [ showFWAddr addr, iStr ": ", 
							  							  	 			  showStkNode heap (hLookup heap addr)]

showStkNode :: TiHeap -> Node -> Iseq
showStkNode heap (NAp fun_addr arg_addr) = iConcat [iStr "NAp ", showFWAddr fun_addr, 
					  		 			 			  	 			 iStr " ", showFWAddr arg_addr, 
																	 iStr "(", 
																	 showNode (hLookup heap arg_addr),
																	 iStr ")"]

showStkNode heap node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2) = iConcat [iStr "NAp ", showAddr a1, iStr " ", showAddr a2]
showNode (NSupercomb name arg body) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = (iStr "NNum ") `iAppend` (iNum n)
showNode (NInd addr) = (iStr "NInd ") `iAppend` (showAddr addr)


showAddr :: Addr -> Iseq
showAddr addr = iStr $ show addr

showFWAddr :: Addr -> Iseq
showFWAddr addr = iStr (space (4 - length str) ++ str)
			  		 	where
						str = show addr


showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats) =
			 iConcat [iNewline, iNewline, 
			 			iStr "Total number of steps = ", 
						iNum (tiStatGetSteps stats)]

runProg :: String -> String
runProg = showResults . eval . compile . parse

-- 

main2 f = do
		  content <- readFile f
		  let results = runProg content
		  print results

main f = do
		  content <- readFile f
		  let lexemes = clex content 
		  print "Lexemes"
		  print lexemes
		  let parsed = syntax lexemes
		  print "All results"
--		  let allres = pProgram lexemes
--		  print allres
		  print "Parsed"
		  print parsed
		  print "Pretty"
		  print $ iDisplay $ pprProgram parsed
		  return ()

