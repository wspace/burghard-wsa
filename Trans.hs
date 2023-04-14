module Trans where
import Data.Char

--translateWSA a = translateWSA' (a ++ ".wsa") (a ++ ".pws") (a ++ ".ws")



translateWSA :: String -> Bool -> IO()
translateWSA filename extSyntax = translateWSAOptions filename [] extSyntax

getWSAOptions :: String -> IO ()
getWSAOptions filename = do
    stringList <- (readFileToStringListWithIncludes filename)
    print (getStringListOptions stringList)

uniqueList :: Eq a => [a] -> [a]
uniqueList list = foldr uniqueList' [] list
	where
		uniqueList' :: Eq a => a -> [a] -> [a]
		uniqueList' x xs | elem x xs = xs
		uniqueList' x xs = (x:xs)


translateWSAOptions :: String -> [String] -> Bool -> IO()
translateWSAOptions filename options extSyntax = do
    stringList <- (readFileToStringListWithIncludes filename)
    let stringListPrecompiled = (precompileStringList stringList options) in
      let ops = (stringListToOps stringListPrecompiled) in
        let opsNorm = normalizeOps ops in
          let pws = translateOpsToPWS opsNorm (getLabels opsNorm) extSyntax in
            let ws = (translatePWSToWS pws) in
              let wsEnded = (translateWSToEndedWS ws) in do
                writeFile (filename++".pws") (pws)
                writeFile (filename++".ws" ) (wsEnded)

translateWSToEndedWS :: String -> String
translateWSToEndedWS s = s ++ "\n\n\nquit\n\n\n"

{-
-- ret ret add to make it compatible to the interpreter
translatePWSToEndedPWS :: String -> String
translatePWSToEndedPWS s = s ++ "\n\n" ++ (take (fromInteger (6 - (getCAtEndCount 0 s))) (repeat 'c')) ++ "\n\n"
	where
		getCAtEndCount :: Integer -> String -> Integer
		getCAtEndCount i ('a':xs) = getCAtEndCount 0 xs
		getCAtEndCount i ('b':xs) = getCAtEndCount 0 xs
		getCAtEndCount i ('c':xs) | i < 5 = getCAtEndCount (i+1) xs
		getCAtEndCount i ('c':xs) = error "too many c are following can not end this file for the interpreter"
		getCAtEndCount i (_:xs) = getCAtEndCount i xs
		getCAtEndCount i [] = i
-}

readFileToStringListWithIncludes :: String -> IO [[String]]
readFileToStringListWithIncludes name = precompileInclude [] (name:[]) []


fileToString :: String -> String
fileToString = removeComments . filterTab . (map toLower)

precompileInclude :: [String] -> [String] -> [[String]] -> IO [[String]]
precompileInclude filesDone filesToDo (("include":file:[]):xs) =
	if( elem file filesDone) then
		(precompileInclude filesDone filesToDo xs)
		else
		(precompileInclude filesDone (file:filesToDo) xs)

precompileInclude filesDone filesToDo (x:xs) = do
	next <-(precompileInclude filesDone filesToDo xs)
	return (x:next)

precompileInclude filesDone (x:xs) [] = do
	file <- readFile (x++".wsa")
	let str = fileToString file in
		let strList = stringToStringList str in
			(precompileInclude (x:filesDone) xs strList)

precompileInclude filesDone [] [] = do return []

precompileStringList :: [[String]] -> [String] -> [[String]]
--precompileStringList a = a
precompileStringList = precompileOption

precompileOption :: [[String]] -> [String] -> [[String]]
precompileOption code options =
	let (done,rest) = precompileOptionCode options code in
		if( rest == [] ) then done else (error "endoption without ifoption")


getStringListOptions :: [[String]] -> [String]
getStringListOptions a = uniqueList (getStringListOptions' a)

getStringListOptions' :: [[String]] -> [String]
getStringListOptions' (("ifoption":a:[]):xs) = a:(getStringListOptions xs)
getStringListOptions' (("elseifoption":a:[]):xs) = a:(getStringListOptions xs)
getStringListOptions' (_:xs) = (getStringListOptions xs)
getStringListOptions' [] = []

precompileOptionCode :: [String] -> [[String]] -> ([[String]],[[String]])
precompileOptionCode opts (("option":a:[]):xs) = precompileOptionCode (a:opts) xs
precompileOptionCode opts (("ifoption":a:[]):xs) =
	let (done,rest) = precompileOptionIf False opts (("ifoption":a:[]):xs) in
		let (done2,rest2) = precompileOptionCode opts rest in
			(done ++ done2,rest2)
precompileOptionCode opts (("elseoption":[]):xs) = ([], (("elseoption":[]):xs))
precompileOptionCode opts (("elseifoption":a:[]):xs) = ([], (("elseifoption":a:[]):xs))
precompileOptionCode opts (("endoption":[]):xs) = ([], (("endoption":[]):xs))
precompileOptionCode opts (x:xs) = let (done,rest) = precompileOptionCode opts xs in ((x:done),rest)
precompileOptionCode opts [] = ([],[])

precompileOptionIf :: Bool -> [String] -> [[String]] -> ([[String]],[[String]])
precompileOptionIf False ops (("ifoption":a:[]):xs) =
	if( elem a ops ) then
		let (done,rest) = precompileOptionCode ops xs in
			let (done2,rest2) = precompileOptionIf True ops rest in
				(done++done2,rest2)
		else
		let (done,rest) = precompileOptionCode ops xs in
			let (done2,rest2) = precompileOptionIf False ops rest in
				(done2,rest2)

precompileOptionIf True ops (("elseoption":[]):xs) =
		let (done,rest) = precompileOptionCode ops xs in
			let (done2,rest2) = precompileOptionIf True ops rest in
				(done2,rest2)

precompileOptionIf False ops (("elseoption":[]):xs) =
		let (done,rest) = precompileOptionCode ops xs in
			let (done2,rest2) = precompileOptionIf True ops rest in
				(done++done2,rest2)

precompileOptionIf True ops (("elseifoption":a:[]):xs) =
		let (done,rest) = precompileOptionCode ops xs in
			let (done2,rest2) = precompileOptionIf True ops rest in
				(done2,rest2)

precompileOptionIf False ops (("elseifoption":a:[]):xs) =
	precompileOptionIf False ops (("ifoption":a:[]):xs)
{-
	if( elem a ops ) then
		let (done,rest) = precompileOptionCode ops xs in
			let (done2,rest2) = precompileOptionIf True ops rest in
				(done++done2,rest2)
		else
		let (done,rest) = precompileOptionCode ops xs in
			let (done2,rest2) = precompileOptionIf False ops rest in
				(done2,rest2)
-}

precompileOptionIf _ ops (("endoption":[]):xs) = ([],xs)
precompileOptionIf _ ops [] = error "if without endif"




data ValueString = ValueStringDirect String | ValueStringVariable String
	deriving (Show)

data ValueInteger = ValueIntegerDirect Integer | ValueIntegerVariable String
	deriving (Show)


data Op =
	Push ValueInteger | Pop |
	Label String |
	Doub | Swap |
--	Add | Sub | Mul | Div | Mod |
--	Store | Retrive |
	Call String | Jump String | JumpZ String | JumpN String |
	Ret | Exit |
	OutN | OutC | InN | InC |
	Noop |
	Include String |
	Debug_PrintStack |
	Debug_PrintHeap |
	Retrive (Maybe ValueInteger) | Store (Maybe ValueInteger) |
	Test ValueInteger | PushS ValueString |
	JumpP String | JumpNZ String |
	JumpNP String | JumpPZ String |
	Add (Maybe ValueInteger) |
	Sub (Maybe ValueInteger) |
	Mul (Maybe ValueInteger) |
	Div (Maybe ValueInteger) |
	Mod (Maybe ValueInteger) |
	ValueS String ValueString |
	ValueI String ValueInteger
	deriving (Show)


stringToStringList = parse



filterTab :: String -> String
filterTab s = map sic s
	where
		sic '\t' = ' '
		sic a = a

stringListToOps :: [[String]] -> [Op]
stringListToOps list = map stringListToOp list

stringListToOp :: [String] -> Op
stringListToOp [] = Noop
stringListToOp ("push": ('_':a):[]) = Push (ValueIntegerVariable ('_':a))
stringListToOp ("push": a:[]) = Push (ValueIntegerDirect (stringToInteger a))
stringListToOp ("pushs": ('_':a):[]) = PushS (ValueStringVariable ('_':a))
stringListToOp ("pushs": a:[]) = PushS (ValueStringDirect a)
stringListToOp ("pop":[]) = Pop
stringListToOp ("label":a:[]) = Label a
stringListToOp ("doub":[]) = Doub
stringListToOp ("swap":[]) = Swap
stringListToOp ("add":[]) = Add Nothing
stringListToOp ("sub":[]) = Sub Nothing
stringListToOp ("mul":[]) = Mul Nothing
stringListToOp ("div":[]) = Div Nothing
stringListToOp ("mod":[]) = Mod Nothing
stringListToOp ("add": ('_':a):[]) = Add (Just (ValueIntegerVariable ('_':a)))
stringListToOp ("sub": ('_':a):[]) = Sub (Just (ValueIntegerVariable ('_':a)))
stringListToOp ("mul": ('_':a):[]) = Mul (Just (ValueIntegerVariable ('_':a)))
stringListToOp ("div": ('_':a):[]) = Div (Just (ValueIntegerVariable ('_':a)))
stringListToOp ("mod": ('_':a):[]) = Mod (Just (ValueIntegerVariable ('_':a)))
stringListToOp ("add": a:[]) = Add (Just (ValueIntegerDirect (stringToInteger a)))
stringListToOp ("sub": a:[]) = Sub (Just (ValueIntegerDirect (stringToInteger a)))
stringListToOp ("mul": a:[]) = Mul (Just (ValueIntegerDirect (stringToInteger a)))
stringListToOp ("div": a:[]) = Div (Just (ValueIntegerDirect (stringToInteger a)))
stringListToOp ("mod": a:[]) = Mod (Just (ValueIntegerDirect (stringToInteger a)))
stringListToOp ("store":[]) = Store Nothing
stringListToOp ("store":('_':a):[]) = Store (Just (ValueIntegerVariable ('_':a) ))
stringListToOp ("store":a:[]) = Store (Just (ValueIntegerDirect (stringToInteger a)))
stringListToOp ("retrive":[]) = Retrive Nothing
stringListToOp ("retrive":('_':a):[]) = Retrive (Just (ValueIntegerVariable ('_':a) ))
stringListToOp ("retrive":a:[]) = Retrive (Just (ValueIntegerDirect (stringToInteger a)))
stringListToOp ("call":a:[]) = Call a
stringListToOp ("jump":a:[]) = Jump a
stringListToOp ("jumpz":a:[]) = JumpZ a
stringListToOp ("jumpn":a:[]) = JumpN a
stringListToOp ("jumpp":a:[]) = JumpP a
stringListToOp ("jumpnz":a:[]) = JumpNZ a
stringListToOp ("jumppz":a:[]) = JumpPZ a
stringListToOp ("jumpnp":a:[]) = JumpNP a
stringListToOp ("jumppn":a:[]) = JumpNP a
stringListToOp ("include":a:[]) = Include a
stringListToOp ("ret":[]) = Ret
stringListToOp ("exit":[]) = Exit
stringListToOp ("outn":[]) = OutN
stringListToOp ("outc":[]) = OutC
stringListToOp ("inn":[]) = InN
stringListToOp ("inc":[]) = InC
stringListToOp ("debug_printstack":[]) = Debug_PrintStack
stringListToOp ("debug_printheap":[]) = Debug_PrintHeap
stringListToOp ("test": ('_':a):[]) = Test (ValueIntegerVariable ('_':a))
stringListToOp ("test": a:[]) = Test (ValueIntegerDirect (stringToInteger a))
stringListToOp ("valuestring": ('_':name):('_':a):[]) = ValueS ('_':name) (ValueStringVariable ('_':a))
stringListToOp ("valuestring": ('_':name):a:[]) = ValueS ('_':name) (ValueStringDirect a)
stringListToOp ("valueinteger": ('_':name):('_':a):[]) = ValueI ('_':name) (ValueIntegerVariable ('_':a))
stringListToOp ("valueinteger": ('_':name):a:[]) = ValueI ('_':name) (ValueIntegerDirect (stringToInteger a))
stringListToOp x = error ("can not parse: " ++ (show x)) --(concat (map (\y->' ':y) x)))

stringToInteger :: String -> Integer
stringToInteger ch = (read ch)::Integer
{-
stringToInteger :: String -> Integer
stringToInteger [] = error "StringToInteger []"
stringToInteger a = stringToInteger' a 0
	where
		stringToInteger' [] i = i
		stringToInteger' (x:[]) i = i * 10 + toInteger (digitToInt x)
		stringToInteger' (x:xs) i = stringToInteger' xs (i * 10 + (toInteger (digitToInt x)))
-}
filterStringList :: [[String]] -> [[String]]
filterStringList list = filter (\x->x/=[]) (map (filter (\x->x/="")) list)

parse :: String -> [[String]]
parse a = filterStringList (parse' a)

parse' :: String -> [[String]]
parse' [] = []
parse' a = (parseLine (getLine' a)): (parse' (dropLine a))

dropLine :: String -> String
dropLine [] = []
dropLine ('\n':xs) = xs
dropLine (x:xs) = dropLine xs

getLine' :: String -> String
getLine' [] = []
getLine' ('\n':xs) = []
getLine' (x:xs) = x:getLine' xs

parseLine :: String -> [String]
parseLine [] = []
parseLine a = (getToken a):(parseLine (dropToken a))

dropToken :: String -> String
dropToken [] = []
dropToken (' ':xs) = xs
dropToken ('"':xs) = dropTokenTill '"' xs

dropToken (x:xs) = dropToken' xs
	where
		dropToken' ('"':xs) = ('"':xs)
		dropToken' a = dropToken a


getToken :: String -> String
getToken ('"':xs) = getTokenTill '"' xs

getToken [] = []
getToken (' ':xs) = []
getToken (x:xs) = x:(getToken' xs)
	where
		getToken' ('"':xs) = []
		getToken' a = getToken a


dropTokenTill c (x:xs) | c==x = xs
dropTokenTill c (x:xs) = dropTokenTill c xs
dropTokenTill c [] = error ("terminating \" not found (drop)")
getTokenTill c (x:xs) | c == x = []
getTokenTill c (x:xs) = x:(getTokenTill c xs)
getTokenTill c [] = error ("terminating \" not found (get)")


-- comment
removeComments :: String -> String
removeComments = (removeCommentsBlocks 0). removeCommentsLines

removeCommentsBlocks :: Integer -> String -> String
removeCommentsBlocks i ('"':xs) = '"':(getTokenTill '"' xs) ++ "\"" ++ (removeCommentsBlocks i (dropTokenTill '"' xs))
removeCommentsBlocks i ('{':'-':xs) = removeCommentsBlocks (i+1) xs
removeCommentsBlocks i ('-':'}':xs) = if( i > 0 ) then (removeCommentsBlocks (i-1) xs) else (error "-} without {-")
removeCommentsBlocks i (x:xs) | i == 0 = x:(removeCommentsBlocks i xs)
removeCommentsBlocks i (x:xs) | i > 0 = (removeCommentsBlocks i xs)
removeCommentsBlocks 0 [] = []
removeCommentsBlocks i [] = (error "{- without -}")

removeCommentsLines :: String -> String
removeCommentsLines [] = []
removeCommentsLines ('"':xs) = '"':(getTokenTill '"' xs) ++ "\"" ++ (removeCommentsLines (dropTokenTill '"' xs))
removeCommentsLines (';':xs) = removeCommentsLines (skipToNewLine xs)
removeCommentsLines ('-':'-':xs) = removeCommentsLines (skipToNewLine xs)
removeCommentsLines (x:xs) = x:(removeCommentsLines xs)


--	where
skipToNewLine ('\n':xs) = '\n':xs -- keep the new line included
skipToNewLine (x:xs) = skipToNewLine xs
skipToNewLine [] = []


type Label = (String,Integer)

getLabels :: [Op] -> [Label]
getLabels ops = getLabelsSic ops [] 0
	where
		getLabelsSic :: [Op] -> [Label] -> Integer -> [Label]
		getLabelsSic ((Label l):xs) ls i | hasLabel ls l = error ("doublicate Label '" ++ l ++ "'")
		getLabelsSic ((Label l):xs) ls i = getLabelsSic xs ((l,i):ls) (i+1)
		getLabelsSic (_:xs) ls i = getLabelsSic xs ls i
		getLabelsSic [] ls i = ls
		hasLabel :: [Label] -> String -> Bool
		hasLabel ((name1,i):xs) name2 | name1 == name2 = True
		hasLabel ((name1,i):xs) name2 = hasLabel xs name2
		hasLabel [] _ = False

integerToString :: Integer -> String
integerToString i = (if(i>=0) then 'a' else 'b') : (reverse (integerToStringSic (abs i))) ++ "c"
	where
        integerToStringSic 0 = "a"
        integerToStringSic 1 = "b"
        integerToStringSic i = ( if( (mod i 2) == 0 ) then 'a' else 'b' ) : (integerToStringSic (div i 2))

labelToString :: [Label] -> String -> String
labelToString ((name1,number):xs) name2 | (name1 == name2) = integerToString number
labelToString (_:xs) name = labelToString xs name
labelToString [] name = error ("label not found: '" ++ name ++ "'")

simplifyIncludeOps :: [Op] -> [Op]
simplifyIncludeOps o = concat (map simplifyIncludeOp o)

simplifyIncludeOp :: Op -> [Op]
simplifyIncludeOp o = [o]
--simplifyIncludeOp (Include f) = simplifyIncludeOps (fileToOps f)
simplifyIncludeOp (Include f) = []


simplifyOps :: [Op] -> [Op]
simplifyOps o = simplifyOps' o 1
	where
		simplifyOps' (x:xs) i = concat((simplifyOp x i):(simplifyOps' xs (i+1)):[])
		simplifyOps' [] i = []

simplifyOp :: Op -> Integer -> [Op]
simplifyOp (PushS (ValueStringDirect [])) i = [(Push (ValueIntegerDirect 0))]
simplifyOp (PushS (ValueStringDirect (x:xs))) i = (simplifyOp (PushS (ValueStringDirect xs)) i) ++ [(Push (ValueIntegerDirect (toInteger (ord x))))]
simplifyOp (JumpP s) i =
	let s1 = (getLabelFromIndex i 0) in
		((Doub):(JumpN s1):(Doub):(JumpZ s1):(Pop):(Jump s):(Label s1):(Pop):[])
simplifyOp (JumpNP s) i =
	let s1 = (getLabelFromIndex i 1) in
		((JumpZ s1):(Jump s):(Label s1):[])
simplifyOp (JumpNZ s) i =
	let s1 = (getLabelFromIndex i 2) in
		let s2 = (getLabelFromIndex i 3) in
			(Doub:(JumpN s1):Doub:(JumpZ s1):(Jump s2):(Label s1):(Pop):(Jump s):(Label s2):(Pop):[])
simplifyOp (JumpPZ s) i =
	let s1 = (getLabelFromIndex i 4) in
		((JumpN s1):(Jump s):(Label s1):[])
simplifyOp (Test s) i =	((Doub):(Push s):(Sub Nothing):[])
simplifyOp (Retrive (Just s)) i =	((Push s):(Retrive Nothing):[])
simplifyOp (Store (Just s)) i =	((Push s):(Swap):(Store Nothing):[])
simplifyOp (Add (Just s)) i =	((Push s):(Add Nothing):[])
simplifyOp (Sub (Just s)) i =	((Push s):(Sub Nothing):[])
simplifyOp (Mul (Just s)) i =	((Push s):(Mul Nothing):[])
simplifyOp (Div (Just s)) i =	((Push s):(Div Nothing):[])
simplifyOp (Mod (Just s)) i =	((Push s):(Mod Nothing):[])
-- simplifyOp (Include f) i =
simplifyOp a i = [a]

data Value = ValueString String String | ValueInteger String Integer

findValueInteger :: [Value] -> String -> Integer
findValueInteger (( ValueString name1 val):xs) name2 = findValueInteger xs name2
findValueInteger (( ValueInteger name1 val):xs) name2 | name1 == name2 = val
findValueInteger (( ValueInteger name1 val):xs) name2 = findValueInteger xs name2
findValueInteger [] name2 = error ("ValueInteger " ++ name2 ++ " not found")

findValueString :: [Value] -> String -> String
findValueString (( ValueInteger name1 val):xs) name2 = findValueString xs name2
findValueString (( ValueString name1 val):xs) name2 | name1 == name2 = val
findValueString (( ValueString name1 val):xs) name2 = findValueString xs name2
findValueString [] name2 = error ("ValueString " ++ name2 ++ " not found")

simplifyValueOp :: [Value] -> Op -> ([Op],[Value])
simplifyValueOp values (ValueS name (ValueStringDirect val)) = ([],(ValueString name val):values)
simplifyValueOp values (ValueS name (ValueStringVariable val)) = ([],(ValueString name (findValueString values val)):values)
simplifyValueOp values (ValueI name (ValueIntegerDirect val)) = ([],(ValueInteger name val):values)
simplifyValueOp values (ValueI name (ValueIntegerVariable val)) = ([],(ValueInteger name (findValueInteger values val)):values)
simplifyValueOp values (PushS (ValueStringVariable name)) = ([PushS (ValueStringDirect (findValueString values name))],values)
simplifyValueOp values (Push (ValueIntegerVariable name)) = ([Push (ValueIntegerDirect (findValueInteger values name))],values)
simplifyValueOp values (Retrive (Just (ValueIntegerVariable name))) = ([Retrive (Just (ValueIntegerDirect (findValueInteger values name)))],values)
simplifyValueOp values (Store (Just (ValueIntegerVariable name))) = ([Store (Just (ValueIntegerDirect (findValueInteger values name)))],values)
simplifyValueOp values (Test (ValueIntegerVariable name)) = ([Test (ValueIntegerDirect (findValueInteger values name))],values)
simplifyValueOp values (Add (Just (ValueIntegerVariable name))) = ([Add (Just (ValueIntegerDirect (findValueInteger values name)))],values)
simplifyValueOp values (Sub (Just (ValueIntegerVariable name))) = ([Sub (Just (ValueIntegerDirect (findValueInteger values name)))],values)
simplifyValueOp values (Mul (Just (ValueIntegerVariable name))) = ([Mul (Just (ValueIntegerDirect (findValueInteger values name)))],values)
simplifyValueOp values (Div (Just (ValueIntegerVariable name))) = ([Div (Just (ValueIntegerDirect (findValueInteger values name)))],values)
simplifyValueOp values (Mod (Just (ValueIntegerVariable name))) = ([Mod (Just (ValueIntegerDirect (findValueInteger values name)))],values)
simplifyValueOp values op = ([op],values)


simplifyValueOps :: [Op] -> [Op]
simplifyValueOps ops = simpilfyValueOps' ops []
	where
  simpilfyValueOps' :: [Op] -> [Value] -> [Op]
  simpilfyValueOps' [] values = []
  simpilfyValueOps' (x:xs) values =
    let (nops,nvalues) = (simplifyValueOp values x) in
      nops ++ (simpilfyValueOps' xs nvalues)


normalizeOps :: [Op] -> [Op]
-- normalizeOps = simplifyOps . simplifyIncludeOps
normalizeOps = simplifyOps . simplifyIncludeOps . simplifyValueOps

getLabelFromIndex :: Integer -> Integer -> String
getLabelFromIndex i1 i2 = "__trans__" ++ (show i1) ++ "__" ++ (show i2) ++ "__"
	-- labels created by the translator begin with "__trans__x__y__

translateOpsToPWS :: [Op] -> [Label] -> Bool -> String
translateOpsToPWS (x:xs) labels extSyntax = (translateOpToPWS x labels extSyntax) ++ "\n" ++ (translateOpsToPWS xs labels extSyntax)
translateOpsToPWS [] labels extSyntax = "\n\n\n"

translateOpToPWS :: Op -> [Label] -> Bool -> String
translateOpToPWS (Push (ValueIntegerDirect i)) ls _ = "aa" ++ " " ++ (integerToString i)
translateOpToPWS (PushS (ValueStringDirect a)) ls _ = error "PushS not allowed here (must been translated)"
translateOpToPWS (Pop) ls _ = "acc"
translateOpToPWS (Label l) ls _ = "caa" ++ " " ++ (labelToString ls l)
translateOpToPWS (Doub) ls _  = "aca"
translateOpToPWS (Swap) ls _  = "acb"
translateOpToPWS (Add Nothing) ls _ = "baaa"
translateOpToPWS (Sub Nothing) ls _ = "baab"
translateOpToPWS (Mul Nothing) ls _ = "baac"
translateOpToPWS (Div Nothing) ls _ = "baba"
translateOpToPWS (Mod Nothing) ls _ = "babb"
translateOpToPWS (Store Nothing) ls _ = "bba"
translateOpToPWS (Retrive Nothing) ls _ = "bbb"
translateOpToPWS (Call l) ls _ = "cab" ++ " " ++ (labelToString ls l)
translateOpToPWS (Jump l) ls _ = "cac" ++ " " ++ (labelToString ls l)
translateOpToPWS (JumpZ l) ls _ = "cba" ++ " " ++ (labelToString ls l)
translateOpToPWS (JumpN l) ls _ = "cbb" ++ " " ++ (labelToString ls l)
translateOpToPWS (Ret) ls _ = "cbc"
translateOpToPWS (Exit) ls _ = "ccc"
translateOpToPWS (OutC) ls _ = "bcaa"
translateOpToPWS (OutN) ls _ = "bcab"
translateOpToPWS (InC) ls _ = "bcba"
translateOpToPWS (InN) ls _ = "bcbb"
translateOpToPWS (Noop) ls _ = ""
translateOpToPWS (Debug_PrintStack) ls True = "ccaaa"
translateOpToPWS (Debug_PrintHeap) ls True = "ccaab"
translateOpToPWS (Debug_PrintStack) ls False = ""
translateOpToPWS (Debug_PrintHeap) ls False = ""
translateOpToPWS x ls _ = error ("can not handle op " ++ (show x))




translatePWSToWS :: String -> String
translatePWSToWS = (map translateCharPWSToWS) . (filter (\x->elem x "abc"))

translateCharPWSToWS :: Char -> Char
translateCharPWSToWS 'a' = ' '
translateCharPWSToWS 'b' = '\t'
translateCharPWSToWS 'c' = '\n'
--translateCharPWSToWS _ = '.'
