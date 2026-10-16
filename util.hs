-- BEGIN SNIPLATE binaryMethod 
-- {{ abbr: binary method }}
-- {{ class: util }}
-- {{ pattern: binaryMethod ok ng f}}
-- This function is puslished at https://github.com/torus711/cplib-hs
binaryMethod ok ng f
	| abs ( ok - ng ) <= 1 = ok
	| otherwise = let mid = ( ok + ng ) `div` 2 in
		if f mid
			then binaryMethod mid ng f
			else binaryMethod ok mid f

-- END SNIPLATE

-- BEGIN SNIPLATE inf  
-- {{ abbr: infty }}
-- {{ class: util }}
-- {{ pattern: inf = maxBound `div` 2 }}
inf = maxBound `div` 2

-- END SNIPLATE

-- BEGIN SNIPLATE runDP 
-- {{ abbr: dynamic programming }}
-- {{ class: util }}
-- {{ pattern: runDP :: Ix i }}
-- This function is puslished at https://github.com/torus711/cplib-hs
runDP :: Ix i
	=> ( Int -> Int -> Int ) -> ( Int -> Int -> Int ) -> Int
	-> ( i, i ) -> [ ( i, Int ) ] -> [ ( i, i, Int ) ]
	-> UArray i Int
runDP op1 op2 e dpRange initStates transitions = runSTUArray do
	dp <- newArray dpRange e
	forM_ initStates $ \( u, val ) -> do
		writeArray dp u val
	forM_ transitions $ \( u, v, val ) -> do
		modifyArray dp v . op2 =<< op1 val <$> readArray dp u
	return dp

-- END SNIPLATE

-- BEGIN SNIPLATE vsort 
-- {{ abbr: sort Data.Vector }}
-- {{ class: util }}
-- {{ pattern: vsort v' = runST do }}
-- These functions is puslished at https://github.com/torus711/cplib-hs
vsort as = runST do
	v <- VU.thaw $ VU.fromList as
	VA.sort v
	VU.toList <$> VU.freeze v

vsort' as = runST do
	v <- V.thaw $ V.fromList as
	VA.sort v
	V.toList <$> V.freeze v

-- END SNIPLATE
