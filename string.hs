-- BEGIN SNIPLATE manacher 
-- {{ abbr: Manacher's Algorithm }}
-- {{ class: string }}
-- This function is published at https://github.com/torus711/cplib-hs/string.hs
manacher s' = runSTArray $ do
	res <- newArray ( 1, n ) 0
	let
		rec i j
			| n < i = return ()
			| 1 <= i - j && i + j <= n && s ! ( i - j ) == s ! ( i + j ) = rec i $ succ j
			| otherwise = do
				writeArray res i j
				k <- rec' 1
				rec ( i + k ) ( j - k )
			where
			rec' k
				| i - k < 1 = return k
				| otherwise = do
					rk <- readArray res ( i - k )
					if k + rk < j
						then do
							writeArray res ( i + k ) rk
							rec' $ succ k
						else return k
	rec 1 0
	return res
	where
	n = length s'
	s = listArray ( 1, n ) s'

-- END SNIPLATE
