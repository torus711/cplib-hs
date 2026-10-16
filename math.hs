-- BEGIN SNIPLATE eratosthenes 
-- {{ abbr: Eratosthenes sieve }}
-- {{ class: math }}
-- {{ pattern: eratosthenes\sn=\srunST }}
-- This function is published at https://github.com/torus711/cplib-hs/math.hs
eratosthenes n = runST $ do
	isPrime <- newArray ( 2, n ) True :: ST s ( STUArray s Int Bool )
	forM_ [ 2 .. n ] $ \i -> do
		whenM ( readArray isPrime i ) $ do
			forM_ ( takeWhile ( ( <= n ) . ( * i ) ) [ 2 .. ] ) $ \j -> do
				writeArray isPrime ( i * j ) False
	filterM ( readArray isPrime ) [ 2 .. n ]

-- END SNIPLATE
