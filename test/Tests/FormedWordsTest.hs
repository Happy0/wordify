module Tests.FormedWordsTest where

	import Tests.SharedTestData

	testBoard :: Board
    testBoard = Board squareMap
        where
            squareMap = M.fromList $ (M.assocs emptySquares ++ verticles ++ horizontals)
            Board (emptySquares) = emptyBoard
