import ChessParser
import ChessSyntax
import Test.HUnit

test_parseMoves :: Test
test_parseMoves =
  TestList
    [ -- Test Pawn move
      parseMoves "e3" ~?= Right [NormalMove Pawn (Square 3 'e') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False)],
      -- Test Pawn move with disambiguation
      parseMoves "e7e3" ~?= Right [NormalMove Pawn (Square 3 'e') (Just (Both (Square 7 'e'))) (Promotion Nothing) (Capture False) (Check False) (Mate False)],
      -- Test King move
      parseMoves "Ka1b4" ~?= Right [NormalMove King (Square 4 'b') (Just (Both (Square 1 'a'))) (Promotion Nothing) (Capture False) (Check False) (Mate False)],
      -- Test Castling
      parseMoves "O-O" ~?= Right [KingSideCastling],
      parseMoves "O-O-O" ~?= Right [QueenSideCastling],
      -- Test Pawn Promotion
      parseMoves "ae7=Q" ~?= Right [NormalMove Pawn (Square 7 'e') (Just (File 'a')) (Promotion (Just Queen)) (Capture False) (Check False) (Mate False)],
      -- Test multiple moves parsing
      parseMoves "e3e4 e3"
        ~?= Right
          [ NormalMove Pawn (Square 4 'e') (Just (Both (Square 3 'e'))) (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 3 'e') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False)
          ],
      parseMoves "Ka3 Rh1h3"
        ~?= Right
          [ NormalMove King (Square 3 'a') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Rook (Square 3 'h') (Just (Both (Square 1 'h'))) (Promotion Nothing) (Capture False) (Check False) (Mate False)
          ],
      parseMoves "Ka3 Rh1h3 Kh2h3 Qa3a4"
        ~?= Right
          [ NormalMove King (Square 3 'a') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Rook (Square 3 'h') (Just (Both (Square 1 'h'))) (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove King (Square 3 'h') (Just (Both (Square 2 'h'))) (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Queen (Square 4 'a') (Just (Both (Square 3 'a'))) (Promotion Nothing) (Capture False) (Check False) (Mate False)
          ],
      parseMoves "Ka3 Rh1h3 Kh2h3 Qa3a4+"
        ~?= Right
          [ NormalMove King (Square 3 'a') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Rook (Square 3 'h') (Just (Both (Square 1 'h'))) (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove King (Square 3 'h') (Just (Both (Square 2 'h'))) (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Queen (Square 4 'a') (Just (Both (Square 3 'a'))) (Promotion Nothing) (Capture False) (Check True) (Mate False)
          ],
      parseMoves "Ka3    Rh1h3+ Kh2h3 Qa3a4"
        ~?= Right
          [ NormalMove King (Square 3 'a') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Rook (Square 3 'h') (Just (Both (Square 1 'h'))) (Promotion Nothing) (Capture False) (Check True) (Mate False),
            NormalMove King (Square 3 'h') (Just (Both (Square 2 'h'))) (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Queen (Square 4 'a') (Just (Both (Square 3 'a'))) (Promotion Nothing) (Capture False) (Check False) (Mate False)
          ],
      parseMoves "Ka3   \n R1h3   e3   e8=Q   Q6h5"
        ~?= Right
          [ NormalMove King (Square 3 'a') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Rook (Square 3 'h') (Just (Rank 1)) (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 3 'e') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 8 'e') Nothing (Promotion (Just Queen)) (Capture False) (Check False) (Mate False),
            NormalMove Queen (Square 5 'h') (Just (Rank 6)) (Promotion Nothing) (Capture False) (Check False) (Mate False)
          ],
      parseMoves "Ka3   \n R1h3   e3   e8=Q   Q6h5+ O-O Rf5"
        ~?= Right
          [ NormalMove King (Square 3 'a') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Rook (Square 3 'h') (Just (Rank 1)) (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 3 'e') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 8 'e') Nothing (Promotion (Just Queen)) (Capture False) (Check False) (Mate False),
            NormalMove Queen (Square 5 'h') (Just (Rank 6)) (Promotion Nothing) (Capture False) (Check True) (Mate False),
            KingSideCastling,
            NormalMove Rook (Square 5 'f') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False)
          ],
      parseMoves "O-O O-O-O \n O-O Bf8 \n\n Bff8"
        ~?= Right
          [ KingSideCastling,
            QueenSideCastling,
            KingSideCastling,
            NormalMove Bishop (Square 8 'f') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Bishop (Square 8 'f') (Just (File 'f')) (Promotion Nothing) (Capture False) (Check False) (Mate False)
          ],
      parseMoves "e8 Nxe8 Qxe8"
        ~?= Right
          [ NormalMove Pawn (Square 8 'e') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Knight (Square 8 'e') Nothing (Promotion Nothing) (Capture True) (Check False) (Mate False),
            NormalMove Queen (Square 8 'e') Nothing (Promotion Nothing) (Capture True) (Check False) (Mate False)
          ],
      parseMoves "e8 Nxe8 Qexe8"
        ~?= Right
          [ NormalMove Pawn (Square 8 'e') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Knight (Square 8 'e') Nothing (Promotion Nothing) (Capture True) (Check False) (Mate False),
            NormalMove Queen (Square 8 'e') (Just (File 'e')) (Promotion Nothing) (Capture True) (Check False) (Mate False)
          ],
      parseMoves "e4 e6 d4 d5 Nc3 Bb4 Bb5+ Bd7 Bxd7+ Qxd7 Ne2 dxe4 O-O"
        ~?= Right
          [ NormalMove Pawn (Square 4 'e') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 6 'e') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 4 'd') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 5 'd') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Knight (Square 3 'c') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Bishop (Square 4 'b') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Bishop (Square 5 'b') Nothing (Promotion Nothing) (Capture False) (Check True) (Mate False),
            NormalMove Bishop (Square 7 'd') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Bishop (Square 7 'd') Nothing (Promotion Nothing) (Capture True) (Check True) (Mate False),
            NormalMove Queen (Square 7 'd') Nothing (Promotion Nothing) (Capture True) (Check False) (Mate False),
            NormalMove Knight (Square 2 'e') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 4 'e') (Just (File 'd')) (Promotion Nothing) (Capture True) (Check False) (Mate False),
            KingSideCastling
          ],
      parseMoves "d4 g6 c4 Nf6 Nc3 Bg7 e4 O-O Bg5 d6 Nf3 Nbd7 e5 dxe5 dxe5 Ng4 Nd5 Ngxe5 Nxe7+ Kh8 Nxg6+ hxg6 Bxd8"
        ~?= Right
          [ NormalMove Pawn (Square 4 'd') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 6 'g') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 4 'c') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Knight (Square 6 'f') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Knight (Square 3 'c') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Bishop (Square 7 'g') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 4 'e') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            KingSideCastling,
            NormalMove Bishop (Square 5 'g') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 6 'd') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Knight (Square 3 'f') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Knight (Square 7 'd') (Just (File 'b')) (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 5 'e') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Pawn (Square 5 'e') (Just (File 'd')) (Promotion Nothing) (Capture True) (Check False) (Mate False),
            NormalMove Pawn (Square 5 'e') (Just (File 'd')) (Promotion Nothing) (Capture True) (Check False) (Mate False),
            NormalMove Knight (Square 4 'g') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Knight (Square 5 'd') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Knight (Square 5 'e') (Just (File 'g')) (Promotion Nothing) (Capture True) (Check False) (Mate False),
            NormalMove Knight (Square 7 'e') Nothing (Promotion Nothing) (Capture True) (Check True) (Mate False),
            NormalMove King (Square 8 'h') Nothing (Promotion Nothing) (Capture False) (Check False) (Mate False),
            NormalMove Knight (Square 6 'g') Nothing (Promotion Nothing) (Capture True) (Check True) (Mate False),
            NormalMove Pawn (Square 6 'g') (Just (File 'h')) (Promotion Nothing) (Capture True) (Check False) (Mate False),
            NormalMove Bishop (Square 8 'd') Nothing (Promotion Nothing) (Capture True) (Check False) (Mate False)
          ]
    ]