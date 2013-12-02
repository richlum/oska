-- 2013 10 25
-- Richard Lum

module Oska_i0r7
(  -- functions exported for unit testing
	oska_i0r7, getsides, flatten, sidecount, getCols,
	getPositions,canMoveRight,canMoveLeft,moveRight,
	buildRboard,removepiece,insertpiece,moveLeft,
	moveUpRight,moveUpLeft, canJumpR, canJumpL,
	jumpRight, jumpLeft, jumpUpRight, jumpUpLeft,
	moveAllRight, moveAllLeft, jumpAllRight,
	jumpAllLeft, generateAllMoves,get_move,
	isBlockedUp,isBlocked,couldJump,couldJumpUp,
	statesearch,isGoalState,playself,rateBoard,
	blackcouldjumpwhites
) where
import Debug.Trace (trace)
import Data.List

-- HIGH LEVEL functions
-- oska_i0r7 ["-ww-","ww-","--","--b","bb-b"] 'w' 2
oska_i0r7 :: [String] -> Char -> Int -> [String]
oska_i0r7 board side moves 
--	|	trace ("i0r7") False = undefined
	|	not (validb board)	= error " invalid board"
	|	not (valids side)	= error " invalid side"
	|	otherwise 		= get_move board side moves

sp = '-'  -- empty board piece
----------------------------------------------------------------------
-- File organization
   ------------------
-- high level functions at top of file (start program, searchstate, )
-- static board evaluation and huerestic functions
-- print functions
-- piece movement functions
----------------------------------------------------------------------------------
-- playself ["-ww-","ww-","--","--b","bb-b"] 'w' 2 0
playself:: [String] -> Char -> Int -> Int -> [String]
playself board side moves counter
  | trace ((show board) ++" "++(show side) ++" "++(show counter) ) False = undefined
  | isGoalState board side = board
  | isGoalState board (otherSide side) = board
  | otherwise = playself (oska_i0r7 board side moves) (otherSide side) moves (counter+1) 



----------------------------------------------------------------------------------
-- front end checks before starting statesearch, 
get_move:: [String] -> Char -> Int ->  [String]
get_move  board side moves
-- | trace ("getmove result:" ++ (show (fst result)) ++ " snd "++ (show (snd result))) False = undefined
	| not (validb board) 	= error "invalid board"
	| not (valids side)	= error "invalid side"
	| moves <= 0		= []	-- not allowed to go any deeper
	| null (filter (not.null)(fst result ))	= board  -- blocked  
	| otherwise 		= ( head (filter (not.null)(fst result )))  
				where
					result = ( (statesearch board side moves  [[]]      optlist)); 
					optlist = [moveAllRight, moveAllLeft, jumpAllRight, jumpAllLeft]


-- main control path
--return a path and a rating for that path represented by best board at leaf for branch
statesearch :: [String] -> Char -> Int -> [[String]]  -> [Char -> [String] -> [[String]]]  ->  ([[String]],Int)
statesearch    board    side    moves   path 		      optlist
-- 	| trace ("\nss: " ++ (show board) ++ " " ++ (show side) ++ " moves: " ++ (show moves) ++ " path: " ++ (show path)) False = undefined
-- 	| trace ("\tpmoves: " ++ (show possiblemoves)) False = undefined
-- 	| trace ("\tmyrating: " ++ (show myrating)) False = undefined
-- 	| trace ("\tbest: " ++ (show bestmove) ++ " rating " ++ (show rating) ) False = undefined
	| isGoalState board side 
			= (path++[board],rateBoard side board)
	| isGoalState board (otherSide side) 
			=  (path++[board],rateBoard (otherSide side) board)
	| moves == 0	= ([],0)								-- protect against illegal input
	| moves == 1	= (path++[bestmove],rating) 	-- gone as far forward looking as allowed - should handle finding goalstate too 
--	| (elem board path)	= (path, myrating)  -- NOT valid :logic error: incorrect rating: need to get leaf value when we previously encountered at a lower level. let it go and rely on haskell to have prev calc ref on hand. Correct not to use this but possibly ineffiecient
	| otherwise 	= (pickFwdBest side [ (statesearch state (otherSide side) (moves-1) (path++[state]) optlist ) | state <- possiblemoves ])
  where                    --  list comprehension to generate list of states
		possiblemoves =  (generateNewStates board side moves optlist);
		bestmove = (pickBest side possiblemoves);
		rating = rateBoard side bestmove;
		myrating = rateBoard side board

		
-- input a list of pairs [(path,rating)]  pick the best rated pair
-- finds best results of statesearch
-- since statesearch does not return anything until it hits a leaf, we
-- get the value of the furthest view
pickFwdBest:: Char -> [([[String]],Int)] -> ([[String]],Int)
pickFwdBest side res
--	| trace ("pickFwdBest: " ++ (show side) ++" "++ (show res) ++"\n") False = undefined
	| null res			= 	([],0)
	| (length res)==1	= 	(head res)
	| side == 'w'		= 	maxValue   (head res) (pickFwdBest side (tail res))
	| side == 'b'		= 	minValue   (head res) (pickFwdBest side (tail res))
	| otherwise 		= trace ("pickfwdbest received illegal side") error "pick a side"

--- these next five methods are likey the best place to put any modifications
-- to allow board ratings of intermediate boards to influence move selection
maxValue :: ([[String]],Int) -> (([[String]],Int)) -> (([[String]],Int))
maxValue  lPair rPair
	| (snd lPair) > (snd rPair)  	= lPair
	| otherwise			=rPair
	
minValue :: ([[String]],Int) -> (([[String]],Int)) -> (([[String]],Int))
minValue  lPair rPair
	| (snd lPair) < (snd rPair)  	= lPair
	| otherwise			=rPair

		
-- given a list of boards pick the best board
-- for the stated side - this is our min max
-- return the next move that has best rated board
-- no input from future boards, static evaluation of current options only
pickBest::Char -> [[String]] -> [String]
pickBest  side boards 
	| null boards	=  []
	| length(boards) == 1	= head boards
	| side == 'w'	= maxBoard  side (head boards) (pickBest side (tail boards))
	| side == 'b'	= minBoard  side (head boards) (pickBest side (tail boards))
	| otherwise = error "what side are you playing?"

--given two boards return the one with max value by rateBoard	
--rateboard handles case where goalstate is reached by either side
maxBoard:: Char ->[String] -> [String] -> [String]
maxBoard side leftB rightB
	| (rateBoard side leftB) > (rateBoard side rightB)
				=	leftB
	| otherwise		=	rightB

minBoard:: Char -> [String] -> [String] -> [String]
minBoard side leftB rightB
--	| trace ("minboard rating : " ++ (show (rateBoard side leftB)) ++ " " ++ (show (rateBoard side rightB))) False = undefined
--	| trace ("minboards : " ++ (show side) ++ (show leftB) ++ " " ++ (show rightB)) False = undefined
	| (rateBoard side leftB) < (rateBoard side rightB)
				= leftB
	| otherwise		= rightB
	
--workhorse control point of generating all possible moves given a start state
-- input is a single unexplored state from which we will generate a list of new states to explore
-- do not block duplicates at this level, statesearch needs to see duplicates so it can control
--generateNewStates :: [String] -> [String] -> [[String]] 
generateNewStates :: [String] -> Char -> Int -> [Char -> [String] -> [[String]]] -> [[String]]
generateNewStates     unexplored side    moves   optlist
--	| trace ("genstates: " ++ (show unexplored) ++ " " ++  (show side) ++ " " ++ (show moves) ++ " " ++"\n") False = undefined
	| unexplored == [] 	= []
	| null optlist 		= []
	| otherwise  		= 	((head optlist) side unexplored  ) ++   -- here's where we actually generate moves
							(generateNewStates unexplored side moves (tail optlist))
				where piecelist = getPositions side unexplored 0




	
--------------------------------------------------------------------------------
-- Board Evaluation Functions and
-- SEARCH POLICY HEURESTIC
-- convention to be used
-- max positive value better for white
-- max negative value better for black
-- convention allows for single universal rating regardless of side
-- need to understand how to balance twin goals and consider opponent goals

maxint = 2000000   -- number representing value of a win/loss board
					--since leaf would not be seen unless it passed max/min
					-- we want any win/loss boards that show up to be either 
					-- pursued at all costs or avoided at all costs
					
capturevalue = 15  -- number representing how much capturing or losing a piece
-- convention, keep all ratings positive until rateBoard which will
-- transform to  negative values for black
-- side is required for rating since it is 'sides' move and
-- this selection should reflect that 

rateBoard:: Char -> [String] -> Int
rateBoard side board
--	| trace ("rb side board : " ++ (show side) ++ " " ++ (show board)) False = undefined
--	| trace ("rateBoard result: " ++ (show result)) False = undefined
	| null board 	=  0 
	| isGoalState board 'w' =  maxint
	| isGoalState board 'b' =  (-maxint)
	| otherwise = result
	where result =
			(max (whiteCaptureRating board) (whiteSideRating board))
			-(max (blackCaptureRating board) (blackSideRating board))
			- (blackcouldjumpwhites board)*capturevalue
			+ (whitecouldjumpblacks board)*capturevalue
			- (whiteBlocks board)*(blockvalue 'w' board) -- white is blocked, value multiplies if a white is already at goal
			+ (blackBlocks board)*(blockvalue 'b' board) -- black is blocked, value is higher if  a black already at goal


-- how close we are to capturing all of the other side
-- need bigger to be closer to goal, so use max-curr count
whiteCaptureRating::[String] -> Int
whiteCaptureRating board
	| otherwise	=  (maxblacks - currblacks)*capturevalue
	where
		maxblacks 	= (length (board!!0) ) 
		currblacks = (length (getPositions 'b' board 0)) 

-- how close we are to getting all of our pieces to other side
-- need bigger = closer to goal, take boardheight *2 as arbitrary reference
-- and subtract remaining linear distance to goal
whiteSideRating::[String] -> Int
whiteSideRating board
	| otherwise = refdist - (sumlineardistances 'w' board positions)
	where positions = getPositions 'w' board 0;
		refdist = 2*(length board)

		
-- how close we are to capturing all of the other side
-- need bigger to be closer to goal, so use max-curr count
blackCaptureRating:: [String] -> Int
blackCaptureRating board
	| otherwise	=  (maxwhites - currwhites)*capturevalue
	where
		maxwhites 	= (length (board!!0) ) 
		currwhites = (length (getPositions 'w' board 0)) 

-- how close we are to getting all of our pieces to other side
-- need bigger = closer to goal, take boardheight *2 as arbitrary reference
-- and subtract remaining linear distance to goal
blackSideRating:: [String] -> Int
blackSideRating board
	| otherwise = refdist - (sumlineardistances 'b' board positions)
	where positions = getPositions 'b' board 0;
		refdist = 2*(length board)
		
-- how many whites could be jumped in the next move
blackcouldjumpwhites:: [String] -> Int
blackcouldjumpwhites board
	| null board	= 0
	| otherwise		= numTrue [(couldJumpUp x board) | x <- positions] 
	where positions = getPositions 'b' board 0
-- how many blacks could be jumped in the next move
whitecouldjumpblacks:: [String] -> Int
whitecouldjumpblacks board
	| null board	= 0
	| otherwise		= numTrue [(couldJump x board) | x <- positions] 
	where positions = getPositions 'w' board 0
	

--always a positive number - probably needs to be higher to represent
-- greater relative value for blocking increasing distance to goal for 
-- getting to the other side  relative to capturing all the opponent pieces
blockvalue::  Char -> [String] -> Int
blockvalue side board 
	| ((side=='w') && (whitesAtGoal > 0))	=	2*capturevalue	--must exceed value of a capture
	| ((side=='b') && (blacksAtGoal > 0))	= 	2*capturevalue
	| otherwise = 1
	where 
		whitesAtGoal = piecesAtGoal 'w' wpositions board;
		blacksAtGoal = piecesAtGoal 'b' bpositions board;
		wpositions = getPositions 'w' board 0;
		bpositions = getPositions 'b' board 0
	
		-- movesToWin = min (blackmovestowin
	-- bodycount = (sidecount 'w' state) - (sidecount 'b') ;
		  -- totaldistdiff = (sumlineardistances 'w' board wpositions) -
						-- (sumlineardistances 'b' board hpositions); -- who is closer to gettinhg all to other side
		  -- wpositions = getPositions 'w' board 0;
		  -- bpositions = getPositions 'h' board 0;

-- no of white pieces that are blocked
whiteBlocks:: [String] -> Int
whiteBlocks board
	| null board 	= 0
	| otherwise		= numTrue [(isBlocked x board) | x <- positions] 
	where positions = getPositions 'w' board 0

-- no of black pieces that are blocked
blackBlocks:: [String] -> Int
blackBlocks board
	| null board 	= 0
	| otherwise		= numTrue [(isBlockedUp x board) | x <- positions] 
	where positions = getPositions 'b' board 0
		  
-- given a list of Bool return count of the number of trues  
numTrue :: [Bool] -> Int
numTrue listBools
	| null listBools			= 0
	| head listBools == True 	= 1+ (numTrue (tail listBools))
	| otherwise					= (numTrue (tail listBools))
	
-- how many pieces are at the goal line
piecesAtGoal side positions board
	| null positions = 0
	| (side == 'b')&&((fst(head positions))==0) 
					= 1 + (piecesAtGoal side (tail positions) board)
	| (side == 'w')&&((fst(head positions))==((length board)-1) ) 
					= 1 + (piecesAtGoal side (tail positions) board)
	|	otherwise	= (piecesAtGoal side (tail positions) board)

min a b 
	| a<b	= a
	| otherwise	= b

	
-- what is the min number of moves required to get all pieces to goal line
sumlineardistances side board positions  
	| null positions = 0
	| otherwise		= -(fst(head positions) - goalrow) + (sumlineardistances side board (tail positions))
	where goalrow = (length board) -1
	

-- for sort order of search   -- no sorting, just picking, not used
-- mazeRank m1 m2
	-- | (rateMaze m1) > (rateMaze m2) = GT
	-- | (rateMaze m1) < (rateMaze m2) = LT
	-- | (rateMaze m1) == (rateMaze m2)= compare m1 m2

myremoveduplicates list1
	| null list1 = []
	| elem (head list1) (tail list1) = myremoveduplicates (tail list1)
	| otherwise = (head list1):(myremoveduplicates (tail list1))

-- are we there yet?  have we captured all or have we reached goal line for all our pieces
isGoalState::[String] -> Char -> Bool
isGoalState  board   side	
-- 	| trace ("isGoalState side: " ++ (show  side ) ++" " ++ (show mypieces) ) False = undefined
-- 	| trace ("isGoalState side: " ++ (show (elem side uniquepieces)) ++" " ++ (show (length uniquepieces)) ) False = undefined
	|	(((length uniquepieces)==2)&&(elem side uniquepieces)) =  True -- only blanks and my pieces left
	|	((not (null mypieces) )&&( allOnRow mypieces targetrow))= True 
	|	otherwise 						= False
	where uniquepieces = (getsides board []); -- includes blanks
		mypieces  = getPositions side board 0;
		targetrow = targetRow board side-- 0 based row numbers

		
-- where is the target row = goal line
targetRow ::[String] -> Char -> Int
targetRow board side 
  | side == 'b'	= 0
  | side == 'w' = ((length board) - 1)  -- zero based row numbers
  | otherwise   = error "bad side for targetRow"
	
-- is everyone there?
allOnRow:: [(Int,Int)] -> Int -> Bool
allOnRow	mypieces	targrow
--	| trace ("allOnRow: " ++ (show targrow)++"mypieces" ++ (show mypieces)) False = undefined
	|	null mypieces				= True
	|	(fst (head mypieces)) == targrow	= True && (allOnRow (tail mypieces) targrow)
	|	otherwise				= False

	
	
-- is board valid shape
validb :: [String] -> Bool
validb board
	| null board 	= False
	| (boardheight ==3)		= ( ((length (board!!0))==3)&&(length (board!!2)==3)&&(length (board!!1)==2))
	| ( -- check for symmetry and expected changes in row lengths
		(boardheight >=3) &&
		(((length (board!!0)) - (length (board!!1))) == 1 ) && 
		((length (board!!0)) == (length(board!!(boardheight-1))))
	  )				= True && validb (init (tail board))
	| otherwise 	= False
	where boardheight = (length board)  
	
	
-- do all board pieces make sense
valids	:: Char		-> Bool
valids	side
	| elem side ['w','b']	 	= True
	| otherwise					= False
	
getsides :: [String] -> String -> String
getsides board	result
	| null board 	= result
	| null line		= getsides (tail board) result
	| not (elem piece result) 
					= getsides ([(tail line)]++(tail board)) (piece:result)  
	| otherwise		= getsides ([(tail line)]++(tail board)) result
	where line = (head board) ;
		piece =  (head line)

otherSide :: Char -> Char
otherSide side
	| side == 'w' = 'b'
	| side == 'b' = 'w'
	| otherwise		= trace ("error other, side = " ++ (show side)) error "picking a side"
		
flatten :: [String] -> String
flatten board 
	| null board	= []
	| otherwise		= (head board) ++ (flatten (tail board))

	
	
-- how many pieces are there for a side
sidecount :: Char -> [String] -> Int
sidecount side board
	|	null board	= 0
	|	otherwise 	= count side (flatten board) 
	
count :: Char -> String -> Int
count side board
	| null board			=	0
	| side == (head board)	=	1 + (count side (tail board))
	| otherwise				=	(count side (tail board))

	
	
-- what is col position of our pieces given the row
getCols:: Char -> String -> Int -> [Int]
getCols side row column
	|	null row			= []
	|	head row == side	= [column] ++ (getCols side (tail row) (column+1))
	|	otherwise			= getCols side (tail row) (column+1) 

	
	
-- where are all our pieces
getPositions:: Char -> [String] -> Int -> [(Int,Int)]
getPositions	side	board	row 
	| null board 			= []
	| elem side (board!!0) 	= (zip rows columns) ++ getPositions side (tail board) (row+1)
	| otherwise 			=  getPositions side (tail board) (row+1)
	where columns = getCols side (head board) 0;
		rows = replicate (length columns) row

-- assumes moving down the board. for moving up, reverse the board
-- to use same methods for other side
canMoveRight:: (Int,Int) -> [String] -> Bool
canMoveRight	position board
--	| trace ( "\ntophalf = " ++ (show topHalf) ++ " rightMostElem = " ++ (show rightMostElem) ) False = undefined
	| (element position board) == sp		= error "I'm sorry Dave, I cant do that" -- trying to move a blank
	| (fst position) >= ((length board)-1)	= False -- trying to move nonexistent row
	| topHalf && rightMostElem				= False -- no where to move
	| not (validIndices targpos board)		= False  -- calculated move right target may be outside of board 
	| otherwise								= True  && (emptySlot targpos board) -- allow for jumps
	where 
			topHalf = topHalfofBoard position board;
			rightMostElem = ((snd position) == ((length (board!!(fst position))) -1));
			targpos = moveRightTargPos position board


canMoveLeft:: (Int,Int) -> [String] -> Bool
canMoveLeft	position board
	| (element position board) == sp		= error "I'm sorry Dave, I cant do that"
	| (fst position) >= ((length board)-1)	= False
	| (topHalfofBoard position board)&& leftMostElem	= False
	| otherwise							= True && (emptySlot targetpos board)
	where 
			leftMostElem = ((snd position) == 0);
			targetpos = moveLeftTargPos position board

topHalfofBoard:: (Int,Int) -> [String] -> Bool
topHalfofBoard position board 
--	| not  (validIndices position board)	= error "invalid indices" -- allow generation of illegal indices for detection higher up
	| otherwise 							= ((fst position) < ((div (length board) 2 ) )) -- note div len board yields middle row idx
						
-- given a position and board, what row,col can it move right to
moveRightTargPos:: (Int,Int) -> [String] -> (Int,Int)
moveRightTargPos src board
	| null board	= error "empty board"
	| not (validIndices src board)	= error "invalid indices"
	| topHalfofBoard src board		= ( (( fst src )+1) , ((snd src)) ) 
	| otherwise						= ( (( fst src )+1) , ((snd src)+1)   ) -- note shift in target pos for move left vs right
	where dst = ((fst src)+1, (snd src)) -- col not yet updated at this point, just placeholder with src col

moveLeftTargPos:: (Int,Int) -> [String] -> (Int, Int)
moveLeftTargPos src board
	| null board	= error "empty board"
	| not (validIndices src board)	= error "invalid indices"
	| topHalfofBoard src board		= ( (( fst src )+1) , ((snd src)-1) ) 
	| otherwise						= ( (( fst src )+1) , ((snd src))   ) -- note shift in target pos for move left vs right

						
						
emptySlot:: (Int,Int) -> [String] -> Bool
emptySlot pos board
--	| trace ("empty? " ++ (show pos) ++ ":" ++ (show board) ) False	= undefined
	|	null board 								= False
	|	not (validIndices pos board)			= False
	|	(element pos board) == sp				= True
	|	otherwise 								= False

validIndices:: (Int,Int) -> [String] -> Bool
validIndices pos board
--	| (fst pos) >= (length board)				= traceStack  ("index oob, row : " ++ (show pos) ++ " board = " ++ (show board))  False
	| (fst pos) >= (length board)				= False
--	| (snd pos) >= (length (board!!(fst pos)))	= trace ("index oob, col : " ++ (show pos) ++ " board = " ++ (show board))  False
	| (snd pos) >= (length (board!!(fst pos)))	= False
	| (snd pos) <  0							= False -- generated by moveLeft fr left col
	| otherwise									= True

-- what is at given position
element:: (Int,Int) -> [String] -> Char
element pos board
	| null board 	= error "null board"
	|(fst pos) > (length board)				= error "row request out of bounds"
	|(snd pos) > (length (board!!(fst pos)))= error "col request out of bounds"
	| otherwise								= ((board!!(fst pos))!!(snd pos))

-- given a row,col, can this piece jump right - assumes downward movement
canJumpR:: (Int,Int) -> [String] -> Bool
canJumpR pos board
	|	null board 							= False -- trace("jmp1") False
	|	not (validIndices pos board)		= False -- trace("jmp2") False
	|	not (validIndices victum board)		= False -- trace("jmp3") False
	|	victumpiece == sp					= False -- trace("jmp4") False
	|	piece == victumpiece				= False -- trace("jmp5") False
	|	not (validIndices jmptarget board)	= False -- trace("jmp6") False
	|	targetpiece /= sp					= False -- trace("jmp7") False
	|	otherwise							= True  -- trace("jmp8") True
		where
			victum = moveRightTargPos pos board; -- where are we jumping over
			victumpiece = element victum board;  -- what are we jumping
			piece = element pos board;	-- what kind are we
			jmptarget = moveRightTargPos victum board; -- where we plan to land
			targetpiece = element jmptarget board -- whats there now

			
canJumpL:: (Int,Int) -> [String] -> Bool
canJumpL pos board
	|	null board 							= False -- trace("jmp1") False
	|	not (validIndices pos board)		= False -- trace("jmp2") False
	|	not (validIndices victum board)		= False -- trace("jmp3") False
	|	victumpiece == sp					= False -- trace("jmp4") False
	|	piece == victumpiece				= False -- trace("jmp5") False
	|	not (validIndices jmptarget board)	= False -- trace("jmp6") False
	|	targetpiece /= sp					= False -- trace("jmp7") False
	|	otherwise							= True  -- trace("jmp8") True
		where
			victum = moveLeftTargPos pos board; -- where are we jumping over
			victumpiece = element victum board;  -- what are we jumping
			piece = element pos board;	-- what kind are we
			jmptarget = moveLeftTargPos victum board; -- where we plan to land
			targetpiece = element jmptarget board -- whats there now
-- these methods are aimed at helping static board evaluation

-- is bad if any of our pieces blocked, is good if we block someone elses
-- intent is to feed it a list of positions and check if any of the positions
-- are blocked
-- downward direction presumed
isBlocked:: (Int,Int) -> [String] ->  Bool
isBlocked    pos          board
	| canMoveRight pos board 	= False
	| canMoveLeft  pos board 	= False
	| canJumpL     pos board	= False
	| canJumpR     pos board	= False
	| otherwise					= True

couldJump:: (Int,Int) -> [String] -> Bool
couldJump   pos board
	| canJumpR pos board	= True
	| canJumpL pos board	= True
	| otherwise				= False
	


-- methods going up the board (as opposed to all above methods which are down the board)
isBlockedUp::  (Int,Int) -> [String] ->  Bool
isBlockedUp  pos			board
	|(element pos board) /= 'b' 	= error "piece cant go that direction"
	| otherwise =  isBlocked (invertRowPos pos board) (reverse board) 

couldJumpUp:: (Int,Int) -> [String] -> Bool
couldJumpUp 	pos board
	|(element pos board) /= 'b' 	= error "piece cant go that direction"
	|otherwise  = couldJump (invertRowPos pos board) (reverse board)
	
--------------------------------------------------------------------------------
-- print functions from Rodrigo Alves


-- Calls the oska function (same params), but prints the next move nicely.
-- Call example: oska_print_i0r7 ["www-","-w-","--","b--","-bbb"] 'b' 4
oska_print_i0r7 :: [String] -> Char -> Int -> IO ()
oska_print_i0r7 board side depth = print_i0r7 (oska_i0r7 board side depth)
 
-----------------------------------------------------------------}
 
-- @param board: the board that will be printed. Prints in the console.
-- Call example 1: print_i0r7 ["ww-","-w","bbb"]
-- Call example 2: print_i0r7 ["w-w-w","--w-","w--","--","---","--bb","bb-b-"]
-- print_i0r7 ["wwwwww","-----","----", "-T-", "--", "---","----","-----","bbbbbb"]
print_i0r7 :: [String] -> IO ()
print_i0r7 board = print_board_i0r7 board 1 (length board) 0 (-1)
       
-- Print helper to print the board on console. Magic happens here.
print_board_i0r7 :: [String] -> Int -> Int -> Int -> Int -> IO ()
print_board_i0r7 [] _ _ previousLength previousIndent =
        print_line_i0r7 previousLength previousIndent (even previousIndent)
print_board_i0r7 (x:xs) currentRow totalRows previousLength previousIndent = do
        print_line_i0r7 (max (length x) previousLength) lineIndent (even lineIndent)
        print_row_i0r7 x indent
        print_board_i0r7 xs (currentRow + 1) totalRows (length x) indent
        where indent = next_indent_i0r7 previousIndent totalRows currentRow
              lineIndent = max indent previousIndent
 
-- @return the next indent based on the idea that the indent is getting
-- bigger until half of the board. Then, it starts to shrink.
next_indent_i0r7 :: Int -> Int -> Int -> Int
next_indent_i0r7 previousIndent totalRows currentRow
        | currentRow > ((div (totalRows+1) 2))        = previousIndent - 1
        | otherwise                             = previousIndent + 1
       
-- @param size: the size of the row of lines (about 4 times that)
-- @param indent: the space before printing the row of lines
-- @param evenIndent: helper to determine if an extra '-' or space is required
-- Call example: print_line_i0r7 3 2 True -> prints "  -------------"
print_line_i0r7 :: Int -> Int -> Bool -> IO ()
print_line_i0r7 0 indent evenIndent
        | evenIndent = putStrLn "-"
        | otherwise = putStrLn ""
print_line_i0r7 size 0 evenIndent = do
        putStr "----"
        print_line_i0r7 (size-1) 0 evenIndent
print_line_i0r7 size 1 False = do
        putStr "-"
        print_line_i0r7 size 0 False
print_line_i0r7 size 1 True = print_line_i0r7 size 0 True
print_line_i0r7 size indent evenIndent = do
        putStr "  "
        print_line_i0r7 size (indent - 1) evenIndent
       
-- @param str: a string that will be printed in the console
-- @param indent: how much space should be printed before printing str.
-- Call example 1: print_row_i0r7 "wwww" 3 -> prints "      | w | w | w | w |"
print_row_i0r7 :: String -> Int -> IO ()
print_row_i0r7 [] _ = putStrLn "|"
print_row_i0r7 (x:xs) 0 = do
        print_piece_i0r7 x
        print_row_i0r7 xs 0 
print_row_i0r7 str indent = do
        putStr "  "
        print_row_i0r7 str (indent - 1)
 
-- Helper of the print function that prints a small portion of a row      
print_piece_i0r7 :: Char -> IO ()
print_piece_i0r7 'w' = putStr "| w "
print_piece_i0r7 'b' = putStr "| b "
print_piece_i0r7 '-' = putStr "|   "
print_piece_i0r7  c  = putStr "| ? "
------------------------------------------------------------------------------------------------
-- MOVE  functions that actuall move pieces on the board

moveRight:: (Int,Int) -> [String] -> [String]
moveRight position board
--	|	trace("position: " ++ (show position) ++ " targ: " ++ (show targpos) ++ " board: " ++ (show board)) False	= undefined
	|	not (validIndices position board)	= []
	|	not (validIndices targpos board)	= []
	|	not (emptySlot targpos board)		= [] 
	|	not (canMoveRight position board)	= []
	|	otherwise							= rightBoard position board 
	where targpos = moveRightTargPos position board

rightBoard :: (Int,Int) -> [String] -> [String]
rightBoard position board = buildRboard board piece 0 frRow frCol toCol
	where
		piece =	element position board;
		frRow = fst position;
		frCol = snd position;
		toCol = (snd (moveRightTargPos position board))
			
buildRboard:: [String] -> Char -> Int-> Int -> Int -> Int -> [String]
buildRboard  board item currentrow frow fcol  tcol
	-- | trace ("buildRboard position = " ++ (show position) ++ " row = " ++ (show row) ++ 
			 -- " board = " ++ (show board) ++ "piece = " ++ (show piece)) False = undefined
--	|	currentrow >	frow	= board
	|	currentrow <	frow	= [(head board)] ++ (buildRboard (tail board) item (currentrow+1) frow fcol tcol )
	|	currentrow ==	frow	= ([(removepiece fcol (head board))] ++ 
							[(insertpiece tcol item (head (tail board)))] ++  
									(tail (tail board))
							)

removepiece:: Int -> String -> String
removepiece colnum rowstring
	| null rowstring	= []
	| otherwise			= insertpiece colnum sp rowstring

insertpiece:: Int -> Char -> String -> String
insertpiece col item rowstring
	-- | trace ("insertpiece col = " ++ (show col) ++ " item = " ++ (show item) ++ " str = " ++ (show rowstring) ) False = undefined
	| null rowstring	= []
	| col == 0			= [item] ++ (tail rowstring)
	| otherwise			= (head rowstring) :(insertpiece (col-1) item (tail rowstring))


moveLeft:: (Int,Int) -> [String] -> [String]
moveLeft position board
	|	not (validIndices position board)	= []
	|	not (validIndices targpos board)	= []
	|	not (emptySlot targpos board)		= []
	|	not (canMoveLeft position board)	= []
	|	otherwise							= leftBoard position board 
	where targpos = moveLeftTargPos position board

-- same buildboard logic for right or left, we just supply the correct parameters
leftBoard :: (Int,Int) -> [String] -> [String]
leftBoard position board = buildRboard board piece 0 frRow frCol toCol
	where
		piece = element position board;
		frRow = fst position;
		frCol = snd position;
		toCol = (snd(moveLeftTargPos position board))

-- moving up is the same as moving down in the reverse order of the board
-- flip the order of the board and use move down methods from above
-- then reverse the board before delivering.
moveUpRight::(Int,Int) -> [String] -> [String]
moveUpRight position board
	= reverse(moveRight (invertRowPos position board) (reverse board))

-- since we invert the board to make it look like moving 
-- up is the same as moving down, we need to change
-- x pos values during calculation and then invert the result back
-- input at the top level is the same row col 
-- eg black starts in bottom row (row 4 in a 5 row game board).
-- indexing is the same for both black and white's perspective
-- columns are all 0 on the left.
invertRowPos:: (Int,Int) -> [String] -> (Int,Int)
invertRowPos pos board
	= ( ((length board)-1)-( fst pos), (snd pos))
	
moveUpLeft::(Int,Int) -> [String] -> [String]
moveUpLeft position board
	= reverse(moveLeft (invertRowPos position board) (reverse board))

-- jumping is based on utilizing moves but disabling checks for
-- occupied destinations (trampling). double trample = jump
--------------------------------------------------------------------
jumpRight:: (Int,Int) -> [String] -> [String]
jumpRight pos board
--	| trace ("jmpR victpos = " ++ (show victumpos)) False = undefined
	| null board					= []
	| not (validIndices pos board)	= []
	| not ( canJumpR pos board)		= []
	| otherwise						= 
		(moveRight victumpos (trampleRight pos board))
	where
		victumpos = moveRightTargPos pos board

-- same as moveRight but does not check for empty target
-- allows for jmp to trample over a piece leaving behind a space
trampleRight:: (Int,Int) -> [String] -> [String]
trampleRight position board
--	|	trace("trample position: " ++ (show position) ++ " targ: " ++ (show targpos) ++ " board: " ++ (show board)) False	= undefined
	|	not (validIndices position board)	= []
	|	not (validIndices targpos board)	= []
--	|	not (emptySlot targpos board)		= [] 
	|	not (canTrampleRight position board)= []  -- like canMoveRight but allows for piece to exist in target spot : remove canJumpRight at higher level duplicates protection??
	|	otherwise							= rightBoard position board 
	where targpos = moveRightTargPos position board

canTrampleRight:: (Int,Int) -> [String] -> Bool
canTrampleRight	position board
	| (element position board) == sp		= error "I'm sorry Dave, I cant do that" -- trying to move a blank
	| (fst position) >= ((length board)-1)	= False -- trying to move nonexistent row
	| topHalf && rightMostElem				= False -- no where to move
	| not (validIndices targpos board)		= False  -- calculated move right target may be outside of board 
	| otherwise								= True   -- would check for empty target here, not checking allows for jumps to overwrite spot
	where 
			topHalf = topHalfofBoard position board;
			rightMostElem = ((snd position) == ((length (board!!(fst position))) -1));
			targpos = moveRightTargPos position board
	

jumpLeft:: (Int,Int) -> [String] -> [String]
jumpLeft pos board
--	| trace ("jmpL victpos = " ++ (show victumpos)) False = undefined
	| null board					= []
	| not (validIndices pos board)	= []
	| not ( canJumpL pos board)		= []
	| otherwise						= 
		(moveLeft victumpos (trampleLeft pos board))
	where
		victumpos = moveLeftTargPos pos board

-- same as moveRight but does not check for empty target
-- allows for jmp to trample over a piece leaving behind a space
trampleLeft:: (Int,Int) -> [String] -> [String]
trampleLeft position board
--	|	trace("trample position: " ++ (show position) ++ " targ: " ++ (show targpos) ++ " board: " ++ (show board)) False	= undefined
	|	not (validIndices position board)	= []
	|	not (validIndices targpos board)	= []
	|	not (canTrampleLeft position board)= []  -- like canMoveLeft but allows for piece to exist in target spot : remove canJumpRight at higher level duplicates protection??
	|	otherwise							= leftBoard position board 
	where targpos = moveLeftTargPos position board

canTrampleLeft:: (Int,Int) -> [String] -> Bool
canTrampleLeft	position board
	| (element position board) == sp		= error "I'm sorry Dave, I cant do that" -- trying to move a blank
	| (fst position) >= ((length board)-1)	= False -- trying to move nonexistent row
	| topHalf && leftMostElem				= False -- no where to move
	| not (validIndices targpos board)		= False  -- calculated move  target may be outside of board 
	| otherwise								= True   -- would check for empty target here, not checking allows for jumps to overwrite spot
	where 
			topHalf = topHalfofBoard position board;
			leftMostElem = ((snd position) == 0);
			targpos = moveLeftTargPos position board



jumpUpRight::(Int,Int) -> [String] -> [String]
jumpUpRight position board
	= reverse(jumpRight (invertRowPos position board) (reverse board))

jumpUpLeft::(Int,Int) -> [String] -> [String]
jumpUpLeft position board
	= reverse(jumpLeft (invertRowPos position board) (reverse board))

----------------------------------------------------------------------------------
moveAllRight:: Char -> [String] -> [[String]]
moveAllRight side board 
	| side == 'w' 	= movelistRight   piecelist board 
	| side == 'b'	= movelistUpRight piecelist board
	| otherwise		= error "pick a side buddy"
	where 
		piecelist =  getPositions side board 0
		
movelistRight:: [(Int,Int)] -> [String] -> [[String]]
movelistRight  positions board
	| null positions 	= []
	| otherwise			= filter (not.null) [moveRight (head positions) board] ++ 
							(movelistRight (tail positions) board )


movelistUpRight:: [(Int,Int)] -> [String] -> [[String]]
movelistUpRight  positions board
	| null positions 	= []
	| otherwise			= filter (not.null) [moveUpRight (head positions) board] ++ 
							(movelistUpRight (tail positions) board )
							

moveAllLeft::Char -> [String] -> [[String]]
moveAllLeft side board
	| side == 'w' 	= movelistLeft piecelist board
	| side == 'b'	= movelistUpLeft piecelist board
	| otherwise		= error "pick a side buddy"
	where 
		piecelist = getPositions side board 0
 
movelistLeft::[(Int,Int)] -> [String] -> [[String]]
movelistLeft positions board
	| null positions	= []
	| otherwise			= filter (not.null) [moveLeft (head positions) board] ++
							(movelistLeft (tail positions) board)

movelistUpLeft::[(Int,Int)] -> [String] -> [[String]]
movelistUpLeft positions board
	| null positions	= []
	| otherwise 		= filter (not.null) [moveUpLeft (head positions) board] ++
							(movelistUpLeft (tail positions) board)

jumpAllRight::Char -> [String] -> [[String]]
jumpAllRight side board
	| side == 'w' 	= jumplistRight piecelist board
	| side == 'b'	= jumplistUpRight piecelist board
	| otherwise		= error "pick a side buddy"
	where 
		piecelist = getPositions side board 0

		
jumplistRight:: [(Int,Int)] -> [String] -> [[String]]
jumplistRight positions board 
	| null positions = []
	| otherwise 	= filter (not.null) [jumpRight (head positions) board] ++
						(jumplistRight (tail positions) board)

jumplistUpRight:: [(Int,Int)] -> [String] -> [[String]]
jumplistUpRight positions board
	| null positions = []
	| otherwise 		= filter (not.null) [jumpUpRight (head positions) board] ++
							(jumplistUpRight (tail positions) board)
		
		
jumpAllLeft::Char -> [String] -> [[String]]
jumpAllLeft side board
	| side == 'w' 	= jumplistLeft piecelist board
	| side == 'b'	= jumplistUpLeft piecelist board
	| otherwise		= error "pick a side buddy"
	where 
		piecelist = getPositions side board 0					
							
jumplistLeft:: [(Int,Int)] -> [String] -> [[String]]
jumplistLeft positions board 
	| null positions = []
	| otherwise		= filter (not.null) [jumpLeft (head positions) board]	++
										(jumplistLeft (tail positions) board)
										
jumplistUpLeft:: [(Int,Int)] -> [String] -> [[String]]
jumplistUpLeft positions board 
	| null positions = []
	| otherwise		= filter (not.null) [jumpUpLeft (head positions) board] ++
										(jumplistUpLeft (tail positions) board)


generateAllMoves :: Char -> [String] -> [ Char -> [String] -> [[String]] ] -> [[String]]
generateAllMoves    side    board       optlist
	| null optlist 	= []
	| otherwise		= ((head optlist) side board ) ++
						(generateAllMoves side board (tail optlist))
