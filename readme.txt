project oska  : 2013 11 08
richard lum

==========================================================

-- to run  oska_i0r7 board side moves    as follows
  Prelude> :l oska_i0r7
  [1 of 1] Compiling Oska_i0r7        ( oska_i0r7.hs, interpreted )
  Ok, modules loaded: Oska_i0r7.
  *Oska_i0r7> oska_i0r7 ["wwww","---","w-","--w","----"] 'w' 1
  ["wwww","---","w-","--w","----"]

-- to pretty print
  *Oska_i0r7> oska_print_i0r7  ["wwww","---","w-","--w","----"] 'w' 1

-- to run unit tests

  *Main> :l testoska
  [1 of 2] Compiling Oska_i0r7        ( Oska_i0r7.hs, interpreted )
  [2 of 2] Compiling Main             ( testoska.hs, interpreted )
  Ok, modules loaded: Oska_i0r7, Main.
  *Main> runTestTT alltests
  Cases: 130  Tried: 130  Errors: 0  Failures: 0
  Counts {cases = 130, tried = 130, errors = 0, failures = 0}


-- Notes

statesearch generates all possible current moves (moves =1) and then
recursively calls itself with generateNewStates for each level it is
allowed to recurse. 

rateBoard is a static rating system that applies an integer to every 
board and is used by picking methods (pickFwdBest, pickBest, maxBoard,
minBoard ...) to rank relative boards.  The rating is the same number
from both black and whites perspective.  Whites's goal is to maximize this
number, and blacks goal is to minimize this number.  The same rules are
applied to scoring black and white with the only difference being at the
summation level, all black scores are made negative.

scoring huerestic accounts for multiple goals by calculating the distance
to goal rating for 
	1. capturing all the other side  or 
	2. for getting all pieces to the other side.  
	
Once calculated, the number maximum of these two is used as the board rating.  
Board rating is propogated from leaf to the top (which is one of the possible
next moves).  Selection of next move is based on the highest value of the deepest
leaf allowd by moves.  Value of all intermediate boards is not considered
in this algorithm.

Distance to goal was made to increase score as player gets closer to goal by subtracting
from an arbitrary number (twice the number of rows of the board).  Similarly
the distance to capture all other players pieces was the difference between
max pieces (other side) and current pieces.  

In addition to this, score points  where given for mazes that contained blocking of 
pieces and for pieces that could be jumped.

To ensure that overriding all these strategies that the presence of a goal board
strongly overrides any of these smaller factors, an arbitrary large number
is used to value a winning board (2000000)

PickFwdBest is where the min max alternation takes place in recognition 
of current player side and next player side.

Scores for first level options are determined by the maximum/minimum values 
of children leaf nodes following the min max route to the leaf. Current 
iteration completely ignores the intermediate ranking values of non leaf 
node boards.  It is presumed that long term optimization
will take care of short term behaviour appropriately.

Osak_i0r7 will handle arbitrary sized boards as long as they follow the 
symmetrical pattern with one additional element for additional row.  
It can also play with any number of pieces starting in any
random position if desired.  There is no limit to number of pieces
or location of pieces as long as they fit on a legal board.  There
is no limit on asymmetry of piece count either.

eg ["wwwwww","wwwww","----","---","--","---","----","bbbbb","bbbbbb"] 
  
playself allows game to play itself with same parameters as oska_i0r7 and
will play both sides of the game to conclusion

eg  playself board side moves counter (counter is board number start, just use 0)
 playself ["-ww-","ww-","--","--b","bb-b"] 'w' 7 0  # start game with partially played moves
 playself ["wwww","---","--","---","bbbb"] 'b' 7 0  # black starts new game

watching moves selected with a moves=1 setting, the pieces appear to behave
as desired, avoiding being jumped, jumping when possible, getting to goal line
when possible, blocking when possible.  When moves > 1, it's much harder to 
understand if current moves are logical or not without dumping the decision tree
and leaf node evaluation.

playself with varying 'moves' and same starting board and side does 
reflect what we would expect if we increase moves.  
As we increment moves between even and odd
the winning side seems to alternate.  I infer from this 
that the advantage of one extra move visibility to one 
side or the other results in that side having an advantage 
in play selection.

sample run of play self starting from arbitrary board with black
making first move and allowing 6 move view into board evaluations

*Main> playself ["-ww-","ww-","--","--b","bb-b"] 'b' 6 0
["-ww-","ww-","--","--b","bb-b"] 'b' 0
["-ww-","ww-","-b","---","bb-b"] 'w' 1
["-ww-","w--","--","--w","bb-b"] 'b' 2
["-ww-","w--","--","-bw","b--b"] 'w' 3
["-w--","ww-","--","-bw","b--b"] 'b' 4
["-w--","ww-","--","bbw","---b"] 'w' 5
["-w--","w--","-w","bbw","---b"] 'b' 6
["-w--","w--","bw","-bw","---b"] 'w' 7
["----","ww-","bw","-bw","---b"] 'b' 8
["----","wwb","b-","--w","---b"] 'w' 9
["----","w-b","--","w-w","---b"] 'b' 10
["----","w-b","-b","w--","----"] 'w' 11
["----","w-b","-b","---","w---"] 'b' 12
["----","wbb","--","---","w---"] 'w' 13
["----","-bb","w-","---","w---"] 'b' 14
["--b-","-b-","w-","---","w---"] 'w' 15
["--b-","-b-","--","w--","w---"] 'b' 16
["-bb-","---","--","w--","w---"] 'w' 17
["-bb-","---","--","w--","w---"]
(42.87 secs, -2375557892 bytes)

-- black wins, redoing with 7 move view results in 
*Main> playself ["-ww-","ww-","--","--b","bb-b"] 'b' 7 0
["-ww-","ww-","--","--b","bb-b"] 'b' 0
["-ww-","ww-","--","-bb","b--b"] 'w' 1
["-w--","www","--","-bb","b--b"] 'b' 2
["-w--","www","-b","-b-","b--b"] 'w' 3
["-w--","w-w","--","-bw","b--b"] 'b' 4
["-w--","w-w","-b","-b-","b---"] 'w' 5
["-w--","--w","wb","-b-","b---"] 'b' 6
["-w--","b-w","-b","---","b---"] 'w' 7
["----","bww","-b","---","b---"] 'b' 8
["----","bww","-b","b--","----"] 'w' 9
["----","b-w","wb","b--","----"] 'b' 10
["----","bbw","-b","---","----"] 'w' 11
["----","bb-","--","-w-","----"] 'b' 12
["-b--","b--","--","-w-","----"] 'w' 13
["-b--","b--","--","---","-w--"] 'b' 14
["-b--","b--","--","---","-w--"]
(78.95 secs, 3545972340 bytes)
 -- white wins , the alternation between who wins seems to consistently follow
 -- odd even number of moves ....










resources:

-print functions from Rodrigo Alves
