-- 20131025
-- richard lum
--  oska project
--  at command line    
--		:l testoska
--		runTestTT alltests
-- this will automatically load in oska_i0r7 and run all test cases against it


import Test.HUnit
import Oska_i0r7
import Data.List


board01 =  ["www-","-w-","--","b--","-bbb"]
expct00 =  ["ww--","-ww","--","b--","-bbb"]
expct01	=	sort("w-b")
expct02	=	"www--w---b---bbb"
expct08 = [(3,0),(4,1),(4,2),(4,3)]
expct09 = [(0,0),(0,1),(0,2),(1,1)]

test01 = TestCase (assertEqual "for: test01" expct00		(oska_i0r7 board01	'w' 2		))
test02 = TestCase (assertEqual "for: test02" expct01 		(sort (getsides board01 [])		))
test03 = TestCase (assertEqual "for: test03" expct02		(flatten board01				))
test04 = TestCase (assertEqual "for: test04" [0,1,2]		(getCols 'w' (board01!!0) 0		))
test05 = TestCase (assertEqual "for: test05" [1,2,3]		(getCols 'b' (board01!!4) 0		))
test06 = TestCase (assertEqual "for: test06" []				(getCols 'b' (board01!!0) 0		))
test07 = TestCase (assertEqual "for: test07" 4				(sidecount 'b' board01	 		))
test08 = TestCase (assertEqual "for: test08" expct08		(getPositions 'b' board01 0		))
test09 = TestCase (assertEqual "for: test09" expct09		(getPositions 'w' board01 0		))
test10 = TestCase (assertEqual "for: test10" True			(canMoveRight (1,1) board01		))
--test10 = TestCase (assertEqual "dummY"  True True)
tests00 = TestList[ test01,test02,test03,test04,test05,test06,test07,test08,test09 ,test10]

board10 =  ["www-","--w","-w","bw-","-bbb"]
--board01 =  ["www-","-w-","--","b--","-bbb"]
expct18 =    ["-ww-","ww-","--","b--","-bbb"]
board19 =  ["www-","---","ww","---","----"]
expct19 =  ["www-","---","-w","-w-","----"]
expct20 =  ["www-","---","w-","--w","----"]

test11 = TestCase (assertEqual "for: test11" True			(canMoveRight (2,1) board10		))
test12 = TestCase (assertEqual "for: test12" False	 		(canMoveRight (0,2) board10		))
test13 = TestCase (assertEqual "for: test13" True			(canMoveLeft (0,1) board10		))
test14 = TestCase (assertEqual "for: test14" False			(canMoveLeft (0,0) board10		))
test15 = TestCase (assertEqual "for: test15" False			(canMoveLeft (2,1) board10		))
test16 = TestCase (assertEqual "for: test16" False			(canMoveLeft (3,1) board10		))
test17 = TestCase (assertEqual "for: test17" True			(canMoveRight (2,1) board10		))
test18 = TestCase (assertEqual "for: test18" expct18		(moveRight (0,0) board01	 	))
test19 = TestCase (assertEqual "for: test19" expct19		(moveRight (2,0) board19	 	))
test20 = TestCase (assertEqual "for: test20" expct20		(moveRight (2,1) board19	 	))

tests01 = TestList[ test11,test12,test13,test14,test15,test16,test17,test18 ,test19,test20 ]

board31 = ["wwww","---","ww","---","----"]
expct31 = ["-www","w--","ww","---","----"]
expct32 = ["w-ww","-w-","ww","---","----"]
expct33 = ["ww-w","--w","ww","---","----"]
expct34 = ["wwww","---","-w","-w-","----"]
expct35 = ["wwww","---","w-","--w","----"]

board36 = ["----","www","--","www","----"]
expct36 = ["----","-ww","w-","www","----"]
expct37 = ["----","w-w","-w","www","----"]
expct38 = ["----","www","--","-ww","-w--"]
expct39 = ["----","www","--","w-w","--w-"]
expct40 = ["----","www","--","ww-","---w"]


test31 = TestCase (assertEqual "for: test31" expct31		(moveRight (0,0) board31	 	))
test32 = TestCase (assertEqual "for: test32" expct32	 	(moveRight (0,1) board31	 	))
test33 = TestCase (assertEqual "for: test33" expct33		(moveRight (0,2) board31	 	))
test34 = TestCase (assertEqual "for: test34" expct34		(moveRight (2,0) board31	 	))
test35 = TestCase (assertEqual "for: test35" expct35		(moveRight (2,1) board31	 	))
test36 = TestCase (assertEqual "for: test36" expct36		(moveRight (1,0) board36	 	))
test37 = TestCase (assertEqual "for: test37" expct37		(moveRight (1,1) board36	 	))
test38 = TestCase (assertEqual "for: test38" expct38		(moveRight (3,0) board36	 	))
test39 = TestCase (assertEqual "for: test39" expct39		(moveRight (3,1) board36	 	))
                                             
test40 =TestCase (assertEqual "for:  test40" expct40		(moveRight (3,2) board36	 	))
test41 = TestCase (assertEqual "for: test41" []				(moveRight (1,2) board36	 	))
-- causes error 
test42 = TestCase (assertEqual "for: test42" []		 		(moveRight (0,4) board36	 	))
--
test43 = TestCase (assertEqual "for: test43" []				(moveRight (0,3) board31	 	))
test44 = TestCase (assertEqual "for: test44" []				(moveRight (4,0) board31	 	))
test45 = TestCase (assertEqual "for: test45" []				(moveRight (4,3) board31	 	))


-- board31 = ["wwww","---","ww","---","----"]
expct47 = ["w-ww","w--","ww","---","----"]
expct48 = ["ww-w","-w-","ww","---","----"]
expct49 = ["www-","--w","ww","---","----"]
expct50 = ["wwww","---","-w","w--","----"]
expct51 = ["wwww","---","w-","-w-","----"]

-- board36 = ["----","www","--","www","----"]
expct53 = ["----","w-w","w-","www","----"]
expct54 = ["----","ww-","-w","www","----"]
expct55 = ["----","www","--","-ww","w---"]
expct56 = ["----","www","--","w-w","-w--"]
expct57 = ["----","www","--","ww-","--w-"]


test46 = TestCase (assertEqual "for: test46" []				(moveLeft (0,0) board31	 	))
test47 = TestCase (assertEqual "for: test47" expct47		(moveLeft (0,1) board31	 	))
test48 = TestCase (assertEqual "for: test48" expct48		(moveLeft (0,2) board31	 	))
test49 = TestCase (assertEqual "for: test49" expct49		(moveLeft (0,3) board31	 	))
test50 = TestCase (assertEqual "for: test50" expct50		(moveLeft (2,0) board31	 	))
test51 = TestCase (assertEqual "for: test51" expct51		(moveLeft (2,1) board31	 	))
test52 = TestCase (assertEqual "for: test52" []				(moveLeft (1,0) board36	 	))
test53 = TestCase (assertEqual "for: test53" expct53		(moveLeft (1,1) board36	 	))
test54 = TestCase (assertEqual "for: test54" expct54		(moveLeft (1,2) board36	 	))
test55 = TestCase (assertEqual "for: test55" expct55		(moveLeft (3,0) board36	 	))
test56 = TestCase (assertEqual "for: test56" expct56		(moveLeft (3,1) board36	 	))
test57 = TestCase (assertEqual "for: test57" expct57		(moveLeft (3,2) board36	 	))


tests02 = TestList[test31,test32,test33,test34,test35,test36,test37,test38 ,test39,test40 ]
tests03 = TestList[test41, test42,test43,test44,test45 ,test46,test47,test48 ,test49,test50 ]
tests04 = TestList[test51, test52,test53,test54,test55 ,test56,test57 ] --,test58 ,test59,test60 ]

board58 = ["----","---","bb","---","bbbb"]
board59 = ["----","bbb","--","bbb","----"]
expct58 = ["----","---","bb","b--","-bbb"] 
expct59 =  ["----","---","bb","-b-","b-bb"]
expct60 = ["----","---","bb","--b","bb-b"] 
expct61 = [] 
expct62 = ["----","-b-","-b","---","bbbb"]
expct63 = ["----","--b","b-","---","bbbb"]
expct64 = ["----","bbb","b-","-bb","----"]
expct65 = ["----","bbb","-b","b-b","----"]
expct66 = []
expct67 = ["-b--","-bb","--","bbb","----"]
expct68 = ["--b-","b-b","--","bbb","----"]
expct69 = ["---b","bb-","--","bbb","----"]
expct70 = []

test58 = TestCase (assertEqual "for: test58" expct58		(moveUpRight	(4,0) board58	 	))
test59 = TestCase (assertEqual "for: test59" expct59		(moveUpRight	(4,1) board58	 	))
test60 = TestCase (assertEqual "for: test60" expct60		(moveUpRight	(4,2) board58	 	))
test61 = TestCase (assertEqual "for: test61" expct61		(moveUpRight	(4,3) board58		))
test62 = TestCase (assertEqual "for: test62" expct62 		(moveUpRight	(2,0) board58		))
test63 = TestCase (assertEqual "for: test63" expct63		(moveUpRight	(2,1) board58		))
test64 = TestCase (assertEqual "for: test64" expct64		(moveUpRight	(3,0) board59		))
test65 = TestCase (assertEqual "for: test65" expct65		(moveUpRight	(3,1) board59		))
test66 = TestCase (assertEqual "for: test66" expct66		(moveUpRight	(3,2) board59		))
--
test67 = TestCase (assertEqual "for: test67" expct67		(moveUpRight	(1,0) board59		))
--
test68 = TestCase (assertEqual "for: test68" expct68		(moveUpRight	(1,1) board59	 	))
test69 = TestCase (assertEqual "for: test69" expct69		(moveUpRight	(1,2) board59	 	))
test70 = TestCase (assertEqual "for: test70" expct70		(moveUpRight	(0,0) board59	 	))

tests05 = TestList[ test58,test59, test60, test61,test62,test63,test64,test65,test66,test67,test68,test69, test70]

--board58 = ["----","---","bb","---","bbbb"]
--board59 = ["----","bbb","--","bbb","----"]
expct71 = [] 
expct72 = ["----","---","bb","b--","b-bb"]
expct73 = ["----","---","bb","-b-","bb-b"]
expct74 = ["----","---","bb","--b","bbb-"] 
expct75 = ["----","b--","-b","---","bbbb"]
expct76 = ["----","-b-","b-","---","bbbb"]
expct77 = []
expct78 = ["----","bbb","b-","b-b","----"]
expct79 = ["----","bbb","-b","bb-","----"]
expct80 = ["b---","-bb","--","bbb","----"]
expct81 = ["-b--","b-b","--","bbb","----"]
expct82 = ["--b-","bb-","--","bbb","----"]

expct83 = []

test71 = TestCase (assertEqual "for: test71" expct71		(moveUpLeft	(4,0) board58	 	))
test72 = TestCase (assertEqual "for: test72" expct72		(moveUpLeft	(4,1) board58	 	))
test73 = TestCase (assertEqual "for: test73" expct73		(moveUpLeft	(4,2) board58	 	))
test74 = TestCase (assertEqual "for: test74" expct74		(moveUpLeft	(4,3) board58		))
test75 = TestCase (assertEqual "for: test75" expct75 		(moveUpLeft	(2,0) board58		))
test76 = TestCase (assertEqual "for: test76" expct76		(moveUpLeft	(2,1) board58		))
test77 = TestCase (assertEqual "for: test77" expct77		(moveUpLeft	(3,0) board59		))
test78 = TestCase (assertEqual "for: test78" expct78		(moveUpLeft	(3,1) board59		))
test79 = TestCase (assertEqual "for: test79" expct79		(moveUpLeft	(3,2) board59		))
test80 = TestCase (assertEqual "for: test80" expct80		(moveUpLeft	(1,0) board59		))
test81 = TestCase (assertEqual "for: test81" expct81		(moveUpLeft	(1,1) board59	 	))
test82 = TestCase (assertEqual "for: test82" expct82		(moveUpLeft	(1,2) board59	 	))
test83 = TestCase (assertEqual "for: test83" expct83		(moveUpLeft	(0,0) board59	 	))

tests07 = TestList[ test71,test72,test73,test74,test75,test76,test77,test78,test79,test80,test81,test82,test83 ]
-- tests08 = TestList[ test84,test85,test86,test87,test88,test89 ]
-- tests09 = TestList[ test91,test92,test93,test94,test95,test96,test97,test98,test99 ]


board84 = ["wwww","bbb","--","www","bbbb"]
board85 = ["----","www","bb","---","bbbb"]
board86 = ["----","www","ww","bbb","----"]
expct84 = ["-www","-bb","w-","www","bbbb"] 
expct85 = ["w-ww","b-b","-w","www","bbbb"]
expct86 = []
expct87 = [] 
expct88 = ["----","www","-w","b-b","--w-"]
expct89 = ["----","www","w-","bb-","---w"]
expct90 = ["----","-ww","-b","-w-","bbbb"]
expct91 =  ["----","w-w","b-","--w","bbbb"]
expct92 = []
expct93 = []
expct94 = []
expct95 = []


test84 = TestCase (assertEqual "for: test84" expct84		(jumpRight	(0,0) board84		))
test85 = TestCase (assertEqual "for: test85" expct85 		(jumpRight	(0,1) board84		))
test86 = TestCase (assertEqual "for: test86" expct86		(jumpRight	(0,2) board84		))
test87 = TestCase (assertEqual "for: test87" expct87		(jumpRight	(0,3) board84		))
test88 = TestCase (assertEqual "for: test88" expct88		(jumpRight	(2,0) board86		))
test89 = TestCase (assertEqual "for: test89" expct89		(jumpRight	(2,1) board86		))
test90 = TestCase (assertEqual "for: test90" expct90		(jumpRight	(1,0) board85		))
test91 = TestCase (assertEqual "for: test91" expct91		(jumpRight	(1,1) board85	 	))
test92 = TestCase (assertEqual "for: test92" expct92		(jumpRight	(1,2) board85	 	))
test93 = TestCase (assertEqual "for: test93" expct93		(jumpRight	(3,0) board84	 	))
test94 = TestCase (assertEqual "for: test91" expct94		(jumpRight	(3,1) board84	 	))
test95 = TestCase (assertEqual "for: test92" expct95		(jumpRight	(3,2) board84	 	))
--test96 = TestCase (assertEqual "for: test93" expct96		(jumpRight	(3,2) board84	 	))


tests08 = TestList[test84,test85,test86,test87,test88,test89,test90,test91,test92,test93,test94,test95]

--board84 = ["wwww","bbb","--","www","bbbb"]
--board85 = ["----","www","bb","---","bbbb"]
--board86 = ["----","www","ww","bbb","----"]

expct96  = []
expct97  = [] 
expct98  = ["ww-w","b-b","w-","www","bbbb"]
expct99  = ["www-","bb-","-w","www","bbbb"]
expct100 = ["----","www","-w","-bb","w---"]
expct101 = ["----","www","w-","b-b","-w--"]
expct102 = []
expct103 = ["----","w-w","-b","w--","bbbb"]
expct104 = ["----","ww-","b-","-w-","bbbb"]
expct105 = []
expct106 = [] 
expct107 = [] 
expct108 = []

test96  = TestCase (assertEqual "for: test96  " expct96 		(jumpLeft	(0,0) board84		))
test97  = TestCase (assertEqual "for: test97  " expct97  		(jumpLeft	(0,1) board84		))
test98  = TestCase (assertEqual "for: test98  " expct98 		(jumpLeft	(0,2) board84		))
test99  = TestCase (assertEqual "for: test99  " expct99 		(jumpLeft	(0,3) board84		))
test100 = TestCase (assertEqual "for: test100 " expct100		(jumpLeft	(2,0) board86		))
test101 = TestCase (assertEqual "for: test101 " expct101		(jumpLeft	(2,1) board86		))
test102 = TestCase (assertEqual "for: test102 " expct102		(jumpLeft	(1,0) board85		))
test103 = TestCase (assertEqual "for: test103 " expct103		(jumpLeft	(1,1) board85	 	))
test104 = TestCase (assertEqual "for: test104 " expct104		(jumpLeft	(1,2) board85	 	))
test105 = TestCase (assertEqual "for: test105 " expct105		(jumpLeft	(3,0) board84	 	))
test106 = TestCase (assertEqual "for: test106 " expct106		(jumpLeft	(3,1) board84	 	))
test107 = TestCase (assertEqual "for: test107 " expct107		(jumpLeft	(3,2) board84	 	))
test108 = TestCase (assertEqual "for: test108 " expct108		(jumpLeft	(3,2) board84	 	))

tests09 = TestList[test96,test97,test98,test99,test100,test101,test102,test103,test104,test105,test106,
			test107,test108]


board118 = ["wwww","bbb","--","www","bbbb"]
board115 = ["bbbb","www","bb","---","bbbb"]
board113 = ["----","bbb","ww","bbb","----"]
board122 = ["wwww","---","bb","www","bbbb"]

expct109	 = []
expct110	 = [] 
expct111	 = []
expct112	 = []
expct113	 = ["w---","-bb","-w","bbb","----"]
expct114	 = ["-w--","b-b","w-","bbb","----"]
expct115	 = []
expct116	 = []
expct117	 = []
expct118	 = []
expct119	 = ["wwww","w--","-b","w-w","bbbb"] 
expct120	 = ["wwww","-w-","b-","ww-","bbbb"] 
expct121	 = []
expct122	 = []
expct123	 = ["wwww","bbb","b-","w-w","bb-b"]
expct124	 = ["wwww","bbb","-b","ww-","bbb-"]

test109 = TestCase (assertEqual "for: test109" expct109	(jumpUpLeft	(0,0) board84		))
test110 = TestCase (assertEqual "for: test110" expct110	(jumpUpLeft	(0,1) board84		))
test111 = TestCase (assertEqual "for: test111" expct111	(jumpUpLeft	(0,2) board84		))
test112 = TestCase (assertEqual "for: test112" expct112	(jumpUpLeft	(0,3) board84		))
test113 = TestCase (assertEqual "for: test113" expct113	(jumpUpLeft	(2,0) board113		))
test114 = TestCase (assertEqual "for: test114" expct114	(jumpUpLeft	(2,1) board113		))
test115 = TestCase (assertEqual "for: test115" expct115	(jumpUpLeft	(1,0) board115		))
test116 = TestCase (assertEqual "for: test116" expct116	(jumpUpLeft	(1,1) board115	 	))
test117 = TestCase (assertEqual "for: test117" expct117	(jumpUpLeft	(1,2) board115	 	))
test118 = TestCase (assertEqual "for: test118" expct118	(jumpUpLeft	(3,0) board122	 	))
test119 = TestCase (assertEqual "for: test119" expct119	(jumpUpLeft	(3,1) board122	 	))
test120 = TestCase (assertEqual "for: test120" expct120	(jumpUpLeft	(3,2) board122	 	))
test121 = TestCase (assertEqual "for: test121" expct121	(jumpUpLeft	(4,0) board118	 	))
test122 = TestCase (assertEqual "for: test122" expct122	(jumpUpLeft	(4,1) board118	 	))
test123 = TestCase (assertEqual "for: test123" expct123	(jumpUpLeft	(4,2) board118	 	))
test124 = TestCase (assertEqual "for: test124" expct124	(jumpUpLeft	(4,3) board118	 	))

tests10 = TestList[test109,test110,test111,test112,test113,test114,test115,test116,
			test117,test118,test119,test120,test121,test122,test123,test124]

-- board118 = ["wwww","bbb","--","www","bbbb"]
-- board115 = ["bbbb","www","bb","---","bbbb"]
-- board113 = ["----","bbb","ww","bbb","----"]
-- board122 = ["wwww","---","bb","www","bbbb"]

expct125	 = []
expct126	 = [] 
expct127	 = []
expct128	 = []
expct129	 = ["--w-","b-b","-w","bbb","----"]
expct130	 = ["---w","bb-","w-","bbb","----"]
expct131	 = []
expct132	 = []
expct133	 = []
expct134	 = ["wwww","-w-","-b","-ww","bbbb"]  -- tired but this looks wrong
expct135	 = ["wwww","--w","b-","w-w","bbbb"] 
expct136	 = [] 
expct137	 = ["wwww","bbb","b-","-ww","-bbb"]
expct138	 =  ["wwww","bbb","-b","w-w","b-bb"]
expct139	 = []
expct140	 = []

test125 = TestCase (assertEqual "for: test125  " expct125	(jumpUpRight	(0,0) board84		))
test126 = TestCase (assertEqual "for: test126  " expct126	(jumpUpRight	(0,1) board84		))
test127 = TestCase (assertEqual "for: test127  " expct127	(jumpUpRight	(0,2) board84		))
test128 = TestCase (assertEqual "for: test128  " expct128	(jumpUpRight	(0,3) board84		))
test129 = TestCase (assertEqual "for: test129  " expct129	(jumpUpRight	(2,0) board113		))
test130 = TestCase (assertEqual "for: test130  " expct130	(jumpUpRight	(2,1) board113		))
test131 = TestCase (assertEqual "for: test131  " expct131	(jumpUpRight	(1,0) board115		))
test132 = TestCase (assertEqual "for: test132  " expct132	(jumpUpRight	(1,1) board115	 	))
test133 = TestCase (assertEqual "for: test133  " expct133	(jumpUpRight	(1,2) board115	 	))
test134 = TestCase (assertEqual "for: test134  " expct134	(jumpUpRight	(3,0) board122	 	))
test135 = TestCase (assertEqual "for: test135  " expct135	(jumpUpRight	(3,1) board122	 	))
test136 = TestCase (assertEqual "for: test146  " expct136	(jumpUpRight	(3,2) board122	 	))
test137 = TestCase (assertEqual "for: test147  " expct137	(jumpUpRight	(4,0) board118	 	))
test138 = TestCase (assertEqual "for: test148  " expct138	(jumpUpRight	(4,1) board118	 	))
test139 = TestCase (assertEqual "for: test149  " expct139	(jumpUpRight	(4,2) board118	 	))
test140 = TestCase (assertEqual "for: test140  " expct140	(jumpUpRight	(4,3) board118	 	))


tests11 = TestList[test125,test126,
			test127,test128,test129,test130,test131,test132,test133,test134,
			test135,test136,test137,test138,test139,test140]

board141=["-ww-","ww-","--","---","bbbb"]
board142=["wwww","bbb","--","www","bbbb"]
board143=["----","---","ww","bbb","----"]

expct141=[["-ww-","ww-","--","b--","-bbb"],["-ww-","ww-","--","-b-","b-bb"],["-ww-","ww-","--","--b","bb-b"]]
expct142=[["-w--","www","--","---","bbbb"],["-ww-","-w-","w-","---","bbbb"],["-ww-","w--","-w","---","bbbb"]]
expct143=[["-ww-","ww-","--","b--","b-bb"],["-ww-","ww-","--","-b-","bb-b"],["-ww-","ww-","--","--b","bbb-"]]
expct144=[["-ww-","w--","w-","---","bbbb"]]
expct145=[["----","---","-w","b-b","--w-"],["----","---","w-","bb-","---w"]]
expct146=[["----","-b-","-w","-bb","----"],["----","--b","w-","b-b","----"]]
expct147=[["----","---","-w","-bb","w---"],["----","---","w-","b-b","-w--"]]
expct148=[["----","b--","-w","b-b","----"],["----","-b-","w-","bb-","----"]]

board144=["----","www","bb","---","----"]
expct149=[["----","-ww","-b","-w-","----"],["----","w-w","b-","--w","----"]]
expct150=[["--b-","w-w","-b","---","----"],["---b","ww-","b-","---","----"]]
expct151=[["----","w-w","-b","w--","----"],["----","ww-","b-","-w-","----"]]
expct152=[["b---","-ww","-b","---","----"],["-b--","w-w","b-","---","----"]]
expct153=[["----","---","-w","b-b","--w-"],["----","---","w-","bb-","---w"],["----","---","-w","-bb","w---"],["----","---","w-","b-b","-w--"]]
expct154=[["----","-b-","-w","-bb","----"],["----","--b","w-","b-b","----"],["----","b--","-w","b-b","----"],["----","-b-","w-","bb-","----"]]
expct155=[["----","-ww","-b","-w-","----"],["----","w-w","b-","--w","----"],["----","w-w","-b","w--","----"],["----","ww-","b-","-w-","----"]]
expct156=[["--b-","w-w","-b","---","----"],["---b","ww-","b-","---","----"],["b---","-ww","-b","---","----"],["-b--","w-w","b-","---","----"]]
expct157=[["-w--","www","--","---","bbbb"],["-ww-","-w-","w-","---","bbbb"],["-ww-","w--","-w","---","bbbb"],["-ww-","w--","w-","---","bbbb"]]
expct158=[["-ww-","ww-","--","b--","-bbb"],["-ww-","ww-","--","-b-","b-bb"],["-ww-","ww-","--","--b","bb-b"],["-ww-","ww-","--","b--","b-bb"],["-ww-","ww-","--","-b-","bb-b"],["-ww-","ww-","--","--b","bbb-"]]


test141 = TestCase (assertEqual "for: test141  " expct141	(moveAllRight 'b' board141		))
test142 = TestCase (assertEqual "for: test142  " expct142	(moveAllRight 'w' board141	 	))
test143 = TestCase (assertEqual "for: test143  " expct143	(moveAllLeft  'b' board141	 	))
test144 = TestCase (assertEqual "for: test144  " expct144	(moveAllLeft  'w' board141	 	))
test145 = TestCase (assertEqual "for: test145  " expct145	(jumpAllRight 'w' board143	 	))
test146 = TestCase (assertEqual "for: test146  " expct146	(jumpAllRight 'b' board143	 	))
test147 = TestCase (assertEqual "for: test147  " expct147	(jumpAllLeft  'w' board143	 	))
test148 = TestCase (assertEqual "for: test148  " expct148	(jumpAllLeft  'b' board143	 	))
test149 = TestCase (assertEqual "for: test149  " expct149	(jumpAllRight 'w' board144	 	))
test150 = TestCase (assertEqual "for: test150  " expct150	(jumpAllRight 'b' board144	 	))
test151 = TestCase (assertEqual "for: test151  " expct151	(jumpAllLeft  'w' board144	 	))
test152 = TestCase (assertEqual "for: test152  " expct152	(jumpAllLeft  'b' board144	 	))

optlist = [moveAllRight, moveAllLeft, jumpAllRight, jumpAllLeft]

test153 = TestCase (assertEqual "for: test153  " expct153	(generateAllMoves 'w' board143	optlist ))
test154 = TestCase (assertEqual "for: test154  " expct154	(generateAllMoves 'b' board143	optlist ))
test155 = TestCase (assertEqual "for: test155  " expct155	(generateAllMoves 'w' board144	optlist ))
test156 = TestCase (assertEqual "for: test156  " expct156	(generateAllMoves 'b' board144	optlist ))
test157 = TestCase (assertEqual "for: test157  " expct157	(generateAllMoves 'w' board141	optlist ))
test158 = TestCase (assertEqual "for: test158  " expct158	(generateAllMoves 'b' board141	optlist ))

-- moveAllRight 'b'  ["-ww-","ww-","--","---","bbbb"]

-- optlist = [moveAllRight, moveAllLeft, jumpAllRight, jumpAllLeft]
-- generateAllMoves 'w' ["-ww-","ww-","--","---","bbbb"] optlist

tests12 = TestList[test141,test142,test143,test144,
			test145,test146,test147,test148,test149,test150,test151,test152,test153,test154,test155,test156,
			test157,test158]

--isBlocked (2,1) ["www-","--w","-w","bw-","-bbb"]
board161 = ["www-","--w","-w","bw-","-bbb"]
board162 = ["www-","--w","w-","bw-","-bbb"]
test161 = TestCase (assertEqual "for: test161  " False		(isBlocked   (2,1) board161		))
test162 = TestCase (assertEqual "for: test162  " True		(isBlocked   (4,1) board161	 	))
test163 = TestCase (assertEqual "for: test163  " False		(isBlocked   (2,1) board161	 	))
test164 = TestCase (assertEqual "for: test164  " True		(isBlockedUp (4,1) board161	 	))
test165 = TestCase (assertEqual "for: test165  " False		(isBlockedUp (4,2) board161	 	))
test166 = TestCase (assertEqual "for: test166  " False		(isBlockedUp (3,0) board161	 	))
test167 = TestCase (assertEqual "for: test167  " False	(isBlockedUp (4,3) board161	 	))
test168 = TestCase (assertEqual "for: test168  " True	(couldJump   (2,0) board162	 	))
test169 = TestCase (assertEqual "for: test169  " False	(couldJump   (2,1) board161	 	))
test170 = TestCase (assertEqual "for: test170  " True	(couldJumpUp (3,0) board162	 	))
test171 = TestCase (assertEqual "for: test171  " True	(couldJumpUp (4,1) board162	 	))
test172 = TestCase (assertEqual "for: test172  " True	(couldJumpUp (3,0) board162	 	))

tests13 = TestList[test161,test162,test163,test164,
			test165,test166,test167,test168,test169,test170,test171,test172] --,test173] --,test174,test175,test176,test177,test178]
			
-- can test higher level functions as well but as these are static tests
-- board output, order etc will cause 'fail' without adding any value
-- As we focus on game logic. higher level functions require evaluation
-- beyond simple assertions. 

--statesearch    unexplored    side    moves   path 		      optlist
-- optlist = [moveAllRight, moveAllLeft, jumpAllRight, jumpAllLeft]
-- statesearch [["www-","--w","-w","bw-","-bbb"]] 'w' 3 [] optlist 
-- oska_i0r7 ["-ww-","ww-","--","--b","bb-b"] 'w' 2
-- playself ["-ww-","ww-","--","--b","bb-b"] 'w' 2 0


alltests = TestList [ tests00 , tests01 ,tests02 , tests03 ,tests04,tests05,tests07,tests08,
					tests09,tests10,tests11,tests12,tests13]
