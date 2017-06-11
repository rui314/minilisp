0 GOTO 110
1 REM FROM 28LINES.BAS at 
2 REM http://www.nausicaa.net/~lgreenf/cocopage.htm
10 POKE&HD7C0,&H00
20 POKE&HD816,&H14
30 POKE&HD89F,&H41
40 POKE&HD8A0,&H42
50 '*********************  Keep the CoCo Alive!!  **************************
60 '**************************  August, 1992 *******************************
70 '** Use the POKEs in this program in your OWN programs! **
80 '**     You can get 28 lines of 80 colums in BASIC!     **
90 POKE&HE046,117:POKE&HFE05,28:POKE&HFE06,&H32:POKE&HFE07,&H80:POKE&HF688,&H32:POKE&HF689,&H80:POKE&HF875,&H30:POKE&HF876,&HE0:POKE&HF8F4,28:'** 28 lines

110 PCLEAR 1
120 CLEAR200,&H3800
130 REM FROM ALLEN HUFFMAN
140 FORA=0TO8:READA$:POKE&H3800+A,VAL("&H"+A$):NEXTA:EXEC&H3800:DATAC6,1,96,BC,1F,2,7E,96,A3
150 LOADM "MINILISP"
160 CLS
170 EXEC
