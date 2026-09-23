000100 01  W90422O1.                                                            
000200*                                 COPYTEXT F÷R MOD W9042200               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 FILLER               PIC X(9).                                    
000800     03 FILLER               PIC X(3).                                    
000900     03 FILLER               PIC X(9).                                    
001000     03 FILLER               PIC X.                                       
001100     03 FILLER               PIC 9.                                       
001200     03 FILLER               PIC X(25).                                   
001300     03 AREA.                                                             
001400        05 FILLER            PIC 9(3).                                    
001500        05 FILLER            PIC X(5).                                    
001600        05 FILLER            PIC 9(4).                                    
001700        05 FILLER            PIC X(4).                                    
001800        05 FILLER            PIC 9(2).                                    
001900        05 FILLER            PIC X(2).                                    
002000        05 FILLER            PIC 9.                                       
002100        05 FILLER            PIC 9.                                       
002200        05 FILLER            PIC 9.                                       
002300        05 FILLER            PIC 9.                                       
002400        05 FILLER            PIC 9(3).                                    
002500        05 FILLER            PIC Z(4)9.                                   
002600        05 FILLER            PIC 9.                                       
002700        05 FILLER            PIC 9.                                       
002800        05 FILLER            PIC 9.                                       
002900        05 FILLER            PIC X.                                       
003000        05 TIURPROD          PIC 9(4).                                    
003100*                                 DATUM UTG≈TT UR PROD   (≈≈VV)           
003200        05 FILLER            PIC 9(2).                                    
003300        05 FILLER            PIC 9(2).                                    
003400        05 FILLER            PIC Z(6)9.                                   
003500        05 FILLER            PIC X.                                       
003600        05 FILLER            PIC 9(2).                                    
003700        05 FILLER            PIC X.                                       
003800        05 FILLER            PIC 9.                                       
003900        05 FILLER            PIC X(2).                                    
004000        05 FILLER            PIC Z(6)9.                                   
004100        05 FILLER            PIC 9(4).                                    
004200        05 FILLER            PIC 9(2).                                    
004300        05 FILLER            PIC X.                                       
004400        05 FILLER            PIC X.                                       
004500        05 FILLER            PIC Z(6)9.                                   
004600        05 FILLER            PIC X.                                       
004700        05 FILLER            PIC 9(2).                                    
004800        05 FILLER            PIC X.                                       
004900        05 FILLER            PIC X.                                       
005000        05 FILLER            PIC X.                                       
005100        05 FILLER            PIC 9.                                       
005200        05 FILLER            PIC 9(2).                                    
005300        05 FILLER            PIC X(2).                                    
005400        05 FILLER            PIC Z(6)9.                                   
005500        05 FILLER            PIC X.                                       
005600        05 FILLER            PIC 9(2).                                    
005700        05 FILLER            PIC X.                                       
005800        05 FILLER            PIC X.                                       
005900        05 FILLER            PIC Z(6)9.                                   
006000        05 FILLER            PIC X.                                       
006100        05 FILLER            PIC 9(2).                                    
006200        05 FILLER            PIC 9.                                       
006300        05 FILLER            PIC Z(6)9.                                   
006400        05 FILLER            PIC X.                                       
006500        05 FILLER            PIC Z(6)9.                                   
006600        05 FILLER            PIC X(5).                                    
006700        05 FILLER            PIC X(5).                                    
006800        05 FILLER            PIC X(5).                                    
006900        05 FILLER            OCCURS 3 TIMES                               
007000                             INDEXED IY                                   
007100                             PIC X(8).                                    
007200        05 FILLER            OCCURS 11 TIMES                              
007300                             INDEXED IZ                                   
007400                             PIC X(5).                                    
007500        05 FILLER            OCCURS 5 TIMES                               
007600                             INDEXED IX                                   
007700                             PIC X(10).                                   
007800        05 FILLER            PIC X(40).                                   
007900        05 TEARTNOT          OCCURS 2 TIMES                               
008000                             PIC X(40).                                   
008100*                                 ARTIKEL NOTERING                        
008200     03 TEMFSINF             PIC X(55).                                   
008300*                                 INFORMATIONSMEDDELANDE                  
008400*** END OF VILMAII-COPY LENGTH= 540 BYTES                                 
