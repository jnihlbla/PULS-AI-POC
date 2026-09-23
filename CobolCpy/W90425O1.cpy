000100 01  MOD-W90425O1.                                                        
000200*                                 MOD-COPYTEXT FÖR W9042500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-FILLER           PIC X(2).                                    
000800     03 MOD-FILLER           PIC X(9).                                    
000900     03 MOD-FILLER           PIC X.                                       
001000     03 MOD-FILLER           PIC X(25).                                   
001100     03 MOD-FILLER           PIC X(2).                                    
001200     03 MOD-FILLER           PIC 9(4).                                    
001300     03 MOD-FILLER           PIC X(2).                                    
001400     03 MOD-FILLER           PIC X.                                       
001500     03 MOD-FILLER           PIC X(2).                                    
001600     03 MOD-FILLER           PIC Z(6)9.                                   
001700     03 MOD-FILLER           PIC X(2).                                    
001800     03 MOD-FILLER           PIC X(2).                                    
001900     03 MOD-FILLER           PIC X(2).                                    
002000     03 MOD-FILLER           PIC X(2).                                    
002100     03 MOD-FILLER           PIC X(2).                                    
002200     03 MOD-FILLER           PIC X(2).                                    
002300     03 MOD-FILLER           PIC X(2).                                    
002400     03 MOD-FILLER           PIC 9.                                       
002500     03 MOD-FILLER           PIC X(2).                                    
002600     03 MOD-FILLER           PIC 9(3).                                    
002700     03 MOD-FILLER           PIC X(2).                                    
002800     03 MOD-FILLER           PIC Z(4)9.                                   
002900     03 MOD-FILLER           PIC X(2).                                    
003000     03 MOD-FILLER           PIC X(2).                                    
003100     03 MOD-FILLER           PIC X(2).                                    
003200     03 MOD-FILLER           PIC X(2).                                    
003300     03 MOD-FILLER           PIC X(2).                                    
003400     03 MOD-FILLER           PIC X(2).                                    
003500     03 MOD-FILLER           PIC X(2).                                    
003600     03 MOD-FILLER           PIC 9.                                       
003700     03 MOD-FILLER           PIC X(2).                                    
003800     03 MOD-FILLER           PIC X.                                       
003900     03 MOD-FILLER           PIC X(2).                                    
004000     03 MOD-FILLER           PIC X.                                       
004100     03 MOD-FILLER           PIC X(2).                                    
004200     03 MOD-FILLER           PIC X(2).                                    
004300     03 MOD-FILLER           PIC X(2).                                    
004400     03 MOD-FILLER           PIC X(2).                                    
004500     03 MOD-FILLER           PIC X(2).                                    
004600     03 MOD-FILLER           PIC X.                                       
004700     03 MOD-FILLER           PIC X(2).                                    
004800     03 MOD-FILLER           PIC 9.                                       
004900     03 MOD-FILLER           PIC X(2).                                    
005000     03 MOD-FILLER           PIC X.                                       
005100     03 MOD-FILLER           PIC X(2).                                    
005200     03 MOD-FILLER           PIC X.                                       
005300     03 MOD-FILLER           PIC X(2).                                    
005400     03 MOD-FILLER           PIC X(2).                                    
005500     03 MOD-FILLER           PIC X(2).                                    
005600     03 MOD-FILLER           PIC X(2).                                    
005700     03 MOD-FILLER           PIC X(2).                                    
005800     03 MOD-FILLER           PIC X(2).                                    
005900     03 MOD-FILLER           PIC X(2).                                    
006000     03 MOD-FILLER           PIC Z(6)9.                                   
006100     03 MOD-FILLER           PIC X(2).                                    
006200     03 MOD-FILLER           PIC X.                                       
006300     03 MOD-FILLER           PIC X(2).                                    
006400     03 MOD-FILLER           PIC X(8).                                    
006500     03 MOD-FILLER           PIC X(2).                                    
006600     03 MOD-FILLER           PIC X(2).                                    
006700     03 MOD-FILLER           PIC X(2).                                    
006800     03 MOD-FILLER           PIC X(2).                                    
006900     03 MOD-FILLER           PIC X(2).                                    
007000     03 MOD-FILLER           PIC X(2).                                    
007100     03 MOD-FILLER           PIC X(2).                                    
007200     03 MOD-FILLER           PIC 9(4).                                    
007300     03 MOD-FILLER           PIC X(2).                                    
007400     03 MOD-FILLER           PIC X.                                       
007500     03 MOD-FILLER           PIC X(2).                                    
007600     03 MOD-FILLER           PIC X(2).                                    
007700     03 MOD-FILLER           PIC X(2).                                    
007800     03 MOD-FILLER           PIC X(2).                                    
007900     03 MOD-TILEVDAGAR       OCCURS 5 TIMES.                              
008000*                                                                         
008100        05 MOD-FILLER        PIC X(2).                                    
008200        05 MOD-FILLER        PIC X(2).                                    
008300     03 MOD-SPAR-TILEVDAGAR.                                              
008400*                                                                         
008500        05 MOD-FILLER        OCCURS 5 TIMES                               
008600                             PIC X(2).                                    
008700     03 MOD-TEARTNOT1-IN-ATTR                                             
008800                             PIC X(2).                                    
008900*                                 MFS BEHANDLING AV INPUTFÄLT             
009000     03 MOD-TEARTNOT1-IN     PIC X(40).                                   
009100*                                 ARTIKEL NOTERING                        
009200     03 MOD-TEARTNOT2-IN-ATTR                                             
009300                             PIC X(2).                                    
009400*                                 MFS BEHANDLING AV INPUTFÄLT             
009500     03 MOD-TEARTNOT2-IN     PIC X(40).                                   
009600*                                 ARTIKEL NOTERING                        
009700     03 MOD-TEMFSINF         PIC X(55).                                   
009800*                                 INFORMATIONSMEDDELANDE                  
009900*** END OF VILMAII-COPY LENGTH= 399 BYTES                                 
