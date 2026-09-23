000100 01  MID-W90425I1.                                                        
000200*                                 MID-COPYTEXT FÖR W9042500               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-FILLER           PIC X.                                       
000800     03 MID-INPUT.                                                        
000900        05 MID-FILLER        PIC X(4).                                    
001000        05 MID-FILLER        PIC X.                                       
001100        05 MID-FILLER        PIC 9(7).                                    
001200        05 MID-FILLER        PIC X.                                       
001300        05 MID-FILLER        PIC 9(3).                                    
001400        05 MID-FILLER        PIC 9(5).                                    
001500        05 MID-FILLER        PIC X.                                       
001600        05 MID-FILLER        PIC X.                                       
001700        05 MID-FILLER        PIC X.                                       
001800        05 MID-FILLER        PIC X.                                       
001900        05 MID-FILLER        PIC X.                                       
002000        05 MID-FILLER        PIC X.                                       
002100        05 MID-FILLER        PIC 9(7).                                    
002200        05 MID-FILLER        PIC X.                                       
002300        05 MID-FILLER        PIC X(8).                                    
002400        05 MID-FILLER        PIC 9(4).                                    
002500        05 MID-FILLER        PIC X.                                       
002600        05 MID-INPUT-TILEVDAGAR.                                          
002700*                                                                         
002800           07 MID-FILLER     OCCURS 5 TIMES                               
002900                             PIC X(2).                                    
003000        05 MID-TEARTNOT1     PIC X(40).                                   
003100*                                 ARTIKEL NOTERING                        
003200        05 MID-TEARTNOT2     PIC X(40).                                   
003300*                                 ARTIKEL NOTERING                        
003400     03 MID-SPAR-TILEVDAGAR.                                              
003500*                                                                         
003600        05 MID-FILLER        OCCURS 5 TIMES                               
003700                             PIC X(2).                                    
003800*** END OF VILMAII-COPY LENGTH= 167 BYTES                                 
