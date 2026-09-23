000100 01  W-WWFORDON.                                                          
000200*                                 ANVÄNDS VID TEST AV GODKÄNDA            
000300*                                 FORDONSKODER INOM KATALOGSYSTEM         
000400     03 W-MAX-ANTAL          PIC S9(4)           COMP                     
000500                             VALUE +3.                                    
000600     03 W-FORDONSVARDE.                                                   
000700        05 FILLER            PIC X(2)                                     
000800                             VALUE 'PV'.                                  
000900        05 FILLER            PIC S9(3)           COMP-3                   
001000                             VALUE +010.                                  
001100        05 FILLER            PIC X(2)                                     
001200                             VALUE 'NL'.                                  
001300        05 FILLER            PIC S9(3)           COMP-3                   
001400                             VALUE +050.                                  
001500        05 FILLER            PIC X(2)                                     
001600                             VALUE 'RE'.                                  
001700        05 FILLER            PIC S9(3)           COMP-3                   
001800                             VALUE +900.                                  
001900     03 FILLER REDEFINES W-FORDONSVARDE.                                  
002000        05 W-FORDONSLAG      OCCURS 3 TIMES                               
002100                             INDEXED IX.                                  
002200           07 W-KDFORDON     PIC X(2).                                    
002300*                                 FORDONSSLAG                             
002400           07 W-IDFORDON     PIC S9(3)           COMP-3.                  
002500*                                 FORDONSSLAG                             
002600*** END COPY WWFORDON    LENGTH=14                                        
