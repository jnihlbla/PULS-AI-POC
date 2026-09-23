000100 01  1156-WDGX1156.                                                       
000200*                                 ÅTERSTARTS REGISTER                     
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400*                                  (SKALL VARA "1")                       
000500     03 1156-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 1156-KVPOST-0        PIC S9(7)           COMP-3.                  
000900*                                 RÄKNARE, ANTAL POSTER                   
001000*                                 RECORD COUNTER                          
001100     03 1156-KVPOST-1        PIC S9(7)           COMP-3.                  
001200*                                 RÄKNARE, ANTAL POSTER                   
001300*                                 RECORD COUNTER                          
001400     03 1156-TIUPPDAT        PIC S9(7)           COMP-3.                  
001500*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001600*                                 UPDATING DATE     (YYMMDD)              
001700     03 1156-TIUPPTID        PIC S9(9)           COMP-3.                  
001800*                                 UPPDATERINGSTID  (TTMMSSTH)             
001900*                                 UPDATING TIME    (HHMMSSTH)             
002000     03 FILLER               PIC X(12).                                   
002100*** END COPY WDGX1156C0  LENGTH=30                                        
