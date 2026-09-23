000100 01  4558-WDGX4558.                                                       
000200*                                 ÅTERSTARTS REGISTER RESTORDER           
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400*                                  (SKALL VARA "1")                       
000500     03 4558-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4558-KVPOST          PIC S9(7)           COMP-3.                  
000900*                                 RÄKNARE, ANTAL POSTER                   
001000*                                 RECORD COUNTER                          
001100     03 4558-IDARTNR         PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300*                                 PART NUMBER                             
001400     03 4558-TIUPPDAT        PIC S9(7)           COMP-3.                  
001500*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001600*                                 UPDATING DATE     (YYMMDD)              
001700     03 4558-TIUPPTID        PIC S9(9)           COMP-3.                  
001800*                                 UPPDATERINGSTID  (TTMMSSTH)             
001900*                                 UPDATING TIME    (HHMMSSTH)             
002000     03 FILLER               PIC X(61).                                   
002100*** END COPY WDGX4558C0  LENGTH=80                                        
