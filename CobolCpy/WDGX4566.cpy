000100 01  4566-WDGX4566.                                                       
000200*                                 ÅTERSTARTS REGISTER O/E                 
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400*                                  (SKALL VARA "1")                       
000500     03 4566-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4566-KVPOST          PIC S9(7)           COMP-3.                  
000900*                                 RÄKNARE, ANTAL POSTER                   
001000*                                 RECORD COUNTER                          
001100     03 4566-TIUPPDAT        PIC S9(7)           COMP-3.                  
001200*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001300*                                 UPDATING DATE     (YYMMDD)              
001400     03 4566-TIUPPTID        PIC S9(9)           COMP-3.                  
001500*                                 UPPDATERINGSTID  (TTMMSSTH)             
001600*                                 UPDATING TIME    (HHMMSSTH)             
001700     03 FILLER               PIC X(66).                                   
001800*** END COPY WDGX4566    LENGTH=80                                        
