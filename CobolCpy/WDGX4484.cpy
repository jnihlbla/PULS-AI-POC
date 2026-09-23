000100 01  4484-WDGX4484.                                                       
000200*                                 ÅTERSTARTS REGISTER NOAC                
000300*                                 FYSISK NYCKEL: KDSEGKEY                 
000400*                                  (SKALL VARA "1")                       
000500     03 4484-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4484-KVPOST          PIC S9(7)           COMP-3.                  
000900*                                 RÄKNARE, ANTAL POSTER                   
001000*                                 RECORD COUNTER                          
001100     03 4484-TIUPPDAT        PIC S9(7)           COMP-3.                  
001200*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001300*                                 UPDATING DATE     (YYMMDD)              
001400     03 4484-TIUPPTID        PIC S9(9)           COMP-3.                  
001500*                                 UPPDATERINGSTID  (TTMMSSTH)             
001600*                                 UPDATING TIME    (HHMMSSTH)             
001700     03 FILLER               PIC X(66).                                   
001800*** END COPY WDGX4484    LENGTH=80                                        
