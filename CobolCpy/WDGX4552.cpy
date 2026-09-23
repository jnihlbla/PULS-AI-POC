000100 01  4552-WDGX4552.                                                       
000200*                                 ÅTERSTARTS REGISTER                     
000300*                                 RESTORDER KANADA                        
000400*                                 FYSISK NYCKEL: KDSEGKEY                 
000500*                                  (SKALL VARA "1")                       
000600     03 4552-KDSEGKEY        PIC X.                                       
000700*                                 TEKNISK SEGMENT-NYCKEL                  
000800*                                 TECHNICAL SEGMENT KEY                   
000900     03 4552-KVPOST          PIC S9(7)           COMP-3.                  
001000*                                 RÄKNARE, ANTAL POSTER                   
001100*                                 RECORD COUNTER                          
001200     03 4552-IDARTNR         PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400*                                 PART NUMBER                             
001500     03 4552-TIUPPDAT        PIC S9(7)           COMP-3.                  
001600*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001700*                                 UPDATING DATE     (YYMMDD)              
001800     03 4552-TIUPPTID        PIC S9(9)           COMP-3.                  
001900*                                 UPPDATERINGSTID  (TTMMSSTH)             
002000*                                 UPDATING TIME    (HHMMSSTH)             
002100     03 4552-FILLER          PIC X(61).                                   
002200*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
