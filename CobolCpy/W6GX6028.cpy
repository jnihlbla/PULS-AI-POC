000100 01  6028-W6GX6028.                                                       
000200*                                 POSTRÄKNARE VID                         
000300*                                 CHECKPOINT                              
000400*                                 FYSISK NYCKEL: KDSEGKEY 1               
000500     03 6028-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 6028-KVPOST          PIC S9(7)           COMP-3.                  
000900*                                 RÄKNARE, ANTAL POSTER                   
001000*                                 RECORD COUNTER                          
001100     03 6028-TIUPPDAT        PIC S9(7)           COMP-3.                  
001200*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001300*                                 UPDATING DATE     (YYMMDD)              
001400     03 6028-TIUPPTID        PIC S9(9)           COMP-3.                  
001500*                                 UPPDATERINGSTID  (TTMMSSTH)             
001600*                                 UPDATING TIME    (HHMMSSTH)             
001700     03 FILLER               PIC X(6).                                    
