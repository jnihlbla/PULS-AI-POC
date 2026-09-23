000100 01  6016-W6GX6016.                                                       
000200*                                 POSTRÄKNARE VID                         
000300*                                 CHECKPOINT                              
000400*                                 FYSISK NYCKEL: KDSEGKEY 1               
000500     03 6016-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 6016-KVPOST          PIC S9(7)           COMP-3.                  
000900*                                 RÄKNARE, ANTAL POSTER                   
001000*                                 RECORD COUNTER                          
001100     03 6016-TIUPPDAT        PIC S9(7)           COMP-3.                  
001200*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001300*                                 UPDATING DATE     (YYMMDD)              
001400     03 6016-TIUPPTID        PIC S9(9)           COMP-3.                  
001500*                                 UPPDATERINGSTID  (TTMMSSTH)             
001600*                                 UPDATING TIME    (HHMMSSTH)             
001700     03 FILLER               PIC X(6).                                    
001800*** END COPY W6GX6016    LENGTH=20                                        
