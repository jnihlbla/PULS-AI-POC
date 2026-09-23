000100 01  SEQH-WDA6H1.                                                         
000200*                                 VOR REGISTER                            
000300*                                                                         
000400*                                 FYSISK NYCKEL: WDA6H1KY                 
000500*                                                                         
000600*                                  (IDDISTR+ IDARTNR + TIREGDAT-          
000700*                                  AVV9 + TIREGTID-AVV9 )                 
000800*                                 SEKUNDÄR NYCKEL: WDA6HSEQ               
000900*                                  (IDDISTR+ IDARTNR + TIREGDAT-          
001000*                                  AVV9 + TIREGTID-AVV9 )                 
001100     03 SEQH-IDDISTR         PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 SEQH-IDARTNR         PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600*                                 PART NUMBER                             
001700     03 SEQH-TIREGDAT-AVV9   PIC S9(7)           COMP-3.                  
001800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001900*                                 REGISTRATION DATE (YYMMDD)              
002000     03 SEQH-TIREGTID-AVV9   PIC S9(9)           COMP-3.                  
002100*                                 KLOCKSLAG (TTMMSSTH)                    
002200*                                 TIME OF DAY (HHMMSSTH)                  
002300*** END OF VILMAII-COPY LENGTH= 17 BYTES                                  
