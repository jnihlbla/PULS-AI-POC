000100 01  SEQE-WDA6E1.                                                         
000200*                                 VOR REGISTER                            
000300*                                                                         
000400*                                 FYSISK NYCKEL: WDA6E1KY                 
000500*                                                                         
000600*                                  (IDDISTR + TIREGDAT-AVV9 +             
000700*                                  TIREGTID-AVV9 )                        
000800*                                 SEKUNDÄR NYCKEL: WDA6ESEQ               
000900*                                  (IDDISTR + TIREGDAT-AVV9 +             
001000*                                  TIREGTID-AVV9 )                        
001100     03 SEQE-IDDISTR         PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 SEQE-TIREGDAT-AVV9   PIC S9(7)           COMP-3.                  
001500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001600*                                 REGISTRATION DATE (YYMMDD)              
001700     03 SEQE-TIREGTID-AVV9   PIC S9(9)           COMP-3.                  
001800*                                 KLOCKSLAG (TTMMSSTH)                    
001900*                                 TIME OF DAY (HHMMSSTH)                  
002000*** END OF VILMAII-COPY LENGTH= 12 BYTES                                  
