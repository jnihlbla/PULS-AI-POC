000100 01  SEQC-WDA6C1.                                                         
000200*                                 VOR REGISTER                            
000300*                                                                         
000400*                                 FYSISK NYCKEL: WDA6C1KY                 
000500*                                                                         
000600*                                  (IDROLL + TIREGDAT-AVV9 +              
000700*                                  TIREGTID-AVV9 )                        
000800*                                 SEKUNDÄR NYCKEL: WDA6CSEQ               
000900*                                  (IDROLL + TIREGDAT-AVV9                
001000*                                  TIREGTID-AVV9 )                        
001100     03 SEQC-IDROLL          PIC X(5).                                    
001200*                                 VOR ROLL ID                             
001300*                                 VOR ROLE ID                             
001400     03 SEQC-TIREGDAT-AVV9   PIC S9(7)           COMP-3.                  
001500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001600*                                 REGISTRATION DATE (YYMMDD)              
001700     03 SEQC-TIREGTID-AVV9   PIC S9(9)           COMP-3.                  
001800*                                 KLOCKSLAG (TTMMSSTH)                    
001900*                                 TIME OF DAY (HHMMSSTH)                  
002000*** END OF VILMAII-COPY LENGTH= 14 BYTES                                  
