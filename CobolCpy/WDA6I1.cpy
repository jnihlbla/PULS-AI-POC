000100 01  SEQI-WDA6I1.                                                         
000200*                                 VOR REGISTER                            
000300*                                                                         
000400*                                 SEKUNDÄRT INDEX TILL WDA6               
000500*                                 FYSISK NYCKEL: WDA6I1KY                 
000600*                                                                         
000700*                                  (IDANSK + TIREGDAT-AVV9 +              
000800*                                  TIREGTID-AVV9 )                        
000900*                                 SEKUNDÄR NYCKEL: WDA6ISEQ               
001000*                                  (IDANSK + TIREGDAT-AVV9                
001100*                                  TIREGTID-AVV9 )                        
001200     03 SEQI-IDANSK          PIC S9(3)           COMP-3.                  
001300*                                 ANSKAFFARNUMMER                         
001400*                                 PROCURER NO.                            
001500     03 SEQI-TIREGDAT-AVV9   PIC S9(7)           COMP-3.                  
001600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001700*                                 REGISTRATION DATE (YYMMDD)              
001800     03 SEQI-TIREGTID-AVV9   PIC S9(9)           COMP-3.                  
001900*                                 KLOCKSLAG (TTMMSSTH)                    
002000*                                 TIME OF DAY (HHMMSSTH)                  
002100*** END OF VILMAII-COPY LENGTH= 11 BYTES                                  
