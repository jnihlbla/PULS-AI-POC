000100 01  SEQK-WDA6K1.                                                         
000200*                                 VOR REGISTER                            
000300*                                                                         
000400*                                 SEKUNDÄRT INDEX TILL WDA6               
000500*                                 FYSISK NYCKEL: WDA6K1KY                 
000600*                                                                         
000700*                                  (IDANSK + IDLEVNR + TIREGDAT-          
000800*                                  AVV9 + TIREGTID-AVV9 )                 
000900*                                 SEKUNDÄR NYCKEL: WDA6KSEQ               
001000*                                  (IDANSK + IDLEVNR + TIREGDAT-          
001100*                                  AVV9 + TIREGTID-AVV9 )                 
001200     03 SEQK-IDANSK          PIC S9(3)           COMP-3.                  
001300*                                 ANSKAFFARNUMMER                         
001400*                                 PROCURER NO.                            
001500     03 SEQK-IDLEVNR         PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001800     03 SEQK-TIREGDAT-AVV9   PIC S9(7)           COMP-3.                  
001900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002000*                                 REGISTRATION DATE (YYMMDD)              
002100     03 SEQK-TIREGTID-AVV9   PIC S9(9)           COMP-3.                  
002200*                                 KLOCKSLAG (TTMMSSTH)                    
002300*                                 TIME OF DAY (HHMMSSTH)                  
002400*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
