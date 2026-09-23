000100 01  SEQL-WDA6L1.                                                         
000200*                                 VOR REGISTER                            
000300*                                                                         
000400*                                 SEKUNDÄRT INDEX TILL WDA6               
000500*                                 FYSISK NYCKEL: WDA6L1KY                 
000600*                                                                         
000700*                                  (IDLEVNR+ TIREGDAT-AVV9 +              
000800*                                  TIREGTID-AVV9 )                        
000900*                                 SEKUNDÄR NYCKEL: WDA6LSEQ               
001000*                                  (IDLEVNR+ TIREGDAT-AVV9                
001100*                                  TIREGTID-AVV9 )                        
001200     03 SEQL-IDLEVNR         PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001500     03 SEQL-TIREGDAT-AVV9   PIC S9(7)           COMP-3.                  
001600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001700*                                 REGISTRATION DATE (YYMMDD)              
001800     03 SEQL-TIREGTID-AVV9   PIC S9(9)           COMP-3.                  
001900*                                 KLOCKSLAG (TTMMSSTH)                    
002000*                                 TIME OF DAY (HHMMSSTH)                  
002100*** END OF VILMAII-COPY LENGTH= 14 BYTES                                  
