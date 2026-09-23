000100 01  SEQJ-WDA6J1.                                                         
000200*                                 VOR REGISTER                            
000300*                                                                         
000400*                                 SEKUNDÄRT INDEX TILL WDA6               
000500*                                 FYSISK NYCKEL: WDA6J1KY                 
000600*                                                                         
000700*                                  (IDARTNR+ TIREGDAT-AVV9 +              
000800*                                  TIREGTID-AVV9 )                        
000900*                                 SEKUNDÄR NYCKEL: WDA6JSEQ               
001000*                                  (IDARTNR+ TIREGDAT-AVV9                
001100*                                  TIREGTID-AVV9 )                        
001200     03 SEQJ-IDARTNR         PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400*                                 PART NUMBER                             
001500     03 SEQJ-TIREGDAT-AVV9   PIC S9(7)           COMP-3.                  
001600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001700*                                 REGISTRATION DATE (YYMMDD)              
001800     03 SEQJ-TIREGTID-AVV9   PIC S9(9)           COMP-3.                  
001900*                                 KLOCKSLAG (TTMMSSTH)                    
002000*                                 TIME OF DAY (HHMMSSTH)                  
002100*** END OF VILMAII-COPY LENGTH= 14 BYTES                                  
