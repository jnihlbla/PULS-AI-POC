000100 01  SEQM-WDA6M1.                                                         
000200*                                 VOR REGISTER                            
000300*                                 ARTIKEL/REG.TID INGÅNG                  
000400*                                 SEKUNDÄRT INDEX TILL WDA6               
000500*                                 FYSISK NYCKEL: WDA6M1KY                 
000600*                                  (TIREGDAT-URSP + TIREGTID-URSP         
000700*                                  + IDARTNR )                            
000800*                                 SEKUNDÄR NYCKEL: WDA6MSEQ               
000900*                                  (TIREGDAT-URSP + TIREGTID-URSP         
001000*                                  + IDARTNR )                            
001100     03 SEQM-TIREGDAT-URSP   PIC S9(7)           COMP-3.                  
001200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001300*                                 REGISTRATION DATE (YYMMDD)              
001400     03 SEQM-TIREGTID-URSP   PIC S9(9)           COMP-3.                  
001500*                                 KLOCKSLAG (TTMMSSTH)                    
001600*                                 TIME OF DAY (HHMMSSTH)                  
001700     03 SEQM-IDARTNR         PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900*                                 PART NUMBER                             
002000*** END OF VILMAII-COPY LENGTH= 14 BYTES                                  
