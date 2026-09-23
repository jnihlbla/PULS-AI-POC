000100 01  SEQG-WDA6G1.                                                         
000200*                                 VOR REGISTER                            
000300*                                                                         
000400*                                 FYSISK NYCKEL: WDA6G1KY                 
000500*                                                                         
000600*                                  (IDDISTR+ IDKUNDNR+ IDARTNR +          
000700*                                 TIREGDAT-AVV9 + TIREGTID-AVV9)          
000800*                                 SEKUNDÄR NYCKEL: WDA6GSEQ               
000900*                                  (IDDISTR+ IDKUNDNR+ IDARTNR +          
001000*                                 TIREGDAT-AVV9 + TIREGTID-AVV9)          
001100     03 SEQG-IDDISTR         PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 SEQG-IDKUNDNR        PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600*                                 CUSTOMER NO                             
001700     03 SEQG-IDARTNR         PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900*                                 PART NUMBER                             
002000     03 SEQG-TIREGDAT-AVV9   PIC S9(7)           COMP-3.                  
002100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002200*                                 REGISTRATION DATE (YYMMDD)              
002300     03 SEQG-TIREGTID-AVV9   PIC S9(9)           COMP-3.                  
002400*                                 KLOCKSLAG (TTMMSSTH)                    
002500*                                 TIME OF DAY (HHMMSSTH)                  
002600*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
