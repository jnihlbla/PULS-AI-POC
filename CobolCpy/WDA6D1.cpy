000100 01  SEQD-WDA6D1.                                                         
000200*                                 VOR REGISTER                            
000300*                                                                         
000400*                                 FYSISK NYCKEL: WDA6D1KY                 
000500*                                                                         
000600*                                  (IDROLL + IDARTNR + TIREGDAT-          
000700*                                  AVV9 + TIREGTID-AVV9 )                 
000800*                                 SEKUNDÄR NYCKEL: WDA6DSEQ               
000900*                                  (IDROLL + IDARTNR + TIREGDAT-          
001000*                                  AVV9 + TIREGTID-AVV9 )                 
001100     03 SEQD-IDROLL          PIC X(5).                                    
001200*                                 VOR ROLL ID                             
001300*                                 VOR ROLE ID                             
001400     03 SEQD-IDARTNR         PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER                           
001600*                                 PART NUMBER                             
001700     03 SEQD-TIREGDAT-AVV9   PIC S9(7)           COMP-3.                  
001800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001900*                                 REGISTRATION DATE (YYMMDD)              
002000     03 SEQD-TIREGTID-AVV9   PIC S9(9)           COMP-3.                  
002100*                                 KLOCKSLAG (TTMMSSTH)                    
002200*                                 TIME OF DAY (HHMMSSTH)                  
002300*** END OF VILMAII-COPY LENGTH= 19 BYTES                                  
