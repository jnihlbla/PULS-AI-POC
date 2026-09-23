000100 01  SEQA-WDM9A1.                                                         
000200*                                 SEKUNDÄRT INDEX TILL WDM911             
000300*                                 FYSISK NYCKEL: WDM9A1KY                 
000400*                                 (ADTRDEST, DADATTI9, IDARTNR)           
000500*                                 SECONDARY KEY: WDM9ASEQ                 
000600*                                 (ADTRDEST)                              
000700     03 SEQA-ADTRDEST        PIC X(3).                                    
000800*                                 TRANSPORTDESTINATION                    
000900*                                 ADDRESS OF TRANSPORT                    
001000     03 SEQA-DADATTID-9KOMPL PIC 9(14).                                   
001100*                                 DATUM+KLOCKSLAG 9-KOMPLEMENT            
001200*                                 DATE+TIME 9-COMPLEMENT                  
001300     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600*** END OF VILMAII-COPY LENGTH= 22 BYTES                                  
