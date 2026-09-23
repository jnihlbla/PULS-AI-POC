000100 01  SEQH-WDK6H1.                                                         
000200*                                 ARTIKELREGISTER                         
000300*                                 SEKUNDÄRT INDEX TILL WDK629             
000400*                                 REFILL DC INFO                          
000500*                                 FYSISK NYCKEL: WDK6H1KY                 
000600*                                  (IDDC-REF + IDARTNR)                   
000700*                                 SECONDARY NYCKEL: WDK6HSEQ              
000800*                                  (IDDC-REF)                             
000900     03 SEQH-IDDC-REF        PIC X(2).                                    
001000*                                 SÄNDANDE LAGER FÖR REFILL               
001100*                                 SENDING WAREHOUSE FOR REFILL            
001200     03 SEQH-IDARTNR         PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400*                                 PART NUMBER                             
001500     03 SEQH-IDPERSON-BUY    PIC S9(3)           COMP-3.                  
001600*                                 PERSONKOD REFILLANSVARIG                
001700*                                 REFILL RESPONSIBLE ID                   
001800*** END OF VILMAII-COPY LENGTH= 9 BYTES                                   
