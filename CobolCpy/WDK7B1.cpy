000100 01  SEQB-WDK7B1.                                                         
000200*                                 ARTIKELREGISTER                         
000300*                                 SEKUNDÄRT INDEX TILL WDK711             
000400*                                 REFILL DC INGÅNG                        
000500*                                 FYSISK NYCKEL: WDK7B1KY                 
000600*                                  (IDDC-REF, IDDC, IDARTNR)              
000700*                                 SÖKFÄLT: IDDC                           
000800*                                 SECONDARY NYCKEL: WDK7BSEQ              
000900*                                  (IDDC-REF, IDDC)                       
001000     03 SEQB-IDDC-REF        PIC X(2).                                    
001100*                                 SÄNDANDE LAGER FÖR REFILL               
001200*                                 SENDING WAREHOUSE FOR REFILL            
001300     03 SEQB-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 SEQB-IDARTNR         PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800*                                 PART NUMBER                             
001900*** END OF VILMAII-COPY LENGTH= 9 BYTES                                   
