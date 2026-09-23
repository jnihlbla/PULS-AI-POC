000100 01  SEQE-WDK6E1.                                                         
000200*                                 ARTIKELREGISTER                         
000300*                                 SEKUNDÄRT INDEX TILL WDK611             
000400*                                 LAGERADRESSINGÅNG                       
000500*                                 FYSISK NYCKEL: WDK6E1KY                 
000600*                                  (ADART, IDARTNR)                       
000700*                                 SECONDARY NYCKEL: WDK6ESEQ              
000800*                                  (ADART)                                
000900     03 SEQE-ADART.                                                       
001000*                                 ARTIKELADRESS I LAGRET                  
001100*                                 PARTS-ADRESS                            
001200        05 SEQE-ADLAGOMR     PIC S9(3)           COMP-3.                  
001300*                                 LAGEROMRÅDE                             
001400*                                 AREA                                    
001500        05 SEQE-ADGANG       PIC S9(3)           COMP-3.                  
001600*                                 GÅNG                                    
001700*                                 AISLE                                   
001800        05 SEQE-ADPLATS      PIC S9(5)           COMP-3.                  
001900*                                 LAGERPLATSNUMMER                        
002000*                                 LOCATION                                
002100     03 SEQE-IDARTNR         PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300*                                 PART NUMBER                             
002400*** END OF VILMAII-COPY LENGTH= 12 BYTES                                  
