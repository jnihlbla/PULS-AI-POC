000100 01  SEQA-WDK7A1.                                                         
000200*                                 ARTIKELREGISTER                         
000300*                                 SEKUNDÄRT INDEX TILL WDK701             
000400*                                 LAGERADRESSINGÅNG                       
000500*                                 FYSISK NYCKEL: WDK7A1KY                 
000600*                                  (IDDC, ADART, IDARTNR)                 
000700*                                 SÖKFÄLT: IDDC                           
000800*                                 SECONDARY NYCKEL: WDK7ASEQ              
000900*                                  (IDDC, ADART)                          
001000     03 SEQA-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 SEQA-ADART.                                                       
001400*                                 ARTIKELADRESS I LAGRET                  
001500*                                 PARTS-ADRESS                            
001600        05 SEQA-ADLAGOMR     PIC S9(3)           COMP-3.                  
001700*                                 LAGEROMRÅDE                             
001800*                                 AREA                                    
001900        05 SEQA-ADGANG       PIC S9(3)           COMP-3.                  
002000*                                 GÅNG                                    
002100*                                 AISLE                                   
002200        05 SEQA-ADPLATS      PIC S9(5)           COMP-3.                  
002300*                                 LAGERPLATSNUMMER                        
002400*                                 LOCATION                                
002500     03 SEQA-IDARTNR         PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700*                                 PART NUMBER                             
002800     03 SEQA-IDLEVNR         PIC X(5).                                    
002900*                                 LEVERANTÖRNUMMER                        
003000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003100*** END OF VILMAII-COPY LENGTH= 19 BYTES                                  
