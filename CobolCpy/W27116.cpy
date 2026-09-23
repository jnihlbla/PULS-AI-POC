000100 01  W27116.                                                              
000200*                                 COPYTEXT FÖR TEMPFIL                    
000300*                                 I PGM W27116 E+                         
000400*                                 ARTIKLAR UTAN OT                        
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 IDDC                 PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 KVLS                 PIC S9(7)           COMP-3.                  
001200*                                 LAGERSALDO                              
001300*                                 STOCK BALANCE                           
001400     03 KVPB-REF             PIC S9(6)V9(1)      COMP-3.                  
001500*                                 PERIODBEHOV REFILLING                   
001600*                                 FORECAST REFILLING                      
001700     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
001800*                                 LAGEROMRÅDE                             
001900*                                 AREA                                    
002000     03 ADGANG               PIC S9(3)           COMP-3.                  
002100*                                 GÅNG                                    
002200*                                 AISLE                                   
002300     03 ADPLATS              PIC S9(5)           COMP-3.                  
002400*                                 LAGERPLATSNUMMER                        
002500*                                 LOCATION                                
002600     03 IDLEVNR              PIC S9(5)           COMP-3.                  
002700*                                 LEVERANTÖRNUMMER                        
002800*                                 SUPPLIER NUMBER (VENDORNUMBER)          
002900*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
