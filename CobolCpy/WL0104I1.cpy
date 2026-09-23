000100 01  REQU-WL0104I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WL0104             
000300*                                 LDC STOCK DATA REGISTRATION             
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-IDARTNR-KEY     PIC 9(9).                                    
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 REQU-ADLAGOMR        PIC X(2).                                    
001100*                                 LAGEROMRÅDE                             
001200*                                 AREA                                    
001300     03 REQU-ADGANG          PIC X(2).                                    
001400*                                 GÅNG                                    
001500*                                 AISLE                                   
001600     03 REQU-ADPLATS         PIC X(5).                                    
001700*                                 LAGERPLATSNUMMER                        
001800*                                 LOCATION                                
001900     03 REQU-VKART2          PIC X(10).                                   
002000*                                 ARTIKELVIKT (G/OZ)                      
002100*                                 PART WEIGHT (G/OZ)                      
002200     03 REQU-VLARTNTO        PIC X(10).                                   
002300*                                 ARTIKELVOLYM NETTO (CM3)                
002400*                                 PART NET VOLUME    (CM3)                
002500     03 REQU-KDVSOP          PIC 9(3).                                    
002600*                                 VSOP-KOD                                
002700*                                 VSOP-CODE                               
002800     03 REQU-KDMATT          PIC X.                                       
002900*                                 MÅTTKOD                                 
003000*                                 MEASUREMENT CODE                        
003100*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  
