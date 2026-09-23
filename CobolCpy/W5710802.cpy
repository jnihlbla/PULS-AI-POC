000100 01  LINE-W5710802.                                                       
000200*                                 INVENTORY ACS AUDIT LIST                
000300     03 LINE-IDAFPRCD        PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500*                                 AFP FORMS RECORD TYPE                   
000600     03 LINE-IDARTNR         PIC Z(8)9.                                   
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 LINE-BEART           PIC X(25).                                   
001000*                                 ARTIKELBENÄMNING                        
001100*                                 PART DESCRIPTION                        
001200     03 LINE-ADLAGOMR        PIC Z9.                                      
001300*                                 LAGEROMRÅDE                             
001400*                                 AREA                                    
001500     03 LINE-ADGANG          PIC Z9.                                      
001600*                                 GÅNG                                    
001700*                                 AISLE                                   
001800     03 LINE-ADPLATS         PIC Z(4)9.                                   
001900*                                 LAGERPLATSNUMMER                        
002000*                                 LOCATION                                
002100     03 LINE-KVLS-DEV        PIC -(7)9.                                   
002200*                                 AVVIAKNDE ANTAL AV LAGERSALDO           
002300*                                 DEVIATION OF STOCK BALANCE              
002400     03 LINE-SUAVCOST-DEV    PIC -(8)9.9(2).                              
002500*                                 AVVIKANDE SUMMA                         
002600*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
002700*                                 DEV SUM AVERAGE COST FOREIGN            
002800*                                 CURRENCY                                
002900*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  
