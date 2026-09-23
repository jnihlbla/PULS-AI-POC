000100 01  LINE-W5710702.                                                       
000200*                                 INVENTORY ACS SELECTION EXCLUDE         
000300*                                 D PARTS                                 
000400     03 LINE-IDAFPRCD        PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600*                                 AFP FORMS RECORD TYPE                   
000700     03 LINE-IDARTNR         PIC Z(8)9.                                   
000800*                                 ARTIKELNUMMER                           
000900*                                 PART NUMBER                             
001000     03 LINE-BEART           PIC X(25).                                   
001100*                                 ARTIKELBENÄMNING                        
001200*                                 PART DESCRIPTION                        
001300     03 LINE-ADLAGOMR        PIC Z9.                                      
001400*                                 LAGEROMRÅDE                             
001500*                                 AREA                                    
001600     03 LINE-ADGANG          PIC Z9.                                      
001700*                                 GÅNG                                    
001800*                                 AISLE                                   
001900     03 LINE-ADPLATS         PIC Z(4)9.                                   
002000*                                 LAGERPLATSNUMMER                        
002100*                                 LOCATION                                
002200     03 LINE-ADBUFFOMR       PIC Z9.                                      
002300*                                 BUFFERTOMRÅDE                           
002400*                                 BUFFER AREA                             
002500     03 LINE-ADBUFFGANG      PIC Z9.                                      
002600*                                 BUFFERT GÅNG                            
002700     03 LINE-ADBUFFPL        PIC Z(4)9.                                   
002800*                                 BUFFERPLATSNUMMER                       
002900*                                 LOCATION IN BUFFER                      
003000     03 LINE-KVLS            PIC -(7)9.                                   
003100*                                 LAGERSALDO                              
003200*                                 STOCK BALANCE                           
003300     03 LINE-MESSAGE         PIC X(41).                                   
003400*                                 MEDDELANDE                              
003500*** END OF VILMAII-COPY LENGTH= 111 BYTES                                 
