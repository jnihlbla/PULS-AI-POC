000100 01  KLBL-W612KLBL.                                                       
000200*                                 COPYTEXT FOR                            
000300*                                 SUBPROGRAM W612KLBL                     
000400     03 KLBL-IDDC            PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 KLBL-PART-DATA       OCCURS 500 TIMES.                            
000800*                                                                         
000900        05 KLBL-IDARTNR      PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200        05 KLBL-ADLAGOMR     PIC 9(2).                                    
001300*                                 LAGEROMRÅDE                             
001400*                                 AREA                                    
001500        05 KLBL-ADGANG       PIC 9(2).                                    
001600*                                 GÅNG                                    
001700*                                 AISLE                                   
001800        05 KLBL-ADPLATS      PIC 9(5).                                    
001900*                                 LAGERPLATSNUMMER                        
002000*                                 LOCATION                                
002100        05 KLBL-BEART-ENG    PIC X(25).                                   
002200*                                 ENGELSK ARTIKELBENÄMNING                
002300        05 KLBL-KVAVIS       PIC S9(7)           COMP-3.                  
002400*                                 AVISERAT ANTAL                          
002500*                                 QUANTITY NOTIFIED                       
002600        05 KLBL-IDKOLLI      PIC S9(5)           COMP-3.                  
002700*                                 KOLLINUMMER                             
002800*                                 CASE NUMBER                             
002900        05 KLBL-IDORDNR7     PIC 9(7).                                    
003000*                                 ORDERNUMMER                             
003100*                                 ORDER NUMBER                            
003200        05 KLBL-KRKC         PIC X(3).                                    
003300        05 KLBL-KVOKS        PIC S9(7)           COMP-3.                  
003400*                                 ORDERKÖSALDO                            
003500*                                 ORDER QUEUE BALANCE                     
003600*** END OF VILMAII-COPY LENGTH= 30002 BYTES                               
