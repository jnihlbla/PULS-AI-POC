000100 01  DP-LINE-W50292.                                                      
000200*                                 PRINTDOCUMENT DATA LINE                 
000300     03 DP-LINE-IDAFPRCD     PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500*                                 AFP FORMS RECORD TYPE                   
000600     03 DP-LINE-ADLAGOMR     PIC X(2).                                    
000700*                                 LAGEROMRÅDE                             
000800*                                 AREA                                    
000900     03 DP-LINE-ADGANG       PIC X(2).                                    
001000*                                 GÅNG                                    
001100*                                 AISLE                                   
001200     03 DP-LINE-ADPLATS      PIC X(5).                                    
001300*                                 LAGERPLATSNUMMER                        
001400*                                 LOCATION                                
001500     03 DP-LINE-IDARTNR      PIC Z(8)9.                                   
001600*                                 ARTIKELNUMMER                           
001700*                                 PART NUMBER                             
001800     03 DP-LINE-ADBUFFOMR    PIC X(2).                                    
001900*                                 BUFFERTOMRÅDE                           
002000*                                 BUFFER AREA                             
002100     03 DP-LINE-ADBUFFGANG   PIC X(2).                                    
002200*                                 BUFFERT GÅNG                            
002300     03 DP-LINE-ADBUFFPL     PIC X(5).                                    
002400*                                 BUFFERPLATSNUMMER                       
002500*                                 LOCATION IN BUFFER                      
002600     03 DP-LINE-BEART        PIC X(25).                                   
002700*                                 ARTIKELBENÄMNING                        
002800*                                 PART DESCRIPTION                        
002900     03 DP-LINE-KVBUFF       PIC X(8).                                    
003000*                                 FÖRÄDLAT BUFFERSALDO                    
003100*                                 BUFFER QUANTANTITY PREPARED             
003200     03 DP-LINE-KVLS         PIC X(8).                                    
003300*                                 LAGERSALDO                              
003400*                                 STOCK BALANCE                           
003500*** END OF VILMAII-COPY LENGTH= 78 BYTES                                  
