000100 01  RESP-W50294O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM W50294         
000300*                                 INVENTORY COUNT UPDATE                  
000400     03 RESP-FLBLINDCO       PIC X.                                       
000500*                                 ANVÄND BLIND COUNT?                     
000600*                                 USE BLIND COUNT?                        
000700     03 RESP-KVRADER         PIC Z(4)9.                                   
000800*                                 ANTAL RADER                             
000900*                                 NUMBER OF LINES                         
001000     03 RESP-TABELLRAD       OCCURS 100 TIMES.                            
001100*                                 GRUPP MED TABELLRADER                   
001200        05 RESP-ADLAGOMR-LINE                                             
001300                             PIC Z9.                                      
001400*                                 LAGEROMRÅDE                             
001500*                                 AREA                                    
001600        05 RESP-ADGANG-LINE  PIC Z9.                                      
001700*                                 GÅNG                                    
001800*                                 AISLE                                   
001900        05 RESP-ADPLATS-LINE PIC Z(4)9.                                   
002000*                                 LAGERPLATSNUMMER                        
002100*                                 LOCATION                                
002200        05 RESP-IDARTNR-LINE PIC Z(8)9.                                   
002300*                                 ARTIKELNUMMER                           
002400*                                 PART NUMBER                             
002500        05 RESP-BEART-LINE   PIC X(25).                                   
002600*                                 ARTIKELBENÄMNING                        
002700*                                 PART DESCRIPTION                        
002800        05 RESP-KVLS-LINE    PIC X(8).                                    
002900*                                 LAGERSALDO                              
003000*                                 STOCK BALANCE                           
003100        05 RESP-KVCOUNT-LINE PIC X(8).                                    
003200*                                 ANTAL TRÄFFAR                           
003300*                                 NUMBER OF HITS                          
003400*** END OF VILMAII-COPY LENGTH= 5906 BYTES                                
