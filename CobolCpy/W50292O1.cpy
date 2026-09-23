000100 01  RESP-W50292O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM W50292         
000300*                                 INVENTORY COUNT SELECTION               
000400     03 RESP-KVRADER-TOT     PIC Z(4)9.                                   
000500*                                 TOTALT ANTAL RADER                      
000600*                                 TOTAL NUMBER OF LINES                   
000700     03 RESP-KVRADER         PIC Z(4)9.                                   
000800*                                 ANTAL RADER                             
000900*                                 NUMBER OF LINES                         
001000     03 RESP-TABELLRAD       OCCURS 100 TIMES.                            
001100*                                 GRUPP MED TABELLRADER                   
001200        05 RESP-ADLAGOMR-LINE                                             
001300                             PIC X(2).                                    
001400*                                 LAGEROMRÅDE                             
001500*                                 AREA                                    
001600        05 RESP-ADGANG-LINE  PIC X(2).                                    
001700*                                 GÅNG                                    
001800*                                 AISLE                                   
001900        05 RESP-ADPLATS-LINE PIC X(5).                                    
002000*                                 LAGERPLATSNUMMER                        
002100*                                 LOCATION                                
002200        05 RESP-IDARTNR-LINE PIC Z(8)9.                                   
002300*                                 ARTIKELNUMMER                           
002400*                                 PART NUMBER                             
002500        05 RESP-ADBUFFOMR-LINE                                            
002600                             PIC X(2).                                    
002700*                                 BUFFERTOMRÅDE                           
002800*                                 BUFFER AREA                             
002900        05 RESP-ADBUFFGANG-LINE                                           
003000                             PIC X(2).                                    
003100*                                 BUFFERT GÅNG                            
003200        05 RESP-ADBUFFPL-LINE                                             
003300                             PIC X(5).                                    
003400*                                 BUFFERPLATSNUMMER                       
003500*                                 LOCATION IN BUFFER                      
003600        05 RESP-BEART-LINE   PIC X(25).                                   
003700*                                 ARTIKELBENÄMNING                        
003800*                                 PART DESCRIPTION                        
003900*** END OF VILMAII-COPY LENGTH= 5210 BYTES                                
