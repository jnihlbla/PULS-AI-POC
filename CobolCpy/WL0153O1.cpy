000100 01  RESP-WL0153O1.                                                       
000200*                                 RESPONS FROM PGM WL0153.                
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDILIST-KEY     PIC Z(4)9.                                   
000600*                                 INLÄGGNINGSLISTEIDENTITET               
000700     03 RESP-FLKLAR          PIC X.                                       
000800*                                 ALLMÄN FLAGGA                           
000900     03 RESP-IDANSTNR        PIC Z(4)9.                                   
001000*                                 ANSTÄLLNINGSNUMMER                      
001100     03 RESP-FLMAK           PIC X.                                       
001200*                                 ALLMÄN FLAGGA                           
001300     03 RESP-FLSKRIV         PIC X.                                       
001400*                                 JA = ÅTERSTART AV BEGÄRD LISTA          
001500     03 RESP-KVRADER         PIC 9(5).                                    
001600*                                 ANTAL RADER                             
001700     03 RESP-RADER           OCCURS 500 TIMES.                            
001800*                                                                         
001900        05 RESP-FLCMD        PIC X.                                       
002000        05 RESP-KVANTAL      PIC Z(5)9.                                   
002100*                                 ANTAL                                   
002200        05 RESP-IDARTNR      PIC Z(7)9.                                   
002300*                                 ARTIKELNUMMER                           
002400        05 RESP-BEART        PIC X(25).                                   
002500*                                 ARTIKELBENÄMNING                        
002600        05 RESP-KVANTAL-KVAR PIC Z(5).                                    
002700*                                 ANTAL                                   
002800        05 RESP-KDANMORS     PIC X(2).                                    
002900*                                 ORSAK TILL LEVERANSANMÄRKNING           
003000        05 RESP-ADLAGOMR     PIC 9(2).                                    
003100*                                 LAGEROMRÅDE                             
003200        05 RESP-ADGANG       PIC 9(2).                                    
003300*                                 GÅNG                                    
003400        05 RESP-ADPLATS      PIC 9(5).                                    
003500*                                 LAGERPLATSNUMMER                        
003600        05 RESP-IDRADNR      PIC Z(3)9.                                   
003700*                                 RADNUMMER                               
003800        05 RESP-IDMSG-ERROR-LINE                                          
003900                             PIC X(3).                                    
004000*                                 FELMEDDELANDE ID                        
004100*** END OF VILMAII-COPY LENGTH= 31520 BYTES                               
