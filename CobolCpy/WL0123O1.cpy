000100 01  RESP-WL0123O1.                                                       
000200*                                 RESPONS FROM PGM WL0123                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDANSTNR-KEY    PIC Z(4)9.                                   
000600*                                 ANSTÄLLNINGSNUMMER                      
000700     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000800*                                 DISTRIKTNUMMER                          
000900     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
001000*                                 KUNDNUMMER                              
001100     03 RESP-IDORDNR-KEY     PIC Z(4)9.                                   
001200*                                 ORDERNUMMER                             
001300     03 RESP-IDKOLLI-KEY     PIC Z(4)9.                                   
001400*                                 KOLLINUMMER                             
001500     03 RESP-IDPRODNR-KEY    PIC Z(6)9.                                   
001600*                                 PRODUKTIONSNUMMER                       
001700     03 RESP-IDPLKLST        PIC Z(2)9.                                   
001800*                                 PLOCKLISTNUMMER                         
001900     03 RESP-FLAGGA          PIC X.                                       
002000*                                 ALLMÄN FLAGGA                           
002100     03 RESP-FLSVAR          PIC X.                                       
002200*                                 ALLMÄN SVARSFLAGGA                      
002300     03 RESP-TEPLATS         PIC X(38).                                   
002400     03 RESP-IDPLKLST-SPAR   PIC 9(3).                                    
002500*                                 PLOCKLISTNUMMER                         
002600     03 RESP-IDRADNR-SPAR    PIC 9(4).                                    
002700*                                 RADNUMMER                               
002800     03 RESP-KVRADER         PIC Z(4)9.                                   
002900*                                 ANTAL RADER                             
003000     03 RESP-RAD             OCCURS 500 TIMES.                            
003100        05 RESP-ADLAGOMR     PIC Z9.                                      
003200*                                 LAGEROMRÅDE                             
003300        05 RESP-IDRADNR      PIC Z(3)9.                                   
003400*                                 RADNUMMER                               
003500        05 RESP-FLNOLLJ      PIC X.                                       
003600*                                 UPPDATERAD AV NOLLJAGARE                
003700        05 RESP-KVORAPP      PIC Z(5)9.                                   
003800*                                 EJ-RAPPORTERAT-ANTAL                    
003900        05 RESP-IDMSG-ERROR-LINE                                          
004000                             PIC X(3).                                    
004100*                                 FELMEDDELANDE ID                        
004200*** END OF VILMAII-COPY LENGTH= 8089 BYTES                                
