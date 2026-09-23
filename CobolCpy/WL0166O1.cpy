000100 01  RESP-WL0166O1-CTX.                                                   
000200*                                 RESPONS FROM PGM WL0166                 
000300*                                                                         
000400     03 RESP-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600     03 RESP-IDDISTR-KEY     PIC Z(3)9.                                   
000700*                                 DISTRIKTNUMMER                          
000800     03 RESP-IDKUNDNR-KEY    PIC Z(5)9.                                   
000900*                                 KUNDNUMMER                              
001000     03 RESP-IDORDNR-KEY     PIC Z(4)9.                                   
001100*                                 ORDERNUMMER                             
001200     03 RESP-FLANNULL        PIC X.                                       
001300*                                 ANNULLATION                             
001400     03 RESP-KDORDKL-UT      PIC 9.                                       
001500*                                 ORDERKLASS                              
001600     03 RESP-KDFRAKT-UT      PIC Z9.                                      
001700*                                 FRAKTSÄTT DC TILL KUND                  
001800     03 RESP-KVRADER         PIC Z(4)9.                                   
001900*                                 ANTAL RADER                             
002000     03 RESP-WL0166O1-001-GRP                                             
002100                             OCCURS 100 TIMES.                            
002200        05 RESP-IDARTNR-006  PIC Z(7)9.                                   
002300*                                 ARTIKELNUMMER                           
002400        05 RESP-KVBEART      PIC Z(5)9.                                   
002500*                                 BESTÄLLT ANTAL STYCKEN                  
002600        05 RESP-BERADREF     PIC X(10).                                   
002700*                                 KUNDENS RADREFERENS                     
002800        05 RESP-FLINVEST     PIC X.                                       
002900*                                 BYTES INVENTERINGSFLAGGA                
003000        05 RESP-IDMSG-ERROR-LINE                                          
003100                             PIC X(3).                                    
003200*                                 FELMEDDELANDE ID                        
003300*** END OF VILMAII-COPY LENGTH= 2826 BYTES                                
