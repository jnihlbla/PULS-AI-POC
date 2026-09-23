000100 01  RESP-WL0167O1.                                                       
000200*                                 RESPONS FROM PGM WL0167                 
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
001600     03 RESP-KVRADER         PIC Z(4)9.                                   
001700*                                 ANTAL RADER                             
001800     03 RESP-RAD             OCCURS 500 TIMES.                            
001900        05 RESP-KDORDBEK     PIC 9(2).                                    
002000*                                 ORDERBEKRÄFTELSEKOD                     
002100        05 RESP-KDBEHX       PIC X.                                       
002200*                                 BEHANDLINGSKOD-X                        
002300        05 RESP-IDARTNR.                                                  
002400           07 RESP-IDARTNR-1--9                                           
002500                             PIC Z(7)9.                                   
002600*                                 ARTIKELNUMMER                           
002700           07 RESP-REKSIFFR  PIC 9.                                       
002800*                                 KONTROLLSIFFRA                          
002900        05 RESP-BEART        PIC X(15).                                   
003000*                                 ARTIKELBENÄMNING      BEART-002         
003100        05 RESP-IDDC-RAD     PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300        05 RESP-KVANTAL      PIC Z(5)9.                                   
003400*                                 ANTAL                                   
003500        05 RESP-KEYS         PIC X(6).                                    
003600*** END OF VILMAII-COPY LENGTH= 20524 BYTES                               
