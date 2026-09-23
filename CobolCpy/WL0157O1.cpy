000100 01  RESP-WL0157O1.                                                       
000200*                                 RESPONS FROM PGM WL0157                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDKOLLI-KEY     PIC Z(5).                                    
000600*                                 KOLLINUMMER                             
000700     03 RESP-FLVISA-KEY      PIC X.                                       
000800*                                 ALLMÄN FLAGGA                           
000900     03 RESP-IDDC-RET1       PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 RESP-KVRADER-RET1    PIC Z(4)9.                                   
001200*                                 ANTAL RADER                             
001300     03 RESP-IDDC-RET2       PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 RESP-KVRADER-RET2    PIC Z(4)9.                                   
001600*                                 ANTAL RADER                             
001700     03 RESP-IDDC-RETUR      PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 RESP-IDKOLLI-FOM     PIC Z(4)9.                                   
002000*                                 KOLLINUMMER                             
002100     03 RESP-IDKOLLI-TOM     PIC Z(4)9.                                   
002200*                                 KOLLINUMMER                             
002300     03 RESP-FLFARLIG-KOLLI  PIC X.                                       
002400*                                 FARLIGT GODS-FLAGGA                     
002500     03 RESP-KVKOLLI         PIC Z(3)9.                                   
002600*                                 ANTAL KOLLI                             
002700     03 RESP-IDDISTR-IN      PIC Z(3)9.                                   
002800*                                 DISTRIKTNUMMER                          
002900     03 RESP-IDKUNDNR-IN     PIC Z(5)9.                                   
003000*                                 KUNDNUMMER                              
003100     03 RESP-IDRAPPNR-IN     PIC Z(6)9.                                   
003200*                                 RAPPORT NUMMER                          
003300     03 RESP-KVRADER         PIC Z(4)9.                                   
003400*                                 ANTAL RADER                             
003500     03 RESP-RADER           OCCURS 500 TIMES.                            
003600*                                                                         
003700        05 RESP-KDCMD        PIC X(4).                                    
003800        05 RESP-IDDISTR      PIC Z(3)9.                                   
003900*                                 DISTRIKTNUMMER                          
004000        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
004100*                                 KUNDNUMMER                              
004200        05 RESP-IDRAPPNR     PIC Z(6)9.                                   
004300*                                 RAPPORT NUMMER                          
004400        05 RESP-KVKOLLI-AAF  PIC Z(3)9.                                   
004500*                                 ANTAL KOLLI                             
004600        05 RESP-FLFARLIG     PIC X.                                       
004700*                                 FARLIGT GODS-FLAGGA                     
004800        05 RESP-IDMSG-ERROR-LINE                                          
004900                             PIC X(3).                                    
005000*                                 FELMEDDELANDE ID                        
005100*** END OF VILMAII-COPY LENGTH= 14561 BYTES                               
