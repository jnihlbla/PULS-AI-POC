000100 01  RESP-WL0158O1.                                                       
000200*                                 RESPONS FROM PGM WL0158                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDRTLOP-KEY     PIC 9(3).                                    
000600*                                 RETUR TERMINAL LÖPNUMMER                
000700     03 RESP-FLVISA-KEY      PIC X.                                       
000800*                                 ALLMÄN FLAGGA                           
000900     03 RESP-FLSNDDOK        PIC X.                                       
001000*                                 ALLMÄN FLAGGA                           
001100     03 RESP-IDDC-RET        PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 RESP-IDDC-RET1       PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 RESP-KVRADER-RET1    PIC Z(4)9.                                   
001600*                                 ANTAL RADER                             
001700     03 RESP-IDDC-RET2       PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 RESP-KVRADER-RET2    PIC Z(4)9.                                   
002000*                                 ANTAL RADER                             
002100     03 RESP-IDDC-TRANSIT    PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 RESP-KVRADER-TRANSIT PIC Z(4)9.                                   
002400*                                 ANTAL RADER                             
002500     03 RESP-IDDC-RETUR      PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 RESP-IDMOD-RADER     PIC X(8).                                    
002800*                                 IMS-MFS-MODNAME                         
002900     03 RESP-KVRADER         PIC Z(4)9.                                   
003000*                                 ANTAL RADER                             
003100     03 RESP-RADER           OCCURS 500 TIMES.                            
003200*                                                                         
003300        05 RESP-KDCMD        PIC X(4).                                    
003400        05 RESP-IDKOLLI      PIC Z(4)9.                                   
003500*                                 KOLLINUMMER                             
003600        05 RESP-FLFARLIG     PIC X.                                       
003700*                                 FARLIGT GODS-FLAGGA                     
003800        05 RESP-IDRT         PIC X(3).                                    
003900*                                 RETURTERMINAL                           
004000        05 RESP-IDRTLOP      PIC 9(3).                                    
004100*                                 RETUR TERMINAL LÖPNUMMER                
004200        05 RESP-IDDC         PIC X(2).                                    
004300*                                 IDENTIFIERARE LAGER                     
004400        05 RESP-IDMSG-ERROR-LINE                                          
004500                             PIC X(3).                                    
004600*                                 FELMEDDELANDE ID                        
004700*** END OF VILMAII-COPY LENGTH= 10545 BYTES                               
