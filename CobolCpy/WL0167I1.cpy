000100 01  REQU-WL0167I1.                                                       
000200*                                 REQUEST TO PGM WL0167                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 REQU-IDORDNR-KEY     PIC 9(5).                                    
001000*                                 ORDERNUMMER                             
001100     03 REQU-KDORDKL-UT      PIC 9.                                       
001200*                                 ORDERKLASS                              
001300     03 REQU-FLANNULL        PIC X.                                       
001400*                                 ANNULLATION                             
001500     03 REQU-KVRADER         PIC 9(5).                                    
001600*                                 ANTAL RADER                             
001700     03 REQU-RAD             OCCURS 500 TIMES.                            
001800        05 REQU-KDORDBEK     PIC 9(2).                                    
001900*                                 ORDERBEKRÄFTELSEKOD                     
002000        05 REQU-IDARTNR.                                                  
002100           07 REQU-IDARTNR-1--9                                           
002200                             PIC 9(9).                                    
002300*                                 ARTIKELNUMMER                           
002400           07 REQU-REKSIFFR  PIC 9.                                       
002500*                                 KONTROLLSIFFRA                          
002600        05 REQU-IDDC-RAD     PIC X(2).                                    
002700*                                 IDENTIFIERARE LAGER                     
002800        05 REQU-KEYS         PIC X(6).                                    
002900*** END OF VILMAII-COPY LENGTH= 10024 BYTES                               
