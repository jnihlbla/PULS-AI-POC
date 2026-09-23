000100 01  REQU-WL0147I1.                                                       
000200*                                 REQUEST TO PGM WL0147                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-IDRT-KEY        PIC X(3).                                    
000600*                                 RETURTERMINAL                           
000700     03 REQU-IDRTLOP-KEY     PIC 9(3).                                    
000800*                                 RETUR TERMINAL LÖPNUMMER                
000900     03 REQU-IDKOLLI-KEY     PIC 9(5).                                    
001000*                                 KOLLINUMMER                             
001100     03 REQU-KVRADER         PIC 9(5).                                    
001200*                                 ANTAL RADER                             
001300     03 REQU-RAD             OCCURS 500 TIMES.                            
001400*                                 RAD                                     
001500        05 REQU-KDCMD        PIC X(4).                                    
001600        05 REQU-ADINLOMR-UPD PIC X(4).                                    
001700*                                 INLEVERANSOMRÅDE                        
001800        05 REQU-ADINLOMR     PIC X(4).                                    
001900*                                 INLEVERANSOMRÅDE                        
002000        05 REQU-IDKOLLI      PIC 9(5).                                    
002100*                                 KOLLINUMMER                             
002200*** END OF VILMAII-COPY LENGTH= 8518 BYTES                                
