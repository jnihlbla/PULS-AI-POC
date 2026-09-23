000100 01  REQU-WL0185I1.                                                       
000200*                                 REQUEST TO PGM WL0185                   
000300     03 REQU-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 REQU-TIRFSDAT-KEY    PIC 9(6).                                    
000600*                                 KLART F÷R TRANSPORT ≈≈MMDD              
000700     03 REQU-IDPRC-FR-IN-KEY.                                             
000800*                                 PRODUKTIONSKANAL                        
000900        05 REQU-IDPRCBAS     PIC X(3).                                    
001000*                                 PRC-BAS                                 
001100        05 REQU-IDPRCVAR     PIC X.                                       
001200*                                 PRC-VARIANT                             
001300     03 REQU-IDPRC-TO-IN-KEY.                                             
001400*                                 PRODUKTIONSKANAL                        
001500        05 REQU-IDPRCBAS     PIC X(3).                                    
001600*                                 PRC-BAS                                 
001700        05 REQU-IDPRCVAR     PIC X.                                       
001800*                                 PRC-VARIANT                             
001900*** END OF VILMAII-COPY LENGTH= 16 BYTES                                  
