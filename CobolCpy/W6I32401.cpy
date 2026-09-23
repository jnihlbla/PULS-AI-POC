000100 01  MID-W6I32401.                                                        
000200*                                                                         
000300     03 MID-KDARBTYP-IN      PIC X(8).                                    
000400*                                 TYP AV ARBETE                           
000500     03 MID-KDARBTYP-UT      PIC X(8).                                    
000600*                                 TYP AV ARBETE                           
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-RAD              OCCURS 5 TIMES.                              
001200*                                                                         
001300        05 MID-KDCMDVAL      PIC X(3).                                    
001400*                                 GENERELL KOMMANDOKOD                    
001500        05 MID-IDUSER-GODK   PIC X(8).                                    
001600*                                 ANVÄNDAR-ID GODKÄNNARE                  
001700     03 MID-INPUT.                                                        
001800*                                                                         
001900        05 MID-BEANST-GODK   PIC X(25).                                   
002000*                                 GODKÄNNARES NAMN                        
002100        05 MID-IDUSER-GODK-IN                                             
002200                             PIC X(8).                                    
002300*                                 ANVÄNDAR-ID GODKÄNNARE                  
002400        05 MID-KVANTAL       PIC X(7).                                    
002500*                                 ANTAL                                   
002600        05 MID-IDUSER-PRI    PIC X(8).                                    
002700*                                 ANVÄNDAR-ID PRIMÄRKONTROLL              
002800        05 MID-IDMAIL        PIC X(60).                                   
002900*                                 MAIL ADRESS                             
003000*** END OF VILMAII-COPY LENGTH= 183 BYTES                                 
