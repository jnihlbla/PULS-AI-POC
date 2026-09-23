000100 01  MOD-W6O32401.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDARBTYP-IN      PIC X(8).                                    
000800*                                 TYP AV ARBETE                           
000900     03 MOD-KDARBTYP-UT      PIC X(8).                                    
001000*                                 TYP AV ARBETE                           
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-RAD              OCCURS 5 TIMES.                              
001600*                                 GRUPP MED RADER                         
001700        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900        05 MOD-KDCMDVAL-IN   PIC X(3).                                    
002000*                                 GENERELL KOMMANDOKOD                    
002100        05 MOD-BEANST-GODK   PIC X(25).                                   
002200*                                 GODKÄNNARES NAMN                        
002300        05 MOD-IDUSER-GODK   PIC X(8).                                    
002400*                                 ANVÄNDAR-ID GODKÄNNARE                  
002500        05 MOD-KVANTAL       PIC Z(7).                                    
002600*                                 ANTAL                                   
002700        05 MOD-IDUSER-OREG   PIC X(8).                                    
002800*                                 ANSVARIGT USERID ORDERREG.              
002900        05 MOD-TIAAMMDD      PIC 9(6).                                    
003000*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
003100        05 MOD-IDUSER-PRI    PIC X(8).                                    
003200*                                 ANVÄNDAR-ID PRIMÄRKONTROLL              
003300        05 MOD-IDMAIL        PIC X(60).                                   
003400*                                 MAIL ADRESS                             
003500     03 MOD-BEANST-GODK-ATTR PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-BEANST-GODK-IN   PIC X(25).                                   
003800*                                 GODKÄNNARES NAMN                        
003900     03 MOD-IDUSER-GODK-ATTR PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-IDUSER-GODK-IN   PIC X(8).                                    
004200*                                 ANVÄNDAR-ID GODKÄNNARE                  
004300     03 MOD-KVANTAL-ATTR     PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-KVANTAL-IN       PIC Z(7).                                    
004600*                                 ANTAL                                   
004700     03 MOD-IDUSER-PRI-ATTR  PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-IDUSER-PRI-IN    PIC X(8).                                    
005000*                                 ANVÄNDAR-ID PRIMÄRKONTROLL              
005100     03 MOD-IDMAIL-ATTR      PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-IDMAIL-IN        PIC X(60).                                   
005400*                                 MAIL ADRESS                             
005500     03 MOD-TEMFSINF         PIC X(55).                                   
005600*                                 INFORMATIONSMEDDELANDE                  
005700*** END OF VILMAII-COPY LENGTH= 872 BYTES                                 
