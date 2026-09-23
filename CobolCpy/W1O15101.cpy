000100 01  MOD-W1O15101.                                                        
000200*                                 MOD-COPYTEXT PGM W10151                 
000300*                                 MARKET STRUCTURE                        
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-KDPRODSL-IN      PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-KDPRODSL-UT      PIC X(2).                                    
001100*                                 PRODUKTSLAG                             
001200     03 MOD-KDBASLM-ENTER    PIC X(6).                                    
001300*                                 BASLAGERMARKNAD                         
001400     03 MOD-KDBASLM-PF8      PIC X(6).                                    
001500*                                 BASLAGERMARKNAD                         
001600     03 MOD-RAD              OCCURS 36 TIMES.                             
001700        05 MOD-KDBASLM       PIC X(6).                                    
001800*                                 BASLAGERMARKNAD                         
001900        05 MOD-IDDISTR       PIC 9(4).                                    
002000*                                 DISTRIKTNUMMER                          
002100     03 MOD-KDCMD-IN-ATTR    PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 MOD-KDCMD-IN         PIC X.                                       
002400*                                 RAD-UPPDATERINGSKOMMANDO                
002500     03 MOD-KDBASLM-IN-ATTR  PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 MOD-KDBASLM-IN       PIC X(6).                                    
002800*                                 BASLAGERMARKNAD                         
002900     03 MOD-RAD              OCCURS 6 TIMES.                              
003000        05 MOD-IDDISTR-ATTR  PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-IDDISTR-IN    PIC 9(4).                                    
003300*                                 DISTRIKTNUMMER                          
003400     03 MOD-TEMFSINF         PIC X(61).                                   
003500*                                 INFORMATIONSMEDDELANDE                  
003600*** END COPY W1O15101C0  LENGTH=528                                       
