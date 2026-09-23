000100 01  MID-W1I15101.                                                        
000200*                                 MID-COPYTEXT PGM W10151                 
000300*                                 MARKET STRUCTURE                        
000400     03 MID-KDPRODSL-IN      PIC X(2).                                    
000500*                                 PRODUKTSLAG                             
000600     03 MID-KDPRODSL-UT      PIC X(2).                                    
000700*                                 PRODUKTSLAG                             
000800     03 MID-KDBASLM-ENTER    PIC X(6).                                    
000900*                                 BASLAGERMARKNAD                         
001000     03 MID-KDBASLM-PF8      PIC X(6).                                    
001100*                                 BASLAGERMARKNAD                         
001200     03 MID-KDCMD            PIC X.                                       
001300      88 MID-KDCMD-INGENTING VALUE ' '.                                   
001400      88 MID-KDCMD-DELETE    VALUE 'D'                                    
001500                             'B'.                                         
001600      88 MID-KDCMD-REPLACE   VALUE 'R'                                    
001700                             'Ä'.                                         
001800      88 MID-KDCMD-INSERT    VALUE 'I'                                    
001900                             'N'.                                         
002000*                                 RAD-UPPDATERINGSKOMMANDO                
002100     03 MID-KDBASLM          PIC X(6).                                    
002200*                                 BASLAGERMARKNAD                         
002300     03 MID-RAD              OCCURS 6 TIMES.                              
002400        05 MID-IDDISTR       PIC 9(4).                                    
002500*                                 DISTRIKTNUMMER                          
002600*** END COPY W1I15101C0  LENGTH=47                                        
