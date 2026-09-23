000100 01  MOD-W4O67101.                                                        
000200*                                 MOD-COPYTEXT FÖR W4067100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 FELMEDDELANDEFÄLT                       
000700     03 MOD-BEROUTE-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-BEROUTE-UT       PIC X(25).                                   
001000*                                 FÄRDVÄG, DESTINATION                    
001100     03 MOD-IDTRANSP-NAMN-IN PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDTRANSP-NAMN-UT PIC X(15).                                   
001400*                                 TRANSPORTMEDEL NAMN                     
001500     03 MOD-BEROUTE-SPA      PIC X(25).                                   
001600*                                 FÄRDVÄG, DESTINATION                    
001700     03 MOD-IDTRANSP-NAMN-SPA                                             
001800                             PIC X(15).                                   
001900*                                 TRANSPORTMEDEL NAMN                     
002000     03 MOD-W4O67101-001     OCCURS 14 TIMES.                             
002100*                                                                         
002200        05 MOD-KDCMD-ATTR    PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400        05 MOD-KDCMD         PIC X.                                       
002500*                                 RAD-UPPDATERINGSKOMMANDO                
002600        05 MOD-BEROUTE       PIC X(25).                                   
002700*                                 FÄRDVÄG, DESTINATION                    
002800        05 MOD-IDTRANSP-NAMN PIC X(15).                                   
002900*                                 TRANSPORTMEDEL NAMN                     
003000     03 MOD-TEMFSINF         PIC X(61).                                   
003100*                                 INFORMATIONSMEDDELANDE                  
003200*** END COPY W4O67101C0  LENGTH=791                                       
