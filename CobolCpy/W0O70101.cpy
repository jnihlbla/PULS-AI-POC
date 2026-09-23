000100 01  MOD-W0O70101.                                                        
000200*                                 COPYTEXT FÖR MOD W0O70101               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 FELMEDDELANDEFÄLT                       
000700     03 MOD-IDRUTIN-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDRUTIN-UT       PIC X(8).                                    
001000*                                 RUTINNAMN (GRUPP AV JOBB)               
001100     03 MOD-IDJOB-IN         PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDJOB-UT         PIC X(8).                                    
001400*                                 JOBBNAMN                                
001500     03 MOD-IDOWNER-SKIP     PIC X(8).                                    
001600*                                 ÄGAREIDENTITET I RACF                   
001700     03 MOD-IDUSER-SKIP      PIC X(8).                                    
001800*                                 ANVÄNDARIDENTITET I RACF                
001900     03 MOD-IDOWNER-ATTR     PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-IDOWNER          PIC X(2).                                    
002200*                                 MFS BEHANDLING AV INPUTFÄLT             
002300     03 MOD-IDUSER-ATTR      PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-IDUSER           PIC X(2).                                    
002600*                                 MFS BEHANDLING AV INPUTFÄLT             
002700     03 MOD-LINES            OCCURS 24 TIMES                              
002800                             INDEXED MOD-IX-LINE.                         
002900        05 MOD-IDOWNER-UT    PIC X(8).                                    
003000*                                 ÄGAREIDENTITET I RACF                   
003100        05 MOD-IDUSER-UT     PIC X(8).                                    
003200*                                 ANVÄNDARIDENTITET I RACF                
003300        05 MOD-TIREGDAT      PIC 9(6).                                    
003400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003500     03 MOD-TEMFSINF         PIC X(61).                                   
003600*                                 INFORMATIONSMEDDELANDE                  
003700*** END COPY W0O70101C0  LENGTH=677                                       
