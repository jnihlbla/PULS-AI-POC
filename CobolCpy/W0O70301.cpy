000100 01  MOD-W0O70301.                                                        
000200*                                 COPYTEXT FÖR MOD W0O70301               
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
001500     03 MOD-IDRUTIN-SKIP     PIC X(8).                                    
001600*                                 RUTINNAMN (GRUPP AV JOBB)               
001700     03 MOD-LINES            OCCURS 13 TIMES                              
001800                             INDEXED MOD-IX-LINE.                         
001900        05 MOD-IDRUTIN       PIC X(8).                                    
002000*                                 RUTINNAMN (GRUPP AV JOBB)               
002100        05 MOD-TIREGDAT      PIC 9(6).                                    
002200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002300        05 MOD-TIUPPDAT      PIC 9(6).                                    
002400*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002500        05 MOD-BERUTIN       PIC X(25).                                   
002600*                                 RUTIN BESKRIVNING                       
002700     03 MOD-TEMFSINF         PIC X(61).                                   
002800*                                 INFORMATIONSMEDDELANDE                  
002900*** END COPY W0O70301C0  LENGTH=718                                       
