000100 01  MOD-W0O70601.                                                        
000200*                                 COPYTEXT FÖR MOD W0O70601               
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
001500     03 MOD-FLKLAR           PIC X.                                       
001600*                                 AVSLUTNINGSMARKERING                    
001700     03 MOD-IDJCLRAD-SKIP    PIC 9(5).                                    
001800*                                 RADNUMMER PÅ JCL                        
001900     03 MOD-LINES            OCCURS 15 TIMES                              
002000                             INDEXED MOD-IX-LINE.                         
002100        05 MOD-IDJCLRAD      PIC 9(5).                                    
002200*                                 RADNUMMER PÅ JCL                        
002300        05 MOD-KDCMD         PIC X.                                       
002400*                                 RAD-UPPDATERINGSKOMMANDO                
002500        05 MOD-TEJCL         PIC X(71).                                   
002600*                                 JCL-KORT                                
002700     03 MOD-TEMFSINF         PIC X(61).                                   
002800*                                 INFORMATIONSMEDDELANDE                  
002900*** END COPY W0O70601C0  LENGTH=1286                                      
