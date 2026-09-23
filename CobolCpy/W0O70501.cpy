000100 01  MOD-W0O70501.                                                        
000200*                                 COPYTEXT FÖR MOD W0O70501               
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
001700     03 MOD-IDJCLRAD-IN-ATTR PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-IDJCLRAD-IN      PIC 9(5).                                    
002000*                                 RADNUMMER PÅ JCL                        
002100     03 MOD-LINES            OCCURS 14 TIMES                              
002200                             INDEXED MOD-IX-LINE.                         
002300        05 MOD-IDJCLRAD-ATTR PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500        05 MOD-IDJCLRAD      PIC 9(5).                                    
002600*                                 RADNUMMER PÅ JCL                        
002700        05 MOD-TEJCL-ATTR    PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900        05 MOD-TEJCL         PIC X(71).                                   
003000*                                 JCL-KORT                                
003100     03 MOD-TEMFSINF         PIC X(61).                                   
003200*                                 INFORMATIONSMEDDELANDE                  
003300*** END COPY W0O70501C0  LENGTH=1253                                      
