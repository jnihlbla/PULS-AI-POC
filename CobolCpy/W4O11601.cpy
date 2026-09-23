000100 01  MOD-W4O11601-CTX.                                                    
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W4O116                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDKRFELI         PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDKRFELU         PIC X(2).                                    
001100*                                 FELKOD FÖR KONTROLLRAPPORT              
001200     03 MOD-IDKRFEL-NX       PIC X(2).                                    
001300*                                 FELKOD FÖR KONTROLLRAPPORT              
001400     03 MOD-IDKRFEL-EN       PIC X(2).                                    
001500*                                 FELKOD FÖR KONTROLLRAPPORT              
001600     03 MOD-W4O11601-001-GRP OCCURS 10 TIMES.                             
001700        05 MOD-IDKRFEL-UT    PIC X(2).                                    
001800*                                 FELKOD FÖR KONTROLLRAPPORT              
001900        05 MOD-BEKRFEL-UT    PIC X(63).                                   
002000     03 MOD-IDKRFEL-IN-ATTR  PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-IDKRFEL-IN       PIC X(2).                                    
002300*                                 MFS BEHANDLING AV INPUTFÄLT             
002400     03 MOD-BEKRFEL-IN-ATTR  PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600     03 MOD-BEKRFEL-IN       PIC X(2).                                    
002700*                                 MFS BEHANDLING AV INPUTFÄLT             
002800     03 MOD-KDCMD-IN-ATTR    PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 MOD-KDCMD-IN         PIC X(2).                                    
003100*                                 MFS BEHANDLING AV INPUTFÄLT             
003200     03 MOD-TEMFSINF         PIC X(55).                                   
003300*                                 INFORMATIONSMEDDELANDE                  
003400*** END OF VILMAII-COPY LENGTH= 769 BYTES                                 
