000100 01  MOD-W3O21301.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W3O213                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-MOD-BYTE         PIC X.                                       
000900     03 MOD-UTRAD            OCCURS 10 TIMES.                             
001000        05 MOD-UTRAD-IDSKURVA                                             
001100                             PIC 9(2).                                    
001200*                                 SÄSONGSKURVA                            
001300        05 MOD-UTRAD-PUNKTER OCCURS 12 TIMES.                             
001400           07 MOD-UTRAD-REFSGSIX                                          
001500                             PIC Z9.9.                                    
001600*                                 SÄSONGSINDEX FÖRSÄLJNING (%)            
001700     03 MOD-INRAD-IDSKURVA-ATTR                                           
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-INRAD-IDSKURVA   PIC 9(2).                                    
002100*                                 SÄSONGSKURVA                            
002200     03 MOD-INRAD-PUNKTER    OCCURS 12 TIMES.                             
002300        05 MOD-INRAD-REFSGSIX-ATTR                                        
002400                             PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600        05 MOD-INRAD-REFSGSIX                                             
002700                             PIC X(2).                                    
002800*                                 MFS BEHANDLING AV INPUTFÄLT             
002900     03 MOD-TEMFSINF         PIC X(61).                                   
003000*                                 INFORMATIONSMEDDELANDE                  
003100*** END COPY W3O21301C0  LENGTH=658                                       
