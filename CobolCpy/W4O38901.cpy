000100 01  MOD-W4O38901.                                                        
000200*                                 COPYTEXT FOR MOD W4O38901               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDUSER-IN        PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDUSER-UT        PIC X(8).                                    
001000*                                 ANVÄNDARENS SÄKERHETS ID                
001100     03 MOD-IDSHIFT-IN       PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDSHIFT-UT       PIC X.                                       
001400*                                 SHIFT IDENTITET                         
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-IDUSER-ENTER     PIC X(8).                                    
002000*                                 ANVÄNDARENS SÄKERHETS ID                
002100     03 MOD-IDUSER-NEXT      PIC X(8).                                    
002200*                                 ANVÄNDARENS SÄKERHETS ID                
002300     03 MOD-RAD              OCCURS 65 TIMES.                             
002400*                                 LINES                                   
002500        05 MOD-IDUSER-RAD-ATTR                                            
002600                             PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800        05 MOD-IDUSER-RAD    PIC X(8).                                    
002900*                                 ANVÄNDARENS SÄKERHETS ID                
003000        05 MOD-IDSHIFT-RAD-ATTR                                           
003100                             PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-IDSHIFT-RAD   PIC X.                                       
003400*                                 SHIFT IDENTITET                         
003500     03 MOD-INDATA.                                                       
003600*                                 INPUT                                   
003700        05 MOD-IDUSER-INPUT-ATTR                                          
003800                             PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-IDUSER-INPUT  PIC X(2).                                    
004100*                                 MFS BEHANDLING AV INPUTFÄLT             
004200        05 MOD-IDSHIFT-INPUT-ATTR                                         
004300                             PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 MOD-IDSHIFT-INPUT PIC X(2).                                    
004600*                                 MFS BEHANDLING AV INPUTFÄLT             
004700     03 MOD-TEMFSINF         PIC X(55).                                   
004800*                                 INFORMATIONSMEDDELANDE                  
