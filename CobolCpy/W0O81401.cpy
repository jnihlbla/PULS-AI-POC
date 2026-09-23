000100 01  MOD-W0O81401.                                                        
000200*                                 MOD-COPYTEXT FÖR W0081400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDRFTAB-IN       PIC X(3).                                    
000800*                                 RANSONERINGSFAKTORTABELL                
000900     03 MOD-IDRFTAB-UT       PIC X(3).                                    
001000*                                 RANSONERINGSFAKTORTABELL                
001100     03 MOD-BERFTAB-ATTR     PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-BERFTAB          PIC X(25).                                   
001400*                                 RANSONERINGSFAKTORTABELL                
001500     03 MOD-KLASS            OCCURS 5 TIMES.                              
001600        05 MOD-INTERVALL     OCCURS 5 TIMES.                              
001700           07 MOD-RERF-FOM-ATTR                                           
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000           07 MOD-RERF-FOM   PIC 9.9(2).                                  
002100*                                 RANSONERINGSFAKTOR                      
002200           07 MOD-RERF-TOM-ATTR                                           
002300                             PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500           07 MOD-RERF-TOM   PIC 9.9(2).                                  
002600*                                 RANSONERINGSFAKTOR                      
002700           07 MOD-RERF-NY-ATTR                                            
002800                             PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000           07 MOD-RERF-NY    PIC 9.9(3).                                  
003100*                                 RANSONERINGSFAKTOR                      
003200     03 MOD-TEMFSINF         PIC X(61).                                   
003300*                                 INFORMATIONSMEDDELANDE                  
003400*** END COPY W0O81401C0  LENGTH=613                                       
