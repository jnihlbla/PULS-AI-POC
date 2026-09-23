000100 01  MID-W0I81401.                                                        
000200*                                 MID-COPYTEXT FÖR W0081400               
000300     03 MID-IDRFTAB-IN       PIC X(3).                                    
000400*                                 RANSONERINGSFAKTORTABELL                
000500     03 MID-IDRFTAB-UT       PIC X(3).                                    
000600*                                 RANSONERINGSFAKTORTABELL                
000700     03 MID-BERFTAB          PIC X(25).                                   
000800*                                 RANSONERINGSFAKTORTABELL                
000900     03 MID-TABELL.                                                       
001000        05 MID-KLASS         OCCURS 5 TIMES.                              
001100           07 MID-INTERVALL  OCCURS 5 TIMES.                              
001200              09 MID-RERF-FOM                                             
001300                             PIC X(4).                                    
001400*                                 RANSONERINGSFAKTOR                      
001500              09 MID-RERF-TOM                                             
001600                             PIC X(4).                                    
001700*                                 RANSONERINGSFAKTOR                      
001800              09 MID-RERF-NY PIC X(5).                                    
001900*                                 RANSONERINGSFAKTOR                      
002000*** END COPY W0I81401C0  LENGTH=356                                       
