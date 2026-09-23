000100 01  4456-WDGX4456.                                                       
000200*                                 BESKRIVNING AV                          
000300*                                 RANSONERINGS TABELL                     
000400*                                 INDEX 1- 4 MOTSVARAR                    
000500*                                 RESPEKTIVE KLASS                        
000600*                                 INDEX 5 ÄR TPO                          
000700*                                 FYSISK NYCKEL                           
000800*                                 IDRFTAB + LOW-VALUE                     
000900     03 4456-IDRFTAB         PIC X(3).                                    
001000*                                 RANSONERINGSFAKTORTABELL                
001100*                                 TABLE WITH RATIONING FACTORS            
001200     03 4456-LOW-VALUE       PIC X(7).                                    
001300     03 4456-BERFTAB         PIC X(25).                                   
001400*                                 RANSONERINGSFAKTORTABELL                
001500*                                 TABLE WITH RATIONING FACTORS            
001600     03 4456-KLASS           OCCURS 5 TIMES.                              
001700        05 4456-INTERVALL    OCCURS 5 TIMES.                              
001800           07 4456-RERF-FOM  PIC S9V9(4)         COMP-3.                  
001900*                                 RANSONERINGSFAKTOR                      
002000*                                 RATIONING FACTOR                        
002100           07 4456-RERF-TOM  PIC S9V9(4)         COMP-3.                  
002200*                                 RANSONERINGSFAKTOR                      
002300*                                 RATIONING FACTOR                        
002400           07 4456-RERF-NY   PIC S9V9(4)         COMP-3.                  
002500*                                 RANSONERINGSFAKTOR                      
002600*                                 RATIONING FACTOR                        
002700     03 4456-FILLER          PIC X(10).                                   
002800*** END COPY WDGX4456C0  LENGTH=270                                       
