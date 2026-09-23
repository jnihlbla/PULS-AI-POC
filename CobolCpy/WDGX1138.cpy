000100 01  1138-WDGX1138.                                                       
000200*                                 NYA ARTIKLAR FRÅN PV, LV                
000300*                                 FUNKTIONSGRUPP TABELL                   
000400*                                 ANSKAFFARE                              
000500*                                 FYSISK NYCKEL: WDGXKEY                  
000600*                                 (IDFKNGRP + LOWVALUE)                   
000700     03 1138-IDFKNGRP-FOM    PIC S9(5)           COMP-3.                  
000800*                                 FUNKTIONSGRUPP                          
000900*                                 FUNCTION GROUP                          
001000     03 1138-LOWVALUE        PIC X(2).                                    
001100     03 1138-IDFKNGRP-TOM    PIC S9(5)           COMP-3.                  
001200*                                 FUNKTIONSGRUPP                          
001300*                                 FUNCTION GROUP                          
001400     03 1138-IDANSK          PIC S9(3)           COMP-3.                  
001500*                                 ANSKAFFARNUMMER                         
001600*                                 PROCURER NO.                            
001700     03 1138-IDUSER          PIC X(8).                                    
001800*                                 ANVÄNDARIDENTITET I RACF                
001900*                                 USER RACF-IDENTITY                      
002000     03 FILLER               PIC X(7).                                    
002100*** END COPY WDGX1138C0  LENGTH=25                                        
