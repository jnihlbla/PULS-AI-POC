000100 01  4314-WDGX4314.                                                       
000200*                                 ORDER UNDER ARBETE                      
000300*                                 ORDERVIS PACKNINGSRAPPORTERING          
000400*                                 DELNINGSSEGMENT                         
000500*                                 NYCKEL: WDGXKEY                         
000600*                                         (IDRADNR, LOWVALUE)             
000700     03 4314-IDRADNR         PIC S9(5)           COMP-3.                  
000800*                                 RADNUMMER PÅ ORDER                      
000900     03 4314-LOWVALUE        PIC X(7).                                    
001000     03 4314-KVLEVART        PIC S9(7)           COMP-3.                  
001100*                                 LEVERERAT ANTAL ARTIKLAR                
001200     03 4314-KDBEHAND-RAD    PIC S9              COMP-3.                  
001300*                                 BEHANDLINGSKOD                          
001400*                                  0 = EJ AKTUELL                         
001500*                                  1 = SKALL BEHANDLAS                    
001600*                                  2 = FÄRDIG BEHANDLAD                   
001700*** END COPY WDGX4314C0  LENGTH=15                                        
