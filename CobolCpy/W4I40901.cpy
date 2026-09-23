000100 01  MID-W4I40901.                                                        
000200*                                 COPYTEXT FÖR MID W4I40901               
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-UT          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-KDPROGOI-IN      PIC X.                                       
000800*                                 TYP PROGNOS ORDERINGÅNG                 
000900     03 MID-KDPROGOI-UT      PIC X(23).                                   
001000     03 MID-IDDC-COPY-IN     PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MID-KDPROGOI-COPY-IN PIC X.                                       
001300*                                 TYP PROGNOS ORDERINGÅNG                 
001400     03 MID-DEFAULT-IN       PIC X.                                       
001500     03 MID-INPUT.                                                        
001600        05 MID-RAD           OCCURS 12 TIMES.                             
001700           07 MID-REFTREND-NORM-IN                                        
001800                             PIC X(3).                                    
001900*                                 TRENDFAKTOR NORMAL                      
002000           07 MID-REFTREND-SVAG-IN                                        
002100                             PIC X(3).                                    
002200*                                 TRENDFAKTOR SVAG                        
002300           07 MID-REFTREND-STARK-IN                                       
002400                             PIC X(3).                                    
002500*                                 TRENDFAKTOR STARK                       
002600*** END OF VILMAII-COPY LENGTH= 140 BYTES                                 
