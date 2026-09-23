000100 01  W2I13901.                                                            
000200*                                 COPYTEXT F÷R MID W2I13901               
000300     03 IDARTNR-IN           PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 IDARTNR-UT           PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 KDBEHX-PLAN-IN       PIC X.                                       
000800*                                 BEHANDLINGSKOD-X                        
000900     03 KDBEHX-PLAN-UT       PIC X.                                       
001000*                                 BEHANDLINGSKOD-X                        
001100     03 IDLEVNR-IN           PIC X(5).                                    
001200*                                 LEVERANT÷RNUMMER                        
001300     03 IDLEVNR-UT           PIC X(5).                                    
001400*                                 LEVERANT÷RNUMMER                        
001500     03 PERIOD-IN            PIC X(4).                                    
001600*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
001700*                                 12 PER ≈R                               
001800     03 PERIOD-UT            PIC X(4).                                    
001900*                                 ≈R - PLANERINGSPERIOD (≈≈PP)            
002000*                                 12 PER ≈R                               
002100     03 INPUT.                                                            
002200*                                                                         
002300        05 PERIODER          OCCURS 2 TIMES.                              
002400*                                                                         
002500           07 VECKOR         OCCURS 5 TIMES.                              
002600*                                                                         
002700              09 KVAVROP-DAG OCCURS 5 TIMES                               
002800                             PIC 9(6).                                    
002900*** END OF VILMAII-COPY LENGTH= 338 BYTES                                 
