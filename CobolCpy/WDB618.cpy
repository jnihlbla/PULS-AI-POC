000100 01  PROG-WDB618.                                                         
000200*                                 DC STYRREGISTER                         
000300*                                 PERIODPROGNOSER                         
000400*                                 FYSISK NYCKEL: KDPROGOI                 
000500     03 PROG-KDPROGOI        PIC X.                                       
000600*                                 TYP PROGNOS ORDERINGÅNG                 
000700*                                 TYPE FORECAST ORDER INTAKE              
000800     03 PROG-DC-PERIOD       OCCURS 12 TIMES.                             
000900*                                 PERIODTRENDER PER MÅNAD                 
001000*                                 TRENDS PER MONTH                        
001100        05 PROG-REFTREND-NORM                                             
001200                             PIC S9V9(2)         COMP-3.                  
001300*                                 TRENDFAKTOR NORMAL                      
001400*                                 TREND FACTOR NORMAL                     
001500        05 PROG-REFTREND-SVAG                                             
001600                             PIC S9V9(2)         COMP-3.                  
001700*                                 TRENDFAKTOR SVAG                        
001800*                                 TREND FACTOR WEAK                       
001900        05 PROG-REFTREND-STARK                                            
002000                             PIC S9V9(2)         COMP-3.                  
002100*                                 TRENDFAKTOR STARK                       
002200*                                 TREND FACTOR STRONG                     
002300*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  
