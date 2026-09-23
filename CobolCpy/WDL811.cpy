000100 01  AAR-WDL811.                                                          
000200*                                 ORDERINGÅNG-STATISTIK                   
000300*                                 ÅR = 53 VECKOR                          
000400*                                 FYSISK NYCKEL: TIAAAA                   
000500     03 AAR-TIAAAA           PIC 9(4).                                    
000600*                                 ÅRTAL (ÅÅÅÅ)                            
000700*                                 YEAR  (YYYY)                            
000800     03 AAR-VECKO            OCCURS 53 TIMES.                             
000900        05 AAR-KVOI-DIV      PIC S9(7)           COMP-3.                  
001000*                                 ORDERINGÅNG DIVERSE OCH TPO             
001100*                                 ORDERINCOME DIVERSE AND TPO             
001200        05 AAR-KVOI-NDC      PIC S9(7)           COMP-3.                  
001300*                                 ORDERINGÅNG LEV FRÅN NDC                
001400*                                 ORDERED PCS PER TIME UNIT               
001500*                                 FROM NDC                                
001600        05 AAR-KVOI-PROG     PIC S9(7)           COMP-3.                  
001700*                                 ORDERINGÅNG PROGNOSPÅVERKANDE           
001800*                                 ORDER INCOME NORMAL                     
001900        05 AAR-KVOI-REFILL   PIC S9(7)           COMP-3.                  
002000*                                 ORDERINGÅNG LEV FRÅN REFILL             
002100*                                 ORDERED PCS PER TIME UNIT               
002200*                                 FROM REFILL                             
002300        05 AAR-KVOI-SATS     PIC S9(7)           COMP-3.                  
002400*                                 ORDERINGÅNG SATSFÖRBRUKNING             
002500*                                 ORDER INCOME FOR KITS                   
002600        05 AAR-KVOI-SDC      PIC S9(7)           COMP-3.                  
002700*                                 ORDERINGÅNG LEV FRÅN SDC                
002800*                                 ORDERED PCS PER TIME UNIT               
002900*                                 FROM SDC                                
003000        05 AAR-KVOI-LEDTID   PIC S9(7)           COMP-3.                  
003100*                                 ORDERINGÅNG FÖRSKUTEN                   
003200        05 AAR-KVOT-DIV      PIC S9(7)           COMP-3.                  
003300*                                 ORDERTRÄFF, DIVERSE                     
003400*                                 ORDER HITS, DIVERSE AND TPO             
003500        05 AAR-KVOT-PROG     PIC S9(7)           COMP-3.                  
003600*                                 ORDERTRÄFF, PROGNOSPÅVERKANDE           
003700*                                 ORDER HITS, AFFECTS FORECASTS           
003800        05 AAR-KVOT-REFILL   PIC S9(7)           COMP-3.                  
003900*                                 ORDERTRÄFF, REFILL                      
004000*                                                                         
004100*                                 ORDER HITS FROM REFILL ORDERS           
004200*                                                                         
004300        05 AAR-KVOT-SATS     PIC S9(7)           COMP-3.                  
004400*                                 ORDERTRÄFF, SATS                        
004500*                                 ORDER HITS FROM KIT ORDERS              
004600*** END OF VILMAII-COPY LENGTH= 2336 BYTES                                
