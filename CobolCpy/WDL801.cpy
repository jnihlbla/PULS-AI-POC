000100 01  ART-WDL801.                                                          
000200*                                 ORDERINGÅNG-STATISTIK                   
000300*                                 ARTIKELINFORMATION                      
000400*                                 DAGLIG I 3 VECKOR = 21 DAGAR            
000500*                                 FYSISK NYCKEL: IDARTNR                  
000600     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800*                                 PART NUMBER                             
000900     03 ART-DAGLIG           OCCURS 21 TIMES.                             
001000        05 ART-TIVVD         PIC 9(3).                                    
001100*                                 VECKA + DAG (VVD)                       
001200*                                 WEEK + DAY (WWD)                        
001300        05 ART-KVOI-DIV      PIC S9(7)           COMP-3.                  
001400*                                 ORDERINGÅNG DIVERSE OCH TPO             
001500*                                 ORDERINCOME DIVERSE AND TPO             
001600        05 ART-KVOI-NDC      PIC S9(7)           COMP-3.                  
001700*                                 ORDERINGÅNG LEV FRÅN NDC                
001800*                                 ORDERED PCS PER TIME UNIT               
001900*                                 FROM NDC                                
002000        05 ART-KVOI-PROG     PIC S9(7)           COMP-3.                  
002100*                                 ORDERINGÅNG PROGNOSPÅVERKANDE           
002200*                                 ORDER INCOME NORMAL                     
002300        05 ART-KVOI-REFILL   PIC S9(7)           COMP-3.                  
002400*                                 ORDERINGÅNG LEV FRÅN REFILL             
002500*                                 ORDERED PCS PER TIME UNIT               
002600*                                 FROM REFILL                             
002700        05 ART-KVOI-SATS     PIC S9(7)           COMP-3.                  
002800*                                 ORDERINGÅNG SATSFÖRBRUKNING             
002900*                                 ORDER INCOME FOR KITS                   
003000        05 ART-KVOI-SDC      PIC S9(7)           COMP-3.                  
003100*                                 ORDERINGÅNG LEV FRÅN SDC                
003200*                                 ORDERED PCS PER TIME UNIT               
003300*                                 FROM SDC                                
003400        05 ART-KVOI-LEDTID   PIC S9(7)           COMP-3.                  
003500*                                 ORDERINGÅNG FÖRSKUTEN                   
003600*** END OF VILMAII-COPY LENGTH= 656 BYTES                                 
