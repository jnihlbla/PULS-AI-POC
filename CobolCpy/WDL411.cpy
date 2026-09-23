000100 01  OIHD-WDL411.                                                         
000200*                                 ORDERINGÅNG-HISTORIK                    
000300*                                 LAGERINFORMATION XDC                    
000400*                                 FYSISK NYCKEL: IDDC                     
000500     03 OIHD-IDDC            PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 OIHD-FOREG-FORBRUKN  OCCURS 2 TIMES.                              
000900        05 OIHD-KVOT-FOREG   PIC S9(7)           COMP-3.                  
001000*                                 ORDERTRÄFFAR TILL SDC                   
001100*                                 ORDERHITS ON SDC                        
001200        05 OIHD-KVOT-CDC-FOREG                                            
001300                             PIC S9(7)           COMP-3.                  
001400*                                 ORDERTRÄFFAR LEV FRÅN CDC               
001500*                                 ORDERHITS ON SDC FORWARDED              
001600*                                 TO CDC                                  
001700        05 OIHD-KVOT-REF-FOREG                                            
001800                             PIC S9(7)           COMP-3.                  
001900*                                 ORDERTRÄFF, REF-FOREG                   
002000*                                 ORDER HITS FROM REF PREV ORDERS         
002100     03 OIHD-ARS-FORBRUKN    OCCURS 5 TIMES.                              
002200        05 OIHD-KVOI-PER     OCCURS 12 TIMES.                             
002300           07 OIHD-KVOI      PIC S9(7)           COMP-3.                  
002400*                                 ORDERINGÅNG I STYCK PER TIDSENH         
002500*                                 ORDERED PCS PER TIME UNIT               
002600           07 OIHD-KVOI-REFILL                                            
002700                             PIC S9(7)           COMP-3.                  
002800*                                 ORDERINGÅNG LEV FRÅN REFILL             
002900*                                 ORDERED PCS PER TIME UNIT               
003000*                                 FROM REFILL                             
003100           07 OIHD-KVVIPER   PIC 9.                                       
003200*                                 ANT VECKOR I REDOVISNINGSPERIOD         
003300*                                 NUMBER OF WEEKS IN A PERIOD             
003400*** END OF VILMAII-COPY LENGTH= 566 BYTES                                 
