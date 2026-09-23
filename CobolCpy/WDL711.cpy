000100 01  DC-WDL711.                                                           
000200*                                 ORDERINGÅNG-STATISTIK                   
000300*                                 LAGERINFORMATION                        
000400*                                 FYSISK NYCKEL: IDDC                     
000500     03 DC-IDDC              PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 DC-TIREFEFT          PIC S9(7)           COMP-3.                  
000900*                                 DATUM SENAST EFTERFRÅGAD                
001000*                                 DATE LATEST DEMAND                      
001100     03 DC-RULL-FORBRUKN     OCCURS 53 TIMES.                             
001200        05 DC-KVOI-RULL      PIC S9(7)           COMP-3.                  
001300*                                 ORDERINGÅNG TILL SDC                    
001400*                                 ORDERED PCS PER TIME UNIT               
001500*                                 ORDERED FROM SDC                        
001600        05 DC-KVOI-CDC-RULL  PIC S9(7)           COMP-3.                  
001700*                                 ORDERINGÅNG LEV FRÅN CDC                
001800*                                 ORDERED PCS PER TIME UNIT               
001900*                                 FORWARDED TO CDC                        
002000        05 DC-KVOI-REF-RULL  PIC S9(7)           COMP-3.                  
002100*                                 ORDERINGÅNG LEV FRÅN REF-RULL           
002200*                                 ORDERED PCS PER TIME UNIT               
002300*                                 FROM REFILL RULL                        
002400        05 DC-KVOT-RULL      PIC S9(7)           COMP-3.                  
002500*                                 ORDERTRÄFFAR PÅ SDC                     
002600*                                 ORDERHITS ON SDC                        
002700        05 DC-KVOT-CDC-RULL  PIC S9(7)           COMP-3.                  
002800*                                 ORDERTRÄFFAR LEV FRÅN CDC               
002900*                                 ORDERHITS ON SDC FORWARDED              
003000*                                 TO CDC                                  
003100        05 DC-KVOT-REF-RULL  PIC S9(7)           COMP-3.                  
003200*                                 ORDERTRÄFF, REF-RULL                    
003300*                                 ORDER HITS FROM REF-RULL ORDERS         
003400     03 DC-INNEV-FORBRUKN    OCCURS 5 TIMES.                              
003500        05 DC-TIVV           PIC S9(3)           COMP-3.                  
003600*                                 VECKA  (VV)                             
003700*                                 WEEK   (WW)                             
003800        05 DC-KVOI-INNEV     PIC S9(7)           COMP-3.                  
003900*                                 ORDERINGÅNG TILL DC INNEV PER           
004000*                                 ORDERED PCS THIS PERIOD                 
004100*                                 ORDERED FROM DC                         
004200        05 DC-KVOI-CDC-INNEV PIC S9(7)           COMP-3.                  
004300*                                 ORDERINGÅNG LEV FRÅN CDC                
004400*                                 ORDERED PCS PER TIME UNIT               
004500*                                 FORWARDED TO CDC                        
004600        05 DC-KVOI-REF-INNEV PIC S9(7)           COMP-3.                  
004700*                                 ORDERINGÅNG LEV FRÅN REF-INNEV          
004800*                                 ORDERED PCS PER TIME UNIT               
004900*                                 FROM REFILL ACTUAL                      
005000        05 DC-KVOI-PP-INNEV  PIC S9(7)           COMP-3.                  
005100*                                 ORDERINGÅNG TILL DC INNEV PER           
005200*                                 ORDERED PCS THIS PERIOD                 
005300*                                 ORDERED FROM DC                         
005400        05 DC-KVOT-INNEV     PIC S9(7)           COMP-3.                  
005500*                                 ORDERTRÄFFAR PÅ SDC                     
005600*                                 ORDERHITS ON SDC                        
005700        05 DC-KVOT-CDC-INNEV PIC S9(7)           COMP-3.                  
005800*                                 ORDERTRÄFFAR LEV FRÅN CDC               
005900*                                 ORDERHITS ON SDC FORWARDED              
006000*                                 TO CDC                                  
006100        05 DC-KVOT-REF-INNEV PIC S9(7)           COMP-3.                  
006200*                                 ORDERTRÄFF, REF-INNEV                   
006300*                                 ORDER HITS FROM REF-ACT ORDERS          
006400        05 DC-KVOT-PP-INNEV  PIC S9(7)           COMP-3.                  
006500*                                 ORDERTRÄFFAR PÅ SDC                     
006600*                                 ORDERHITS ON SDC                        
006700*** END OF VILMAII-COPY LENGTH= 1448 BYTES                                
