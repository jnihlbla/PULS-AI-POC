000100 01  DC-W01186.                                                           
000200*                                 UTDRAG UT WDL701 OCH WDL711             
000300*                                                                         
000400     03 DC-IDARTNR           PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 DC-IDDC              PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 DC-FILLERX2          PIC X(2).                                    
001100     03 DC-TIREFEFT          PIC S9(7)           COMP-3.                  
001200*                                 DATUM SENAST EFTERFRÅGAD                
001300*                                 DATE LATEST DEMAND                      
001400     03 DC-RULL-FORBRUKN     OCCURS 53 TIMES.                             
001500        05 DC-KVOI-RULL      PIC S9(7)           COMP-3.                  
001600*                                 ORDERINGÅNG TILL SDC                    
001700*                                 ORDERED PCS PER TIME UNIT               
001800*                                 ORDERED FROM SDC                        
001900        05 DC-KVOI-CDC-RULL  PIC S9(7)           COMP-3.                  
002000*                                 ORDERINGÅNG LEV FRÅN CDC                
002100*                                 ORDERED PCS PER TIME UNIT               
002200*                                 FORWARDED TO CDC                        
002300        05 DC-KVOI-REF-RULL  PIC S9(7)           COMP-3.                  
002400*                                 ORDERINGÅNG LEV FRÅN REF-RULL           
002500*                                 ORDERED PCS PER TIME UNIT               
002600*                                 FROM REFILL RULL                        
002700        05 DC-KVOT-RULL      PIC S9(7)           COMP-3.                  
002800*                                 ORDERTRÄFFAR PÅ SDC                     
002900*                                 ORDERHITS ON SDC                        
003000        05 DC-KVOT-CDC-RULL  PIC S9(7)           COMP-3.                  
003100*                                 ORDERTRÄFFAR LEV FRÅN CDC               
003200*                                 ORDERHITS ON SDC FORWARDED              
003300*                                 TO CDC                                  
003400        05 DC-KVOT-REF-RULL  PIC S9(7)           COMP-3.                  
003500*                                 ORDERTRÄFF, REF-RULL                    
003600*                                 ORDER HITS FROM REF-RULL ORDERS         
003700     03 DC-INNEV-FORBRUKN    OCCURS 5 TIMES.                              
003800        05 DC-TIVV           PIC S9(3)           COMP-3.                  
003900*                                 VECKA  (VV)                             
004000*                                 WEEK   (WW)                             
004100        05 DC-KVOI-INNEV     PIC S9(7)           COMP-3.                  
004200*                                 ORDERINGÅNG TILL DC INNEV PER           
004300*                                 ORDERED PCS THIS PERIOD                 
004400*                                 ORDERED FROM DC                         
004500        05 DC-KVOI-CDC-INNEV PIC S9(7)           COMP-3.                  
004600*                                 ORDERINGÅNG LEV FRÅN CDC                
004700*                                 ORDERED PCS PER TIME UNIT               
004800*                                 FORWARDED TO CDC                        
004900        05 DC-KVOI-REF-INNEV PIC S9(7)           COMP-3.                  
005000*                                 ORDERINGÅNG LEV FRÅN REF-INNEV          
005100*                                 ORDERED PCS PER TIME UNIT               
005200*                                 FROM REFILL ACTUAL                      
005300        05 DC-KVOI-PP-INNEV  PIC S9(7)           COMP-3.                  
005400*                                 ORDERINGÅNG TILL DC INNEV PER           
005500*                                 ORDERED PCS THIS PERIOD                 
005600*                                 ORDERED FROM DC                         
005700        05 DC-KVOT-INNEV     PIC S9(7)           COMP-3.                  
005800*                                 ORDERTRÄFFAR PÅ SDC                     
005900*                                 ORDERHITS ON SDC                        
006000        05 DC-KVOT-CDC-INNEV PIC S9(7)           COMP-3.                  
006100*                                 ORDERTRÄFFAR LEV FRÅN CDC               
006200*                                 ORDERHITS ON SDC FORWARDED              
006300*                                 TO CDC                                  
006400        05 DC-KVOT-REF-INNEV PIC S9(7)           COMP-3.                  
006500*                                 ORDERTRÄFF, REF-INNEV                   
006600*                                 ORDER HITS FROM REF-ACT ORDERS          
006700        05 DC-KVOT-PP-INNEV  PIC S9(7)           COMP-3.                  
006800*                                 ORDERTRÄFFAR PÅ SDC                     
006900*                                 ORDERHITS ON SDC                        
007000     03 DC-FOREG-FORBRUKN    OCCURS 2 TIMES.                              
007100        05 DC-KVOT-FOREG     PIC S9(7)           COMP-3.                  
007200*                                 ORDERTRÄFFAR TILL SDC                   
007300*                                 ORDERHITS ON SDC                        
007400        05 DC-KVOT-CDC-FOREG PIC S9(7)           COMP-3.                  
007500*                                 ORDERTRÄFFAR LEV FRÅN CDC               
007600*                                 ORDERHITS ON SDC FORWARDED              
007700*                                 TO CDC                                  
007800        05 DC-KVOT-REF-FOREG PIC S9(7)           COMP-3.                  
007900*                                 ORDERTRÄFF, REF-FOREG                   
008000*                                 ORDER HITS FROM REF PREV ORDERS         
008100     03 DC-ARS-FORBRUKN      OCCURS 5 TIMES.                              
008200        05 DC-KVOI-PER       OCCURS 12 TIMES.                             
008300           07 DC-KVOI        PIC S9(7)           COMP-3.                  
008400*                                 ORDERINGÅNG I STYCK PER TIDSENH         
008500*                                 ORDERED PCS PER TIME UNIT               
008600           07 DC-KVOI-REFILL PIC S9(7)           COMP-3.                  
008700*                                 ORDERINGÅNG LEV FRÅN REFILL             
008800*                                 ORDERED PCS PER TIME UNIT               
008900*                                 FROM REFILL                             
009000           07 DC-KVVIPER     PIC 9.                                       
009100*                                 ANT VECKOR I REDOVISNINGSPERIOD         
009200*                                 NUMBER OF WEEKS IN A PERIOD             
009300     03 DC-FILLER            PIC X(75).                                   
009400*** END OF VILMAII-COPY LENGTH= 2094 BYTES                                
