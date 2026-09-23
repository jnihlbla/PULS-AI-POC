000100 01  W56056.                                                              
000200     03 IDARTNR              PIC 9(8).                                    
000300*                                 ARTIKELNUMMER                           
000400*                                 PART NUMBER                             
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 KDSORT               PIC X(2).                                    
000900*                                 SORT-KOD                                
001000*                                 UNIT OF MEASURE                         
001100     03 PRAVCOST             PIC 9(7)V9(2).                               
001200*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
001300*                                 AVERAGE COST FOREIGN CURRENCY           
001400     03 KVLS                 PIC 9(7).                                    
001500*                                 LAGERSALDO                              
001600*                                 STOCK BALANCE                           
001700     03 KVOKS-DAG            PIC 9(7).                                    
001800*                                 ORDERKÖSALDO, KLASS 1                   
001900*                                 ORDER QUEUE BALANCE, CLASS 1            
002000     03 KVOKS-BULK           PIC 9(7).                                    
002100*                                 ORDERKÖSALDO, KLASS 2-4                 
002200*                                 ORDER QUEUE BALANCE, CLASS 2-4          
002300     03 KVEFRS               PIC 9(7).                                    
002400*                                 EJ FAKTURERAT ANTAL STYCK               
002500*                                 ORDERED NOT INVOICED QTY                
002600     03 KVROS-DAG            PIC 9(7).                                    
002700*                                 RESTORDERSALDO, KLASS 1                 
002800*                                 BACK ORDER BALANCE, CLASS 1             
002900     03 KVROS-BULK           PIC 9(7).                                    
003000*                                 RESTORDERSALDO, KLASS 2-4               
003100*                                 BACK ORDER BALANCE, CLASS 2-4           
003200     03 KVAKS-PAV            PIC 9(7).                                    
003300*                                 DEL AV AK PÅ VÄG                        
003400*                                 PART OF AK ON ITS WAY                   
003500     03 KVAKS-SDC            PIC 9(7).                                    
003600*                                 DEL AV AK SOM LIGGER I SDC              
003700*                                 PART OF AK IN THE SDC                   
003800     03 KVSPARR-KVAL         PIC 9(7).                                    
003900*                                 SPÄRRAT ANTAL KVALITETSFEL              
004000*                                 BLOCKED QUANTITY QUALITY ERROR          
004100     03 KVUTRS               PIC 9(7).                                    
004200*                                 UTREDNINGSSALDO                         
004300*                                 INVESTIGATION BALANCE                   
004400     03 FILLER               PIC X(200).                                  
004500*** END OF VILMAII-COPY LENGTH= 291 BYTES                                 
