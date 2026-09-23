000100 01  W61154.                                                              
000200*                                 LÄNKAREA FÖR W61154                     
000300*                                 W61154 SKA VARA IDENTISK MED            
000400*                                 W61158                                  
000500     03 INDATAFLT.                                                        
000600        05 IDARTNR           PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800        05 TIAAVVD           PIC S9(5)           COMP-3.                  
000900*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
001000     03 RESULTATFLT.                                                      
001100        05 KVBEHOV-CD        OCCURS 4 TIMES                               
001200                             PIC S9(7)V9(2)      COMP-3.                  
001300*                                 SUMMA BEHOV FÖR ETT CROSS               
001400*                                 DOCKING OMRÅDE                          
001500*                                                                         
001600        05 IDDC-GRUPP.                                                    
001700*                                 IDDC FYLLS I NÄR REFILLORDER            
001800*                                 SKA SKAPAS                              
001900           07 IDDC           OCCURS 20 TIMES                              
002000                             PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200        05 KVBEHOV-DC-GRUPP.                                              
002300*                                 IDDC FYLLS I NÄR REFILLORDER            
002400*                                 SKA SKAPAS                              
002500           07 KVBEHOV-DC     OCCURS 20 TIMES                              
002600                             PIC S9(7)V9(2)      COMP-3.                  
002700*                                 BEHOVSSTORLEK                           
002800*** END OF VILMAII-COPY LENGTH= 168 BYTES                                 
