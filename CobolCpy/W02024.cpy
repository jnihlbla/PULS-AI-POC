000100 01  W02024.                                                              
000200*                                 HELT BIPACKAD RESTORDER SUM-            
000300*                                 MERAD PER MARKNAD, ORDERKL OCH          
000400*                                 PSLAG.                                  
000500*                                 KOMPLETTERAD MED MARKNAD.               
000600     03 IDSRMARK             PIC 9(3).                                    
000700*                                 MARKNADSKOD SERVICEGRADER               
000800     03 KDORDKL              PIC S9              COMP-3.                  
000900*                                 ORDERKLASS                              
001000     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001100*                                 PRODUKTSLAG                             
001200     03 TIAAVV               PIC S9(5)           COMP-3.                  
001300*                                 ≈R - VECKA  (≈≈VV)                      
001400     03 SULEVANT             OCCURS 15 TIMES                              
001500                             INDEXED TIK-IX                               
001600                             PIC S9(9)           COMP-3.                  
001700*                                 SUMMA LEVERERAT ANTAL                   
001800*                                 AV 1 ARTIKEL                            
001900*** END COPY W02024      LENGTH=84                                        
