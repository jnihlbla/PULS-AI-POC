000100 01  W02022.                                                              
000200*                                 HELT BIPACKAD RESTORDER SUM-            
000300*                                 MERAD PER MARKNAD, ORDERKL OCH          
000400*                                 PSLAG.                                  
000500     03 IDDISTR              PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 KDORDKL              PIC S9              COMP-3.                  
000800*                                 ORDERKLASS                              
000900     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001000*                                 PRODUKTSLAG                             
001100     03 TIAAVV               PIC S9(5)           COMP-3.                  
001200*                                 ≈R - VECKA  (≈≈VV)                      
001300     03 SULEVANT             OCCURS 15 TIMES                              
001400                             INDEXED TIK-IX                               
001500                             PIC S9(9)           COMP-3.                  
001600*                                 SUMMA LEVERERAT ANTAL                   
001700*                                 AV 1 ARTIKEL                            
001800*** END COPY W02022      LENGTH=84                                        
