000100 01  W02022X-CTX.                                                         
000200*                                 HELT BIPACKAD RESTORDER SUM-            
000300*                                 MERAD PER MARKNAD, ORDERKL OCH          
000400*                                 PSLAG.                                  
000500     03 IDDISTR              PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 KDORDKL              PIC S9              COMP-3.                  
001000*                                 ORDERKLASS                              
001100     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001200*                                 PRODUKTSLAG                             
001300     03 TIAAVV               PIC S9(5)           COMP-3.                  
001400*                                 ≈R - VECKA  (≈≈VV)                      
001500     03 SULEVANT             OCCURS 15 TIMES                              
001600                             INDEXED TIK-IX                               
001700                             PIC S9(9)           COMP-3.                  
001800*                                 SUMMA LEVERERAT ANTAL                   
001900*                                 AV 1 ARTIKEL                            
002000*** END OF VILMAII-COPY LENGTH= 89 BYTES                                  
