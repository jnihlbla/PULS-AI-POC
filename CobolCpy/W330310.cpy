000100 01  W330310.                                                             
000200*                                 ARTIKELSTATISTIK                        
000300*                                 PRIMÄWREGISTER                          
000400*                                 DISTRIKTSPOST                           
000500*                                 FSG TOTALT OCH                          
000600*                                 FSG PÅ DAGORDER                         
000700     03 IDPTYP               PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 IDDISTR              PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100     03 SULEVANT             PIC S9(9)           COMP-3.                  
001200*                                 SUMMA LEVERERAT ANTAL                   
001300*                                 AV 1 ARTIKEL                            
001400     03 SUARTFSG             PIC S9(9)V9(2)      COMP-3.                  
001500*                                 SUMMA FÖRSÄLJNING PER ARTIKEL           
001600*                                                                         
001700     03 SULEVANT-DO          PIC S9(9)           COMP-3.                  
001800*                                 ANTAL LEVERERADE ARTIKLAR               
001900*                                 PÅ DAGORDER                             
002000     03 SUARTFSG-DO          PIC S9(9)V9(2)      COMP-3.                  
002100*                                 SUMMA FSG/ARTIKEL PÅ                    
002200*                                 DAGORDER                                
002300*** END COPY W330310CC0  LENGTH=28                                        
