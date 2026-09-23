000100 01  2226-WDGX2226.                                                       
000200*                                 TID FÖR ANSKAFFARE                      
000300*                                 ATT BEKRÄFTA TPO                        
000400*                                 FYSISK NYCKEL                           
000500*                                 KDSEGKEY = 1                            
000600     03 2226-KDSEGKEY        PIC X.                                       
000700*                                 TEKNISK SEGMENT-NYCKEL                  
000800*                                 TECHNICAL SEGMENT KEY                   
000900     03 2226-KLASS           OCCURS 4 TIMES.                              
001000        05 2226-TPOTYP       OCCURS 6 TIMES.                              
001100           07 2226-KVARBTIM  PIC S9(3)           COMP-3.                  
001200*                                 ANTAL ARBETSTIMMAR                      
001300*                                 NUMBER OF WORKING HOURS                 
001400     03 2226-FILLER          PIC X(6).                                    
001500*** END COPY WDGX2226C0  LENGTH=55                                        
