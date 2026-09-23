000100 01  1137-WDGX1137.                                                       
000200*                                 NYA ARTIKLAR FRÅN PV, LV                
000300*                                 FUNKTIONSGRUPP TABELL                   
000400*                                 ANSKAFFARE                              
000500*                                 FYSISK NYCKEL: WDGXKEY                  
000600*                                 (IDHTYP + KDPRODSL +                    
000700*                                  LOWVALUE)                              
000800     03 1137-IDHTYP          PIC X(4).                                    
000900*                                 HÄNDELSETYP                             
001000     03 1137-KDPRODSL        PIC S9(3)           COMP-3.                  
001100*                                 PRODUKTSLAG                             
001200*                                 TYPE OF ASSORTMENT                      
001300     03 1137-LOWVALUE        PIC X(24).                                   
001400*** END COPY WDGX1137C0  LENGTH=30                                        
