000100 01  1131-WDGX1131.                                                       
000200*                                 NYA ARTIKLAR FRÅN PV, LV                
000300*                                 PROJEKT-INFORMATION                     
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (IDHTYP + KDPRODSL +                    
000600*                                  LOWVALUE)                              
000700     03 1131-IDHTYP          PIC X(4).                                    
000800*                                 HÄNDELSETYP                             
000900     03 1131-KDPRODSL        PIC S9(3)           COMP-3.                  
001000*                                 PRODUKTSLAG                             
001100*                                 TYPE OF ASSORTMENT                      
001200     03 1131-LOWVALUE        PIC X(24).                                   
001300*** END COPY WDGX1131C0  LENGTH=30                                        
