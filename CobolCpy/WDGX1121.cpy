000100 01  1121-WDGX1121.                                                       
000200*                                 BASLAGER                                
000300*                                 GODK. MARKNADSSTRUKTURER                
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (IDHTYP + KDPRODSL +                    
000600*                                  + LOWVALUE)                            
000700     03 1121-IDHTYP          PIC X(4).                                    
000800*                                 HÄNDELSETYP                             
000900     03 1121-KDPRODSL        PIC S9(3)           COMP-3.                  
001000*                                 PRODUKTSLAG                             
001100*                                 TYPE OF ASSORTMENT                      
001200     03 1121-LOWVALUE        PIC X(24).                                   
001300*** END COPY WDGX1121C0  LENGTH=30                                        
