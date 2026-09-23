000100 01  1123-WDGX1123.                                                       
000200*                                 BASLAGER                                
000300*                                 PROJEKTSTRUKTURER                       
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (IDHTYP + KDPRODSL +                    
000600*                                  + IDPROJ + LOWVALUE)                   
000700     03 1123-IDHTYP          PIC X(4).                                    
000800*                                 HÄNDELSETYP                             
000900     03 1123-KDPRODSL        PIC S9(3)           COMP-3.                  
001000*                                 PRODUKTSLAG                             
001100*                                 TYPE OF ASSORTMENT                      
001200     03 1123-IDPROJ          PIC X(4).                                    
001300*                                 PARTS PROJEKTIDENTITET                  
001400*                                 PARTS PROJECT IDENTITY                  
001500     03 1123-LOWVALUE        PIC X(20).                                   
001600*** END COPY WDGX1123C0  LENGTH=30                                        
