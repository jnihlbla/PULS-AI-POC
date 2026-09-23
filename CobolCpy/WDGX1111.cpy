000100 01  1111-WDGX1111.                                                       
000200*                                 ERSÄTTNING                              
000300*                                 MELLANLAGRING VID ÄNDRING               
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (IDHTYP + IDARTNR +                     
000600*                                  + LOWVALUE)                            
000700     03 1111-IDHTYP          PIC X(4).                                    
000800*                                 HÄNDELSETYP                             
000900     03 1111-IDARTNR         PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 1111-LOWVALUE        PIC X(21).                                   
001300*** END COPY WDGX1111C0  LENGTH=30                                        
