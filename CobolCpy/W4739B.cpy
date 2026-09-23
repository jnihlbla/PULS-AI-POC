000100 01  W4739B.                                                              
000200*                                 FÖLJESEDELNUMMER    PROFORMA            
000300*                                                                         
000400     03 EMBNR-TAB            OCCURS 2 TIMES.                              
000500*                                 EMBALLAGENRSERIE  C1 OCH C2             
000600        05 IDEMBNR-MIN       PIC S9(7)           COMP-3.                  
000700*                                 EMBALLAGENUMMER MIN                     
000800        05 IDEMBNR-MAX       PIC S9(7)           COMP-3.                  
000900*                                 EMBALLAGENUMMER MAX                     
001000        05 IDEMBNR           PIC S9(7)           COMP-3.                  
001100*                                 EMBALLAGENUMMER                         
001200*** END COPY W4739BCCC0  LENGTH=24                                        
