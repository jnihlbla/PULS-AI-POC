000100 01  1139-WDGX1139.                                                       
000200*                                 NYA ARTIKLAR FRÅN PV-CN                 
000300*                                 FUNKTIONSGRUPP TABELL                   
000400*                                 ANSKAFFARE                              
000500*                                 FYSISK NYCKEL: WDGXKEY                  
000600*                                 (IDHTYP + KDPRODSL + LOWVALUE)          
000700*                                                                         
000800     03 1139-IDHTYP          PIC X(4).                                    
000900*                                 HÄNDELSETYP                             
001000     03 1139-KDPRODSL        PIC S9(3)           COMP-3.                  
001100*                                 PRODUKTSLAG                             
001200*                                 PRODUCT GROUP                           
001300     03 1139-LOWVALUE        PIC X(24).                                   
001400*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
