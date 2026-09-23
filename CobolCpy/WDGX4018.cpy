000100 01  4018-WDGX4018.                                                       
000200*                                 UTSKRIFT AV PLOCKSATSER LDC-GB          
000300*                                 SPARAD TABELL VID OMSTART               
000400*                                 FYSISK NYCKEL: KDSEGKEY                 
000500     03 4018-KDSEGKEY        PIC X.                                       
000600*                                 TEKNISK SEGMENT-NYCKEL                  
000700*                                 TECHNICAL SEGMENT KEY                   
000800     03 4018-LDC-TABELL      OCCURS 100 TIMES.                            
000900        05 4018-BERADREF     PIC X(10).                                   
001000*                                 KUNDENS RADREFERENS                     
001100*                                 CUSTOMERS ITEM REF.                     
001200        05 4018-KVANTART     PIC S9(5)           COMP-3.                  
001300*                                 ANTAL-ARTIKLAR                          
001400*                                 QUANTITY PARTS                          
001500*** END OF VILMAII-COPY LENGTH= 1301 BYTES                                
