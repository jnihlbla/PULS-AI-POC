000100 01  W91087.                                                              
000200*                                 UTDRAG UR WDD301                        
000300*                                                                         
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 BEART-TAB            OCCURS 17 TIMES.                             
000700        05 IDSKYLT           PIC X(3).                                    
000800*                                 NATIONALITETSTECKEN                     
000900*                                 SPRÅKIDENTIFIKATION                     
001000        05 BEARTEXT          PIC X(100).                                  
001100*                                 UTÖKAD ARTIKELBENÄMNING                 
001200        05 BEART-FILLER REDEFINES BEARTEXT.                               
001300           07 BEART          PIC X(25).                                   
001400*                                 ARTIKELBENÄMNING                        
001500           07 FILLER         PIC X(75).                                   
001600*** END OF VILMAII-COPY LENGTH= 1756 BYTES                                
