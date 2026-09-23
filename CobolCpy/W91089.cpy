000100 01  W91089.                                                              
000200*                                 UTDRAG UR WDD301                        
000300*                                                                         
000400     03 IDARTNR              PIC 9(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 IDFKNGRP             PIC 9(4).                                    
000700*                                 FUNKTIONSGRUPP                          
000800     03 KDERS                PIC 9(2).                                    
000900*                                 ERSÄTTNINGSKOD                          
001000     03 BEART-TAB            OCCURS 17 TIMES.                             
001100        05 IDSKYLT           PIC X(3).                                    
001200*                                 NATIONALITETSTECKEN                     
001300*                                 SPRÅKIDENTIFIKATION                     
001400        05 BEARTEXT          PIC X(100).                                  
001500*                                 UTÖKAD ARTIKELBENÄMNING                 
001600        05 BEART-FILLER REDEFINES BEARTEXT.                               
001700           07 BEART          PIC X(25).                                   
001800*                                 ARTIKELBENÄMNING                        
001900           07 FILLER         PIC X(75).                                   
002000*** END OF VILMAII-COPY LENGTH= 1766 BYTES                                
