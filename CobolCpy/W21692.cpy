000100 01  W21692.                                                              
000200*                                 ARTIKLAR MED UPPGIFT OM SÄSONG          
000300*                                                                         
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 KDCLAGER             PIC S9              COMP-3.                  
000700*                                 CENTRALLAGERKOD                         
000800     03 IDFTG                PIC 9(2).                                    
000900*                                 FÖRETAGSID EKONOM REDOVISNING           
001000     03 SEASON-GRP.                                                       
001100        05 RESEASON          OCCURS 12 TIMES                              
001200                             PIC S9V9(2)         COMP-3.                  
001300*                                 SÄSONGSINDEX                            
001400*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
