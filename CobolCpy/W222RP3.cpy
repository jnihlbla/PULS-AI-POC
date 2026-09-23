000100 01  W222RP3.                                                             
000200*                                 UPPDATERING AV SÄSONGINDEX              
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 KDCLAGER             PIC S9              COMP-3.                  
000800*                                 CENTRALLAGERKOD                         
000900     03 RESEASON             OCCURS 12 TIMES                              
001000                             PIC S9V9(2)         COMP-3.                  
001100*                                 SÄSONGSINDEX                            
001200     03 FLABORT-SEASON       PIC X.                                       
001300*                                 BORTTAG AV BEFINTLIGA                   
001400*                                 SÄSONGSINDEX?                           
001500*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
