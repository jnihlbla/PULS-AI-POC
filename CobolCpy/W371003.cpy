000100 01  W3718A00.                                                            
000200*                                 ARTIKELSTATISTIK                        
000300*                                 PRIMÄWREGISTER                          
000400*                                 ARTIKELPOST                             
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 BYTESNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 PRODNR               OCCURS 3 TIMES                               
001000                             PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200*** END COPY W371003CC0  LENGTH=23                                        
