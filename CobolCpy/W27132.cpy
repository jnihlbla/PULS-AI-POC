000100 01  W27132.                                                              
000200*                                 COPYTEXT TILL FILEN W27132,             
000300*                                 ARTIKLAR VARS SÄSONGSINDEX              
000400*                                 SKA UPPDATERAS                          
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 RESEASON             OCCURS 12 TIMES                              
001000                             PIC S9V9(2)         COMP-3.                  
001100*                                 SÄSONGSINDEX                            
001200     03 OSAKERHET            PIC 9(3)V9(1).                               
001300     03 ANT-HIST-AR          PIC S9              COMP-3.                  
001400     03 ANT-HIST-MAN         PIC 9(2).                                    
001500     03 SEASON-ARTIKEL       PIC X.                                       
001600*** END OF VILMAII-COPY LENGTH= 39 BYTES                                  
