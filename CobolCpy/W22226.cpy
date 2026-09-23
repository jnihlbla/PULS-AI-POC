000100 01  W22226-CTX.                                                          
000200*                                 COPYTEXT TILL FILEN W22226,             
000300*                                 ARTIKLAR VARS SÄSONGSINDEX              
000400*                                 SKA UPPDATERAS PÅ WDK6                  
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 RESEASON             OCCURS 12 TIMES                              
000800                             PIC S9V9(2)         COMP-3.                  
000900*                                 SÄSONGSINDEX                            
001000     03 OSAKERHET            PIC 9(3)V9(1).                               
001100     03 ANT-HIST-AR          PIC S9              COMP-3.                  
001200     03 SEASON-ARTIKEL       PIC X.                                       
001300*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  
