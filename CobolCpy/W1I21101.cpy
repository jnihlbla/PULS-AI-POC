000100 01  MID-W1I21101.                                                        
000200*                                 MID-COPYTEXT FÖR W121100                
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDSKYLT-IN       PIC X(3).                                    
000800*                                 NATIONALITETSTECKEN                     
000900*                                 SPRÅKIDENTIFIKATION                     
001000     03 MID-IDSKYLT-UT       PIC X(3).                                    
001100*                                 NATIONALITETSTECKEN                     
001200*                                 SPRÅKIDENTIFIKATION                     
001300     03 MID-INPUT.                                                        
001400*                                                                         
001500        05 MID-IDARTNR-NY    PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700        05 MID-BEART-IN      PIC X(25).                                   
001800*                                 ARTIKELBENÄMNING                        
001900        05 MID-KDBENHOM-IN   PIC X.                                       
002000*                                 HOMONYMKOD                              
002100        05 MID-BORT          PIC X.                                       
002200*                                 ALLMÄN SVARSFLAGGA                      
002300        05 MID-KDPRODSL-IN   PIC X(2).                                    
002400*                                 PRODUKTSLAG                             
002500        05 MID-IDFKNGRP-IN   PIC X(4).                                    
002600*                                 FUNKTIONSGRUPP                          
002700        05 MID-IDSTRTYP-IN   PIC X.                                       
002800*                                 STRUKTURTYP                             
002900        05 MID-TESTRNOT      OCCURS 2 TIMES                               
003000                             PIC X(70).                                   
003100*                                 STRUKTURNOTERING                        
003200*** END OF VILMAII-COPY LENGTH= 207 BYTES                                 
