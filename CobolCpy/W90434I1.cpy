000100 01  MID-W90434I1.                                                        
000200*                                 MID-COPYTEXT FÖR W9043400               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-FILLER           PIC X(3).                                    
000800     03 MID-FILLER           PIC X(3).                                    
000900     03 MID-INPUT.                                                        
001000*                                                                         
001100        05 MID-FILLER        PIC X(9).                                    
001200        05 MID-FILLER        PIC X(25).                                   
001300        05 MID-FILLER        PIC X.                                       
001400        05 MID-FILLER        PIC X.                                       
001500        05 MID-FILLER        PIC X(2).                                    
001600        05 MID-FILLER        PIC X(4).                                    
001700        05 MID-IDSTRTYP-IN   PIC X.                                       
001800*                                 STRUKTURTYP                             
001900        05 MID-FILLER        OCCURS 2 TIMES                               
002000                             PIC X(70).                                   
002100*** END OF VILMAII-COPY LENGTH= 207 BYTES                                 
