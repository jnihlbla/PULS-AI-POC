000100 01  MID-W6I15201.                                                        
000200*                                                                         
000300     03 MID-ADLASTPL-IN      PIC X(3).                                    
000400*                                 LASTPLATS                               
000500     03 MID-ADLASTPL-UT      PIC X(3).                                    
000600*                                 LASTPLATS                               
000700     03 MID-KDTRPSTA-IN      PIC X.                                       
000800*                                 TRANSPORTSTATUS                         
000900     03 MID-KDTRPSTA-UT      PIC X.                                       
001000*                                 TRANSPORTSTATUS                         
001100     03 MID-IDARTNR-IN       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MID-IDARTNR-UT       PIC X(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MID-INPUT.                                                        
001600*                                                                         
001700        05 MID-RAD           OCCURS 13 TIMES.                             
001800*                                                                         
001900           07 MID-CMD        PIC X.                                       
002000           07 MID-KDTRPSTA   PIC X.                                       
002100*                                 TRANSPORTSTATUS                         
002200        05 MID-KVANTAL       PIC X(6).                                    
002300*                                 ANTAL                                   
002400        05 MID-LAST-KLAR     PIC X.                                       
002500        05 MID-PRINTER       PIC X(4).                                    
002600*** END OF VILMAII-COPY LENGTH= 63 BYTES                                  
