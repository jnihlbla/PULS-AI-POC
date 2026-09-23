000100 01  MID-W1I23101.                                                        
000200*                                 MID-COPYTEXT FÖR W1023100               
000300     03 MID-LEV-IN           PIC X.                                       
000400*                                 ANGER ATT ARTIKELN ÄR UTGÅNGEN          
000500     03 MID-KDPRODSL-IN      PIC X(2).                                    
000600*                                 PRODUKTSLAG                             
000700     03 MID-IDSKYLT-IN       PIC X(3).                                    
000800*                                 NATIONALITETSTECKEN                     
000900*                                 SPRÅKIDENTIFIKATION                     
001000     03 MID-LEV-UT           PIC X.                                       
001100     03 MID-KDPRODSL-UT      PIC X(2).                                    
001200*                                 PRODUKTSLAG                             
001300     03 MID-IDSKYLT-UT       PIC X(3).                                    
001400*                                 NATIONALITETSTECKEN                     
001500*                                 SPRÅKIDENTIFIKATION                     
001600     03 MID-IDARTNR-STR-ENTER                                             
001700                             PIC 9(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MID-IDARTNR-STR-NEXT PIC 9(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MID-INPUT            OCCURS 14 TIMES.                             
002200*                                                                         
002300        05 MID-SELECT        PIC X.                                       
002400        05 MID-IDARTNR-STR   PIC 9(9).                                    
002500*                                 ARTIKELNUMMER                           
002600*** END OF VILMAII-COPY LENGTH= 170 BYTES                                 
