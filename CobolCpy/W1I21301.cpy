000100 01  MID-W1I21301.                                                        
000200*                                 MID-COPYTEXT FÖR W121300                
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDSKYLT-IN       PIC X(3).                                    
000800*                                 NATIONALITETSTECKEN                     
000900     03 MID-IDSKYLT-UT       PIC X(3).                                    
001000*                                 NATIONALITETSTECKEN                     
001100     03 MID-IDRADNR-IN       PIC X(4).                                    
001200*                                 RADNUMMER                               
001300     03 MID-IDRADNR-UT       PIC X(4).                                    
001400*                                 RADNUMMER                               
001500     03 MID-IDRADNR-DOLT     PIC 9(4).                                    
001600*                                 RADNUMMER                               
001700     03 MID-IDRADNR-DOLT2    PIC 9(4).                                    
001800*                                 RADNUMMER                               
001900     03 MID-IDSATSNR-DOLT    PIC X(9).                                    
002000*                                 ARTIKELNUMMER                           
002100     03 MID-INPUT            OCCURS 12 TIMES.                             
002200*                                                                         
002300        05 MID-SELECT        PIC X.                                       
002400        05 MID-IDRADNR       PIC 9(4).                                    
002500*                                 RADNUMMER                               
002600     03 MID-KDPRTVAL         PIC X.                                       
002700*                                 PRINTER-VAL KOD                         
002800*** END COPY W1I21301C0  LENGTH=110                                       
