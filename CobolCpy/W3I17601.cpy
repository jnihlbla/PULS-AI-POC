000100 01  MID-W3I17601.                                                        
000200*                                 MID-COPYTEXT FÖR W3017600               
000300     03 MID-IDDISTR-IN       PIC X(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDDISTR-UT       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-IDKUNDNR-IN      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MID-IDFAKT-IN        PIC X(7).                                    
001200*                                 FAKTURANUMMER                           
001300     03 MID-IDFAKT-UT        PIC X(7).                                    
001400*                                 FAKTURANUMMER                           
001500     03 MID-IDBYTRAP-IN      PIC X(7).                                    
001600*                                 RAPPORTNUMMER  BYTES                    
001700     03 MID-IDBYTRAP-UT      PIC X(7).                                    
001800*                                 RAPPORTNUMMER  BYTES                    
001900     03 MID-INPUT            OCCURS 27 TIMES.                             
002000*                                 RADINFORMATION                          
002100        05 MID-IDARTNR-RAD   PIC 9(8).                                    
002200*                                 ARTIKELNUMMER                           
002300        05 MID-KVRETUR-RAD   PIC 9(7).                                    
002400*                                 ANTAL I RETUR                           
002500        05 MID-IDTABNR-RAD   PIC 9(3).                                    
002600*                                 TABELLNUMMER                            
002700     03 MID-FLKLAR           PIC X.                                       
002800*                                 AVSLUTNINGSMARKERING                    
002900*** END OF VILMAII-COPY LENGTH= 535 BYTES                                 
