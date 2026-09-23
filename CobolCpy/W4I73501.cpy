000100 01  MID-W4I73501.                                                        
000200*                                 MID-COPYTEXT FÖR W40735                 
000300     03 MID-IDSNDNNR-IN      PIC X(6).                                    
000400     03 MID-IDSNDNNR-UT      PIC X(6).                                    
000500     03 MID-IDKOLLI-IN       PIC X(5).                                    
000600*                                 KOLLINUMMER                             
000700     03 MID-IDKOLLI-UT       PIC X(5).                                    
000800*                                 KOLLINUMMER                             
000900     03 MID-IDDISTR-IN       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MID-IDDISTR-UT       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001600*                                 KUNDNUMMER                              
001700     03 MID-IDRAPPNR-IN      PIC X(7).                                    
001800*                                 RAPPORT NUMMER                          
001900     03 MID-IDRAPPNR-UT      PIC X(7).                                    
002000*                                 RAPPORT NUMMER                          
002100     03 MID-INPUT.                                                        
002200*                                 INMATNINGSFÄLT                          
002300        05 MID-IDPRT         PIC X(3).                                    
002400*                                 LOGISK PRINTERIDENTITET                 
002500        05 MID-IDANSTNR      PIC 9(5).                                    
002600*                                 ANSTÄLLNINGSNUMMER                      
002700        05 MID-KDCMD         OCCURS 13 TIMES                              
002800                             PIC X(4).                                    
002900     03 MID-KEYFIELD         OCCURS 13 TIMES.                             
003000*                                 NYCKELFÄLT PÅ RADEN                     
003100        05 MID-IDKOLLI       PIC X(5).                                    
003200*                                 KOLLINUMMER                             
003300        05 MID-IDDISTR       PIC X(4).                                    
003400*                                 DISTRIKTNUMMER                          
003500        05 MID-IDKUNDNR      PIC X(6).                                    
003600*                                 KUNDNUMMER                              
003700        05 MID-IDRAPPNR      PIC X(7).                                    
003800*                                 RAPPORT NUMMER                          
003900        05 MID-TERETNOT      PIC X(18).                                   
004000*** END OF VILMAII-COPY LENGTH= 636 BYTES                                 
