000100 01  MID-W4I72101.                                                        
000200*                                 MID-COPYTEXT FÖR W40721                 
000300     03 MID-IDANSV-IN        PIC X(6).                                    
000400     03 MID-IDANSV-UT        PIC X(6).                                    
000500     03 MID-TILEVANM-IN      PIC 9(6).                                    
000600*                                 DATUM LEVERANSANMÄRKNING                
000700     03 MID-TILEVANM-UT      PIC 9(6).                                    
000800*                                 DATUM LEVERANSANMÄRKNING                
000900     03 MID-KDANMORS-IN      PIC X(2).                                    
001000*                                 ORSAK TILL LEVERANSANMÄRKNING           
001100     03 MID-KDANMORS-UT      PIC X(2).                                    
001200*                                 ORSAK TILL LEVERANSANMÄRKNING           
001300     03 MID-KDKREBEH-IN      PIC X(3).                                    
001400*                                 BEHANDLINGSSTATUS                       
001500     03 MID-KDKREBEH-UT      PIC X(3).                                    
001600*                                 BEHANDLINGSSTATUS                       
001700     03 MID-FLSUM-IN         PIC X.                                       
001800*                                 ALLMÄN FLAGGA                           
001900     03 MID-FLSUM-UT         PIC X.                                       
002000*                                 ALLMÄN FLAGGA                           
002100     03 MID-INPUT.                                                        
002200*                                 INMATNINGSFÄLT                          
002300        05 MID-KDCMD         OCCURS 12 TIMES                              
002400                             PIC X(4).                                    
002500     03 MID-KEYFIELD         OCCURS 12 TIMES.                             
002600*                                 NYCKELFÄLT PÅ RADEN                     
002700        05 MID-IDDISTR       PIC X(4).                                    
002800*                                 DISTRIKTNUMMER                          
002900        05 MID-IDKUNDNR      PIC X(6).                                    
003000*                                 KUNDNUMMER                              
003100        05 MID-IDRAPPNR      PIC X(7).                                    
003200*                                 RAPPORT NUMMER                          
003300        05 MID-IDRADNR       PIC X(4).                                    
003400*                                 RADNUMMER                               
003500        05 MID-IDARTNR       PIC X(8).                                    
003600*                                 ARTIKELNUMMER                           
003700*** END OF VILMAII-COPY LENGTH= 432 BYTES                                 
