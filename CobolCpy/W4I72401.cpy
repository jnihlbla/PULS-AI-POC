000100 01  MID-W4I72401.                                                        
000200*                                 MID-COPYTEXT FÖR W4072400               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MID-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-KDANMORS-IN      PIC 9(2).                                    
001600*                                 ORSAK TILL LEVERAN KDANMORS-002         
001700     03 MID-KDANMORS-UT      PIC 9(2).                                    
001800*                                 ORSAK TILL LEVERAN KDANMORS-002         
001900     03 MID-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MID-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MID-IDDISTR-ENTER    PIC 9(4).                                    
002400*                                 DISTRIKTNUMMER                          
002500     03 MID-IDKUNDNR-ENTER   PIC 9(6).                                    
002600*                                 KUNDNUMMER                              
002700     03 MID-IDRAPPNR-ENTER   PIC 9(7).                                    
002800*                                 RAPPORT NUMMER                          
002900     03 MID-IDRADNR-ENTER    PIC 9(5).                                    
003000*                                 RADNUMMER                               
003100     03 MID-IDDISTR-NEXT     PIC 9(4).                                    
003200*                                 DISTRIKTNUMMER                          
003300     03 MID-IDKUNDNR-NEXT    PIC 9(6).                                    
003400*                                 KUNDNUMMER                              
003500     03 MID-IDRAPPNR-NEXT    PIC 9(7).                                    
003600*                                 RAPPORT NUMMER                          
003700     03 MID-IDRADNR-NEXT     PIC 9(5).                                    
003800*                                 RADNUMMER                               
003900     03 MID-INPUT.                                                        
004000        05 MID-KDCMDVAL      OCCURS 13 TIMES                              
004100                             PIC X(3).                                    
004200*                                 GENERELL KOMMANDOKOD                    
004300     03 MID-RAD-INFO         OCCURS 13 TIMES.                             
004400        05 MID-IDDISTR       PIC 9(4).                                    
004500*                                 DISTRIKTNUMMER                          
004600        05 MID-IDKUNDNR      PIC 9(6).                                    
004700*                                 KUNDNUMMER                              
004800        05 MID-IDRAPPNR      PIC 9(7).                                    
004900*                                 RAPPORT NUMMER                          
005000*** END OF VILMAII-COPY LENGTH= 350 BYTES                                 
