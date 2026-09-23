000100 01  MID-W4I67101.                                                        
000200*                                 MID-COPYTEXT FÖR W4067100               
000300     03 MID-BEROUTE-IN       PIC X(25).                                   
000400*                                 FÄRDVÄG, DESTINATION                    
000500     03 MID-BEROUTE-UT       PIC X(25).                                   
000600*                                 FÄRDVÄG, DESTINATION                    
000700     03 MID-IDTRANSP-NAMN-IN PIC X(15).                                   
000800*                                 TRANSPORTMEDEL NAMN                     
000900     03 MID-IDTRANSP-NAMN-UT PIC X(15).                                   
001000*                                 TRANSPORTMEDEL NAMN                     
001100     03 MID-BEROUTE-SPA      PIC X(25).                                   
001200*                                 FÄRDVÄG, DESTINATION                    
001300     03 MID-IDTRANSP-NAMN-SPA                                             
001400                             PIC X(15).                                   
001500*                                 TRANSPORTMEDEL NAMN                     
001600     03 MID-W4I67101-001     OCCURS 14 TIMES.                             
001700*                                 MID-COPYTEXT FÖR W4067100               
001800        05 MID-KDCMD         PIC X.                                       
001900         88 MID-KDCMD-INGENTING                                           
002000                             VALUE ' '.                                   
002100         88 MID-KDCMD-DELETE VALUE 'D'.                                   
002200         88 MID-KDCMD-REPLACE                                             
002300                             VALUE 'R'.                                   
002400         88 MID-KDCMD-INSERT VALUE 'I'.                                   
002500         88 MID-KDCMD-BORTTAG                                             
002600                             VALUE 'B'.                                   
002700         88 MID-KDCMD-AENDRA VALUE 'Ä'.                                   
002800         88 MID-KDCMD-NYUPPLAEGG                                          
002900                             VALUE 'N'.                                   
003000*                                 RAD-UPPDATERINGSKOMMANDO                
003100        05 MID-BEROUTE       PIC X(25).                                   
003200*                                 FÄRDVÄG, DESTINATION                    
003300        05 MID-IDTRANSP-NAMN PIC X(15).                                   
003400*                                 TRANSPORTMEDEL NAMN                     
003500*** END COPY W4I67101C0  LENGTH=694                                       
