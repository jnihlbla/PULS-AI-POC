000100 01  MID-W0I70601.                                                        
000200*                                 COPYTEXT FÖR MID W0I70601               
000300     03 MID-IDRUTIN-IN       PIC X(8).                                    
000400*                                 RUTINNAMN (GRUPP AV JOBB)               
000500     03 MID-IDRUTIN-UT       PIC X(8).                                    
000600*                                 RUTINNAMN (GRUPP AV JOBB)               
000700     03 MID-IDJOB-IN         PIC X(8).                                    
000800*                                 JOBBNAMN                                
000900     03 MID-IDJOB-UT         PIC X(8).                                    
001000*                                 JOBBNAMN                                
001100     03 MID-FLKLAR           PIC X.                                       
001200*                                 AVSLUTNINGSMARKERING                    
001300     03 MID-IDJCLRAD-SKIP    PIC 9(5).                                    
001400*                                 RADNUMMER PÅ JCL                        
001500     03 MID-LINES            OCCURS 15 TIMES                              
001600                             INDEXED MID-IX-LINE.                         
001700        05 MID-IDJCLRAD      PIC 9(5).                                    
001800*                                 RADNUMMER PÅ JCL                        
001900        05 MID-KDCMD         PIC X.                                       
002000         88 MID-KDCMD-INGENTING                                           
002100                             VALUE ' '.                                   
002200         88 MID-KDCMD-DELETE VALUE 'D'.                                   
002300         88 MID-KDCMD-REPLACE                                             
002400                             VALUE 'R'.                                   
002500         88 MID-KDCMD-INSERT VALUE 'I'.                                   
002600         88 MID-KDCMD-BORTTAG                                             
002700                             VALUE 'B'.                                   
002800         88 MID-KDCMD-AENDRA VALUE 'Ä'.                                   
002900         88 MID-KDCMD-NYUPPLAEGG                                          
003000                             VALUE 'N'.                                   
003100*                                 RAD-UPPDATERINGSKOMMANDO                
003200*** END COPY W0I70601C0  LENGTH=128                                       
