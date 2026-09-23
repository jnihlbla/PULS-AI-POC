000100 01  MID-W5I11701.                                                        
000200*                                 MID-COPY TEXT FÖR W5011700              
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-KDPRIBEH-IN      PIC X.                                       
000600*                                 PRISBEHANDLINGSKOD                      
000700*                                  B = BORTTAGSMARKERAD. BEH EJ           
000800*                                  J = UPPDATERAS DIREKT                  
000900*                                  N = BEHANDLAS EJ. EJ KONTROLL.         
001000*                                  V = BEHANDLAS I VECKOKÖRNINGEN         
001100     03 MID-KDPRIBEH-UT      PIC X.                                       
001200*                                 PRISBEHANDLINGSKOD                      
001300*                                  B = BORTTAGSMARKERAD. BEH EJ           
001400*                                  J = UPPDATERAS DIREKT                  
001500*                                  N = BEHANDLAS EJ. EJ KONTROLL.         
001600*                                  V = BEHANDLAS I VECKOKÖRNINGEN         
001700     03 MID-INPUT            OCCURS 13 TIMES.                             
001800        05 MID-KDBEH         PIC X.                                       
001900*                                 BEHANDLINGSKOD                          
002000        05 MID-IDARTNR       PIC X(9).                                    
002100*                                 ARTIKELNUMMER                           
002200        05 MID-KDPRIBEH      PIC X.                                       
002300*                                 PRISBEHANDLINGSKOD                      
002400*                                  B = BORTTAGSMARKERAD. BEH EJ           
002500*                                  J = UPPDATERAS DIREKT                  
002600*                                  N = BEHANDLAS EJ. EJ KONTROLL.         
002700*                                  V = BEHANDLAS I VECKOKÖRNINGEN         
002800*** END OF VILMAII-COPY LENGTH= 154 BYTES                                 
