000100 01  MID-W0I60801.                                                        
000200*                                 MID-COPYTEXT FÖR W0060800               
000300     03 MID-W0I60801-001     OCCURS 2 TIMES.                              
000400*                                 MID-COPYTEXT FÖR W0060800               
000500        05 MID-KDCMD         PIC X.                                       
000600         88 MID-KDCMD-INGENTING                                           
000700                             VALUE ' '.                                   
000800         88 MID-KDCMD-DELETE VALUE 'D'                                    
000900                             'B'.                                         
001000         88 MID-KDCMD-REPLACE                                             
001100                             VALUE 'R'                                    
001200                             'Ä'.                                         
001300         88 MID-KDCMD-INSERT VALUE 'I'                                    
001400                             'N'.                                         
001500*                                 RAD-UPPDATERINGSKOMMANDO                
001600*** END COPY W0I60801C0  LENGTH=2                                         
