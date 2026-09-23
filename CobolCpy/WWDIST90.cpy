000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS VID TEST AV:                     
000300*                            ***  - ÖSTERRIKE DISTRIKT I                  
000400*                            ***    FAKTURERINGEN:EFTER                   
000500*                            ***                                          
000600*                            ************************************         
000700                                                                          
000800 01  DIST90-IDDISTR          PIC 9(5)     COMP-3.                         
000900*                                                                         
001000     88  DIST90-AUSTRIA          VALUE   2370 THRU 2378.                  
001100*                                                                         
001000     88  DIST90-AUSTRIA-BALKAN   VALUE   2370 THRU 2376.                  
001100*                                                                         
001110     88  DIST90-AUSTRIA-EGEN     VALUE   2378.                            
001200*                                                                         
001300*** END COPY WWDIST90C0  LENGTH=0     OLD LENGTH=                         
