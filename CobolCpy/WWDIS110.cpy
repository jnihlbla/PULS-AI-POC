000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS VID TEST AV:                     
000300*                            ***    DISTRIKT SOM SKALL HA                 
000400*                            ***    ALLA BELOPP I FAKTURERINGEN           
000500*                            ***    UTSKRIVET I SIN EGEN VALUTA           
000501*                            ***                                          
000510*                            ***    AUD = AUSTRALIENSISKA DOLLAR          
000530*                            ***    THB = THAILÄNDSKA BATH                
000600*                            *************************************        
000700 01  DIS110-IDDISTR                 PIC 9(5)     COMP-3.                  
000800*                                                                         
000900     88  DIS110-AUD-VALUTA          VALUE 7838.                           
001100                                                                          
002400     88  DIS110-THB-VALUTA          VALUE 6225 6251.                      
