000010*** EDIT ALLOWED                                                          
000100*                            *************************************        
000200*                            *** ANVÄNDS VID TEST AV:                     
000300*                            ***    TIDSGRÄNSER INOM                      
000400*                            ***    KREDITERINGEN                         
000500*                            ***                                          
000600*                            *************************************        
000700 01  DIS104-IDDISTR                 PIC 9(5)     COMP-3.                  
000800*                                                                         
000900     88  DIS104-60-DAGAR     VALUE 1 THRU 1499                            
001000                                   1600 THRU 2399.                        
001350*                                                                         
001360     88  DIS104-90-DAGAR     VALUE 1500 THRU 1599                         
001370                                   2400 THRU 9999.                        
002100*                                                                         
002200*** END COPY WWDIS104C0  LENGTH=3     OLD LENGTH=                         
