000100 01  1122-WDGX1122.                                                       
000200*                                 BASLAGER                                
000300*                                 GODK. MARKNADSSTRUKTURER                
000400*                                 FYSISK NYCKEL: WDGXKEY                  
000500*                                 (KDBASLM + LOWVALUE                     
000600     03 1122-KDBASLM         PIC X(6).                                    
000700*                                 BASLAGERMARKNAD                         
000800*                                 BASIC STOCK MARKET                      
000900     03 1122-LOWVALUE        PIC X(9).                                    
001000     03 1122-IDDISTR         OCCURS 6 TIMES                               
001100                             PIC S9(5)           COMP-3.                  
001200*                                 DISTRIKTNUMMER                          
001300*                                 DISTRICT NUMBER                         
001400     03 FILLER               PIC X(32).                                   
001500*** END COPY WDGX1122C0  LENGTH=65                                        
