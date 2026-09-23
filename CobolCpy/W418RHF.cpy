000100 01  W418RHF.                                                             
000200*                                 RHF LEVERANSANMÄRKNING  TEXT            
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 SORTAREA             PIC X(25).                                   
000700     03 FILLER REDEFINES SORTAREA.                                        
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 KDCLAGER          PIC S9              COMP-3.                  
001300*                                 CENTRALLAGERKOD                         
001400        05 IDLEVANM          PIC X(7).                                    
001500*                                 LEVERANSANMÄRKNINGSNUMMER               
001600        05 FILLER            PIC X(10).                                   
001700     03 TEKRENOT             PIC X(59).                                   
001800*                                 KREDITERINGSNOTERING                    
001900*** END COPY W418RHFCC0  LENGTH=87                                        
