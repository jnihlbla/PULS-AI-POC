000100 01  W9I13201.                                                            
000200*                                 COPYTEXT F÷R MID                        
000300*                                 W9I13201                                
000400     03 IDDISTR-IN           PIC X(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 IDDISTR-UT           PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 IDARTNR-IN           PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 IDARTNR-UT           PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 IDARTNR-FIRST        PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 TIREGDAT-FIRST       PIC X(6).                                    
001500*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001600     03 KDOBJEKT-FIRST       PIC X.                                       
001700*                                 OBJEKTSKOD                              
001800     03 IDORDNR-KEY-FIRST    PIC X(5).                                    
001900*                                 ORDERNUMMER                             
002000     03 IDARTNR-LAST         PIC X(9).                                    
002100*                                 ARTIKELNUMMER                           
002200     03 TIREGDAT-LAST        PIC X(6).                                    
002300*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002400     03 KDOBJEKT-LAST        PIC X.                                       
002500*                                 OBJEKTSKOD                              
002600     03 IDORDNR-KEY-LAST     PIC X(5).                                    
002700*                                 ORDERNUMMER                             
002800*** END COPY W9I13201C0  LENGTH=68                                        
