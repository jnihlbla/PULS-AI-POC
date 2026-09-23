000100 01  W418003A-CTX.                                                        
000200*                                 TEXTINFO TILL VIPS VIA VCOM             
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDDISTR              PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900     03 IDRAPPNR             PIC 9(7).                                    
001000*                                 RAPPORT NUMMER                          
001100     03 IDRADNR              PIC S9(5)           COMP-3.                  
001200*                                 RADNUMMER                               
001300     03 TEANMNOT-DLR         OCCURS 3 TIMES                               
001400                             PIC X(70).                                   
001500*                                 FRI TEXT FRÅN ADM. TILL DEALER          
001600*** END OF VILMAII-COPY LENGTH= 230 BYTES                                 
