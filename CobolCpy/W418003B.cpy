000100 01  W418003B-CTX.                                                        
000200*                                 TEXT INFO TO MQ VIA VCOM                
000300     03 W418003A-CTX.                                                     
000400*                                 TEXTINFO TILL VIPS VIA VCOM             
000500        05 IDPTYP            PIC X(3).                                    
000600*                                 POSTTYP                                 
000700        05 IDDISTR           PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100        05 IDRAPPNR          PIC 9(7).                                    
001200*                                 RAPPORT NUMMER                          
001300        05 IDRADNR           PIC S9(5)           COMP-3.                  
001400*                                 RADNUMMER                               
001500        05 TEANMNOT-DLR      OCCURS 3 TIMES                               
001600                             PIC X(70).                                   
001700*                                 FRI TEXT FRÅN ADM. TILL DEALER          
001800     03 IDLANDX2             PIC X(2).                                    
001900*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
002000*** END OF VILMAII-COPY LENGTH= 232 BYTES                                 
