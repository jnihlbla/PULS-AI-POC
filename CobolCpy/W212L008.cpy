000100 01  W212L008.                                                            
000200*                                 LÄNKAREA VID CALL MOT IMSMODUL          
000300*                                 I W21202.  UPPDATERAR W212AA30          
000400*                                                                         
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3.                  
000700      88 UPPDAT-MTRLF-INFO   VALUE +17.                                   
000800     03 NYCKLAR.                                                          
000900        05 IDARTNR           PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100        05 IDLEVNR           PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 IO-AREA.                                                          
001400        05 KDAVT             PIC S9              COMP-3.                  
001500*                                 AVTALSMÄRKNING                          
001600        05 KDKSP             PIC S9              COMP-3.                  
001700*                                 KÖPSPÄRR                                
001800        05 IDINK             PIC X(4).                                    
001900*                                 INKÖPARNUMMER                           
002000*** END OF VILMAII-COPY LENGTH= 18 BYTES                                  
