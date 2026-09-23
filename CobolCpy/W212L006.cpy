000100 01  W212L006.                                                            
000200*                                 LÄNKAREA VID CALL MOT IMSMODUL          
000300*                                 I W21202.                               
000400*                                                                         
000500*                                 FÖR UPPDATERING OCH NYUPPLÄGG           
000600*                                 AV W212AA92                             
000700*                                                                         
000800*                                                                         
000900     03 KDCALL               PIC S9(3)           COMP-3.                  
001000      88 NYUPPL-LEVPLAN-INFO VALUE +12.                                   
001100      88 UPPDAT-LEVPLAN-INFO VALUE +13.                                   
001200     03 NYCKLAR.                                                          
001300        05 IDARTNR           PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500        05 IDLEVNR           PIC X(5).                                    
001600*                                 LEVERANTÖRNUMMER                        
001700     03 IO-AREA.                                                          
001800        05 KVBR              PIC S9(7)           COMP-3.                  
001900*                                 BESTÄLLNINGSREST                        
002000        05 TILEVPL           PIC S9(7)           COMP-3.                  
002100*                                 LEVERANSPLANEDATUM  (ÅÅMMDD)            
002200*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
