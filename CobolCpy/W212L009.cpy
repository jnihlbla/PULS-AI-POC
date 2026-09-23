000100 01  W212L009.                                                            
000200*                                 LÄNKAREA VID CALL MOT IMSMODUL          
000300*                                 I W21202.  UPPDATERAR                   
000400*                                 HÄNDELSEREGISTRET                       
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3.                  
000700      88 NYUPPL-BYTE-HLEV    VALUE +19.                                   
000800      88 NYUPPL-OMSPEC-LEVPL VALUE +20.                                   
000900      88 NYUPPL-OMRAKN-TID   VALUE +21.                                   
001000     03 NYCKLAR.                                                          
001100        05 IDARTNR           PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300        05 IDLEVNR           PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 IO-AREA.                                                          
001600        05 KDLPORS           PIC S9(3)           COMP-3.                  
001700*                                 LEVERANSPLANEORSAK                      
001800*** END OF VILMAII-COPY LENGTH= 14 BYTES                                  
