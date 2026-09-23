000100 01  W261L005.                                                            
000200*                                 LÄNKAREA FÖR CALL MOT                   
000300*                                 LEVPLANEREG (WDD9) BENÄMN  REG          
000400*                                 (WDD3)                                  
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-ART-INFO        VALUE +5.                                    
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 FLJANEJ-ANROP        PIC X.                                       
001000*                                 JA/NEJ-FLAGGA                           
001100     03 IO-AREA.                                                          
001200        05 KVBR              PIC S9(7)           COMP-3.                  
001300*                                 BESTÄLLNINGSREST                        
001400        05 TEXT-BEART        PIC X(25).                                   
001500*                                 ARTIKELBENÄMNING                        
001600        05 KVBUFF            PIC S9(7)           COMP-3.                  
001700*                                 FÖRÄDLAT BUFFERSALDO                    
001800*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
