000100 01  W231L005.                                                            
000200*                                 LÄNKAREA FÖR LÄSNING AV LEVINFO         
000300*                                 PÅ LEVERANSPLANEREG                     
000400     03 KDCALL               PIC S9(3)           COMP-3.                  
000500      88 LAES-LEVINFO        VALUE +5.                                    
000600     03 FLJANEJ-LEVINFO      PIC X.                                       
000700*                                 JA/NEJ-FLAGGA                           
000800     03 IO-AREA.                                                          
000900        05 KVBR              PIC S9(7)           COMP-3.                  
001000*                                 BESTÄLLNINGSREST                        
001100*** END COPY W231L005C0  LENGTH=7                                         
