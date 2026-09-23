000100 01  MID-W6I10301.                                                        
000200*                                 MID-COPYTEXT FÖR W60103                 
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDC-UT          PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 MID-ADINLOMR-PRT     PIC X(4).                                    
000800*                                 PRINTERPLACERING                        
000900     03 MID-INPUT.                                                        
001000*                                 INDATA-RADER FÖR UPPDATERING            
001100        05 MID-IDLEVNR-KOLLI OCCURS 12 TIMES                              
001200                             PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER KOLLI                  
001400        05 MID-IDOKOLLI      OCCURS 12 TIMES                              
001500                             PIC X(9).                                    
001600*                                 ODETTE KOLLINUMMER                      
