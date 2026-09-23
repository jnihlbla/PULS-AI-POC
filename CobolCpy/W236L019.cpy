000100 01  W236L019.                                                            
000200*                                 HÄMTAR LEVERANSBESKED KOMM.             
000300*                                 MELLAN W2362000 W2362010                
000400     03 KDCALL               PIC S9(3)           COMP-3.                  
000500      88 LAS-FRAM-LEVBSK     VALUE +109.                                  
000600     03 NYCKLAR.                                                          
000700        05 IDLEVNR           PIC S9(5)           COMP-3.                  
000800*                                 LEVERANTÖRNUMMER                        
000900        05 IDARTNR           PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*** END COPY W236L019C0  LENGTH=10                                        
