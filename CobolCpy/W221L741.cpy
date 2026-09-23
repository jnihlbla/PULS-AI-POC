000100 01  W221L741.                                                            
000200*                                 LÄNKAREA FÖR PGM W22174                 
000300*                                 UPPDATERING BEST.RESTER FÖR             
000400*                                 BYTESOBJEKT                             
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3.                  
000700      88 UPPDAT-BEST-REST    VALUE +101.                                  
000800*                                 ANROPSTYP       KDCALL-W221-002         
000900     03 NYCKLAR.                                                          
001000        05 IDARTNR           PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200        05 IDLEVNR           PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400     03 IOAREA.                                                           
001500        05 KVBR              PIC S9(7)           COMP-3.                  
001600*                                 BESTÄLLNINGSREST                        
001700        05 TILEVPL           PIC S9(7)           COMP-3.                  
001800*                                 LEVERANSPLANEDATUM  (ÅÅMMDD)            
001900*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
