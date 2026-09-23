000100 01  MOD-W9O20101.                                                        
000200*                                 MODCOPYTEXT TILL W90201.                
000300     03 MOD-QW90-DATA        OCCURS 80 TIMES.                             
000400*                                 GRUPP FÖR W9O20101                      
000500        05 MOD-IDDISTR       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700        05 MOD-IDKUNDNR      PIC X(6).                                    
000800*                                 KUNDNUMMER                              
000900        05 MOD-IDARTNR       PIC 9(9).                                    
001000*                                 ARTIKELNUMMER                           
001100        05 MOD-PRARTNTO      PIC 9(7)V9(2).                               
001200*                                 ARTIKELPRIS NETTO                       
001300        05 MOD-KDPRTYP       PIC X.                                       
001400*                                 TYP AV PRISTILLÄMPNING                  
001500        05 MOD-PRARTBTO-MARK PIC 9(7)V9(2).                               
001600*                                 BRUTTOPRIS PER MARKNAD (FOB)            
001700        05 MOD-IDMFSFEL      PIC X(3).                                    
001800*                                 MFS FELMEDDELANDE NUMMER                
001900*** END OF VILMAII-COPY LENGTH= 3280 BYTES                                
