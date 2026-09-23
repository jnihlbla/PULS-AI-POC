000100 01  SAVT-WDK723.                                                         
000200*                                 AVTALSINFORMATION FÖR NDC:ER            
000300*                                 FYSISK NYCKEL: WDK723KY                 
000400*                                 (IDAVTAL + IDLEVNR-AVT)                 
000500     03 SAVT-IDAVTAL         PIC S9(13)          COMP-3.                  
000600*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
000700*                                 PPP   = INKÖPARNR (PREFIX)              
000800*                                 BBBBB = BESTÄLLARNR                     
000900*                                 SSS   = SUFFIX                          
001000     03 SAVT-IDLEVNR-AVT     PIC X(5).                                    
001100*                                 LEVERANTÖR ENLIGT AVTAL                 
001200     03 SAVT-IDLEVNR-SHIP    PIC X(5).                                    
001300*                                 SKEPPANDE LEVERANTÖR                    
001400*                                 SHIPPING SUPPLIER                       
001500     03 SAVT-TIAVTAL         PIC S9(7)           COMP-3.                  
001600*                                 AVTALSDATUM  (ÅÅMMDD)                   
001700*** END OF VILMAII-COPY LENGTH= 21 BYTES                                  
