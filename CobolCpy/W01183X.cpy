000100 01  SAVT-W01183X.                                                        
000200*                                 WDK723 EXTRACT IN READABLE FORM         
000300*                                 AT                                      
000400     03 SAVT-IDARTNR         PIC Z(7)9.                                   
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 SAVT-IDDC            PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900*                                 WAREHOUSE IDENTIFIER                    
001000     03 SAVT-IDAVTAL         PIC Z(11)9.                                  
001100*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
001200*                                 PPP   = INKÖPARNR (PREFIX)              
001300*                                 BBBBB = BESTÄLLARNR                     
001400*                                 SSS   = SUFFIX                          
001500     03 SAVT-IDLEVNR-AVT     PIC X(5).                                    
001600*                                 LEVERANTÖR ENLIGT AVTAL                 
001700     03 SAVT-IDLEVNR-SHIP    PIC X(5).                                    
001800*                                 SKEPPANDE LEVERANTÖR                    
001900*                                 SHIPPING SUPPLIER                       
002000     03 SAVT-TIAVTAL         PIC 9(6).                                    
002100*                                 AVTALSDATUM  (ÅÅMMDD)                   
002200*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
