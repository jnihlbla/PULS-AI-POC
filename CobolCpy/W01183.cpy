000100 01  SAVT-W01183.                                                         
000200*                                 UTDRAG UR WDK723                        
000300     03 SAVT-IDARTNR         PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500*                                 PART NUMBER                             
000600     03 SAVT-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 SAVT-IDAVTAL         PIC S9(13)          COMP-3.                  
001000*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
001100*                                 PPP   = INKÖPARNR (PREFIX)              
001200*                                 BBBBB = BESTÄLLARNR                     
001300*                                 SSS   = SUFFIX                          
001400     03 SAVT-IDLEVNR-AVT     PIC X(5).                                    
001500*                                 LEVERANTÖR ENLIGT AVTAL                 
001600     03 SAVT-IDLEVNR-SHIP    PIC X(5).                                    
001700*                                 SKEPPANDE LEVERANTÖR                    
001800*                                 SHIPPING SUPPLIER                       
001900     03 SAVT-TIAVTAL         PIC S9(7)           COMP-3.                  
002000*                                 AVTALSDATUM  (ÅÅMMDD)                   
002100*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
