000100 01  AVT-WDK623.                                                          
000200*                                 AVTALSINFORMATION                       
000300*                                 SÖKBEGREPP IDAVTAL                      
000400     03 AVT-IDAVTAL          PIC S9(13)          COMP-3.                  
000500*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
000600*                                 PPP   = INKÖPARNR (PREFIX)              
000700*                                 BBBBB = BESTÄLLARNR                     
000800*                                 SSS   = SUFFIX                          
000900     03 AVT-IDLEVNR-AVT      PIC X(5).                                    
001000*                                 LEVERANTÖR ENLIGT AVTAL                 
001100     03 AVT-IDLEVNR-SHIP     PIC X(5).                                    
001200*                                 SKEPPANDE LEVERANTÖR                    
001300*                                 SHIPPING SUPPLIER                       
001400     03 AVT-KDBEH-AVT        PIC S9              COMP-3.                  
001500*                                 BEHANDLINGSKOD AVTAL                    
001600     03 AVT-KVAVTANT         PIC S9(7)           COMP-3.                  
001700*                                 ÅRSANTAL AVTAL                          
001800     03 AVT-TIAVTAL          PIC S9(7)           COMP-3.                  
001900*                                 AVTALSDATUM  (ÅÅMMDD)                   
002000*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
