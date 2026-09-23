000100 01  AVT-W01163.                                                          
000200*                                 UTDRAG UR WDK623                        
000300     03 AVT-IDARTNR          PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500*                                 PART NUMBER                             
000600     03 AVT-IDAVTAL          PIC S9(13)          COMP-3.                  
000700*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
000800*                                 PPP   = INKÖPARNR (PREFIX)              
000900*                                 BBBBB = BESTÄLLARNR                     
001000*                                 SSS   = SUFFIX                          
001100     03 AVT-IDLEVNR-AVT      PIC X(5).                                    
001200*                                 LEVERANTÖR ENLIGT AVTAL                 
001300     03 AVT-KDBEH-AVT        PIC S9              COMP-3.                  
001400*                                 BEHANDLINGSKOD AVTAL                    
001500     03 AVT-KVAVTANT         PIC S9(7)           COMP-3.                  
001600*                                 ÅRSANTAL AVTAL                          
001700     03 AVT-TIAVTAL          PIC S9(7)           COMP-3.                  
001800*                                 AVTALSDATUM  (ÅÅMMDD)                   
001900*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
