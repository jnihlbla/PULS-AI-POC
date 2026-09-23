000100 01  AVT-W2120302.                                                        
000200*                                 DATA FÖR FÖRÄNDRING AV                  
000300*                                 AVTALSSEGMENT (ARTC23)                  
000400*                                 NYUPPLÄGG   = PTYP NAV                  
000500*                                 BORTTAG     = PTYP BAV                  
000600     03 AVT-IDPTYP           PIC X(3).                                    
000700*                                 POSTTYP                                 
000800     03 AVT-IDARTNR          PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 AVT-IDAVTAL          PIC S9(13)          COMP-3.                  
001100*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
001200*                                 PPP   = INKÖPARNR (PREFIX)              
001300*                                 BBBBB = BESTÄLLARNR                     
001400*                                 SSS   = SUFFIX                          
001500     03 AVT-IDLEVNR-AVT      PIC X(5).                                    
001600*                                 LEVERANTÖR ENLIGT AVTAL                 
001700     03 AVT-IDLEVNR-SHIP     PIC X(5).                                    
001800*                                 SKEPPANDE LEVERANTÖR                    
001900     03 AVT-KDBEH-AVT        PIC S9              COMP-3.                  
002000*                                 BEHANDLINGSKOD AVTAL                    
002100     03 AVT-KVAVTANT         PIC S9(7)           COMP-3.                  
002200*                                 ÅRSANTAL AVTAL                          
002300     03 AVT-TIAVTAL          PIC S9(7)           COMP-3.                  
002400*                                 AVTALSDATUM  (ÅÅMMDD)                   
002500*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
