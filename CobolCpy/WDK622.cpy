000100 01  BEST-WDK622.                                                         
000200*                                 BESTÄLLNINGSINFORMATION                 
000300*                                 SÖKBEGREPP IDBEST                       
000400     03 BEST-IDBEST          PIC S9(13)          COMP-3.                  
000500*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
000600*                                 PPP   = (PREFIX) INKÖPARNR              
000700*                                 BBBBBB= BESTÄLLARNR                     
000800*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
000900     03 BEST-IDLEVNR-BEST    PIC X(5).                                    
001000*                                 LEVERANTÖR ENL. BESTÄLLNING             
001100     03 BEST-KDBEH-BEST      PIC S9              COMP-3.                  
001200*                                 BEHANDLINGSKOD BESTÄLLNING              
001300     03 BEST-KVBEST          PIC S9(7)           COMP-3.                  
001400*                                 BESTÄLLT ANTAL                          
001500     03 BEST-KVBEST-BEKR     PIC S9(7)           COMP-3.                  
001600*                                 BEKRÄFTAT BESTÄLLT ANTAL                
001700     03 BEST-TIBEST          PIC S9(7)           COMP-3.                  
001800*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
001900*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
