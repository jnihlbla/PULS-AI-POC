000100 01  NBES-WDK725.                                                         
000200*                                 BESTÄLLNINGSINFORMATION                 
000300*                                 GÄLLER NDC:ER                           
000400*                                 SÖKBEGREPP IDBEST                       
000500     03 NBES-IDBEST          PIC S9(13)          COMP-3.                  
000600*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
000700*                                 PPP   = (PREFIX) INKÖPARNR              
000800*                                 BBBBBB= BESTÄLLARNR                     
000900*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
001000     03 NBES-IDLEVNR-BEST    PIC X(5).                                    
001100*                                 LEVERANTÖR ENL. BESTÄLLNING             
001200     03 NBES-KDBEH-BEST      PIC S9              COMP-3.                  
001300*                                 BEHANDLINGSKOD BESTÄLLNING              
001400     03 NBES-TIBEST          PIC S9(7)           COMP-3.                  
001500*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
001600*** END OF VILMAII-COPY LENGTH= 17 BYTES                                  
