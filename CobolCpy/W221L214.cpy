000100 01  W221L214.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22121 MOT ARTIKELREGISTRET             
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 UPPLAGG-BEST-INFO   VALUE +401.                                  
000700*                                 ANROPSTYP     KDCALL-W221               
000800     03 FLJANEJ-ANROP        PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 IOAREA.                                                           
001500*                                                                         
001600        05 IDBEST            PIC S9(13)          COMP-3.                  
001700*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
001800*                                 PPP   = (PREFIX) INKÖPARNR              
001900*                                 BBBBBB= BESTÄLLARNR                     
002000*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
002100        05 IDLEVNR-BEST      PIC X(5).                                    
002200*                                 LEVERANTÖR ENL. BESTÄLLNING             
002300        05 KDBEH-BEST        PIC S9              COMP-3.                  
002400*                                 BEHANDLINGSKOD BESTÄLLNING              
002500        05 KVBEST            PIC S9(7)           COMP-3.                  
002600*                                 BESTÄLLT ANTAL                          
002700        05 KVBEST-BEKR       PIC S9(7)           COMP-3.                  
002800*                                 BESTÄLLT ANTAL                          
002900        05 TIBEST            PIC S9(7)           COMP-3.                  
003000*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
003100*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
