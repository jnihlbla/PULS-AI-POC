000100 01  W212L002.                                                            
000200*                                 LÄNKAREA VID CALL MOT IMSMODUL          
000300*                                 I W21202.  HÄMTAR INFORMATION           
000400*                                 FRÅN WLARTC   SAMT KONTROLLERAR         
000500*                                 OM LEVNR FINNS REGISTRERAT OCH          
000600*                                 OM EVENTUELL LEVERANSPLAN               
000700*                                 FINNS.                                  
000800*                                                                         
000900     03 KDCALL               PIC S9(3)           COMP-3.                  
001000      88 HAMTA-MTRLF-INFO    VALUE +2.                                    
001100     03 FLJANEJ-LEVNR        PIC X.                                       
001200      88 LEVNR-FANNS         VALUE 'J'.                                   
001300      88 LEVNR-SAKNAS        VALUE 'N'.                                   
001400*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001500     03 FLJANEJ-LEVPLAN      PIC X.                                       
001600      88 LEVPLAN-FANNS       VALUE 'J'.                                   
001700      88 LEVPLAN-SAKNAS      VALUE 'N'.                                   
001800*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001900     03 NYCKLAR.                                                          
002000        05 IDARTNR           PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER                           
002200        05 IDLEVNR           PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400        05 FLAVRART          PIC X.                                       
002500*                                 AVROPSARTIKEL                           
002600     03 IO-AREA.                                                          
002700        05 KDAVT             PIC S9              COMP-3.                  
002800*                                 AVTALSMÄRKNING                          
002900        05 KDKSP             PIC S9              COMP-3.                  
003000*                                 KÖPSPÄRR                                
003100        05 IDANSK            PIC S9(3)           COMP-3.                  
003200*                                 ANSKAFFARNUMMER                         
003300        05 IDINK             PIC X(4).                                    
003400*                                 INKÖPARNUMMER                           
003500        05 KVBR              PIC S9(7)           COMP-3.                  
003600*                                 BESTÄLLNINGSREST                        
003700        05 IDLEVNR-REG       PIC X(5).                                    
003800*                                 LEVERANTÖRNUMMER                        
003900        05 AVTAL-IDAVTAL     PIC S9(13)          COMP-3.                  
004000*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
004100*                                 PPP   = INKÖPARNR (PREFIX)              
004200*                                 BBBBB = BESTÄLLARNR                     
004300*                                 SSS   = SUFFIX                          
004400        05 AVTAL-IDLEVNR-AVT PIC X(5).                                    
004500*                                 LEVERANTÖR ENLIGT AVTAL                 
004600        05 IDLEVNR-SHIP      PIC X(5).                                    
004700*                                 SKEPPANDE LEVERANTÖR                    
004800        05 KDPRODSL          PIC S9(3)           COMP-3.                  
004900*                                 PRODUKTSLAG                             
005000     03 PARMA-OCH-KONV       PIC X.                                       
005100*                                 JA/NEJ-FLAGGA                           
005200     03 IDLEVNR-MOTSV        PIC X(5).                                    
005300*                                 MOTSVARANDE LEVERANTÖRSID               
005400     03 KDERS                PIC S9(3)           COMP-3.                  
005500*                                 ERSÄTTNINGSKOD                          
005600*** END OF VILMAII-COPY LENGTH= 59 BYTES                                  
