000100 01  W221L211.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22121 MOT ARTIKELREGISTRET             
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-ART-INFO        VALUE +101.                                  
000700*                                 ANROPSTYP     KDCALL-W221               
000800     03 FLJANEJ-ANROP        PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 IOAREA.                                                           
001500*                                                                         
001600*                                                                         
001700        05 IDANSK            PIC S9(3)           COMP-3.                  
001800*                                 ANSKAFFARNUMMER                         
001900        05 IDLEVNR           PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100        05 IDINK             PIC X(4).                                    
002200*                                 INKÖPARNUMMER                           
002300        05 IDPROD            PIC S9(3)           COMP-3.                  
002400*                                 PRODUKTKOD, DEL AV PRODUKTSLAG          
002500        05 KDHF              PIC S9              COMP-3.                  
002600*                                 HUVUDFÖRRÅDSMÄRKNING                    
002700        05 KDLPSP            PIC S9              COMP-3.                  
002800*                                 LEVERANSPLANESPÄRR                      
002900        05 KDGK              PIC S9              COMP-3.                  
003000*                                 GODSMOTTAGAREKOD                        
003100*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
