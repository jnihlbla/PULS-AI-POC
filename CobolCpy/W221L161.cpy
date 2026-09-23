000100 01  W221L161.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22116 MOT ARTIKELREGISTER              
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAS-ARTINFO         VALUE +1.                                    
000700      88 REPL-ARTINFO        VALUE +3.                                    
000800*                                 ANROPSTYP     KDCALL-W221               
000900     03 FLJANEJ-ANROP        PIC X.                                       
001000      88 ANROP-OK            VALUE 'J'.                                   
001100      88 ANROP-FEL           VALUE 'N'.                                   
001200*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001300     03 IDARTNR              PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500     03 IOAREA-ARTIKELDATA.                                               
001600*                                                                         
001700        05 IDLEVNR           PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900        05 KDHF              PIC S9              COMP-3.                  
002000*                                 HUVUDFÖRRÅDSMÄRKNING                    
002100        05 BEFT              PIC S9(3)           COMP-3.                  
002200*                                 FÖRPACKNINGSTYP                         
002300        05 FILLER            PIC X(3).                                    
002400        05 IDINK             PIC X(4).                                    
002500*                                 INKÖPARNUMMER                           
002600        05 KDAVT             PIC S9              COMP-3.                  
002700*                                 AVTALSMÄRKNING                          
002800        05 KVDAGAR-TT        PIC S9(3)           COMP-3.                  
002900*                                 DAGAR TULL- OCH TRANSPORT-TID           
003000        05 KVDAGAR-INLEV     PIC S9(3)           COMP-3.                  
003100*                                 INLEVERANSTID     (ANTAL DAGAR)         
003200        05 KVDAGAR-FFH       PIC S9(3)           COMP-3.                  
003300*                                 FRAMFÖRHÅLLNING   (ANTAL DAGAR)         
003400        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
003500*                                 ANTAL VECKOR LEDTID                     
003600        05 KVVECKOR-AT       PIC S9(3)           COMP-3.                  
003700*                                 ANTAL VECKOR ANSKAFFNINGSTID            
003800        05 KVVECKOR-FT       PIC S9(3)           COMP-3.                  
003900*                                 ANTAL VECKOR FRYSNINGSTID               
004000        05 KVVECKOR-BT       PIC S9(3)           COMP-3.                  
004100*                                 ANTAL VECKOR BESTÄLLNINGSTID            
004200*** END OF VILMAII-COPY LENGTH= 38 BYTES                                  
