000100 01  W221L402.                                                            
000200*                                 LÄNKAREA I W221 MOT                     
000300*                                 LEVERANSPLANEN                          
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-AVROP-FIRST    VALUE +420.                                  
000700      88 LAES-AVROP-NEXT     VALUE +421.                                  
000800      88 LAES-AVROP-LEV-F    VALUE +426.                                  
000900      88 LAES-AVROP-LEV-N    VALUE +427.                                  
001000*                                 ANROPSTYP     KDCALL-W221               
001100     03 FLJANEJ-ANROP        PIC X.                                       
001200      88 ANROP-OK            VALUE 'J'.                                   
001300      88 ANROP-FEL           VALUE 'N'.                                   
001400*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001500     03 IDARTNR              PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 IDLEVNR              PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900     03 KDAVROP              PIC S9              COMP-3.                  
002000*                                 AVROPSKOD                               
002100     03 IOAREA.                                                           
002200*                                                                         
002300        05 TIAVROP-AVS       PIC S9(5)           COMP-3.                  
002400*                                 AVSÄNDNINGSVECKA (PLANERAD)             
002500*                                 (ÅÅVV)                                  
002600        05 TIAVROP-INL       PIC S9(5)           COMP-3.                  
002700*                                 INLEVERANSDATUM (PLANERAD)              
002800*                                 (ÅÅVV)                                  
002900        05 TIAVROP-DISP      PIC S9(5)           COMP-3.                  
003000*                                 DISPONIBELVECKA  (PLANERAD)             
003100*                                 (ÅÅVV)                                  
003200        05 KVAVROP           PIC S9(7)           COMP-3.                  
003300*                                 AVROPSKVANTITET                         
003400     03 TILEVDAG             PIC S9              COMP-3.                  
003500*                                 AVSÄNDNINGSDAG INOM VECKA               
003600*** END OF VILMAII-COPY LENGTH= 28 BYTES                                  
