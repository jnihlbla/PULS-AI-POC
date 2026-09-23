000100 01  W221L502.                                                            
000200*                                 LÄNKAREA I W221 MOT                     
000300*                                 LEVERANSPLANEN                          
000400*                                                                         
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3.                  
000700      88 LAES-AVROP-FIRST    VALUE +521.                                  
000800      88 LAES-AVROP-NEXT     VALUE +522.                                  
000900      88 BORTTAG-ALLA-FOERSLAG                                            
001000                             VALUE +523.                                  
001100      88 NYUPPL-AVROP        VALUE +524.                                  
001200      88 NYUPPL-DAG-AVROP    VALUE +525.                                  
001300*                                 ANROPSTYP       KDCALL-W221-002         
001400     03 FLJANEJ-ANROP        PIC X.                                       
001500      88 ANROP-OK            VALUE 'J'.                                   
001600      88 ANROP-FEL           VALUE 'N'.                                   
001700      88 SEGMENT-SAKNAS      VALUE 'S'.                                   
001800*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001900     03 IDARTNR              PIC S9(9)           COMP-3.                  
002000*                                 ARTIKELNUMMER                           
002100     03 IDLEVNR              PIC X(5).                                    
002200*                                 LEVERANTÖRNUMMER                        
002300     03 KDAVROP              PIC S9              COMP-3.                  
002400*                                 AVROPSKOD                               
002500     03 IOAREA.                                                           
002600*                                                                         
002700        05 TIAVROP-AVS       PIC S9(5)           COMP-3.                  
002800*                                 AVSÄNDNINGSVECKA (PLANERAD)             
002900*                                 (ÅÅVV)                                  
003000        05 TIAVROP-INL       PIC S9(5)           COMP-3.                  
003100*                                 INLEVERANSDATUM (PLANERAD)              
003200*                                 (ÅÅVV)                                  
003300        05 TIAVROP-DISP      PIC S9(5)           COMP-3.                  
003400*                                 DISPONIBELVECKA  (PLANERAD)             
003500*                                 (ÅÅVV)                                  
003600        05 KVAVROP           PIC S9(7)           COMP-3.                  
003700*                                 AVROPSKVANTITET                         
003800     03 IOAREA-DAG.                                                       
003900*                                                                         
004000        05 TIAVRDAT-INL      PIC S9(7)           COMP-3.                  
004100*                                 PLANERAT INLEVERANSDATUM                
004200        05 TIAVRDAT-DISP     PIC S9(7)           COMP-3.                  
004300*                                 PLANERAT DISPONIBLEDATUM                
004400        05 TILEVDAG          PIC S9              COMP-3.                  
004500*                                 AVSÄNDNINGSDAG INOM VECKA               
004600*** END OF VILMAII-COPY LENGTH= 36 BYTES                                  
