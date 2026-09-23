000100 01  W221L641.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W23320 MOT LEVERANTÖRSREGISTER          
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3                   
000600                             VALUE ZEROS.                                 
000700*                                 ANROPSTYP                               
000800     03 KDCALL-VARDEN.                                                    
000900        05 LAS-LEVERANSPLAN  PIC S9(3)           COMP-3                   
001000                             VALUE +101.                                  
001100        05 LAS-BESTALLNINGSREST                                           
001200                             PIC S9(3)           COMP-3                   
001300                             VALUE +102.                                  
001400        05 LAS-AVROP         PIC S9(3)           COMP-3                   
001500                             VALUE +103.                                  
001600     03 FL-ANROP-OK          PIC X                                        
001700                             VALUE SPACE.                                 
001800      88 ANROP-OK            VALUE 'J'.                                   
001900      88 ANROP-FEL           VALUE 'N'.                                   
002000*                                 JA/NEJ-FLAGGA FÖR R2XX                  
002100     03 IDARTNR              PIC S9(9)           COMP-3                   
002200                             VALUE ZEROS.                                 
002300*                                 ARTIKELNUMMER                           
002400     03 IDLEVNR              PIC X(5)                                     
002500                             VALUE SPACES.                                
002600*                                 LEVERANTÖRNUMMER                        
002700     03 IOAREA.                                                           
002800*                                                                         
002900        05 KVBR              PIC S9(7)           COMP-3                   
003000                             VALUE ZEROS.                                 
003100*                                 BESTÄLLNINGSREST                        
003200        05 TIAVROP-DISP      PIC S9(5)           COMP-3                   
003300                             VALUE ZEROS.                                 
003400*                                 DISPONIBELVECKA  (PLANERAD)             
003500*                                 (ÅÅVV)                                  
003600        05 KVAVROP           PIC S9(7)           COMP-3                   
003700                             VALUE ZEROS.                                 
003800*                                 AVROPSKVANTITET                         
003900*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
