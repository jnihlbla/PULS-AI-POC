000100 01  W215L004.                                                            
000200*                                 LÄNKAREA I W215 MOT SATSENS             
000300*                                 LEVPLAN                                 
000400*                                                                         
000500*                                                                         
000600     03 KDCALL               PIC S9(3)           COMP-3.                  
000700      88 LAES-LEVPLAN-DATUM  VALUE +10.                                   
000800      88 BORTTAG-LEVPLAN-DATUM                                            
000900                             VALUE +11.                                   
001000     03 FLJANEJ-ANROP        PIC X.                                       
001100      88 ANROP-OK            VALUE 'J'.                                   
001200      88 ANROP-FEL           VALUE 'N'.                                   
001300*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001400     03 IDARTNR-SATS         PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER FÖR SATS                  
001600     03 IDLEVNR              PIC S9(5)           COMP-3.                  
001700*                                 LEVERANTÖRNUMMER                        
001800     03 IOAREA.                                                           
001900*                                                                         
002000        05 TISPECST          PIC S9(5)           COMP-3.                  
002100*                                 SPECAD FR.O.M DATUM   (ÅÅVV)            
002200*** END COPY W215L004C0  LENGTH=14                                        
