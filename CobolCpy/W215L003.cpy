000100 01  W215L003.                                                            
000200*                                 LÄNKAREA I W215 MOT ARTIKELREG          
000300*                                 OCH SATSENS ARTIKELNR                   
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-ARTREG-SATS    VALUE +8.                                    
000700      88 UPPDATERA-ARTREG-SATS                                            
000800                             VALUE +9.                                    
000900      88 KONTROLLERA-FLIART  VALUE +20.                                   
001000     03 FLJANEJ-ANROP        PIC X.                                       
001100      88 ANROP-OK            VALUE 'J'.                                   
001200      88 ANROP-FEL           VALUE 'N'.                                   
001300*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001400     03 IDARTNR-SATS         PIC S9(9)           COMP-3.                  
001500*                                 ARTIKELNUMMER FÖR SATS                  
001600     03 IDARTNR-ING          PIC S9(9)           COMP-3.                  
001700*                                 INGÅENDE ARTIKELNUMMER                  
001800     03 TIAAMMDD             PIC S9(7)           COMP-3.                  
001900*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
002000     03 IOAREA.                                                           
002100*                                                                         
002200        05 KDLPSP            PIC S9              COMP-3.                  
002300*                                 LEVERANSPLANESPÄRR                      
002400        05 TIOMSPEC          PIC S9(5)           COMP-3.                  
002500*                                 OMSPECIFIKATIONSDATUM  (ÅÅVV)           
002600        05 IDANSK            PIC S9(3)           COMP-3.                  
002700*                                 ANSKAFFARNUMMER                         
002800        05 FLIART            PIC X.                                       
002900*                                 INGÅENDE ARTIKEL                        
003000        05 IDFTG             PIC 9(2).                                    
003100*                                 FÖRETAGSID EKONOM REDOVISNING           
003200*** END COPY W215L003C0  LENGTH=26                                        
