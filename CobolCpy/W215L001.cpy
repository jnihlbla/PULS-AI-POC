000100 01  W215L001.                                                            
000200*                                 LÄNKAREA I W215 MOT SATSTRUKTUR         
000300*                                 SATSNR                                  
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-SATS-INFO      VALUE +1.                                    
000700      88 BORTTAG-HEL-SATS    VALUE +2.                                    
000800     03 FLJANEJ-ANROP        PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 IDARTNR-SATS         PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER FÖR SATS                  
001400     03 IOAREA.                                                           
001500*                                                                         
001600        05 TIBORT            PIC S9(5)           COMP-3.                  
001700*                                 BORTTAGSDATUM  (ÅÅVV)                   
001800        05 FLALTERS          PIC X.                                       
001900*                                 ALTERNATIVT ERSATT ?                    
002000        05 IDSATS            PIC S9              COMP-3.                  
002100*                                 SATSTYP                                 
002200*** END COPY W215L001C0  LENGTH=13                                        
