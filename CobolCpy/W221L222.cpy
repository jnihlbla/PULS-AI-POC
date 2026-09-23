000100 01  W221L222.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22122 MOT DATABASER                    
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-ATTENTION      VALUE +201.                                  
000700      88 LAES-BENAMNING      VALUE +202.                                  
000800*                                 ANROPSTYP           KDCALL-W221         
000900     03 FLJANEJ-ANROP        PIC X.                                       
001000      88 ANROP-OK            VALUE 'J'.                                   
001100      88 ANROP-FEL           VALUE 'N'.                                   
001200*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001300     03 IDATTENT             PIC S9(3)           COMP-3.                  
001400*                                 ATTENTION NUMMER                        
001500     03 KDSPRAK              PIC S9              COMP-3.                  
001600      88 KDSPRAK-SVENSKA     VALUE +0.                                    
001700      88 KDSPRAK-ENGELSKA    VALUE +1.                                    
001800      88 KDSPRAK-FRANSKA     VALUE +2.                                    
001900      88 KDSPRAK-SPANSKA     VALUE +3.                                    
002000      88 KDSPRAK-TYSKA       VALUE +4.                                    
002100*                                 SPRÅKKOD                                
002200     03 IOAREA.                                                           
002300*                                                                         
002400        05 ADATTENT          PIC X(40).                                   
002500*                                 ATTENTIONADRESS                         
002600        05 BEART             PIC X(25).                                   
002700*                                 ARTIKELBENÄMNING                        
002800*** END COPY W221L222C0  LENGTH=71                                        
