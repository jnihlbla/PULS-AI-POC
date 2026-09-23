000100 01  W222L225.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22222 MOT ART.REG ORDER ENTRY          
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-TPO-BEHOV      VALUE +501.                                  
000700      88 LAES-TPO-BEHOV-NEXT VALUE +502.                                  
000800*                                                                         
000900     03 FLJANEJ-ANROP        PIC X.                                       
001000      88 ANROP-OK            VALUE 'J'.                                   
001100      88 ANROP-FEL           VALUE 'N'.                                   
001200*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001300     03 IDARTNR              PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500     03 IOAREA.                                                           
001600*                                                                         
001700        05 TIBEHOV           PIC S9(5)           COMP-3.                  
001800*                                 BEHOVSVECKA           (ÅÅVV)            
001900        05 KVBEHOV-VECKA     PIC S9(7)V9(2)      COMP-3.                  
002000*                                 BEHOV PER VECKA                         
