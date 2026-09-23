000100 01  W222L224.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W22222 MOT ARTIKELREG                   
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-SATS-BEHOV     VALUE +401.                                  
000700*                                                                         
000800     03 FLJANEJ-ANROP        PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 IDARTNR              PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400     03 IOAREA.                                                           
001500*                                                                         
001600        05 KVBEHOV-TOTSATS   PIC S9(7)V9(2)      COMP-3.                  
001700*                                 TOTALT SATSBEHOV                        
001800        05 TIBEHOV-SATS      PIC S9(5)           COMP-3.                  
001900*                                 BEHOVSDATUM FÖR SATS  (ÅÅVV)            
002000*** END COPY W222L224C0  LENGTH=16                                        
