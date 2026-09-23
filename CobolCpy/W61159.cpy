000100 01  W61159.                                                              
000200*                                 LÄNKAREA FÖR W61159                     
000300*                                                                         
000400     03 INDATAFLT.                                                        
000500        05 IDARTNR           PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700        05 IDDC              PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900        05 TIAAVV-AKTUELL    PIC S9(5)           COMP-3.                  
001000*                                 KÖRNINGSVECKA FÖR NÄSTA                 
001100*                                 VECKOKÖRNING (AAVV)                     
001200        05 TID-AKTUELL       PIC S9              COMP-3.                  
001300*                                 DAGNUMMER I VECKA (MÅNDAG = 1)          
001400        05 TIBEHOV-START     PIC S9(5)           COMP-3.                  
001500*                                 BEHOVSVECKA           (ÅÅVV)            
001600        05 KVVECKOR-BEHOV    PIC S9(3)           COMP-3.                  
001700*                                 ANTAL BEHOVSVECKOR                      
001800     03 RESULTATFLT.                                                      
001900        05 FLJANEJ-ANROP     PIC X.                                       
002000         88 ANROP-OK         VALUE 'J'.                                   
002100         88 ANROP-FEL        VALUE 'N'.                                   
002200*                                 JA/NEJ-FLAGGA FÖR R2XX                  
002300        05 KVBEHOV-DESSUTOM  PIC S9(7)V9(2)      COMP-3.                  
002400*                                 SUMMA SATSBEHOV INNAN BEGÄRD            
002500*                                 VECKA (SLÄPANDE SATSBEHOV)              
002600*                                                                         
002700        05 KVBEHOV-VECKA     OCCURS 70 TIMES                              
002800                             PIC S9(7)V9(2)      COMP-3.                  
002900*                                 BEHOV PER VECKA                         
003000        05 TIAAVVD-BEHOV     OCCURS 70 TIMES                              
003100                             PIC S9(5)           COMP-3.                  
003200*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
003300*** END OF VILMAII-COPY LENGTH= 582 BYTES                                 
