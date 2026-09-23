000100 01  W222L230.                                                            
000200*                                 LÄNKAREA FÖR XXXXX                      
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
001800        05 KDBEHOV           PIC X(2).                                    
001900         88 ENDAST-SEPARATBEHOV                                           
002000                             VALUE '01'.                                  
002100         88 ENDAST-SATSBEHOV VALUE '02'.                                  
002200         88 SEP-SATS-BEHOV   VALUE '03'.                                  
002300         88 ENDAST-LEVBEHOV  VALUE '04'.                                  
002400         88 SEP-DO-BEHOV     VALUE '05'.                                  
002500         88 SATS-DO-BEHOV    VALUE '06'.                                  
002600         88 SEP-SATS-DO-BEHOV                                             
002700                             VALUE '07'.                                  
002800         88 SATS-TPO-LEVBEHOV                                             
002900                             VALUE '08'.                                  
003000         88 SEP-SATS-TPO-LEVBEHOV                                         
003100                             VALUE '09'.                                  
003200         88 ENDAST-SDCBEHOV  VALUE '10'.                                  
003300         88 SATS-TPO-SDCBEHOV                                             
003400                             VALUE '11'.                                  
003500         88 SEP-SATS-TPO-SDCBEHOV                                         
003600                             VALUE '12'.                                  
003700         88 SATS-TPO-LEV-SDCBEHOV                                         
003800                             VALUE '13'.                                  
003900         88 SEP-SATS-TPO-LEV-SDCBEHOV                                     
004000                             VALUE '14'.                                  
004100         88 ENDAST-NDCBEHOV  VALUE '15'.                                  
004200         88 SATS-TPO-SDC-NDC VALUE '16'.                                  
004300         88 SEP-SATS-TPO-SDC-NDC                                          
004400                             VALUE '17'.                                  
004500         88 SATS-TPO-LEV-SDC-NDC                                          
004600                             VALUE '18'.                                  
004700         88 SEP-SATS-TPO-LEV-SDC-NDC                                      
004800                             VALUE '19'.                                  
004900         88 PB-TOTAL         VALUE '20'.                                  
005000*                                 BEHOVSKOD                               
005100     03 RESULTATFLT.                                                      
005200        05 FLJANEJ-ANROP     PIC X.                                       
005300         88 ANROP-OK         VALUE 'J'.                                   
005400         88 ANROP-FEL        VALUE 'N'.                                   
005500*                                 JA/NEJ-FLAGGA FÖR R2XX                  
005600        05 KVBEHOV-SUMMA     PIC S9(7)V9(2)      COMP-3.                  
005700*                                 SUMMAN AV BEGÄRDA BEHOV                 
005800        05 KVBEHOV-DESSUTOM  PIC S9(7)V9(2)      COMP-3.                  
005900*                                 SUMMA SATSBEHOV INNAN BEGÄRD            
006000*                                 VECKA (SLÄPANDE SATSBEHOV)              
006100*                                                                         
006200        05 TIBEHOV-FIRST     PIC S9(5)           COMP-3.                  
006300*                                 FÖRSTA TPO-BEHOVSDAT (ÅÅVV)             
006400        05 KVBEHOV-VECKA     OCCURS 70 TIMES                              
006500                             PIC S9(7)V9(2)      COMP-3.                  
006600*                                 BEHOV PER VECKA                         
006700        05 TIAAVVD-BEHOV     OCCURS 70 TIMES                              
006800                             PIC S9(5)           COMP-3.                  
006900*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
007000*** END OF VILMAII-COPY LENGTH= 592 BYTES                                 
