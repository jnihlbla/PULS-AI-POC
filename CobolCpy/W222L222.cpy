000100 01  W222L222.                                                            
000200*                                 LÄNKAREA FÖR BEHOVSMODULEN              
000300*                                 W22222 (USING W222L222).                
000400*                                                                         
000500     03 INDATAFLT.                                                        
000600        05 IDARTNR           PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800        05 IDDC              PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000        05 TIAAVV-AKTUELL    PIC S9(5)           COMP-3.                  
001100*                                 KÖRNINGSVECKA FÖR NÄSTA                 
001200*                                 VECKOKÖRNING (AAVV)                     
001300        05 TID-AKTUELL       PIC S9              COMP-3.                  
001400*                                 DAGNUMMER I VECKA (MÅNDAG = 1)          
001500        05 TIBEHOV-START     PIC S9(5)           COMP-3.                  
001600*                                 BEHOVSVECKA           (ÅÅVV)            
001700        05 KVVECKOR-BEHOV    PIC S9(3)           COMP-3.                  
001800*                                 ANTAL BEHOVSVECKOR                      
001900        05 KDBEHOV           PIC X(2).                                    
002000         88 ENDAST-SEPARATBEHOV                                           
002100                             VALUE '01'.                                  
002200         88 ENDAST-SATSBEHOV VALUE '02'.                                  
002300         88 SEP-SATS-BEHOV   VALUE '03'.                                  
002400         88 ENDAST-LEVBEHOV  VALUE '04'.                                  
002500         88 SEP-DO-BEHOV     VALUE '05'.                                  
002600         88 SATS-DO-BEHOV    VALUE '06'.                                  
002700         88 SEP-SATS-DO-BEHOV                                             
002800                             VALUE '07'.                                  
002900         88 SATS-TPO-LEVBEHOV                                             
003000                             VALUE '08'.                                  
003100         88 SEP-SATS-TPO-LEVBEHOV                                         
003200                             VALUE '09'.                                  
003300         88 ENDAST-SDCBEHOV  VALUE '10'.                                  
003400         88 SATS-TPO-SDCBEHOV                                             
003500                             VALUE '11'.                                  
003600         88 SEP-SATS-TPO-SDCBEHOV                                         
003700                             VALUE '12'.                                  
003800         88 SATS-TPO-LEV-SDCBEHOV                                         
003900                             VALUE '13'.                                  
004000         88 SEP-SATS-TPO-LEV-SDCBEHOV                                     
004100                             VALUE '14'.                                  
004200         88 ENDAST-NDCBEHOV  VALUE '15'.                                  
004300         88 SATS-TPO-SDC-NDC VALUE '16'.                                  
004400         88 SEP-SATS-TPO-SDC-NDC                                          
004500                             VALUE '17'.                                  
004600         88 SATS-TPO-LEV-SDC-NDC                                          
004700                             VALUE '18'.                                  
004800         88 SEP-SATS-TPO-LEV-SDC-NDC                                      
004900                             VALUE '19'.                                  
005000         88 PB-TOTAL         VALUE '20'.                                  
005100         88 SEP-TPO-SDC-NDC  VALUE '21'.                                  
005200*                                 BEHOVSKOD                               
005300        05 FLINKLDIRLEV      PIC X.                                       
005400     03 RESULTATFLT.                                                      
005500        05 FLJANEJ-ANROP     PIC X.                                       
005600         88 ANROP-OK         VALUE 'J'.                                   
005700         88 ANROP-FEL        VALUE 'N'.                                   
005800*                                 JA/NEJ-FLAGGA FÖR R2XX                  
005900        05 KVBEHOV-SUMMA     PIC S9(7)V9(2)      COMP-3.                  
006000*                                 SUMMAN AV BEGÄRDA BEHOV                 
006100        05 KVBEHOV-DESSUTOM  PIC S9(7)V9(2)      COMP-3.                  
006200*                                 SUMMA SATSBEHOV INNAN BEGÄRD            
006300*                                 VECKA (SLÄPANDE SATSBEHOV)              
006400*                                                                         
006500        05 TIBEHOV-FIRST     PIC S9(5)           COMP-3.                  
006600*                                 FÖRSTA TPO-BEHOVSDAT (ÅÅVV)             
006700        05 KVBEHOV-VECKA     OCCURS 156 TIMES                             
006800                             PIC S9(7)V9(2)      COMP-3.                  
006900*                                 BEHOV PER VECKA                         
007000*** END OF VILMAII-COPY LENGTH= 813 BYTES                                 
