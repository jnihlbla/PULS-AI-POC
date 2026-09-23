000100 01  W222BHDC.                                                            
000200*                                 LÄNKAREA FÖR BEHOVSMODULEN NDC          
000300*                                 VID LOKAL ANSKAFFNING, EJ CDC.          
000400*                                 W222BHDC USING W222BHDC).               
000500*                                                                         
000600     03 INDATAFLT.                                                        
000700        05 IDARTNR           PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900        05 IDDC              PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100        05 TIAAVV-AKTUELL    PIC S9(5)           COMP-3.                  
001200*                                 KÖRNINGSVECKA FÖR NÄSTA                 
001300*                                 VECKOKÖRNING (AAVV)                     
001400        05 TID-AKTUELL       PIC S9              COMP-3.                  
001500*                                 DAGNUMMER I VECKA (MÅNDAG = 1)          
001600        05 TIBEHOV-START     PIC S9(5)           COMP-3.                  
001700*                                 BEHOVSVECKA           (ÅÅVV)            
001800        05 KVVECKOR-BEHOV    PIC S9(3)           COMP-3.                  
001900*                                 ANTAL BEHOVSVECKOR                      
002000        05 KVTILLG-TOT-CDC   PIC S9(7)           COMP-3.                  
002100*                                 LAGERTILLGÅNG CDC TOTALT                
002200        05 KDBEHOV           PIC X(2).                                    
002300         88 ENDAST-SEPARATBEHOV                                           
002400                             VALUE '01'.                                  
002500         88 ENDAST-XDCBEHOV  VALUE '02'.                                  
002600         88 PB-TOTAL-SEP-LEV-XDC                                          
002700                             VALUE '03'.                                  
002800         88 ENDAST-CDCBEHOV  VALUE '04'.                                  
002900         88 XDC-CDC-BEHOV    VALUE '05'.                                  
003000         88 ENDAST-GLOBALBEHOV                                            
003100                             VALUE '06'.                                  
003200         88 ENDAST-LOKALBEHOV                                             
003300                             VALUE '07'.                                  
003400*                                 BEHOVSKOD                               
003500     03 RESULTATFLT.                                                      
003600        05 FLJANEJ-ANROP     PIC X.                                       
003700         88 ANROP-OK         VALUE 'J'.                                   
003800         88 ANROP-FEL        VALUE 'N'.                                   
003900*                                 JA/NEJ-FLAGGA FÖR R2XX                  
004000        05 KVBEHOV-SUMMA     PIC S9(7)V9(2)      COMP-3.                  
004100*                                 SUMMAN AV BEGÄRDA BEHOV                 
004200        05 KVBEHOV-DESSUTOM  PIC S9(7)V9(2)      COMP-3.                  
004300*                                 SUMMA SATSBEHOV INNAN BEGÄRD            
004400*                                 VECKA (SLÄPANDE SATSBEHOV)              
004500*                                                                         
004600        05 TIBEHOV-FIRST     PIC S9(5)           COMP-3.                  
004700*                                 FÖRSTA TPO-BEHOVSDAT (ÅÅVV)             
004800        05 KVBEHOV-VECKA     OCCURS 156 TIMES                             
004900                             PIC S9(7)V9(2)      COMP-3.                  
005000*                                 BEHOV PER VECKA                         
005100*** END OF VILMAII-COPY LENGTH= 816 BYTES                                 
