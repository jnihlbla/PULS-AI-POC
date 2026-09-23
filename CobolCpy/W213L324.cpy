000100 01  W213L324.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W21332 MOT DC-REGISTRET WDB6            
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-DC-SEGMENT     VALUE +401.                                  
000700*                                 ANROPSTYP FÖR SYSTEM R2XX               
000800     03 FLJANEJ-ANROP        PIC X.                                       
000900      88 ANROP-OK            VALUE 'J'.                                   
001000      88 ANROP-FEL           VALUE 'N'.                                   
001100*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001200     03 IDLEVNR              PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400     03 IOAREA.                                                           
001500*                                                                         
001600        05 IDDC-REF          PIC X(2).                                    
001700*                                 SÄNDANDE LAGER FÖR REFILL               
001800        05 IDLANDX2          PIC X(2).                                    
001900         88 AUSTRALIA        VALUE 'AU'.                                  
002000         88 AUSTRIA          VALUE 'AT'.                                  
002100         88 BELGIUM          VALUE 'BE'.                                  
002200         88 BRASIL           VALUE 'BR'.                                  
002300         88 CANADA           VALUE 'CA'.                                  
002400         88 SWIZERLAND       VALUE 'CH'.                                  
002500         88 CHINA            VALUE 'CN'.                                  
002600         88 GERMANY          VALUE 'DE'.                                  
002700         88 SPAIN            VALUE 'ES'.                                  
002800         88 FINLAND          VALUE 'FI'.                                  
002900         88 FRANCE           VALUE 'FR'.                                  
003000         88 ENGLAND          VALUE 'GB'.                                  
003100         88 INDIA            VALUE 'IN'.                                  
003200         88 ITALY            VALUE 'IT'.                                  
003300         88 JAPAN            VALUE 'JP'.                                  
003400         88 KOREA            VALUE 'KR'.                                  
003500         88 MALAYSIA         VALUE 'MY'.                                  
003600         88 MEXICO           VALUE 'MX'.                                  
003700         88 HOLLAND          VALUE 'NL'.                                  
003800         88 NORWAY           VALUE 'NO'.                                  
003900         88 RUSSIA           VALUE 'RU'.                                  
004000         88 SWEDEN           VALUE 'SE'.                                  
004100         88 THAILAND         VALUE 'TH'.                                  
004200         88 TURKEY           VALUE 'TR'.                                  
004300         88 TAIWAN           VALUE 'TW'.                                  
004400         88 USA              VALUE 'US'.                                  
004500         88 SOUTH-AFRICA     VALUE 'ZA'.                                  
004600         88 NDC-LAND         VALUE 'AU'                                   
004700                             'CA'                                         
004800                             'CN'                                         
004900                             'JP'                                         
005000                             'US'.                                        
005100*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
005200*** END OF VILMAII-COPY LENGTH= 12 BYTES                                  
