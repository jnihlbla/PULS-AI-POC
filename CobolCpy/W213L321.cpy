000100 01  W213L321.                                                            
000200*                                 LÄNKAREA FÖR IMS-CALL FÖR PGM           
000300*                                 W21332 MOT ARTIKELREGISTER              
000400*                                                                         
000500     03 KDCALL               PIC S9(3)           COMP-3.                  
000600      88 LAES-ARTIKELDATA    VALUE +101.                                  
000700      88 UPPD-ARTIKELDATA    VALUE +102.                                  
000800      88 DELETE-KOPPLING     VALUE +103.                                  
000900      88 INSERT-KOPPLING     VALUE +104.                                  
001000*                                 ANROPSTYP FÖR SYSTEM R2XX               
001100     03 FLJANEJ-ANROP        PIC X.                                       
001200      88 ANROP-OK            VALUE 'J'.                                   
001300      88 ANROP-FEL           VALUE 'N'.                                   
001400*                                 JA/NEJ-FLAGGA FÖR R2XX                  
001500     03 IDARTNR              PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 IDLEVNR              PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900     03 IDLEVNR-SHIP         PIC X(5).                                    
002000*                                 SKEPPANDE LEVERANTÖR                    
002100     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002200*                                 PRODUKTSLAG                             
002300     03 FLAGGA-KVPB-JUST-XDC PIC X.                                       
002400*                                 ALLMÄN FLAGGA                           
002500     03 KVPB-PLAN            PIC S9(6)V9(1)      COMP-3.                  
002600*                                 PLANERAT PERIODBEHOV                    
002700     03 RESEASON-PLAN        OCCURS 12 TIMES                              
002800                             PIC S9V9(2)         COMP-3.                  
002900*                                 SÄSONGSINDEX INKLUSIVE REFILL           
003000     03 IOAREA-ARTIKELDATA.                                               
003100*                                                                         
003200        05 KVDAGAR-TT        PIC S9(3)           COMP-3.                  
003300*                                 DAGAR TULL- OCH TRANSPORT-TID           
003400        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
003500*                                 ANTAL VECKOR LEDTID                     
003600        05 FLMANLT           PIC X.                                       
003700*                                 MANUELLT SATT LEDTID ?                  
003800        05 KVVECKOR-AT       PIC S9(3)           COMP-3.                  
003900*                                 ANTAL VECKOR ANSKAFFNINGSTID            
004000        05 FLMANAT           PIC X.                                       
004100*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
004200        05 KDAVT             PIC S9              COMP-3.                  
004300*                                 AVTALSMÄRKNING                          
004400        05 KDKSP             PIC S9              COMP-3.                  
004500*                                 KÖPSPÄRR                                
004600        05 IDANSK            PIC S9(3)           COMP-3.                  
004700*                                 ANSKAFFARNUMMER                         
004800        05 IDPLANGR-AG       PIC S9              COMP-3.                  
004900*                                 PLANERINGSGRUPP ANSKAFFARE              
005000        05 IDPLANGR-LEV      PIC S9              COMP-3.                  
005100*                                 PLANERINGSGRUPP                         
005200        05 KDGK              PIC S9              COMP-3.                  
005300*                                 GODSMOTTAGAREKOD                        
005400        05 KDLTK             PIC S9              COMP-3.                  
005500*                                 LAGERTILLHÖRIGHETSKOD                   
005600        05 FLMANGK           PIC X.                                       
005700*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
005800        05 KDLPSP            PIC S9              COMP-3.                  
005900*                                 LEVERANSPLANESPÄRR                      
006000        05 KDHF              PIC S9              COMP-3.                  
006100*                                 HUVUDFÖRRÅDSMÄRKNING                    
006200        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
006300*                                 ARTIKELSTANDARDPRIS                     
006400        05 IDDC-REF          PIC X(2).                                    
006500*                                 SÄNDANDE LAGER FÖR REFILL               
006600        05 KDERS             PIC S9(3)           COMP-3.                  
006700*                                 ERSÄTTNINGSKOD                          
006800        05 IDLANDX2          PIC X(2).                                    
006900         88 AUSTRALIA        VALUE 'AU'.                                  
007000         88 AUSTRIA          VALUE 'AT'.                                  
007100         88 BELGIUM          VALUE 'BE'.                                  
007200         88 BRASIL           VALUE 'BR'.                                  
007300         88 CANADA           VALUE 'CA'.                                  
007400         88 SWIZERLAND       VALUE 'CH'.                                  
007500         88 CHINA            VALUE 'CN'.                                  
007600         88 GERMANY          VALUE 'DE'.                                  
007700         88 SPAIN            VALUE 'ES'.                                  
007800         88 FINLAND          VALUE 'FI'.                                  
007900         88 FRANCE           VALUE 'FR'.                                  
008000         88 ENGLAND          VALUE 'GB'.                                  
008100         88 HUNGARY          VALUE 'HU'.                                  
008200         88 INDIA            VALUE 'IN'.                                  
008300         88 ITALY            VALUE 'IT'.                                  
008400         88 JAPAN            VALUE 'JP'.                                  
008500         88 KOREA            VALUE 'KR'.                                  
008600         88 MALAYSIA         VALUE 'MY'.                                  
008700         88 MAROCKO          VALUE 'MA'.                                  
008800         88 MEXICO           VALUE 'MX'.                                  
008900         88 HOLLAND          VALUE 'NL'.                                  
009000         88 NORWAY           VALUE 'NO'.                                  
009100         88 POLAND           VALUE 'PL'.                                  
009200         88 RUSSIA           VALUE 'RU'.                                  
009300         88 SWEDEN           VALUE 'SE'.                                  
009400         88 THAILAND         VALUE 'TH'.                                  
009500         88 TURKEY           VALUE 'TR'.                                  
009600         88 TAIWAN           VALUE 'TW'.                                  
009700         88 USA              VALUE 'US'.                                  
009800         88 SOUTH-AFRICA     VALUE 'ZA'.                                  
009900         88 LAND-NON-VCC-OWNED                                            
010000                             VALUE 'CN'                                   
010100                             'IN'                                         
010200                             'KR'                                         
010300                             'MY'                                         
010400                             'RU'                                         
010500                             'TH'                                         
010600                             'TW'.                                        
010700*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
010800*** END OF VILMAII-COPY LENGTH= 79 BYTES                                  
