000100 01  FCOI-W200FCOI.                                                       
000200*                                 LINK AREA FOR W200FCOI                  
000300     03 FCOI-INDATA.                                                      
000400        05 FCOI-IDARTNR-IN   PIC 9(9).                                    
000500*                                 ARTIKELNUMMER                           
000600        05 FCOI-IDDC-IN      PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 FCOI-UTDATA.                                                      
000900        05 FCOI-IDDC         PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100        05 FCOI-IDLANDX2     PIC X(2).                                    
001200         88 FCOI-EMIRATES    VALUE 'AE'.                                  
001300         88 FCOI-AUSTRALIA   VALUE 'AU'.                                  
001400         88 FCOI-AUSTRIA     VALUE 'AT'.                                  
001500         88 FCOI-BELGIUM     VALUE 'BE'.                                  
001600         88 FCOI-BRASIL      VALUE 'BR'.                                  
001700         88 FCOI-CANADA      VALUE 'CA'.                                  
001800         88 FCOI-SWIZERLAND  VALUE 'CH'.                                  
001900         88 FCOI-CHINA       VALUE 'CN'.                                  
002000         88 FCOI-GERMANY     VALUE 'DE'.                                  
002100         88 FCOI-SPAIN       VALUE 'ES'.                                  
002200         88 FCOI-FINLAND     VALUE 'FI'.                                  
002300         88 FCOI-FRANCE      VALUE 'FR'.                                  
002400         88 FCOI-ENGLAND     VALUE 'GB'.                                  
002500         88 FCOI-HUNGARY     VALUE 'HU'.                                  
002600         88 FCOI-INDIA       VALUE 'IN'.                                  
002700         88 FCOI-ITALY       VALUE 'IT'.                                  
002800         88 FCOI-JAPAN       VALUE 'JP'.                                  
002900         88 FCOI-KOREA       VALUE 'KR'.                                  
003000         88 FCOI-MALAYSIA    VALUE 'MY'.                                  
003100         88 FCOI-MAROCKO     VALUE 'MA'.                                  
003200         88 FCOI-MEXICO      VALUE 'MX'.                                  
003300         88 FCOI-HOLLAND     VALUE 'NL'.                                  
003400         88 FCOI-NORWAY      VALUE 'NO'.                                  
003500         88 FCOI-POLAND      VALUE 'PL'.                                  
003600         88 FCOI-RUSSIA      VALUE 'RU'.                                  
003700         88 FCOI-SWEDEN      VALUE 'SE'.                                  
003800         88 FCOI-THAILAND    VALUE 'TH'.                                  
003900         88 FCOI-TURKEY      VALUE 'TR'.                                  
004000         88 FCOI-TAIWAN      VALUE 'TW'.                                  
004100         88 FCOI-USA         VALUE 'US'.                                  
004200         88 FCOI-SOUTH-AFRICA                                             
004300                             VALUE 'ZA'.                                  
004400         88 FCOI-LAND-NON-VCC-OWNED                                       
004500                             VALUE 'AE'                                   
004600                             'BR'                                         
004700                             'CN'                                         
004800                             'IN'                                         
004900                             'KR'                                         
005000                             'MY'                                         
005100                             'MX'                                         
005200                             'RU'                                         
005300                             'TH'                                         
005400                             'TR'                                         
005500                             'TW'                                         
005600                             'ZA'.                                        
005700*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
005800        05 FCOI-KVPB-REF     PIC S9(6)V9(1)      COMP-3.                  
005900*                                 PERIODBEHOV REFILLING                   
006000        05 FCOI-OI-PER       OCCURS 6 TIMES.                              
006100           07 FCOI-TIPP      PIC S9(2)           COMP-3.                  
006200*                                 PLANERINGSPERIOD (PP)                   
006300*                                 12 PER ÅR                               
006400           07 FCOI-TIVV-FOM-TOM                                           
006500                             PIC X(5).                                    
006600           07 FCOI-KVOI      PIC S9(7)           COMP-3.                  
006700*                                 ORDERINGÅNG I STYCK PER TIDSENH         
006800        05 FCOI-OI-CURRW.                                                 
006900           07 FCOI-KVOI-INNEV                                             
007000                             PIC S9(7)           COMP-3.                  
007100*                                 ORDERINGÅNG I STYCK PER TIDSENH         
007200        05 FCOI-OI-CDC.                                                   
007300           07 FCOI-KVOI-RULL-12-CDC                                       
007400                             PIC 9(7).                                    
007500           07 FCOI-AARSFORB-CDC                                           
007600                             PIC 9(7).                                    
007700     03 FCOI-KDSVAR          PIC X.                                       
007800      88 FCOI-KDSVAR-OK      VALUE ' '.                                   
007900      88 FCOI-KDSVAR-FEL     VALUE 'F'.                                   
008000*                                                       KDSVAR-88         
008100*                                 SVARSKOD FRÅN SUBPROGRAM                
008200     03 FCOI-IDMSG-ERROR     PIC X(3).                                    
008300*                                 FELMEDDELANDE ID                        
008400     03 FCOI-IDELMT-ERROR    PIC X(16).                                   
008500*                                 DATAELEMENTIDENTITET                    
008600     03 FCOI-FEL-TEXT        PIC X(25).                                   
008700*** END OF VILMAII-COPY LENGTH= 148 BYTES                                 
