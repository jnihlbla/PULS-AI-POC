000100 01  DCST-W200DCST.                                                       
000200*                                 LINK AREA FOR W200DCST                  
000300     03 DCST-INPUT-DATA.                                                  
000400        05 DCST-IDARTNR-IN   PIC 9(9).                                    
000500*                                 ARTIKELNUMMER                           
000600        05 DCST-IDDC-IN      PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 DCST-OUTPUT-DATA.                                                 
000900        05 DCST-IDDC         PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100        05 DCST-KVLS         PIC S9(7)           COMP-3.                  
001200*                                 LAGERSALDO                              
001300        05 DCST-KVDISP       PIC S9(7)           COMP-3.                  
001400*                                 DISPONIBELT LAGER                       
001500        05 DCST-KVOKS-DAG    PIC S9(7)           COMP-3.                  
001600*                                 ORDERKÖSALDO, KLASS 1                   
001700        05 DCST-KVOKS-BULK   PIC S9(7)           COMP-3.                  
001800*                                 ORDERKÖSALDO, KLASS 2-4                 
001900        05 DCST-KVROS-DAG    PIC S9(7)           COMP-3.                  
002000*                                 RESTORDERSALDO, KLASS 1                 
002100        05 DCST-KVROS-BULK   PIC S9(7)           COMP-3.                  
002200*                                 RESTORDERSALDO, KLASS 2-4               
002300        05 DCST-KVOKS-PREL   PIC S9(7)           COMP-3.                  
002400*                                 PREL ORDERKÖSALDO VERKST.ORDER          
002500        05 DCST-KVEFRS       PIC S9(7)           COMP-3.                  
002600*                                 EJ FAKTURERAT ANTAL STYCK               
002700        05 DCST-KVAKS        PIC S9(7)           COMP-3.                  
002800*                                 ANKOMSTSALDO                            
002900        05 DCST-KVRESS       PIC S9(7)           COMP-3.                  
003000*                                 RESERVERAT ANTAL ARTIKLAR               
003100        05 DCST-KVVORKO      PIC S9(7)           COMP-3.                  
003200*                                 VOR-KÖ KVANT                            
003300        05 DCST-IDLANDX2     PIC X(2).                                    
003400         88 DCST-EMIRATES    VALUE 'AE'.                                  
003500         88 DCST-AUSTRALIA   VALUE 'AU'.                                  
003600         88 DCST-AUSTRIA     VALUE 'AT'.                                  
003700         88 DCST-BELGIUM     VALUE 'BE'.                                  
003800         88 DCST-BRASIL      VALUE 'BR'.                                  
003900         88 DCST-CANADA      VALUE 'CA'.                                  
004000         88 DCST-SWIZERLAND  VALUE 'CH'.                                  
004100         88 DCST-CHINA       VALUE 'CN'.                                  
004200         88 DCST-GERMANY     VALUE 'DE'.                                  
004300         88 DCST-SPAIN       VALUE 'ES'.                                  
004400         88 DCST-FINLAND     VALUE 'FI'.                                  
004500         88 DCST-FRANCE      VALUE 'FR'.                                  
004600         88 DCST-ENGLAND     VALUE 'GB'.                                  
004700         88 DCST-HUNGARY     VALUE 'HU'.                                  
004800         88 DCST-INDIA       VALUE 'IN'.                                  
004900         88 DCST-ITALY       VALUE 'IT'.                                  
005000         88 DCST-JAPAN       VALUE 'JP'.                                  
005100         88 DCST-KOREA       VALUE 'KR'.                                  
005200         88 DCST-MALAYSIA    VALUE 'MY'.                                  
005300         88 DCST-MAROCKO     VALUE 'MA'.                                  
005400         88 DCST-MEXICO      VALUE 'MX'.                                  
005500         88 DCST-HOLLAND     VALUE 'NL'.                                  
005600         88 DCST-NORWAY      VALUE 'NO'.                                  
005700         88 DCST-POLAND      VALUE 'PL'.                                  
005800         88 DCST-RUSSIA      VALUE 'RU'.                                  
005900         88 DCST-SWEDEN      VALUE 'SE'.                                  
006000         88 DCST-THAILAND    VALUE 'TH'.                                  
006100         88 DCST-TURKEY      VALUE 'TR'.                                  
006200         88 DCST-TAIWAN      VALUE 'TW'.                                  
006300         88 DCST-USA         VALUE 'US'.                                  
006400         88 DCST-SOUTH-AFRICA                                             
006500                             VALUE 'ZA'.                                  
006600         88 DCST-LAND-NON-VCC-OWNED                                       
006700                             VALUE 'AE'                                   
006800                             'BR'                                         
006900                             'CN'                                         
007000                             'IN'                                         
007100                             'KR'                                         
007200                             'MY'                                         
007300                             'MX'                                         
007400                             'RU'                                         
007500                             'TH'                                         
007600                             'TR'                                         
007700                             'TW'                                         
007800                             'ZA'.                                        
007900*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
008000        05 DCST-FLKAMPART    PIC X.                                       
008100*                                 KAMPANJ PART                            
008200     03 DCST-KDSVAR          PIC X.                                       
008300      88 DCST-KDSVAR-OK      VALUE ' '.                                   
008400      88 DCST-KDSVAR-FEL     VALUE 'F'.                                   
008500*                                                       KDSVAR-88         
008600*                                 SVARSKOD FRÅN SUBPROGRAM                
008700     03 DCST-IDMSG-ERROR     PIC X(3).                                    
008800*                                 FELMEDDELANDE ID                        
008900     03 DCST-IDELMT-ERROR    PIC X(16).                                   
009000*                                 DATAELEMENTIDENTITET                    
009100     03 DCST-FEL-TEXT        PIC X(25).                                   
009200*** END OF VILMAII-COPY LENGTH= 105 BYTES                                 
