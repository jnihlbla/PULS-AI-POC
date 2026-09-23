000010*** EDIT ALLOWED                                                          
000100                                  BEVAT DE INHOUD VAN DE DB N6A4,         
000200                                  N6A401,N6A402 ADI-FILE PLANNING         
000300                                  + GEDECODEERDE PRESTATIE                
0004003 RECTYP                     PIC X(3).                                    
000500                                  IS EEN IDENTIFICATIE VAN EEN RE         
000600                                  CORD IN EEN FILE                        
0007003 SECIND                     PIC X.                                       
000800                                  IS EEN BYTE WELKE OP EEN BEPAAL         
000900                                  DE WAARDE GEZET WORDT VOOR KREA         
001000                                  TIE                                     
001100                                  VAN DE SECONDARY INDEX                  
001200                                                                          
001300                                                                          
0014003 SEGMKODE                   PIC X.                                       
001500                                                                          
001600                                                                          
0017003 SEGKEY.                                                                 
001800                                  KEY SEGMENT N6A401                      
001900  05 TEHDAT                  PIC 9(5).                                    
002000                                  VOOR ELKE KALENDERDAG WORDT DE          
002100                                  DAG,DE WEEK EN HET JAAR AANGEGE         
002200                                  VEN                                     
002300  05 PERSNR                  PIC 9(5).                                    
002400                                  IDENTIFICATIE NUMMER VAN DE WER         
002500                                  KNEMER BIJ DE VENNOOTSCHAP              
0026003 VENTKO                     PIC X.                                       
002700                                  KODE VAN DE JURIDISCHE IDENTITE         
002800                                  IT                                      
0029003 FABKODE                    PIC 9(2).                                    
003000                                  IS EEN KODE WELKE EEN N.V. INDE         
003100                                  ELD IN VERSCHILL. GEBIEDEN              
0032003 MSTGPL                     PIC 9(5).                                    
003300                                  IS DE AANDUIDING WIE ER DE KONT         
003400                                  ROLE HEEFT OVER EEN BEPAALD KPL         
003500                                   STATION                                
0036003 CIRPLA                     PIC X(5).                                    
003700                                  IS HETCIRKULATIEADRES NORMAAL O         
003800                                  F CIRKULATIEADRES TIJDELIJK             
003900                                                                          
0040003 STATPL                     PIC X(4).                                    
004100                                  HET STATION DAT VOORTVLOEIT UIT         
004200                                   STATION NORMAAL OF STATION TIJ         
004300                                  DELIJK                                  
0044003 LGRSTA                     PIC X.                                       
004500                                  IS DE KWALIFICATIE VAN HET STAT         
004600                                  ION                                     
0047003 KOKPLS                     PIC X.                                       
004800                                  DUIDT AAN OF EEN KOSTENPLAATS S         
004900                                  TATION PLANNING EEN TIJDELIJK           
005000                                  OF EEN NORMAAL KPL STATION IS           
005100                                                                          
0052003 ROOSTR                     PIC 9(3).                                    
005300                                  KODERING VAN HET ARBEIDSREGIME          
005400                                  WAARBIJ GLOBALISEREND AANGEGEVE         
005500                                  N                                       
005600                                  WORDT HOE EEN STATION WERKT             
005700                                                                          
005800                                                                          
0059003 SYSTEM                     PIC 9(4).                                    
006000                                  BESCHRIJVING VAN DE WEEKCYCLUS          
006100                                  WAARUIT EEN ROOSTER BESTAAT             
0062003 PRBLKO                     PIC X(2).                                    
006300                                  DE WERKLEIDERSKODES DIE GEBRUIK         
006400                                  T WORDEN OM ALLE AFWIJKENDE             
006500                                  VERSCHIJNSELEN OP HET PRESTATIE         
006600                                  BLAD TE NOTEREN                         
0067003 PLANUR                     PIC S9(2)V9(2).                              
006800                                  AANTAL UREN DAT HET STATION MOE         
006900                                  T BEMAND WORDEN ZOALS AANGEGEVE         
007000                                  N                                       
007100                                  IN HET ARBEIDSSCHEMA.                   
007200                                                                          
007300                                                                          
0074003 PWTELL                     PIC S9(2)V9(2).                              
007500                                  TOTALE WERKELIJKE PRESTATIE PER         
007600                                   DAG DIT IS ZONDER AFWEZIGHEDEN         
007700                                                                          
007800                                  BEHALVE IR EN AD                        
007900                                                                          
008000                                                                          
0081003 IMPUTA.                                                                 
008200                                  IS EEN VERZAMELING VAN REKENING         
008300                                  EN EN ORDERS VOOR DE BOEKHOUDIN         
008400                                  G                                       
008500                                  DIE ELK SPECIFIEK AANDUIDEN WEL         
008600                                  KE DE HERKOMST EN BESTEMMING IS         
008700                                                                          
008800  05 IMPVEN                  PIC 9.                                       
008900                                  IS DE VENNOOTSCHAPS KODE EN DUI         
009000                                  DT AAN WELKE VENNOOT                    
009100                                                                          
009200  05 IMPPRG                  PIC 9.                                       
009300                                  IS DE PRODUKTGROEP BINNEN IN DE         
009400                                   VENNOOTSCHAP                           
009500  05 IMPREK                  PIC 9(3).                                    
009600                                  IS EEN REKENING DIE VERDER KAN          
009700                                  ONDERVERDEELD WORDEN                    
009800                                                                          
009900  05 IMPORE                  PIC 9(4).                                    
010000                                  DIT IS EEN VERDERE AFBRAAK VAN          
010100                                  DE IMPUTATIE REKENING                   
010200                                                                          
010300  05 IMPPRK                  PIC 9(3).                                    
010400                                  DUIDT OP DE AARD VAN DE PRODUKT         
010500                                  IE                                      
010600                                                                          
010700  05 IMPRCO                  PIC 9(3).                                    
010800                                  DIENDT VOOR DE INDIVIDUALISATIE         
010900                                   VAN EEN REKENING                       
011000                                                                          
011100  05 IMPKPL                  PIC 9(5).                                    
011200                                  IS DE KODE VAN DE KOSTENDRAGER          
011300                                  EN KAN DUS ENKEL VOORKOMEN              
011400                                                                          
011500                                  ZO WE HET TE DOEN HEBBEN MET EE         
011600                                  N KOSTENREKENING                        
011700                                                                          
011800  05 IMPMAC                  PIC X(8).                                    
011900                                  IS EEN VERDERE SPECIFICATIE VAN         
012000                                   EEN IMPUTATIE KOSTENPLAATS             
012100                                                                          
012200  05 IMPORD                  PIC 9(9).                                    
012300                                  IS EEN BOEKHOUDKUNDIGE CODIFICA         
012400                                  TIE VAN HET ONDERHANDEN WERK            
012500                                                                          
0126003 FUKODE                     PIC 9(3).                                    
012700                                  IS EEN KODE DIE DE INHOUD VAN H         
012800                                  ET STATION BESCHRIJFT TEN BEHOE         
012900                                  VE                                      
013000                                  VAN DE LOOPBAAN OPVOLGING               
013100                                                                          
013200                                                                          
0133003 LINDPL                     PIC X(2).                                    
013400                                  IS HET LOONINDIKATIEF WELKE VOO         
013500                                  RTVLOEIT UIT HET LOONINDIKATIEF         
013600                                  NORMAAL OF TIJDELIJK                    
013700                                                                          
0138003 PSNAAM                     PIC X(22).                                   
013900                                  IS NAAM-VOORNAAM VAN EEN WERKNE         
014000                                  MER GEKOPPELD AAN EEN PERSONEEL         
014100                                  SNUMMER                                 
0142003 PSVNAM                     PIC X(13).                                   
014300                                  IS DE VOORNAAM VAN EEN WERKNEME         
014400                                  R GEKOPPELD AAN EEN PERSONEELSN         
014500                                  UMMER                                   
0146003 KONTYP                     PIC X(2).                                    
014700                                  TYPE OVEREENKOMST TUSSEN PERSOO         
014800                                  N EN NV BIJ INDIENSTTREDING OF          
014900                                  BIJ                                     
015000                                  INHURING OF BIJ BRUGPENSIOEN            
015100                                                                          
015200                                                                          
0153003 VNVKTY                     PIC X.                                       
015400                                  AANDUIDING OF EEN KONTRAKTTYPE          
015500                                  EEN VOLVO OF EEN NIET VOLVO             
015600                                                                          
015700                                  KONTRAKTTYPE IS                         
015800                                                                          
015900                                                                          
0160003 EDKTYP                     PIC 9(6).                                    
016100                                  LAATSTE DAG WAAROP HET KONTRAKT         
016200                                  TYPE GELDIG WAS                         
016300                                                                          
0164003 KODBEV                     PIC X.                                       
016500                                  KODE WELKE AANDUIDT OF DE WERKL         
016600                                  EIDER BEVESTIGD DAT ALLE PB VAN         
016700                                   EEN                                    
016800                                  TECH DATUM BINNEN ZIJN OP DE PL         
016900                                  ANNINGS DB IS DEZE DAG GEDEKODE         
017000                                  ERD                                     
0171003 KOPLVL                     PIC X.                                       
017200                                  IS EEN KODE DIE AANDUIDT OF DE          
017300                                  PLANNING + AFWIJKINGEN VAN EEN          
017400                                                                          
017500                                  BEPAALDE DAG AL VERLOOND ZIJN           
017600                                                                          
017700                                                                          
0178003 KOATPL                     PIC X.                                       
017900                                  AANDUIDING OF EEN PLANNINGS DAG         
018000                                   EEN KODE ATTEST HEEFT                  
018100                                                                          
0182003 KODHVL                     PIC X.                                       
018300                                  INFORMATIE OP DE AFGEGEVEN AFWE         
018400                                  ZIGHEIDSPERIODE WERD GEVOLGD DO         
018500                                  OR                                      
018600                                  EEN NIEUWE AFWEZIGHEIDSPERIODE          
018700                                  TENGEVOLGE VAN EEN HERVAL OF EE         
018800                                  N                                       
0189003 KOBLDS                     PIC X.                                       
019000                                  IS EEN KODE DIE AANGEEFT OF EEN         
019100                                   BEPAALDE DAG IN DE PLANNING EE         
019200                                  N                                       
019300                                  BUSLOZE DAG IS                          
019400                                                                          
019500                                                                          
0196003 PRCKUL                     PIC S9(3)V9(2).                              
019700                                  PROCENTUELE OPGAVE VAN HET UURL         
019800                                  OON MOEST HIJ IN EEN NORMAAL UU         
019900                                  RLOON                                   
020000                                  WERKEN BIJ EEN BEPAALD LOONINDI         
020100                                  KATIEF                                  
0202003 PRCVOL                     PIC S9(3)V9(2).                              
020300                                  PROCENTUELE OPGAVE VAN DE ARBEI         
020400                                  DSDUUR T.O.V. EEN VOLTIJDSE             
020500                                  BETREKKING                              
020600                                                                          
0207003 KOKB36                     PIC 9.                                       
020800                                  AANDUIDING OF ER EEN INHOUDING          
020900                                  KINDERLOZE GEZINNEN MOET GEBEUR         
021000                                  EN                                      
0211003 INHSOF                     PIC 9.                                       
021200                                  AANDUIDING AL DAN NIET AANGESLO         
021300                                  TEN IS BIJ HET VRIJWILLIG DEEL          
021400                                  VAN                                     
021500                                  MEDISCH SOCIAAL FONDS                   
021600                                                                          
021700                                                                          
0218003 TRSPKO                     PIC 9.                                       
021900                                  AANDUIDING VAN HET VERVOERMIDDE         
022000                                  L DAT DE ARBEIDER GEBRUIKT              
022100                                  OM ZICH NAAR HET BEDRIJF TE BEG         
022200                                  EVEN                                    
0223003 KM-TV                      PIC 9(3).                                    
022400                                  AANTAL KM TUSSEN DE WOONPLAATS          
022500                                  EN HET BEDRIJF                          
0226003 KM-TH                      PIC 9(3).                                    
022700                                  AANTAL KM TUSSEN DE WOONPLAATS          
022800                                  EN DE DICHST BIJZIJNDE HALTE            
022900                                  VAN HET VOLVO VERVOER                   
023000                                                                          
0231003 HALTNR                     PIC 9(3).                                    
023200                                 IS DE IDENTIFICATIE VAN DE HALT          
023300                                 E OP EEN BUSLIJN                         
0234003 BUSLNR                     PIC 9(2).                                    
023500                                 IDENTIFICATIE VAN DE VOLVOBUSSE          
023600                                 N                                        
0237003 USERID                     PIC X(6).                                    
023800                                 IS HET IMS USERID VAN DE PERSOO          
023900                                 N WELKE EEN TRX UPDATE                   
0240003 DATE                       PIC X(6).                                    
024100                                 IS DE DATUM WANNEER EEN SEGMENT          
024200                                  UPGEDATE WORDT                          
0243003 TIME                       PIC X(8).                                    
024400                                 IS DE UUR WANNEER EEN SEGMENT U          
024500                                 PGEDTE WORDT                             
0246003 LTERM                      PIC X(8).                                    
024700                                 IS DE LOGICAL TERMINAL NAAM WAN          
024800                                 NEER EEN SEGMENT UPGEDATE WORDT          
0249003 TEBTUR                     PIC S9(2)V9(2).                              
025000                                 GEEFT HET AANTAL UUR AAN DAT MA          
025100                                 G UITBETAALD WORDEN OP DAGBASIS          
025200                                  EN VOL                                  
025300                                 GENS HET SYSTEEM WAARIN GEWERKT          
025400                                  WORDT - VOOR PDC                        
0255003 FLUR                       PIC S9(2)V9(2)      COMP-3.                  
025600                                 DE GEPLANDE FLEXIBLE UREN                
025700                                                                          
025800                                                                          
0259003 FLFLAG                     PIC 9.                                       
026000                                 INDIKATIE DAT ER GEWERKT WORDT           
026100                                 MET FLEXIBELE UREN (OF SYSTEEM)          
026200                                                                          
0263003 QUATRO                     PIC X.                                       
026400                                 DUIDT AAN OF HET PERSNR WERKT V          
026500                                 OLGENS EEN QUATRO-ROOSTER OF NI          
026600                                 ET                                       
0267003 RVAVER                     PIC S9(4)V9(2).                              
026800                                 HET WEEKLOON VAN MENSEN MET EEN          
026900                                  RV KONTRAKTTYPE                         
0270003 EVURRA                     PIC S9(3)V9(2)      COMP-3.                  
027100                                 EV :IS HET EDUKATIEF VERLOF URE          
027200                                 NRECHT DAT DAG OP DAG BEREKEND           
027300                                 WORDT                                    
027400                                 VOOR ALLE CURSUSSEN VAN HET EV-          
027500                                 TYPE = A                                 
0276003 EVURRB                     PIC S9(3)V9(2)      COMP-3.                  
027700                                 EV :IS HET EDUKATIEF VERLOF URE          
027800                                 NRECHT DAT DAG OP DAG BEREKEND           
027900                                 WORDT                                    
028000                                 VOOR ALLE CURSUSSEN VAN HET EV-          
028100                                 TYPE = B                                 
0282003 EVURRO                     PIC S9(3)V9(2)      COMP-3.                  
028300                                 EV :IS HET EDUKATIEF VERLOF URE          
028400                                 NRECHT DAT DAG OP DAG BEREKEND           
028500                                 WORDT                                    
028600                                 VOOR ALLE CURSUSSEN VAN HET EV-          
028700                                 TYPE = O                                 
0288003 EVINITA                    PIC 9.                                       
028900                                 EV : INITIALISATIE UREN-GENOMEN          
029000                                  V.H. EV-TYPE A:WORDT AANGEZET           
029100                                 OP DE                                    
029200                                 TECHNISCHE DATUM,VOLGEND OP DE           
029300                                 TECHNISCHE EINDDATUM VAN DE CUR          
029400                                 SUS                                      
0295003 EVINITB                    PIC 9.                                       
029600                                 EV : INITIALISATIE UREN-GENOMEN          
029700                                  V.H. EV-TYPE B:WORDT AANGEZET           
029800                                 OP DE                                    
029900                                 TECHNISCHE DATUM,VOLGEND OP DE           
030000                                 TECHNISCHE EINDDATUM VAN DE CUR          
030100                                 SUS                                      
0302003 EVINITO                    PIC 9.                                       
030300                                 EV : INITIALISATIE UREN-GENOMEN          
030400                                  V.H. EV-TYPE O:WORDT AANGEZET           
030500                                 OP DE                                    
030600                                 TECHNISCHE DATUM,VOLGEND OP DE           
030700                                 TECHNISCHE EINDDATUM VAN DE CUR          
030800                                 SUS                                      
0309003 FILLER                     OCCURS 32 TIMES                              
031000                       PIC X.                                             
0311003 SEGMKODE                   PIC X.                                       
031200                                                                          
031300                                                                          
0314003 SEGKEY.                                                                 
031500                                 KEY SEGMENT N6A402                       
031600  05 RECONR                  PIC 9(2).                                    
031700                                 AANDUIDING VAN DE VOLGORDE VAN           
031800                                 HET RECORD VOOR SLEUTELUITBREID          
031900                                 ING                                      
0320003 ADATPB                     PIC 9(5).                                    
032100                                 DATUM VAN EEN AANPASSING OP HET          
032200                                  PRESTATIEBLAD WANEER DE PRESTA          
032300                                 TIE                                      
032400                                 GEBEURT IS  INDIEN GEEN ACHTERS          
032500                                 TALLIGE PB IS DEZE DATUM = TECH          
032600                                 NDAT                                     
0327003 KLWERK                     PIC X(5).                                    
032800                                 IS HET CIRKULATIEADRES VAN DE W          
032900                                 ERKELIJKE PRESTATIE                      
0330003 STWERK                     PIC X(4).                                    
033100                                 STATION WAAROP HIJ WERKELIJK GE          
033200                                 WERKT HEEFT                              
0333003 LINDWE                     PIC X(2).                                    
033400                                 IS HET LOONINDIKATIEF WAARTEGEN          
033500                                  EEN ARBEIDER BETAALD WORDT              
0336003 ROOSTR                     PIC 9(3).                                    
033700                                 KODERING VAN HET ARBEIDSREGIME           
033800                                 WAARBIJ GLOBALISEREND AANGEGEVE          
033900                                 N                                        
034000                                 WORDT HOE EEN STATION WERKT              
034100                                                                          
034200                                                                          
0343003 SYSTEM                     PIC 9(4).                                    
034400                                 BESCHRIJVING VAN DE WEEKCYCLUS           
034500                                 WAARUIT EEN ROOSTER BESTAAT              
0346003 IMPUTA.                                                                 
034700                                 IS EEN VERZAMELING VAN REKENING          
034800                                 EN EN ORDERS VOOR DE BOEKHOUDIN          
034900                                 G                                        
035000                                 DIE ELK SPECIFIEK AANDUIDEN WEL          
035100                                 KE DE HERKOMST EN BESTEMMING IS          
035200                                                                          
035300  05 IMPVEN                  PIC 9.                                       
035400                                 IS DE VENNOOTSCHAPS KODE EN DUI          
035500                                 DT AAN WELKE VENNOOT                     
035600                                                                          
035700  05 IMPPRG                  PIC 9.                                       
035800                                 IS DE PRODUKTGROEP BINNEN IN DE          
035900                                  VENNOOTSCHAP                            
036000  05 IMPREK                  PIC 9(3).                                    
036100                                 IS EEN REKENING DIE VERDER KAN           
036200                                 ONDERVERDEELD WORDEN                     
036300                                                                          
036400  05 IMPORE                  PIC 9(4).                                    
036500                                 DIT IS EEN VERDERE AFBRAAK VAN           
036600                                 DE IMPUTATIE REKENING                    
036700                                                                          
036800  05 IMPPRK                  PIC 9(3).                                    
036900                                 DUIDT OP DE AARD VAN DE PRODUKT          
037000                                 IE                                       
037100                                                                          
037200  05 IMPRCO                  PIC 9(3).                                    
037300                                 DIENDT VOOR DE INDIVIDUALISATIE          
037400                                  VAN EEN REKENING                        
037500                                                                          
037600  05 IMPKPL                  PIC 9(5).                                    
037700                                 IS DE KODE VAN DE KOSTENDRAGER           
037800                                 EN KAN DUS ENKEL VOORKOMEN               
037900                                                                          
038000                                 ZO WE HET TE DOEN HEBBEN MET EE          
038100                                 N KOSTENREKENING                         
038200                                                                          
038300  05 IMPMAC                  PIC X(8).                                    
038400                                 IS EEN VERDERE SPECIFICATIE VAN          
038500                                  EEN IMPUTATIE KOSTENPLAATS              
038600                                                                          
038700  05 IMPORD                  PIC 9(9).                                    
038800                                 IS EEN BOEKHOUDKUNDIGE CODIFICA          
038900                                 TIE VAN HET ONDERHANDEN WERK             
039000                                                                          
0391003 URAFPR                     PIC S9(2)V9(2).                              
039200                                 AANTAL UREN WAAROP HET AFWIJKEN          
039300                                 DE VERSCHIJNSEL SLAAT                    
0394003 PRBLKO                     PIC X(2).                                    
039500                                 DE WERKLEIDERSKODES DIE GEBRUIK          
039600                                 T WORDEN OM ALLE AFWIJKENDE              
039700                                 VERSCHIJNSELEN OP HET PRESTATIE          
039800                                 BLAD TE NOTEREN                          
0399003 KODAAR                     PIC 9(3).                                    
040000                                 AANDUIDING VAN DE AARD VAN DE G          
040100                                 EPRESTEERDE UREN                         
040200                                                                          
0403003 KODTYP                     PIC 9(3).                                    
040400                                 AANDUIDING IN DE LOONKODE VAN P          
040500                                 RESTATIES OF NIET PRESTATIES             
040600                                                                          
0407003 KODEPL                     PIC 9(3).                                    
040800                                 KODE DIE AANGEEFT WELKE PLOEGVE          
040900                                 RGOEDING AAN DE PRESTATIE VERBO          
041000                                 NDEN IS                                  
0411003 TBETUL                     PIC S9(4)V9(2).                              
041200                                 IS HET WERKELIJK TE BETALEN UUR          
041300                                 LOON                                     
0414003 RECBED                     PIC S9(5)V9(2).                              
041500                                 IS URENAFWIJK-PRESTATBLAD X TEB          
041600                                 ETALEN-UURLOON                           
0417003 REPLBE                     PIC S9(4)V9(2).                              
041800                                 IS RECORDBEDRAG X PLOEGENTOESLA          
041900                                 G                                        
0420003 USERID                     PIC X(6).                                    
042100                                 IS HET IMS USERID VAN DE PERSOO          
042200                                 N WELKE EEN TRX UPDATE                   
0423003 DATE                       PIC X(6).                                    
042400                                 IS DE DATUM WANNEER EEN SEGMENT          
042500                                  UPGEDATE WORDT                          
0426003 TIME                       PIC X(8).                                    
042700                                 IS DE UUR WANNEER EEN SEGMENT U          
042800                                 PGEDTE WORDT                             
0429003 LTERM                      PIC X(8).                                    
043000                                 IS DE LOGICAL TERMINAL NAAM WAN          
043100                                 NEER EEN SEGMENT UPGEDATE WORDT          
0432003 FILLER                     OCCURS 72 TIMES                              
043300                       PIC X.                                             
043400D COPY N60100DCC0       LENGTH=450                                        
