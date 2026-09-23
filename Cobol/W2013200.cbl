000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2013200.                                                
000300 AUTHOR.         KENT HELLQVIST.                                          
000400 DATE-WRITTEN.   FEBRUARI 1986.                                           
000500     REMARKS.                                                             
000600*        FUNKTION.                                                        
000700*        SKIP2                                                            
000800*        INDATA.                                                          
000900*            TRANSAKTION: W2T132                                          
001000*                         W2T132U                                         
001100*            MID:     W2I13201                                            
001200*        UTDATA.                                                          
001300*            MOD:     W2O13201                                            
001400*        DYNAMISKA SUBPROGRAM.                                            
001500*   ÄNDRINGAR:                                                            
001600*        05-08-23  E-TRACKER 2199243                                      
001700*                  BORTTAG AV HÅRDKODADE ID'N SOM KAN UPPDATERA           
001800*                  FÄLTET RELEASE SPÄRR //L.A.                            
001900*        05-04-26  E-TRACKER 1861513                                      
002000*                  UNDANTAG FÖR LEVNR 10987/BQ8VA VID UPPDATERING         
002100*                  AV DIREKTLEVERANS ANDEL. //L.A.                        
002200*        04-09-15. E-TRACKER 1459688                                      
002300*                  LAGT TILL KDKSP FÖR UPPDATERING I FÄLT DÄR             
002400*                  KVULOAD TIDIGARE LÅG, PÅ UPPDRAG AV CDC                
002500*                  TILLGÄNGLIG. //L.A                                     
002600*        04-09-28  E-TRACKER 690684                                       
002700*                  TILLÄGG FÖR DDGS. //L.A.                               
002800*        03-05-15. TILLAGT FUNKTION FÖR ATT BEGRÄNSA INFORMATION          
002900*                  FÖR USER VARS SEC-IDLEVNR PÅ USER-BASEN                
003000*                  INTE ÄR LIKA MED HUVUDLEVERANTÖREN.                    
003100*                  ( SEC-IDLEVNR = SPACE, FÅR SE ALLT )    /C.E.          
003200*                                                                         
003300*      2006-03-21. BORTTAG AV FÄLTEN KDHF. ETRACKER = 3146035             
003400*                                                          /C.E.          
003500*      SOMMAREN 2014 TILLÄGG AV TIFINLV (REGLER FRÅN 1117)                
003600*                                                                         
003700*      2016-12-09  E-TRACKER 10273761                                     
003800*                  POSSIBILITY TO EXCLUDE 223-ALARM /INGER STENING        
003900                                                                          
004000     EJECT                                                                
004100 ENVIRONMENT DIVISION.                                                    
004200     SKIP3                                                                
004300 DATA DIVISION.                                                           
004400 WORKING-STORAGE SECTION.                                                 
004500*    -COPY WY2000W1                                                       
004600*    -COPY WY2000W2                                                       
004700*    -COPY WY2000W3                                                       
004800     SKIP3                                                                
004900 77  PROGRAM-NAMN                PIC X(8)   VALUE 'W2013200'.             
005000 77  JA                          PIC X(01)         VALUE 'J'.             
005010 77  YES                         PIC X(01)         VALUE 'Y'.             
005100 77  NEJ                         PIC X(01)         VALUE 'N'.             
005200 77  INDX                        PIC S9(9)  VALUE +0   COMP SYNC.         
005300 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +427 COMP SYNC.         
005400 77  MAX-TILEVDAGAR              PIC 9(5)   VALUE 5.                      
005500 77  WS-REDIRLEV-C1              PIC 9V9(02)       VALUE ZERO.            
005600 77  WS-DISP-LAGER               PIC 9(09) COMP-3  VALUE ZERO.            
005700 77  WS-IDLOGLOP                 PIC 9(01) COMP-3  VALUE ZERO.            
005800 77  WS-IDARTNR-8                PIC 9(08).                               
005900                                                                          
006000 77  WS-IDLEVNR-8                PIC X(08) VALUE SPACE.                   
006100                                                                          
006200 77  WS-START-DATUM              PIC 9(6)    VALUE ZERO.                  
006300 77  WS-PRARTSTD-SPAR            PIC 9(7)V9(2) COMP-3  VALUE ZERO.        
006400 77  WS-IDLEVNR-SPAR             PIC X(05)         VALUE SPACE.           
006500 77  WS-IDLEVNR-NUM              PIC 9(05)         VALUE ZERO.            
006600 77  WS-KVRESS-SPAR              PIC 9(07) COMP-3  VALUE ZERO.            
006700 77  WS-KVLS-SPAR                PIC 9(07) COMP-3  VALUE ZERO.            
006800 77  WS-KDAVT-SPAR               PIC 9(01) COMP-3  VALUE ZERO.            
006900 77  WS-KDHF-SPAR                PIC 9(01) COMP-3  VALUE ZERO.            
007000 77  WS-KDLTK-SPAR               PIC 9(01) COMP-3  VALUE ZERO.            
007100 77  WS-KDLTK-ANDR-TEST          PIC 9(01) COMP-3  VALUE ZERO.            
007200 77  WS-KVVECKOR-LVAR-2          PIC 9(2)V9(1).                           
007300 77  WS-KVVECKOR-LVAR-3          PIC X(4).                                
007400 77  WS-IDTRANS                  PIC X(04).                               
007500     88  EGEN-BILD                                 VALUE '2132'.          
007600 77  WS-SATS                     PIC X(1).                                
007700     88  SAKNAS-I-SATS                             VALUE 'N'.             
007800 77  SW-UPPDAT-ARTC12            PIC X(1)          VALUE SPACE.           
007900 77  SW-UPPDAT-ARTC12-OK         PIC X(1)          VALUE SPACE.           
008000 77  SW-LARM-09                  PIC X(01)         VALUE 'N'.             
008100 77  TAECKNING                   PIC X             VALUE SPACE.           
008200 77  RANS-GRANS                  PIC S9(7) COMP-3  VALUE +0.              
008300 77  DISPONIBELT                 PIC S9(7) COMP-3  VALUE +0.              
008400 77  DEFINITIV                   PIC S9    COMP-3  VALUE +1.              
008500 77  W-ART-KDLEVSP               PIC S9(3) COMP-3  VALUE +0.              
008600 77  W-ART-KVLS                  PIC S9(7) COMP-3  VALUE +0.              
008700 77  W-ART-KVUTRS                PIC S9(7) COMP-3  VALUE +0.              
008800 77  W-ART-KVRESS                PIC S9(7) COMP-3  VALUE +0.              
008900 77  W-ART-KVSPANT               PIC S9(7) COMP-3  VALUE +0.              
009000 77  W-ART-KDERS                 PIC S9(3) COMP-3  VALUE +0.              
009100 77  W-ART-KVSLAGER              PIC S9(7) COMP-3  VALUE +0.              
009200 77  W-ART-KDLTK                 PIC S9    COMP-3  VALUE +0.              
009300 77  W-MID-FLRELSP               PIC X(1)          VALUE SPACE.           
009400 77  WS-IDINK                    PIC 9(3)          VALUE ZERO.            
009500 77  TEST-IDINK                  PIC 9(3)          VALUE ZERO.            
009600 77  WS-TIAAVV                   PIC S9(5)   VALUE ZERO COMP-3.           
009700 77  W-ANTAL-VECKOR              PIC 9(3)    VALUE ZERO COMP-3.           
009800 77  IDLEVNR-ALFA                PIC X(5)          VALUE SPACE.           
009900 77  IDARTNR-WS                  PIC X(9).                                
010000 77  WS-IDARTNR REDEFINES IDARTNR-WS  PIC 9(9).                           
010100 77  INDATA-OK                   PIC X(1)    VALUE SPACE.                 
010200 77  W-TIAAVV                    PIC 9(4)    VALUE ZERO.                  
010300                                                                          
010400 *01 -COPY WWDCKONS                                                       
010500                                                                          
010600 01  SPAR-DAGENS-DATUM           PIC 9(06)  VALUE ZERO.                   
010700 01  SPAR-DAGENS-AAVV.                                                    
010800     05  SPAR-DAGENS-AA          PIC 9(02)  VALUE ZERO.                   
010900     05  SPAR-DAGENS-VV          PIC 9(02)  VALUE ZERO.                   
011000 01  SPAR-DAGENS-AAVV-R  REDEFINES  SPAR-DAGENS-AAVV                      
011100                                     PIC 9(04).                           
011200 01  SPAR-DAFINLV-AAAAMMDD       PIC 9(08)  VALUE ZERO.                   
011300 01  SPAR-TIFINLV-AAVVD          PIC 9(05)  VALUE ZERO.                   
011400                                                                          
011500 01  SPAR-TIFINLV-AAVV.                                                   
011600     10  SPAR-TIFINLV-AA           PIC 9(02)  VALUE ZERO.                 
011700     10  SPAR-TIFINLV-VV           PIC 9(02)  VALUE ZERO.                 
011800 01  SPAR-TIFINLV-AAVV-R  REDEFINES  SPAR-TIFINLV-AAVV                    
011900                                  PIC 9(04).                              
012000                                                                          
012100 01  XX-TIFINLV                  PIC X(05)  VALUE SPACE.                  
012200 01  FILLER  REDEFINES  XX-TIFINLV.                                       
012300     05  XX-AAR                   PIC X(02).                              
012400     05  XX-VV                    PIC X(02).                              
012500     05  XX-DAG                   PIC X(01).                              
012600                                                                          
012700 01  VECKOR.                                                              
012800     03  AAVVD                   PIC 9(5).                                
012900     03  FILLER REDEFINES AAVVD.                                          
013000       05  AAVV                  PIC 9(4).                                
013100       05  FILLER REDEFINES AAVV.                                         
013200         07  AA                  PIC 9(2).                                
013300         07  VV                  PIC 9(2).                                
013400       05  D                     PIC 9(1).                                
013500 01  W-AAR-HELP                  PIC 9(4).                                
013600     EJECT                                                                
013700                                                                          
013800 01  W-IDARTNR-8                 PIC 9(8)          VALUE ZERO.            
013900 01  W-IDLOGLOP                  PIC S9(1)         VALUE ZERO.            
014000                                                                          
014100     EJECT                                                                
014200 01  SWITCHAR.                                                            
014300     05  SW-INPUT-RAETT          PIC X(01)  VALUE 'J'.                    
014400     05  SW-BASEN-RAETT          PIC X(01)  VALUE 'J'.                    
014500     05  SW-GODKAENT-ID          PIC X(01)  VALUE 'N'.                    
014600     05  SW-GODKAENT-ID-NYB      PIC X(01)  VALUE 'N'.                    
014700     05  SW-KOLLA-TIFINLV        PIC X(01)  VALUE 'N'.                    
014800                                                                          
014900 01  WS-HJALP-AAVV1.                                                      
015000     05  WS-AAVV-NOLL            PIC 9(01)  VALUE ZERO.                   
015100     05  WS-AA                   PIC 9(02).                               
015200     05  WS-VV                   PIC 9(02).                               
015300                                                                          
015400 01  WS-HJALP-AAVV2  REDEFINES WS-HJALP-AAVV1                             
015500                                 PIC S9(05).                              
015600                                                                          
015700 01  WS-IDAVTAL                  PIC 9(13).                               
015800 01  WS-IDAVTAL-RED REDEFINES WS-IDAVTAL.                                 
015900     05  FILLER                  PIC X(01).                               
016000     05  WS-IDAVTAL-PREFIX       PIC X(03).                               
016100     05  WS-IDAVTAL-AVTALNR      PIC X(06).                               
016200     05  WS-IDAVTAL-SUFFIX       PIC X(03).                               
016300 01  WS-IDAVTAL-PREFIX-NUM       PIC 9(3).                                
016400                                                                          
016500 01  FILLER.                                                              
016600     03  WS-KDKSP                PIC 9(1) VALUE ZERO.                     
016700     03  WS-PRARTBES-PR          PIC S9(7)V99 COMP-3.                     
016800     03  WS-PRARTBES             PIC X(01)  VALUE 'N'.                    
016900                                                                          
017000 01  W-DAGENS-DATUM-PL-ONE-YEAR            PIC 9(6) VALUE ZERO.           
017100 01  FILLER REDEFINES W-DAGENS-DATUM-PL-ONE-YEAR.                         
017200     03  W-DAGENS-DATUM-PL-ONE-YEAR-YY     PIC 9(2).                      
017300     03  W-DAGENS-DATUM-PL-ONE-YEAR-MM     PIC 9(2).                      
017400     03  W-DAGENS-DATUM-PL-ONE-YEAR-DD     PIC 9(2).                      
017500                                                                          
017600 01  DAGENS-AAAAMMDD             PIC 9(8)   VALUE ZERO.                   
017700 01  DAGENS-DATUM                PIC 9(6)   VALUE ZERO.                   
017800 01  FILLER REDEFINES DAGENS-DATUM.                                       
017900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
018000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
018100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
018200                                                                          
018300     EJECT                                                                
018400                                                                          
018500 01  W-IDAVTAL-RED               PIC 9(13).                               
018600 01  W-IDAVTAL REDEFINES W-IDAVTAL-RED.                                   
018700     03  FILLER                  PIC X.                                   
018800     03  W-PREFIX                PIC X(3).                                
018900     03  W-AVTALSNR              PIC X(6).                                
019000     03  W-SUFFIX                PIC X(3).                                
019100 01  W-PREFIX-NUM                PIC 9(3).                                
019200     SKIP2                                                                
019300                                                                          
019400 01  ANVAENDAR-TABELL.                                                    
019500     03  FILLER              PIC X(18) VALUE 'PC10964 CAMILLA NI'.        
019600     03  FILLER              PIC X(18) VALUE 'PC33574 RICKARD HO'.        
019700     03  FILLER              PIC X(18) VALUE 'PC45491 JULIA ROLF'.        
019800 01  FILLER REDEFINES ANVAENDAR-TABELL.                                   
019900     03  ANVAENDAR-RAD OCCURS 3                                           
020000         INDEXED BY ANVAENDAR-IX.                                         
020100         05  RAD-USERID      PIC X(8).                                    
020200         05  RAD-DELNAMN     PIC X(10).                                   
020300                                                                          
020400                                                                          
020500 01  DYNAMISKA-SUBPROGRAM.                                                
020600     05  WDATKONV                PIC X(08)  VALUE 'WDATKONV'.             
020700     05  WDECEDIT                PIC X(08)  VALUE 'WDECEDIT'.             
020800     05  FELLOG                  PIC X(08)  VALUE 'FELLOG  '.             
020900     05  CBLTDLI                 PIC X(08)  VALUE 'CBLTDLI '.             
021000     05  WMEDKONV                PIC X(08)  VALUE 'WMEDKONV'.             
021100     05  W005INIT                PIC X(08)  VALUE 'W005INIT'.             
021200     05  W009VADD                PIC X(8)   VALUE 'W009VADD'.             
021300     EJECT                                                                
021400*01  AREA -COPY W092W001        -PRE W092-                                
021500     EJECT                                                                
021600******************************************************************        
021700*    I N K Ö P S - P O S T    P V                                         
021800******************************************************************        
021900*                                                                         
022000*01  -COPY A310TB65             -PRE A310-                                
022100     EJECT                                                                
022200******************************************************************        
022300*    I N K Ö P S - P O S T    L V                                         
022400******************************************************************        
022500*                                                                         
022600*    ANV EJ  T310TTV5                                                     
022700     EJECT                                                                
022800                                                                          
022900******************************************************************        
023000*    N Y C K L A R  T I L L  D L I                                        
023100******************************************************************        
023200*                                                                         
023300 01  NYCKLAR-TILL-DLI.                                                    
023400     03  W-IDARTNR-X.                                                     
023500         05  W-IDARTNR            PIC S9(09) COMP-3 VALUE ZERO.           
023600                                                                          
023700     03  W-IDLEVNR-X.                                                     
023800         05  W-IDLEVNR            PIC X(05)  VALUE SPACE.                 
023900                                                                          
024000     03  W-DAPRLIST-X.                                                    
024100         05  W-DAPRLIST           PIC 9(08)  VALUE ZERO.                  
024200                                                                          
024300     03  W-WDG3KEY-2221-X.                                                
024400         05  FILLER               PIC X(04)  VALUE '2221'.                
024500         05  FILLER               PIC X(26)  VALUE LOW-VALUE.             
024600                                                                          
024700     03  W-WDG3KEY-2213-X.                                                
024800         05  FILLER               PIC X(04)  VALUE '2213'.                
024900         05  W-IDDC               PIC X(02)  VALUE '11'.                  
025000         05  FILLER               PIC X(24)  VALUE LOW-VALUE.             
025100                                                                          
025200     03  W-IDSKYLT-X.                                                     
025300         05  W-IDSKYLT            PIC X(03)  VALUE SPACE.                 
025400                                                                          
025500     03  W-KDNOTTYP-X.                                                    
025600         05  W-KDNOTTYP           PIC S9(01) COMP-3 VALUE ZERO.           
025700                                                                          
025800     03  W-1141KEY-X.                                                     
025900         05  FILLER              PIC X(04)  VALUE '1141'.                 
026000         05  FILLER              PIC X(26)  VALUE LOW-VALUE.              
026100                                                                          
026200     03  W-WDD7A1KY-MIN.                                                  
026300         05  W-IDARTNR-MIN7       PIC S9(9)  COMP-3 VALUE ZERO.           
026400         05  FILLER               PIC X(7)   VALUE LOW-VALUE.             
026500                                                                          
026600     03  W-WDD7A1KY-MAX.                                                  
026700         05  W-IDARTNR-MAX7       PIC S9(9)  COMP-3                       
026800                                  VALUE ZERO.                             
026900         05  FILLER               PIC X(7)   VALUE HIGH-VALUE.            
027000                                                                          
027100     03  W-WDGXKEY-2203-X.                                                
027200         05  FILLER              PIC X(4)    VALUE '2203'.                
027300         05  W-IDDC-2203         PIC X(2)    VALUE '  '.                  
027400         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
027500                                                                          
027600                                                                          
027700******************************************************************        
027800*    F E L M E D D E L A N D E N                                          
027900******************************************************************        
028000*                                                                         
028100*01 -COPY WMEDAREA                                                        
028200     SKIP3                                                                
028300 01  MESSAGE-CODES.                                                       
028400     03  ERR-CORR-HILITE-FLDS    PIC X(3)    VALUE '001'.                 
028500     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
028600     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
028700     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
028800     03  ERR-KEY-MISSING         PIC X(3)    VALUE '005'.                 
028900     03  ERR-FUTURE-DATE         PIC X(3)    VALUE '363'.                 
029000     03  ERR-ONE-YEAR-IN-FUTURE  PIC X(3)    VALUE '364'.                 
029100     03  ERR-PART-EXPIRED        PIC X(3)    VALUE '018'.                 
029200     03  ERR-PART-MISSING        PIC X(3)    VALUE '017'.                 
029300     03  ERR-USER-NOT-AUTH       PIC X(3)    VALUE '405'.                 
029400     03  INF-REFILL-PART         PIC X(3)    VALUE '434'.                 
029500*                                                                         
029600     EJECT                                                                
029700*01  -COPY WDATAREA                                                       
029800     EJECT                                                                
029900*                    **** PARAMETRAR TILL W005INIT                        
030000*01  -COPY WMSGINIT                                                       
030100     EJECT                                                                
030200*01  -COPY WDECAREA                                                       
030300     EJECT                                                                
030400*                        ****    MFS OCH SKÄRMHANTERING                   
030500 01  FILLER              PIC X(16)   VALUE 'MFS-WS'.                      
030600     SKIP2                                                                
030700*01  MID -COPY W2I13201                                                   
030800     EJECT                                                                
030900*01  -COPY WMSGAREA                                                       
031000     EJECT                                                                
031100*    03  MOD -COPY W2O13201  -RED MSG-AREA.                               
031200     EJECT                                                                
031300*01  -COPY WMFSAREA.                                                      
031400     EJECT                                                                
031500******************************************************************        
031600*****                                                                     
031700*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
031800*****                                                                     
031900 01  IMS-WS.                                                              
032000     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
032100     SKIP3                                                                
032200*****                    **** STATUS-KOD FRÅN IMS                         
032300     03  STATUS-WS               PIC X(2).                                
032400         88  SEGMENT-FINNS                   VALUE '  '.                  
032500         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
032600     SKIP3                                                                
032700     03  GODK-STATUSKODER.                                                
032800         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
032900     SKIP3                                                                
033000 01  SSA1                        PIC X(64).                               
033100 01  SSA2                        PIC X(64).                               
033200 01  SSA3                        PIC X(64).                               
033300     EJECT                                                                
033400*                            IMS FUNKTIONSKODER                           
033500*01  -COPY W0003                                                          
033600     EJECT                                                                
033700*                            DLI INPUT-OUTPUT AREA                        
033800 01  DLI-IO-AREA-01.                                                      
033900     03  IO-AREA-01                PIC X(200) VALUE SPACE.                
034000     SKIP3                                                                
034100*    03  ARTC -COPY WDK601        -RED IO-AREA-01.                        
034200     EJECT                                                                
034300 01  DLI-IO-AREA-11.                                                      
034400     03  IO-AREA-11                PIC X(900) VALUE SPACE.                
034500     SKIP3                                                                
034600*    03  ARTC -COPY WDK611        -RED IO-AREA-11.                        
034700     EJECT                                                                
034800 01  DLI-IO-AREA-21.                                                      
034900*    03  ARTC -COPY WDK621                                                
035000     EJECT                                                                
035100 01  DLI-IO-AREA1.                                                        
035200     03  IO-AREA1                  PIC X(200) VALUE SPACE.                
035300     SKIP3                                                                
035400*    03  ARTC -COPY WDK623        -RED IO-AREA1.                          
035500     EJECT                                                                
035600*    03  ARTC -COPY WDK625        -RED IO-AREA1.                          
035700     EJECT                                                                
035800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
035900 01  DLI-IO-WDK701.                                                       
036000*    03  -COPY WDK701                                                     
036100     SKIP3                                                                
036200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
036300 01  DLI-IO-WDK712.                                                       
036400*    03  -COPY WDK712                                                     
036500     EJECT                                                                
036600 01  DLI-IO-AREA2.                                                        
036700     03  IO-AREA2                  PIC X(200) VALUE SPACE.                
036800     SKIP3                                                                
036900*    03  BENA -COPY WDD301        -PRE BENA- -RED IO-AREA2.               
037000     EJECT                                                                
037100*    03  BENA -COPY WDD311        -PRE BENA- -RED IO-AREA2.               
037200     EJECT                                                                
037300 01  DLI-IO-AREA3.                                                        
037400     03  IO-AREA3                  PIC X(100) VALUE SPACE.                
037500     SKIP3                                                                
037600*    03  LEVA -COPY WDF101        -PRE LEVA- -RED IO-AREA3.               
037700     EJECT                                                                
037800 01  DLI-IO-AREA4.                                                        
037900     03  IO-AREA4                  PIC X(50) VALUE SPACE.                 
038000     SKIP3                                                                
038100*    03  XXBI -COPY WDGX01      -PRE XXBI-        -RED IO-AREA4.          
038200     EJECT                                                                
038300*    03  XXBI -COPY WDGX2214    -PRE XXBI-        -RED IO-AREA4.          
038400     EJECT                                                                
038500*    03  XXBN -COPY WDGX01      -PRE XXBN-        -RED IO-AREA4.          
038600     EJECT                                                                
038700*    03  XXBN -COPY WDGX2222    -PRE XXBN-        -RED IO-AREA4.          
038800     EJECT                                                                
038900 01  DLI-IO-AREA5.                                                        
039000     03  IO-AREA5                  PIC X(150) VALUE SPACE.                
039100     SKIP3                                                                
039200*    03  ZZAC -COPY WDGZ01        -PRE ZZAC-   -RED IO-AREA5.             
039300                                                                          
039400 01  FILLER                      PIC X(16)   VALUE 'K601-AREA'.           
039500 01  DLI-IO-AREA-K601.                                                    
039600*    03  -COPY WDK601 -PRE K601-                                          
039700                                                                          
039800 01  FILLER                      PIC X(16)   VALUE 'K611-AREA'.           
039900 01  DLI-IO-AREA-K611.                                                    
040000*    03  -COPY WDK611 -PRE K611-                                          
040100                                                                          
040200 01  FILLER                      PIC X(16)   VALUE 'ERSB-AREA'.           
040300 01  DLI-IO-AREA-ERSB.                                                    
040400*    03  ERSB -COPY WDD7A1      -PRE ERSB01-                              
040500                                                                          
040600 01  FILLER                      PIC X(16)   VALUE 'WDG3-AREA'.           
040700 01  DLI-IO-AREA-2204.                                                    
040800*    03  -COPY WDGX2204   -PRE XXBJ11-                                    
040900     03  FILLER                  PIC X(3)  VALUE SPACE.                   
041000     EJECT                                                                
041100 LINKAGE SECTION.                                                         
041200     SKIP2                                                                
041300*01  -COPY W0009     -PRE MSG-                                            
041400     EJECT                                                                
041500*01  -COPY W0008     -PRE USEA-                                           
041600         05  FILLER              PIC X.                                   
041700     EJECT                                                                
041800*01  -COPY W0008     -PRE ARTC-                                           
041900         05  FILLER              PIC X.                                   
042000     EJECT                                                                
042100*01  -COPY W0008     -PRE WDK7-                                           
042200         05  FILLER              PIC X.                                   
042300     EJECT                                                                
042400*01  -COPY W0008     -PRE BENA-                                           
042500         05  FILLER              PIC X.                                   
042600     EJECT                                                                
042700*01  -COPY W0008     -PRE LEVA-                                           
042800         05  FILLER              PIC X.                                   
042900     EJECT                                                                
043000*01  -COPY W0008     -PRE XXBI-                                           
043100         05  FILLER              PIC X.                                   
043200     EJECT                                                                
043300*01  -COPY W0008     -PRE XXBN-                                           
043400         05  FILLER              PIC X.                                   
043500     EJECT                                                                
043600*01  -COPY W0008     -PRE ZZAC-                                           
043700         05  FILLER              PIC X.                                   
043800     EJECT                                                                
043900*01  -COPY W0008     -PRE W009AB-                                         
044000         05  FILLER              PIC X.                                   
044100     EJECT                                                                
044200*01  -COPY W0008     -PRE WDK6-                                           
044300         05  FILLER              PIC X.                                   
044400     EJECT                                                                
044500*01  -COPY W0008     -PRE ERSB-                                           
044600         05  FILLER              PIC X.                                   
044700     EJECT                                                                
044800*01  -COPY W0008     -PRE XXBJ-                                           
044900         05  FILLER              PIC X.                                   
045000     EJECT                                                                
045100 PROCEDURE DIVISION USING MSG-PCB USEA-PCB ARTC-PCB WDK7-PCB              
045200                                  BENA-PCB LEVA-PCB                       
045300     XXBI-PCB XXBN-PCB ZZAC-PCB W009AB-PCB                                
045400     WDK6-PCB ERSB-PCB XXBJ-PCB.                                          
045500     SKIP1                                                                
045600     ENTRY 'DLITCBL' USING MSG-PCB USEA-PCB                               
045700                                   ARTC-PCB                               
045800                                   WDK7-PCB                               
045900                                   BENA-PCB                               
046000                                   LEVA-PCB                               
046100                                   XXBI-PCB                               
046200                                   XXBN-PCB                               
046300                                   ZZAC-PCB                               
046400                                   W009AB-PCB                             
046500                                   WDK6-PCB                               
046600                                   ERSB-PCB                               
046700                                   XXBJ-PCB.                              
046800                                                                          
046900     PERFORM IMS-GET-MSG                                                  
047000     IF SEGMENT-FINNS                                                     
047100        PERFORM A-INIT-SPARA-INPUT                                        
047200        IF WS-IDARTNR NUMERIC                                             
047300           MOVE WS-IDARTNR  TO W-IDARTNR                                  
047400           PERFORM IMS-GET-ARTC01                                         
047500           IF SEGMENT-FINNS                                               
047600              MOVE ART-IDLEVNR TO WS-IDLEVNR-8                            
047700*             --- KOLLA BEHÖRIGHET                                        
047800              IF MSGI-KDARBTYP-SEC-IDLEV = WS-IDLEVNR-8                   
047900              OR MSGI-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE             
048000*                --- BEHÖRIG USER                                         
048100                 IF ART-KDERS-UTG > ZERO                                  
048200                    MOVE ERR-PART-EXPIRED      TO MED-IDMFSFEL            
048300                 ELSE                                                     
048400                    IF ART-FLIART = 'J'                                   
048500                       MOVE JA                 TO WS-SATS                 
048600                    ELSE                                                  
048700                       MOVE NEJ                TO WS-SATS                 
048800                    END-IF                                                
048900                    IF (MFS-UPDATE AND EGEN-BILD) OR                      
049000                       (MFS-UPD-V AND EGEN-BILD)                          
049100                       PERFORM B-KOLLA-INPUT                              
049200                       IF SW-INPUT-RAETT = JA                             
049300                          PERFORM C-KOLLA-MOT-BASEN                       
049400                          IF SW-BASEN-RAETT = JA                          
049500                             PERFORM D-UPPDATERA                          
049600                          END-IF                                          
049700                       END-IF                                             
049800                    ELSE                                                  
049900                       PERFORM E-VISA-BILD                                
050000                    END-IF                                                
050100                 END-IF                                                   
050200              ELSE                                                        
050300*                -- EJ BEHÖRIG USER                                       
050400                 MOVE ERR-USER-NOT-AUTH   TO MED-IDMFSFEL                 
050500              END-IF                                                      
050600           ELSE                                                           
050700              MOVE ERR-PART-MISSING   TO MED-IDMFSFEL                     
050800           END-IF                                                         
050900        ELSE                                                              
051000           MOVE ERR-KEY-MISSING      TO MED-IDMFSFEL                      
051100        END-IF                                                            
051200                                                                          
051300        IF MED-IDMFSFEL = ERR-PART-EXPIRED       OR                       
051400                          ERR-USER-NOT-AUTH      OR                       
051500                          ERR-PART-MISSING       OR                       
051600                          ERR-KEY-MISSING                                 
051700           CALL WMEDKONV USING MED-WMEDAREA                               
051800           MOVE MED-MFSFEL  TO MOD-TEMFSFEL                               
051900        END-IF                                                            
052000                                                                          
052100        MOVE MAX-MOD-LAENGD TO MSG-KVLL                                   
052200        PERFORM IMS-INSERT-MSG                                            
052300     END-IF                                                               
052400                                                                          
052500     MOVE ZERO TO RETURN-CODE                                             
052600     GOBACK                                                               
052700     .                                                                    
052800     EJECT                                                                
052900 A-INIT-SPARA-INPUT SECTION.                                              
053000     SKIP2                                                                
053100     IF MSG-DUBBLA-TRANSKODER                                             
053200         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W2I13201               
053300         MOVE MSG-IDTRANS-2 TO MFS-IDTRANS WS-IDTRANS                     
053400         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
053500         MOVE MSG-KDTRTYP          TO MFS-KDTRTYP                         
053600     ELSE                                                                 
053700         MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W2I13201                
053800         MOVE MSG-IDTRANS-1 TO MFS-IDTRANS WS-IDTRANS                     
053900         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
054000         MOVE SPACE TO MFS-KDTRTYP                                        
054100     END-IF                                                               
054200                                                                          
054300     MOVE ALL '+' TO MSGI-WMSGINIT                                        
054400     MOVE '001'             TO MSGI-KDCALL                                
054500     MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                                
054600     MOVE MSG-LTERM-NAME    TO MSGI-IDLTERM-USER                          
054700     MOVE '2132'            TO MSGI-IDTRANS                               
054800     IF MFS-IDTRANS = '2132'                                              
054900     OR (MID-IDARTNR-IN NUMERIC                                           
055000     AND MID-IDARTNR-IN > ZERO)                                           
055100         MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                              
055200     END-IF                                                               
055300     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
055400     MOVE MSGI-IDARTNR TO WS-IDARTNR                                      
055500     INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                       
055600                                                                          
055700     IF MID-IDARTNR-IN = ALL '+'                                          
055800        CONTINUE                                                          
055900     ELSE                                                                 
056000        MOVE SPACE TO MFS-KDTRTYP                                         
056100     END-IF                                                               
056200                                                                          
056300     MOVE LOW-VALUE TO MOD-W2O13201                                       
056400     MOVE 'W2O132N1' TO MFS-IDMOD                                         
056500     MOVE '2132' TO MOD-IDTRANS                                           
056600                                                                          
056700     MOVE WS-IDARTNR TO MOD-IDARTNR-UT                                    
056800     INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE               
056900                                                                          
057000     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
057100                             MOD-TEMFSINF                                 
057200                             MOD-IDARTNR-IN                               
057300                                                                          
057400     ACCEPT DAGENS-DATUM FROM DATE                                        
057500                                                                          
057600     MOVE DAGENS-DATUM          TO DAT-I-TIDATUM                          
057700     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
057800     PERFORM S99-WDATKONV                                                 
057900                                                                          
058000     IF DAT-KDSVAR-OK                                                     
058100        MOVE DAT-TIAA-VECKA     TO SPAR-DAGENS-AA                         
058200        MOVE DAT-TIVV           TO SPAR-DAGENS-VV                         
058300     END-IF                                                               
058400                                                                          
058500     MOVE 1               TO MOD-C2FAELT-SW                               
058600                                                                          
058700*TO SHOW THE SCREEN IN ENGLISH                                            
059110     MOVE 'GB ' TO MED-IDSKYLT                                            
059120*                                                                         
059200     .                                                                    
059300     EJECT                                                                
059400 B-KOLLA-INPUT SECTION.                                                   
059500     SKIP2                                                                
059600     MOVE JA TO SW-INPUT-RAETT                                            
059700     IF MID-INPUT = ALL '+'                                               
059800        MOVE ERR-PF11-AND-NO-DATA TO MED-IDMFSFEL                         
059900        CALL WMEDKONV USING MED-WMEDAREA                                  
060000        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
060100        PERFORM MFS-ROER-EJ-FAELT-IN                                      
060200        PERFORM S02-ROER-EJ-FAELT                                         
060300        MOVE NEJ TO SW-INPUT-RAETT                                        
060400     ELSE                                                                 
060500        PERFORM BA-KOLLA-MID-INDATA                                       
060600        IF SW-INPUT-RAETT = NEJ                                           
060700          IF MED-IDMFSFEL = ERR-FUTURE-DATE OR                            
060800                            ERR-ONE-YEAR-IN-FUTURE                        
060900             CONTINUE                                                     
061000          ELSE                                                            
061100             MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                    
061200          END-IF                                                          
061300          CALL WMEDKONV USING MED-WMEDAREA                                
061400          MOVE MED-MFSFEL   TO MOD-TEMFSFEL                               
061500          PERFORM S02-ROER-EJ-FAELT                                       
061600          PERFORM MFS-ROER-EJ-FAELT-IN                                    
061700        END-IF                                                            
061800     END-IF                                                               
061900                                                                          
062000     PERFORM IMS-GET-ARTC01                                               
062100     IF SEGMENT-FINNS                                                     
062200        PERFORM IMS-GHNP-ARTC11                                           
062300        IF SEGMENT-FINNS                                                  
062400          IF CLAG-IDDC-REF NOT = SPACE                                    
062500             MOVE INF-REFILL-PART   TO MED-IDMFSFEL                       
062600             CALL WMEDKONV       USING MED-WMEDAREA                       
062700             MOVE MED-MFSFEL        TO MOD-TEMFSFEL                       
062800             PERFORM S010-CLOSE-ARTC12-FAELT                              
062900          END-IF                                                          
063000        END-IF                                                            
063100     END-IF                                                               
063200                                                                          
063300     .                                                                    
063400     EJECT                                                                
063500                                                                          
063600                                                                          
063700 BA-KOLLA-MID-INDATA SECTION.                                             
063800                                                                          
063900     IF MID-FLFSP NOT = ALL '+'                                           
064000        IF MID-FLFSP = JA OR YES OR NEJ                                   
064100           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLFSP-IN-ATTR              
064200        ELSE                                                              
064300           MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLFSP-IN-ATTR              
064400           MOVE NEJ                     TO SW-INPUT-RAETT                 
064500        END-IF                                                            
064600        MOVE MFS-ROER-EJ-FAELT          TO MOD-FLFSP-IN                   
064700     ELSE                                                                 
064800        MOVE MFS-RENSA-FAELT            TO MOD-FLFSP-IN                   
064900     END-IF                                                               
065000                                                                          
065100     IF MID-KVSLUTKP NOT = ALL '+'                                        
065200        IF MID-KVSLUTKP NUMERIC                                           
065300           MOVE MFS-NUM-FAELT-RAETT     TO MOD-KVSLUTKP-IN-ATTR           
065400        ELSE                                                              
065500           MOVE MFS-NUM-FAELT-FEL       TO MOD-KVSLUTKP-IN-ATTR           
065600           MOVE NEJ                     TO SW-INPUT-RAETT                 
065700        END-IF                                                            
065800        MOVE MFS-ROER-EJ-FAELT          TO MOD-KVSLUTKP-IN                
065900     ELSE                                                                 
066000        MOVE MFS-RENSA-FAELT            TO MOD-KVSLUTKP-IN                
066100     END-IF                                                               
066200                                                                          
066300     IF MID-KDKSP NOT = ALL '+'                                           
066400        IF MID-KDKSP NUMERIC                                              
066500           IF MID-KDKSP < 7                                               
066600              MOVE MFS-NUM-FAELT-RAETT                                    
066700                                      TO MOD-KDKSP-IN-ATTR                
066800           ELSE                                                           
066900              MOVE NEJ                TO SW-INPUT-RAETT                   
067000              MOVE MFS-NUM-FAELT-FEL  TO MOD-KDKSP-IN-ATTR                
067100           END-IF                                                         
067200        ELSE                                                              
067300           MOVE NEJ                TO SW-INPUT-RAETT                      
067400           MOVE MFS-NUM-FAELT-FEL     TO MOD-KDKSP-IN-ATTR                
067500        END-IF                                                            
067600     ELSE                                                                 
067700        MOVE MFS-RENSA-FAELT          TO MOD-KDKSP-IN                     
067800     END-IF                                                               
067900                                                                          
068000     IF MID-TISLUTKP NOT = ALL '+'                                        
068100        IF MID-TISLUTKP NUMERIC                                           
068200          IF MID-TISLUTKP > ZERO                                          
068300            MOVE MID-TISLUTKP          TO DAT-I-TIDATUM                   
068400            MOVE 'AAVVD '              TO DAT-KDDATFORM                   
068500            CALL WDATKONV USING DAT-KDDATFORM                             
068600                                DAT-I-TIDATUM                             
068700                                DAT-O-TIDATUM                             
068800                                DAT-KDSVAR                                
068900            MOVE DAT-TIAAMMDD   TO TMP1-YYMMDD                            
069000            MOVE DAGENS-DATUM   TO TMP2-YYMMDD                            
069100            PERFORM WY2000P1                                              
069200            IF  DAT-KDSVAR-OK                                             
069300            AND TMP1-YYMMDD > TMP2-YYMMDD                                 
069400            AND TMP1-YYMMDD <= (TMP2-YYMMDD + 10000)                      
069500              MOVE MFS-NUM-FAELT-RAETT TO MOD-TISLUTKP-IN-ATTR            
069600            ELSE                                                          
069700              MOVE MFS-NUM-FAELT-FEL   TO MOD-TISLUTKP-IN-ATTR            
069800              MOVE NEJ                 TO SW-INPUT-RAETT                  
069900            END-IF                                                        
070000          END-IF                                                          
070100        ELSE                                                              
070200          MOVE MFS-NUM-FAELT-FEL       TO MOD-TISLUTKP-IN-ATTR            
070300          MOVE NEJ                     TO SW-INPUT-RAETT                  
070400        END-IF                                                            
070500        MOVE MFS-ROER-EJ-FAELT         TO MOD-TISLUTKP-IN                 
070600     ELSE                                                                 
070700        MOVE MFS-RENSA-FAELT           TO MOD-TISLUTKP-IN                 
070800     END-IF                                                               
070900                                                                          
071000     IF MID-KVSLUTKP = ALL '+'                                            
071100     OR MID-TISLUTKP = ALL '+'                                            
071200       CONTINUE                                                           
071300     ELSE                                                                 
071400       IF MID-KVSLUTKP = ZERO                                             
071500         IF MID-TISLUTKP > ZERO                                           
071600           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVSLUTKP-IN-ATTR            
071700                                          MOD-TISLUTKP-IN-ATTR            
071800           MOVE NEJ                    TO SW-INPUT-RAETT                  
071900         END-IF                                                           
072000       ELSE                                                               
072100         IF MID-TISLUTKP = ZERO                                           
072200           MOVE MFS-NUM-FAELT-FEL      TO MOD-KVSLUTKP-IN-ATTR            
072300                                          MOD-TISLUTKP-IN-ATTR            
072400           MOVE NEJ                    TO SW-INPUT-RAETT                  
072500         END-IF                                                           
072600       END-IF                                                             
072700     END-IF                                                               
072800     EJECT                                                                
072900******************************************************************        
073000*  AVTALSKOD = 1 FÅR ENDAST UPPDATERAS AV BEHÖRIGA MED PF23,     *        
073100*  DVS EJ MED PF11.                                              *        
073200******************************************************************        
073300                                                                          
073400     IF MID-KDAVT NOT = ALL '+'                                           
073500       IF MFS-UPD-V                                                       
073600         IF MID-KDAVT = '0' OR '1' OR '2' OR '3' OR '4'                   
073700           MOVE MFS-NUM-FAELT-RAETT    TO MOD-KDAVT-IN-ATTR               
073800         ELSE                                                             
073900           MOVE MFS-NUM-FAELT-FEL      TO MOD-KDAVT-IN-ATTR               
074000           MOVE NEJ                    TO SW-INPUT-RAETT                  
074100         END-IF                                                           
074200         MOVE MFS-ROER-EJ-FAELT          TO MOD-KDAVT-IN                  
074300       ELSE                                                               
074400         IF MFS-UPDATE                                                    
074500           IF MID-KDAVT = '0' OR '2' OR '3' OR '4'                        
074600             MOVE MFS-NUM-FAELT-RAETT  TO MOD-KDAVT-IN-ATTR               
074700           ELSE                                                           
074800             MOVE MFS-NUM-FAELT-FEL    TO MOD-KDAVT-IN-ATTR               
074900             MOVE NEJ                  TO SW-INPUT-RAETT                  
075000           END-IF                                                         
075100           MOVE MFS-ROER-EJ-FAELT        TO MOD-KDAVT-IN                  
075200         END-IF                                                           
075300       END-IF                                                             
075400     ELSE                                                                 
075500       MOVE MFS-RENSA-FAELT            TO MOD-KDAVT-IN                    
075600     END-IF                                                               
075700                                                                          
075800     IF MID-IDINK NOT = ALL '+'                                           
075900        IF MID-IDINK (1:3) NUMERIC                                        
076000           MOVE MFS-NUM-FAELT-RAETT     TO MOD-IDINK-IN-ATTR              
076100        ELSE                                                              
076200           IF MID-IDINK (2:3) NUMERIC                                     
076300              MOVE MFS-NUM-FAELT-RAETT  TO MOD-IDINK-IN-ATTR              
076400           ELSE                                                           
076500              MOVE MFS-NUM-FAELT-FEL    TO MOD-IDINK-IN-ATTR              
076600              MOVE NEJ                  TO SW-INPUT-RAETT                 
076700           END-IF                                                         
076800        END-IF                                                            
076900        MOVE MFS-ROER-EJ-FAELT          TO MOD-IDINK-IN                   
077000     ELSE                                                                 
077100        MOVE MFS-RENSA-FAELT            TO MOD-IDINK-IN                   
077200     END-IF                                                               
077300     EJECT                                                                
077400                                                                          
077500******************************************************************        
077600*  FLNYBER       FÅR ENDAST UPPDATERAS AV BEHÖRIGA MED PF23,     *        
077700*  DVS EJ MED PF11.                                              *        
077800******************************************************************        
077900                                                                          
078000     IF MID-FLNYBER NOT = ALL '+'                                         
078100       IF MFS-UPD-V                                                       
078200        IF MID-FLNYBER = JA OR YES OR NEJ                                 
078300           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLNYBER-IN-ATTR            
078400        ELSE                                                              
078500           MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLNYBER-IN-ATTR            
078600           MOVE NEJ                     TO SW-INPUT-RAETT                 
078700        END-IF                                                            
078800        MOVE MFS-ROER-EJ-FAELT          TO MOD-FLNYBER-IN                 
078900       ELSE                                                               
079000           MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLNYBER-IN-ATTR            
079100           MOVE NEJ                     TO SW-INPUT-RAETT                 
079200       END-IF                                                             
079300     ELSE                                                                 
079400        MOVE MFS-RENSA-FAELT            TO MOD-FLNYBER-IN                 
079500     END-IF                                                               
079600                                                                          
079700     EJECT                                                                
079800     IF MID-KVVECKOR-LVAR NOT = ALL '+'                                   
079900       IF MID-KVVECKOR-LVAR(1:1) = ZERO                                   
080000         IF MID-KVVECKOR-LVAR(2:1) = ZERO                                 
080100           IF MID-KVVECKOR-LVAR(3:1) = ZERO                               
080200             MOVE MID-KVVECKOR-LVAR(4:1) TO                               
080300                                         WS-KVVECKOR-LVAR-2(2:1)          
080400           ELSE                                                           
080500             IF MID-KVVECKOR-LVAR(3:1) > ZERO                             
080600               MOVE MID-KVVECKOR-LVAR(3:2) TO                             
080700                                          WS-KVVECKOR-LVAR-2(1:2)         
080800             ELSE                                                         
080900               MOVE MID-KVVECKOR-LVAR(4:1) TO                             
081000                                           WS-KVVECKOR-LVAR-2(3:1)        
081100             END-IF                                                       
081200           END-IF                                                         
081300         ELSE                                                             
081400           MOVE ZERO                   TO WS-KVVECKOR-LVAR-2(1:1)         
081500           MOVE MID-KVVECKOR-LVAR(2:1) TO                                 
081600                                        WS-KVVECKOR-LVAR-2(2:1)           
081700           MOVE MID-KVVECKOR-LVAR(4:1) TO                                 
081800                                        WS-KVVECKOR-LVAR-2(3:1)           
081900         END-IF                                                           
082000       ELSE                                                               
082100         MOVE MID-KVVECKOR-LVAR(1:2) TO WS-KVVECKOR-LVAR-2(1:2)           
082200         MOVE MID-KVVECKOR-LVAR(4:1) TO WS-KVVECKOR-LVAR-2(3:1)           
082300       END-IF                                                             
082400       MOVE MFS-NUM-FAELT-RAETT       TO MOD-KVVECKOR-LVAR-IN-ATTR        
082500       MOVE MFS-ROER-EJ-FAELT         TO MOD-KVVECKOR-LVAR-IN             
082600     ELSE                                                                 
082700        MOVE MFS-RENSA-FAELT          TO MOD-KVVECKOR-LVAR-IN             
082800     END-IF                                                               
082900     EJECT                                                                
083000                                                                          
083100     IF MID-FLJIT NOT = ALL '+'                                           
083200        IF MID-FLJIT = JA OR YES OR NEJ                                   
083300           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLJIT-IN-ATTR              
083400        ELSE                                                              
083500           MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLJIT-IN-ATTR              
083600           MOVE NEJ                     TO SW-INPUT-RAETT                 
083700        END-IF                                                            
083800        MOVE MFS-ROER-EJ-FAELT          TO MOD-FLJIT-IN                   
083900     ELSE                                                                 
084000        MOVE MFS-RENSA-FAELT            TO MOD-FLJIT-IN                   
084100     END-IF                                                               
084200                                                                          
084300     IF MID-TISTODAT-LARM NOT = ALL '+'                                   
084400       INSPECT MID-TISTODAT-LARM REPLACING LEADING SPACE BY ZERO          
084500       IF MID-TISTODAT-LARM NUMERIC                                       
084600         IF MID-TISTODAT-LARM = 999999 OR ZEROES                          
084700           MOVE MFS-NUM-FAELT-RAETT TO MOD-TISTODAT-LARM-IN-ATTR          
084800         ELSE                                                             
084900           MOVE MID-TISTODAT-LARM     TO WS-START-DATUM                   
085000           MOVE WS-START-DATUM        TO DAT-I-TIDATUM                    
085100           MOVE 'AAMMDD'              TO DAT-KDDATFORM                    
085200           CALL WDATKONV USING DAT-KDDATFORM, DAT-I-TIDATUM               
085300                               DAT-O-TIDATUM, DAT-KDSVAR                  
085400           IF DAT-KDSVAR-OK                                               
085500             MOVE DAGENS-DATUM       TO W-DAGENS-DATUM-PL-ONE-YEAR        
085600             ADD +1               TO W-DAGENS-DATUM-PL-ONE-YEAR-YY        
085700             IF WS-START-DATUM NOT > DAGENS-DATUM                         
085800               MOVE ERR-FUTURE-DATE   TO MED-IDMFSFEL                     
085900               MOVE MFS-NUM-FAELT-FEL TO MOD-TISTODAT-LARM-IN-ATTR        
086000               MOVE NEJ               TO SW-INPUT-RAETT                   
086100             ELSE                                                         
086200               IF WS-START-DATUM > W-DAGENS-DATUM-PL-ONE-YEAR             
086300                 MOVE ERR-ONE-YEAR-IN-FUTURE TO MED-IDMFSFEL              
086400                 MOVE MFS-NUM-FAELT-FEL                                   
086500                                      TO MOD-TISTODAT-LARM-IN-ATTR        
086600                 MOVE NEJ             TO SW-INPUT-RAETT                   
086700               ELSE                                                       
086800                 MOVE MFS-NUM-FAELT-RAETT                                 
086900                                      TO MOD-TISTODAT-LARM-IN-ATTR        
087000               END-IF                                                     
087100             END-IF                                                       
087200           ELSE                                                           
087300             MOVE MFS-NUM-FAELT-FEL   TO MOD-TISTODAT-LARM-IN-ATTR        
087400             MOVE NEJ                 TO SW-INPUT-RAETT                   
087500           END-IF                                                         
087600         END-IF                                                           
087700       ELSE                                                               
087800         MOVE MFS-NUM-FAELT-FEL       TO MOD-TISTODAT-LARM-IN-ATTR        
087900         MOVE NEJ                     TO SW-INPUT-RAETT                   
088000       END-IF                                                             
088100     ELSE                                                                 
088200       MOVE MFS-RENSA-FAELT           TO MOD-TISTODAT-LARM-IN-ATTR        
088300     END-IF                                                               
088400                                                                          
088500     IF MID-KVKP NOT = ALL '+'                                            
088600        IF MID-KVKP NUMERIC                                               
088700           MOVE MFS-NUM-FAELT-RAETT     TO MOD-KVKP-IN-ATTR               
088800        ELSE                                                              
088900           MOVE MFS-NUM-FAELT-FEL       TO MOD-KVKP-IN-ATTR               
089000           MOVE NEJ                     TO SW-INPUT-RAETT                 
089100        END-IF                                                            
089200        MOVE MFS-ROER-EJ-FAELT          TO MOD-KVKP-IN                    
089300     ELSE                                                                 
089400        MOVE MFS-RENSA-FAELT            TO MOD-KVKP-IN                    
089500     END-IF                                                               
089600     EJECT                                                                
089700     IF MID-FLMANKP NOT = ALL '+'                                         
089800        IF MID-FLMANKP = JA OR YES                                        
089900           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLMANKP-IN-ATTR            
090000        ELSE                                                              
090100           IF MID-FLMANKP = NEJ                                           
090200              IF MID-KVKP NUMERIC                                         
090300                 MOVE MFS-ALFA-FAELT-FEL  TO MOD-FLMANKP-IN-ATTR          
090400                 MOVE NEJ                 TO SW-INPUT-RAETT               
090500              ELSE                                                        
090600                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLMANKP-IN-ATTR         
090700              END-IF                                                      
090800           ELSE                                                           
090900              MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLMANKP-IN-ATTR         
091000              MOVE NEJ                     TO SW-INPUT-RAETT              
091100           END-IF                                                         
091200        END-IF                                                            
091300        MOVE MFS-ROER-EJ-FAELT             TO MOD-FLMANKP-IN              
091400     ELSE                                                                 
091500        MOVE MFS-RENSA-FAELT               TO MOD-FLMANKP-IN              
091600     END-IF                                                               
091700                                                                          
091800     EJECT                                                                
091900     PERFORM BAA-KOLLA-TIFINLV                                            
092000                                                                          
092100     IF MID-TIURPROD NOT = ALL '+'                                        
092200        IF MID-TIURPROD NUMERIC                                           
092300           MOVE MFS-NUM-FAELT-RAETT     TO MOD-TIURPROD-IN-ATTR           
092400        ELSE                                                              
092500           MOVE MFS-NUM-FAELT-FEL       TO MOD-TIURPROD-IN-ATTR           
092600           MOVE NEJ                     TO SW-INPUT-RAETT                 
092700        END-IF                                                            
092800        MOVE MFS-ROER-EJ-FAELT          TO MOD-TIURPROD-IN                
092900     ELSE                                                                 
093000        MOVE MFS-RENSA-FAELT            TO MOD-TIURPROD-IN                
093100     END-IF                                                               
093200                                                                          
093300     IF MID-KDSOP NOT = ALL '+'                                           
093400        IF MID-KDSOP = '0' OR '1'                                         
093500           MOVE MFS-NUM-FAELT-RAETT  TO MOD-KDSOP-IN-ATTR                 
093600        ELSE                                                              
093700           MOVE MFS-NUM-FAELT-FEL    TO MOD-KDSOP-IN-ATTR                 
093800           MOVE NEJ                  TO SW-INPUT-RAETT                    
093900        END-IF                                                            
094000        MOVE MFS-ROER-EJ-FAELT       TO MOD-KDSOP-IN                      
094100     ELSE                                                                 
094200        MOVE MFS-RENSA-FAELT         TO MOD-KDSOP-IN                      
094300     END-IF                                                               
094400                                                                          
094500     IF MID-KVEOP NOT = ALL '+'                                           
094600        IF MID-KVEOP NUMERIC                                              
094700           MOVE MFS-NUM-FAELT-RAETT  TO MOD-KVEOP-IN-ATTR                 
094800        ELSE                                                              
094900           MOVE MFS-NUM-FAELT-FEL    TO MOD-KVEOP-IN-ATTR                 
095000           MOVE NEJ                  TO SW-INPUT-RAETT                    
095100        END-IF                                                            
095200        MOVE MFS-ROER-EJ-FAELT       TO MOD-KVEOP-IN                      
095300     ELSE                                                                 
095400        MOVE MFS-RENSA-FAELT         TO MOD-KVEOP-IN                      
095500     END-IF                                                               
095600                                                                          
095700     IF MID-FLBRAND      NOT = ALL '+'                                    
095800        IF MID-FLBRAND          = JA OR YES OR NEJ                        
095900           MOVE MFS-ALFA-FAELT-RAETT    TO                                
096000                                        MOD-FLBRAND-IN-ATTR               
096100        ELSE                                                              
096200           MOVE MFS-ALFA-FAELT-FEL      TO                                
096300                                        MOD-FLBRAND-IN-ATTR               
096400           MOVE NEJ                     TO SW-INPUT-RAETT                 
096500        END-IF                                                            
096600        MOVE MFS-ROER-EJ-FAELT          TO MOD-FLBRAND-IN                 
096700     ELSE                                                                 
096800        MOVE MFS-RENSA-FAELT            TO MOD-FLBRAND-IN                 
096900     END-IF                                                               
097000                                                                          
097100     IF MID-FLBSNES NOT = ALL '+'                                         
097200        IF MID-FLBSNES          = JA OR YES OR NEJ                        
097300           MOVE MFS-ALFA-FAELT-RAETT    TO                                
097400                                      MOD-FLBSNES-IN-ATTR                 
097500        ELSE                                                              
097600           MOVE MFS-ALFA-FAELT-FEL      TO                                
097700                                      MOD-FLBSNES-IN-ATTR                 
097800           MOVE NEJ                     TO SW-INPUT-RAETT                 
097900        END-IF                                                            
098000        MOVE MFS-ROER-EJ-FAELT          TO MOD-FLBSNES-IN                 
098100     ELSE                                                                 
098200        MOVE MFS-RENSA-FAELT            TO MOD-FLBSNES-IN                 
098300     END-IF                                                               
098400                                                                          
098500     IF MID-FLRELSP NOT = ALL '+'                                         
098600        IF MID-FLRELSP = JA OR YES OR NEJ                                 
098700           MOVE MFS-ALFA-FAELT-RAETT    TO MOD-FLRELSP-IN-ATTR            
098800        ELSE                                                              
098900           MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLRELSP-IN-ATTR            
099000           MOVE NEJ                     TO SW-INPUT-RAETT                 
099100        END-IF                                                            
099200        MOVE MFS-ROER-EJ-FAELT          TO MOD-FLRELSP-IN                 
099300     ELSE                                                                 
099400        MOVE MFS-RENSA-FAELT            TO MOD-FLRELSP-IN                 
099500     END-IF                                                               
099600                                                                          
099700     IF MID-FLRELSP = ALL '+'                                             
099800        CONTINUE                                                          
099900     ELSE                                                                 
100000        PERFORM S01-TESTA-USERID                                          
100100        IF SW-GODKAENT-ID = JA                                            
100200           CONTINUE                                                       
100300        ELSE                                                              
100400           MOVE MFS-ALFA-FAELT-FEL      TO MOD-FLRELSP-IN-ATTR            
100500           MOVE NEJ                     TO SW-INPUT-RAETT                 
100600        END-IF                                                            
100700     END-IF                                                               
100800                                                                          
100900     IF MID-REDIRLEV-C1 NOT = ALL '+'                                     
101000        MOVE MID-REDIRLEV-C1            TO DEC-IDFRIDATA                  
101100        MOVE 1                          TO DEC-KVHELTAL                   
101200        MOVE 2                          TO DEC-KVDECIMAL                  
101300        CALL WDECEDIT USING DEC-WDECAREA                                  
101400                                                                          
101500        IF DEC-KDSVAR-OK                                                  
101600           IF DEC-IDEDITDATA > 1                                          
101700              MOVE MFS-NUM-FAELT-FEL    TO MOD-REDIRLEV-C1-IN-ATTR        
101800              MOVE NEJ                  TO SW-INPUT-RAETT                 
101900           ELSE                                                           
102000              MOVE MFS-NUM-FAELT-RAETT  TO MOD-REDIRLEV-C1-IN-ATTR        
102100              MOVE DEC-IDEDITDATA       TO WS-REDIRLEV-C1                 
102200           END-IF                                                         
102300        ELSE                                                              
102400           MOVE MFS-NUM-FAELT-FEL    TO MOD-REDIRLEV-C1-IN-ATTR           
102500           MOVE NEJ                  TO SW-INPUT-RAETT                    
102600        END-IF                                                            
102700        MOVE MFS-ROER-EJ-FAELT          TO MOD-REDIRLEV-C1-IN             
102800     ELSE                                                                 
102900        MOVE MFS-RENSA-FAELT            TO MOD-REDIRLEV-C1-IN             
103000     END-IF                                                               
103100                                                                          
103200     EJECT                                                                
103300                                                                          
103400     MOVE +1 TO INDX                                                      
103500     PERFORM UNTIL INDX > MAX-TILEVDAGAR                                  
103600       IF MID-DAG-POS(INDX) NOT = ALL '+'                                 
103700         IF MID-DAG-POS(INDX) NOT = SPACE                                 
103800                                                                          
103900           IF MID-DAG-POS(INDX) = 'MÅ' OR 'TI' OR                         
104000                                   'ON' OR 'TO' OR 'FR' OR                
104100                                  'MO' OR 'TU' OR                         
104200                                   'WE' OR 'TH'                           
104300             MOVE MFS-ALFA-FAELT-RAETT TO                                 
104400                               MOD-DAG-POS-ATTR(INDX)                     
104500           ELSE                                                           
104600             MOVE MFS-ALFA-FAELT-FEL   TO                                 
104700                               MOD-DAG-POS-ATTR(INDX)                     
104800             MOVE NEJ                  TO SW-INPUT-RAETT                  
104900           END-IF                                                         
105000         END-IF                                                           
105100       END-IF                                                             
105200       ADD +1 TO INDX                                                     
105300     END-PERFORM                                                          
105400     EJECT                                                                
105500                                                                          
105600     IF MID-TEARTNOT1 NOT = ALL '+'                                       
105700        MOVE MFS-ALFA-FAELT-RAETT       TO MOD-TEARTNOT1-IN-ATTR          
105800     ELSE                                                                 
105900        MOVE MFS-RENSA-FAELT            TO MOD-TEARTNOT1-IN               
106000     END-IF                                                               
106100                                                                          
106200     IF MID-TEARTNOT2 NOT = ALL '+'                                       
106300        MOVE MFS-ALFA-FAELT-RAETT       TO MOD-TEARTNOT2-IN-ATTR          
106400     ELSE                                                                 
106500        MOVE MFS-RENSA-FAELT            TO MOD-TEARTNOT2-IN               
106600     END-IF                                                               
106700                                                                          
106800     IF SW-INPUT-RAETT = NEJ                                              
106900        PERFORM S02-ROER-EJ-FAELT                                         
107000     END-IF                                                               
107100     .                                                                    
107200     EJECT                                                                
107300                                                                          
107400 BAA-KOLLA-TIFINLV SECTION.                                               
107500                                                                          
107600     IF MID-TIFINLV NOT = ALL '+'                                         
107700        IF MID-TIFINLV NUMERIC                                            
107800           MOVE MID-TIFINLV             TO XX-TIFINLV                     
107900           MOVE 1                       TO XX-DAG                         
108000           MOVE XX-TIFINLV              TO SPAR-TIFINLV-AAVVD             
108100           IF SPAR-TIFINLV-AAVVD = 99991                                  
108200              MOVE MFS-NUM-FAELT-RAETT  TO MOD-TIFINLV-IN-ATTR            
108300              MOVE 99999999             TO SPAR-DAFINLV-AAAAMMDD          
108400              MOVE 99999                TO SPAR-TIFINLV-AAVVD             
108500           ELSE                                                           
108600              MOVE 'AAVVD '             TO DAT-KDDATFORM                  
108700              MOVE SPAR-TIFINLV-AAVVD   TO DAT-I-TIDATUM                  
108800              PERFORM S99-WDATKONV                                        
108900              IF DAT-KDSVAR-OK                                            
109000                 MOVE DAT-TIAA-VECKA    TO SPAR-TIFINLV-AA                
109100                 MOVE DAT-TIVV          TO SPAR-TIFINLV-VV                
109200                 MOVE SPAR-TIFINLV-AAVV-R  TO TMP1-YYWW                   
109300                 MOVE SPAR-DAGENS-AAVV-R   TO TMP2-YYWW                   
109400                 MOVE DAT-TIAAMMDD         TO                             
109500                                      SPAR-DAFINLV-AAAAMMDD               
109600                 MOVE DAT-TISEKEL          TO                             
109700                                      SPAR-DAFINLV-AAAAMMDD (1:2)         
109800                 PERFORM WY2000P3                                         
109900                 IF TMP1-YYWW > TMP2-YYWW                                 
110000                    PERFORM S98-OM-TVA-AAR                                
110100                    MOVE SPAR-TIFINLV-AAVVD TO TMP1-YYWWD                 
110200                    MOVE AAVVD              TO TMP2-YYWWD                 
110300                    IF TMP1-YYWWD NOT < TMP2-YYWWD                        
110400                       MOVE MFS-NUM-FAELT-FEL                             
110500                                        TO MOD-TIFINLV-IN-ATTR            
110600                       MOVE NEJ         TO SW-INPUT-RAETT                 
110700                    ELSE                                                  
110800                       MOVE MFS-NUM-FAELT-RAETT                           
110900                                        TO MOD-TIFINLV-IN-ATTR            
111000                    END-IF                                                
111100                 ELSE                                                     
111200                    MOVE MFS-NUM-FAELT-FEL                                
111300                                        TO MOD-TIFINLV-IN-ATTR            
111400                    MOVE NEJ            TO SW-INPUT-RAETT                 
111500                 END-IF                                                   
111600              ELSE                                                        
111700                 MOVE MFS-NUM-FAELT-FEL                                   
111800                                        TO MOD-TIFINLV-IN-ATTR            
111900                 MOVE NEJ               TO SW-INPUT-RAETT                 
112000              END-IF                                                      
112100              MOVE MFS-ROER-EJ-FAELT    TO MOD-TIFINLV-IN                 
112200           END-IF                                                         
112300        ELSE                                                              
112400           MOVE MFS-NUM-FAELT-FEL       TO MOD-TIFINLV-IN-ATTR            
112500           MOVE NEJ                     TO SW-INPUT-RAETT                 
112600        END-IF                                                            
112700        MOVE MFS-ROER-EJ-FAELT          TO MOD-TIFINLV-IN                 
112800     ELSE                                                                 
112900        MOVE MFS-RENSA-FAELT            TO MOD-TIFINLV-IN                 
113000     END-IF                                                               
113100     .                                                                    
113200     EJECT                                                                
113300                                                                          
113400 C-KOLLA-MOT-BASEN SECTION.                                               
113500     SKIP3                                                                
113600******************************************************************        
113700*  TEST OM C2-SEGMENT FINNS UTFÖRS NÄR     IFYLLD                *        
113800*  I VISSA LÄGEN SKALL OM INTE C2 FINNS ANROP PÅ SUBMODULEN      *        
113900*  W200C2UP GÖRAS, SOM SKAPAR C2-SEGMENT MED DEFAULT-VÄRDEN.     *        
114000******************************************************************        
114100*                                                                         
114200     MOVE JA                         TO SW-BASEN-RAETT                    
114300                                                                          
114400     PERFORM IMS-GNP-ARTC11                                               
114500                                                                          
114600     IF  MID-TISLUTKP NOT = ALL '+'                                       
114700     AND MID-TISLUTKP > ZERO                                              
114800         IF CLAG-KVSLUTKP = ZERO AND MID-KVSLUTKP = ALL '+'               
114900         OR MID-KVSLUTKP = ZERO                                           
115000             MOVE MFS-NUM-FAELT-FEL TO MOD-TISLUTKP-IN-ATTR               
115100             MOVE NEJ               TO SW-BASEN-RAETT                     
115200         ELSE                                                             
115300             CONTINUE                                                     
115400         END-IF                                                           
115500     END-IF                                                               
115600                                                                          
115700     IF WS-REDIRLEV-C1 > ZERO                                             
115800       PERFORM CA-GET-PRISLISTA                                           
115900       IF ART-IDLEVNR        = '10987' OR 'BQ8VA'                         
116000         IF CLAG-IDSTATNR (3) = ZERO  OR                                  
116100            CLAG-KDARTURS     = SPACE OR                                  
116200            CLAG-VKART        = 0     OR                                  
116300            CLAG-VLARTNTO     = 0     OR                                  
116400             WS-PRARTBES-PR   = 0                                         
116500           MOVE MFS-NUM-FAELT-FEL TO MOD-REDIRLEV-C1-IN-ATTR              
116600           MOVE NEJ               TO SW-BASEN-RAETT                       
116700         END-IF                                                           
116800       ELSE                                                               
116900         IF CLAG-IDSTATNR (3) = ZERO  OR                                  
117000            CLAG-KDARTURS     = SPACE OR                                  
117100            CLAG-VKART        = 0     OR                                  
117200            CLAG-VLARTNTO     = 0     OR                                  
117300             WS-PRARTBES-PR   = 0     OR                                  
117400            CLAG-KDAVT        = 0                                         
117500           MOVE MFS-NUM-FAELT-FEL TO MOD-REDIRLEV-C1-IN-ATTR              
117600           MOVE NEJ               TO SW-BASEN-RAETT                       
117700         END-IF                                                           
117800       END-IF                                                             
117900     END-IF                                                               
118000                                                                          
118100     IF MID-FLRELSP = ALL '+'                                             
118200        IF MID-REDIRLEV-C1 = ALL '+'                                      
118300           CONTINUE                                                       
118400        ELSE                                                              
118500           IF CLAG-FLRELSP = JA                                           
118600              IF WS-REDIRLEV-C1 > 0.01                                    
118700                 MOVE MFS-NUM-FAELT-FEL TO MOD-REDIRLEV-C1-IN-ATTR        
118800                 MOVE NEJ TO SW-BASEN-RAETT                               
118900              END-IF                                                      
119000           END-IF                                                         
119100        END-IF                                                            
119200     ELSE                                                                 
119300        IF MID-REDIRLEV-C1 = ALL '+'                                      
119400           IF MID-FLRELSP = JA OR YES                                     
119500              IF CLAG-REDIRLEV > 0.01                                     
119600                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRELSP-IN-ATTR           
119700                 MOVE NEJ TO SW-BASEN-RAETT                               
119800              END-IF                                                      
119900           END-IF                                                         
120000        ELSE                                                              
120100           IF MID-FLRELSP = JA OR YES                                     
120200              IF WS-REDIRLEV-C1 > 0.01                                    
120300                 MOVE MFS-NUM-FAELT-FEL TO MOD-REDIRLEV-C1-IN-ATTR        
120400                 MOVE MFS-ALFA-FAELT-FEL TO MOD-FLRELSP-IN-ATTR           
120500                 MOVE NEJ TO SW-BASEN-RAETT                               
120600              END-IF                                                      
120700           END-IF                                                         
120800        END-IF                                                            
120900     END-IF                                                               
121000                                                                          
121100     IF SW-BASEN-RAETT = NEJ                                              
121200        PERFORM S02-ROER-EJ-FAELT                                         
121300        MOVE ERR-CORR-HILITE-FLDS TO MED-IDMFSFEL                         
121400        CALL WMEDKONV USING MED-WMEDAREA                                  
121500        MOVE MED-MFSFEL TO MOD-TEMFSFEL                                   
121600     END-IF                                                               
121700     .                                                                    
121800     EJECT                                                                
121900                                                                          
122000 CA-GET-PRISLISTA SECTION.                                                
122100                                                                          
122200     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-AAAAMMDD                   
122300     COMPUTE W-DAPRLIST = 99999999 - DAGENS-AAAAMMDD                      
122400     PERFORM IMS-GNP-ARTC21                                               
122500     IF SEGMENT-SAKNAS                                                    
122600       MOVE ZERO                TO WS-PRARTBES-PR                         
122700     ELSE                                                                 
122800       MOVE NEJ                 TO WS-PRARTBES                            
122900       PERFORM UNTIL  SEGMENT-SAKNAS                                      
123000         IF PRL-SUINLEV-PR > ZERO                                         
123100           MOVE PRL-PRARTBES-PR  TO WS-PRARTBES-PR                        
123200           SET SEGMENT-SAKNAS TO TRUE                                     
123300         ELSE                                                             
123400           IF WS-PRARTBES = NEJ                                           
123500             MOVE PRL-PRARTBES-PR TO WS-PRARTBES-PR                       
123600             MOVE JA              TO WS-PRARTBES                          
123700           END-IF                                                         
123800           PERFORM IMS-GNP-ARTC21                                         
123900         END-IF                                                           
124000       END-PERFORM                                                        
124100     END-IF                                                               
124200     .                                                                    
124300     EJECT                                                                
124400                                                                          
124500 D-UPPDATERA SECTION.                                                     
124600     SKIP2                                                                
124700                                                                          
124800     PERFORM S02-ROER-EJ-FAELT                                            
124900                                                                          
125000     IF MID-REDIRLEV-C1 = ALL '+'                                         
125100          CONTINUE                                                        
125200     ELSE                                                                 
125300        IF MID-REDIRLEV-C1 NOT = ALL '+'                                  
125400           PERFORM IMS-GHNP-ARTC11                                        
125500           MOVE WS-REDIRLEV-C1          TO CLAG-REDIRLEV                  
125600                                           MOD-REDIRLEV-C1                
125700           MOVE MFS-ADD-LYS-UPP-FAELT   TO MOD-REDIRLEV-C1-ATTR           
125800           PERFORM IMS-REPLACE-ARTC-11                                    
125900        ELSE                                                              
126000           CONTINUE                                                       
126100        END-IF                                                            
126200                                                                          
126300     END-IF                                                               
126400                                                                          
126500     EJECT                                                                
126600     IF MID-FLJIT NOT = ALL '+'                                           
126700        PERFORM IMS-GET-ARTC01                                            
126800        PERFORM IMS-GHNP-ARTC11                                           
126900                                                                          
126910        IF MID-FLJIT      = YES OR JA                                     
126920           MOVE JA              TO CLAG-FLJIT                             
126930                                   MOD-FLJIT                              
126940        ELSE                                                              
126950           MOVE MID-FLJIT       TO CLAG-FLJIT                             
126960                                   MOD-FLJIT                              
126970        END-IF                                                            
127200        MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLJIT-ATTR                      
127300        PERFORM IMS-REPLACE-ARTC-11                                       
127400     END-IF                                                               
127500                                                                          
127600     IF MID-KDAVT    = ALL '+'   AND                                      
127700        MID-FLFSP    = ALL '+'   AND                                      
127800        MID-IDINK    = ALL '+'   AND                                      
127900        MID-KVKP     = ALL '+'   AND                                      
128000        MID-FLMANKP  = ALL '+'   AND                                      
128100        MID-KVSLUTKP = ALL '+'   AND                                      
128200        MID-KDKSP    = ALL '+'   AND                                      
128300        MID-TISLUTKP = ALL '+'   AND                                      
128400        MID-FLNYBER  = ALL '+'   AND                                      
128500        MID-KVVECKOR-LVAR  = ALL '+'  AND                                 
128600        MID-FLRELSP  = ALL '+'   AND                                      
128700        MID-TIFINLV  = ALL '+'   AND                                      
128800        MID-TIURPROD = ALL '+'   AND                                      
128900        MID-KDSOP= ALL '+'       AND                                      
129000        MID-KVEOP= ALL '+'       AND                                      
129100        MID-FLBRAND = ALL '+'       AND                                   
129200        MID-FLBSNES         = ALL '+'                                     
129300        MOVE SPACE                   TO WS-IDLEVNR-SPAR                   
129400     ELSE                                                                 
129500*FIX*                                                                     
129600        IF MID-TIURPROD NOT = ALL '+'                                     
129700        OR MID-TIFINLV  NOT = ALL '+'                                     
129800        OR MID-KDSOP NOT = ALL '+'                                        
129900        OR MID-KVEOP NOT = ALL '+'                                        
130000        OR MID-FLBRAND NOT = ALL '+'                                      
130100        OR MID-FLBSNES NOT = ALL '+'                                      
130200           PERFORM IMS-GHU-ARTC01                                         
130300                                                                          
130400           IF MID-TIURPROD NOT = ALL '+'                                  
130500              MOVE MID-TIURPROD          TO ART-TIURPROD                  
130600                                            MOD-TIURPROD                  
130700              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIURPROD-ATTR             
130800           END-IF                                                         
130900           IF MID-TIFINLV  NOT = ALL '+'                                  
131000              MOVE XX-TIFINLV            TO ART-TIFINLV                   
131100                                            MOD-TIFINLV                   
131200              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TIFINLV-ATTR              
131300                                                                          
131400              PERFORM DH-EV-SKAPA-LARM                                    
131500              PERFORM DO-KOLLA-UPPDAT-WDK7                                
131600           END-IF                                                         
131700           IF MID-KDSOP NOT = ALL '+'                                     
131800              MOVE MID-KDSOP             TO ART-KDSOP                     
131900                                            MOD-KDSOP                     
132000              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDSOP-ATTR                
132100           END-IF                                                         
132200           IF MID-KVEOP NOT = ALL '+'                                     
132300              MOVE MID-KVEOP             TO ART-KVEOP                     
132400                                            MOD-KVEOP                     
132500              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVEOP-ATTR                
132600           END-IF                                                         
132700           IF MID-FLBRAND NOT = ALL '+'                                   
132710              IF MID-FLBRAND    = YES OR JA                               
132720                 MOVE YES             TO ART-FLBRAND                      
132721                                         MOD-FLBRAND                      
132730              ELSE                                                        
132740                 MOVE MID-FLBRAND     TO ART-FLBRAND                      
132741                                         MOD-FLBRAND                      
132750              END-IF                                                      
133000              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLBRAND-ATTR              
133100           END-IF                                                         
133200           IF MID-FLBSNES NOT = ALL '+'                                   
133210              IF MID-FLBSNES    = YES OR JA                               
133220                 MOVE YES             TO ART-FLBSNES                      
133230                                         MOD-FLBSNES                      
133240              ELSE                                                        
133250                 MOVE MID-FLBSNES     TO ART-FLBSNES                      
133260                                         MOD-FLBSNES                      
133270              END-IF                                                      
133500              MOVE MFS-ADD-LYS-UPP-FAELT TO                               
133600                                         MOD-FLBSNES-ATTR                 
133700           END-IF                                                         
133800           PERFORM IMS-REPLACE-ARTC-01                                    
133900        END-IF                                                            
134000*FIX*                                                                     
134100        PERFORM IMS-GET-ARTC01                                            
134200                                                                          
134300        MOVE ART-IDLEVNR             TO WS-IDLEVNR-SPAR                   
134400        PERFORM IMS-GHNP-ARTC11                                           
134500        MOVE CLAG-KDAVT              TO WS-KDAVT-SPAR                     
134600        MOVE CLAG-KDHF               TO WS-KDHF-SPAR                      
134700                                                                          
134800        IF MID-IDINK NOT = ALL '+'                                        
134900           MOVE MID-IDINK            TO CLAG-IDINK                        
135000                                        MOD-IDINK                         
135100           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-IDINK-ATTR                   
135200        END-IF                                                            
135300                                                                          
135400        IF MID-FLNYBER NOT = ALL '+'                                      
135410           IF MID-FLNYBER    = YES OR JA                                  
135420              MOVE JA              TO CLAG-FLNYBER                        
135430                                      MOD-FLNYBER                         
135440           ELSE                                                           
135450              MOVE MID-FLNYBER     TO CLAG-FLNYBER                        
135460                                      MOD-FLNYBER                         
135470           END-IF                                                         
135700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLNYBER-ATTR                 
135800        END-IF                                                            
135900                                                                          
136000        IF MID-KVVECKOR-LVAR NOT = ALL '+'                                
136100           MOVE WS-KVVECKOR-LVAR-2  TO   CLAG-KVVECKOR-LVAR               
136200           MOVE WS-KVVECKOR-LVAR-2(1:2) TO WS-KVVECKOR-LVAR-3(1:2)        
136300           MOVE '.'                     TO WS-KVVECKOR-LVAR-3(3:1)        
136400           MOVE WS-KVVECKOR-LVAR-2(3:1) TO WS-KVVECKOR-LVAR-3(4:1)        
136500           MOVE WS-KVVECKOR-LVAR-3      TO MOD-KVVECKOR-LVAR-UT           
136600*          MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVVECKOR-LVAR-IN-ATTR        
136700        END-IF                                                            
136800                                                                          
136900     EJECT                                                                
137000        IF MID-KVKP NOT = ALL '+'                                         
137100           MOVE MID-KVKP             TO CLAG-KVKP                         
137200                                        MOD-KVKP                          
137300           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVKP-ATTR                    
137400           MOVE JA                    TO CLAG-FLMANKP                     
137500                                         MOD-FLMANKP                      
137600           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLMANKP-ATTR                 
137700        END-IF                                                            
137800                                                                          
137900        IF MID-FLFSP NOT = ALL '+'                                        
137910           IF MID-FLFSP      = YES OR JA                                  
137920              MOVE JA              TO CLAG-FLFSP                          
137930                                      MOD-FLFSP                           
137940           ELSE                                                           
137950              MOVE MID-FLFSP       TO CLAG-FLFSP                          
137960                                      MOD-FLFSP                           
137970           END-IF                                                         
138200           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLFSP-ATTR                   
138300        END-IF                                                            
138400                                                                          
138500        IF MID-FLMANKP NOT = ALL '+'                                      
138510           IF MID-FLMANKP    = YES OR JA                                  
138520              MOVE JA              TO CLAG-FLMANKP                        
138530                                      MOD-FLMANKP                         
138540           ELSE                                                           
138550              MOVE MID-FLMANKP     TO CLAG-FLMANKP                        
138560                                      MOD-FLMANKP                         
138570           END-IF                                                         
138800           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLMANKP-ATTR                 
138900        END-IF                                                            
139000                                                                          
139100        IF MID-KVSLUTKP NOT = ALL '+'                                     
139200           IF MID-KVSLUTKP = ZERO                                         
139300              MOVE ZERO              TO CLAG-KVSLUTKP                     
139400                                        CLAG-TISLUTKP                     
139500              MOVE MFS-RENSA-FAELT   TO MOD-KVSLUTKP                      
139600                                        MOD-TISLUTKP                      
139700              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVSLUTKP-ATTR             
139800                                            MOD-TISLUTKP-ATTR             
139900           ELSE                                                           
140000              MOVE MID-KVSLUTKP         TO CLAG-KVSLUTKP                  
140100                                           MOD-KVSLUTKP                   
140200              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KVSLUTKP-ATTR             
140300           END-IF                                                         
140400        END-IF                                                            
140500                                                                          
140600                                                                          
140700        IF MID-KDKSP NOT = ALL '+'                                        
140800           MOVE MID-KDKSP             TO WS-KDKSP                         
140900           MOVE WS-KDKSP              TO CLAG-KDKSP                       
141000           MOVE MID-KDKSP             TO MOD-KDKSP-UT                     
141100           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDKSP-ATTR-UT                
141200           IF WS-KDKSP = 3                                                
141300             MOVE 'AAMMDD'     TO DAT-KDDATFORM                           
141400             MOVE DAGENS-DATUM TO DAT-I-TIDATUM                           
141500             CALL WDATKONV USING DAT-KDDATFORM                            
141600                                   DAT-I-TIDATUM                          
141700                                   DAT-O-TIDATUM                          
141800                                   DAT-KDSVAR                             
141900                                                                          
142000             IF DAT-KDSVAR-OK                                             
142100                MOVE DAT-TIAAVV-GRP      TO W-TIAAVV                      
142200                MOVE W-TIAAVV            TO WS-TIAAVV                     
142300                MOVE 10                  TO W-ANTAL-VECKOR                
142400                CALL W009VADD USING                                       
142500                               WS-TIAAVV W-ANTAL-VECKOR                   
142600                MOVE WS-TIAAVV           TO CLAG-TIBESRPT-PAAM            
142700                PERFORM IMS-REPLACE-ARTC-11                               
142800             ELSE                                                         
142900                CALL FELLOG                                               
143000             END-IF                                                       
143100             PERFORM IMS-GET-ARTC01                                       
143200             PERFORM IMS-GHNP-ARTC11                                      
143300           END-IF                                                         
143400           IF WS-KDKSP = 3                                                
143500             PERFORM S28-SKAPA-B65-TRANS                                  
143600             PERFORM IMS-GET-ARTC01                                       
143700             PERFORM IMS-GHNP-ARTC11                                      
143800           END-IF                                                         
143900        END-IF                                                            
144000                                                                          
144100        IF MID-TISLUTKP NOT = ALL '+'                                     
144200           IF MID-TISLUTKP > ZERO                                         
144300              MOVE MID-TISLUTKP           TO DAT-I-TIDATUM                
144400              MOVE 'AAVVD '               TO DAT-KDDATFORM                
144500              CALL WDATKONV USING DAT-KDDATFORM                           
144600                                  DAT-I-TIDATUM                           
144700                                  DAT-O-TIDATUM                           
144800                                  DAT-KDSVAR                              
144900              MOVE DAT-TIAAMMDD         TO CLAG-TISLUTKP                  
145000              MOVE MID-TISLUTKP         TO MOD-TISLUTKP                   
145100           ELSE                                                           
145200              MOVE ZERO                 TO CLAG-TISLUTKP                  
145300              MOVE MFS-RENSA-FAELT      TO MOD-TISLUTKP                   
145400           END-IF                                                         
145500           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISLUTKP-ATTR                
145600        END-IF                                                            
145700                                                                          
145800        IF MID-FLRELSP NOT = ALL '+'                                      
145810           IF MID-FLRELSP    = YES OR JA                                  
145820              MOVE JA              TO CLAG-FLRELSP                        
145830                                      MOD-FLRELSP                         
145840           ELSE                                                           
145850              MOVE MID-FLRELSP     TO CLAG-FLRELSP                        
145860                                      MOD-FLRELSP                         
145870           END-IF                                                         
146100           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-FLRELSP-ATTR                 
146200        END-IF                                                            
146300                                                                          
146400        IF MID-KDAVT NOT = ALL '+'                                        
146500           MOVE MID-KDAVT            TO CLAG-KDAVT                        
146600                                        MOD-KDAVT                         
146700           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-KDAVT-ATTR                   
146800           PERFORM DC-SKAPA-HANDELSETR-2213                               
146900           IF MID-KDAVT = ZERO                                            
147000              PERFORM DB-SKAPA-HANDELSETR-2221                            
147100           END-IF                                                         
147200        END-IF                                                            
147300                                                                          
147400                                                                          
147500        PERFORM IMS-REPLACE-ARTC-11                                       
147600     END-IF                                                               
147700                                                                          
147800     EJECT                                                                
147900     IF MID-IDINK NOT = ALL '+'                                           
148000        IF MID-IDINK (1:3) NUMERIC                                        
148100           MOVE MID-IDINK (1:3) TO WS-IDINK                               
148200        ELSE                                                              
148300           MOVE ZERO            TO WS-IDINK                               
148400        END-IF                                                            
148500        IF (WS-IDINK >   99 AND  < 790) OR                                
148600           (WS-IDINK >  799 AND  < 987) OR                                
148700           (WS-IDINK > 987 AND   < 1000)                                  
148800           IF WS-KDAVT-SPAR = 1  AND  MID-KDAVT = ZERO                    
148900              PERFORM IMS-GET-ARTC23                                      
149000              IF SEGMENT-FINNS                                            
149100                 IF AVT-IDAVTAL NOT = ZERO                                
149200                    MOVE AVT-IDAVTAL TO WS-IDAVTAL                        
149300                    PERFORM DA-SKAPA-POST-TILL-INKOEP                     
149400                 ELSE                                                     
149500                    CONTINUE                                              
149600                 END-IF                                                   
149700              END-IF                                                      
149800           END-IF                                                         
149900        END-IF                                                            
150000     END-IF                                                               
150100     EJECT                                                                
150200     IF MID-TEARTNOT1 = ALL '+'  AND                                      
150300        MID-TEARTNOT2 = ALL '+'                                           
150400           CONTINUE                                                       
150500     ELSE                                                                 
150600        IF MID-TEARTNOT1 NOT = ALL '+'                                    
150700           MOVE 1                      TO W-KDNOTTYP                      
150800           PERFORM IMS-GHNP-ARTC25                                        
150900           IF MID-TEARTNOT1  = SPACE                                      
151000              IF SEGMENT-FINNS                                            
151100                 PERFORM IMS-DELETE                                       
151200                 MOVE SPACE            TO MOD-TEARTNOT1-IN                
151300                 MOVE MFS-ADD-LYS-UPP-FAELT TO                            
151400                                          MOD-TEARTNOT1-IN-ATTR           
151500              ELSE                                                        
151600                 CONTINUE                                                 
151700              END-IF                                                      
151800           ELSE                                                           
151900              MOVE MID-TEARTNOT1       TO NOT-TEARTNOT                    
152000                                          MOD-TEARTNOT1-IN                
152100              MOVE 1                   TO NOT-KDNOTTYP                    
152200              IF SEGMENT-FINNS                                            
152300                 PERFORM IMS-REPLACE-ARTC                                 
152400              ELSE                                                        
152500                 PERFORM IMS-INSERT-ARTC25                                
152600              END-IF                                                      
152700              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEARTNOT1-IN-ATTR         
152800           END-IF                                                         
152900        END-IF                                                            
153000                                                                          
153100        IF MID-TEARTNOT2 NOT = ALL '+'                                    
153200           MOVE 2                      TO W-KDNOTTYP                      
153300           PERFORM IMS-GHNP-ARTC25                                        
153400           IF MID-TEARTNOT2  = SPACE                                      
153500              IF SEGMENT-FINNS                                            
153600                 PERFORM IMS-DELETE                                       
153700                 MOVE SPACE            TO MOD-TEARTNOT2-IN                
153800                 MOVE MFS-ADD-LYS-UPP-FAELT TO                            
153900                                        MOD-TEARTNOT2-IN-ATTR             
154000              ELSE                                                        
154100                 CONTINUE                                                 
154200              END-IF                                                      
154300           ELSE                                                           
154400              MOVE MID-TEARTNOT2       TO NOT-TEARTNOT                    
154500                                          MOD-TEARTNOT2-IN                
154600              MOVE 2                   TO NOT-KDNOTTYP                    
154700              IF SEGMENT-FINNS                                            
154800                 PERFORM IMS-REPLACE-ARTC                                 
154900              ELSE                                                        
155000                 PERFORM IMS-INSERT-ARTC25                                
155100              END-IF                                                      
155200              MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TEARTNOT2-IN-ATTR         
155300           END-IF                                                         
155400        END-IF                                                            
155500     END-IF                                                               
155600     EJECT                                                                
155700                                                                          
155800     IF MID-FLFSP         = ALL '+'    AND                                
155900        MID-TISTODAT-LARM = ALL '+'                                       
156000           CONTINUE                                                       
156100     ELSE                                                                 
156200        PERFORM IMS-GHNP-ARTC11                                           
156300     EJECT                                                                
156400                                                                          
156500*       IF MID-KDLTK NOT = ALL '+'                                        
156600*          MOVE 'IDAG  '               TO DAT-KDDATFORM                   
156700*          CALL WDATKONV USING DAT-KDDATFORM                              
156800*                              DAT-I-TIDATUM                              
156900*                              DAT-O-TIDATUM                              
157000*                              DAT-KDSVAR                                 
157100*          MOVE DAT-TIAA               TO WS-AA                           
157200*          MOVE DAT-TIVV               TO WS-VV                           
157300*          MOVE WS-HJALP-AAVV2         TO CLAG-TILTK                      
157400*       END-IF                                                            
157500                                                                          
157600     EJECT                                                                
157700                                                                          
157800        IF MID-TISTODAT-LARM NOT = ALL '+'                                
157900           MOVE MID-TISTODAT-LARM   TO CLAG-TISTODAT-LARM                 
158000                                       MOD-TISTODAT-LARM                  
158100           IF MOD-TISTODAT-LARM = ZEROES                                  
158200              INSPECT MOD-TISTODAT-LARM                                   
158300                      REPLACING LEADING ZERO BY SPACE                     
158400           END-IF                                                         
158500           MOVE MFS-ADD-LYS-UPP-FAELT TO MOD-TISTODAT-LARM-ATTR           
158600        END-IF                                                            
158700                                                                          
158800        PERFORM IMS-REPLACE-ARTC-11                                       
158900     END-IF                                                               
159000                                                                          
159100*PAH                                                                      
159200       IF MID-INPUT-TILEVDAGAR NOT = ALL '+'                              
159300         PERFORM DF-SAETT-NYA-TILEVDAGAR                                  
159400         PERFORM DG-REDIGERA-TILL-SKAERM                                  
159500       ELSE                                                               
159600          MOVE +1 TO INDX                                                 
159700          PERFORM UNTIL INDX > MAX-TILEVDAGAR                             
159800            MOVE MFS-ROER-EJ-FAELT TO MOD-DAG-POS(INDX)                   
159900                                      MOD-SPAR-DAG(INDX)                  
160000            ADD +1 TO INDX                                                
160100          END-PERFORM                                                     
160200       END-IF                                                             
160300*PAH                                                                      
160400                                                                          
160500     PERFORM IMS-GET-BENA01-BSEQ                                          
160600     MOVE 'S  '                      TO W-IDSKYLT                         
160700     PERFORM IMS-GET-BENA11-BSEQ                                          
160800     MOVE BENA-TEXT-BEART            TO MOD-BEART                         
160900                                                                          
161000     MOVE MFS-RENSA-FAELT            TO                                   
161100                                        MOD-FLFSP-IN                      
161200                                        MOD-KVSLUTKP-IN                   
161300                                        MOD-KDKSP-IN                      
161400                                        MOD-IDINK-IN                      
161500                                        MOD-KDAVT-IN                      
161600                                        MOD-TISLUTKP-IN                   
161700                                        MOD-FLNYBER-IN                    
161800                                        MOD-KVVECKOR-LVAR-IN              
161900                                        MOD-FLJIT-IN                      
162000                                        MOD-TISTODAT-LARM-IN              
162100                                        MOD-KVKP-IN                       
162200                                        MOD-FLMANKP-IN                    
162300                                        MOD-TIFINLV-IN                    
162400                                        MOD-TIURPROD-IN                   
162500                                        MOD-KDSOP-IN                      
162600                                        MOD-KVEOP-IN                      
162700                                        MOD-FLBRAND-IN                    
162800                                        MOD-FLBSNES-IN                    
162900                                        MOD-FLRELSP-IN                    
163000                                        MOD-REDIRLEV-C1-IN                
163100                                                                          
163200     EJECT                                                                
163300                                                                          
163400     IF CLAG-IDDC-REF = SPACE                                             
163500        MOVE MFS-FORMATETS-ATTR      TO                                   
163600                                         MOD-KVSLUTKP-IN-ATTR             
163700                                         MOD-KDKSP-IN-ATTR                
163800                                         MOD-TISLUTKP-IN-ATTR             
163900                                         MOD-KVKP-IN-ATTR                 
164000                                         MOD-FLMANKP-IN-ATTR              
164100     END-IF                                                               
164200     MOVE MFS-FORMATETS-ATTR         TO                                   
164300                                        MOD-FLFSP-IN-ATTR                 
164400                                        MOD-IDINK-IN-ATTR                 
164500                                        MOD-KDAVT-IN-ATTR                 
164600                                        MOD-FLNYBER-IN-ATTR               
164700                                        MOD-KVVECKOR-LVAR-IN-ATTR         
164800                                        MOD-FLJIT-IN-ATTR                 
164900                                        MOD-TISTODAT-LARM-IN-ATTR         
165000                                        MOD-FLMANKP-IN-ATTR               
165100                                        MOD-TIFINLV-IN-ATTR               
165200                                        MOD-TIURPROD-IN-ATTR              
165300                                        MOD-KDSOP-IN-ATTR                 
165400                                        MOD-KVEOP-IN-ATTR                 
165500                                        MOD-FLBRAND-IN-ATTR               
165600                                        MOD-FLBSNES-IN-ATTR               
165700                                        MOD-FLRELSP-IN-ATTR               
165800                                        MOD-REDIRLEV-C1-IN-ATTR           
165900                                                                          
166000     MOVE INF-UPDATE-DONE           TO MED-IDMFSINF                       
166100     CALL WMEDKONV USING MED-WMEDAREA                                     
166200     MOVE MED-MFSINF                TO MOD-TEMFSINF                       
166300     .                                                                    
166400     EJECT                                                                
166500 DA-SKAPA-POST-TILL-INKOEP SECTION.                                       
166600     SKIP2                                                                
166700     ACCEPT ZZAC-TIKLOCK  FROM TIME                                       
166800     ACCEPT ZZAC-TIAAMMDD FROM DATE                                       
166900     ADD 1                            TO WS-IDLOGLOP                      
167000     MOVE WS-IDLOGLOP                 TO ZZAC-IDLOGLOP                    
167100     IF MID-IDINK (1:3) NUMERIC                                           
167200        MOVE MID-IDINK (1:3)          TO WS-IDINK                         
167300     ELSE                                                                 
167400        MOVE ZERO                     TO WS-IDINK                         
167500     END-IF                                                               
167600                                                                          
167700     MOVE WS-IDAVTAL-PREFIX TO WS-IDAVTAL-PREFIX-NUM                      
167800     IF MID-IDINK NOT = ALL '+'                                           
167900***        TAG ÄVEN MED NAP-AVTAL, PREFIX = 004                           
168000        IF (WS-IDAVTAL-PREFIX-NUM > 99    AND  < 790)  OR                 
168100           (WS-IDAVTAL-PREFIX-NUM > 799   AND  < 987)  OR                 
168200           (WS-IDAVTAL-PREFIX-NUM > 987   AND  < 1000)  OR                
168300           (WS-IDAVTAL-PREFIX-NUM = 004)                                  
168400*          SKAPAR POST TILL INKÖP  PV                                     
168500           MOVE SPACE                    TO A310-LEVNUM-GODSM             
168600                                            A310-ANT-BESTANN              
168700           MOVE 'RY2'                    TO A310-KT                       
168800           MOVE DAGENS-DATUM             TO A310-DATUM-UTSKR              
168900           IF AVT-IDLEVNR-AVT (5:1) = SPACE                               
169000*****        LEVNUM SKALL TILLS VIDARE VARA NUMERISKT I X(5)              
169100             MOVE ZERO                     TO TALLY                       
169200             INSPECT AVT-IDLEVNR-AVT TALLYING TALLY                       
169300                            FOR CHARACTERS BEFORE INITIAL SPACE           
169400             IF TALLY = ZERO                                              
169500                MOVE ZERO                  TO WS-IDLEVNR-NUM              
169600             ELSE                                                         
169700                MOVE AVT-IDLEVNR-AVT (1:TALLY)                            
169800                                           TO WS-IDLEVNR-NUM              
169900             END-IF                                                       
170000             MOVE WS-IDLEVNR-NUM           TO A310-LEVNUM                 
170100           ELSE                                                           
170200             MOVE AVT-IDLEVNR-AVT          TO A310-LEVNUM                 
170300           END-IF                                                         
170400           MOVE W-IDARTNR                TO WS-IDARTNR-8                  
170500           MOVE WS-IDARTNR-8             TO A310-ARTNR                    
170600                                            W092-SORTBGP                  
170700           MOVE AVT-IDAVTAL              TO WS-IDAVTAL                    
170800           MOVE WS-IDAVTAL-PREFIX        TO A310-BESTPREF                 
170900           MOVE WS-IDAVTAL-AVTALNR       TO A310-BESTLNR                  
171000           MOVE WS-IDAVTAL-SUFFIX        TO A310-BESTSUFF                 
171100           MOVE A310-A310B65             TO ZZAC-LOGGPOST                 
171200           MOVE W092-AREA                TO ZZAC-SORTPOST                 
171300           PERFORM IMS-ISRT-ZZAC                                          
171400        END-IF                                                            
171500     END-IF                                                               
171600     .                                                                    
171700     EJECT                                                                
171800 DB-SKAPA-HANDELSETR-2221 SECTION.                                        
171900     SKIP2                                                                
172000     MOVE W-IDARTNR                   TO XXBN-2222-IDARTNR                
172100     MOVE WS-IDLEVNR-SPAR             TO XXBN-2222-IDLEVNR                
172200                                                                          
172300     MOVE 'W201'                      TO XXBN-2222-IDSYSTEM               
172400                                                                          
172500     PERFORM IMS-ISRT-XXBN11                                              
172600     .                                                                    
172700     EJECT                                                                
172800  DC-SKAPA-HANDELSETR-2213 SECTION.                                       
172900     SKIP2                                                                
173000     MOVE W-IDARTNR                   TO XXBI-2214-IDARTNR                
173100                                                                          
173200     PERFORM IMS-ISRT-XXBI11                                              
173300     .                                                                    
173400     EJECT                                                                
173500 DF-SAETT-NYA-TILEVDAGAR SECTION.                                         
173600                                                                          
173700*PAH                                                                      
173800     PERFORM IMS-GHNP-ARTC11                                              
173900                                                                          
174000     MOVE +1 TO INDX                                                      
174100     PERFORM UNTIL INDX > MAX-TILEVDAGAR                                  
174200       IF MID-DAG-POS(INDX) NOT = ALL '+'                                 
174300         IF MID-DAG-POS(INDX) = MID-SPAR-DAG(INDX)                        
174400           CONTINUE                                                       
174500         ELSE                                                             
174600                                                                          
174700******* ÄNDRAD AVSÄNDNINGSDAG (TILEVDAG) NOLLSTÄLLS ***                   
174800                                                                          
174900           EVALUATE MID-SPAR-DAG(INDX)                                    
175000                WHEN 'MÅ'                                                 
175100                   MOVE ZERO TO CLAG-TILEVDAG(1)                          
175200                WHEN 'TI'                                                 
175300                   MOVE ZERO TO CLAG-TILEVDAG(2)                          
175400                WHEN 'ON'                                                 
175500                   MOVE ZERO TO CLAG-TILEVDAG(3)                          
175600                WHEN 'TO'                                                 
175700                   MOVE ZERO TO CLAG-TILEVDAG(4)                          
175800                WHEN 'FR'                                                 
175900                   MOVE ZERO TO CLAG-TILEVDAG(5)                          
176000                WHEN 'MO'                                                 
176100                   MOVE ZERO TO CLAG-TILEVDAG(1)                          
176200                WHEN 'TU'                                                 
176300                   MOVE ZERO TO CLAG-TILEVDAG(2)                          
176400                WHEN 'WE'                                                 
176500                   MOVE ZERO TO CLAG-TILEVDAG(3)                          
176600                WHEN 'TH'                                                 
176700                   MOVE ZERO TO CLAG-TILEVDAG(4)                          
176800                WHEN OTHER                                                
176900                   CONTINUE                                               
177000           END-EVALUATE                                                   
177100                                                                          
177200******* NY AVSÄNDNINGSDAG (TILEVDAG) SÄTTS ************                   
177300                                                                          
177400           EVALUATE MID-DAG-POS(INDX)                                     
177500                WHEN 'MÅ'                                                 
177600                   MOVE 1 TO CLAG-TILEVDAG(1)                             
177700                WHEN 'TI'                                                 
177800                   MOVE 2 TO CLAG-TILEVDAG(2)                             
177900                WHEN 'ON'                                                 
178000                   MOVE 3 TO CLAG-TILEVDAG(3)                             
178100                WHEN 'TO'                                                 
178200                   MOVE 4 TO CLAG-TILEVDAG(4)                             
178300                WHEN 'FR'                                                 
178400                   MOVE 5 TO CLAG-TILEVDAG(5)                             
178500                WHEN 'MO'                                                 
178600                   MOVE 1 TO CLAG-TILEVDAG(1)                             
178700                WHEN 'TU'                                                 
178800                   MOVE 2 TO CLAG-TILEVDAG(2)                             
178900                WHEN 'WE'                                                 
179000                   MOVE 3 TO CLAG-TILEVDAG(3)                             
179100                WHEN 'TH'                                                 
179200                   MOVE 4 TO CLAG-TILEVDAG(4)                             
179300           END-EVALUATE                                                   
179400         END-IF                                                           
179500       END-IF                                                             
179600       ADD +1 TO INDX                                                     
179700     END-PERFORM                                                          
179800                                                                          
179900     PERFORM IMS-REPLACE-ARTC-11                                          
180000     .                                                                    
180100     EJECT                                                                
180200 DG-REDIGERA-TILL-SKAERM SECTION.                                         
180300                                                                          
180400********* HÄR SPARAS+VISAS DEN/DE NYA AVSÄNDNINGSDAGARNA                  
180500********* I RÄTT ORDNING 'PÅ SKÄRMEN'                                     
180600                                                                          
180700     MOVE ZERO TO INDX                                                    
180800                                                                          
180900     IF CLAG-TILEVDAG(1) = 1                                              
181000       ADD +1 TO INDX                                                     
181100       MOVE 'MÅ' TO MOD-SPAR-DAG(INDX)                                    
181200                    MOD-DAG-POS(INDX)                                     
181300       MOVE MFS-ADD-LYS-UPP-FAELT TO                                      
181400                    MOD-DAG-POS-ATTR(INDX)                                
181500     END-IF                                                               
181600                                                                          
181700     IF CLAG-TILEVDAG(2) = 2                                              
181800       ADD +1 TO INDX                                                     
181900       MOVE 'TI' TO MOD-SPAR-DAG(INDX)                                    
182000                    MOD-DAG-POS(INDX)                                     
182100       MOVE MFS-ADD-LYS-UPP-FAELT TO                                      
182200                    MOD-DAG-POS-ATTR(INDX)                                
182300     END-IF                                                               
182400                                                                          
182500     IF CLAG-TILEVDAG(3) = 3                                              
182600       ADD +1 TO INDX                                                     
182700       MOVE 'ON' TO MOD-SPAR-DAG(INDX)                                    
182800                    MOD-DAG-POS(INDX)                                     
182900       MOVE MFS-ADD-LYS-UPP-FAELT TO                                      
183000                    MOD-DAG-POS-ATTR(INDX)                                
183100     END-IF                                                               
183200                                                                          
183300     IF CLAG-TILEVDAG(4) = 4                                              
183400       ADD +1 TO INDX                                                     
183500       MOVE 'TO' TO MOD-SPAR-DAG(INDX)                                    
183600                    MOD-DAG-POS(INDX)                                     
183700       MOVE MFS-ADD-LYS-UPP-FAELT TO                                      
183800                    MOD-DAG-POS-ATTR(INDX)                                
183900     END-IF                                                               
184000                                                                          
184100     IF CLAG-TILEVDAG(5) = 5                                              
184200       ADD +1 TO INDX                                                     
184300       MOVE 'FR' TO MOD-SPAR-DAG(INDX)                                    
184400                    MOD-DAG-POS(INDX)                                     
184500       MOVE MFS-ADD-LYS-UPP-FAELT TO                                      
184600                    MOD-DAG-POS-ATTR(INDX)                                
184700     END-IF                                                               
184800                                                                          
184900     IF INDX < MAX-TILEVDAGAR                                             
185000       ADD +1 TO INDX                                                     
185100       PERFORM UNTIL INDX > MAX-TILEVDAGAR                                
185200         MOVE MFS-RENSA-FAELT TO                                          
185300                                 MOD-SPAR-DAG(INDX)                       
185400                                 MOD-DAG-POS(INDX)                        
185500         ADD +1 TO INDX                                                   
185600       END-PERFORM                                                        
185700     END-IF                                                               
185800     .                                                                    
185900     EJECT                                                                
186000                                                                          
186100 DH-EV-SKAPA-LARM SECTION.                                                
186200                                                                          
186300     MOVE NEJ  TO SW-LARM-09                                              
186400     IF ART-FLERS = JA                                                    
186500                                                                          
186600*       --- KOLLA OM LARM-09 SKALL SKAPAS                                 
186700        MOVE WS-IDARTNR         TO W-IDARTNR-MIN7                         
186800                                   W-IDARTNR-MAX7                         
186900                                                                          
187000*       --- LÄSER ERSB MED IDARTNR-TILLK                                  
187100        PERFORM IMS-GU-ERSB01                                             
187200        PERFORM UNTIL SEGMENT-SAKNAS                                      
187300*          --- KOLLA IFALL ERSATT ARTIKEL SKALL LARMAS                    
187400           MOVE ERSB01-ERS-IDARTNR  TO W-IDARTNR                          
187500           PERFORM IMS-GU-WDK601                                          
187600           IF SEGMENT-FINNS AND K601-ART-KDERS-UTG = ZERO                 
187700              PERFORM IMS-GNP-WDK611                                      
187800              IF K611-CLAG-KDERS = +01 OR +02 OR +03                      
187900                                OR +04 OR +05 OR +06                      
188000                                OR +07 OR +08                             
188100                 PERFORM DHA-SKAPA-LARM-ORSAK-09                          
188200                 MOVE JA TO SW-LARM-09                                    
188300              END-IF                                                      
188400           END-IF                                                         
188500           PERFORM IMS-GN-ERSB01                                          
188600        END-PERFORM                                                       
188700*       --- ÅTERSTÄLLER NYCKELN TILL IDARTNR-TILLK                        
188800        MOVE WS-IDARTNR          TO W-IDARTNR                             
188900                                                                          
189000        IF SW-LARM-09 = JA                                                
189100*          --- ERSATTA ARTIKLAR ÄR LARMADE.                               
189200*          --- LARMA DÄRFÖR NU ÄVEN DENNA ERSÄTTANDE ARTIKELN             
189300           PERFORM DHA-SKAPA-LARM-ORSAK-09                                
189400        END-IF                                                            
189500     END-IF                                                               
189600     .                                                                    
189700                                                                          
189800                                                                          
189900 DHA-SKAPA-LARM-ORSAK-09 SECTION.                                         
190000     SKIP2                                                                
190100     MOVE WC-CDC-SE  TO W-IDDC-2203                                       
190200     MOVE W-IDARTNR  TO XXBJ11-2204-IDARTNR                               
190300     MOVE +09        TO XXBJ11-2204-KDLPORS                               
190400     PERFORM IMS-ISRT-XXBJ-2204                                           
190500     .                                                                    
190600                                                                          
190700                                                                          
190800 DO-KOLLA-UPPDAT-WDK7 SECTION.                                            
190900                                                                          
191000     MOVE NEJ  TO SW-LARM-09                                              
191100     PERFORM IMS-GU-WDK701                                                
191200     IF SEGMENT-FINNS                                                     
191300        PERFORM IMS-GHNP-WDK712                                           
191400        PERFORM UNTIL SEGMENT-SAKNAS                                      
191500           IF SPAR-DAFINLV-AAAAMMDD > LART-DAPUBL                         
191600              MOVE ZERO TO LART-DAPUBL                                    
191700              PERFORM IMS-REPL-WDK7                                       
191800           END-IF                                                         
191900           PERFORM IMS-GHNP-WDK712                                        
192000        END-PERFORM                                                       
192100     END-IF                                                               
192200     .                                                                    
192300                                                                          
192400 E-VISA-BILD SECTION.                                                     
192500     SKIP2                                                                
192600     PERFORM IMS-GNP-ARTC11                                               
192700     MOVE CLAG-FLJIT                 TO MOD-FLJIT                         
192800     MOVE CLAG-FLMANKP               TO MOD-FLMANKP                       
192900     MOVE CLAG-IDINK                 TO MOD-IDINK                         
193000     MOVE CLAG-FLNYBER               TO MOD-FLNYBER                       
193100     IF CLAG-KVVECKOR-LVAR NUMERIC                                        
193200        MOVE CLAG-KVVECKOR-LVAR      TO WS-KVVECKOR-LVAR-2                
193300     ELSE                                                                 
193400        MOVE ZERO                    TO WS-KVVECKOR-LVAR-2                
193500     END-IF                                                               
193600     MOVE WS-KVVECKOR-LVAR-2(1:2)    TO WS-KVVECKOR-LVAR-3(1:2)           
193700     MOVE '.'                        TO WS-KVVECKOR-LVAR-3(3:1)           
193800     MOVE WS-KVVECKOR-LVAR-2(3:1)    TO WS-KVVECKOR-LVAR-3(4:1)           
193900     MOVE WS-KVVECKOR-LVAR-3         TO MOD-KVVECKOR-LVAR-UT              
194000     MOVE CLAG-KDAVT                 TO MOD-KDAVT                         
194100     MOVE CLAG-KVKP                  TO MOD-KVKP                          
194200     MOVE CLAG-KVSLUTKP              TO MOD-KVSLUTKP                      
194300     MOVE CLAG-KDKSP                 TO MOD-KDKSP-UT                      
194400     MOVE CLAG-FLRELSP               TO MOD-FLRELSP                       
194500     IF CLAG-TISLUTKP > 0                                                 
194600       MOVE CLAG-TISLUTKP            TO DAT-I-TIDATUM                     
194700       MOVE 'AAMMDD'                 TO DAT-KDDATFORM                     
194800       CALL WDATKONV USING DAT-KDDATFORM                                  
194900                           DAT-I-TIDATUM                                  
195000                           DAT-O-TIDATUM                                  
195100                           DAT-KDSVAR                                     
195200       MOVE DAT-TIAAVVD              TO MOD-TISLUTKP                      
195300     ELSE                                                                 
195400       MOVE MFS-RENSA-FAELT          TO MOD-TISLUTKP                      
195500     END-IF                                                               
195600                                                                          
195700     MOVE CLAG-ADINPORT              TO MOD-ADINPORT                      
195800     MOVE ART-TIFINLV                TO MOD-TIFINLV                       
195900     MOVE ART-TIURPROD               TO MOD-TIURPROD                      
196000     MOVE ART-KDSOP                  TO MOD-KDSOP                         
196100     MOVE ART-KVEOP                  TO MOD-KVEOP                         
196200     MOVE ART-FLBRAND                TO MOD-FLBRAND                       
196300     MOVE ART-FLBSNES                TO MOD-FLBSNES                       
196400                                                                          
196500     MOVE MFS-OEPPNA-NUM-FAELT       TO MOD-REDIRLEV-C1-IN-ATTR           
196600     MOVE CLAG-REDIRLEV              TO MOD-REDIRLEV-C1                   
196700                                                                          
196800     MOVE 1                          TO MOD-C2FAELT-SW                    
196900                                                                          
197000     MOVE 1                          TO W-KDNOTTYP                        
197100                                                                          
197200     IF CLAG-IDDC-REF NOT = SPACE                                         
197300        MOVE INF-REFILL-PART         TO MED-IDMFSFEL                      
197400        CALL WMEDKONV             USING MED-WMEDAREA                      
197500        MOVE MED-MFSFEL              TO MOD-TEMFSFEL                      
197600        PERFORM S010-CLOSE-ARTC12-FAELT                                   
197700     END-IF                                                               
197800                                                                          
197900     PERFORM IMS-GET-ARTC25                                               
198000     IF SEGMENT-FINNS                                                     
198100        MOVE NOT-TEARTNOT            TO MOD-TEARTNOT1-IN                  
198200     ELSE                                                                 
198300        MOVE MFS-RENSA-FAELT         TO MOD-TEARTNOT1-IN                  
198400     END-IF                                                               
198500                                                                          
198600     MOVE 2                          TO W-KDNOTTYP                        
198700     PERFORM IMS-GET-ARTC25                                               
198800     IF SEGMENT-FINNS                                                     
198900        MOVE NOT-TEARTNOT            TO MOD-TEARTNOT2-IN                  
199000     ELSE                                                                 
199100        MOVE MFS-RENSA-FAELT         TO MOD-TEARTNOT2-IN                  
199200     END-IF                                                               
199300                                                                          
199400     MOVE CLAG-FLFSP                 TO MOD-FLFSP                         
199500     MOVE CLAG-TISTODAT-LARM         TO MOD-TISTODAT-LARM                 
199600     IF MOD-TISTODAT-LARM = ZERO                                          
199700        INSPECT MOD-TISTODAT-LARM REPLACING LEADING ZERO BY SPACE         
199800     END-IF                                                               
199900     MOVE CLAG-FLRELSP               TO MOD-FLRELSP                       
200000                                                                          
200100     MOVE +1 TO INDX                                                      
200200                                                                          
200300     IF CLAG-TILEVDAG(1) = 1                                              
200400       MOVE 'MÅ' TO MOD-DAG-POS(INDX)                                     
200500                    MOD-SPAR-DAG(INDX)                                    
200600       ADD +1    TO INDX                                                  
200700     END-IF                                                               
200800                                                                          
200900     IF CLAG-TILEVDAG(2) = 2                                              
201000       MOVE 'TI' TO MOD-DAG-POS(INDX)                                     
201100                    MOD-SPAR-DAG(INDX)                                    
201200       ADD +1    TO INDX                                                  
201300     END-IF                                                               
201400                                                                          
201500     IF CLAG-TILEVDAG(3) = 3                                              
201600       MOVE 'ON' TO MOD-DAG-POS(INDX)                                     
201700                    MOD-SPAR-DAG(INDX)                                    
201800       ADD +1    TO INDX                                                  
201900     END-IF                                                               
202000                                                                          
202100     IF CLAG-TILEVDAG(4) = 4                                              
202200       MOVE 'TO' TO MOD-DAG-POS(INDX)                                     
202300                    MOD-SPAR-DAG(INDX)                                    
202400       ADD +1    TO INDX                                                  
202500     END-IF                                                               
202600                                                                          
202700     IF CLAG-TILEVDAG(5) = 5                                              
202800       MOVE 'FR' TO MOD-DAG-POS(INDX)                                     
202900                    MOD-SPAR-DAG(INDX)                                    
203000     END-IF                                                               
203100                                                                          
203200                                                                          
203300     MOVE MFS-ADD-SAETT-CURSOR       TO MOD-REDIRLEV-C1-IN-ATTR           
203400                                                                          
203500*    FORTSÄTTNING.                                                        
203600     EJECT                                                                
203700     PERFORM IMS-GET-BENA01-BSEQ                                          
203800     MOVE 'S  '                      TO W-IDSKYLT                         
203900     PERFORM IMS-GET-BENA11-BSEQ                                          
204000     MOVE BENA-TEXT-BEART            TO MOD-BEART                         
204100                                                                          
204200                                                                          
204300                                                                          
204400     EJECT                                                                
204500                                                                          
204600     IF MID-IDARTNR-IN = ALL '+'  AND  EGEN-BILD                          
204700        MOVE INF-PRESS-PF11        TO MED-IDMFSINF                        
204800        CALL WMEDKONV USING MED-WMEDAREA                                  
204900        MOVE MED-MFSINF            TO MOD-TEMFSINF                        
205000        IF CLAG-IDDC-REF = SPACE                                          
205100           MOVE MFS-ADD-LAES-IN-FAELT TO                                  
205200                                      MOD-KVSLUTKP-IN-ATTR                
205300                                      MOD-KDKSP-IN-ATTR                   
205400                                      MOD-TISLUTKP-IN-ATTR                
205500                                      MOD-KVKP-IN-ATTR                    
205600                                      MOD-FLMANKP-IN-ATTR                 
205700        END-IF                                                            
205800                                                                          
205900        MOVE MFS-ADD-LAES-IN-FAELT TO                                     
206000                                      MOD-FLFSP-IN-ATTR                   
206100                                      MOD-KDAVT-IN-ATTR                   
206200                                      MOD-IDINK-IN-ATTR                   
206300                                      MOD-FLNYBER-IN-ATTR                 
206400                                      MOD-KVVECKOR-LVAR-IN-ATTR           
206500                                      MOD-FLJIT-IN-ATTR                   
206600                                      MOD-TISTODAT-LARM-IN-ATTR           
206700                                      MOD-TIFINLV-IN-ATTR                 
206800                                      MOD-TIURPROD-IN-ATTR                
206900                                      MOD-KDSOP-IN-ATTR                   
207000                                      MOD-KVEOP-IN-ATTR                   
207100                                      MOD-FLBRAND-IN-ATTR                 
207200                                      MOD-FLBSNES-IN-ATTR                 
207300                                      MOD-FLRELSP-IN-ATTR                 
207400                                      MOD-REDIRLEV-C1-IN-ATTR             
207500                                                                          
207600        MOVE MFS-ROER-EJ-FAELT     TO                                     
207700                                      MOD-FLFSP-IN                        
207800                                      MOD-KVSLUTKP-IN                     
207900                                      MOD-KDKSP-IN                        
208000                                      MOD-KDAVT-IN                        
208100                                      MOD-IDINK-IN                        
208200                                      MOD-TISLUTKP-IN                     
208300                                      MOD-FLNYBER-IN                      
208400                                      MOD-KVVECKOR-LVAR-IN                
208500                                      MOD-FLJIT-IN                        
208600                                      MOD-TISTODAT-LARM-IN                
208700                                      MOD-KVKP-IN                         
208800                                      MOD-FLMANKP-IN                      
208900                                      MOD-TIFINLV-IN                      
209000                                      MOD-TIURPROD-IN                     
209100                                      MOD-KDSOP-IN                        
209200                                      MOD-KVEOP-IN                        
209300                                      MOD-FLBRAND-IN                      
209400                                      MOD-FLBSNES-IN                      
209500                                      MOD-FLRELSP-IN                      
209600                                      MOD-REDIRLEV-C1-IN                  
209700                                                                          
209800        IF MID-DAG-POS(1) NOT = ALL '+'                                   
209900            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-DAG-POS-ATTR(1)             
210000        END-IF                                                            
210100        IF MID-DAG-POS(2) NOT = ALL '+'                                   
210200            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-DAG-POS-ATTR(2)             
210300        END-IF                                                            
210400        IF MID-DAG-POS(3) NOT = ALL '+'                                   
210500            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-DAG-POS-ATTR(3)             
210600        END-IF                                                            
210700        IF MID-DAG-POS(4) NOT = ALL '+'                                   
210800            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-DAG-POS-ATTR(4)             
210900        END-IF                                                            
211000        IF MID-DAG-POS(5) NOT = ALL '+'                                   
211100            MOVE MFS-ADD-LAES-IN-FAELT TO MOD-DAG-POS-ATTR(5)             
211200        END-IF                                                            
211300                                                                          
211400        MOVE +1 TO INDX                                                   
211500        PERFORM UNTIL INDX > MAX-TILEVDAGAR                               
211600          MOVE MFS-ROER-EJ-FAELT TO MOD-DAG-POS(INDX)                     
211700                                    MOD-SPAR-DAG(INDX)                    
211800          ADD +1 TO INDX                                                  
211900        END-PERFORM                                                       
212000                                                                          
212100     ELSE                                                                 
212200        PERFORM S03-RENSA-MOD-INMATNINGSFAELT                             
212300     END-IF                                                               
212400     .                                                                    
212500     EJECT                                                                
212600 S01-TESTA-USERID SECTION.                                                
212700*** ENDAST VISSA USERID:N FÅR UPPDATERA RELEASE-SPÄRREN                   
212800                                                                          
212900     SET ANVAENDAR-IX TO 1                                                
213000     MOVE NEJ TO SW-GODKAENT-ID                                           
213100     PERFORM UNTIL ANVAENDAR-IX > 3                                       
213200         IF MSG-SIGNON-USERID = RAD-USERID  (ANVAENDAR-IX)                
213300             MOVE JA TO SW-GODKAENT-ID                                    
213400         END-IF                                                           
213500         SET ANVAENDAR-IX UP BY 1                                         
213600     END-PERFORM                                                          
213700     .                                                                    
213800 EJECT                                                                    
213900 S02-ROER-EJ-FAELT SECTION.                                               
214000     SKIP3                                                                
214100     MOVE MFS-ROER-EJ-FAELT          TO MOD-BEART                         
214200                                        MOD-FLFSP                         
214300                                        MOD-KVSLUTKP                      
214400                                        MOD-KDKSP-UT                      
214500                                        MOD-IDINK                         
214600                                        MOD-KDAVT                         
214700                                        MOD-TISLUTKP                      
214800                                        MOD-FLNYBER                       
214900                                        MOD-KVVECKOR-LVAR-UT              
215000                                        MOD-FLJIT                         
215100                                        MOD-TISTODAT-LARM                 
215200                                        MOD-KVKP                          
215300                                        MOD-FLMANKP                       
215400                                        MOD-ADINPORT                      
215500                                        MOD-TIFINLV                       
215600                                        MOD-TIURPROD                      
215700                                        MOD-KDSOP                         
215800                                        MOD-KVEOP                         
215900                                        MOD-FLBRAND                       
216000                                        MOD-FLBSNES                       
216100                                        MOD-FLRELSP                       
216200                                        MOD-REDIRLEV-C1                   
216300                                        MOD-FLRELSP-IN                    
216400                                        MOD-TEARTNOT1-IN                  
216500                                        MOD-TEARTNOT2-IN                  
216600                                                                          
216700     MOVE +1 TO INDX                                                      
216800     PERFORM UNTIL INDX > MAX-TILEVDAGAR                                  
216900       MOVE MFS-ROER-EJ-FAELT TO MOD-DAG-POS(INDX)                        
217000                                 MOD-SPAR-DAG(INDX)                       
217100       ADD +1 TO INDX                                                     
217200     END-PERFORM                                                          
217300     .                                                                    
217400     EJECT                                                                
217500 S03-RENSA-MOD-INMATNINGSFAELT SECTION.                                   
217600     SKIP3                                                                
217700     MOVE MFS-RENSA-FAELT             TO                                  
217800                                         MOD-FLFSP-IN                     
217900                                         MOD-KVSLUTKP-IN                  
218000                                         MOD-KDKSP-IN                     
218100                                         MOD-KDAVT-IN                     
218200                                         MOD-IDINK-IN                     
218300                                         MOD-TISLUTKP-IN                  
218400                                         MOD-FLNYBER-IN                   
218500                                         MOD-KVVECKOR-LVAR-IN             
218600                                         MOD-FLJIT-IN                     
218700                                         MOD-TISTODAT-LARM-IN             
218800                                         MOD-KVKP-IN                      
218900                                         MOD-FLMANKP-IN                   
219000                                         MOD-TIFINLV-IN                   
219100                                         MOD-TIURPROD-IN                  
219200                                         MOD-KDSOP-IN                     
219300                                         MOD-KVEOP-IN                     
219400                                         MOD-FLBRAND-IN                   
219500                                         MOD-FLBSNES-IN                   
219600                                         MOD-FLRELSP-IN                   
219700                                         MOD-REDIRLEV-C1-IN               
219800     .                                                                    
219900     EJECT                                                                
220000                                                                          
220100 S010-CLOSE-ARTC12-FAELT SECTION.                                         
220200                                                                          
220300     MOVE MFS-CLOSE-FIELD             TO MOD-KVSLUTKP-IN-ATTR             
220400                                         MOD-KDKSP-IN-ATTR                
220500                                         MOD-TISLUTKP-IN-ATTR             
220600                                         MOD-KVKP-IN-ATTR                 
220700                                         MOD-FLMANKP-IN-ATTR              
220800     .                                                                    
220900     EJECT                                                                
221000 S28-SKAPA-B65-TRANS SECTION.                                             
221100                                                                          
221200     IF CLAG-IDINK (1:3) NUMERIC                                          
221300        MOVE CLAG-IDINK (1:3) TO TEST-IDINK                               
221400     ELSE                                                                 
221500        MOVE ZERO             TO TEST-IDINK                               
221600***     IF CLAG-IDINK (2:3) NUMERIC                                       
221700***        MOVE CLAG-IDINK (2:3) TO TEST-IDINK                            
221800***     ELSE                                                              
221900***        MOVE ZERO             TO TEST-IDINK                            
222000***     END-IF                                                            
222100     END-IF                                                               
222200     MOVE WS-IDARTNR TO W-IDARTNR                                         
222300     PERFORM IMS-GET-ARTC01                                               
222400     PERFORM IMS-GET-ARTC23                                               
222500     PERFORM UNTIL SEGMENT-SAKNAS                                         
222600       MOVE AVT-IDAVTAL TO W-IDAVTAL-RED                                  
222700       MOVE W-PREFIX TO W-PREFIX-NUM                                      
222800***        TAG ÄVEN MED NAP-AVTAL, PREFIX = 004                           
222900       IF (W-PREFIX-NUM > 99 AND W-PREFIX-NUM < 987) OR                   
223000          (W-PREFIX-NUM > 987 AND W-PREFIX-NUM < 1000) OR                 
223100          (W-PREFIX-NUM = 004)                                            
223200           IF AVT-IDLEVNR-AVT (5:1) = SPACE                               
223300             MOVE AVT-IDLEVNR-AVT TO IDLEVNR-ALFA                         
223400             MOVE ZERO TO TALLY                                           
223500             INSPECT IDLEVNR-ALFA TALLYING TALLY FOR                      
223600             CHARACTERS BEFORE INITIAL SPACE                              
223700             IF TALLY = ZERO                                              
223800                MOVE ZERO TO WS-IDLEVNR-NUM                               
223900             ELSE                                                         
224000                MOVE IDLEVNR-ALFA(1:TALLY) TO WS-IDLEVNR-NUM              
224100             END-IF                                                       
224200             MOVE WS-IDLEVNR-NUM        TO A310-LEVNUM                    
224300           ELSE                                                           
224400              MOVE AVT-IDLEVNR-AVT       TO A310-LEVNUM                   
224500           END-IF                                                         
224600           MOVE AVT-IDAVTAL           TO W-IDAVTAL-RED                    
224700           MOVE W-PREFIX              TO A310-BESTPREF                    
224800           MOVE W-AVTALSNR            TO A310-BESTLNR                     
224900           MOVE W-SUFFIX              TO A310-BESTSUFF                    
225000           MOVE SPACE                 TO A310-LEVNUM-GODSM                
225100                                            A310-ANT-BESTANN              
225200           MOVE 'RY2'                 TO A310-KT                          
225300           MOVE DAGENS-DATUM          TO A310-DATUM-UTSKR                 
225400           MOVE IDARTNR-WS            TO W-IDARTNR-8                      
225500           MOVE W-IDARTNR-8           TO A310-ARTNR                       
225600                                         W092-SORTBGP                     
225700                                                                          
225800           ACCEPT ZZAC-TIKLOCK        FROM TIME                           
225900           ACCEPT ZZAC-TIAAMMDD       FROM DATE                           
226000           ADD +1 TO W-IDLOGLOP                                           
226100           MOVE W-IDLOGLOP           TO ZZAC-IDLOGLOP                     
226200           MOVE A310-A310B65         TO ZZAC-LOGGPOST                     
226300           MOVE W092-AREA            TO ZZAC-SORTPOST                     
226400           PERFORM IMS-ISRT-ZZAC                                          
226500                                                                          
226600        END-IF                                                            
226700        PERFORM IMS-GET-ARTC23                                            
226800     END-PERFORM                                                          
226900     .                                                                    
227000     EJECT                                                                
227100 S98-OM-TVA-AAR SECTION.                                                  
227200     SKIP2                                                                
227300     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
227400     PERFORM S99-WDATKONV                                                 
227500     MOVE DAT-TIAAVVD TO AAVVD                                            
227600                                                                          
227700     MOVE FUNCTION CURRENT-DATE (1:4) TO W-AAR-HELP                       
227800     ADD 2 TO W-AAR-HELP                                                  
227900     MOVE W-AAR-HELP (3:2) TO AA                                          
228000                                                                          
228100     IF VV = 53                                                           
228200       MOVE 52 TO VV                                                      
228300     END-IF                                                               
228400     MOVE 1 TO D                                                          
228500     .                                                                    
228600     EJECT                                                                
228700 S99-WDATKONV SECTION.                                                    
228800     SKIP2                                                                
228900     CALL WDATKONV USING DAT-KDDATFORM                                    
229000                         DAT-I-TIDATUM                                    
229100                         DAT-O-TIDATUM                                    
229200                         DAT-KDSVAR.                                      
229300     .                                                                    
229400     EJECT                                                                
229500                                                                          
229600  MFS-ROER-EJ-FAELT-IN SECTION.                                           
229700                                                                          
229800     MOVE MFS-ROER-EJ-FAELT           TO                                  
229900                                         MOD-FLFSP-IN                     
230000                                         MOD-KVSLUTKP-IN                  
230100                                         MOD-KDKSP-IN                     
230200                                         MOD-KDAVT-IN                     
230300                                         MOD-IDINK-IN                     
230400                                         MOD-TISLUTKP-IN                  
230500                                         MOD-FLNYBER-IN                   
230600                                         MOD-KVVECKOR-LVAR-IN             
230700                                         MOD-FLJIT-IN                     
230800                                         MOD-TISTODAT-LARM-IN             
230900                                         MOD-KVKP-IN                      
231000                                         MOD-FLMANKP-IN                   
231100                                         MOD-TIFINLV-IN                   
231200                                         MOD-TIURPROD-IN                  
231300                                         MOD-KDSOP-IN                     
231400                                         MOD-KVEOP-IN                     
231500                                         MOD-FLBRAND-IN                   
231600                                         MOD-FLBSNES-IN                   
231700                                         MOD-FLRELSP-IN                   
231800                                         MOD-REDIRLEV-C1-IN               
231900     .                                                                    
232000     EJECT                                                                
232100                                                                          
232200* IMS SEKTIONER                                                           
232300     SKIP3                                                                
232400 IMS-GET-MSG SECTION.                                                     
232500     SKIP2                                                                
232600     MOVE '  QC' TO GODK-STATUSKODER                                      
232700     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
232800     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
232900     PERFORM IMS-STATUS-KONTROLL                                          
233000     .                                                                    
233100     SKIP3                                                                
233200 IMS-INSERT-MSG SECTION.                                                  
233300      SKIP2                                                               
233400     IF MSGI-IDLAND-SPR = 'GB'                                            
233500        MOVE 'N' TO MFS-KDHUVOMR                                          
233600     END-IF                                                               
233700     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
233800     MOVE SPACE TO GODK-STATUSKODER                                       
233900     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
234000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
234100     PERFORM IMS-STATUS-KONTROLL                                          
234200     .                                                                    
234300     EJECT                                                                
234400 IMS-GET-ARTC01 SECTION.                                                  
234500     SKIP2                                                                
234600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
234700             DELIMITED BY SIZE INTO SSA1                                  
234800     MOVE '  GE' TO GODK-STATUSKODER                                      
234900     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA-01 SSA1                   
235000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
235100     PERFORM IMS-STATUS-KONTROLL                                          
235200     .                                                                    
235300     SKIP3                                                                
235400*FIX*                                                                     
235500 IMS-GHU-ARTC01 SECTION.                                                  
235600     SKIP2                                                                
235700     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
235800             DELIMITED BY SIZE INTO SSA1                                  
235900     MOVE '  GE' TO GODK-STATUSKODER                                      
236000     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA-01 SSA1                  
236100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
236200     PERFORM IMS-STATUS-KONTROLL                                          
236300     .                                                                    
236400     SKIP3                                                                
236500*FIX*                                                                     
236600 IMS-GHNP-ARTC11 SECTION.                                                 
236700     SKIP2                                                                
236800     MOVE 'WLARTC11*F'  TO SSA1                                           
236900     MOVE '  GE' TO GODK-STATUSKODER                                      
237000     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA-11 SSA1                 
237100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
237200     PERFORM IMS-STATUS-KONTROLL                                          
237300     .                                                                    
237400     SKIP3                                                                
237500 IMS-GNP-ARTC11 SECTION.                                                  
237600     SKIP2                                                                
237700     MOVE 'WLARTC11*F '  TO SSA1                                          
237800     MOVE '  GE' TO GODK-STATUSKODER                                      
237900     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-11 SSA1                  
238000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
238100     PERFORM IMS-STATUS-KONTROLL                                          
238200     .                                                                    
238300     SKIP3                                                                
238400 IMS-GNP-ARTC21 SECTION.                                                  
238500     SKIP2                                                                
238600     STRING 'WLARTC21(DAPRLIST=>' W-DAPRLIST-X ')'                        
238700             DELIMITED BY SIZE INTO SSA1                                  
238800     MOVE '  GE' TO GODK-STATUSKODER                                      
238900     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA-21 SSA1                  
239000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
239100     PERFORM IMS-STATUS-KONTROLL                                          
239200     .                                                                    
239300     EJECT                                                                
239400 IMS-INSERT-ARTC25 SECTION.                                               
239500     SKIP2                                                                
239600     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
239700             DELIMITED BY SIZE INTO SSA1                                  
239800     MOVE  'WLARTC11(KDSEGKEY =1)'  TO SSA2                               
239900     MOVE  'WLARTC25 ' TO SSA3                                            
240000     MOVE '  ' TO GODK-STATUSKODER                                        
240100     CALL CBLTDLI USING ISRT ARTC-PCB                                     
240200                               DLI-IO-AREA1                               
240300                               SSA1                                       
240400                               SSA2                                       
240500                               SSA3                                       
240600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
240700     PERFORM IMS-STATUS-KONTROLL                                          
240800     .                                                                    
240900     SKIP3                                                                
241000 IMS-GET-ARTC23 SECTION.                                                  
241100     SKIP2                                                                
241200     MOVE  'WLARTC11(KDSEGKEY =1)'  TO SSA1                               
241300     MOVE 'WLARTC23 '  TO SSA2                                            
241400     MOVE '  GE' TO GODK-STATUSKODER                                      
241500     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA1 SSA1 SSA2               
241600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
241700     PERFORM IMS-STATUS-KONTROLL                                          
241800     .                                                                    
241900     SKIP3                                                                
242000 IMS-GHNP-ARTC25 SECTION.                                                 
242100     SKIP2                                                                
242200     MOVE  'WLARTC11*F(KDSEGKEY =1)'  TO SSA1                             
242300     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
242400            DELIMITED BY SIZE INTO SSA2                                   
242500     MOVE '  GE' TO GODK-STATUSKODER                                      
242600     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA1 SSA1 SSA2              
242700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
242800     PERFORM IMS-STATUS-KONTROLL                                          
242900     .                                                                    
243000     SKIP3                                                                
243100 IMS-GET-ARTC25 SECTION.                                                  
243200     SKIP2                                                                
243300     MOVE  'WLARTC11*F(KDSEGKEY =1)'  TO SSA1                             
243400     STRING 'WLARTC25(KDNOTTYP =' W-KDNOTTYP-X ')'                        
243500            DELIMITED BY SIZE INTO SSA2                                   
243600     MOVE '  GE' TO GODK-STATUSKODER                                      
243700     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA1 SSA1 SSA2               
243800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
243900     PERFORM IMS-STATUS-KONTROLL                                          
244000     .                                                                    
244100     EJECT                                                                
244200 IMS-GET-BENA01-BSEQ SECTION.                                             
244300     SKIP2                                                                
244400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
244500             DELIMITED BY SIZE INTO SSA1                                  
244600     MOVE '  ' TO GODK-STATUSKODER                                        
244700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA2 SSA1                     
244800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
244900     PERFORM IMS-STATUS-KONTROLL                                          
245000     .                                                                    
245100     SKIP3                                                                
245200 IMS-GET-BENA11-BSEQ SECTION.                                             
245300     SKIP2                                                                
245400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
245500             DELIMITED BY SIZE INTO SSA1                                  
245600     MOVE '  ' TO GODK-STATUSKODER                                        
245700     CALL CBLTDLI USING GNP BENA-PCB DLI-IO-AREA2 SSA1                    
245800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
245900     PERFORM IMS-STATUS-KONTROLL                                          
246000     .                                                                    
246100     EJECT                                                                
246200 IMS-GET-LEVA01 SECTION.                                                  
246300     SKIP2                                                                
246400     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
246500             DELIMITED BY SIZE INTO SSA1                                  
246600     MOVE '  ' TO GODK-STATUSKODER                                        
246700     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-AREA3 SSA1                     
246800     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
246900     PERFORM IMS-STATUS-KONTROLL                                          
247000     .                                                                    
247100     EJECT                                                                
247200 IMS-ISRT-XXBI11 SECTION.                                                 
247300     SKIP2                                                                
247400     STRING 'WLXXBI01(WDG3KEY  =' W-WDG3KEY-2213-X ')'                    
247500             DELIMITED BY SIZE INTO SSA1                                  
247600     MOVE 'WLXXBI11 '            TO SSA2                                  
247700     MOVE '  ' TO GODK-STATUSKODER                                        
247800     CALL CBLTDLI USING ISRT XXBI-PCB DLI-IO-AREA4 SSA1 SSA2              
247900     MOVE XXBI-STATUS-CODE TO STATUS-WS                                   
248000     PERFORM IMS-STATUS-KONTROLL                                          
248100     .                                                                    
248200     EJECT                                                                
248300 IMS-ISRT-XXBN11 SECTION.                                                 
248400     SKIP2                                                                
248500     STRING 'WLXXBN01(WDG3KEY  =' W-WDG3KEY-2221-X ')'                    
248600             DELIMITED BY SIZE INTO SSA1                                  
248700     MOVE 'WLXXBN11 '            TO SSA2                                  
248800     MOVE '  ' TO GODK-STATUSKODER                                        
248900     CALL CBLTDLI USING ISRT XXBN-PCB DLI-IO-AREA4 SSA1 SSA2              
249000     MOVE XXBN-STATUS-CODE TO STATUS-WS                                   
249100     PERFORM IMS-STATUS-KONTROLL                                          
249200     .                                                                    
249300     EJECT                                                                
249400 IMS-ISRT-ZZAC SECTION.                                                   
249500     SKIP2                                                                
249600     MOVE 'WLZZAC01 '        TO SSA1                                      
249700     MOVE '  '               TO GODK-STATUSKODER                          
249800     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA5 SSA1                   
249900     MOVE ZZAC-STATUS-CODE     TO STATUS-WS                               
250000     PERFORM IMS-STATUS-KONTROLL                                          
250100     .                                                                    
250200     EJECT                                                                
250300 IMS-REPLACE-ARTC SECTION.                                                
250400     SKIP2                                                                
250500     MOVE '  '   TO GODK-STATUSKODER                                      
250600     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA1                        
250700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
250800     PERFORM IMS-STATUS-KONTROLL                                          
250900     .                                                                    
251000     SKIP3                                                                
251100 IMS-REPLACE-ARTC-01 SECTION.                                             
251200     SKIP2                                                                
251300     MOVE '  '   TO GODK-STATUSKODER                                      
251400     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-01                      
251500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
251600     PERFORM IMS-STATUS-KONTROLL                                          
251700     .                                                                    
251800     SKIP3                                                                
251900 IMS-REPLACE-ARTC-11 SECTION.                                             
252000     SKIP2                                                                
252100     MOVE '  '   TO GODK-STATUSKODER                                      
252200     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA-11                      
252300     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
252400     PERFORM IMS-STATUS-KONTROLL                                          
252500     .                                                                    
252600     SKIP3                                                                
252700 IMS-DELETE SECTION.                                                      
252800     SKIP2                                                                
252900     MOVE '  '   TO GODK-STATUSKODER                                      
253000     CALL CBLTDLI USING DLET ARTC-PCB DLI-IO-AREA1                        
253100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
253200     PERFORM IMS-STATUS-KONTROLL                                          
253300     .                                                                    
253400     EJECT                                                                
253500 IMS-GU-WDK701   SECTION.                                                 
253600     SKIP2                                                                
253700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
253800             DELIMITED BY SIZE INTO SSA1                                  
253900     MOVE '  GE' TO GODK-STATUSKODER                                      
254000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
254100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
254200     PERFORM IMS-STATUS-KONTROLL.                                         
254300     SKIP2                                                                
254400 IMS-GHNP-WDK712   SECTION.                                               
254500     SKIP2                                                                
254600     MOVE 'WDK712   '      TO SSA1                                        
254700     MOVE '  GE'           TO GODK-STATUSKODER                            
254800     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK712 SSA1                  
254900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
255000     PERFORM IMS-STATUS-KONTROLL.                                         
255100     SKIP2                                                                
255200 IMS-REPL-WDK7   SECTION.                                                 
255300     SKIP2                                                                
255400     MOVE '  '   TO GODK-STATUSKODER                                      
255500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
255600     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
255700     PERFORM IMS-STATUS-KONTROLL.                                         
255800                                                                          
255900                                                                          
256000 IMS-GU-WDK601 SECTION.                                                   
256100                                                                          
256200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
256300       DELIMITED BY SIZE INTO SSA1                                        
256400     MOVE '  GE'           TO GODK-STATUSKODER                            
256500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-K601 SSA1                 
256600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
256700     PERFORM IMS-STATUS-KONTROLL.                                         
256800                                                                          
256900                                                                          
257000 IMS-GNP-WDK611 SECTION.                                                  
257100     SKIP2                                                                
257200     MOVE 'WDK611   '      TO SSA1                                        
257300     MOVE '  '             TO GODK-STATUSKODER                            
257400     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-K611 SSA1                
257500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
257600     PERFORM IMS-STATUS-KONTROLL.                                         
257700                                                                          
257800                                                                          
257900 IMS-GU-ERSB01 SECTION.                                                   
258000                                                                          
258100     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
258200                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
258300       DELIMITED BY SIZE INTO SSA1                                        
258400     MOVE '  GE'           TO GODK-STATUSKODER                            
258500     CALL CBLTDLI USING GU ERSB-PCB DLI-IO-AREA-ERSB SSA1                 
258600     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
258700     PERFORM IMS-STATUS-KONTROLL.                                         
258800                                                                          
258900                                                                          
259000 IMS-GN-ERSB01 SECTION.                                                   
259100     SKIP2                                                                
259200     STRING 'WLERSB01(WDD7A1KY=>' W-WDD7A1KY-MIN                          
259300                    '&WDD7A1KY=<' W-WDD7A1KY-MAX ')'                      
259400       DELIMITED BY SIZE INTO SSA1                                        
259500     MOVE '  GE'           TO GODK-STATUSKODER                            
259600     CALL CBLTDLI USING GN ERSB-PCB DLI-IO-AREA1 SSA1                     
259700     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
259800     PERFORM IMS-STATUS-KONTROLL.                                         
259900                                                                          
260000                                                                          
260100 IMS-ISRT-XXBJ-2204 SECTION.                                              
260200     SKIP2                                                                
260300     STRING 'WLXXBJ01(WDG3KEY  =' W-WDGXKEY-2203-X ')'                    
260400       DELIMITED BY SIZE INTO SSA1                                        
260500     MOVE 'WLXXBJ11 '      TO SSA2                                        
260600     MOVE '  '             TO GODK-STATUSKODER                            
260700     CALL CBLTDLI USING ISRT XXBJ-PCB DLI-IO-AREA-2204 SSA1 SSA2          
260800     MOVE XXBJ-STATUS-CODE TO STATUS-WS                                   
260900     PERFORM IMS-STATUS-KONTROLL.                                         
261000                                                                          
261100 IMS-STATUS-KONTROLL SECTION.                                             
261200     SET STATUS-IX TO 1                                                   
261300     SEARCH GODK-STATUS                                                   
261400       AT END CALL FELLOG                                                 
261500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
261600     END-SEARCH                                                           
261700     .                                                                    
261800     EJECT                                                                
261900*    -COPY WY2000P1                                                       
262000*    -COPY WY2000P2                                                       
262100*    -COPY WY2000P3                                                       
