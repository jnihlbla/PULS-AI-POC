000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W9040800.                                                
000400 AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   JUNI 1987.                                               
000600     REMARKS.                                                             
000700*    FUNKTION.                                                            
000800*                ERSÄTTNINGSREGISTRERING.                                 
000900*                BYTE ERSÄTTNINGSKOD.                                     
001000*                UPPDATERING TILLKOMMANDE ARTIKLAR.                       
001100*                                                                         
001200*     ÄNDRING: I SAMBAND MED EVEREST-PROJEKTET (IDLEVNR) ÄNDRAS           
001300*              OCKSÅ SEG-NAMN OCH CTX-NAMN FÖR WLXXBT                     
001400*              GAMLA XXBT-TRANSEN  BYTER TILL 2304 MED HTYP-2303          
001500*                                                                         
001600*    ÄNDRING:                                                             
001700*        2005-FEB  ETRACKER=1476814.  VISA KAMPANJ-INFO                   
001800*                                     TILLAGT DB2-LÄSNING  /C.E.          
001900*                                   (VISAS PÅ INF-RAD + ÖVR.MEDD.)        
002000*                                                                         
002100*    SKIP2                                                                
002200*    INDATA.                                                              
002300*    . . . . TRANSAKTION: W90408T                                         
002400*                         W90408U                                         
002500*    . . . . . . . . MID: W90408I1                                        
002600*    UTDATA.                                                              
002700*    . . . . . . . . MOD: W90408O1                                        
002800*    DYNAMISKA SUBPROGRAM.                                                
002900*                         FELLOG                                          
003000*                         CBLTDLI                                         
003100*                         WDECEDIT                                        
003200*                         WDATKONV                                        
003300     SKIP3                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP3                                                                
003600 DATA DIVISION.                                                           
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900*    -COPY WY2000W2                                                       
004000     SKIP3                                                                
004100*    -COPY WY2000W1                                                       
004200     SKIP3                                                                
004300*    -COPY WY2000W3                                                       
004400     SKIP3                                                                
004500 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W9040800'.            
004600 77  JA                          PIC X(1)    VALUE 'J'.                   
004700 77  YES                         PIC X(1)    VALUE 'Y'.                   
004800 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004900 77  TIO                         PIC S9(3)   COMP-3 VALUE +10.            
005000 77  SPRAK-IX                    PIC S9(9)   VALUE +0  COMP SYNC.         
005100 77  INPUT-RETT                  PIC X(1)    VALUE 'J'.                   
005200 77  LAS-VIDARE                  PIC X(1)    VALUE 'J'.                   
005300 77  TEXT-FINNS                  PIC X(1)    VALUE 'N'.                   
005400 77  FLFORTS                     PIC X(1)    VALUE 'N'.                   
005500 77  FINNS-PA-ACTION-FILE        PIC X(1)    VALUE 'N'.                   
005600 77  UPPDATERING-TILLATEN        PIC X(1)    VALUE 'J'.                   
005700 77  RADER-UPPDATERADE           PIC X(1)    VALUE 'J'.                   
005800 77  UPPDAT-TIERSDAT-TEARTNOT    PIC X(1)    VALUE 'N'.                   
005900 77  WS-EXTERN-SATS              PIC X(1)    VALUE 'N'.                   
006000 77  IDAO-FINNS                  PIC X(1)    VALUE 'N'.                   
006100 77  SW-IDAO-FIXAD               PIC X(1)    VALUE 'N'.                   
006200 77  SW-TOMMA-IDAO-FINNS         PIC X(1)    VALUE 'N'.                   
006300 77  PPMS-TRANS                  PIC X(1)    VALUE 'N'.                   
006400 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006500 77  WS-KDPRODSL                 PIC 9(2)    VALUE ZERO.                  
006600 77  WS-KDBPSR                   PIC 9(1)    VALUE ZERO.                  
006700 77  WS-TIFINLV-MAX              PIC 9(5)    VALUE ZERO.                  
006800 77  WS-DIERS-TILLK              PIC 9(4)V9(3)  VALUE ZERO.               
006900 77  WS-IDKORTNR                 PIC 9(3)    VALUE ZERO.                  
007000 77  WS-IDKORTNR-SPAR1           PIC 9(3)    VALUE ZERO.                  
007100 77  WS-IDKORTNR-SPAR2           PIC 9(3)    VALUE ZERO.                  
007200 77  WS-IDKORTNR-SPAR3           PIC 9(3)    VALUE ZERO.                  
007300 77  WS-TEARTNOT                 PIC X(40)   VALUE SPACE.                 
007400 77  WS-IDANSK                   PIC S9(3)   COMP-3 VALUE ZERO.           
007500 77  WS-IDANSK-ALARM             PIC S9(3)   COMP-3 VALUE ZERO.           
007600 77  WS-IDDC-ALARM               PIC X(2)    VALUE SPACES.                
007700 77  WS-FLGEMART                 PIC X(1)    VALUE 'N'.                   
007800 77  SPAR-IDPROJ                 PIC X(4)    VALUE SPACE.                 
007900 77  MAX-RAD                     PIC S9(3)   VALUE +09  COMP-3.           
008000 77  MAX-TAB                     PIC S9(3)   VALUE +990 COMP-3.           
008100 77  RAD-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
008200 77  TAB-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
008300 77  AO-IX                       PIC S9(9)   VALUE +0   COMP SYNC.        
008400 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
008500 77  STR-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
008600 77  STR-IX-MAX                  PIC S9(9)   VALUE +99  COMP SYNC.        
008700 77  MAX-IX-VAERDE               PIC S9(9)   VALUE +999999998             
008800                                             COMP SYNC.                   
008900 77  MIN-IX-VAERDE               PIC S9(9)   VALUE +1 COMP SYNC.          
009000 77  IX-MINUS-1                  PIC S9(9)   VALUE +0 COMP SYNC.          
009100 77  IX-PLUS-1                   PIC S9(9)   VALUE +0 COMP SYNC.          
009200 77  MAX-MOD-LAENGD              PIC S9(4) VALUE +902  COMP SYNC.         
009300 77  FL-NYPONART                 PIC  X(1)   VALUE 'N'.                   
009400 77  IDPROJK-IX                  PIC  9(2)   VALUE ZERO.                  
009500 77  IDPROJK-IX-MAX              PIC  9(2)   VALUE 75.                    
009600 77  IDPROJK-IX-MAX-PLUS-1       PIC  9(2)   VALUE 76.                    
009700 77  IDLEVNR-ALFA                PIC X(5)    VALUE SPACE.                 
009800 77  WS-IDLEVNR-NUM              PIC 9(5)    VALUE ZERO.                  
009900 77  SPAR-KDANSKQ                PIC X       VALUE SPACE.                 
010000                                                                          
010100*01  -COPY WWPRODSL                                                       
010200     EJECT                                                                
010300                                                                          
010400 01  ARBETSAREOR.                                                         
010500     03 FILLER                   PIC X(16)   VALUE                        
010600                                             'WS-DB2-SEKTION'.            
010700     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
010800                                                                          
010900     03  WS-DAGENS-AAAAMMDD      PIC 9(8).                                
011000     03  WS-JMFR-AAAAMMDD        PIC 9(8).                                
011100     03  FILLER REDEFINES WS-JMFR-AAAAMMDD.                               
011200        05 FILLER                PIC 9(2).                                
011300        05 WS-JMFR-AA            PIC 9(2).                                
011400        05 FILLER                PIC 9(4).                                
011500     03  WS-FLAGGA-Q-KAMP        PIC X(1)    VALUE SPACE.                 
011600     03  WS-FLAGGA-W-S-KAMP      PIC X(1)    VALUE SPACE.                 
011700     03  WS-KVLS-REM             PIC S9(7)   VALUE ZERO COMP-3.           
011800     03  WS-ANTAL-KAMP           PIC 9(7)    VALUE ZERO.                  
011900                                                                          
012000     03  WS-TEMFSINF             PIC X(42)   VALUE SPACE.                 
012100     03  WS-TEMFSINF-KAMP        PIC X(10)   VALUE SPACE.                 
012200     03  WS-TEMFSINF-SPLIT       PIC X(3)    VALUE '-- '.                 
012300                                                                          
012400*      --- VALID IDDC CODES                                               
012500*                                                                         
012600*01    -COPY WWDC99                                                       
012700*01    -COPY WWDCKONS                                                     
012800       EJECT                                                              
012900 01  WS-IDSEKVNR                 PIC S9(3) VALUE ZERO COMP-3.             
013000                                                                          
013100 01  W-IDLOGLOP                  PIC S9(1) VALUE ZERO.                    
013200 01  WS-IDARTNR                             PIC X(9) VALUE ZERO.          
013300 01  IDARTNR-WS REDEFINES WS-IDARTNR        PIC 9(9).                     
013400     SKIP2                                                                
013500 01  WS-IDARTNR-TILLK                       PIC X(9) VALUE ZERO.          
013600 01  IDARTNR-TILLK-WS REDEFINES                                           
013700          WS-IDARTNR-TILLK                  PIC 9(9).                     
013800 01  WS-PPMS-IDARTNR-TILLK                  PIC X(9) VALUE ZERO.          
013900 01  PPMS-IDARTNR-TILLK-WS                  PIC 9(9) VALUE ZERO.          
014000 01  W-IDARTNR-8                            PIC 9(8) VALUE ZERO.          
014100                                                                          
014200     SKIP2                                                                
014300 01  WS-KDERS                    PIC 9(2)   VALUE ZERO.                   
014400 01  FILLER REDEFINES WS-KDERS.                                           
014500     03  WS-KDERS-1              PIC 9.                                   
014600     03  WS-KDERS-2              PIC 9.                                   
014700     SKIP2                                                                
014800 01  NY-KDERS-C1                 PIC 9(2)    VALUE ZERO.                  
014900     SKIP2                                                                
015000 01  WS-KVBR-TOT                 PIC 9(7)   VALUE ZERO.                   
015100 01  WS-KVBR                     PIC 9(7)   VALUE ZERO.                   
015200     SKIP2                                                                
015300 01  WS-IDANSK-KOLL              PIC 9(3).                                
015400 01  FILLER REDEFINES WS-IDANSK-KOLL.                                     
015500     03  WS-IDANSK-POS1-2        PIC 9(2).                                
015600     03  FILLER                  PIC 9.                                   
015700     SKIP2                                                                
015800 01  W-IDAVTAL-RED               PIC 9(13).                               
015900 01  W-IDAVTAL REDEFINES W-IDAVTAL-RED.                                   
016000     03  FILLER                  PIC X.                                   
016100     03  W-PREFIX                PIC X(3).                                
016200     03  W-AVTALSNR              PIC X(6).                                
016300     03  W-SUFFIX                PIC X(3).                                
016400 01  W-PREFIX-NUM                PIC 9(3).                                
016500     SKIP2                                                                
016600 01  RAKNARE.                                                             
016700     03  B-RAKN                  PIC S9     COMP-3 VALUE ZERO.            
016800     03  R-RAKN                  PIC S9     COMP-3 VALUE ZERO.            
016900     03  WS-RAKNARE              PIC S9(5)  COMP-3 VALUE ZERO.            
017000     03  NOLL-RAKNARE            PIC S9(5)  COMP-3 VALUE ZERO.            
017100     SKIP2                                                                
017200 01  SWITCHAR.                                                            
017300     03  SW-TIKO                 PIC X             VALUE 'N'.             
017400     03  SW-LISTA                PIC X             VALUE 'N'.             
017500     SKIP2                                                                
017600 01  SPARADE-IDAO.                                                        
017700     03  SPARAD-IDAO OCCURS 5.                                            
017800        05  IDAO-POS-1           PIC X.                                   
017900        05  IDAO-RESTEN          PIC X(9).                                
018000     SKIP2                                                                
018100 01  NYA-IDAO.                                                            
018200     03  NY-IDAO                 PIC X(10) OCCURS 5.                      
018300     SKIP2                                                                
018400 01  WS-TIFINLV-KOLL             PIC 9(5).                                
018500 01  FILLER REDEFINES WS-TIFINLV-KOLL.                                    
018600     03  WS-TIFINLV-AAVV         PIC 9(4).                                
018700     03  FILLER                  PIC 9.                                   
018800     SKIP2                                                                
018900 01  WS-TIFINLV                  PIC 9(5).                                
019000 01  FILLER REDEFINES WS-TIFINLV.                                         
019100     03  WS-AAVV                 PIC 9(4).                                
019200     03  FILLER                  PIC 9.                                   
019300     SKIP2                                                                
019400 01  WS-TIFINLV-SW               PIC X.                                   
019500     88   TIFINLV-OK             VALUE 'J'.                               
019600     SKIP2                                                                
019700 01  WS-TIERSDAT-PREL            PIC 9(5) VALUE ZERO.                     
019800 01  FILLER REDEFINES WS-TIERSDAT-PREL.                                   
019900     03  FILLER                  PIC 9(2).                                
020000     03  WS-VECKA-PREL           PIC 9(2).                                
020100     03  WS-DAG-PREL             PIC 9.                                   
020200     SKIP2                                                                
020300 01  XX-TIERSDAT-PREL            PIC X(5) VALUE SPACE.                    
020400 01  FILLER REDEFINES XX-TIERSDAT-PREL.                                   
020500     03  FILLER                  PIC X(2).                                
020600     03  XX-VECKA                PIC X(2).                                
020700     03  XX-DAG                  PIC X.                                   
020800     SKIP2                                                                
020900 01  WS-TISTODAT                 PIC S9(7) COMP-3 VALUE ZERO.             
021000     EJECT                                                                
021100 01  FILLER                      PIC X(11) VALUE 'ERSATT-AREA'.           
021200 01  ERSATT-AREA.                                                         
021300     03   ERSATT-KDERS-UTG       PIC S9(3)   COMP-3 VALUE ZERO.           
021400     03   ERSATT-FLERS           PIC X       VALUE SPACE.                 
021500     03   ERSATT-FLIART          PIC X       VALUE SPACE.                 
021600     03   ERSATT-KDKSP           PIC S9      COMP-3 VALUE ZERO.           
021700     03   ERSATT-IDLEVNR         PIC X(5)    VALUE SPACE.                 
021800     03   ERSATT-FLLSRDEL        PIC X       VALUE SPACE.                 
021900     03   ERSATT-KDPRODSL        PIC 9(2)    VALUE ZERO.                  
022000     03   ERSATT-KDAVT           PIC 9(1)    VALUE ZERO.                  
022100     03   ERSATT-IDINK           PIC 9(3)    VALUE ZERO.                  
022200     03   ERSATT-KDBPSR          PIC 9(1)    VALUE ZERO.                  
022300     03   ERSATT-PRARTSTD        PIC S9(7)V9(2)  VALUE ZERO.              
022400                                                                          
022500     03   ERSATT-KDERS-C1        PIC 9(2)    VALUE ZERO.                  
022600     03   FILLER REDEFINES ERSATT-KDERS-C1.                               
022700          05 ERSATT-KDERS-C1-1   PIC 9.                                   
022800          05 ERSATT-KDERS-C1-2   PIC 9.                                   
022900     SKIP3                                                                
023000 01  TABELL.                                                              
023100     03 TAB-RAD OCCURS 990.                                               
023200        05   TAB-IDRADNR         PIC 9(3).                                
023300        05   TAB-IDARTNR-TILLK   PIC 9(9).                                
023400        05   TAB-DIERS-TILLK     PIC 9(4)V9(3).                           
023500        05   TAB-BEERS           PIC X(20).                               
023600     SKIP3                                                                
023700 01  WS-STR-TABELL.                                                       
023800     03 WS-STR-TILLKART          PIC S9(9) COMP-3 VALUE ZERO              
023900                                         OCCURS 99.                       
024000     SKIP3                                                                
024100 01  ALARM-SSCODE-SW             PIC X   VALUE 'N'.                       
024200     88 ALARM-SSCODE-JA                  VALUE 'J'.                       
024300     SKIP3                                                                
024400 01  DYNAMISKA-SUBPROGRAM.                                                
024500     03  WDECEDIT                PIC X(8)       VALUE 'WDECEDIT'.         
024600     03  WDATKONV                PIC X(8)       VALUE 'WDATKONV'.         
024700     03  CBLTDLI                 PIC X(8)       VALUE 'CBLTDLI '.         
024800     03  FELLOG                  PIC X(8)       VALUE 'FELLOG  '.         
024900     03  W005INIT                PIC X(8)       VALUE 'W005INIT'.         
025000     EJECT                                                                
025100*01  -COPY W10111                                                         
025200     EJECT                                                                
025300*01  AREA   -COPY W092W001     -PRE W092-.                                
025400     EJECT                                                                
025500*01         -COPY W111PPMS     -PRE PPMS-.                                
025600     EJECT                                                                
025700*01         -COPY A310TB65     -PRE A310-                                 
025800     EJECT                                                                
025900*01  -COPY WDECAREA                                                       
026000     EJECT                                                                
026100*01  -COPY WDATAREA                                                       
026200     EJECT                                                                
026300*                    ****   PARAMETRAR TILL W005INIT                      
026400*01  -COPY WMSGINIT                                                       
026500     EJECT                                                                
026600 01  NYCKLAR-TILL-DLI.                                                    
026700     03  W-IDARTNR-X.                                                     
026800         05  W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.            
026900     03  W-IDDC-X.                                                        
027000         05  W-IDDC               PIC X(2)  VALUE SPACE.                  
027100     03  W-KDSEGKEY-X.                                                    
027200         05  W-KDSEGKEY           PIC X(1)  VALUE '1'.                    
027300     03  W-WDD901KY-X.                                                    
027400         05  W-IDARTNR-D9         PIC S9(9) COMP-3 VALUE ZERO.            
027500         05  W-IDDC-D9            PIC X(2)  VALUE  SPACE.                 
027600     03  W-1116-IDARTNR-X.                                                
027700         05  W-1116-IDARTNR        PIC S9(9) COMP-3 VALUE ZERO.           
027800     03  W-1158-IDARTNR-X.                                                
027900         05  W-1158-IDARTNR        PIC S9(9) COMP-3 VALUE ZERO.           
028000     03  W-IDARTNR-TILLK-X.                                               
028100         05  W-IDARTNR-TILLK      PIC S9(9) COMP-3 VALUE ZERO.            
028200     03  W-IDSKYLT-X.                                                     
028300         05  W-IDSKYLT            PIC X(3)  VALUE 'S  '.                  
028400     03  W-IDKORTNR-X.                                                    
028500         05  W-IDKORTNR           PIC S9(3) COMP-3 VALUE ZERO.            
028600     03  W-1111-KEY-X.                                                    
028700         05  FILLER               PIC X(4)  VALUE '1111'.                 
028800         05  W-1111-IDARTNR       PIC S9(9) COMP-3 VALUE ZERO.            
028900         05  FILLER               PIC X(21) VALUE LOW-VALUE.              
029000     03  W-1112-KEY-X.                                                    
029100         05  W-1112-IDARTNR       PIC S9(9) COMP-3 VALUE ZERO.            
029200         05  FILLER               PIC X(5)  VALUE LOW-VALUE.              
029300     03  W-1114-KEY-X.                                                    
029400         05  W-1114-IDKORTNR      PIC S9(3) COMP-3 VALUE ZERO.            
029500         05  FILLER               PIC X(8)  VALUE LOW-VALUE.              
029600     03  W-2203-KEY-X.                                                    
029700         05  FILLER               PIC X(4)  VALUE '2203'.                 
029800         05  W-2203-IDDC          PIC X(2).                               
029900         05  FILLER               PIC X(24) VALUE LOW-VALUE.              
030000     03  W-2303-KEY-X.                                                    
030100         05  FILLER               PIC X(4)  VALUE '2303'.                 
030200         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
030300                                                                          
030400*    03  W-2301-KEY-X.  GAMLA NYCKELN FÖR XXBT  NYA ÄR 2303               
030500*        05  FILLER               PIC X(4)  VALUE '2301'.                 
030600*        05  FILLER               PIC X(26) VALUE LOW-VALUE.              
030700                                                                          
030800     03  W-9101-KEY-X.                                                    
030900         05  FILLER               PIC X(4)  VALUE '9101'.                 
031000         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
031100     03  W-1139-KEY-X.                                                    
031200         05  FILLER               PIC X(4)  VALUE '1139'.                 
031300         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
031400     03  W-1140-KEY-X.                                                    
031500         05  FILLER               PIC X(1)  VALUE '1'.                    
031600     03  W-1141-KEY-X.                                                    
031700         05  FILLER               PIC X(4)  VALUE '1141'.                 
031800         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
031900     03  W-1142-KEY-X.                                                    
032000         05  FILLER               PIC X     VALUE SPACE.                  
032100     03  W-1115-KEY-X.                                                    
032200         05  FILLER               PIC X(4)  VALUE '1115'.                 
032300         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
032400     03  W-1157-KEY-X.                                                    
032500         05  FILLER               PIC X(4)  VALUE '1157'.                 
032600         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
032700     03  W-IDARTNR-ERS-LOW-X.                                             
032800         05  W-IDARTNR-ERS-LOW   PIC S9(9) COMP-3  VALUE ZERO.            
032900     03  W-IDARTNR-ERS-HIGH-X.                                            
033000       05  W-IDARTNR-ERS-HIGH  PIC S9(9) COMP-3  VALUE +999999999.        
033100     03  W-IDKORTNR-LOW-X.                                                
033200         05  W-IDKORTNR-LOW      PIC S9(3) COMP-3  VALUE ZERO.            
033300     03  W-IDKORTNR-HIGH-X.                                               
033400         05  W-IDKORTNR-HIGH     PIC S9(3) COMP-3  VALUE +999.            
033500     03  W-IDAO-X.                                                        
033600         05  W-IDAO              PIC X(10) VALUE SPACE.                   
033700     03  W-WDJ1CSEQ-X.                                                    
033800         05  W-IDLEVNR-S         PIC X(5)  VALUE SPACE.                   
033900         05  W-BELEVART-S        PIC X(30) VALUE SPACE.                   
034000         05  W-IDARTNR-S         PIC S9(9) COMP-3 VALUE ZERO.             
034100     03  W-WDJ111KY-X.                                                    
034200         05  W-KDSTRRAD          PIC X VALUE SPACE.                       
034300         05  W-IDRADNR           PIC S9(5) COMP-3 VALUE ZERO.             
034400     03  W-WDGX2223-X.                                                    
034500         05  W-IDHTYP-2223       PIC X(4)  VALUE '2223'.                  
034600         05  W-IDANSK-2223       PIC S9(3) VALUE ZERO COMP-3.             
034700         05  W-VALFRI-2225       PIC X(24) VALUE LOW-VALUE.               
034800                                                                          
034900     03  W-WDGX2224-X.                                                    
035000         05  W-TISENBEK-DAG-2224 PIC S9(7) VALUE ZERO COMP-3.             
035100         05  W-TISENBEK-KL-2224  PIC S9(7) VALUE ZERO COMP-3.             
035200         05  W-KDLARM-2224       PIC S9(3) VALUE ZERO COMP-3.             
035300                                                                          
035400     03  W-WDGX2231-X.                                                    
035500         05  W-IDHTYP-2231       PIC X(4)  VALUE '2231'.                  
035600         05  W-VALFRI-2231       PIC X(26) VALUE LOW-VALUE.               
035700                                                                          
035800     03  W-WDGX2232-X.                                                    
035900         05  W-IDANSK-2232       PIC S9(3) VALUE ZERO COMP-3.             
036000         05  W-LOW-VALUE-2232    PIC X(3)  VALUE LOW-VALUE.               
036100 01  W-2227KEY-X.                                                         
036200     03 W-IDHTYP        PIC X(4)    VALUE '2227'.                         
036300     03 FILLER          PIC X(26)   VALUE LOW-VALUE.                      
036400*                                                                         
036500     EJECT                                                                
036600 01  MEDDELANDE.                                                          
036700                                                                          
036800     03  W-FEL-1.                                                         
036900         05  FILLER              PIC X(40)  VALUE                         
037000            'ARTIKELNUMMER EJ NUMERISKT              '.                   
037100         05  FILLER              PIC X(40)  VALUE                         
037200            'PARTNUMBER NOT NUMERIC                  '.                   
037300     03  FILLER REDEFINES W-FEL-1.                                        
037400         05  FEL-1               PIC X(40)  OCCURS 2.                     
037500                                                                          
037600     03  W-FEL-2.                                                         
037700         05  FILLER              PIC X(40)  VALUE                         
037800            'ARTIKELNUMRET SAKNAS PÅ ARTIKLEREGISTRET'.                   
037900         05  FILLER              PIC X(40)  VALUE                         
038000            'PARTNUMBER IS MISSING IN PARTS FILE     '.                   
038100     03  FILLER REDEFINES W-FEL-2.                                        
038200         05  FEL-2               PIC X(40)  OCCURS 2.                     
038300                                                                          
038400     03  W-FEL-3.                                                         
038500         05  FILLER              PIC X(40)  VALUE                         
038600            'KORRIGERA UPPLYSTA FÄLT                 '.                   
038700         05  FILLER              PIC X(40)  VALUE                         
038800            'CORRECT HIGHLIGHTED FIELDS              '.                   
038900     03  FILLER REDEFINES W-FEL-3.                                        
039000         05  FEL-3               PIC X(40)  OCCURS 2.                     
039100                                                                          
039200     03  W-FEL-4.                                                         
039300         05  FILLER              PIC X(40)  VALUE                         
039400            'TRYCK PF11 FÖR UPPDATERING              '.                   
039500         05  FILLER              PIC X(40)  VALUE                         
039600            'PRESS PF11 FOR UPDATING                 '.                   
039700     03  FILLER REDEFINES W-FEL-4.                                        
039800         05  FEL-4               PIC X(40)  OCCURS 2.                     
039900                                                                          
040000     03  W-MED-1.                                                         
040100         05  FILLER              PIC X(40)  VALUE                         
040200            'ANTAL SAKNAS                            '.                   
040300         05  FILLER              PIC X(40)  VALUE                         
040400            'QUANTITY IS MISSING                     '.                   
040500     03  FILLER REDEFINES W-MED-1.                                        
040600         05  MED-1               PIC X(40)  OCCURS 2.                     
040700                                                                          
040800     03  W-MED-2.                                                         
040900         05  FILLER              PIC X(40)  VALUE                         
041000            'ARTIKEL ÄR TILLKOMMANDE I ANNAN ERS     '.                   
041100         05  FILLER              PIC X(40)  VALUE                         
041200            'PART NO IS ADDED IN ANOTHER SUPERSESSION'.                   
041300     03  FILLER REDEFINES W-MED-2.                                        
041400         05  MED-2               PIC X(40)  OCCURS 2.                     
041500                                                                          
041600     03  W-MED-3.                                                         
041700         05  FILLER              PIC X(40)  VALUE                         
041800              'EJ GODKÄND ERSÄTTNINGSKOD             '.                   
041900         05  FILLER              PIC X(40)  VALUE                         
042000              'NOT A VALID SUPERSESSION CODE         '.                   
042100     03  FILLER REDEFINES W-MED-3.                                        
042200         05  MED-3               PIC X(40)  OCCURS 2.                     
042300                                                                          
042400     03  W-MED-4.                                                         
042500         05  FILLER              PIC X(40)  VALUE                         
042600              'ERSÄTTNINGSDATUM SAKNAS               '.                   
042700         05  FILLER              PIC X(40)  VALUE                         
042800              'DATE OF SUPERSESSION IS MISSING       '.                   
042900     03  FILLER REDEFINES W-MED-4.                                        
043000         05  MED-4               PIC X(40)  OCCURS 2.                     
043100                                                                          
043200     03  W-MED-5.                                                         
043300         05  FILLER              PIC X(40)  VALUE                         
043400              'ÄNDRA ERSÄTTNINGSDATUM                '.                   
043500         05  FILLER              PIC X(40)  VALUE                         
043600              'CHANGE DATE OF SUPERSESSION           '.                   
043700     03  FILLER REDEFINES W-MED-5.                                        
043800         05  MED-5               PIC X(40)  OCCURS 2.                     
043900                                                                          
044000     03  W-MED-6.                                                         
044100         05  FILLER              PIC X(40)  VALUE                         
044200              'TILLKOMMANDE ARTIKEL SAKNAS PÅ ART.REG'.                   
044300         05  FILLER              PIC X(40)  VALUE                         
044400              'ADDING PART NO IS MISSING IN PARTS FILE'.                  
044500     03  FILLER REDEFINES W-MED-6.                                        
044600         05  MED-6               PIC X(40)  OCCURS 2.                     
044700                                                                          
044800     03  W-MED-7.                                                         
044900         05  FILLER              PIC X(40)  VALUE                         
045000              'TILLKOMMANDE ARTIKLAR FÅR EJ FINNAS   '.                   
045100         05  FILLER              PIC X(40)  VALUE                         
045200              'ADDING PART NO IS NOT ALLOWED         '.                   
045300     03  FILLER REDEFINES W-MED-7.                                        
045400         05  MED-7               PIC X(40)  OCCURS 2.                     
045500                                                                          
045600                                                                          
045700                                                                          
045800                                                                          
045900     03  W-MED-9.                                                         
046000         05  FILLER              PIC X(40)  VALUE                         
046100              'ARTIKEL INGÅR I SATS                  '.                   
046200         05  FILLER              PIC X(40)  VALUE                         
046300              'PART NO IS INCLUDED IN A KIT          '.                   
046400     03  FILLER REDEFINES W-MED-9.                                        
046500         05  MED-9               PIC X(40)  OCCURS 2.                     
046600                                                                          
046700     03  W-MED-10.                                                        
046800         05  FILLER              PIC X(40)  VALUE                         
046900              'EJ ENT ERS MÅSTE BÖRJA MED TEXT       '.                   
047000         05  FILLER              PIC X(40)  VALUE                         
047100              'ALT SUPERSESSION MUST START WITH TEXT '.                   
047200     03  FILLER REDEFINES W-MED-10.                                       
047300         05  MED-10              PIC X(40)  OCCURS 2.                     
047400                                                                          
047500     03  W-MED-11.                                                        
047600         05  FILLER              PIC X(40)  VALUE                         
047700              'ÄO-SAKNAS                             '.                   
047800         05  FILLER              PIC X(40)  VALUE                         
047900              'DCN-NO IS MISSING                     '.                   
048000     03  FILLER REDEFINES W-MED-11.                                       
048100         05  MED-11              PIC X(40)  OCCURS 2.                     
048200                                                                          
048300                                                                          
048400                                                                          
048500                                                                          
048600     03  W-MED-13.                                                        
048700         05  FILLER              PIC X(40)  VALUE                         
048800              'TILLKOMMANDE ARTIKELNUMMER ÄR ERSATT  '.                   
048900         05  FILLER              PIC X(40)  VALUE                         
049000              'ADDING PART NO IS SUPERSEDED          '.                   
049100     03  FILLER REDEFINES W-MED-13.                                       
049200         05  MED-13              PIC X(40)  OCCURS 2.                     
049300                                                                          
049400                                                                          
049500                                                                          
049600                                                                          
049700                                                                          
049800     03  W-MED-15.                                                        
049900         05  FILLER              PIC X(40)  VALUE                         
050000              'UPPDATERING UTFÖRD                    '.                   
050100         05  FILLER              PIC X(40)  VALUE                         
050200              'FIELDS ARE UPDATED                    '.                   
050300     03  FILLER REDEFINES W-MED-15.                                       
050400         05  MED-15              PIC X(40)  OCCURS 2.                     
050500                                                                          
050600     03  W-MED-16.                                                        
050700         05  FILLER              PIC X(40)  VALUE                         
050800              'FLER RADER FINNS                      '.                   
050900         05  FILLER              PIC X(40)  VALUE                         
051000              'MORE LINES                            '.                   
051100     03  FILLER REDEFINES W-MED-16.                                       
051200         05  MED-16              PIC X(40)  OCCURS 2.                     
051300                                                                          
051400     03  W-MED-17.                                                        
051500         05  FILLER              PIC X(40)  VALUE                         
051600              'ARTIKEL RENSAD                        '.                   
051700         05  FILLER              PIC X(40)  VALUE                         
051800              'PART NO IS DELETED                    '.                   
051900     03  FILLER REDEFINES W-MED-17.                                       
052000         05  MED-17              PIC X(40)  OCCURS 2.                     
052100                                                                          
052200     03  W-MED-18.                                                        
052300         05  FILLER              PIC X(40)  VALUE                         
052400              'UPPDATERAS AV ANNAN ANVÄNDARE'.                            
052500         05  FILLER              PIC X(40)  VALUE                         
052600              'IS BEING UPDATED BY ANOTHER USER    '.                     
052700     03  FILLER REDEFINES W-MED-18.                                       
052800         05  MED-18              PIC X(40)  OCCURS 2.                     
052900                                                                          
053000                                                                          
053100                                                                          
053200                                                                          
053300                                                                          
053400     03  W-MED-20.                                                        
053500         05  FILLER              PIC X(40)  VALUE                         
053600              'ERSÄTTNINGEN BACKAS FÖRE UPPDATERING'.                     
053700         05  FILLER              PIC X(40)  VALUE                         
053800              'THE SUP. MUST BE REVERSED BEFORE UPDATE'.                  
053900     03  FILLER REDEFINES W-MED-20.                                       
054000         05  MED-20              PIC X(40)  OCCURS 2.                     
054100                                                                          
054200     03  W-MED-21.                                                        
054300         05  FILLER              PIC X(40)  VALUE                         
054400              'ÄNDRA TIFINLV PÅ TILLK ARTIKEL       '.                    
054500         05  FILLER              PIC X(40)  VALUE                         
054600              'CHANGE PUBL. WEEK ON ADDING PART NO   '.                   
054700     03  FILLER REDEFINES W-MED-21.                                       
054800         05  MED-21              PIC X(40)  OCCURS 2.                     
054900                                                                          
055000     03  W-MED-22.                                                        
055100         05  FILLER              PIC X(40)  VALUE                         
055200              'UPPDATERING EJ GJORD                 '.                    
055300         05  FILLER              PIC X(40)  VALUE                         
055400              'UPDATE IS NOT DONE                    '.                   
055500     03  FILLER REDEFINES W-MED-22.                                       
055600         05  MED-22              PIC X(40)  OCCURS 2.                     
055700                                                                          
055800     03  W-MED-23.                                                        
055900         05  FILLER              PIC X(40)  VALUE                         
056000              'ERSÄTTNINGEN FÅR EJ UPPDATERAS       '.                    
056100         05  FILLER              PIC X(40)  VALUE                         
056200              'THE SUP. IS NOT ALLOWED TO BE UPDATED'.                    
056300     03  FILLER REDEFINES W-MED-23.                                       
056400         05  MED-23              PIC X(40)  OCCURS 2.                     
056500                                                                          
056600     03  W-MED-24.                                                        
056700         05  FILLER              PIC X(40)  VALUE                         
056800              'COMMON PART                          '.                    
056900         05  FILLER              PIC X(40)  VALUE                         
057000              'COMMON PART                           '.                   
057100     03  FILLER REDEFINES W-MED-24.                                       
057200         05  MED-24              PIC X(40)  OCCURS 2.                     
057300                                                                          
057400     03  W-MED-25.                                                        
057500         05  FILLER              PIC X(40)  VALUE                         
057600              'ARTIKEL INGÅR I 1002-SATS            '.                    
057700         05  FILLER              PIC X(40)  VALUE                         
057800              'PART NO IS INCLUDED IN A 1002-KIT     '.                   
057900     03  FILLER REDEFINES W-MED-25.                                       
058000         05  MED-25              PIC X(40)  OCCURS 2.                     
058100                                                                          
058200     03  W-MED-26.                                                        
058300         05  FILLER              PIC X(40)  VALUE                         
058400              'ARTIKEL INGÅR I EJ 1002-SATS         '.                    
058500         05  FILLER              PIC X(40)  VALUE                         
058600              'PART NO NOT INCLUDED IN A 1002-KIT    '.                   
058700     03  FILLER REDEFINES W-MED-26.                                       
058800         05  MED-26              PIC X(40)  OCCURS 2.                     
058900                                                                          
059000     03  W-MED-27.                                                        
059100         05  FILLER              PIC X(40)  VALUE                         
059200              'SATSEN ÄR DEF ERSATT                 '.                    
059300         05  FILLER              PIC X(40)  VALUE                         
059400              'THE KIT IS DEFINITIVELY SUPERSEDED    '.                   
059500     03  FILLER REDEFINES W-MED-27.                                       
059600         05  MED-27              PIC X(40)  OCCURS 2.                     
059700                                                                          
059800     03  W-MED-28.                                                        
059900         05  FILLER              PIC X(40)  VALUE                         
060000              'ALTERNATIV ERSÄTTN - UPPDATERA RASA  '.                    
060100         05  FILLER              PIC X(40)  VALUE                         
060200              'ALTERNATIVE SUPERSESSION - UPDATE RASA'.                   
060300     03  FILLER REDEFINES W-MED-28.                                       
060400         05  MED-28              PIC X(40)  OCCURS 2.                     
060500                                                                          
060600     03  W-MED-29.                                                        
060700         05  FILLER              PIC X(40)  VALUE                         
060800              'ARTIKEL INGÅR I STRUKTUR             '.                    
060900         05  FILLER              PIC X(40)  VALUE                         
061000              'PART NO IS INCLUDED IN A STRUCTURE '.                      
061100     03  FILLER REDEFINES W-MED-29.                                       
061200         05  MED-29              PIC X(40)  OCCURS 2.                     
061300     03  W-MED-30.                                                        
061400         05  FILLER              PIC X(40)  VALUE                         
061500              'GEMENSAM PV/LV                       '.                    
061600         05  FILLER              PIC X(40)  VALUE                         
061700              'COMMON CP/TP                       '.                      
061800     03  FILLER REDEFINES W-MED-30.                                       
061900         05  MED-30              PIC X(40)  OCCURS 2.                     
062000     03  W-MED-31.                                                        
062100         05  FILLER              PIC X(40)  VALUE                         
062200              'UPPDATERING EJ TILLÅTEN              '.                    
062300         05  FILLER              PIC X(40)  VALUE                         
062400              'UPDATE NOT ALLOWED                 '.                      
062500     03  FILLER REDEFINES W-MED-31.                                       
062600         05  MED-31              PIC X(40)  OCCURS 2.                     
062700     EJECT                                                                
062800                                                                          
062900                                                                          
063000****           MED-91 KAN KOMBINERAS MED MEDDELANDENA                     
063100****                  MED-2, MED-16, MED-18, MED-30 VID LÄS-TRANS         
063200     03    W-MED-91.                                                      
063300         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
063400         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
063500     03    FILLER REDEFINES W-MED-91.                                     
063600         05    MED-91        PIC X(10) OCCURS 2.                          
063700                                                                          
063800****           MED-92 KAN KOMBINERAS MED MEDDELANDENA                     
063900****                  MED-2, MED-16, MED-18, MED-30 VID LÄS-TRANS         
064000     03    W-MED-92.                                                      
064100         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
064200         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
064300     03    FILLER REDEFINES W-MED-92.                                     
064400         05    MED-92        PIC X(10) OCCURS 2.                          
064500                                                                          
064600                                                                          
064700*                        ****    MFS OCH SKÄRMHANTERING                   
064800 01  FILLER              PIC X(16)   VALUE 'MFS-WS'.                      
064900*01  MID -COPY W90408I1                                                   
065000     EJECT                                                                
065100*01  -COPY WMSGKOM                                                        
065200     EJECT                                                                
065300*01  -COPY WMSGAREA                                                       
065400     EJECT                                                                
065500*03  MOD -COPY W90408O1  -RED MSG-AREA.                                   
065600     EJECT                                                                
065700*01  -COPY WMFSAREA                                                       
065800     EJECT                                                                
065900******************************************************************        
066000*                                                                         
066100*        ARBETS-AREOR TILL DB2-SEKTIONERNA                                
066200*                                                                         
066300 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
066400       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
066500                                                                          
066600 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
066700 01  DB2-WS.                                                              
066800     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
066900         88  CURSOR-OK                      VALUE 000.                    
067000         88  LINES-FOUND                    VALUE 000.                    
067100         88  LINES-MISSING                  VALUE 100.                    
067200         88  RESOURCE-WRONG                 VALUE 904.                    
067300     03  GOOD-SQLCODECODES.                                               
067400         05  GOOD-SQLCODE OCCURS 5                                        
067500             INDEXED BY SQLCODE-IX PIC 9(3).                              
067600     EJECT                                                                
067700******************************************************************        
067800*****                                                                     
067900*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
068000*****                                                                     
068100 01  IMS-WS.                                                              
068200     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
068300     SKIP3                                                                
068400*****                    **** STATUS-KOD FRÅN IMS                         
068500     03  STATUS-WS               PIC X(2).                                
068600         88  SEGMENT-FINNS                   VALUE '  '.                  
068700         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
068800         88  BASEN-SLUT                      VALUE 'GB'.                  
068900         88  SEGMENT-FINNS-REDAN             VALUE 'II'.                  
069000     SKIP3                                                                
069100     03  GODK-STATUSKODER.                                                
069200         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
069300     SKIP3                                                                
069400 01  SSA1                        PIC X(128).                              
069500 01  SSA2                        PIC X(128).                              
069600 01  SSA3                        PIC X(128).                              
069700     EJECT                                                                
069800*                            IMS FUNKTIONSKODER                           
069900*01  -COPY W0003                                                          
070000     EJECT                                                                
070100*                            DLI INPUT-OUTPUT AREA                        
070200 01  DLI-IO-AREA.                                                         
070300     03  IO-AREA                 PIC X(900)  VALUE SPACE.                 
070400     SKIP3                                                                
070500*    03  WLERSA  -COPY WDD701   -PRE ERSA01-  -RED IO-AREA.               
070600     EJECT                                                                
070700*    03  WLERSA  -COPY WDD702   -PRE ERSA11-  -RED IO-AREA.               
070800     EJECT                                                                
070900*    03  WLERSA  -COPY WDD704   -PRE ERSA13-  -RED IO-AREA.               
071000     EJECT                                                                
071100*    03  WLINLB  -COPY WDD902   -PRE INLB11-  -RED IO-AREA.               
071200     EJECT                                                                
071300*    03  WLARTC  -COPY WDK601                 -RED IO-AREA.               
071400     EJECT                                                                
071500*    03  WLARTC  -COPY WDK611                 -RED IO-AREA.               
071600     EJECT                                                                
071700*    03  WLARTC  -COPY WDK623                 -RED IO-AREA.               
071800     EJECT                                                                
071900*    03  WDGX2304 -COPY WDGX2304 -PRE 2303-  -RED IO-AREA.                
072000     EJECT                                                                
072100*    03  WLXXBJ  -COPY WDGX2204             -RED IO-AREA.                 
072200     EJECT                                                                
072300*    03  WLXXID  -COPY WDG39101 -PRE XXID-  -RED IO-AREA.                 
072400     EJECT                                                                
072500*    03  WLXXAW  -COPY WDGX1116 -PRE XXAW-  -RED IO-AREA.                 
072600     EJECT                                                                
072700*    03  WLXXCW  -COPY WDGX1158 -PRE XXCW-  -RED IO-AREA.                 
072800     EJECT                                                                
072900*    03  WLZZAC  -COPY WDGZ01   -PRE ZZAC-  -RED IO-AREA.                 
073000     EJECT                                                                
073100*    03  WLXXAV  -COPY WDGX1142 -PRE XXAV-  -RED IO-AREA.                 
073200     EJECT                                                                
073300 01  DLI-IO-AREA-2.                                                       
073400     03  IO-AREA-2               PIC X(200)  VALUE SPACE.                 
073500     SKIP3                                                                
073600*    03  WLXXAN  -COPY WDGX1111 -PRE XXAN-  -RED IO-AREA-2.               
073700     EJECT                                                                
073800*    03  WLXXAN  -COPY WDGX1112 -PRE XXAN-  -RED IO-AREA-2.               
073900     EJECT                                                                
074000*    03  WLXXAN  -COPY WDGX1114 -PRE XXAN-  -RED IO-AREA-2.               
074100     EJECT                                                                
074200 01  DLI-IO-AREA-3.                                                       
074300     03  IO-AREA-3               PIC X(600)  VALUE SPACE.                 
074400     SKIP3                                                                
074500*    03  WLARTG  -COPY WDD201   -PRE ARTG-  -RED IO-AREA-3.               
074600     EJECT                                                                
074700 01  DLI-IO-AREA-5.                                                       
074800     03  IO-AREA-5               PIC X(500)  VALUE SPACE.                 
074900     03  WDJ1CSEQ  REDEFINES IO-AREA-5.                                   
075000*       05  WLSATB11  -COPY WDJ111  -PRE SATB-                            
075100*       05  WLSATB01  -COPY WDJ101  -PRE SATB-                            
075200     EJECT                                                                
075300 01  DLI-IO-AREA-6.                                                       
075400     03  IO-AREA-6               PIC X(20)  VALUE SPACE.                  
075500     SKIP3                                                                
075600*    03  WLXXBW01 INGEN COPYTEXT TOM ROT                                  
075700*    03  WLXXBW11  -COPY WDGX2228     -RED IO-AREA-6.                     
075800     EJECT                                                                
075900 01  DLI-IO-AREA-7.                                                       
076000*    03  AREA    -COPY WDR301   -PRE FILC-                                
076100     EJECT                                                                
076200 01  DLI-IO-AREA-8.                                                       
076300*    03  -COPY WDB601                                                     
076400     EJECT                                                                
076500 01  FILLER                      PIC X(16)   VALUE 'WDK701-AREA'.         
076600 01  DLI-IO-AREA-WDK701.                                                  
076700*    03  -COPY WDK701                                                     
076800     EJECT                                                                
076900 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA'.         
077000 01  DLI-IO-AREA-WDK711.                                                  
077100*    03  -COPY WDK711                                                     
077200     EJECT                                                                
077300 01  FILLER                      PIC X(16)   VALUE 'WDK722-AREA'.         
077400 01  DLI-IO-AREA-WDK722.                                                  
077500*    03  -COPY WDK722                                                     
077600     EJECT                                                                
077700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDR220'.        
077800 01  DLI-IO-AREA-2232.                                                    
077900*    03  -COPY WDGX2232   -PRE WDR220-                                    
078000     EJECT                                                                
078100 01  FILLER                      PIC X(16)  VALUE 'WDR501-AREA'.          
078200 01  DLI-IO-AREA-2223.                                                    
078300*    03  -COPY WDGX2223   -PRE WDR501-                                    
078400     EJECT                                                                
078500 01  FILLER                      PIC X(16)  VALUE 'WDR550-AREA'.          
078600 01  DLI-IO-AREA-2224.                                                    
078700*    03  -COPY WDGX2224   -PRE WDR550-                                    
078800     EJECT                                                                
078900                                                                          
079000*    ------------- D B 2 INPUT-OUTPUT AREA ---------------                
079100                                                                          
079200 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
079300*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
079400     EJECT                                                                
079500 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
079600*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
079700     EJECT                                                                
079800     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
079900     EJECT                                                                
080000     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
080100     EJECT                                                                
080200                                                                          
080300 LINKAGE SECTION.                                                         
080400     SKIP2                                                                
080500*01  -COPY W0009     -PRE MSG-                                            
080600     EJECT                                                                
080700*01  -COPY W0009     -PRE MSGKOM-                                         
080800     EJECT                                                                
080900*01  -COPY W0008     -PRE USEA-                                           
081000         05  FILLER              PIC X.                                   
081100     EJECT                                                                
081200*01  -COPY W0008     -PRE ERSA-                                           
081300         05  FILLER              PIC X.                                   
081400     EJECT                                                                
081500*01  -COPY W0008     -PRE ERSB-                                           
081600         05  FILLER              PIC X.                                   
081700     EJECT                                                                
081800*01  -COPY W0008     -PRE ARTC-                                           
081900         05  FILLER              PIC X.                                   
082000     EJECT                                                                
082100*01  -COPY W0008     -PRE INLB-                                           
082200         05  FILLER              PIC X.                                   
082300     EJECT                                                                
082400*01  -COPY W0008     -PRE XXAN-                                           
082500         05  FILLER              PIC X.                                   
082600     EJECT                                                                
082700*01  -COPY W0008     -PRE 2303-                                           
082800         05  FILLER              PIC X.                                   
082900     EJECT                                                                
083000*01  -COPY W0008     -PRE XXBJ-                                           
083100         05  FILLER              PIC X.                                   
083200     EJECT                                                                
083300*01  -COPY W0008     -PRE XXID-                                           
083400         05  FILLER              PIC X.                                   
083500     EJECT                                                                
083600*01  -COPY W0008     -PRE ZZAC-                                           
083700         05  FILLER              PIC X.                                   
083800     EJECT                                                                
083900*01  -COPY W0008     -PRE ART2-                                           
084000         05  FILLER              PIC X.                                   
084100     EJECT                                                                
084200*01  -COPY W0008     -PRE XXAV-                                           
084300         05  FILLER              PIC X.                                   
084400     EJECT                                                                
084500*01  -COPY W0008     -PRE ARTG-                                           
084600         05  FILLER              PIC X.                                   
084700     EJECT                                                                
084800*01  -COPY W0008     -PRE XXAW-                                           
084900         05  FILLER              PIC X.                                   
085000     EJECT                                                                
085100*01  -COPY W0008     -PRE SATE-                                           
085200         05  FILLER              PIC X.                                   
085300     EJECT                                                                
085400*01  -COPY W0008     -PRE SATB-                                           
085500         05  FILLER              PIC X.                                   
085600     EJECT                                                                
085700*01  -COPY W0008     -PRE XXBW-                                           
085800         05  FILLER              PIC X.                                   
085900     EJECT                                                                
086000*01  -COPY W0008     -PRE XXCW-                                           
086100         05  FILLER              PIC X.                                   
086200     EJECT                                                                
086300*01  -COPY W0008     -PRE FILC-                                           
086400         05  FILLER              PIC X.                                   
086500     EJECT                                                                
086600*01  -COPY W0008     -PRE WDK7-                                           
086700     05  FILLER                  PIC X.                                   
086800     EJECT                                                                
086900*01  -COPY W0008     -PRE WDB6-                                           
087000     05  FILLER                  PIC X.                                   
087100     EJECT                                                                
087200*01  -COPY W0008     -PRE WDR2-                                           
087300     05  FILLER                  PIC X.                                   
087400     EJECT                                                                
087500*01  -COPY W0008     -PRE WDR5-                                           
087600     05  FILLER                  PIC X.                                   
087700     EJECT                                                                
087800 PROCEDURE DIVISION USING MSG-PCB MSGKOM-PCB USEA-PCB                     
087900                          ARTC-PCB ERSA-PCB ERSB-PCB                      
088000                          INLB-PCB          XXAN-PCB                      
088100                          2303-PCB XXBJ-PCB                               
088200                          XXID-PCB ZZAC-PCB ART2-PCB                      
088300                          XXAV-PCB ARTG-PCB XXAW-PCB                      
088400                          SATE-PCB SATB-PCB                               
088500                          XXBW-PCB XXCW-PCB                               
088600                          FILC-PCB WDK7-PCB WDB6-PCB                      
088700                          WDR2-PCB WDR5-PCB.                              
088800                                                                          
088900     ENTRY 'DLITCBL' USING MSG-PCB MSGKOM-PCB USEA-PCB                    
089000                           ARTC-PCB ERSA-PCB ERSB-PCB                     
089100                           INLB-PCB          XXAN-PCB                     
089200                           2303-PCB XXBJ-PCB                              
089300                           XXID-PCB ZZAC-PCB ART2-PCB                     
089400                           XXAV-PCB ARTG-PCB XXAW-PCB                     
089500                           SATE-PCB SATB-PCB                              
089600                           XXBW-PCB XXCW-PCB                              
089700                           FILC-PCB WDK7-PCB WDB6-PCB                     
089800                           WDR2-PCB WDR5-PCB.                             
089900     SKIP2                                                                
090000     PERFORM IMS-GET-MSG                                                  
090100     IF SEGMENT-FINNS                                                     
090200       PERFORM IMS-GET-WMSGKOM                                            
090300       PERFORM A-INIT                                                     
090400       IF WS-IDARTNR NUMERIC                                              
090500                                                                          
090600         IF MFS-UPDATE OR MFS-UPD-X                                       
090700           PERFORM C-KOLLA-ACTION-FILE                                    
090800           IF UPPDATERING-TILLATEN = NEJ                                  
090900             MOVE MED-18(SPRAK-IX) TO MOD-TEMFSINF                        
091000             PERFORM S22-ROER-EJ-FAELT                                    
091100           ELSE                                                           
091200             PERFORM H-LAS-ERSATT-ARTIKEL                                 
091300             IF INPUT-RETT = JA                                           
091400               PERFORM B-KOLLA-INDATA                                     
091500               IF INPUT-RETT = JA                                         
091600                 IF MID-KDERS = ALL '+'                                   
091700                   MOVE IDARTNR-WS TO W-IDARTNR                           
091800                   PERFORM IMS-GET-ERSA01                                 
091900                   IF SEGMENT-FINNS                                       
092000                     IF UPPDAT-TIERSDAT-TEARTNOT = JA                     
092100                       IF MID-FLKLAR = JA OR YES                          
092200                         PERFORM K-UPPDAT-TIERSDAT-TEARTNOT               
092300                       ELSE                                               
092400                         MOVE MFS-ADD-SAETT-CURSOR TO                     
092500                                                 MOD-FLKLAR-ATTR          
092600                         MOVE MED-22(SPRAK-IX) TO MOD-TEMFSINF            
092700                       END-IF                                             
092800                     ELSE                                                 
092900                       IF MID-FLKLAR = NEJ                                
093000                         PERFORM S17-UPPDAT-TILLK-ART-ACTION              
093100                       ELSE                                               
093200                         PERFORM I-UPPDAT-TILLK-ART-ERS-REG               
093300                         IF INPUT-RETT = JA                               
093400                           MOVE ERSATT-KDERS-C1 TO WS-KDERS               
093500                           PERFORM S16-SKAPA-VR-TRANS                     
093600                         ELSE                                             
093700                           MOVE FEL-3(SPRAK-IX) TO MOD-TEMFSFEL           
093800                         END-IF                                           
093900                       END-IF                                             
094000                     END-IF                                               
094100                   END-IF                                                 
094200                 ELSE                                                     
094300                   IF MID-KDERS = ZERO                                    
094400                     IF MID-FLKLAR = JA OR YES                            
094500                        PERFORM D-RIVNING-AV-ERSAETTNING                  
094600                     ELSE                                                 
094700                        MOVE MED-22(SPRAK-IX) TO MOD-TEMFSINF             
094800                        MOVE MFS-ADD-SAETT-CURSOR                         
094900                                         TO MOD-FLKLAR-ATTR               
095000                     END-IF                                               
095100                   ELSE                                                   
095200                     PERFORM F-KONTROLL-REG-UPPDAT-AV-ERS                 
095300                     IF INPUT-RETT = JA                                   
095400                       IF MID-FLKLAR = JA OR YES                          
095500                          PERFORM S16-SKAPA-VR-TRANS                      
095600                          PERFORM S24-TRANS-TILL-BASL                     
095700                          PERFORM S25-TRANS-TILL-PPMS                     
095800                          PERFORM S27-TRANS-TILL-WDR5                     
095900                          PERFORM S28-SKAPA-B65-TRANS                     
096000                       END-IF                                             
096100                     ELSE                                                 
096200                       MOVE FEL-3(SPRAK-IX) TO MOD-TEMFSFEL               
096300                     END-IF                                               
096400                   END-IF                                                 
096500                 END-IF                                                   
096600               ELSE                                                       
096700                 MOVE FEL-3(SPRAK-IX) TO MOD-TEMFSFEL                     
096800               END-IF                                                     
096900             ELSE                                                         
097000               MOVE FEL-3(SPRAK-IX) TO MOD-TEMFSFEL                       
097100             END-IF                                                       
097200           END-IF                                                         
097300         ELSE                                                             
097400****       IT IS NEITHER MFS-UPDATE NOR MFS-UPD-X                         
097500           PERFORM G-LAS                                                  
097600         END-IF                                                           
097700       ELSE                                                               
097800         MOVE FEL-1(SPRAK-IX) TO MOD-TEMFSFEL                             
097900       END-IF                                                             
098000       IF MFS-IDTRANS = '2129' AND MFS-KDMFSFOR = '3'                     
098100         CONTINUE                                                         
098200       ELSE                                                               
098300*        MOVE IDARTNR-WS TO MOD-IDARTNR-UT                                
098400*        INSPECT MOD-IDARTNR-UT REPLACING LEADING ZERO BY SPACE           
098500         IF MFS-UPD-X                                                     
098600           PERFORM IMS-INSERT-WMSGKOM                                     
098700         ELSE                                                             
098800           MOVE MAX-MOD-LAENGD TO MSG-KVLL                                
098900           PERFORM IMS-INSERT-MSG                                         
099000         END-IF                                                           
099100       END-IF                                                             
099200     END-IF                                                               
099300                                                                          
099400     MOVE ZERO TO RETURN-CODE                                             
099500     GOBACK                                                               
099600     .                                                                    
099700     EJECT                                                                
099800 A-INIT SECTION.                                                          
099900                                                                          
100000     IF MSG-DUBBLA-TRANSKODER                                             
100100         MOVE MSG-INDATA-MINUS-2-TRANSKODER TO MID-W90408I1               
100200         MOVE MSG-IDTRANS-2  TO MFS-IDTRANS                               
100300         MOVE MSG-KDMFSFOR-2 TO MFS-KDMFSFOR                              
100400         MOVE MSG-KDTRTYP    TO MFS-KDTRTYP                               
100500     ELSE                                                                 
100600         MOVE MSG-INDATA-MINUS-1-TRANSKOD TO MID-W90408I1                 
100700         MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                               
100800         MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                              
100900         IF MSG-KDTRANS-1 = 'W1T113X'                                     
101000            MOVE MSG-KDTRTYP TO MFS-KDTRTYP                               
101100          ELSE                                                            
101200            MOVE ' '         TO MFS-KDTRTYP                               
101300         END-IF                                                           
101400     END-IF                                                               
101500                                                                          
101600     IF MFS-UPD-X                                                         
101700        IF MID-IDARTNR-IN = ALL '+' OR SPACE                              
101800           MOVE MID-IDARTNR-UT TO WS-IDARTNR                              
101900           CONTINUE                                                       
102000        ELSE                                                              
102100           MOVE MID-IDARTNR-IN TO WS-IDARTNR                              
102200           MOVE '7'   TO MFS-IDPFK                                        
102300           MOVE SPACE TO MFS-KDTRTYP                                      
102400        END-IF                                                            
102500        INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                    
102600        MOVE WC-CDC-SE TO WS-IDDC                                         
102700     ELSE                                                                 
102800        MOVE ALL '+' TO MSGI-WMSGINIT                                     
102900        MOVE '001'             TO MSGI-KDCALL                             
103000        MOVE MSG-SIGNON-USERID TO MSGI-IDUSER                             
103100                                  MSGI-IDLTERM-USER                       
103200        MOVE '9408'            TO MSGI-IDTRANS                            
103300        IF MFS-IDTRANS = '9408'                                           
103400        OR (MID-IDARTNR-IN NUMERIC                                        
103500        AND MID-IDARTNR-IN > ZERO)                                        
103600            MOVE MID-IDARTNR-IN TO MSGI-IDARTNR                           
103700        END-IF                                                            
103800        CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                        
103900        MOVE MSGI-IDARTNR TO WS-IDARTNR                                   
104000        MOVE MSGI-IDDC    TO WS-IDDC                                      
104100        INSPECT WS-IDARTNR REPLACING ALL SPACE BY ZERO                    
104200     END-IF                                                               
104300                                                                          
104400     IF MID-IDARTNR-IN = ALL '+' OR SPACE                                 
104500        CONTINUE                                                          
104600     ELSE                                                                 
104700        MOVE SPACE TO MFS-KDTRTYP                                         
104800        MOVE ZERO TO WS-IDKORTNR-SPAR1                                    
104900                     WS-IDKORTNR-SPAR2                                    
105000                     WS-IDKORTNR-SPAR3                                    
105100     END-IF                                                               
105200                                                                          
105300     IF MFS-IDTRANS = '9408'                                              
105400        CONTINUE                                                          
105500     ELSE                                                                 
105600        IF MFS-IDTRANS = '2129' AND MFS-KDMFSFOR = '3'                    
105700           MOVE 'U' TO MFS-KDTRTYP                                        
105800           MOVE ZERO TO WS-IDKORTNR-SPAR1                                 
105900                        WS-IDKORTNR-SPAR2                                 
106000                        WS-IDKORTNR-SPAR3                                 
106100        ELSE                                                              
106200           MOVE SPACE TO MFS-KDTRTYP                                      
106300           MOVE ZERO TO WS-IDKORTNR-SPAR1                                 
106400                        WS-IDKORTNR-SPAR2                                 
106500                        WS-IDKORTNR-SPAR3                                 
106600        END-IF                                                            
106700     END-IF                                                               
106800                                                                          
106900     ACCEPT DAGENS-DATUM FROM DATE                                        
107000                                                                          
107100     MOVE LOW-VALUE TO MOD-W90408O1                                       
107200     MOVE 'W90408O1' TO MFS-IDMOD                                         
107300     MOVE '9408' TO MOD-IDTRANS                                           
107400                                                                          
107500     MOVE MFS-RENSA-FAELT TO MOD-TEMFSFEL                                 
107600                             MOD-TEMFSINF                                 
107700                                                                          
107800     IF MSGI-IDLAND-SPR = 'GB'                                            
107900       MOVE +2    TO SPRAK-IX                                             
108000       MOVE 'GB ' TO W-IDSKYLT                                            
108100     ELSE                                                                 
108200       MOVE +1    TO SPRAK-IX                                             
108300       MOVE 'S  ' TO W-IDSKYLT                                            
108400     END-IF                                                               
108500     .                                                                    
108600     EJECT                                                                
108700 B-KOLLA-INDATA SECTION.                                                  
108800     SKIP2                                                                
108900     MOVE NEJ TO RADER-UPPDATERADE                                        
109000                 UPPDAT-TIERSDAT-TEARTNOT                                 
109100     MOVE MID-KDERS TO WS-KDERS                                           
109200     MOVE MID-TIERSDAT-PREL TO XX-TIERSDAT-PREL                           
109300     MOVE '+'               TO XX-DAG                                     
109400     MOVE XX-TIERSDAT-PREL  TO MID-TIERSDAT-PREL                          
109500                                                                          
109600     IF MID-KDERS = ALL '+'                                               
109700       IF MID-TIERSDAT-PREL = ALL '+'                                     
109800         AND MID-DIERS-ERS = ALL '+'                                      
109900         AND MID-IDAO = ALL '+' AND MID-TEARTNOT = ALL '+'                
110000         PERFORM BA-KOLLA-RADER                                           
110100       ELSE                                                               
110200         IF MID-DIERS-ERS = ALL '+' AND MID-IDAO = ALL '+'                
110300           AND MID-TEARTNOT = ALL '+'                                     
110400           MOVE +1 TO RAD-IX                                              
110500           PERFORM UNTIL RAD-IX > MAX-RAD                                 
110600             IF  MID-IDARTNR-TILLK(RAD-IX) = ALL '+'                      
110700             AND MID-BEERS(RAD-IX) = ALL '+'                              
110800             AND MID-DIERS-TILLK(RAD-IX) = ALL '+'                        
110900               CONTINUE                                                   
111000             ELSE                                                         
111100               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                  
111200               MOVE NEJ TO INPUT-RETT                                     
111300             END-IF                                                       
111400             ADD +1 TO RAD-IX                                             
111500           END-PERFORM                                                    
111600           PERFORM BC-KOLLA-TIERSDAT-PREL                                 
111700         ELSE                                                             
111800           IF MID-DIERS-ERS = ALL '+' AND MID-IDAO = ALL '+'              
111900           AND MID-TIERSDAT-PREL = ALL '+'                                
112000             MOVE +1 TO RAD-IX                                            
112100             PERFORM UNTIL RAD-IX > MAX-RAD                               
112200               IF  MID-IDARTNR-TILLK(RAD-IX) = ALL '+'                    
112300               AND MID-BEERS(RAD-IX) = ALL '+'                            
112400               AND MID-DIERS-TILLK(RAD-IX) = ALL '+'                      
112500                 CONTINUE                                                 
112600               ELSE                                                       
112700                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                
112800                 MOVE NEJ TO INPUT-RETT                                   
112900               END-IF                                                     
113000               ADD +1 TO RAD-IX                                           
113100             END-PERFORM                                                  
113200             IF MID-TEARTNOT = ALL '+' OR SPACE                           
113300               MOVE NEJ TO INPUT-RETT                                     
113400             ELSE                                                         
113500               MOVE MFS-ALFA-FAELT-RAETT TO                               
113600                                      MOD-TEARTNOT-ATTR                   
113700             END-IF                                                       
113800             IF INPUT-RETT = JA                                           
113900               MOVE JA TO UPPDAT-TIERSDAT-TEARTNOT                        
114000             END-IF                                                       
114100           ELSE                                                           
114200             MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                    
114300             MOVE NEJ TO INPUT-RETT                                       
114400             MOVE MED-3(SPRAK-IX) TO MOD-TEMFSINF                         
114500           END-IF                                                         
114600         END-IF                                                           
114700       END-IF                                                             
114800     ELSE                                                                 
114900****** OM MID-KDERS IFYLLT                                                
115000       IF MID-KDERS NUMERIC                                               
115100         MOVE MID-KDERS TO WS-KDERS                                       
115200         IF MID-KDERS = '01' OR '02' OR '03' OR '04' OR '05'              
115300                     OR '06' OR '07' OR '08' OR '09'                      
115400                     OR '21' OR '22' OR '23' OR '24' OR '25'              
115500                     OR '26' OR '27' OR '28' OR '29'                      
115600                     OR '00' OR '52'                                      
115700           MOVE MFS-ALFA-FAELT-RAETT TO MOD-KDERS-ATTR                    
115800           IF MID-KDERS = ZERO                                            
115900             IF ERSATT-KDERS-UTG > 0                                      
116000               MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                  
116100               MOVE NEJ TO INPUT-RETT                                     
116200               MOVE MED-17(SPRAK-IX) TO MOD-TEMFSINF                      
116300             ELSE                                                         
116400               IF MID-TIERSDAT-PREL = ALL '+'                             
116500               AND MID-TEARTNOT     = ALL '+'                             
116600               AND MID-DIERS-ERS    = ALL '+'                             
116700                 MOVE +1 TO RAD-IX                                        
116800                 PERFORM UNTIL RAD-IX > MAX-RAD                           
116900                   IF  MID-IDARTNR-TILLK(RAD-IX) = ALL '+'                
117000                   AND MID-BEERS(RAD-IX)         = ALL '+'                
117100                   AND MID-DIERS-TILLK(RAD-IX)   = ALL '+'                
117200                     CONTINUE                                             
117300                   ELSE                                                   
117400                     MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR            
117500                     MOVE NEJ TO INPUT-RETT                               
117600                   END-IF                                                 
117700                   ADD +1 TO RAD-IX                                       
117800                 END-PERFORM                                              
117900               ELSE                                                       
118000                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                
118100                 MOVE NEJ TO INPUT-RETT                                   
118200               END-IF                                                     
118300             END-IF                                                       
118400           ELSE                                                           
118500**********   OM MID-KDERS > 00                                            
118600             IF WS-KDERS-2 = 3 OR 6                                       
118700               IF MID-TIERSDAT-PREL NOT = ALL '+'                         
118800                 MOVE SPACE             TO XX-DAG                         
118900                 MOVE XX-TIERSDAT-PREL  TO MID-TIERSDAT-PREL              
119000                 IF MID-TIERSDAT-PREL NOT = SPACE                         
119100                   MOVE '1'              TO XX-DAG                        
119200                   MOVE XX-TIERSDAT-PREL TO MID-TIERSDAT-PREL             
119300                   IF MID-TIERSDAT-PREL NUMERIC                           
119400                     MOVE MID-TIERSDAT-PREL TO WS-TIERSDAT-PREL           
119500                     IF WS-VECKA-PREL > 00 AND < 54                       
119600                       MOVE 'AAVVD  ' TO DAT-KDDATFORM                    
119700                       MOVE MID-TIERSDAT-PREL TO DAT-I-TIDATUM            
119800                       CALL WDATKONV USING DAT-KDDATFORM                  
119900                                           DAT-I-TIDATUM                  
120000                                           DAT-O-TIDATUM                  
120100                                           DAT-KDSVAR                     
120200                       IF DAT-KDSVAR-OK                                   
120300                         MOVE DAT-TIAAMMDD   TO TMP1-YYMMDD               
120400                         MOVE DAGENS-DATUM   TO TMP2-YYMMDD               
120500                         PERFORM WY2000P1                                 
120600                         IF TMP1-YYMMDD > TMP2-YYMMDD                     
120700                           MOVE MFS-ALFA-FAELT-RAETT                      
120800                                      TO MOD-TIERSDAT-PREL-ATTR           
120900                         ELSE                                             
121000                           MOVE MFS-ALFA-FAELT-FEL TO                     
121100                                         MOD-TIERSDAT-PREL-ATTR           
121200                           MOVE NEJ TO INPUT-RETT                         
121300                           MOVE MED-5(SPRAK-IX) TO MOD-TEMFSINF           
121400                         END-IF                                           
121500                       ELSE                                               
121600                         MOVE MFS-ALFA-FAELT-FEL TO                       
121700                                            MOD-TIERSDAT-PREL-ATTR        
121800                         MOVE NEJ TO INPUT-RETT                           
121900                         MOVE MED-5(SPRAK-IX) TO MOD-TEMFSINF             
122000                       END-IF                                             
122100                     ELSE                                                 
122200                       MOVE MFS-ALFA-FAELT-FEL                            
122300                                TO MOD-TIERSDAT-PREL-ATTR                 
122400                       MOVE NEJ TO INPUT-RETT                             
122500                       MOVE MED-5(SPRAK-IX) TO MOD-TEMFSINF               
122600                     END-IF                                               
122700                   ELSE                                                   
122800                     MOVE MFS-ALFA-FAELT-FEL                              
122900                              TO MOD-TIERSDAT-PREL-ATTR                   
123000                     MOVE NEJ TO INPUT-RETT                               
123100                     MOVE MED-4(SPRAK-IX) TO MOD-TEMFSINF                 
123200                   END-IF                                                 
123300                 ELSE                                                     
123400                   MOVE MFS-ALFA-FAELT-FEL                                
123500                            TO MOD-TIERSDAT-PREL-ATTR                     
123600                   MOVE NEJ TO INPUT-RETT                                 
123700                   MOVE MED-4(SPRAK-IX) TO MOD-TEMFSINF                   
123800                 END-IF                                                   
123900               ELSE                                                       
124000                 MOVE MFS-ALFA-FAELT-FEL                                  
124100                          TO MOD-TIERSDAT-PREL-ATTR                       
124200                 MOVE NEJ TO INPUT-RETT                                   
124300                 MOVE MED-4(SPRAK-IX) TO MOD-TEMFSINF                     
124400               END-IF                                                     
124500             ELSE                                                         
124600               IF MID-TIERSDAT-PREL = ALL '+'                             
124700                 CONTINUE                                                 
124800               ELSE                                                       
124900                 MOVE SPACE TO XX-DAG                                     
125000                 MOVE XX-TIERSDAT-PREL TO MID-TIERSDAT-PREL               
125100                 IF MID-TIERSDAT-PREL = SPACE                             
125200                   CONTINUE                                               
125300                 ELSE                                                     
125400                   MOVE MFS-ALFA-FAELT-FEL                                
125500                            TO MOD-TIERSDAT-PREL-ATTR                     
125600                   MOVE NEJ TO INPUT-RETT                                 
125700                 END-IF                                                   
125800               END-IF                                                     
125900             END-IF                                                       
126000                                                                          
126100             IF MID-KDERS = '09' OR '29' OR '52'                          
126200               IF ERSATT-FLIART = JA                                      
126300                 MOVE IDARTNR-WS TO W-IDARTNR-S                           
126400                 MOVE SPACE TO W-IDLEVNR-S                                
126500                               W-BELEVART-S                               
126600                 PERFORM IMS-GET-SATB-CSEQ                                
126700                 PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT               
126800                   IF SATB-STR-IDLEVNR = '1002 '                          
126900                     IF SATB-STR-IDARTNR < 100000000                      
127000                       IF SATB-STR-TIBORT = 0                             
127100                         MOVE SATB-RAD-TISTODAT  TO TMP1-YYMMDD           
127200                         MOVE DAGENS-DATUM       TO TMP2-YYMMDD           
127300                         PERFORM WY2000P1                                 
127400                         IF TMP1-YYMMDD > TMP2-YYMMDD                     
127500                           MOVE SATB-STR-IDARTNR TO W-IDARTNR             
127600                           PERFORM IMS-GET-ARTC01                         
127700                           PERFORM IMS-GET-ARTC11                         
127800                           IF SEGMENT-FINNS                               
127900                             IF CLAG-KDERS < 09                           
128000                                MOVE NEJ TO INPUT-RETT                    
128100                                MOVE MFS-ALFA-FAELT-FEL                   
128200                                             TO MOD-KDERS-ATTR            
128300                                MOVE MED-9(SPRAK-IX)                      
128400                                             TO MOD-TEMFSINF              
128500                             END-IF                                       
128600                           END-IF                                         
128700                         END-IF                                           
128800                       END-IF                                             
128900                     END-IF                                               
129000                   END-IF                                                 
129100                   PERFORM IMS-GET-SATB-CSEQ                              
129200                 END-PERFORM                                              
129300               END-IF                                                     
129400             END-IF                                                       
129500                                                                          
129600             IF ERSATT-KDERS-UTG > 0                                      
129700               IF WS-KDERS-1 = 2                                          
129800                 CONTINUE                                                 
129900               ELSE                                                       
130000                 MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                
130100                 MOVE NEJ TO INPUT-RETT                                   
130200               END-IF                                                     
130300             END-IF                                                       
130400                                                                          
130500             IF MID-IDAO = ALL '+' OR SPACE                               
130600               IF ERSATT-KDERS-C1 = 00                                    
130700                 AND MID-KDERS = '09'                                     
130800                 CONTINUE                                                 
130900               ELSE                                                       
131000                 IF (ERSATT-KDERS-C1 = 21 OR 22 OR 23 OR 24 OR 25         
131100                 OR 26 OR 27 OR 28 OR 29)                                 
131200                   CONTINUE                                               
131300                 ELSE                                                     
131400                   MOVE MFS-ALFA-FAELT-FEL TO MOD-IDAO-ATTR               
131500                   MOVE NEJ TO INPUT-RETT                                 
131600                   MOVE MED-11(SPRAK-IX) TO MOD-TEMFSINF                  
131700                 END-IF                                                   
131800               END-IF                                                     
131900           END-IF                                                         
132000                                                                          
132100           IF MID-DIERS-ERS = ALL '+' OR SPACE                            
132200             MOVE MFS-NUM-FAELT-FEL TO MOD-DIERS-ERS-ATTR                 
132300             MOVE NEJ TO INPUT-RETT                                       
132400             MOVE MED-1(SPRAK-IX) TO MOD-TEMFSINF                         
132500           ELSE                                                           
132600             MOVE MID-DIERS-ERS TO DEC-IDFRIDATA                          
132700             MOVE 3             TO DEC-KVHELTAL                           
132800             MOVE 3             TO DEC-KVDECIMAL                          
132900             CALL WDECEDIT USING DEC-WDECAREA                             
133000             IF DEC-KDSVAR-OK                                             
133100               IF DEC-IDEDITDATA = ZERO                                   
133200                 MOVE MFS-NUM-FAELT-FEL TO MOD-DIERS-ERS-ATTR             
133300                 MOVE NEJ TO INPUT-RETT                                   
133400               ELSE                                                       
133500                 MOVE MFS-NUM-FAELT-RAETT TO MOD-DIERS-ERS-ATTR           
133600               END-IF                                                     
133700             ELSE                                                         
133800               MOVE MFS-NUM-FAELT-FEL TO                                  
133900                    MOD-DIERS-ERS-ATTR                                    
134000               MOVE NEJ TO INPUT-RETT                                     
134100             END-IF                                                       
134200           END-IF                                                         
134300                                                                          
134400           IF MID-TEARTNOT = ALL '+' OR SPACE                             
134500             CONTINUE                                                     
134600           ELSE                                                           
134700             MOVE MFS-ALFA-FAELT-RAETT TO MOD-TEARTNOT-ATTR               
134800           END-IF                                                         
134900                                                                          
135000           PERFORM BA-KOLLA-RADER                                         
135100           END-IF                                                         
135200         ELSE                                                             
135300           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                      
135400           MOVE NEJ TO INPUT-RETT                                         
135500           MOVE MED-3(SPRAK-IX) TO MOD-TEMFSINF                           
135600         END-IF                                                           
135700       ELSE                                                               
135800         MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                        
135900         MOVE NEJ TO INPUT-RETT                                           
136000         MOVE MED-3(SPRAK-IX) TO MOD-TEMFSINF                             
136100       END-IF                                                             
136200     END-IF                                                               
136300                                                                          
136400     IF MID-KDERS = ALL '+'                                               
136500       PERFORM BB-KOLLA-PREL-ERSKOD                                       
136600     END-IF                                                               
136700                                                                          
136800     IF MID-FLKLAR = JA OR YES OR NEJ                                     
136900       MOVE MFS-ALFA-FAELT-RAETT TO MOD-FLKLAR-ATTR                       
137000     ELSE                                                                 
137100       MOVE MFS-ALFA-FAELT-FEL TO MOD-FLKLAR-ATTR                         
137200       MOVE NEJ TO INPUT-RETT                                             
137300     END-IF                                                               
137400     .                                                                    
137500     EJECT                                                                
137600 BA-KOLLA-RADER SECTION.                                                  
137700     SKIP2                                                                
137800     IF MID-KDERS = ALL '+'                                               
137900        MOVE ERSATT-KDERS-C1 TO WS-KDERS                                  
138000     ELSE                                                                 
138100        MOVE MID-KDERS TO WS-KDERS                                        
138200     END-IF                                                               
138300                                                                          
138400     MOVE +1 TO RAD-IX                                                    
138500     PERFORM UNTIL RAD-IX > MAX-RAD                                       
138600      MOVE 'F' TO DEC-KDSVAR                                              
138700                                                                          
138800      IF MID-IDARTNR-TILLK(RAD-IX) = ALL '+' AND                          
138900         MID-DIERS-TILLK(RAD-IX) = ALL '+' AND                            
139000         MID-BEERS(RAD-IX) = ALL '+'                                      
139100         CONTINUE                                                         
139200      ELSE                                                                
139300       IF MID-IDKORTNR(RAD-IX) = SPACE                                    
139400          CONTINUE                                                        
139500       ELSE                                                               
139600         INSPECT MID-IDKORTNR(RAD-IX) REPLACING ALL SPACE                 
139700         BY ZERO                                                          
139800       END-IF                                                             
139900       EVALUATE TRUE                                                      
140000       WHEN MID-IDKORTNR(RAD-IX) = ALL '+'                                
140100          IF (MID-IDARTNR-TILLK(RAD-IX) = ALL '+' OR                      
140200              SPACE) AND (MID-DIERS-TILLK(RAD-IX) = SPACE                 
140300              OR ALL '+') AND (MID-BEERS(RAD-IX) = SPACE                  
140400              OR ALL '+')                                                 
140500            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKORTNR-ATTR                
140600                         (RAD-IX)                                         
140700          ELSE                                                            
140800            MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKORTNR-ATTR(RAD-IX)          
140900            MOVE NEJ TO INPUT-RETT                                        
141000          END-IF                                                          
141100       WHEN MID-IDKORTNR(RAD-IX) = SPACE                                  
141200          IF (MID-IDARTNR-TILLK(RAD-IX) = ALL '+' OR                      
141300              SPACE) AND (MID-DIERS-TILLK(RAD-IX) = SPACE                 
141400              OR ALL '+') AND (MID-BEERS(RAD-IX) = SPACE                  
141500              OR ALL '+')                                                 
141600            MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKORTNR-ATTR                
141700                         (RAD-IX)                                         
141800          ELSE                                                            
141900            MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKORTNR-ATTR                  
142000                         (RAD-IX)                                         
142100            MOVE NEJ TO INPUT-RETT                                        
142200          END-IF                                                          
142300       WHEN MID-IDKORTNR(RAD-IX) NUMERIC                                  
142400          IF MID-IDKORTNR(RAD-IX) = ZERO                                  
142500             IF (MID-IDARTNR-TILLK(RAD-IX) = ALL '+' OR                   
142600                 SPACE) AND (MID-DIERS-TILLK(RAD-IX) = SPACE              
142700                 OR ALL '+') AND (MID-BEERS(RAD-IX) = SPACE               
142800                 OR ALL '+')                                              
142900                 MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKORTNR-ATTR           
143000                              (RAD-IX)                                    
143100             ELSE                                                         
143200                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKORTNR-ATTR             
143300                              (RAD-IX)                                    
143400                 MOVE NEJ TO INPUT-RETT                                   
143500             END-IF                                                       
143600          ELSE                                                            
143700             MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDKORTNR-ATTR               
143800                             (RAD-IX)                                     
143900          END-IF                                                          
144000                                                                          
144100       WHEN OTHER                                                         
144200          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDKORTNR-ATTR                    
144300                          (RAD-IX)                                        
144400          MOVE NEJ TO INPUT-RETT                                          
144500       END-EVALUATE                                                       
144600                                                                          
144700       IF MID-IDARTNR-TILLK(RAD-IX) = SPACE                               
144800          CONTINUE                                                        
144900       ELSE                                                               
145000          INSPECT MID-IDARTNR-TILLK(RAD-IX)                               
145100              REPLACING ALL SPACE BY ZERO                                 
145200       END-IF                                                             
145300                                                                          
145400       IF MID-DIERS-TILLK(RAD-IX) = SPACE                                 
145500          CONTINUE                                                        
145600       ELSE                                                               
145700          INSPECT MID-DIERS-TILLK(RAD-IX)                                 
145800              REPLACING ALL SPACE BY ZERO                                 
145900       END-IF                                                             
146000       IF (WS-KDERS-2 = 1 OR 2 OR 3 OR 7) AND (WS-KDERS NOT               
146100         = 52)                                                            
146200        EVALUATE TRUE                                                     
146300         WHEN MID-IDARTNR-TILLK(RAD-IX) = SPACE                           
146400           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDARTNR-TILLK-ATTR            
146500                                        (RAD-IX)                          
146600           IF MID-DIERS-TILLK(RAD-IX) = SPACE OR ZERO                     
146700             MOVE MFS-ALFA-FAELT-RAETT TO MOD-DIERS-TILLK-ATTR            
146800                                     (RAD-IX)                             
146900           ELSE                                                           
147000             MOVE MFS-ALFA-FAELT-FEL                                      
147100                  TO MOD-DIERS-TILLK-ATTR(RAD-IX)                         
147200             MOVE NEJ TO INPUT-RETT                                       
147300           END-IF                                                         
147400         WHEN MID-IDARTNR-TILLK(RAD-IX) = ALL '+'                         
147500           IF MID-DIERS-TILLK(RAD-IX) = ALL '+'                           
147600              MOVE MFS-ALFA-FAELT-RAETT TO MOD-DIERS-TILLK-ATTR           
147700                     (RAD-IX)                                             
147800           ELSE                                                           
147900              IF MID-DIERS-TILLK(RAD-IX) = SPACE                          
148000                 MOVE MFS-ALFA-FAELT-RAETT TO                             
148100                        MOD-DIERS-TILLK-ATTR(RAD-IX)                      
148200              ELSE                                                        
148300                 MOVE MFS-ALFA-FAELT-FEL                                  
148400                             TO MOD-DIERS-TILLK-ATTR(RAD-IX)              
148500                 MOVE NEJ TO INPUT-RETT                                   
148600              END-IF                                                      
148700           END-IF                                                         
148800         WHEN MID-IDARTNR-TILLK(RAD-IX) NOT NUMERIC                       
148900           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-TILLK-ATTR              
149000                                        (RAD-IX)                          
149100           MOVE NEJ TO INPUT-RETT                                         
149200           IF MID-DIERS-TILLK(RAD-IX) = ALL '+'                           
149300              MOVE MFS-ALFA-FAELT-FEL TO MOD-DIERS-TILLK-ATTR             
149400                        (RAD-IX)                                          
149500              MOVE NEJ TO INPUT-RETT                                      
149600           ELSE                                                           
149700              PERFORM BAB-KOLLA-DIERS-TILLK                               
149800                                                                          
149900              IF DEC-KDSVAR-OK                                            
150000                IF DEC-IDEDITDATA = ZERO                                  
150100                  MOVE MFS-ALFA-FAELT-FEL TO MOD-DIERS-TILLK-ATTR         
150200                                             (RAD-IX)                     
150300                  MOVE NEJ TO INPUT-RETT                                  
150400                ELSE                                                      
150500                  MOVE MFS-ALFA-FAELT-RAETT                               
150600                          TO MOD-DIERS-TILLK-ATTR(RAD-IX)                 
150700                END-IF                                                    
150800             ELSE                                                         
150900                MOVE MFS-ALFA-FAELT-FEL TO                                
151000                      MOD-DIERS-TILLK-ATTR(RAD-IX)                        
151100                MOVE NEJ TO INPUT-RETT                                    
151200             END-IF                                                       
151300           END-IF                                                         
151400         WHEN MID-IDARTNR-TILLK(RAD-IX) NUMERIC                           
151500           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDARTNR-TILLK-ATTR            
151600                                       (RAD-IX)                           
151700           IF MID-DIERS-TILLK(RAD-IX) = ALL '+'                           
151800              MOVE MFS-ALFA-FAELT-FEL TO MOD-DIERS-TILLK-ATTR             
151900                        (RAD-IX)                                          
152000              MOVE NEJ TO INPUT-RETT                                      
152100           ELSE                                                           
152200              PERFORM BAB-KOLLA-DIERS-TILLK                               
152300                                                                          
152400              IF DEC-KDSVAR-OK                                            
152500                IF DEC-IDEDITDATA = ZERO                                  
152600                  MOVE MFS-ALFA-FAELT-FEL TO MOD-DIERS-TILLK-ATTR         
152700                                             (RAD-IX)                     
152800                  MOVE NEJ TO INPUT-RETT                                  
152900                ELSE                                                      
153000                  MOVE MFS-ALFA-FAELT-RAETT                               
153100                          TO MOD-DIERS-TILLK-ATTR(RAD-IX)                 
153200                END-IF                                                    
153300             ELSE                                                         
153400                MOVE MFS-ALFA-FAELT-FEL TO                                
153500                      MOD-DIERS-TILLK-ATTR(RAD-IX)                        
153600                MOVE NEJ TO INPUT-RETT                                    
153700             END-IF                                                       
153800           END-IF                                                         
153900        END-EVALUATE                                                      
154000                                                                          
154100         IF MID-BEERS(RAD-IX) = ALL '+'                                   
154200             MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEERS-ATTR(RAD-IX)          
154300         ELSE                                                             
154400            IF MID-BEERS(RAD-IX) = SPACE                                  
154500               MOVE MFS-ALFA-FAELT-RAETT TO                               
154600                                    MOD-BEERS-ATTR(RAD-IX)                
154700            ELSE                                                          
154800               MOVE MFS-ALFA-FAELT-FEL TO                                 
154900                               MOD-BEERS-ATTR(RAD-IX)                     
155000               MOVE NEJ TO INPUT-RETT                                     
155100            END-IF                                                        
155200         END-IF                                                           
155300                                                                          
155400         PERFORM BAA-KOLLA-TILLK-ARTIKEL                                  
155500        END-IF                                                            
155600                                                                          
155700                                                                          
155800        IF WS-KDERS-2 = 4 OR 5 OR 6 OR 8                                  
155900         EVALUATE TRUE                                                    
156000          WHEN MID-IDARTNR-TILLK(RAD-IX) = SPACE                          
156100           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDARTNR-TILLK-ATTR            
156200                                        (RAD-IX)                          
156300           IF MID-BEERS(RAD-IX) = ALL '+'                                 
156400             MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEERS-ATTR(RAD-IX)          
156500           ELSE                                                           
156600              IF MID-BEERS(RAD-IX) = SPACE                                
156700                 MOVE MFS-ALFA-FAELT-RAETT TO                             
156800                                 MOD-BEERS-ATTR(RAD-IX)                   
156900              ELSE                                                        
157000                 MOVE MFS-ALFA-FAELT-FEL TO                               
157100                                 MOD-BEERS-ATTR(RAD-IX)                   
157200                 MOVE NEJ TO INPUT-RETT                                   
157300              END-IF                                                      
157400           END-IF                                                         
157500           IF MID-DIERS-TILLK(RAD-IX) = SPACE OR ZERO                     
157600             MOVE MFS-ALFA-FAELT-RAETT TO MOD-DIERS-TILLK-ATTR            
157700                                          (RAD-IX)                        
157800           ELSE                                                           
157900             MOVE MFS-ALFA-FAELT-FEL                                      
158000                  TO MOD-DIERS-TILLK-ATTR(RAD-IX)                         
158100             MOVE NEJ TO INPUT-RETT                                       
158200           END-IF                                                         
158300          WHEN MID-IDARTNR-TILLK(RAD-IX) = ALL '+'                        
158400             IF MID-BEERS(RAD-IX) = ALL '+'                               
158500                   MOVE MFS-ALFA-FAELT-FEL                                
158600                         TO MOD-BEERS-ATTR(RAD-IX)                        
158700                   MOVE NEJ TO INPUT-RETT                                 
158800             ELSE                                                         
158900                MOVE MFS-ALFA-FAELT-RAETT                                 
159000                      TO MOD-BEERS-ATTR(RAD-IX)                           
159100             END-IF                                                       
159200             IF MID-DIERS-TILLK(RAD-IX) = ALL '+'                         
159300                MOVE MFS-ALFA-FAELT-RAETT TO MOD-DIERS-TILLK-ATTR         
159400                          (RAD-IX)                                        
159500             ELSE                                                         
159600                IF MID-DIERS-TILLK(RAD-IX) = SPACE                        
159700                   MOVE MFS-ALFA-FAELT-RAETT TO                           
159800                       MOD-DIERS-TILLK-ATTR(RAD-IX)                       
159900                ELSE                                                      
160000                   MOVE MFS-ALFA-FAELT-FEL TO                             
160100                           MOD-DIERS-TILLK-ATTR(RAD-IX)                   
160200                   MOVE NEJ TO INPUT-RETT                                 
160300                END-IF                                                    
160400             END-IF                                                       
160500          WHEN MID-IDARTNR-TILLK(RAD-IX) NUMERIC                          
160600           MOVE MFS-ALFA-FAELT-RAETT TO MOD-IDARTNR-TILLK-ATTR            
160700                                        (RAD-IX)                          
160800           PERFORM BAB-KOLLA-DIERS-TILLK                                  
160900           IF DEC-KDSVAR-OK                                               
161000             IF DEC-IDEDITDATA = ZERO                                     
161100                MOVE MFS-ALFA-FAELT-FEL TO MOD-DIERS-TILLK-ATTR           
161200                                           (RAD-IX)                       
161300                MOVE NEJ TO INPUT-RETT                                    
161400             ELSE                                                         
161500                MOVE MFS-ALFA-FAELT-RAETT TO MOD-DIERS-TILLK-ATTR         
161600                                             (RAD-IX)                     
161700             END-IF                                                       
161800          ELSE                                                            
161900              MOVE MFS-ALFA-FAELT-FEL TO                                  
162000                    MOD-DIERS-TILLK-ATTR(RAD-IX)                          
162100              MOVE NEJ TO INPUT-RETT                                      
162200          END-IF                                                          
162300                                                                          
162400          IF MID-BEERS(RAD-IX) =  ALL '+'                                 
162500              MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEERS-ATTR(RAD-IX)         
162600          ELSE                                                            
162700             IF MID-BEERS(RAD-IX) = SPACE                                 
162800                MOVE MFS-ALFA-FAELT-RAETT TO                              
162900                                 MOD-BEERS-ATTR(RAD-IX)                   
163000             ELSE                                                         
163100                MOVE MFS-ALFA-FAELT-FEL TO                                
163200                              MOD-BEERS-ATTR(RAD-IX)                      
163300                MOVE NEJ TO INPUT-RETT                                    
163400             END-IF                                                       
163500          END-IF                                                          
163600                                                                          
163700         WHEN MID-IDARTNR-TILLK(RAD-IX) NOT NUMERIC                       
163800           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-TILLK-ATTR              
163900                                        (RAD-IX)                          
164000           MOVE NEJ TO INPUT-RETT                                         
164100           IF MID-BEERS(RAD-IX) = ALL '+'                                 
164200              MOVE MFS-ALFA-FAELT-RAETT TO MOD-BEERS-ATTR(RAD-IX)         
164300           ELSE                                                           
164400              IF MID-BEERS(RAD-IX) = SPACE                                
164500                 MOVE MFS-ALFA-FAELT-RAETT TO                             
164600                                MOD-BEERS-ATTR(RAD-IX)                    
164700              ELSE                                                        
164800                 MOVE MFS-ALFA-FAELT-FEL TO                               
164900                                MOD-BEERS-ATTR(RAD-IX)                    
165000                 MOVE NEJ TO INPUT-RETT                                   
165100              END-IF                                                      
165200           END-IF                                                         
165300           IF MID-DIERS-TILLK(RAD-IX) = ALL '+'                           
165400             MOVE MFS-ALFA-FAELT-FEL                                      
165500                  TO MOD-DIERS-TILLK-ATTR(RAD-IX)                         
165600             MOVE NEJ TO INPUT-RETT                                       
165700           ELSE                                                           
165800             PERFORM BAB-KOLLA-DIERS-TILLK                                
165900             IF DEC-KDSVAR-OK                                             
166000               IF DEC-IDEDITDATA = ZERO                                   
166100                  MOVE MFS-ALFA-FAELT-FEL TO MOD-DIERS-TILLK-ATTR         
166200                                             (RAD-IX)                     
166300                  MOVE NEJ TO INPUT-RETT                                  
166400               ELSE                                                       
166500                  MOVE MFS-ALFA-FAELT-RAETT                               
166600                       TO MOD-DIERS-TILLK-ATTR(RAD-IX)                    
166700               END-IF                                                     
166800             ELSE                                                         
166900               MOVE MFS-ALFA-FAELT-FEL TO                                 
167000                     MOD-DIERS-TILLK-ATTR(RAD-IX)                         
167100               MOVE NEJ TO INPUT-RETT                                     
167200             END-IF                                                       
167300         END-IF                                                           
167400        END-EVALUATE                                                      
167500        PERFORM BAA-KOLLA-TILLK-ARTIKEL                                   
167600      END-IF                                                              
167700                                                                          
167800        IF WS-KDERS = '09' OR '29' OR '52'                                
167900          MOVE MFS-ALFA-FAELT-FEL TO MOD-BEERS-ATTR(RAD-IX)               
168000                                     MOD-IDARTNR-TILLK-ATTR               
168100                                         (RAD-IX)                         
168200                                     MOD-DIERS-TILLK-ATTR                 
168300                                         (RAD-IX)                         
168400          MOVE NEJ TO INPUT-RETT                                          
168500          MOVE MED-7(SPRAK-IX) TO MOD-TEMFSINF                            
168600        END-IF                                                            
168700       END-IF                                                             
168800       ADD +1 TO RAD-IX                                                   
168900     END-PERFORM                                                          
169000     .                                                                    
169100     EJECT                                                                
169200 BAA-KOLLA-TILLK-ARTIKEL SECTION.                                         
169300                                                                          
169400     IF MID-IDARTNR-TILLK(RAD-IX) NUMERIC                                 
169500      IF MID-IDARTNR-TILLK(RAD-IX) = IDARTNR-WS                           
169600         MOVE NEJ TO INPUT-RETT                                           
169700         MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-TILLK-ATTR                
169800         (RAD-IX)                                                         
169900      END-IF                                                              
170000      MOVE MID-IDARTNR-TILLK(RAD-IX) TO WS-IDARTNR-TILLK                  
170100                                                                          
170200      MOVE IDARTNR-TILLK-WS TO W-IDARTNR                                  
170300      PERFORM IMS-GET-ARTC01                                              
170400      IF SEGMENT-FINNS                                                    
170500         MOVE ART-TIFINLV  TO WS-TIFINLV-KOLL                             
170600         MOVE ART-KDPRODSL TO TEST-KDPRODSL                               
170700         IF ART-KDERS-UTG > ZERO                                          
170800           MOVE NEJ TO INPUT-RETT                                         
170900           MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-TILLK-ATTR              
171000                                        (RAD-IX)                          
171100           MOVE MED-17(SPRAK-IX) TO MOD-TEMFSINF                          
171200         ELSE                                                             
171300           IF KDPRODSL-LOCAL                                              
171400              MOVE ERSATT-KDPRODSL TO TEST-KDPRODSL                       
171500              IF KDPRODSL-LOCAL                                           
171600                 PERFORM IMS-GET-ARTC11                                   
171700                 IF SEGMENT-FINNS                                         
171800                    IF CLAG-KDERS > ZERO                                  
171900                       MOVE MFS-ALFA-FAELT-FEL TO                         
172000                                   MOD-IDARTNR-TILLK-ATTR(RAD-IX)         
172100                       MOVE MED-13(SPRAK-IX) TO MOD-TEMFSINF              
172200                       MOVE NEJ TO INPUT-RETT                             
172300                    END-IF                                                
172400                 END-IF                                                   
172500              ELSE                                                        
172600                 MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-TILLK-ATTR        
172700                                            (RAD-IX)                      
172800                 MOVE NEJ TO INPUT-RETT                                   
172900              END-IF                                                      
173000           ELSE                                                           
173100              PERFORM IMS-GET-ARTC11                                      
173200              IF SEGMENT-FINNS                                            
173300                 IF CLAG-KDERS > ZERO                                     
173400*********** 971212 FIX                                                    
173500*                   IF W-IDARTNR = 30857249 OR 30857250                   
173600*                      CONTINUE                                           
173700*                   ELSE                                                  
173800*********** 971212 FIX                                                    
173900                       MOVE MFS-ALFA-FAELT-FEL TO                         
174000                                   MOD-IDARTNR-TILLK-ATTR(RAD-IX)         
174100                       MOVE MED-13(SPRAK-IX) TO MOD-TEMFSINF              
174200                       MOVE NEJ TO INPUT-RETT                             
174300*                   END-IF                                                
174400                 END-IF                                                   
174500               END-IF                                                     
174600            END-IF                                                        
174700         END-IF                                                           
174800                                                                          
174900         IF ERSATT-KDERS-C1 = 25 AND WS-KDERS = 05                        
175000            PERFORM BAAA-KOLLA-TIFINLV                                    
175100            IF TIFINLV-OK                                                 
175200               CONTINUE                                                   
175300            ELSE                                                          
175400               MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-TILLK-ATTR          
175500                                         (RAD-IX)                         
175600               MOVE MED-21(SPRAK-IX) TO MOD-TEMFSINF                      
175700               MOVE NEJ TO INPUT-RETT                                     
175800            END-IF                                                        
175900         ELSE                                                             
176000            IF ERSATT-KDERS-C1 = 22 AND WS-KDERS = 02                     
176100               PERFORM BAAA-KOLLA-TIFINLV                                 
176200               IF TIFINLV-OK                                              
176300                  CONTINUE                                                
176400               ELSE                                                       
176500                  MOVE MFS-ALFA-FAELT-FEL TO                              
176600                          MOD-IDARTNR-TILLK-ATTR(RAD-IX)                  
176700                  MOVE MED-21(SPRAK-IX) TO MOD-TEMFSINF                   
176800                  MOVE NEJ TO INPUT-RETT                                  
176900               END-IF                                                     
177000            END-IF                                                        
177100         END-IF                                                           
177200                                                                          
177300      ELSE                                                                
177400          MOVE MFS-ALFA-FAELT-FEL TO MOD-IDARTNR-TILLK-ATTR               
177500                                     (RAD-IX)                             
177600          MOVE NEJ TO INPUT-RETT                                          
177700          MOVE MED-6(SPRAK-IX) TO MOD-TEMFSINF                            
177800      END-IF                                                              
177900     END-IF                                                               
178000     .                                                                    
178100     EJECT                                                                
178200 BAAA-KOLLA-TIFINLV SECTION.                                              
178300     SKIP2                                                                
178400     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
178500     CALL WDATKONV USING DAT-KDDATFORM                                    
178600                         DAT-I-TIDATUM                                    
178700                         DAT-O-TIDATUM                                    
178800                         DAT-KDSVAR                                       
178900     IF DAT-KDSVAR-OK                                                     
179000        MOVE DAT-TIAAVVD TO WS-TIFINLV                                    
179100        MOVE WS-TIFINLV-AAVV   TO TMP1-YYWW                               
179200        MOVE WS-AAVV           TO TMP2-YYWW                               
179300        PERFORM WY2000P3                                                  
179400        IF TMP1-YYWW  > TMP2-YYWW                                         
179500           MOVE JA TO WS-TIFINLV-SW                                       
179600        ELSE                                                              
179700           MOVE NEJ TO WS-TIFINLV-SW                                      
179800        END-IF                                                            
179900     ELSE                                                                 
180000        MOVE NEJ TO WS-TIFINLV-SW                                         
180100     END-IF                                                               
180200     .                                                                    
180300     EJECT                                                                
180400 BAB-KOLLA-DIERS-TILLK SECTION.                                           
180500     SKIP2                                                                
180600     MOVE MID-DIERS-TILLK(RAD-IX)  TO DEC-IDFRIDATA                       
180700     MOVE 3                        TO DEC-KVHELTAL                        
180800     MOVE 3                        TO DEC-KVDECIMAL                       
180900     CALL WDECEDIT USING DEC-WDECAREA                                     
181000     .                                                                    
181100     EJECT                                                                
181200 BB-KOLLA-PREL-ERSKOD SECTION.                                            
181300     SKIP2                                                                
181400     IF ERSATT-KDERS-C1 = 11 OR 14 OR 17 OR 18 OR 19                      
181500        MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                         
181600        MOVE NEJ TO INPUT-RETT                                            
181700        MOVE MED-23(SPRAK-IX) TO MOD-TEMFSINF                             
181800     END-IF                                                               
181900     .                                                                    
182000     EJECT                                                                
182100 BC-KOLLA-TIERSDAT-PREL SECTION.                                          
182200     SKIP2                                                                
182300     IF ERSATT-KDERS-C1 = 03 OR 06                                        
182400       MOVE MID-TIERSDAT-PREL TO XX-TIERSDAT-PREL                         
182500       MOVE '1' TO XX-DAG                                                 
182600       MOVE XX-TIERSDAT-PREL TO MID-TIERSDAT-PREL                         
182700       IF MID-TIERSDAT-PREL NUMERIC                                       
182800         MOVE MID-TIERSDAT-PREL TO WS-TIERSDAT-PREL                       
182900         IF WS-VECKA-PREL > 00 AND < 54                                   
183000           MOVE MFS-ALFA-FAELT-RAETT TO MOD-TIERSDAT-PREL-ATTR            
183100         ELSE                                                             
183200           MOVE MFS-ALFA-FAELT-FEL TO MOD-TIERSDAT-PREL-ATTR              
183300           MOVE NEJ TO INPUT-RETT                                         
183400         END-IF                                                           
183500       ELSE                                                               
183600         MOVE MFS-ALFA-FAELT-FEL TO MOD-TIERSDAT-PREL-ATTR                
183700         MOVE NEJ TO INPUT-RETT                                           
183800       END-IF                                                             
183900     ELSE                                                                 
184000       MOVE MFS-ALFA-FAELT-FEL TO MOD-TIERSDAT-PREL-ATTR                  
184100       MOVE NEJ TO INPUT-RETT                                             
184200     END-IF                                                               
184300     IF INPUT-RETT = JA                                                   
184400         MOVE JA TO UPPDAT-TIERSDAT-TEARTNOT                              
184500     END-IF                                                               
184600     .                                                                    
184700     EJECT                                                                
184800 C-KOLLA-ACTION-FILE SECTION.                                             
184900     SKIP2                                                                
185000     MOVE NEJ TO FINNS-PA-ACTION-FILE                                     
185100     MOVE JA TO UPPDATERING-TILLATEN                                      
185200                                                                          
185300     MOVE IDARTNR-WS TO W-1111-IDARTNR                                    
185400                        W-1112-IDARTNR                                    
185500     PERFORM IMS-GET-XXAN01                                               
185600     IF SEGMENT-FINNS                                                     
185700        PERFORM IMS-GET-XXAN11                                            
185800        IF XXAN-1112-TIUPPDAT = DAGENS-DATUM                              
185900           IF XXAN-1112-IDUSER = MSG-SIGNON-USERID                        
186000               MOVE JA TO FINNS-PA-ACTION-FILE                            
186100                          UPPDATERING-TILLATEN                            
186200           ELSE                                                           
186300               MOVE NEJ TO UPPDATERING-TILLATEN                           
186400           END-IF                                                         
186500        ELSE                                                              
186600          MOVE JA TO UPPDATERING-TILLATEN                                 
186700          PERFORM IMS-GET-XXAN01                                          
186800          PERFORM IMS-DLET-XXAN                                           
186900        END-IF                                                            
187000     END-IF                                                               
187100     .                                                                    
187200     EJECT                                                                
187300 D-RIVNING-AV-ERSAETTNING SECTION.                                        
187400     SKIP2                                                                
187500     MOVE IDARTNR-WS TO W-IDARTNR                                         
187600     PERFORM IMS-GET-ERSA01                                               
187700     IF SEGMENT-FINNS                                                     
187800        PERFORM S06-RIV-TILLK-ART                                         
187900        MOVE IDARTNR-WS TO W-IDARTNR                                      
188000        PERFORM IMS-GET-ERSA01                                            
188100        IF SEGMENT-FINNS                                                  
188200           PERFORM IMS-DLET-ERSA                                          
188300        END-IF                                                            
188400        PERFORM DA-UPPDATERA-REGISTER                                     
188500     ELSE                                                                 
188600        IF ERSATT-KDERS-C1 = 29 OR 52                                     
188700           PERFORM DA-UPPDATERA-REGISTER                                  
188800        END-IF                                                            
188900     END-IF                                                               
189000     .                                                                    
189100     EJECT                                                                
189200 DA-UPPDATERA-REGISTER SECTION.                                           
189300     SKIP2                                                                
189400     MOVE IDARTNR-WS TO W-1111-IDARTNR                                    
189500     PERFORM IMS-GET-XXAN01                                               
189600     IF SEGMENT-FINNS                                                     
189700        PERFORM IMS-DLET-XXAN                                             
189800     END-IF                                                               
189900                                                                          
190000     IF (ERSATT-FLIART = JA )                                             
190100     OR (ERSATT-IDLEVNR  = '1002 ')                                       
190200        PERFORM S12-SKAPA-SATSTRANS                                       
190300     END-IF                                                               
190400                                                                          
190500     MOVE IDARTNR-WS TO W-IDARTNR                                         
190600     PERFORM IMS-GET-ARTC01                                               
190700     MOVE ART-KDPRODSL TO WS-KDPRODSL                                     
190800                                                                          
190900     IF (ERSATT-KDERS-C1 > 20)                                            
191000        MOVE ZERO TO ART-TIERSDAT                                         
191100        PERFORM IMS-REPL-ARTC                                             
191200     END-IF                                                               
191300                                                                          
191400     PERFORM IMS-GET-ARTC11                                               
191500     IF  CLAG-KDERS        > ZERO                                         
191600     AND CLAG-KDERS    NOT = 09                                           
191700        MOVE CLAG-IDANSK  TO WS-IDANSK-ALARM                              
191800        MOVE WC-CDC-SE    TO WS-IDDC-ALARM                                
191900        SET ALARM-SSCODE-JA                                               
192000                          TO TRUE                                         
192100     END-IF                                                               
192200     MOVE ZERO TO CLAG-KDERS                                              
192300     IF ERSATT-KDKSP = +4 OR +1                                           
192400        MOVE ZERO TO CLAG-KDKSP                                           
192500     END-IF                                                               
192600     PERFORM IMS-REPL-ARTC                                                
192700     IF ALARM-SSCODE-JA                                                   
192800        PERFORM S29-ALARM-KDERS-UPD                                       
192900     END-IF                                                               
193000                                                                          
193100     PERFORM S16-SKAPA-VR-TRANS                                           
193200                                                                          
193300     MOVE 'R'             TO KDP-KDUART                                   
193400     MOVE IDARTNR-WS      TO W-IDARTNR                                    
193500     PERFORM S21-SKAPA-KDP-TRANS                                          
193600     PERFORM S23-TRANS-TIKO-RSLISTA                                       
193700     PERFORM S24-TRANS-TILL-BASL                                          
193800     PERFORM S25-TRANS-TILL-PPMS                                          
193900     MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF                                
194000     MOVE '101'            TO MSG-KOM-IDMFSMED                            
194100     .                                                                    
194200     EJECT                                                                
194300 F-KONTROLL-REG-UPPDAT-AV-ERS SECTION.                                    
194400     SKIP2                                                                
194500     IF  ERSATT-KDERS-C1 = 00                                             
194600        IF WS-KDERS-1 = 0                                                 
194700        OR WS-KDERS   = 52                                                
194800           IF MID-FLKLAR = NEJ                                            
194900              IF FINNS-PA-ACTION-FILE = NEJ                               
195000                 MOVE IDARTNR-WS TO W-1111-IDARTNR                        
195100                                    W-1112-IDARTNR                        
195200                                    W-IDARTNR                             
195300                                                                          
195400                 MOVE IDARTNR-WS     TO XXAN-1111-IDARTNR                 
195500                 MOVE '1111'         TO XXAN-1111-IDHTYP                  
195600                 MOVE LOW-VALUE      TO XXAN-1111-LOWVALUE                
195700                 PERFORM IMS-ISRT-XXAN01                                  
195800              END-IF                                                      
195900                                                                          
196000              PERFORM IMS-GU-XXAN11                                       
196100              MOVE IDARTNR-WS     TO XXAN-1112-IDARTNR                    
196200              MOVE LOW-VALUE      TO XXAN-1112-LOWVALUE                   
196300              MOVE MSG-SIGNON-USERID  TO XXAN-1112-IDUSER                 
196400              MOVE DAGENS-DATUM       TO XXAN-1112-TIUPPDAT               
196500              IF SEGMENT-FINNS                                            
196600                 PERFORM IMS-REPL-XXAN                                    
196700              ELSE                                                        
196800                 PERFORM IMS-ISRT-XXAN11                                  
196900              END-IF                                                      
197000              PERFORM S03-UPPDATERA-RADER                                 
197100           ELSE                                                           
197200              PERFORM FA-REGISTRERA-ERSAETTNING                           
197300              IF ERSATT-FLIART = JA                                       
197400                IF WS-KDERS-2 = 4 OR 5 OR 6 OR 8                          
197500                    MOVE MED-28(SPRAK-IX) TO MOD-TEMFSFEL                 
197600                END-IF                                                    
197700              END-IF                                                      
197800           END-IF                                                         
197900        ELSE                                                              
198000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                      
198100           MOVE NEJ TO INPUT-RETT                                         
198200           MOVE MED-3(SPRAK-IX) TO MOD-TEMFSINF                           
198300        END-IF                                                            
198400     ELSE                                                                 
198500        IF ((ERSATT-KDERS-C1 = 04 OR 05 OR 06 OR 08)                      
198600        AND (WS-KDERS        = 04 OR 05 OR 06 OR 08))                     
198700        OR                                                                
198800           ((ERSATT-KDERS-C1 = 01 OR 02 OR 03 OR 07)                      
198900        AND (WS-KDERS        = 01 OR 02 OR 03 OR 07))                     
199000        OR                                                                
199100           ((ERSATT-KDERS-C1 = 24 OR 25 OR 26 OR 28)                      
199200        AND (WS-KDERS        = 24 OR 25 OR 26 OR 28))                     
199300        OR                                                                
199400           ((ERSATT-KDERS-C1 = 21 OR 22 OR 23 OR 27)                      
199500        AND (WS-KDERS        = 21 OR 22 OR 23 OR 27))                     
199600***** 021218                                                              
199700*****   OR                                                                
199800*****      (ERSATT-KDERS-C1 = 24 AND WS-KDERS = 29)                       
199900***** 021218                                                              
200000           IF MID-FLKLAR = NEJ                                            
200100              PERFORM S17-UPPDAT-TILLK-ART-ACTION                         
200200              MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF                       
200300           ELSE                                                           
200400              PERFORM FB-UPPDATERA-ERSAETTNING                            
200500           END-IF                                                         
200600        ELSE                                                              
200700           IF (ERSATT-KDERS-C1 = 21 OR 22 OR 23 OR 24 OR 25               
200800                              OR 26 OR 27 OR 28)                          
200900           AND (WS-KDERS       = 01 OR 02 OR 03 OR 04 OR 05               
201000                              OR 06 OR 07 OR 08)                          
201100              IF ERSATT-KDERS-C1-2 = WS-KDERS-2                           
201200                 IF MID-FLKLAR = NEJ                                      
201300                    PERFORM S17-UPPDAT-TILLK-ART-ACTION                   
201400                    MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF                 
201500                 ELSE                                                     
201600                    PERFORM FB-UPPDATERA-ERSAETTNING                      
201700                 END-IF                                                   
201800              ELSE                                                        
201900                 MOVE MED-20(SPRAK-IX) TO MOD-TEMFSINF                    
202000                 MOVE NEJ TO INPUT-RETT                                   
202100              END-IF                                                      
202200           ELSE                                                           
202300              IF ((ERSATT-KDERS-C1 = 01 OR 02 OR 03 OR 07)                
202400              AND (WS-KDERS    = 27))                                     
202500              OR                                                          
202600                 ((ERSATT-KDERS-C1 = 04 OR 05 OR 06 OR 08)                
202700              AND (WS-KDERS    = 28))                                     
202800                 IF MID-FLKLAR = NEJ                                      
202900                    PERFORM S17-UPPDAT-TILLK-ART-ACTION                   
203000                    MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF                 
203100                 ELSE                                                     
203200                    PERFORM FB-UPPDATERA-ERSAETTNING                      
203300                 END-IF                                                   
203400              ELSE                                                        
203610                 IF (ERSATT-KDERS-C1 = 11 OR 14 OR 17 OR 18 OR 19)        
203620                 AND (WS-KDERS       = 01 OR 04 OR 07 OR 08 OR 09)        
203700                    IF ERSATT-KDERS-C1-2 = WS-KDERS-2                     
203800                       IF MID-FLKLAR = NEJ                                
203900                         PERFORM S17-UPPDAT-TILLK-ART-ACTION              
204000                         MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF            
204100                       ELSE                                               
204200                          PERFORM FB-UPPDATERA-ERSAETTNING                
204300                       END-IF                                             
204400                    ELSE                                                  
204500                       MOVE MED-20(SPRAK-IX) TO MOD-TEMFSINF              
204600                       MOVE NEJ TO INPUT-RETT                             
204700                    END-IF                                                
204800                 ELSE                                                     
204900                    MOVE MED-20(SPRAK-IX) TO MOD-TEMFSINF                 
205000                    MOVE NEJ TO INPUT-RETT                                
205100                 END-IF                                                   
205200              END-IF                                                      
205300           END-IF                                                         
205400        END-IF                                                            
205500     END-IF                                                               
205600     .                                                                    
205700     EJECT                                                                
205800 FA-REGISTRERA-ERSAETTNING SECTION.                                       
205900     SKIP2                                                                
206000     MOVE JA TO INPUT-RETT                                                
206100                                                                          
206200     MOVE +1 TO TAB-IX                                                    
206300     PERFORM UNTIL TAB-IX > MAX-TAB                                       
206400        MOVE ZERO  TO TAB-IDRADNR      (TAB-IX)                           
206500                      TAB-IDARTNR-TILLK(TAB-IX)                           
206600                      TAB-DIERS-TILLK  (TAB-IX)                           
206700        MOVE SPACE TO TAB-BEERS        (TAB-IX)                           
206800        ADD +1 TO TAB-IX                                                  
206900     END-PERFORM                                                          
207000                                                                          
207100     IF FINNS-PA-ACTION-FILE = JA                                         
207200        PERFORM S03-UPPDATERA-RADER                                       
207300        PERFORM S01-LAS-ACTIONFILE-TILL-TAB                               
207400     ELSE                                                                 
207500        PERFORM S05-UPPDATERA-TABELL                                      
207600     END-IF                                                               
207700                                                                          
207800     PERFORM S18-RAKNA-KVKORT                                             
207900     IF WS-RAKNARE = ZERO                                                 
208000        IF MID-KDERS = '09' OR '29' OR '52'                               
208100           CONTINUE                                                       
208200        ELSE                                                              
208300           MOVE NEJ TO INPUT-RETT                                         
208400           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                      
208500        END-IF                                                            
208600     END-IF                                                               
208700     IF WS-KDERS-2 = 4 OR 5 OR 6 OR 8                                     
208800        IF TEXT-FINNS = NEJ                                               
208900           MOVE NEJ TO INPUT-RETT                                         
209000           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                      
209100           MOVE MED-10(SPRAK-IX) TO MOD-TEMFSINF                          
209200        END-IF                                                            
209300     END-IF                                                               
209400                                                                          
209500     IF INPUT-RETT = JA                                                   
209600                                                                          
209700       MOVE IDARTNR-WS TO W-1111-IDARTNR                                  
209800       PERFORM IMS-GET-XXAN01                                             
209900       IF SEGMENT-FINNS                                                   
210000          PERFORM IMS-DLET-XXAN                                           
210100       END-IF                                                             
210200                                                                          
210300       IF MID-IDAO = ALL '+' OR SPACE                                     
210400          CONTINUE                                                        
210500       ELSE                                                               
210600          PERFORM S10-LAGG-UPP-IDAO                                       
210700       END-IF                                                             
210800                                                                          
210900       IF ERSATT-KDERS-UTG = 00                                           
211000                                                                          
211100          IF (ERSATT-FLIART = JA)                                         
211200          OR (ERSATT-IDLEVNR = '1002 ')                                   
211300               PERFORM S12-SKAPA-SATSTRANS                                
211400          END-IF                                                          
211500                                                                          
211600          MOVE WS-KDERS  TO NY-KDERS-C1                                   
211700                                                                          
211800          MOVE ZERO TO WS-KVBR                                            
211900          MOVE ZERO TO WS-KVBR-TOT                                        
212000          MOVE IDARTNR-WS TO W-IDARTNR-D9                                 
212100          MOVE WS-IDDC    TO W-IDDC-D9                                    
212200          PERFORM IMS-GET-INLB01                                          
212300          IF SEGMENT-FINNS                                                
212400             PERFORM IMS-GET-INLB11                                       
212500             PERFORM UNTIL SEGMENT-SAKNAS                                 
212600               MOVE INLB11-KVBR TO WS-KVBR                                
212700               COMPUTE WS-KVBR-TOT = WS-KVBR-TOT + WS-KVBR                
212800               PERFORM IMS-GET-INLB11-NAESTA                              
212900             END-PERFORM                                                  
213000             MOVE WS-KVBR-TOT TO INLB11-KVBR                              
213100          ELSE                                                            
213200             MOVE ZERO TO INLB11-KVBR                                     
213300          END-IF                                                          
213400                                                                          
213500          IF ERSATT-KDERS-C1 = 00                                         
213600             IF INLB11-KVBR = ZERO                                        
213700                CONTINUE                                                  
213800             ELSE                                                         
213900                PERFORM S15-SKAPA-LEV-PLANTRANS                           
214000             END-IF                                                       
214100          END-IF                                                          
214200                                                                          
214300          MOVE IDARTNR-WS TO W-IDARTNR                                    
214400          PERFORM IMS-GET-ARTC01                                          
214500          IF WS-KDERS > 20                                                
214600             MOVE 'IDAG  ' TO DAT-KDDATFORM                               
214700             CALL WDATKONV USING DAT-KDDATFORM                            
214800                                 DAT-I-TIDATUM                            
214900                                 DAT-O-TIDATUM                            
215000                                 DAT-KDSVAR                               
215100             MOVE DAT-TIAAVVD    TO ART-TIERSDAT                          
215200          ELSE                                                            
215300             MOVE ZERO           TO ART-TIERSDAT                          
215400          END-IF                                                          
215500          PERFORM IMS-REPL-ARTC                                           
215600                                                                          
215700          PERFORM IMS-GET-ARTC11                                          
215800          IF  (NY-KDERS-C1 > 0 AND < +10)                                 
215900          AND (ERSATT-KDKSP = +0)                                         
216000              MOVE +4 TO CLAG-KDKSP                                       
216100          ELSE                                                            
216200              IF (NY-KDERS-C1 > +19)                                      
216300              AND (ERSATT-KDKSP = +0 OR +4)                               
216400                 MOVE +1 TO CLAG-KDKSP                                    
216500              ELSE                                                        
216600                 IF (ERSATT-KDERS-C1 > +9)                                
216700                 AND (NY-KDERS-C1 > +0 AND < +10)                         
216800                 AND (ERSATT-KDKSP = +1)                                  
216900                    MOVE +4 TO CLAG-KDKSP                                 
217000                 END-IF                                                   
217100              END-IF                                                      
217200          END-IF                                                          
217300                                                                          
217400          MOVE NY-KDERS-C1 TO CLAG-KDERS                                  
217500          PERFORM IMS-REPL-ARTC                                           
217600                                                                          
217700       ELSE                                                               
217800                                                                          
217900          MOVE IDARTNR-WS TO W-IDARTNR                                    
218000          PERFORM IMS-GET-ARTC01                                          
218100          MOVE WS-KDERS TO ART-KDERS-UTG                                  
218200          PERFORM IMS-REPL-ARTC                                           
218300                                                                          
218400       END-IF                                                             
218500       MOVE SPACE TO WS-TEARTNOT                                          
218600       PERFORM S07-SKAPA-ERSATT-ART-TO-ERSREG                             
218700       IF WS-KDERS = '09' OR '29' OR '52'                                 
218800          MOVE +1 TO ERSA01-KVKORT                                        
218900       ELSE                                                               
219000          MOVE WS-RAKNARE TO ERSA01-KVKORT                                
219100       END-IF                                                             
219200       PERFORM IMS-ISRT-ERSA01                                            
219300       PERFORM S08-SKAPA-TILLK-ART-TO-ERSREG                              
219400       PERFORM S19-SKAPA-BEVAKNINGS-SEGMENT                               
219500       MOVE IDARTNR-WS TO W-IDARTNR                                       
219600       PERFORM IMS-ISRT-ERSA13                                            
219700       IF WS-KDERS = '09' OR '19' OR '29' OR '52'                         
219800          MOVE 'U' TO KDP-KDUART                                          
219900       ELSE                                                               
220000          MOVE 'E' TO KDP-KDUART                                          
220100       END-IF                                                             
220200       MOVE IDARTNR-WS TO W-IDARTNR                                       
220300       PERFORM IMS-GET-ARTC01                                             
220400       MOVE ART-KDPRODSL TO WS-KDPRODSL                                   
220500       PERFORM S21-SKAPA-KDP-TRANS                                        
220600       PERFORM S23-TRANS-TIKO-RSLISTA                                     
220700       MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF                              
220800       MOVE '101'            TO MSG-KOM-IDMFSMED                          
220900     END-IF                                                               
221000     .                                                                    
221100     EJECT                                                                
221200 FB-UPPDATERA-ERSAETTNING SECTION.                                        
221300     SKIP2                                                                
221400     MOVE JA TO INPUT-RETT                                                
221500                                                                          
221600     MOVE +1 TO TAB-IX                                                    
221700     PERFORM UNTIL TAB-IX > MAX-TAB                                       
221800        MOVE ZERO  TO TAB-IDRADNR      (TAB-IX)                           
221900                      TAB-IDARTNR-TILLK(TAB-IX)                           
222000                      TAB-DIERS-TILLK  (TAB-IX)                           
222100        MOVE SPACE TO TAB-BEERS        (TAB-IX)                           
222200        ADD +1 TO TAB-IX                                                  
222300     END-PERFORM                                                          
222400                                                                          
222500     IF FINNS-PA-ACTION-FILE = JA                                         
222600        PERFORM S03-UPPDATERA-RADER                                       
222700        PERFORM S01-LAS-ACTIONFILE-TILL-TAB                               
222800     ELSE                                                                 
222900        PERFORM S04-LAS-ERSREG-TILL-TABELL                                
223000        PERFORM S05-UPPDATERA-TABELL                                      
223100     END-IF                                                               
223200************021218                                                        
223300**** IF (ERSATT-KDERS-C1 = 24 AND WS-KDERS = 29)                          
223400****     MOVE JA TO RADER-UPPDATERADE                                     
223500****       MOVE +1 TO TAB-IX                                              
223600****       PERFORM UNTIL TAB-IX > MAX-TAB                                 
223700****          MOVE ZERO  TO TAB-IDRADNR      (TAB-IX)                     
223800****                        TAB-IDARTNR-TILLK(TAB-IX)                     
223900****                        TAB-DIERS-TILLK  (TAB-IX)                     
224000****          MOVE SPACE TO TAB-BEERS        (TAB-IX)                     
224100****          ADD +1 TO TAB-IX                                            
224200****       END-PERFORM                                                    
224300**** END-IF                                                               
224400************021218                                                        
224500                                                                          
224600     IF RADER-UPPDATERADE = NEJ AND FINNS-PA-ACTION-FILE = NEJ            
224700        CONTINUE                                                          
224800     ELSE                                                                 
224900        PERFORM S18-RAKNA-KVKORT                                          
225000        IF WS-RAKNARE = ZERO                                              
225100           IF MID-KDERS = '09' OR '29' OR '52'                            
225200              CONTINUE                                                    
225300           ELSE                                                           
225400              MOVE NEJ TO INPUT-RETT                                      
225500              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                   
225600           END-IF                                                         
225700        END-IF                                                            
225800        IF WS-KDERS-2 = 4 OR 5 OR 6 OR 8                                  
225900           IF TEXT-FINNS = NEJ                                            
226000              MOVE NEJ TO INPUT-RETT                                      
226100              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                   
226200              MOVE MED-10(SPRAK-IX) TO MOD-TEMFSINF                       
226300           END-IF                                                         
226400        END-IF                                                            
226500     END-IF                                                               
226600                                                                          
226700     IF INPUT-RETT = JA                                                   
226800       MOVE IDARTNR-WS TO W-1111-IDARTNR                                  
226900       PERFORM IMS-GET-XXAN01                                             
227000       IF SEGMENT-FINNS                                                   
227100          PERFORM IMS-DLET-XXAN                                           
227200       END-IF                                                             
227300                                                                          
227400       IF MID-IDAO = ALL '+' OR SPACE                                     
227500          CONTINUE                                                        
227600       ELSE                                                               
227700          PERFORM S10-LAGG-UPP-IDAO                                       
227800       END-IF                                                             
227900                                                                          
228000       IF ERSATT-KDERS-UTG = 0                                            
228100                                                                          
228200          IF (ERSATT-FLIART = JA)                                         
228300          OR (ERSATT-IDLEVNR = '1002 ')                                   
228400             PERFORM S12-SKAPA-SATSTRANS                                  
228500          END-IF                                                          
228600                                                                          
228700          MOVE WS-KDERS TO NY-KDERS-C1                                    
228800                                                                          
228900          MOVE IDARTNR-WS TO W-IDARTNR                                    
229000          PERFORM IMS-GET-ARTC01                                          
229100          IF WS-KDERS > 20                                                
229200             MOVE 'IDAG  ' TO DAT-KDDATFORM                               
229300             CALL WDATKONV USING DAT-KDDATFORM                            
229400                                 DAT-I-TIDATUM                            
229500                                 DAT-O-TIDATUM                            
229600                                 DAT-KDSVAR                               
229700             MOVE DAT-TIAAVVD    TO ART-TIERSDAT                          
229800          ELSE                                                            
229900             MOVE ZERO           TO ART-TIERSDAT                          
230000          END-IF                                                          
230100          PERFORM IMS-REPL-ARTC                                           
230200          PERFORM IMS-GET-ARTC11                                          
230300                                                                          
230400          IF (NY-KDERS-C1 > 0 AND < +10)                                  
230500          AND (ERSATT-KDKSP = +0)                                         
230600             MOVE +4 TO CLAG-KDKSP                                        
230700          ELSE                                                            
230800             IF (NY-KDERS-C1 > +19)                                       
230900             AND (ERSATT-KDKSP = +0 OR +4)                                
231000                MOVE +1 TO CLAG-KDKSP                                     
231100             ELSE                                                         
231200                IF (ERSATT-KDERS-C1 > +9)                                 
231300                AND (NY-KDERS-C1 > +0 AND < +10)                          
231400                AND (ERSATT-KDKSP = +1)                                   
231500                   MOVE +4 TO CLAG-KDKSP                                  
231600                END-IF                                                    
231700             END-IF                                                       
231800          END-IF                                                          
231900                                                                          
232000          MOVE NY-KDERS-C1 TO CLAG-KDERS                                  
232100          PERFORM IMS-REPL-ARTC                                           
232200                                                                          
232300       ELSE                                                               
232400                                                                          
232500          MOVE IDARTNR-WS TO W-IDARTNR                                    
232600          PERFORM IMS-GET-ARTC01                                          
232700          MOVE WS-KDERS TO ART-KDERS-UTG                                  
232800          PERFORM IMS-REPL-ARTC                                           
232900                                                                          
233000       END-IF                                                             
233100                                                                          
233200       IF (RADER-UPPDATERADE = JA)                                        
233300       OR (RADER-UPPDATERADE = NEJ                                        
233400                          AND FINNS-PA-ACTION-FILE = JA)                  
233500          PERFORM S06-RIV-TILLK-ART                                       
233600          MOVE IDARTNR-WS TO W-IDARTNR                                    
233700          PERFORM IMS-GET-ERSA01                                          
233800          MOVE ERSA01-TEARTNOT TO WS-TEARTNOT                             
233900          IF SEGMENT-FINNS                                                
234000             PERFORM IMS-DLET-ERSA                                        
234100          END-IF                                                          
234200          PERFORM S07-SKAPA-ERSATT-ART-TO-ERSREG                          
234300          IF WS-KDERS = '09' OR '29' OR '52'                              
234400             MOVE +1 TO ERSA01-KVKORT                                     
234500          ELSE                                                            
234600             MOVE WS-RAKNARE TO ERSA01-KVKORT                             
234700          END-IF                                                          
234800          PERFORM IMS-ISRT-ERSA01                                         
234900          PERFORM S08-SKAPA-TILLK-ART-TO-ERSREG                           
235000          MOVE IDARTNR-WS TO W-IDARTNR                                    
235100          PERFORM S19-SKAPA-BEVAKNINGS-SEGMENT                            
235200          PERFORM IMS-ISRT-ERSA13                                         
235300          MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF                           
235400     ELSE                                                                 
235500        IF RADER-UPPDATERADE = NEJ                                        
235600        AND FINNS-PA-ACTION-FILE = NEJ                                    
235700          PERFORM IMS-GET-ERSA01                                          
235800          MOVE ERSA01-TEARTNOT TO WS-TEARTNOT                             
235900          PERFORM S07-SKAPA-ERSATT-ART-TO-ERSREG                          
236000          PERFORM IMS-REPL-ERSA                                           
236100          MOVE +1 TO TAB-IX                                               
236200          PERFORM UNTIL TAB-IX > MAX-TAB                                  
236300           IF TAB-IDARTNR-TILLK(TAB-IX) > ZERO                            
236400              MOVE TAB-IDARTNR-TILLK(TAB-IX) TO WS-IDARTNR-TILLK          
236500              MOVE IDARTNR-TILLK-WS TO W-IDARTNR                          
236600              PERFORM IMS-GET-ARTC01                                      
236700              MOVE ART-TIFINLV      TO TMP1-YYWWD                         
236800              MOVE WS-TIFINLV-MAX   TO TMP2-YYWWD                         
236900              PERFORM WY2000P2                                            
237000              IF TMP1-YYWWD > TMP2-YYWWD                                  
237100                 MOVE ART-TIFINLV TO WS-TIFINLV-MAX                       
237200              END-IF                                                      
237300           END-IF                                                         
237400           ADD +1 TO TAB-IX                                               
237500          END-PERFORM                                                     
237600          MOVE IDARTNR-WS TO W-IDARTNR                                    
237700          PERFORM IMS-GET-ERSA01                                          
237800          PERFORM IMS-GET-ERSA13                                          
237900          PERFORM S19-SKAPA-BEVAKNINGS-SEGMENT                            
238000          IF SEGMENT-FINNS                                                
238100             PERFORM IMS-REPL-ERSA                                        
238200          ELSE                                                            
238300            PERFORM IMS-ISRT-ERSA13                                       
238400          END-IF                                                          
238500          MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF                           
238600        END-IF                                                            
238700       END-IF                                                             
238800                                                                          
238900       MOVE NEJ TO FL-NYPONART                                            
239000       MOVE IDARTNR-WS TO W-IDARTNR                                       
239100       PERFORM IMS-GET-ARTG01                                             
239200       IF SEGMENT-FINNS                                                   
239300          MOVE JA TO FL-NYPONART                                          
239400       END-IF                                                             
239500                                                                          
239600     END-IF                                                               
239700     .                                                                    
239800     EJECT                                                                
239900 G-LAS SECTION.                                                           
240000     SKIP2                                                                
240100***** I SLUTET AV DENNA SEKTION SÄTTS MOD-TEMFSINF IHOP FRÅN              
240200*****                       WS-TEMFSINF-KAMP   OCH   WS-TEMFSINF          
240300     SKIP2                                                                
240400     MOVE NEJ TO FLFORTS                                                  
240500     MOVE IDARTNR-WS TO W-1111-IDARTNR                                    
240600                        W-1112-IDARTNR                                    
240700                        W-IDARTNR                                         
240800     MOVE ZERO       TO W-1114-IDKORTNR                                   
240900                        W-IDKORTNR                                        
241000                                                                          
241100     IF WS-IDKORTNR-SPAR1 = ZERO AND WS-IDKORTNR-SPAR2 = ZERO             
241200         AND WS-IDKORTNR-SPAR3 = ZERO                                     
241300                                                                          
241400        PERFORM IMS-GET-ARTC01                                            
241500        IF SEGMENT-FINNS                                                  
241600                                                                          
241700*          --- KOLLAR OM REG. ARTIKEL ÄR KAMPANJARTIKEL                   
241800           PERFORM GC-KOLLA-KAMPANJ                                       
241900                                                                          
242000           IF ART-FLIART = JA                                             
242100              MOVE MED-25(SPRAK-IX) TO MOD-TEMFSFEL                       
242200           END-IF                                                         
242300           PERFORM S26-KOLLA-RASA                                         
242400           IF WS-EXTERN-SATS = JA                                         
242500               MOVE MED-26(SPRAK-IX) TO MOD-TEMFSFEL                      
242600           END-IF                                                         
242700           IF ART-FLIART = JA                                             
242800           AND WS-EXTERN-SATS = JA                                        
242900              MOVE MED-29(SPRAK-IX) TO MOD-TEMFSFEL                       
243000           END-IF                                                         
243100           IF ART-FLERS = JA                                              
243200              MOVE MED-2(SPRAK-IX) TO WS-TEMFSINF                         
243300           END-IF                                                         
243400           PERFORM IMS-GET-ARTC11                                         
243500           IF SEGMENT-FINNS                                               
243600              IF CLAG-FLGEMART = JA                                       
243700                 MOVE JA TO WS-FLGEMART                                   
243800                 MOVE MED-30(SPRAK-IX) TO WS-TEMFSINF                     
243900              END-IF                                                      
244000           END-IF                                                         
244100                                                                          
244200           PERFORM IMS-GET-ERSA01                                         
244300           IF SEGMENT-FINNS                                               
244400                                                                          
244500              PERFORM IMS-GET-XXAN01                                      
244600              IF SEGMENT-FINNS                                            
244700                PERFORM IMS-GET-XXAN11                                    
244800                IF XXAN-1112-TIUPPDAT = DAGENS-DATUM                      
244900                  IF XXAN-1112-IDUSER = MSG-SIGNON-USERID                 
245000                     PERFORM GA-LAS-ACTIONFILE                            
245100                  ELSE                                                    
245200                    PERFORM GB-LAS-ERSREG                                 
245300                    MOVE +1 TO RAD-IX                                     
245400                    PERFORM UNTIL RAD-IX > MAX-RAD                        
245500                      MOVE MFS-STAENG-FAELT TO                            
245600                           MOD-IDARTNR-TILLK-ATTR(RAD-IX)                 
245700                           MOD-DIERS-TILLK-ATTR(RAD-IX)                   
245800                           MOD-BEERS-ATTR(RAD-IX)                         
245900                           MOD-IDKORTNR-ATTR(RAD-IX)                      
246000                      ADD +1 TO RAD-IX                                    
246100                    END-PERFORM                                           
246200                    MOVE MFS-STAENG-FAELT TO MOD-DIERS-ERS-ATTR           
246300                                             MOD-IDAO-ATTR                
246400                                             MOD-TEARTNOT-ATTR            
246500                                             MOD-KDERS-ATTR               
246600                    MOVE MFS-STAENG-FAELT TO                              
246700                                     MOD-TIERSDAT-PREL-ATTR               
246800                    MOVE MED-18(SPRAK-IX) TO WS-TEMFSINF                  
246900                  END-IF                                                  
247000                ELSE                                                      
247100                  PERFORM GB-LAS-ERSREG                                   
247200                END-IF                                                    
247300             ELSE                                                         
247400                PERFORM GB-LAS-ERSREG                                     
247500             END-IF                                                       
247600           ELSE                                                           
247700             PERFORM IMS-GET-XXAN01                                       
247800             IF SEGMENT-FINNS                                             
247900                PERFORM IMS-GET-XXAN11                                    
248000                IF XXAN-1112-TIUPPDAT = DAGENS-DATUM                      
248100                  IF XXAN-1112-IDUSER = MSG-SIGNON-USERID                 
248200                     PERFORM GA-LAS-ACTIONFILE                            
248300                  ELSE                                                    
248400                    MOVE MED-18(SPRAK-IX) TO WS-TEMFSINF                  
248500                    MOVE +1 TO RAD-IX                                     
248600                    PERFORM UNTIL RAD-IX > MAX-RAD                        
248700                       MOVE MFS-STAENG-FAELT TO                           
248800                                     MOD-IDKORTNR-ATTR(RAD-IX)            
248900                       MOVE MFS-STAENG-FAELT TO                           
249000                                      MOD-IDARTNR-TILLK-ATTR              
249100                                      (RAD-IX)                            
249200                       MOVE MFS-STAENG-FAELT TO                           
249300                                      MOD-DIERS-TILLK-ATTR                
249400                                      (RAD-IX)                            
249500                       MOVE MFS-STAENG-FAELT TO                           
249600                                      MOD-BEERS-ATTR(RAD-IX)              
249700                       ADD +1 TO RAD-IX                                   
249800                    END-PERFORM                                           
249900                    MOVE MFS-STAENG-FAELT TO MOD-DIERS-ERS-ATTR           
250000                                             MOD-IDAO-ATTR                
250100                                             MOD-TEARTNOT-ATTR            
250200                                             MOD-KDERS-ATTR               
250300                    MOVE MFS-STAENG-FAELT TO                              
250400                                     MOD-TIERSDAT-PREL-ATTR               
250500                  END-IF                                                  
250600              END-IF                                                      
250700             END-IF                                                       
250800           END-IF                                                         
250900         ELSE                                                             
251000            MOVE FEL-2(SPRAK-IX) TO MOD-TEMFSFEL                          
251100         END-IF                                                           
251200     ELSE                                                                 
251300        IF WS-IDKORTNR-SPAR1 > ZERO                                       
251400           MOVE WS-IDKORTNR-SPAR1 TO W-IDKORTNR                           
251500           PERFORM GB-LAS-ERSREG                                          
251600           PERFORM IMS-GET-XXAN01                                         
251700           IF SEGMENT-FINNS                                               
251800              PERFORM IMS-GET-XXAN11                                      
251900              IF XXAN-1112-TIUPPDAT = DAGENS-DATUM                        
252000                  MOVE +1 TO RAD-IX                                       
252100                  PERFORM UNTIL RAD-IX > MAX-RAD                          
252200                    MOVE MFS-STAENG-FAELT TO                              
252300                         MOD-IDARTNR-TILLK-ATTR(RAD-IX)                   
252400                         MOD-DIERS-TILLK-ATTR(RAD-IX)                     
252500                         MOD-BEERS-ATTR(RAD-IX)                           
252600                         MOD-IDKORTNR-ATTR(RAD-IX)                        
252700                    ADD +1 TO RAD-IX                                      
252800                  END-PERFORM                                             
252900                  MOVE MED-18(SPRAK-IX) TO WS-TEMFSINF                    
253000              END-IF                                                      
253100           END-IF                                                         
253200        ELSE                                                              
253300           IF WS-IDKORTNR-SPAR2 > ZERO                                    
253400              MOVE WS-IDKORTNR-SPAR2 TO W-1114-IDKORTNR                   
253500              MOVE JA TO FLFORTS                                          
253600              PERFORM GA-LAS-ACTIONFILE                                   
253700           ELSE                                                           
253800              MOVE MFS-ADD-SAETT-CURSOR TO                                
253900                               MOD-IDARTNR-TILLK-ATTR(1)                  
254000           END-IF                                                         
254100        END-IF                                                            
254200     END-IF                                                               
254300                                                                          
254400*    ---- SÄTT IHOP EV. MEDDELANDEN PÅ RAD 23 ------------                
254500     IF WS-TEMFSINF = SPACE  AND WS-TEMFSINF-KAMP = SPACE                 
254600         CONTINUE                                                         
254700     ELSE                                                                 
254800       IF WS-TEMFSINF = SPACE                                             
254900           MOVE WS-TEMFSINF-KAMP TO MOD-TEMFSINF                          
255000       ELSE                                                               
255100         IF WS-TEMFSINF-KAMP = SPACE                                      
255200             MOVE WS-TEMFSINF TO MOD-TEMFSINF                             
255300         ELSE                                                             
255400*            --- OBS MAX. 55 TECKEN                                       
255500             STRING WS-TEMFSINF-KAMP  DELIMITED BY SIZE                   
255600                    WS-TEMFSINF-SPLIT DELIMITED BY SIZE                   
255700                    WS-TEMFSINF       DELIMITED BY SIZE                   
255800             INTO MOD-TEMFSINF                                            
255900         END-IF                                                           
256000       END-IF                                                             
256100     END-IF                                                               
256200                                                                          
256300     .                                                                    
256400     EJECT                                                                
256500 GA-LAS-ACTIONFILE SECTION.                                               
256600     SKIP2                                                                
256700     MOVE +1 TO RAD-IX                                                    
256800     MOVE JA TO LAS-VIDARE                                                
256900                                                                          
257000     IF FLFORTS = JA                                                      
257100        PERFORM IMS-GU-XXAN11                                             
257200        PERFORM IMS-GNP-XXAN21                                            
257300     ELSE                                                                 
257400        PERFORM IMS-GET-XXAN21                                            
257500     END-IF                                                               
257600     PERFORM UNTIL SEGMENT-SAKNAS OR LAS-VIDARE = NEJ                     
257700        IF RAD-IX = MAX-RAD                                               
257800           MOVE NEJ TO LAS-VIDARE                                         
257900           MOVE MED-16(SPRAK-IX) TO MOD-TEMFSINF                          
258000        ELSE                                                              
258100         IF LAS-VIDARE = JA                                               
258200           IF XXAN-1114-FLTEXT = NEJ                                      
258300              MOVE XXAN-1114-DIERS-TILLK TO WS-DIERS-TILLK                
258400              MOVE XXAN-1114-IDARTNR-TILLK TO W-IDARTNR-TILLK             
258500              MOVE MFS-STAENG-FAELT TO MOD-BEERS-ATTR(RAD-IX)             
258600           ELSE                                                           
258700              MOVE MFS-STAENG-FAELT TO MOD-IDARTNR-TILLK-ATTR             
258800                                      (RAD-IX)                            
258900              MOVE MFS-STAENG-FAELT TO MOD-DIERS-TILLK-ATTR               
259000                                      (RAD-IX)                            
259100           END-IF                                                         
259200         END-IF                                                           
259300           MOVE MFS-STAENG-FAELT TO MOD-IDKORTNR-ATTR(RAD-IX)             
259400           ADD +1 TO RAD-IX                                               
259500        END-IF                                                            
259600        PERFORM IMS-GET-XXAN21                                            
259700     END-PERFORM                                                          
259800                                                                          
259900     IF SEGMENT-FINNS                                                     
260000        CONTINUE                                                          
260100     ELSE                                                                 
260200        IF RAD-IX < MAX-RAD                                               
260300           CONTINUE                                                       
260400        ELSE                                                              
260500           IF RAD-IX = MAX-RAD                                            
260600              MOVE TIO TO WS-IDKORTNR-SPAR3                               
260700           END-IF                                                         
260800        END-IF                                                            
260900     END-IF                                                               
261000     .                                                                    
261100     EJECT                                                                
261200 GB-LAS-ERSREG SECTION.                                                   
261300     SKIP2                                                                
261400     MOVE +1 TO RAD-IX                                                    
261500     MOVE JA TO LAS-VIDARE                                                
261600                                                                          
261700     PERFORM IMS-GET-ERSA01                                               
261800     PERFORM IMS-GNP-ERSA11                                               
261900     PERFORM UNTIL SEGMENT-SAKNAS OR LAS-VIDARE = NEJ                     
262000        IF RAD-IX = MAX-RAD                                               
262100           MOVE NEJ TO LAS-VIDARE                                         
262200           MOVE ERSA11-IDKORTNR TO WS-IDKORTNR-SPAR1                      
262300           MOVE MED-16(SPRAK-IX) TO MOD-TEMFSINF                          
262400        ELSE                                                              
262500         IF LAS-VIDARE = JA                                               
262600           IF ERSA11-FLTEXT = NEJ                                         
262700              MOVE ERSA11-DIERS-TILLK TO WS-DIERS-TILLK                   
262800              MOVE ERSA11-IDARTNR-TILLK TO W-IDARTNR-TILLK                
262900              MULTIPLY ERSA11-IDKORTNR BY TIO GIVING WS-IDKORTNR          
263000              MOVE MFS-STAENG-FAELT TO MOD-IDKORTNR-ATTR(RAD-IX)          
263100                                       MOD-BEERS-ATTR(RAD-IX)             
263200           ELSE                                                           
263300              MULTIPLY ERSA11-IDKORTNR BY TIO GIVING WS-IDKORTNR          
263400              MOVE MFS-STAENG-FAELT TO MOD-IDKORTNR-ATTR(RAD-IX)          
263500                                       MOD-IDARTNR-TILLK-ATTR             
263600                                       (RAD-IX)                           
263700              MOVE MFS-STAENG-FAELT TO MOD-DIERS-TILLK-ATTR               
263800                                       (RAD-IX)                           
263900           END-IF                                                         
264000         END-IF                                                           
264100           ADD +1 TO RAD-IX                                               
264200        END-IF                                                            
264300        PERFORM IMS-GNP-ERSA11                                            
264400     END-PERFORM                                                          
264500                                                                          
264600     IF SEGMENT-FINNS                                                     
264700        CONTINUE                                                          
264800     ELSE                                                                 
264900        IF RAD-IX < MAX-RAD                                               
265000           CONTINUE                                                       
265100        ELSE                                                              
265200           IF RAD-IX = MAX-RAD                                            
265300              MOVE TIO TO WS-IDKORTNR-SPAR3                               
265400           END-IF                                                         
265500        END-IF                                                            
265600     END-IF                                                               
265700     .                                                                    
265800     EJECT                                                                
265900 GC-KOLLA-KAMPANJ   SECTION.                                              
266000     SKIP2                                                                
266100     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
266200     IF SQLCODE-WS = ZERO                                                 
266300       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
266400     END-IF                                                               
266500                                                                          
266600     MOVE ZERO               TO WS-ANTAL-KAMP                             
266700     MOVE NEJ                TO WS-FLAGGA-Q-KAMP                          
266800                                WS-FLAGGA-W-S-KAMP                        
266900     PERFORM UNTIL SQLCODE > ZERO                                         
267000       IF TP1KAMP-TISTODAT-KAMP > ZERO                                    
267100         MOVE TP1KAMP-TISTODAT-KAMP                                       
267200                             TO WS-JMFR-AAAAMMDD                          
267300       ELSE                                                               
267400         MOVE TP1KAMP-TISTADAT-KAMP                                       
267500                             TO WS-JMFR-AAAAMMDD                          
267600       END-IF                                                             
267700       IF WS-JMFR-AA > 50                                                 
267800         MOVE 19             TO WS-JMFR-AAAAMMDD (1:2)                    
267900       ELSE                                                               
268000         MOVE 20             TO WS-JMFR-AAAAMMDD (1:2)                    
268100       END-IF                                                             
268200       IF TP1KAMP-TISTODAT-KAMP = ZERO                                    
268300*    LÄGG TILL 5 ÅR                                                       
268400         ADD 50000           TO WS-JMFR-AAAAMMDD                          
268500       END-IF                                                             
268600       IF WS-JMFR-AAAAMMDD >= WS-DAGENS-AAAAMMDD                          
268700         IF TP1KAMP-KDKAMP = 'Q'                                          
268800           MOVE JA           TO WS-FLAGGA-Q-KAMP                          
268900         END-IF                                                           
269000         IF TP1KAMP-KDKAMP = 'W'                                          
269100         OR TP1KAMP-KDKAMP = 'S'                                          
269200           MOVE JA           TO WS-FLAGGA-W-S-KAMP                        
269300         END-IF                                                           
269400       END-IF                                                             
269500       ADD 1                 TO WS-ANTAL-KAMP                             
269600       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
269700     END-PERFORM                                                          
269800                                                                          
269900     IF  WS-FLAGGA-Q-KAMP   = JA                                          
270000     AND WS-FLAGGA-W-S-KAMP = NEJ                                         
270100       MOVE MED-92 (SPRAK-IX)    TO WS-TEMFSINF-KAMP                      
270200*            SM ETC                                                       
270300     ELSE                                                                 
270400       IF WS-FLAGGA-W-S-KAMP = JA                                         
270500       MOVE MED-91 (SPRAK-IX)    TO WS-TEMFSINF-KAMP                      
270600*            CAMPAIGN                                                     
270700       END-IF                                                             
270800     END-IF                                                               
270900     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
271000     .                                                                    
271100     EJECT                                                                
271200  H-LAS-ERSATT-ARTIKEL SECTION.                                           
271300     SKIP2                                                                
271400     MOVE JA TO INPUT-RETT                                                
271500                                                                          
271600     MOVE IDARTNR-WS TO W-IDARTNR                                         
271700     PERFORM IMS-GET-ARTC01                                               
271800     IF SEGMENT-SAKNAS                                                    
271900        MOVE FEL-2(SPRAK-IX) TO MOD-TEMFSINF                              
272000        MOVE NEJ TO INPUT-RETT                                            
272100     ELSE                                                                 
272200        MOVE ART-KDERS-UTG TO ERSATT-KDERS-UTG                            
272300        MOVE ART-FLERS TO ERSATT-FLERS                                    
272400        MOVE ART-IDLEVNR TO ERSATT-IDLEVNR                                
272500        MOVE ART-FLIART TO ERSATT-FLIART                                  
272600        MOVE ART-KDPRODSL TO ERSATT-KDPRODSL                              
272700        PERFORM IMS-GET-ARTC11                                            
272800        IF SEGMENT-FINNS                                                  
272900           MOVE CLAG-IDPROJ TO SPAR-IDPROJ                                
273000           MOVE CLAG-KDKSP TO ERSATT-KDKSP                                
273100           MOVE CLAG-FLLSRDEL TO ERSATT-FLLSRDEL                          
273200           MOVE CLAG-KDERS TO ERSATT-KDERS-C1                             
273300           MOVE CLAG-KDAVT TO ERSATT-KDAVT                                
273400           MOVE CLAG-IDINK TO ERSATT-IDINK                                
273500           MOVE CLAG-KDBPSR TO ERSATT-KDBPSR                              
273600           MOVE CLAG-PRARTSTD TO ERSATT-PRARTSTD                          
273700        ELSE                                                              
273800           MOVE ERSATT-KDERS-UTG TO ERSATT-KDERS-C1                       
273900        END-IF                                                            
274000        MOVE ERSATT-KDPRODSL     TO TEST-KDPRODSL                         
274100        IF KDPRODSL-VOLVO-ALL                                             
274200           IF CDC OR SDC                                                  
274300              CONTINUE                                                    
274400           ELSE                                                           
274500              MOVE MED-31(SPRAK-IX) TO MOD-TEMFSINF                       
274600              MOVE NEJ TO INPUT-RETT                                      
274700           END-IF                                                         
274800        END-IF                                                            
274900     END-IF                                                               
275000     .                                                                    
275100     EJECT                                                                
275200 I-UPPDAT-TILLK-ART-ERS-REG SECTION.                                      
275300     SKIP2                                                                
275400     MOVE JA TO INPUT-RETT                                                
275500     MOVE IDARTNR-WS TO W-1111-IDARTNR                                    
275600                        W-1112-IDARTNR                                    
275700                        W-IDARTNR                                         
275800                                                                          
275900     MOVE +1 TO TAB-IX                                                    
276000     PERFORM UNTIL TAB-IX > MAX-TAB                                       
276100        MOVE ZERO TO TAB-IDRADNR(TAB-IX)                                  
276200                     TAB-IDARTNR-TILLK(TAB-IX)                            
276300                     TAB-DIERS-TILLK(TAB-IX)                              
276400        MOVE SPACE TO TAB-BEERS(TAB-IX)                                   
276500        ADD +1 TO TAB-IX                                                  
276600     END-PERFORM                                                          
276700                                                                          
276800     IF FINNS-PA-ACTION-FILE = NEJ                                        
276900        PERFORM S04-LAS-ERSREG-TILL-TABELL                                
277000        PERFORM S05-UPPDATERA-TABELL                                      
277100     ELSE                                                                 
277200        PERFORM S03-UPPDATERA-RADER                                       
277300        PERFORM S01-LAS-ACTIONFILE-TILL-TAB                               
277400     END-IF                                                               
277500                                                                          
277600     IF FINNS-PA-ACTION-FILE = NEJ AND RADER-UPPDATERADE = NEJ            
277700        CONTINUE                                                          
277800     ELSE                                                                 
277900        PERFORM S18-RAKNA-KVKORT                                          
278000        IF WS-RAKNARE = ZERO                                              
278100           MOVE NEJ TO INPUT-RETT                                         
278200           MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                      
278300        END-IF                                                            
278400        IF WS-KDERS-2 = 4 OR 5 OR 6 OR 8                                  
278500           IF TEXT-FINNS = NEJ                                            
278600              MOVE NEJ TO INPUT-RETT                                      
278700              MOVE MFS-ALFA-FAELT-FEL TO MOD-KDERS-ATTR                   
278800              MOVE MED-10(SPRAK-IX) TO MOD-TEMFSINF                       
278900           END-IF                                                         
279000        END-IF                                                            
279100                                                                          
279200        IF INPUT-RETT = JA                                                
279300           PERFORM IMS-GET-XXAN01                                         
279400           IF SEGMENT-FINNS                                               
279500              PERFORM IMS-DLET-XXAN                                       
279600           END-IF                                                         
279700           PERFORM S06-RIV-TILLK-ART                                      
279800           PERFORM S08-SKAPA-TILLK-ART-TO-ERSREG                          
279900           MOVE IDARTNR-WS TO W-IDARTNR                                   
280000           PERFORM IMS-GET-ERSA01                                         
280100           MOVE WS-RAKNARE TO ERSA01-KVKORT                               
280200           PERFORM IMS-REPL-ERSA                                          
280300           PERFORM IMS-GET-ERSA01                                         
280400           PERFORM IMS-GET-ERSA13                                         
280500           IF SEGMENT-FINNS                                               
280600              IF ERSATT-KDERS-C1 = 01 OR 02 OR 04 OR 05                   
280700                 MOVE WS-TIFINLV-MAX TO ERSA13-TIERSDAT-PREL-C1           
280800                 PERFORM IMS-REPL-ERSA                                    
280900              END-IF                                                      
281000          END-IF                                                          
281100          MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF                           
281200       END-IF                                                             
281300     END-IF                                                               
281400     .                                                                    
281500     EJECT                                                                
281600 K-UPPDAT-TIERSDAT-TEARTNOT SECTION.                                      
281700     SKIP2                                                                
281800     IF MID-TEARTNOT = ALL '+' OR SPACE                                   
281900        CONTINUE                                                          
282000     ELSE                                                                 
282100        MOVE MID-TEARTNOT TO ERSA01-TEARTNOT                              
282200        PERFORM IMS-REPL-ERSA                                             
282300        MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF                             
282400     END-IF                                                               
282500                                                                          
282600     IF MID-TIERSDAT-PREL = ALL '+' OR SPACE                              
282700        CONTINUE                                                          
282800     ELSE                                                                 
282900        PERFORM IMS-GET-ERSA13                                            
283000        IF SEGMENT-FINNS                                                  
283100           MOVE MID-TIERSDAT-PREL TO ERSA13-TIERSDAT-PREL-C1              
283200           PERFORM IMS-REPL-ERSA                                          
283300           IF ERSATT-FLIART = JA                                          
283400              PERFORM KA-UPPDATERA-RASA                                   
283500           END-IF                                                         
283600           MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF                          
283700        END-IF                                                            
283800     END-IF                                                               
283900     .                                                                    
284000     EJECT                                                                
284100 KA-UPPDATERA-RASA SECTION.                                               
284200     SKIP2                                                                
284300     MOVE IDARTNR-WS TO W-IDARTNR-S                                       
284400                        W-IDARTNR                                         
284500     MOVE SPACE TO W-IDLEVNR-S                                            
284600     MOVE SPACE TO W-BELEVART-S                                           
284700     PERFORM IMS-GET-SATB-CSEQ                                            
284800     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
284900        IF SATB-STR-IDARTNR < 100000000                                   
285000          IF SATB-STR-IDLEVNR = '1002 '                                   
285100           IF SATB-RAD-KDISATS = 'E'                                      
285200              MOVE SATB-RAD-TISTODAT TO WS-TISTODAT                       
285300              MOVE SATB-RAD-IDRADNR  TO W-IDRADNR                         
285400              MOVE SATB-RAD-KDSTRRAD TO W-KDSTRRAD                        
285500                                                                          
285600              MOVE SATB-STR-IDARTNR TO W-IDARTNR                          
285700              PERFORM IMS-GET-SATB01                                      
285800              PERFORM IMS-GHNP-SATB11                                     
285900              MOVE MID-TIERSDAT-PREL TO DAT-I-TIDATUM                     
286000              MOVE 'AAVVD '          TO DAT-KDDATFORM                     
286100              CALL WDATKONV USING DAT-KDDATFORM                           
286200                                  DAT-I-TIDATUM                           
286300                                  DAT-O-TIDATUM                           
286400                                  DAT-KDSVAR                              
286500              MOVE DAT-TIAAMMDD TO SATB-RAD-TISTODAT                      
286600              PERFORM IMS-REPL-SATB                                       
286700                                                                          
286800              MOVE +1 TO STR-IX                                           
286900              PERFORM UNTIL STR-IX > STR-IX-MAX                           
287000                 MOVE ZERO TO WS-STR-TILLKART(STR-IX)                     
287100                 ADD +1 TO STR-IX                                         
287200              END-PERFORM                                                 
287300                                                                          
287400              MOVE +1 TO STR-IX                                           
287500              MOVE IDARTNR-WS TO W-IDARTNR                                
287600              PERFORM IMS-GET-ERSA01                                      
287700              PERFORM IMS-GET-ERSA11                                      
287800              PERFORM UNTIL SEGMENT-SAKNAS                                
287900                IF ERSA11-FLTEXT = NEJ                                    
288000                  MOVE ERSA11-IDARTNR-TILLK TO                            
288100                     WS-STR-TILLKART(STR-IX)                              
288200                     ADD +1 TO STR-IX                                     
288300                END-IF                                                    
288400                PERFORM IMS-GET-ERSA11                                    
288500              END-PERFORM                                                 
288600                                                                          
288700              PERFORM IMS-GET-SATB11-FIRST                                
288800              PERFORM UNTIL SEGMENT-SAKNAS                                
288900                IF SATB-RAD-KDISATS = 'T'                                 
289000                  IF SATB-RAD-TISTADAT = WS-TISTODAT                      
289100                    MOVE +1 TO STR-IX                                     
289200                    PERFORM UNTIL STR-IX > STR-IX-MAX                     
289300                      IF SATB-RAD-IDARTNR =                               
289400                         WS-STR-TILLKART(STR-IX)                          
289500                         MOVE DAT-TIAAMMDD TO SATB-RAD-TISTADAT           
289600                         PERFORM IMS-REPL-SATB                            
289700                         MOVE +99 TO STR-IX                               
289800                      END-IF                                              
289900                      ADD +1 TO STR-IX                                    
290000                    END-PERFORM                                           
290100                   END-IF                                                 
290200                  END-IF                                                  
290300                  PERFORM IMS-GET-SATB11                                  
290400                END-PERFORM                                               
290500             END-IF                                                       
290600          END-IF                                                          
290700        END-IF                                                            
290800        PERFORM IMS-GET-SATB-CSEQ                                         
290900     END-PERFORM                                                          
291000     .                                                                    
291100     EJECT                                                                
291200 S01-LAS-ACTIONFILE-TILL-TAB SECTION.                                     
291300     SKIP2                                                                
291400     MOVE IDARTNR-WS TO W-1111-IDARTNR                                    
291500                        W-1112-IDARTNR                                    
291600     PERFORM IMS-GET-XXAN01                                               
291700     PERFORM IMS-GET-XXAN11                                               
291800     PERFORM IMS-GET-XXAN21                                               
291900     PERFORM UNTIL SEGMENT-SAKNAS                                         
292000        MOVE XXAN-1114-IDKORTNR TO TAB-IX                                 
292100        MOVE XXAN-1114-IDKORTNR TO TAB-IDRADNR(TAB-IX)                    
292200        IF XXAN-1114-FLTEXT = JA                                          
292300           MOVE XXAN-1114-BEERS TO TAB-BEERS(TAB-IX)                      
292400        ELSE                                                              
292500           MOVE XXAN-1114-IDARTNR-TILLK TO TAB-IDARTNR-TILLK              
292600                                       (TAB-IX)                           
292700           MOVE XXAN-1114-DIERS-TILLK TO TAB-DIERS-TILLK                  
292800                                        (TAB-IX)                          
292900        END-IF                                                            
293000        PERFORM IMS-GET-XXAN21                                            
293100     END-PERFORM                                                          
293200     .                                                                    
293300     EJECT                                                                
293400 S02-LAS-ERSREG-TILL-ACTIONFILE SECTION.                                  
293500     SKIP2                                                                
293600     MOVE IDARTNR-WS TO W-IDARTNR                                         
293700                        W-1111-IDARTNR                                    
293800                        W-1112-IDARTNR                                    
293900     MOVE ZERO       TO W-IDKORTNR                                        
294000     PERFORM IMS-GET-ERSA01                                               
294100                                                                          
294200     MOVE IDARTNR-WS TO XXAN-1111-IDARTNR                                 
294300     MOVE '1111'     TO XXAN-1111-IDHTYP                                  
294400     MOVE LOW-VALUE  TO XXAN-1111-LOWVALUE                                
294500     PERFORM IMS-ISRT-XXAN01                                              
294600                                                                          
294700     MOVE IDARTNR-WS        TO XXAN-1112-IDARTNR                          
294800     MOVE LOW-VALUE         TO XXAN-1112-LOWVALUE                         
294900     MOVE MSG-SIGNON-USERID TO XXAN-1112-IDUSER                           
295000     MOVE DAGENS-DATUM      TO XXAN-1112-TIUPPDAT                         
295100     PERFORM IMS-ISRT-XXAN11                                              
295200                                                                          
295300     PERFORM IMS-GNP-ERSA11                                               
295400     PERFORM UNTIL SEGMENT-SAKNAS                                         
295500        MULTIPLY ERSA11-IDKORTNR  BY TIO GIVING                           
295600                                       XXAN-1114-IDKORTNR                 
295700        MOVE LOW-VALUE              TO XXAN-1114-LOWVALUE                 
295800        MOVE ERSA11-FLTEXT          TO XXAN-1114-FLTEXT                   
295900        IF ERSA11-FLTEXT = NEJ                                            
296000           MOVE ERSA11-IDARTNR-TILLK TO XXAN-1114-IDARTNR-TILLK           
296100           MOVE ERSA11-DIERS-TILLK   TO XXAN-1114-DIERS-TILLK             
296200        ELSE                                                              
296300           MOVE ERSA11-BEERS        TO XXAN-1114-BEERS                    
296400        END-IF                                                            
296500        PERFORM IMS-ISRT-XXAN21                                           
296600        MOVE SPACE TO XXAN-1114-BEERS                                     
296700        PERFORM IMS-GNP-ERSA11                                            
296800     END-PERFORM                                                          
296900     .                                                                    
297000     EJECT                                                                
297100  S03-UPPDATERA-RADER SECTION.                                            
297200     SKIP2                                                                
297300     MOVE IDARTNR-WS TO W-1111-IDARTNR                                    
297400                        W-1112-IDARTNR                                    
297500                                                                          
297600     MOVE +1 TO RAD-IX                                                    
297700     PERFORM UNTIL RAD-IX > MAX-RAD                                       
297800      IF MID-IDKORTNR(RAD-IX) NUMERIC                                     
297900        MOVE MID-IDKORTNR(RAD-IX) TO WS-IDKORTNR                          
298000        MOVE WS-IDKORTNR          TO W-1114-IDKORTNR                      
298100                                                                          
298200        IF MID-IDARTNR-TILLK(RAD-IX) = ALL '+' AND                        
298300           MID-DIERS-TILLK(RAD-IX) = ALL '+' AND                          
298400           MID-BEERS(RAD-IX) = ALL '+'                                    
298500           CONTINUE                                                       
298600        ELSE                                                              
298700          IF MID-IDARTNR-TILLK(RAD-IX) = SPACE                            
298800             PERFORM IMS-GHU-XXAN21                                       
298900             IF SEGMENT-FINNS                                             
299000               IF XXAN-1114-FLTEXT = NEJ                                  
299100                  MOVE ZERO TO XXAN-1114-IDARTNR-TILLK                    
299200                            XXAN-1114-DIERS-TILLK                         
299300                  PERFORM IMS-REPL-XXAN                                   
299400                  MOVE JA TO RADER-UPPDATERADE                            
299500                  MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF                   
299600               END-IF                                                     
299700             END-IF                                                       
299800          ELSE                                                            
299900            IF (MID-IDARTNR-TILLK(RAD-IX) NUMERIC) AND                    
300000               (MID-DIERS-TILLK(RAD-IX) NOT = SPACE)                      
300100               MOVE MID-IDARTNR-TILLK(RAD-IX) TO                          
300200                                  WS-IDARTNR-TILLK                        
300300               MOVE MID-DIERS-TILLK(RAD-IX) TO DEC-IDFRIDATA              
300400               MOVE 3                       TO DEC-KVHELTAL               
300500               MOVE 3                       TO DEC-KVDECIMAL              
300600               CALL WDECEDIT USING DEC-WDECAREA                           
300700               PERFORM IMS-GHU-XXAN21                                     
300800               IF SEGMENT-FINNS                                           
300900                 IF XXAN-1114-FLTEXT = NEJ                                
301000                    MOVE IDARTNR-TILLK-WS  TO                             
301100                               XXAN-1114-IDARTNR-TILLK                    
301200                    MOVE DEC-IDEDITDATA    TO                             
301300                               XXAN-1114-DIERS-TILLK                      
301400                    PERFORM IMS-REPL-XXAN                                 
301500                    MOVE JA TO RADER-UPPDATERADE                          
301600                    MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF                 
301700                 END-IF                                                   
301800               ELSE                                                       
301900                 MOVE WS-IDKORTNR  TO XXAN-1114-IDKORTNR                  
302000                 MOVE LOW-VALUE    TO XXAN-1114-LOWVALUE                  
302100                 MOVE NEJ          TO XXAN-1114-FLTEXT                    
302200                 MOVE DEC-IDEDITDATA  TO                                  
302300                                XXAN-1114-DIERS-TILLK                     
302400                 MOVE IDARTNR-TILLK-WS TO                                 
302500                                XXAN-1114-IDARTNR-TILLK                   
302600                 PERFORM IMS-ISRT-XXAN21                                  
302700                 MOVE JA TO RADER-UPPDATERADE                             
302800                 MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF                    
302900               END-IF                                                     
303000            ELSE                                                          
303100              IF MID-BEERS(RAD-IX) = SPACE                                
303200                PERFORM IMS-GHU-XXAN21                                    
303300                IF SEGMENT-FINNS                                          
303400                  IF XXAN-1114-FLTEXT = JA                                
303500                     MOVE SPACE TO XXAN-1114-BEERS                        
303600                     PERFORM IMS-REPL-XXAN                                
303700                     MOVE JA TO RADER-UPPDATERADE                         
303800                     MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF                
303900                  END-IF                                                  
304000                END-IF                                                    
304100              ELSE                                                        
304200                IF MID-BEERS(RAD-IX) NOT = SPACE                          
304300                  IF MID-BEERS(RAD-IX) NOT = ALL '+'                      
304400                     PERFORM IMS-GHU-XXAN21                               
304500                     IF SEGMENT-FINNS                                     
304600                        IF XXAN-1114-FLTEXT = JA                          
304700                           MOVE MID-BEERS(RAD-IX)  TO                     
304800                                   XXAN-1114-BEERS                        
304900                           PERFORM IMS-REPL-XXAN                          
305000                           MOVE JA TO RADER-UPPDATERADE                   
305100                           MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF          
305200                        END-IF                                            
305300                     ELSE                                                 
305400                       MOVE WS-IDKORTNR TO XXAN-1114-IDKORTNR             
305500                       MOVE LOW-VALUE   TO XXAN-1114-LOWVALUE             
305600                       MOVE JA          TO XXAN-1114-FLTEXT               
305700                       MOVE JA TO RADER-UPPDATERADE                       
305800                       MOVE MID-BEERS(RAD-IX) TO XXAN-1114-BEERS          
305900                       PERFORM IMS-ISRT-XXAN21                            
306000                       MOVE MED-15(SPRAK-IX) TO MOD-TEMFSINF              
306100                     END-IF                                               
306200                  END-IF                                                  
306300                END-IF                                                    
306400              END-IF                                                      
306500            END-IF                                                        
306600          END-IF                                                          
306700****************************                                              
306800        END-IF                                                            
306900       END-IF                                                             
307000       ADD +1 TO RAD-IX                                                   
307100     END-PERFORM                                                          
307200     .                                                                    
307300     EJECT                                                                
307400  S04-LAS-ERSREG-TILL-TABELL SECTION.                                     
307500     SKIP2                                                                
307600     MOVE ZERO TO TAB-IX                                                  
307700     MOVE IDARTNR-WS TO W-IDARTNR                                         
307800     MOVE ZERO       TO W-IDKORTNR                                        
307900     PERFORM IMS-GET-ERSA01                                               
308000                                                                          
308100     PERFORM IMS-GNP-ERSA11                                               
308200     PERFORM UNTIL SEGMENT-SAKNAS                                         
308300       MULTIPLY ERSA11-IDKORTNR BY TIO GIVING TAB-IX                      
308400       MULTIPLY ERSA11-IDKORTNR BY TIO GIVING TAB-IDRADNR(TAB-IX)         
308500       IF ERSA11-FLTEXT = NEJ                                             
308600         MOVE ERSA11-IDARTNR-TILLK TO TAB-IDARTNR-TILLK(TAB-IX)           
308700         MOVE ERSA11-DIERS-TILLK   TO TAB-DIERS-TILLK(TAB-IX)             
308800       ELSE                                                               
308900         MOVE ERSA11-BEERS         TO TAB-BEERS(TAB-IX)                   
309000       END-IF                                                             
309100       PERFORM IMS-GNP-ERSA11                                             
309200     END-PERFORM                                                          
309300     .                                                                    
309400     EJECT                                                                
309500 S05-UPPDATERA-TABELL SECTION.                                            
309600     SKIP2                                                                
309700     MOVE +1 TO RAD-IX                                                    
309800     PERFORM UNTIL RAD-IX > MAX-RAD                                       
309900        IF MID-IDKORTNR(RAD-IX) NUMERIC                                   
310000           MOVE MID-IDKORTNR(RAD-IX) TO WS-IDKORTNR                       
310100           MOVE WS-IDKORTNR TO TAB-IX                                     
310200                                                                          
310300           IF  MID-IDARTNR-TILLK(RAD-IX) = ALL '+'                        
310400           AND MID-DIERS-TILLK(RAD-IX) = ALL '+'                          
310500           AND MID-BEERS(RAD-IX) = ALL '+'                                
310600              CONTINUE                                                    
310700           ELSE                                                           
310800              IF MID-IDARTNR-TILLK(RAD-IX) = SPACE                        
310900                 MOVE ZERO TO TAB-IDARTNR-TILLK(TAB-IX)                   
311000                 MOVE JA TO RADER-UPPDATERADE                             
311100              ELSE                                                        
311200                 IF  (MID-IDARTNR-TILLK(RAD-IX) NUMERIC)                  
311300                 AND (MID-DIERS-TILLK(RAD-IX) NOT = SPACE)                
311400                    MOVE MID-IDARTNR-TILLK(RAD-IX)                        
311500                                TO  WS-IDARTNR-TILLK                      
311600                    MOVE WS-IDARTNR-TILLK                                 
311700                                TO TAB-IDARTNR-TILLK(TAB-IX)              
311800                    MOVE MID-DIERS-TILLK(RAD-IX)                          
311900                                TO DEC-IDFRIDATA                          
312000                    MOVE 3      TO DEC-KVHELTAL                           
312100                    MOVE 3      TO DEC-KVDECIMAL                          
312200                    CALL WDECEDIT USING DEC-WDECAREA                      
312300                    MOVE DEC-IDEDITDATA                                   
312400                                TO TAB-DIERS-TILLK(TAB-IX)                
312500                    MOVE JA     TO RADER-UPPDATERADE                      
312600                 ELSE                                                     
312700                    IF MID-BEERS(RAD-IX) = SPACE                          
312800                       MOVE SPACE TO TAB-BEERS(TAB-IX)                    
312900                       MOVE JA   TO RADER-UPPDATERADE                     
313000                    ELSE                                                  
313100                       IF MID-BEERS(RAD-IX) NOT = SPACE                   
313200                          IF MID-BEERS(RAD-IX) NOT = ALL '+'              
313300                             MOVE MID-BEERS(RAD-IX)                       
313400                                     TO TAB-BEERS(TAB-IX)                 
313500                             MOVE JA TO RADER-UPPDATERADE                 
313600                          END-IF                                          
313700                       END-IF                                             
313800                    END-IF                                                
313900                 END-IF                                                   
314000              END-IF                                                      
314100           END-IF                                                         
314200        END-IF                                                            
314300        ADD +1 TO RAD-IX                                                  
314400     END-PERFORM                                                          
314500     .                                                                    
314600     EJECT                                                                
314700 S06-RIV-TILLK-ART SECTION.                                               
314800     SKIP2                                                                
314900     MOVE IDARTNR-WS TO W-IDARTNR                                         
315000     PERFORM IMS-GET-ERSA01                                               
315100     IF SEGMENT-FINNS                                                     
315200         PERFORM IMS-GET-ERSA11                                           
315300         PERFORM UNTIL SEGMENT-SAKNAS                                     
315400            IF ERSA11-FLTEXT = 'N'                                        
315500               MOVE ERSA11-IDARTNR-TILLK TO W-IDARTNR                     
315600               PERFORM IMS-DLET-ERSA                                      
315700               PERFORM IMS-GET-ERSB01                                     
315800               IF SEGMENT-SAKNAS                                          
315900                  PERFORM IMS-GET-ARTC01                                  
316000                  IF SEGMENT-FINNS                                        
316100                     MOVE NEJ TO ART-FLERS                                
316200                     PERFORM IMS-REPL-ARTC                                
316300                  END-IF                                                  
316400               END-IF                                                     
316500            ELSE                                                          
316600               PERFORM IMS-DLET-ERSA                                      
316700            END-IF                                                        
316800            PERFORM IMS-GET-ERSA11                                        
316900         END-PERFORM                                                      
317000     END-IF                                                               
317100     .                                                                    
317200     EJECT                                                                
317300 S07-SKAPA-ERSATT-ART-TO-ERSREG SECTION.                                  
317400     SKIP2                                                                
317500     MOVE IDARTNR-WS         TO ERSA01-IDARTNR                            
317600     MOVE MID-DIERS-ERS      TO DEC-IDFRIDATA                             
317700     MOVE 3                  TO DEC-KVHELTAL                              
317800     MOVE 3                  TO DEC-KVDECIMAL                             
317900     CALL WDECEDIT USING DEC-WDECAREA                                     
318000     MOVE DEC-IDEDITDATA     TO ERSA01-DIERS-ERS                          
318100     IF WS-KDERS =  07 OR 08 OR 09 OR 27 OR 28                            
318200         OR 29 OR 52                                                      
318300         MOVE NEJ            TO ERSA01-FLPUB                              
318400     ELSE                                                                 
318500         MOVE JA             TO ERSA01-FLPUB                              
318600     END-IF                                                               
318700     MOVE MSG-SIGNON-USERID  TO ERSA01-IDUSER                             
318800     IF MID-TEARTNOT = ALL '+' OR SPACE                                   
318900        IF WS-TEARTNOT = SPACE                                            
319000           MOVE SPACE        TO ERSA01-TEARTNOT                           
319100        ELSE                                                              
319200           MOVE WS-TEARTNOT  TO ERSA01-TEARTNOT                           
319300        END-IF                                                            
319400     ELSE                                                                 
319500        MOVE MID-TEARTNOT    TO ERSA01-TEARTNOT                           
319600     END-IF                                                               
319700     .                                                                    
319800     EJECT                                                                
319900 S08-SKAPA-TILLK-ART-TO-ERSREG SECTION.                                   
320000     SKIP2                                                                
320100     MOVE IDARTNR-WS TO W-IDARTNR                                         
320200     PERFORM IMS-GET-ARTC01                                               
320300     PERFORM IMS-GET-ARTC11                                               
320400     IF SEGMENT-FINNS                                                     
320500        MOVE CLAG-IDANSK TO WS-IDANSK                                     
320600     ELSE                                                                 
320700        MOVE ZERO        TO WS-IDANSK                                     
320800     END-IF                                                               
320900     MOVE ZERO TO WS-RAKNARE                                              
321000     MOVE +1 TO TAB-IX                                                    
321100     PERFORM UNTIL TAB-IX > MAX-TAB                                       
321200        IF TAB-IDARTNR-TILLK(TAB-IX) > ZERO                               
321300           PERFORM S09-LAS-TILLK-ART                                      
321400           ADD +1 TO WS-RAKNARE                                           
321500           MOVE SPACE                     TO ERSA11-BEERS                 
321600           MOVE TAB-IDARTNR-TILLK(TAB-IX) TO ERSA11-IDARTNR-TILLK         
321700           MOVE TAB-DIERS-TILLK(TAB-IX)   TO ERSA11-DIERS-TILLK           
321800           MOVE WS-RAKNARE                TO ERSA11-IDKORTNR              
321900           MOVE NEJ                       TO ERSA11-FLTEXT                
322000           MOVE IDARTNR-WS                TO W-IDARTNR                    
322100           PERFORM IMS-ISRT-ERSA11                                        
322200        ELSE                                                              
322300           IF TAB-BEERS(TAB-IX) NOT = SPACE                               
322400              MOVE SPACE        TO ERSA11-BEERS                           
322500              ADD +1 TO WS-RAKNARE                                        
322600              MOVE TAB-BEERS(TAB-IX)  TO ERSA11-BEERS                     
322700              MOVE WS-RAKNARE         TO ERSA11-IDKORTNR                  
322800              MOVE JA                 TO ERSA11-FLTEXT                    
322900              MOVE IDARTNR-WS TO W-IDARTNR                                
323000              PERFORM IMS-ISRT-ERSA11                                     
323100           END-IF                                                         
323200        END-IF                                                            
323300        ADD +1 TO TAB-IX                                                  
323400     END-PERFORM                                                          
323500     .                                                                    
323600     EJECT                                                                
323700 S09-LAS-TILLK-ART SECTION.                                               
323800     SKIP2                                                                
323900     MOVE TAB-IDARTNR-TILLK(TAB-IX) TO WS-IDARTNR-TILLK                   
324000     MOVE IDARTNR-TILLK-WS TO W-IDARTNR                                   
324100     PERFORM IMS-GET-ARTC01                                               
324200     MOVE ART-TIFINLV      TO TMP1-YYWWD                                  
324300     MOVE WS-TIFINLV-MAX   TO TMP2-YYWWD                                  
324400     PERFORM WY2000P2                                                     
324500     IF TMP1-YYWWD > TMP2-YYWWD                                           
324600        MOVE ART-TIFINLV TO WS-TIFINLV-MAX                                
324700     END-IF                                                               
324800     MOVE JA TO ART-FLERS                                                 
324900     PERFORM IMS-REPL-ARTC                                                
325000     .                                                                    
325100     EJECT                                                                
325200 S10-LAGG-UPP-IDAO SECTION.                                               
325300     SKIP2                                                                
325400                                                                          
325500     MOVE SPACE TO SPARADE-IDAO                                           
325600                   NYA-IDAO                                               
325700     MOVE NEJ   TO IDAO-FINNS                                             
325800                   SW-IDAO-FIXAD                                          
325900                   SW-TOMMA-IDAO-FINNS                                    
326000                                                                          
326100     IF MID-IDAO = ALL '+' OR SPACE                                       
326200        CONTINUE                                                          
326300     ELSE                                                                 
326400        MOVE IDARTNR-WS TO W-IDARTNR                                      
326500        PERFORM IMS-GET-ARTC01                                            
326600        MOVE +1 TO IX                                                     
326700        PERFORM UNTIL IX > 5                                              
326800           IF ART-IDAO(IX) = SPACE                                        
326900              MOVE +6 TO IX                                               
327000           ELSE                                                           
327100              IF MID-IDAO = ART-IDAO(IX)                                  
327200                 MOVE JA TO IDAO-FINNS                                    
327300                 MOVE +6 TO IX                                            
327400              ELSE                                                        
327500                 MOVE ART-IDAO(IX) TO SPARAD-IDAO(IX)                     
327600******************************************************************        
327700*                OM ÄO:T HAR "A" I 1:A POSITION, SKA MAN BYTA UT          
327800*                DETTA ÄO:T I FÖRSTA HAND.                                
327900*                (FÖRUTSATT ATT DET  REDAN FINNS 5 REG ÄO, ALLA           
328000*                 PLATSER FULLA!)                                         
328100*                OBS, MAN KONTROLLERAR ALDRIG DET 1:A ÄO:T, DET SK        
328200*                MAN ALDRIG RÖRA.                                         
328300******************************************************************        
328400                 IF (IX > 1) AND ( IDAO-POS-1(IX) = 'A' )                 
328500                    IF ART-IDAO(5) > SPACE                                
328600                       MOVE MID-IDAO TO ART-IDAO(IX)                      
328700                       MOVE MAX-IX-VAERDE TO IX                           
328800                       MOVE JA TO SW-IDAO-FIXAD                           
328900                    END-IF                                                
329000                 END-IF                                                   
329100                 ADD +1 TO IX                                             
329200              END-IF                                                      
329300           END-IF                                                         
329400        END-PERFORM                                                       
329500*                                                                         
329600******************************************************************        
329700*       ARTC01-IDAO(1) SKA ALDRIG RÖRAS *** ÄT-BEREDNINGEN 8906***        
329800*       DET 2:A ÄO:T 'PUTTAS ÖVER KANTEN ', DET NYA KOMMER IN             
329900*       PÅ PLATS 5.                                                       
330000******************************************************************        
330100        IF (IDAO-FINNS = NEJ) AND (SW-IDAO-FIXAD = NEJ)                   
330200           MOVE +5 TO IX                                                  
330300           PERFORM UNTIL (IX < 1) OR (SW-IDAO-FIXAD = JA)                 
330400              IF SPARAD-IDAO(IX) = SPACE                                  
330500                 MOVE JA TO SW-TOMMA-IDAO-FINNS                           
330600                 SUBTRACT 1 FROM IX                                       
330700              ELSE                                                        
330800                 IF SW-TOMMA-IDAO-FINNS = JA                              
330900                    COMPUTE                                               
331000                    IX-PLUS-1 = IX + 1                                    
331100                    MOVE MID-IDAO TO ART-IDAO(IX-PLUS-1)                  
331200                    MOVE JA TO SW-IDAO-FIXAD                              
331300                 ELSE                                                     
331400                    IF IX < 2                                             
331500                       CONTINUE                                           
331600                    ELSE                                                  
331700                       COMPUTE                                            
331800                       IX-MINUS-1 = IX - 1                                
331900                       MOVE SPARAD-IDAO(IX) TO                            
332000                                    ART-IDAO(IX-MINUS-1)                  
332100                    END-IF                                                
332200                    SUBTRACT 1 FROM IX                                    
332300                 END-IF                                                   
332400              END-IF                                                      
332500           END-PERFORM                                                    
332600           IF SW-TOMMA-IDAO-FINNS = NEJ                                   
332700              MOVE MID-IDAO TO ART-IDAO(5)                                
332800           END-IF                                                         
332900        END-IF                                                            
333000******* ENDAST OM ÄO SAKNAS UPPDATERAS ÄO-IND(1)     ************         
333100        IF SPARAD-IDAO(1) = SPACE                                         
333200           MOVE MID-IDAO TO ART-IDAO(1)                                   
333300        ELSE                                                              
333400           MOVE  SPARAD-IDAO(1) TO ART-IDAO(1)                            
333500        END-IF                                                            
333600        PERFORM IMS-REPL-ARTC                                             
333700     END-IF                                                               
333800     .                                                                    
333900     EJECT                                                                
334000 S12-SKAPA-SATSTRANS SECTION.                                             
334100     SKIP2                                                                
334200     MOVE LOW-VALUE         TO IO-AREA                                    
334300     MOVE ZERO              TO 2303-2304-IDARTNR-ING                      
334400                               2303-2304-IDARTNR-SATS                     
334500     IF ERSATT-FLIART = JA                                                
334600        MOVE IDARTNR-WS     TO 2303-2304-IDARTNR-ING                      
334700     END-IF                                                               
334800     IF ERSATT-IDLEVNR = '1002 '                                          
334900        MOVE IDARTNR-WS     TO 2303-2304-IDARTNR-SATS                     
335000     END-IF                                                               
335100     MOVE WS-KDERS          TO 2303-2304-KDERS-NEW                        
335200     MOVE ERSATT-KDERS-C1   TO 2303-2304-KDERS-OLD                        
335300                                                                          
335400     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
335500     CALL WDATKONV USING DAT-KDDATFORM                                    
335600                         DAT-I-TIDATUM                                    
335700                         DAT-O-TIDATUM                                    
335800                         DAT-KDSVAR                                       
335900     MOVE DAT-TIAAVVD       TO 2303-2304-TIERSDAT-PREL                    
336000                                                                          
336100     PERFORM IMS-ISRT-2304                                                
336200     .                                                                    
336300     EJECT                                                                
336400 S15-SKAPA-LEV-PLANTRANS SECTION.                                         
336500     SKIP2                                                                
336600     MOVE LOW-VALUE         TO IO-AREA                                    
336700     MOVE WC-CDC-SE         TO W-2203-IDDC                                
336800     MOVE IDARTNR-WS        TO 2204-IDARTNR                               
336900     MOVE +21               TO 2204-KDLPORS                               
337000                                                                          
337100     PERFORM IMS-ISRT-XXBJ                                                
337200     .                                                                    
337300     EJECT                                                                
337400 S16-SKAPA-VR-TRANS SECTION.                                              
337500     SKIP2                                                                
337600*****************************************************************         
337700*  9101-TRANS SKAPAS VID                                        *         
337800*  -  ERSÄTTNING UPPÅT TILL NY EK > 10                          *         
337900*  -  (RIVNING AV ERSÄTTNING FRÅN EK > 10 TILL 0)               *         
338000*  -  ÄT FEBR 93 VR VILL HA SAMTLIGA RIVNINGAR TILL 00          *         
338100*                OBEROENDE AV TIDIGARE EK                       *         
338200*                VIPS VILL HA RIVNINGAR > 20 TILL < 10          *         
338300*                BEHANDLAS I VIPS SOM RIVNING TILL 00           *         
338400*  -  BYTE EK FRÅN EK > 20                                      *         
338500*  -  UPPDATERING AV TILLK ARTIKLAR VID EK > 20                 *         
338600*  -  (UPPDAT AV TILLK ART / BYTE EK VID PREL EK EJ TILLÅTEN)   *         
338700*****************************************************************         
338800     IF (WS-KDERS < 10 AND > ZERO) AND (ERSATT-KDERS-C1 < 20)             
338900        CONTINUE                                                          
339000     ELSE                                                                 
339100        MOVE LOW-VALUE TO    IO-AREA                                      
339200        MOVE IDARTNR-WS      TO XXID-IDARTNR                              
339300        MOVE ERSATT-KDERS-C1 TO XXID-KDERS-OLD                            
339400        MOVE WS-KDERS        TO XXID-KDERS-NEW                            
339500                                                                          
339600        PERFORM IMS-ISRT-XXID                                             
339700     END-IF                                                               
339800     .                                                                    
339900     EJECT                                                                
340000 S17-UPPDAT-TILLK-ART-ACTION SECTION.                                     
340100     SKIP2                                                                
340200     IF FINNS-PA-ACTION-FILE = NEJ                                        
340300        PERFORM S02-LAS-ERSREG-TILL-ACTIONFILE                            
340400        PERFORM S03-UPPDATERA-RADER                                       
340500     ELSE                                                                 
340600        PERFORM S03-UPPDATERA-RADER                                       
340700     END-IF                                                               
340800     .                                                                    
340900     EJECT                                                                
341000 S18-RAKNA-KVKORT SECTION.                                                
341100     SKIP2                                                                
341200     MOVE NEJ TO TEXT-FINNS                                               
341300     MOVE ZERO TO WS-RAKNARE                                              
341400     MOVE +1 TO TAB-IX                                                    
341500     PERFORM UNTIL TAB-IX > MAX-TAB                                       
341600        IF TAB-IDARTNR-TILLK(TAB-IX) > ZERO                               
341700           ADD +1 TO WS-RAKNARE                                           
341800        ELSE                                                              
341900           IF TAB-BEERS(TAB-IX) NOT = SPACE                               
342000              ADD +1 TO WS-RAKNARE                                        
342100              IF WS-RAKNARE = 1                                           
342200                 MOVE JA TO TEXT-FINNS                                    
342300              END-IF                                                      
342400           END-IF                                                         
342500        END-IF                                                            
342600        ADD +1 TO TAB-IX                                                  
342700     END-PERFORM                                                          
342800     .                                                                    
342900     EJECT                                                                
343000 S19-SKAPA-BEVAKNINGS-SEGMENT SECTION.                                    
343100     SKIP2                                                                
343200     IF WS-KDERS = '01' OR '02' OR '04' OR '05'                           
343300        MOVE 2                 TO ERSA13-KDSTATUS-C1                      
343400        MOVE WS-TIFINLV-MAX    TO ERSA13-TIERSDAT-PREL-C1                 
343500     ELSE                                                                 
343600        IF WS-KDERS = '03' OR '06'                                        
343700           MOVE 1              TO ERSA13-KDSTATUS-C1                      
343800           MOVE MID-TIERSDAT-PREL TO ERSA13-TIERSDAT-PREL-C1              
343900        ELSE                                                              
344000           IF WS-KDERS = '07' OR '08'                                     
344100              MOVE 6           TO ERSA13-KDSTATUS-C1                      
344200              MOVE ZERO        TO ERSA13-TIERSDAT-PREL-C1                 
344300           ELSE                                                           
344400              IF WS-KDERS = '09'                                          
344500                 MOVE 3        TO ERSA13-KDSTATUS-C1                      
344600                 MOVE ZERO     TO ERSA13-TIERSDAT-PREL-C1                 
344700              ELSE                                                        
344800                 MOVE ZERO     TO ERSA13-KDSTATUS-C1                      
344900                                  ERSA13-TIERSDAT-PREL-C1                 
345000              END-IF                                                      
345100           END-IF                                                         
345200        END-IF                                                            
345300     END-IF                                                               
345400                                                                          
345500     MOVE ZERO                    TO ERSA13-KDSTATUS-C2                   
345600                                     ERSA13-TIERSDAT-PREL-C2              
345700                                                                          
345800     MOVE 'IDAG  '    TO DAT-KDDATFORM                                    
345900     CALL WDATKONV USING DAT-KDDATFORM                                    
346000                         DAT-I-TIDATUM                                    
346100                         DAT-O-TIDATUM                                    
346200                         DAT-KDSVAR                                       
346300     MOVE DAT-TIAAVVD TO ERSA13-TIERSDAT-REG                              
346400     .                                                                    
346500     EJECT                                                                
346600 S21-SKAPA-KDP-TRANS SECTION.                                             
346700     SKIP2                                                                
346800     MOVE WS-KDPRODSL            TO TEST-KDPRODSL                         
346900     IF KDPRODSL-VCBV                                                     
347000        CONTINUE                                                          
347100     ELSE                                                                 
347200        MOVE LOW-VALUE       TO IO-AREA                                   
347300        ACCEPT ZZAC-TIKLOCK  FROM TIME                                    
347400        ACCEPT ZZAC-TIAAMMDD FROM DATE                                    
347500        ADD +1               TO W-IDLOGLOP                                
347600        MOVE W-IDLOGLOP      TO ZZAC-IDLOGLOP                             
347700        MOVE 'RZU'           TO KDP-IDPTYP                                
347800        MOVE IDARTNR-WS      TO KDP-IDARTNR                               
347900        MOVE IDARTNR-WS      TO W092-SORTBGP                              
348000        MOVE KDP-W10111      TO ZZAC-LOGGPOST                             
348100        MOVE W092-AREA       TO ZZAC-SORTPOST                             
348200        PERFORM IMS-ISRT-ZZAC                                             
348300     END-IF                                                               
348400     .                                                                    
348500     EJECT                                                                
348600 S22-ROER-EJ-FAELT SECTION.                                               
348700     SKIP2                                                                
348800     MOVE MFS-STAENG-FAELT  TO MOD-DIERS-ERS-ATTR                         
348900                               MOD-IDAO-ATTR                              
349000                               MOD-TIERSDAT-PREL-ATTR                     
349100                               MOD-TEARTNOT-ATTR                          
349200                               MOD-FLKLAR-ATTR                            
349300                               MOD-KDERS-ATTR                             
349400     MOVE +1 TO RAD-IX                                                    
349500     PERFORM UNTIL RAD-IX > MAX-RAD                                       
349600        MOVE MFS-STAENG-FAELT  TO MOD-IDARTNR-TILLK-ATTR                  
349700                                  (RAD-IX)                                
349800        MOVE MFS-STAENG-FAELT  TO MOD-DIERS-TILLK-ATTR(RAD-IX)            
349900                                  MOD-BEERS-ATTR(RAD-IX)                  
350000                                  MOD-IDKORTNR-ATTR(RAD-IX)               
350100        ADD +1 TO RAD-IX                                                  
350200     END-PERFORM                                                          
350300     .                                                                    
350400     EJECT                                                                
350500 S23-TRANS-TIKO-RSLISTA SECTION.                                          
350600     SKIP2                                                                
350700     MOVE NEJ TO FL-NYPONART                                              
350800                                                                          
350900     MOVE IDARTNR-WS TO W-IDARTNR                                         
351000     PERFORM IMS-GET-ARTG01                                               
351100     IF SEGMENT-FINNS                                                     
351200        MOVE JA TO FL-NYPONART                                            
351300        IF WS-KDERS = '00'                                                
351400           IF ERSATT-PRARTSTD = ZERO                                      
351500              IF ERSATT-KDBPSR = 8 AND ERSATT-FLLSRDEL = NEJ              
351600                 CONTINUE                                                 
351700              ELSE                                                        
351800                 MOVE '1' TO ARTG-ART-KDANSKQ                             
351900              END-IF                                                      
352000           END-IF                                                         
352100           MOVE 'R' TO ARTG-ART-KDRESBED                                  
352200           PERFORM IMS-REPL-ARTG                                          
352300        ELSE                                                              
352400           IF WS-KDERS = '09' OR '19' OR '29' OR '52'                     
352500              MOVE 'U' TO ARTG-ART-KDRESBED                               
352600           ELSE                                                           
352700              MOVE 'E' TO ARTG-ART-KDRESBED                               
352800           END-IF                                                         
352900           MOVE ARTG-ART-KDANSKQ TO SPAR-KDANSKQ                          
353000                                                                          
353100           IF WS-KDERS > 20                                               
353200             IF ARTG-ART-KDANSKQ NOT = 9                                  
353300               MOVE +0 TO ARTG-ART-KDANSKQ                                
353400             END-IF                                                       
353500           END-IF                                                         
353600                                                                          
353700           IF WS-KDERS > 0 AND WS-KDERS < 20                              
353800           OR WS-KDERS = 52                                               
353900              IF SPAR-KDANSKQ = '2' OR '4'                                
354000               MOVE '0' TO ARTG-ART-KDANSKQ                               
354100               MOVE 0 TO ARTG-ART-KVPROG                                  
354200              END-IF                                                      
354300           END-IF                                                         
354400                                                                          
354500           PERFORM IMS-REPL-ARTG                                          
354600                                                                          
354700           PERFORM IMS-GET-ARTC01                                         
354800           MOVE ART-KDPRODSL   TO WS-KDPRODSL                             
354900                                  TEST-KDPRODSL                           
355000           PERFORM IMS-GET-ARTC11                                         
355100           MOVE CLAG-KDBPSR    TO WS-KDBPSR                               
355200*          *********************************************                  
355300*          ********** KDPRODSL-PV-SKALL-KÖPA ***********                  
355400*          *********************************************                  
355500           IF KDPRODSL-UTAN-EMB                                           
355600           OR KDPRODSL-LOCAL                                              
355700              IF SPAR-IDPROJ = '9501' OR '9502'                           
355800              OR ERSATT-KDAVT > 0                                         
355900                 CONTINUE                                                 
356000              ELSE                                                        
356100                 IF ARTG-ART-IDPROJK = SPACE                              
356200                    CONTINUE                                              
356300                 ELSE                                                     
356400                    MOVE JA TO SW-TIKO                                    
356500                    IF SPAR-KDANSKQ = '2'                                 
356600                    AND ARTG-ART-KVLEVBEG > 0                             
356700                       CONTINUE                                           
356800                    ELSE                                                  
356900                       IF SPAR-KDANSKQ = '4'                              
357000                       AND ARTG-ART-KVLEVBEG > 0                          
357100                          MOVE '4'        TO W-1142-KEY-X                 
357200                          PERFORM IMS-GHU-XXAV11-GE                       
357300                          IF SEGMENT-FINNS                                
357400                             PERFORM IMS-DLET-XXAV11                      
357500                          END-IF                                          
357600                       END-IF                                             
357700                       MOVE NEJ TO SW-TIKO                                
357800                    END-IF                                                
357900                                                                          
358000                    IF SW-TIKO = JA                                       
358100                      MOVE NEJ      TO SW-TIKO                            
358200                      PERFORM IMS-GU-XXAV01                               
358300                      MOVE '1'        TO W-1142-KEY-X                     
358400                      PERFORM IMS-GNP-XXAV11                              
358500                                                                          
358600                      PERFORM UNTIL SW-TIKO = JA                          
358700                         OR SEGMENT-SAKNAS                                
358800                            IF IDARTNR-WS = XXAV-1142-IDARTNR             
358900                               MOVE JA     TO SW-TIKO                     
359000                            ELSE                                          
359100                               PERFORM IMS-GNP-XXAV11                     
359200                            END-IF                                        
359300                      END-PERFORM                                         
359400                                                                          
359500                      IF SW-TIKO = NEJ                                    
359600                         MOVE SPACE   TO XXAV-1142-WDGX1142               
359700                         MOVE +1      TO XXAV-1142-KDSEGKEY               
359800                         MOVE IDARTNR-WS TO                               
359900                                            XXAV-1142-IDARTNR             
360000                         PERFORM IMS-ISRT-XXAV                            
360100                      END-IF                                              
360200                    END-IF                                                
360300                 END-IF                                                   
360400              END-IF                                                      
360500           END-IF                                                         
360600        END-IF                                                            
360700     END-IF                                                               
360800     .                                                                    
360900     EJECT                                                                
361000  S24-TRANS-TILL-BASL SECTION.                                            
361100*****************************************************************         
361200*  ÄT NOV 92  TRANS 1158 TILL ERSÄTTNINGSBEVAKNING BASLAGER     *         
361300*             VID NY EK > 10. TRANSEN LÄSES OCH DELEATAS I      *         
361400*             I W115D1.                                         *         
361500*****************************************************************         
361600     SKIP2                                                                
361700     IF FL-NYPONART = JA                                                  
361800        IF ERSATT-KDERS-C1 = 00 OR WS-KDERS = 0                           
361900           MOVE IDARTNR-WS      TO W-1116-IDARTNR                         
362000           PERFORM IMS-GET-XXAW11-GHU                                     
362100           IF SEGMENT-FINNS                                               
362200              MOVE ERSATT-KDERS-C1 TO XXAW-1116-KDERS-OLD                 
362300              MOVE WS-KDERS        TO XXAW-1116-KDERS-NEW                 
362400              MOVE DAGENS-DATUM    TO XXAW-1116-TIREGDAT                  
362500              PERFORM IMS-REPL-XXAW                                       
362600           ELSE                                                           
362700              MOVE LOW-VALUE       TO IO-AREA                             
362800              MOVE IDARTNR-WS      TO XXAW-1116-IDARTNR                   
362900              MOVE ERSATT-KDERS-C1 TO XXAW-1116-KDERS-OLD                 
363000              MOVE WS-KDERS        TO XXAW-1116-KDERS-NEW                 
363100              MOVE DAGENS-DATUM    TO XXAW-1116-TIREGDAT                  
363200              PERFORM IMS-ISRT-XXAW                                       
363300           END-IF                                                         
363400        END-IF                                                            
363500                                                                          
363600        IF (ERSATT-KDERS-C1 < 10 AND WS-KDERS > 10) OR                    
363700           (ERSATT-KDERS-C1 > 10 AND WS-KDERS = ZERO)                     
363800            MOVE IDARTNR-WS TO W-1158-IDARTNR                             
363900            PERFORM IMS-GET-XXCW11                                        
364000            IF SEGMENT-FINNS                                              
364100               IF WS-KDERS > 10                                           
364200                  MOVE WS-KDERS      TO XXCW-1158-KDERS                   
364300                  MOVE DAGENS-DATUM  TO XXCW-1158-TIREGDAT                
364400                  PERFORM IMS-REPL-XXCW                                   
364500               ELSE                                                       
364600                  PERFORM IMS-DLET-XXCW                                   
364700               END-IF                                                     
364800            ELSE                                                          
364900               IF WS-KDERS > 10                                           
365000                  MOVE LOW-VALUE TO IO-AREA                               
365100                  MOVE IDARTNR-WS    TO XXCW-1158-IDARTNR                 
365200                  MOVE WS-KDERS      TO XXCW-1158-KDERS                   
365300                  MOVE DAGENS-DATUM  TO XXCW-1158-TIREGDAT                
365400                  PERFORM IMS-ISRT-XXCW                                   
365500               END-IF                                                     
365600            END-IF                                                        
365700         END-IF                                                           
365800     END-IF                                                               
365900     .                                                                    
366000     EJECT                                                                
366100 S25-TRANS-TILL-PPMS SECTION.                                             
366200     SKIP2                                                                
366300     MOVE NEJ TO PPMS-TRANS                                               
366400                                                                          
366500     EVALUATE TRUE                                                        
366600     WHEN ERSATT-KDERS-C1 < 10 AND WS-KDERS > 10                          
366700        MOVE JA TO PPMS-TRANS                                             
366800     WHEN ERSATT-KDERS-C1 > 10 AND WS-KDERS < 10                          
366900        MOVE JA TO PPMS-TRANS                                             
367000     WHEN ERSATT-KDERS-C1 > 10 AND WS-KDERS > 10                          
367100        MOVE JA TO PPMS-TRANS                                             
367200     WHEN OTHER                                                           
367300        CONTINUE                                                          
367400     END-EVALUATE                                                         
367500                                                                          
367600     IF PPMS-TRANS = JA                                                   
367700        MOVE SPACE              TO PPMS-W111PPMS                          
367800        MOVE IDARTNR-WS         TO PPMS-IDARTNR                           
367900        IF WS-KDERS > 10                                                  
368000           MOVE WS-KDERS        TO PPMS-KDERS                             
368100        ELSE                                                              
368200           MOVE ZERO            TO PPMS-KDERS                             
368300        END-IF                                                            
368400                                                                          
368500        EVALUATE TRUE                                                     
368600        WHEN WS-KDERS = ZERO                                              
368700           CONTINUE                                                       
368800        WHEN WS-KDERS = 09 OR 19 OR 29 OR 52                              
368900           CONTINUE                                                       
369000        WHEN WS-KDERS-2 =  4 OR 5 OR 6 OR 8                               
369100           MOVE 'FLERA'         TO PPMS-ANMARKNING                        
369200        WHEN OTHER                                                        
369300           MOVE IDARTNR-WS TO W-IDARTNR                                   
369400           PERFORM IMS-GET-ERSA01                                         
369500           IF ERSA01-KVKORT > 1                                           
369600              MOVE 'FLERA'          TO PPMS-ANMARKNING                    
369700           ELSE                                                           
369800              PERFORM IMS-GET-ERSA11                                      
369900              MOVE ERSA11-IDARTNR-TILLK TO                                
370000                      PPMS-IDARTNR-TILLK-WS                               
370100              MOVE PPMS-IDARTNR-TILLK-WS TO                               
370200                    WS-PPMS-IDARTNR-TILLK                                 
370300              INSPECT WS-PPMS-IDARTNR-TILLK TALLYING                      
370400                    NOLL-RAKNARE FOR LEADING ZERO                         
370500              ADD +1 TO NOLL-RAKNARE                                      
370600              UNSTRING WS-PPMS-IDARTNR-TILLK INTO                         
370700                   PPMS-ANMARKNING WITH POINTER                           
370800                   NOLL-RAKNARE                                           
370900           END-IF                                                         
371000        END-EVALUATE                                                      
371100                                                                          
371200       MOVE 'RPP'       TO PPMS-IDPTYP                                    
371300                                                                          
371400       MOVE SPACE                  TO FILC-AREA                           
371500       MOVE PROGRAM-NAMN           TO FILC-FIL-IDPGM                      
371600       ACCEPT FILC-FIL-TIREGDAT    FROM DATE                              
371700       ACCEPT FILC-FIL-TIKLOCK     FROM TIME                              
371800       ADD +1                      TO WS-IDSEKVNR                         
371900       MOVE WS-IDSEKVNR            TO FILC-FIL-IDSEKVNR                   
372000       MOVE 'W111RPP '             TO FILC-FIL-IDCPYTXT                   
372100       MOVE PPMS-W111PPMS          TO FILC-FIL-WDR301-DATA                
372200                                                                          
372300       PERFORM IMS-ISRT-FILC-TRANS                                        
372400       PERFORM UNTIL SEGMENT-FINNS                                        
372500         ACCEPT FILC-FIL-TIREGDAT    FROM DATE                            
372600         ACCEPT FILC-FIL-TIKLOCK     FROM TIME                            
372700         ADD +1                      TO WS-IDSEKVNR                       
372800         MOVE WS-IDSEKVNR            TO FILC-FIL-IDSEKVNR                 
372900                                                                          
373000         PERFORM IMS-ISRT-FILC-TRANS                                      
373100       END-PERFORM                                                        
373200     END-IF                                                               
373300     .                                                                    
373400     EJECT                                                                
373500 S26-KOLLA-RASA SECTION.                                                  
373600     SKIP2                                                                
373700     MOVE NEJ TO WS-EXTERN-SATS                                           
373800     MOVE IDARTNR-WS TO W-IDARTNR-S                                       
373900     MOVE SPACE TO W-IDLEVNR-S                                            
374000     MOVE SPACE TO W-BELEVART-S                                           
374100     PERFORM IMS-GET-SATB-CSEQ                                            
374200     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
374300        OR WS-EXTERN-SATS = JA                                            
374400        IF SATB-STR-IDLEVNR = SPACE OR LOW-VALUE                          
374500           IF SATB-STR-IDARTNR < 100000000                                
374600             MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                      
374700             MOVE DAGENS-DATUM        TO TMP2-YYMMDD                      
374800             PERFORM WY2000P1                                             
374900             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
375000               MOVE JA TO WS-EXTERN-SATS                                  
375100             END-IF                                                       
375200           END-IF                                                         
375300        END-IF                                                            
375400        PERFORM IMS-GET-SATB-CSEQ                                         
375500     END-PERFORM                                                          
375600     .                                                                    
375700 S27-TRANS-TILL-WDR5 SECTION.                                             
375800     SKIP2                                                                
375900     IF (ERSATT-KDERS-C1 < 10 AND MID-KDERS > 10                          
376000        OR ERSATT-KDERS-C1 > 10 AND MID-KDERS < 10)                       
376100        MOVE W-IDARTNR  TO     2228-IDARTNR                               
376200        MOVE LOW-VALUE  TO     2228-LOW-VALUE                             
376300        MOVE SPACE      TO     2228-FILLER                                
376400        PERFORM IMS-ISRT-2228                                             
376500      END-IF                                                              
376600     .                                                                    
376700     EJECT                                                                
376800 S28-SKAPA-B65-TRANS SECTION.                                             
376900                                                                          
377000     IF WS-KDERS = 52                                                     
377100        IF ERSATT-KDAVT = 1                                               
377200           MOVE IDARTNR-WS TO W-IDARTNR                                   
377300           PERFORM IMS-GET-ARTC01                                         
377400           PERFORM IMS-GET-ARTC23                                         
377500           PERFORM UNTIL SEGMENT-SAKNAS                                   
377600             MOVE AVT-IDAVTAL TO W-IDAVTAL-RED                            
377700             MOVE W-PREFIX TO W-PREFIX-NUM                                
377800                                                                          
377900***             ÄVEN NAP-AVTAL TAS MED, PREFIX = 004                      
378000             IF (W-PREFIX-NUM > 99 AND W-PREFIX-NUM < 790) OR             
378100             (W-PREFIX-NUM > 799 AND W-PREFIX-NUM < 987) OR               
378200             (W-PREFIX-NUM > 987 AND W-PREFIX-NUM < 1000) OR              
378300             (W-PREFIX-NUM = 004)                                         
378400               IF AVT-IDLEVNR-AVT (5:1) = SPACE                           
378500                 MOVE AVT-IDLEVNR-AVT TO IDLEVNR-ALFA                     
378600                 MOVE ZERO TO TALLY                                       
378700                 INSPECT IDLEVNR-ALFA TALLYING TALLY FOR                  
378800                     CHARACTERS BEFORE INITIAL SPACE                      
378900                 IF TALLY = ZERO                                          
379000                    MOVE ZERO TO WS-IDLEVNR-NUM                           
379100                 ELSE                                                     
379200                    MOVE IDLEVNR-ALFA(1:TALLY) TO WS-IDLEVNR-NUM          
379300                 END-IF                                                   
379400                 MOVE WS-IDLEVNR-NUM  TO A310-LEVNUM                      
379500               ELSE                                                       
379600                 MOVE AVT-IDLEVNR-AVT TO A310-LEVNUM                      
379700               END-IF                                                     
379800               MOVE AVT-IDAVTAL     TO W-IDAVTAL-RED                      
379900               MOVE W-PREFIX        TO A310-BESTPREF                      
380000               MOVE W-AVTALSNR      TO A310-BESTLNR                       
380100               MOVE W-SUFFIX        TO A310-BESTSUFF                      
380200               MOVE SPACE           TO A310-LEVNUM-GODSM                  
380300                                       A310-ANT-BESTANN                   
380400               MOVE 'RY2'           TO A310-KT                            
380500               MOVE DAGENS-DATUM    TO A310-DATUM-UTSKR                   
380600               MOVE IDARTNR-WS      TO W-IDARTNR-8                        
380700               MOVE W-IDARTNR-8     TO A310-ARTNR                         
380800                                       W092-SORTBGP                       
380900                                                                          
381000               ACCEPT ZZAC-TIKLOCK  FROM TIME                             
381100               ACCEPT ZZAC-TIAAMMDD FROM DATE                             
381200               ADD +1 TO W-IDLOGLOP                                       
381300               MOVE W-IDLOGLOP     TO ZZAC-IDLOGLOP                       
381400               MOVE A310-A310B65   TO ZZAC-LOGGPOST                       
381500               MOVE W092-AREA      TO ZZAC-SORTPOST                       
381600               PERFORM IMS-ISRT-ZZAC                                      
381700                                                                          
381800             ELSE                                                         
381900               IF (W-PREFIX-NUM > 639 AND W-PREFIX-NUM < 660)             
382000                 MOVE SPACE      TO XXAV-1142-WDGX1142                    
382100                 MOVE IDARTNR-WS TO XXAV-1142-IDARTNR                     
382200                 MOVE +2         TO XXAV-1142-KDSEGKEY                    
382300                 MOVE 'A'        TO XXAV-1142-KDSVAR                      
382400                 PERFORM IMS-ISRT-XXAV                                    
382500              END-IF                                                      
382600           END-IF                                                         
382700           PERFORM IMS-GET-ARTC23                                         
382800         END-PERFORM                                                      
382900       END-IF                                                             
383000     END-IF                                                               
383100     .                                                                    
383200     EJECT                                                                
383300 S29-ALARM-KDERS-UPD SECTION.                                             
383400*    CREATE ALARM WHEN SS CODE IS UPDATED TO ZERO                         
383500     MOVE WS-IDANSK-ALARM     TO W-IDANSK-2232                            
383600     PERFORM IMS-GU-R220                                                  
383700     IF SEGMENT-FINNS                                                     
383800       MOVE WDR220-2232-IDANSK-LARM                                       
383900                              TO W-IDANSK-2223                            
384000     ELSE                                                                 
384100       MOVE ZERO              TO W-IDANSK-2223                            
384200     END-IF                                                               
384300     MOVE '2223'              TO WDR501-2223-IDHTYP                       
384400     MOVE W-IDANSK-2223       TO WDR501-2223-IDANSK                       
384500     MOVE LOW-VALUE           TO WDR501-2223-LOW-VALUE                    
384600     PERFORM IMS-ISRT-R501                                                
384700     PERFORM IMS-GHU-R501                                                 
384800     MOVE FUNCTION CURRENT-DATE(3:6)                                      
384900                              TO WDR550-2224-TISENBEK-DAG                 
385000     MOVE FUNCTION CURRENT-DATE(11:6)                                     
385100                              TO WDR550-2224-TISENBEK-KL                  
385200     MOVE 610                 TO WDR550-2224-KDLARM                       
385300     MOVE W-IDARTNR           TO WDR550-2224-IDARTNR                      
385400     MOVE WS-IDDC-ALARM       TO WDR550-2224-IDDC                         
385500     MOVE JA                  TO WDR550-2224-FLNYLARM                     
385600     MOVE ZERO                TO WDR550-2224-IDDISTR                      
385700                                 WDR550-2224-IDKUNDNR                     
385800     MOVE '0000000   '        TO WDR550-2224-IDKUNDRF                     
385900     MOVE 1                   TO WDR550-2224-IDLOPNR                      
386000     MOVE DAGENS-DATUM        TO WDR550-2224-TIREGDAT                     
386100     MOVE SPACE               TO WDR550-2224-IDTRANS                      
386200                                 WDR550-2224-KDMFSFOR                     
386300     MOVE ZERO                TO WDR550-2224-IDKR                         
386400     MOVE SPACE               TO WDR550-2224-IDLEVNR                      
386500                                                                          
386600     PERFORM IMS-ISRT-R550                                                
386700*                                                                         
386800*    IF PART LOCALLY PROCURED IN CN OR US, CREATE ALARM                   
386900     PERFORM S29A-CHK-PART-SOURCING                                       
387000     .                                                                    
387100     EJECT                                                                
387200 S29A-CHK-PART-SOURCING SECTION.                                          
387300                                                                          
387400     PERFORM IMS-GU-WDK701                                                
387500     IF SEGMENT-FINNS                                                     
387600        PERFORM IMS-GN-WDK711                                             
387700        IF SEGMENT-FINNS                                                  
387800          PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                      
387900*           CHECK IF PART IS PROCURED IN US OR CHINA                      
388000            IF SLAG-IDDC-REF = SPACES                                     
388100               MOVE SLAG-IDDC                TO W-IDDC                    
388200               PERFORM IMS-GU-WDB601                                      
388300               IF SEGMENT-FINNS                                           
388400                  IF DCS-NDC-NA OR DCS-NDC-CN                             
388500                     MOVE ZERO               TO WS-IDANSK-ALARM           
388600                     MOVE SLAG-IDDC          TO WS-IDDC-ALARM             
388700                     PERFORM IMS-GNP-WDK722                               
388800                     IF SEGMENT-FINNS                                     
388900                        MOVE XLAG-IDANSK     TO WS-IDANSK-ALARM           
389000                     END-IF                                               
389100                     PERFORM S29AA-CREATE-SS-ALARM-LOCAL                  
389200                  END-IF                                                  
389300               END-IF                                                     
389400            END-IF                                                        
389500            PERFORM IMS-GN-WDK711                                         
389600          END-PERFORM                                                     
389700        END-IF                                                            
389800     END-IF                                                               
389900     .                                                                    
390000     EJECT                                                                
390100 S29AA-CREATE-SS-ALARM-LOCAL  SECTION.                                    
390200                                                                          
390300     MOVE WS-IDANSK-ALARM     TO W-IDANSK-2232                            
390400     PERFORM IMS-GU-R220                                                  
390500     IF SEGMENT-FINNS                                                     
390600       MOVE WDR220-2232-IDANSK-LARM                                       
390700                              TO W-IDANSK-2223                            
390800     ELSE                                                                 
390900       MOVE ZERO              TO W-IDANSK-2223                            
391000     END-IF                                                               
391100     MOVE '2223'              TO WDR501-2223-IDHTYP                       
391200     MOVE W-IDANSK-2223       TO WDR501-2223-IDANSK                       
391300     MOVE LOW-VALUE           TO WDR501-2223-LOW-VALUE                    
391400     PERFORM IMS-ISRT-R501                                                
391500     PERFORM IMS-GHU-R501                                                 
391600     MOVE FUNCTION CURRENT-DATE(3:6)                                      
391700                              TO WDR550-2224-TISENBEK-DAG                 
391800     MOVE FUNCTION CURRENT-DATE(11:6)                                     
391900                              TO WDR550-2224-TISENBEK-KL                  
392000     MOVE 610                 TO WDR550-2224-KDLARM                       
392100     MOVE W-IDARTNR           TO WDR550-2224-IDARTNR                      
392200     MOVE WS-IDDC-ALARM       TO WDR550-2224-IDDC                         
392300     MOVE JA                  TO WDR550-2224-FLNYLARM                     
392400     MOVE ZERO                TO WDR550-2224-IDDISTR                      
392500                                 WDR550-2224-IDKUNDNR                     
392600     MOVE '0000000   '        TO WDR550-2224-IDKUNDRF                     
392700     MOVE 1                   TO WDR550-2224-IDLOPNR                      
392800     MOVE DAGENS-DATUM        TO WDR550-2224-TIREGDAT                     
392900     MOVE SPACE               TO WDR550-2224-IDTRANS                      
393000                                 WDR550-2224-KDMFSFOR                     
393100     MOVE ZERO                TO WDR550-2224-IDKR                         
393200     MOVE SPACE               TO WDR550-2224-IDLEVNR                      
393300                                                                          
393400     PERFORM IMS-ISRT-R550                                                
393500     .                                                                    
393600     EJECT                                                                
393700*** IMS SEKTIONER                                                         
393800     SKIP3                                                                
393900 IMS-GET-MSG SECTION.                                                     
394000     SKIP2                                                                
394100     MOVE '  QC' TO GODK-STATUSKODER                                      
394200     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
394300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
394400     PERFORM IMS-STATUS-KONTROLL                                          
394500     .                                                                    
394600     SKIP3                                                                
394700 IMS-INSERT-MSG SECTION.                                                  
394800     SKIP2                                                                
394900     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
395000     MOVE SPACE TO GODK-STATUSKODER                                       
395100     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA MFS-IDMOD                
395200     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
395300     PERFORM IMS-STATUS-KONTROLL                                          
395400     .                                                                    
395500     EJECT                                                                
395600 IMS-GET-WMSGKOM SECTION.                                                 
395700     SKIP2                                                                
395800     MOVE '  QD' TO GODK-STATUSKODER                                      
395900     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
396000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
396100     PERFORM IMS-STATUS-KONTROLL                                          
396200     .                                                                    
396300     SKIP3                                                                
396400 IMS-INSERT-WMSGKOM SECTION.                                              
396500     SKIP2                                                                
396600     MOVE '  ' TO GODK-STATUSKODER                                        
396700     CALL CBLTDLI USING ISRT MSGKOM-PCB MSG-KOM-WMSGKOM                   
396800     MOVE MSGKOM-STATUS-CODE TO STATUS-WS                                 
396900     PERFORM IMS-STATUS-KONTROLL                                          
397000     .                                                                    
397100     EJECT                                                                
397200 IMS-GET-ARTC01 SECTION.                                                  
397300     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
397400            DELIMITED BY SIZE INTO SSA1                                   
397500     MOVE '  GE' TO GODK-STATUSKODER                                      
397600     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1                     
397700     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
397800     PERFORM IMS-STATUS-KONTROLL                                          
397900     .                                                                    
398000     SKIP3                                                                
398100 IMS-GET-ARTC11 SECTION.                                                  
398200     MOVE  'WLARTC11 ' TO  SSA1                                           
398300     MOVE '  GE' TO GODK-STATUSKODER                                      
398400     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
398500     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
398600     PERFORM IMS-STATUS-KONTROLL                                          
398700     .                                                                    
398800     EJECT                                                                
398900 IMS-GET-ARTC23 SECTION.                                                  
399000     MOVE  'WLARTC11 ' TO  SSA1                                           
399100     MOVE  'WLARTC23 ' TO  SSA2                                           
399200     MOVE '  GE' TO GODK-STATUSKODER                                      
399300     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                
399400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
399500     PERFORM IMS-STATUS-KONTROLL                                          
399600     .                                                                    
399700     SKIP2                                                                
399800 IMS-REPL-ARTC SECTION.                                                   
399900     MOVE '  ' TO GODK-STATUSKODER                                        
400000     CALL CBLTDLI USING REPL    ARTC-PCB DLI-IO-AREA                      
400100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
400200     PERFORM IMS-STATUS-KONTROLL                                          
400300     .                                                                    
400400     EJECT                                                                
400500 IMS-GET-ERSB01 SECTION.                                                  
400600     STRING 'WLERSB01(WDD7A1KY >' W-IDARTNR-X                             
400700                                  W-IDARTNR-ERS-LOW-X                     
400800                                  W-IDKORTNR-LOW-X                        
400900                    '&WDD7A1KY <' W-IDARTNR-X                             
401000                                  W-IDARTNR-ERS-HIGH-X                    
401100                                  W-IDKORTNR-HIGH-X                       
401200                            ')'                                           
401300            DELIMITED BY SIZE INTO SSA1                                   
401400     MOVE '  GE' TO GODK-STATUSKODER                                      
401500     CALL CBLTDLI USING GU ERSB-PCB DLI-IO-AREA SSA1                      
401600     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
401700     PERFORM IMS-STATUS-KONTROLL                                          
401800     .                                                                    
401900     EJECT                                                                
402000 IMS-GET-ERSA01 SECTION.                                                  
402100     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
402200            DELIMITED BY SIZE INTO SSA1                                   
402300     MOVE '  GE' TO GODK-STATUSKODER                                      
402400     CALL CBLTDLI USING GHU ERSA-PCB DLI-IO-AREA SSA1                     
402500     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
402600     PERFORM IMS-STATUS-KONTROLL                                          
402700     .                                                                    
402800     SKIP3                                                                
402900 IMS-GET-ERSA11 SECTION.                                                  
403000     MOVE 'WLERSA11 ' TO SSA1                                             
403100     MOVE '  GE' TO GODK-STATUSKODER                                      
403200     CALL CBLTDLI USING GHNP ERSA-PCB DLI-IO-AREA SSA1                    
403300     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
403400     PERFORM IMS-STATUS-KONTROLL                                          
403500     .                                                                    
403600     SKIP3                                                                
403700 IMS-GNP-ERSA11 SECTION.                                                  
403800     STRING 'WLERSA11(IDKORTNR>=' W-IDKORTNR-X ')'                        
403900            DELIMITED BY SIZE INTO SSA1                                   
404000     MOVE '  GE' TO GODK-STATUSKODER                                      
404100     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
404200     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
404300     PERFORM IMS-STATUS-KONTROLL                                          
404400     .                                                                    
404500     SKIP3                                                                
404600 IMS-GET-ERSA13 SECTION.                                                  
404700     MOVE 'WLERSA13 ' TO SSA1                                             
404800     MOVE '  GE' TO GODK-STATUSKODER                                      
404900     CALL CBLTDLI USING GHNP ERSA-PCB DLI-IO-AREA SSA1                    
405000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
405100     PERFORM IMS-STATUS-KONTROLL                                          
405200     .                                                                    
405300     EJECT                                                                
405400 IMS-ISRT-ERSA01 SECTION.                                                 
405500     MOVE 'WLERSA01' TO SSA1                                              
405600     MOVE '  ' TO GODK-STATUSKODER                                        
405700     CALL CBLTDLI USING ISRT ERSA-PCB DLI-IO-AREA SSA1                    
405800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
405900     PERFORM IMS-STATUS-KONTROLL                                          
406000     .                                                                    
406100     SKIP3                                                                
406200 IMS-ISRT-ERSA11 SECTION.                                                 
406300     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
406400            DELIMITED BY SIZE INTO SSA1                                   
406500     MOVE  'WLERSA11'  TO SSA2                                            
406600     MOVE '  ' TO GODK-STATUSKODER                                        
406700     CALL CBLTDLI USING ISRT ERSA-PCB DLI-IO-AREA SSA1 SSA2               
406800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
406900     PERFORM IMS-STATUS-KONTROLL                                          
407000     .                                                                    
407100     SKIP3                                                                
407200 IMS-ISRT-ERSA13 SECTION.                                                 
407300     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
407400            DELIMITED BY SIZE INTO SSA1                                   
407500     MOVE  'WLERSA13'  TO SSA2                                            
407600     MOVE '  ' TO GODK-STATUSKODER                                        
407700     CALL CBLTDLI USING ISRT ERSA-PCB DLI-IO-AREA SSA1 SSA2               
407800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
407900     PERFORM IMS-STATUS-KONTROLL                                          
408000     .                                                                    
408100     EJECT                                                                
408200 IMS-DLET-ERSA SECTION.                                                   
408300     MOVE '  ' TO GODK-STATUSKODER                                        
408400     CALL CBLTDLI USING DLET    ERSA-PCB DLI-IO-AREA                      
408500     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
408600     PERFORM IMS-STATUS-KONTROLL                                          
408700     .                                                                    
408800     SKIP3                                                                
408900 IMS-REPL-ERSA SECTION.                                                   
409000     MOVE '  ' TO GODK-STATUSKODER                                        
409100     CALL CBLTDLI USING REPL ERSA-PCB DLI-IO-AREA                         
409200     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
409300     PERFORM IMS-STATUS-KONTROLL                                          
409400     .                                                                    
409500     SKIP3                                                                
409600 IMS-GET-INLB01 SECTION.                                                  
409700     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
409800            DELIMITED BY SIZE INTO SSA1                                   
409900     MOVE '  GE' TO GODK-STATUSKODER                                      
410000     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1                      
410100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
410200     PERFORM IMS-STATUS-KONTROLL                                          
410300     .                                                                    
410400     SKIP3                                                                
410500 IMS-GET-INLB11 SECTION.                                                  
410600     MOVE 'WLINLB11*F ' TO SSA1                                           
410700     MOVE '  GE' TO GODK-STATUSKODER                                      
410800     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
410900     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
411000     PERFORM IMS-STATUS-KONTROLL                                          
411100     .                                                                    
411200     EJECT                                                                
411300 IMS-GET-INLB11-NAESTA SECTION.                                           
411400     MOVE 'WLINLB11 ' TO SSA1                                             
411500     MOVE '  GE' TO GODK-STATUSKODER                                      
411600     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
411700     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
411800     PERFORM IMS-STATUS-KONTROLL                                          
411900     .                                                                    
412000     SKIP3                                                                
412100 IMS-GET-XXAN01 SECTION.                                                  
412200     STRING 'WLXXAN01(WDGXKEY  =' W-1111-KEY-X ')'                        
412300             DELIMITED BY SIZE INTO SSA1                                  
412400     MOVE 'GE   ' TO GODK-STATUSKODER                                     
412500     CALL CBLTDLI USING GHU XXAN-PCB DLI-IO-AREA-2 SSA1                   
412600     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
412700     PERFORM IMS-STATUS-KONTROLL                                          
412800     .                                                                    
412900     SKIP3                                                                
413000 IMS-GET-XXAN11 SECTION.                                                  
413100     STRING 'WLXXAN11(WDGXKEY  =' W-1112-KEY-X ')'                        
413200             DELIMITED BY SIZE INTO SSA1                                  
413300     MOVE 'GE   ' TO GODK-STATUSKODER                                     
413400     CALL CBLTDLI USING GNP XXAN-PCB DLI-IO-AREA-2 SSA1                   
413500     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
413600     PERFORM IMS-STATUS-KONTROLL                                          
413700     .                                                                    
413800     EJECT                                                                
413900 IMS-GU-XXAN11 SECTION.                                                   
414000     STRING 'WLXXAN01(WDGXKEY  =' W-1111-KEY-X ')'                        
414100             DELIMITED BY SIZE INTO SSA1                                  
414200     STRING 'WLXXAN11(WDGXKEY  =' W-1112-KEY-X ')'                        
414300             DELIMITED BY SIZE INTO SSA2                                  
414400     MOVE 'GE   ' TO GODK-STATUSKODER                                     
414500     CALL CBLTDLI USING GHU XXAN-PCB DLI-IO-AREA-2 SSA1 SSA2              
414600     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
414700     PERFORM IMS-STATUS-KONTROLL                                          
414800     .                                                                    
414900     SKIP3                                                                
415000 IMS-GNP-XXAN21 SECTION.                                                  
415100     STRING 'WLXXAN21(WDGXKEY  =' W-1114-KEY-X ')'                        
415200             DELIMITED BY SIZE INTO SSA1                                  
415300     MOVE 'GE   ' TO GODK-STATUSKODER                                     
415400     CALL CBLTDLI USING GNP XXAN-PCB DLI-IO-AREA-2 SSA1                   
415500     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
415600     PERFORM IMS-STATUS-KONTROLL                                          
415700     .                                                                    
415800     SKIP3                                                                
415900 IMS-GHU-XXAN21 SECTION.                                                  
416000     STRING 'WLXXAN01(WDGXKEY  =' W-1111-KEY-X ')'                        
416100             DELIMITED BY SIZE INTO SSA1                                  
416200     STRING 'WLXXAN11(WDGXKEY  =' W-1112-KEY-X ')'                        
416300             DELIMITED BY SIZE INTO SSA2                                  
416400     STRING 'WLXXAN21(WDGXKEY  =' W-1114-KEY-X ')'                        
416500             DELIMITED BY SIZE INTO SSA3                                  
416600     MOVE 'GE   ' TO GODK-STATUSKODER                                     
416700     CALL CBLTDLI USING GHU XXAN-PCB DLI-IO-AREA-2                        
416800                SSA1 SSA2 SSA3                                            
416900     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
417000     PERFORM IMS-STATUS-KONTROLL                                          
417100     .                                                                    
417200     SKIP3                                                                
417300 IMS-GET-XXAN21 SECTION.                                                  
417400     MOVE 'WLXXAN21 ' TO SSA1                                             
417500     MOVE 'GE   ' TO GODK-STATUSKODER                                     
417600     CALL CBLTDLI USING GNP XXAN-PCB DLI-IO-AREA-2 SSA1                   
417700     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
417800     PERFORM IMS-STATUS-KONTROLL                                          
417900     .                                                                    
418000     SKIP3                                                                
418100 IMS-DLET-XXAN SECTION.                                                   
418200     MOVE '  ' TO GODK-STATUSKODER                                        
418300     CALL CBLTDLI USING DLET XXAN-PCB DLI-IO-AREA-2                       
418400     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
418500     PERFORM IMS-STATUS-KONTROLL                                          
418600     .                                                                    
418700     EJECT                                                                
418800 IMS-REPL-XXAN SECTION.                                                   
418900     MOVE '  ' TO GODK-STATUSKODER                                        
419000     CALL CBLTDLI USING REPL XXAN-PCB DLI-IO-AREA-2                       
419100     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
419200     PERFORM IMS-STATUS-KONTROLL                                          
419300     .                                                                    
419400     SKIP3                                                                
419500 IMS-ISRT-XXAN01 SECTION.                                                 
419600     MOVE 'WLXXAN01 ' TO SSA1                                             
419700     MOVE '  ' TO GODK-STATUSKODER                                        
419800     CALL CBLTDLI USING ISRT XXAN-PCB DLI-IO-AREA-2 SSA1                  
419900     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
420000     PERFORM IMS-STATUS-KONTROLL                                          
420100     .                                                                    
420200     SKIP3                                                                
420300 IMS-ISRT-XXAN11 SECTION.                                                 
420400     STRING 'WLXXAN01(WDGXKEY  =' W-1111-KEY-X ')'                        
420500              DELIMITED BY SIZE INTO SSA1                                 
420600     MOVE 'WLXXAN11 ' TO SSA2                                             
420700     MOVE '  ' TO GODK-STATUSKODER                                        
420800     CALL CBLTDLI USING ISRT XXAN-PCB DLI-IO-AREA-2 SSA1 SSA2             
420900     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
421000     PERFORM IMS-STATUS-KONTROLL                                          
421100     .                                                                    
421200     EJECT                                                                
421300 IMS-ISRT-XXAN21 SECTION.                                                 
421400     STRING 'WLXXAN01(WDGXKEY  =' W-1111-KEY-X ')'                        
421500              DELIMITED BY SIZE INTO SSA1                                 
421600     STRING 'WLXXAN11(WDGXKEY  =' W-1112-KEY-X ')'                        
421700              DELIMITED BY SIZE INTO SSA2                                 
421800     MOVE 'WLXXAN21 ' TO SSA3                                             
421900     MOVE '  ' TO GODK-STATUSKODER                                        
422000     CALL CBLTDLI USING ISRT XXAN-PCB DLI-IO-AREA-2 SSA1                  
422100                               SSA2 SSA3                                  
422200     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
422300     PERFORM IMS-STATUS-KONTROLL                                          
422400     .                                                                    
422500     EJECT                                                                
422600 IMS-ISRT-2304 SECTION.                                                   
422700     STRING 'WDG301  (WDG3KEY  =' W-2303-KEY-X ')'                        
422800              DELIMITED BY SIZE INTO SSA1                                 
422900     MOVE 'WDGX2304*L' TO SSA2                                            
423000     MOVE '  ' TO GODK-STATUSKODER                                        
423100     CALL CBLTDLI USING ISRT 2303-PCB DLI-IO-AREA SSA1 SSA2               
423200     MOVE 2303-STATUS-CODE TO STATUS-WS                                   
423300     PERFORM IMS-STATUS-KONTROLL                                          
423400     .                                                                    
423500     SKIP3                                                                
423600 IMS-ISRT-XXBJ SECTION.                                                   
423700     STRING 'WLXXBJ01(WDG3KEY  =' W-2203-KEY-X ')'                        
423800              DELIMITED BY SIZE INTO SSA1                                 
423900     MOVE 'WLXXBJ11*L' TO SSA2                                            
424000     MOVE '  ' TO GODK-STATUSKODER                                        
424100     CALL CBLTDLI USING ISRT XXBJ-PCB DLI-IO-AREA SSA1 SSA2               
424200     MOVE XXBJ-STATUS-CODE TO STATUS-WS                                   
424300     PERFORM IMS-STATUS-KONTROLL                                          
424400     .                                                                    
424500     EJECT                                                                
424600 IMS-ISRT-XXID SECTION.                                                   
424700     STRING 'WLXXID01(WDG3KEY  =' W-9101-KEY-X ')'                        
424800              DELIMITED BY SIZE INTO SSA1                                 
424900     MOVE 'WLXXID11*L' TO SSA2                                            
425000     MOVE '  ' TO GODK-STATUSKODER                                        
425100     CALL CBLTDLI USING ISRT XXID-PCB DLI-IO-AREA SSA1 SSA2               
425200     MOVE XXID-STATUS-CODE TO STATUS-WS                                   
425300     PERFORM IMS-STATUS-KONTROLL                                          
425400     .                                                                    
425500     SKIP3                                                                
425600 IMS-ISRT-ZZAC SECTION.                                                   
425700     MOVE 'WLZZAC01 ' TO SSA1                                             
425800     MOVE '  ' TO GODK-STATUSKODER                                        
425900     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
426000     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
426100     PERFORM IMS-STATUS-KONTROLL                                          
426200     .                                                                    
426300     EJECT                                                                
426400 IMS-GET-ARTG01 SECTION.                                                  
426500     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
426600             DELIMITED BY SIZE INTO SSA1                                  
426700     MOVE '  GE' TO GODK-STATUSKODER                                      
426800     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA-3 SSA1                   
426900     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
427000     PERFORM IMS-STATUS-KONTROLL                                          
427100     .                                                                    
427200     SKIP3                                                                
427300 IMS-REPL-ARTG SECTION.                                                   
427400     MOVE '  ' TO GODK-STATUSKODER                                        
427500     CALL CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA-3                       
427600     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
427700     PERFORM IMS-STATUS-KONTROLL                                          
427800     .                                                                    
427900     EJECT                                                                
428000 IMS-GU-XXAV01      SECTION.                                              
428100     STRING 'WLXXAV01(WDGXKEY  =' W-1141-KEY-X ')'                        
428200              DELIMITED BY SIZE INTO SSA1                                 
428300     MOVE '  '   TO GODK-STATUSKODER                                      
428400     CALL CBLTDLI USING GU    XXAV-PCB DLI-IO-AREA SSA1                   
428500     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
428600     PERFORM IMS-STATUS-KONTROLL                                          
428700     .                                                                    
428800     SKIP2                                                                
428900 IMS-GNP-XXAV11      SECTION.                                             
429000     STRING 'WLXXAV11(KDSEGKEY =' W-1142-KEY-X ')'                        
429100              DELIMITED BY SIZE INTO SSA1                                 
429200     MOVE '  GE'   TO GODK-STATUSKODER                                    
429300     CALL CBLTDLI USING GNP XXAV-PCB DLI-IO-AREA SSA1                     
429400     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
429500     PERFORM IMS-STATUS-KONTROLL                                          
429600     .                                                                    
429700     SKIP2                                                                
429800 IMS-ISRT-XXAV SECTION.                                                   
429900     STRING 'WLXXAV01(WDGXKEY  =' W-1141-KEY-X ')'                        
430000              DELIMITED BY SIZE INTO SSA1                                 
430100     MOVE 'WLXXAV11 ' TO SSA2                                             
430200     MOVE '  '   TO GODK-STATUSKODER                                      
430300     CALL CBLTDLI USING ISRT XXAV-PCB DLI-IO-AREA SSA1 SSA2               
430400     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
430500     PERFORM IMS-STATUS-KONTROLL                                          
430600     .                                                                    
430700     EJECT                                                                
430800 IMS-ISRT-XXAW SECTION.                                                   
430900     STRING 'WLXXAW01(WDGXKEY  =' W-1115-KEY-X ')'                        
431000              DELIMITED BY SIZE INTO SSA1                                 
431100     MOVE 'WLXXAW11 ' TO SSA2                                             
431200     MOVE '  II' TO GODK-STATUSKODER                                      
431300     CALL CBLTDLI USING ISRT XXAW-PCB DLI-IO-AREA SSA1 SSA2               
431400     MOVE XXAW-STATUS-CODE TO STATUS-WS                                   
431500     PERFORM IMS-STATUS-KONTROLL                                          
431600     .                                                                    
431700     SKIP2                                                                
431800 IMS-GET-XXAW11-GHU  SECTION.                                             
431900     STRING 'WLXXAW01(WDGXKEY  =' W-1115-KEY-X ')'                        
432000              DELIMITED BY SIZE INTO SSA1                                 
432100     STRING 'WLXXAW11(WDGXKEY  =' W-1116-IDARTNR-X ')'                    
432200              DELIMITED BY SIZE INTO SSA2                                 
432300     MOVE '  GE' TO GODK-STATUSKODER                                      
432400     CALL CBLTDLI USING GHU XXAW-PCB DLI-IO-AREA SSA1 SSA2                
432500     MOVE XXAW-STATUS-CODE TO STATUS-WS                                   
432600     PERFORM IMS-STATUS-KONTROLL                                          
432700     .                                                                    
432800     SKIP2                                                                
432900 IMS-REPL-XXAW SECTION.                                                   
433000     MOVE '  ' TO GODK-STATUSKODER                                        
433100     CALL CBLTDLI USING REPL XXAW-PCB DLI-IO-AREA                         
433200     MOVE XXAW-STATUS-CODE TO STATUS-WS                                   
433300     PERFORM IMS-STATUS-KONTROLL                                          
433400     .                                                                    
433500     SKIP2                                                                
433600 IMS-GET-SATB-CSEQ SECTION.                                               
433700     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
433800             DELIMITED BY SIZE INTO SSA1                                  
433900     MOVE 'WLSATB01 ' TO SSA2                                             
434000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
434100     CALL CBLTDLI USING GN SATE-PCB DLI-IO-AREA-5                         
434200                SSA1 SSA2                                                 
434300     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
434400     PERFORM IMS-STATUS-KONTROLL                                          
434500     .                                                                    
434600     SKIP3                                                                
434700 IMS-GET-SATB01 SECTION.                                                  
434800     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
434900            DELIMITED BY SIZE INTO SSA1                                   
435000     MOVE '  ' TO GODK-STATUSKODER                                        
435100     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA-5 SSA1                    
435200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
435300     PERFORM IMS-STATUS-KONTROLL                                          
435400     .                                                                    
435500     SKIP3                                                                
435600 IMS-GET-SATB11 SECTION.                                                  
435700     MOVE 'WLSATB11 ' TO SSA1                                             
435800     MOVE '  GE' TO GODK-STATUSKODER                                      
435900     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA-5 SSA1                  
436000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
436100     PERFORM IMS-STATUS-KONTROLL                                          
436200     .                                                                    
436300 IMS-GET-SATB11-FIRST SECTION.                                            
436400     MOVE 'WLSATB11*F' TO SSA1                                            
436500     MOVE '  GE' TO GODK-STATUSKODER                                      
436600     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA-5 SSA1                  
436700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
436800     PERFORM IMS-STATUS-KONTROLL                                          
436900     .                                                                    
437000 IMS-GHNP-SATB11 SECTION.                                                 
437100     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
437200            DELIMITED BY SIZE INTO SSA1                                   
437300     MOVE '  GE' TO GODK-STATUSKODER                                      
437400     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA-5 SSA1                  
437500     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
437600     PERFORM IMS-STATUS-KONTROLL                                          
437700     .                                                                    
437800 IMS-REPL-SATB SECTION.                                                   
437900     MOVE '  ' TO GODK-STATUSKODER                                        
438000     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA-5                       
438100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
438200     PERFORM IMS-STATUS-KONTROLL                                          
438300     .                                                                    
438400     SKIP3                                                                
438500                                                                          
438600 IMS-ISRT-2228 SECTION.                                                   
438700     STRING 'WLXXBW01(WDGXKEY  =' W-2227KEY-X ')'                         
438800            DELIMITED BY SIZE INTO SSA1                                   
438900     MOVE   'WLXXBW11 ' TO SSA2                                           
439000     MOVE '  II' TO GODK-STATUSKODER                                      
439100     CALL CBLTDLI USING ISRT XXBW-PCB DLI-IO-AREA-6 SSA1 SSA2             
439200     MOVE XXBW-STATUS-CODE TO STATUS-WS                                   
439300     PERFORM IMS-STATUS-KONTROLL                                          
439400     .                                                                    
439500     EJECT                                                                
439600 IMS-GET-XXCW11 SECTION.                                                  
439700     STRING 'WLXXCW01(WDG3KEY  =' W-1157-KEY-X ')'                        
439800              DELIMITED BY SIZE INTO SSA1                                 
439900     STRING 'WLXXCW11(WDGXKEY  =' W-1158-IDARTNR-X ')'                    
440000              DELIMITED BY SIZE INTO SSA2                                 
440100     MOVE '  GE' TO GODK-STATUSKODER                                      
440200     CALL CBLTDLI USING GHU XXCW-PCB DLI-IO-AREA SSA1 SSA2                
440300     MOVE XXCW-STATUS-CODE TO STATUS-WS                                   
440400     PERFORM IMS-STATUS-KONTROLL                                          
440500     .                                                                    
440600     SKIP2                                                                
440700 IMS-ISRT-XXCW SECTION.                                                   
440800     STRING 'WLXXCW01(WDG3KEY  =' W-1157-KEY-X ')'                        
440900              DELIMITED BY SIZE INTO SSA1                                 
441000     MOVE 'WLXXCW11 ' TO SSA2                                             
441100     MOVE '  ' TO GODK-STATUSKODER                                        
441200     CALL CBLTDLI USING ISRT XXCW-PCB DLI-IO-AREA SSA1 SSA2               
441300     MOVE XXCW-STATUS-CODE TO STATUS-WS                                   
441400     PERFORM IMS-STATUS-KONTROLL                                          
441500     .                                                                    
441600     SKIP2                                                                
441700 IMS-REPL-XXCW SECTION.                                                   
441800     MOVE '  ' TO GODK-STATUSKODER                                        
441900     CALL CBLTDLI USING REPL XXCW-PCB DLI-IO-AREA                         
442000     MOVE XXCW-STATUS-CODE TO STATUS-WS                                   
442100     PERFORM IMS-STATUS-KONTROLL                                          
442200     .                                                                    
442300     SKIP2                                                                
442400 IMS-DLET-XXCW SECTION.                                                   
442500     MOVE '  ' TO GODK-STATUSKODER                                        
442600     CALL CBLTDLI USING DLET XXCW-PCB DLI-IO-AREA                         
442700     MOVE XXCW-STATUS-CODE TO STATUS-WS                                   
442800     PERFORM IMS-STATUS-KONTROLL                                          
442900     .                                                                    
443000     EJECT                                                                
443100 IMS-ISRT-FILC-TRANS  SECTION.                                            
443200     MOVE 'WLFILC01 ' TO SSA1                                             
443300     MOVE '  II' TO GODK-STATUSKODER                                      
443400     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-7  SSA1                 
443500     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
443600     PERFORM IMS-STATUS-KONTROLL                                          
443700     .                                                                    
443800     EJECT                                                                
443900 IMS-GHU-XXAV11-GE  SECTION.                                              
444000     STRING 'WLXXAV01(WDGXKEY  =' W-1141-KEY-X ')'                        
444100             DELIMITED BY SIZE INTO SSA1                                  
444200     STRING 'WLXXAV11(KDSEGKEY =' W-1142-KEY-X                            
444300                    '&IDARTNR  =' W-IDARTNR-X ')'                         
444400             DELIMITED BY SIZE INTO SSA2                                  
444500     MOVE '  GE' TO GODK-STATUSKODER                                      
444600     CALL CBLTDLI USING GHU XXAV-PCB DLI-IO-AREA SSA1 SSA2                
444700     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
444800     PERFORM IMS-STATUS-KONTROLL                                          
444900     .                                                                    
445000     SKIP3                                                                
445100 IMS-DLET-XXAV11 SECTION.                                                 
445200     MOVE '  '   TO GODK-STATUSKODER                                      
445300     CALL CBLTDLI USING DLET XXAV-PCB DLI-IO-AREA                         
445400     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
445500     PERFORM IMS-STATUS-KONTROLL                                          
445600     .                                                                    
445700     EJECT                                                                
445800 IMS-GU-WDB601    SECTION.                                                
445900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
446000                    DELIMITED BY SIZE INTO SSA1                           
446100     MOVE '  GE' TO GODK-STATUSKODER                                      
446200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-8 SSA1                    
446300     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
446400     PERFORM IMS-STATUS-KONTROLL                                          
446500     .                                                                    
446600     EJECT                                                                
446700 IMS-GU-WDK701 SECTION.                                                   
446800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
446900          DELIMITED BY SIZE INTO SSA1                                     
447000     MOVE '  GE' TO GODK-STATUSKODER                                      
447100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK701 SSA1               
447200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
447300     PERFORM IMS-STATUS-KONTROLL                                          
447400     .                                                                    
447500     EJECT                                                                
447600 IMS-GN-WDK711 SECTION.                                                   
447700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
447800          DELIMITED BY SIZE INTO SSA1                                     
447900     MOVE   'WDK711   ' TO SSA2                                           
448000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
448100     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-WDK711 SSA1  SSA2         
448200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
448300     PERFORM IMS-STATUS-KONTROLL                                          
448400     .                                                                    
448500     EJECT                                                                
448600 IMS-GNP-WDK722 SECTION.                                                  
448700     MOVE 'WDK722 '     TO SSA1                                           
448800     MOVE '  GE' TO GODK-STATUSKODER                                      
448900     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK722 SSA1              
449000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
449100     PERFORM IMS-STATUS-KONTROLL                                          
449200     .                                                                    
449300     EJECT                                                                
449400 IMS-GU-R220 SECTION.                                                     
449500     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
449600          DELIMITED BY SIZE INTO SSA1                                     
449700     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
449800          DELIMITED BY SIZE INTO SSA2                                     
449900     MOVE '  GE' TO GODK-STATUSKODER                                      
450000     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA-2232 SSA1 SSA2            
450100     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
450200     PERFORM IMS-STATUS-KONTROLL                                          
450300     .                                                                    
450400     EJECT                                                                
450500 IMS-GHU-R501 SECTION.                                                    
450600     STRING 'WDR501  (WDGXKEY  =' W-WDGX2223-X ')'                        
450700          DELIMITED BY SIZE INTO SSA1                                     
450800     MOVE '  GE'           TO GODK-STATUSKODER                            
450900     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-AREA-2223 SSA1                
451000     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
451100     PERFORM IMS-STATUS-KONTROLL                                          
451200     .                                                                    
451300     EJECT                                                                
451400 IMS-ISRT-R501 SECTION.                                                   
451500     MOVE 'WDR501   '      TO SSA1                                        
451600     MOVE '  II'           TO GODK-STATUSKODER                            
451700     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-AREA-2223 SSA1               
451800     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
451900     PERFORM IMS-STATUS-KONTROLL                                          
452000     .                                                                    
452100     EJECT                                                                
452200 IMS-ISRT-R550 SECTION.                                                   
452300     MOVE 'WDR550   '      TO SSA1                                        
452400     MOVE '  II'           TO GODK-STATUSKODER                            
452500     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-AREA-2224 SSA1               
452600     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
452700     PERFORM IMS-STATUS-KONTROLL                                          
452800     .                                                                    
452900     EJECT                                                                
453000 IMS-STATUS-KONTROLL SECTION.                                             
453100     SET STATUS-IX TO 1                                                   
453200     SEARCH GODK-STATUS AT END CALL FELLOG                                
453300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
453400     END-SEARCH.                                                          
453500     EJECT                                                                
453600                                                                          
453700* DB2 SEKTIONER                                                           
453800     SKIP3                                                                
453900                                                                          
454000 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
454100     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
454200                                                                          
454300     MOVE 000100  TO GOOD-SQLCODECODES                                    
454400                                                                          
454500     EXEC SQL                                                             
454600         DECLARE TP1ARTK-CRS CURSOR FOR                                   
454700           SELECT  A.IDKAMP                                               
454800                  ,A.IDARTNR                                              
454900                  ,B.TISTADAT_KAMP                                        
455000                  ,B.TISTODAT_KAMP                                        
455100                  ,B.KDKAMP                                               
455200                                                                          
455300           FROM    TP1ARTK A                                              
455400                  ,TP1KAMP B                                              
455500                                                                          
455600           WHERE   A.IDARTNR = :W-IDARTNR                                 
455700               AND A.IDKAMP  =  B.IDKAMP                                  
455800                                                                          
455900           ORDER BY A.IDARTNR                                             
456000     END-EXEC                                                             
456100                                                                          
456200     MOVE 000100  TO GOOD-SQLCODECODES                                    
456300     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
456400     .                                                                    
456500     SKIP3                                                                
456600 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
456700     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
456800     SKIP2                                                                
456900     MOVE 000100  TO GOOD-SQLCODECODES                                    
457000     EXEC SQL                                                             
457100         FETCH TP1ARTK-CRS INTO                                           
457200                    :TP1KAMP-IDKAMP                                       
457300                   ,:TP1ARTK-IDARTNR                                      
457400                   ,:TP1KAMP-TISTADAT-KAMP                                
457500                   ,:TP1KAMP-TISTODAT-KAMP                                
457600                   ,:TP1KAMP-KDKAMP                                       
457700     END-EXEC                                                             
457800                                                                          
457900     MOVE SQLCODE TO SQLCODE-WS                                           
458000     PERFORM DB2-STATUS-CHECK                                             
458100     .                                                                    
458200     SKIP3                                                                
458300 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
458400     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
458500                                                                          
458600     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
458700     .                                                                    
458800     EJECT                                                                
458900 DB2-STATUS-CHECK  SECTION.                                               
459000                                                                          
459100     SET SQLCODE-IX TO 1                                                  
459200     SEARCH GOOD-SQLCODE                                                  
459300       AT END                                                             
459400*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
459500*         DELIMITED BY SIZE INTO ERROR-TEXT                               
459600          CALL FELLOG                                                     
459700       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
459800     END-SEARCH                                                           
459900     .                                                                    
460000     EJECT                                                                
460100*    -COPY WY2000P3                                                       
460200     EJECT                                                                
460300*    -COPY WY2000P1                                                       
460400     EJECT                                                                
460500*    -COPY WY2000P2                                                       
