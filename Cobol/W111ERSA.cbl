000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W111ERSA.                                                
000400 AUTHOR.         SATHISH T.                                               
000500*DATE-WRITTEN.   DEC 2025.                                                
000600     REMARKS.                                                             
000700*    FUNCTION.                                                            
000800*    SUPERSESSION REGISTRATION & UPDATE.                                  
000900*    UPDATE ADDITIONAL PARTS.                                             
001000*                                                                         
001100*    WILL BE CALLED FROM W10113(CLASSIC) & W11816(TCPLM) .                
001200*                                                                         
001300*    INDATA.                                                              
001400*    . . . . TRANSAKTION: W1T113                                          
001500*                         W1T113U                                         
001600*                         W1T113X                                         
001700*    . . . . . .COPYBOOK: W111ERIN                                        
001800*    UTDATA.                                                              
001900*    . . . . . .COPYBOOK: W111ERUT                                        
002000*    DYNAMISKA SUBPROGRAM.                                                
002100*                         FELLOG                                          
002200*                         CBLTDLI                                         
002300*                         WDECEDIT                                        
002400*                         WDATKONV                                        
002500                                                                          
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP3                                                                
002800 DATA DIVISION.                                                           
002900     EJECT                                                                
003000 WORKING-STORAGE SECTION.                                                 
003100*    -COPY WY2000W2                                                       
003200     SKIP3                                                                
003300*    -COPY WY2000W1                                                       
003400     SKIP3                                                                
003500*    -COPY WY2000W3                                                       
003600     SKIP3                                                                
003700 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W111ERSA'.            
003800 77  JA                          PIC X(1)    VALUE 'J'.                   
003900 77  YES                         PIC X(1)    VALUE 'Y'.                   
004000 77  NEJ                         PIC X(1)    VALUE 'N'.                   
004100 77  FEL                         PIC X(1)    VALUE 'F'.                   
004200 77  TIO                         PIC S9(3)   COMP-3 VALUE +10.            
004300 77  SPRAK-IX                    PIC S9(9)   VALUE +0  COMP SYNC.         
004400 77  INPUT-RETT                  PIC X(1)    VALUE 'J'.                   
004500 77  LAS-VIDARE                  PIC X(1)    VALUE 'J'.                   
004600 77  TEXT-FINNS                  PIC X(1)    VALUE 'N'.                   
004700 77  FLFORTS                     PIC X(1)    VALUE 'N'.                   
004800 77  FLTCPLM                     PIC X(1)    VALUE 'N'.                   
004900 77  FINNS-PA-ACTION-FILE        PIC X(1)    VALUE 'N'.                   
005000 77  UPPDATERING-TILLATEN        PIC X(1)    VALUE 'J'.                   
005100 77  RADER-UPPDATERADE           PIC X(1)    VALUE 'J'.                   
005200 77  UPPDAT-TIERSDAT-TEARTNOT    PIC X(1)    VALUE 'N'.                   
005300 77  WS-EXTERN-SATS              PIC X(1)    VALUE 'N'.                   
005400 77  IDAO-FINNS                  PIC X(1)    VALUE 'N'.                   
005500 77  SW-IDAO-FIXAD               PIC X(1)    VALUE 'N'.                   
005600 77  SW-TOMMA-IDAO-FINNS         PIC X(1)    VALUE 'N'.                   
005700 77  SW-SUPPLIER-OK              PIC X(1)    VALUE 'N'.                   
005800 77  PPMS-TRANS                  PIC X(1)    VALUE 'N'.                   
005810 77  WS-FLGEMART                 PIC X(1)    VALUE 'N'.                   
005820 77  FL-NYPONART                 PIC X(1)    VALUE 'N'.                   
005900 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006000 77  WS-KDPRODSL                 PIC 9(2)    VALUE ZERO.                  
006100 77  WS-KDBPSR                   PIC 9(1)    VALUE ZERO.                  
006200 77  WS-TIFINLV-MAX              PIC 9(5)    VALUE ZERO.                  
006400 77  WS-IDKORTNR                 PIC 9(3)    VALUE ZERO.                  
006800 77  WS-TEARTNOT                 PIC X(40)   VALUE SPACE.                 
006900 77  WS-IDANSK                   PIC S9(3)   COMP-3 VALUE ZERO.           
007000 77  WS-IDANSK-ALARM             PIC S9(3)   COMP-3 VALUE ZERO.           
007100 77  WS-IDDC-ALARM               PIC X(2)    VALUE SPACES.                
007300 77  SPAR-IDPROJ                 PIC X(4)    VALUE SPACE.                 
007400 77  MAX-RAD                     PIC S9(3)   VALUE +0   COMP-3.           
007500 77  MAX-TAB                     PIC S9(3)   VALUE +990 COMP-3.           
007600 77  LEV05-IX                    PIC S9(9)   VALUE +0  COMP SYNC.         
007700 77  RAD-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
007800 77  TAB-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
007900 77  AO-IX                       PIC S9(9)   VALUE +0   COMP SYNC.        
008000 77  IX                          PIC S9(9)   VALUE +0   COMP SYNC.        
008100 77  STR-IX                      PIC S9(9)   VALUE +0   COMP SYNC.        
008200 77  STR-IX-MAX                  PIC S9(9)   VALUE +99  COMP SYNC.        
008300 77  MAX-IX-VAERDE               PIC S9(9)   VALUE +999999998             
008400                                             COMP SYNC.                   
008500 77  MIN-IX-VAERDE               PIC S9(9)   VALUE +1 COMP SYNC.          
008600 77  IX-MINUS-1                  PIC S9(9)   VALUE +0 COMP SYNC.          
008700 77  IX-PLUS-1                   PIC S9(9)   VALUE +0 COMP SYNC.          
008800 77  MAX-MOD-LAENGD              PIC S9(4) VALUE +902  COMP SYNC.         
009000 77  IDPROJK-IX                  PIC  9(2)   VALUE ZERO.                  
009100 77  IDPROJK-IX-MAX              PIC  9(2)   VALUE 75.                    
009200 77  IDPROJK-IX-MAX-PLUS-1       PIC  9(2)   VALUE 76.                    
009300 77  IDLEVNR-ALFA                PIC X(5)    VALUE SPACE.                 
009400 77  WS-IDLEVNR-NUM              PIC 9(5)    VALUE ZERO.                  
009500 77  SPAR-KDANSKQ                PIC X       VALUE SPACE.                 
009600                                                                          
009700*01  -COPY WWPRODSL                                                       
009800     EJECT                                                                
009900                                                                          
010000 01  ARBETSAREOR.                                                         
010100     03 FILLER                   PIC X(16)   VALUE                        
010200                                             'WS-DB2-SEKTION'.            
010300     03 WS-DB2-SEKTION           PIC X(24)   VALUE SPACE.                 
010400                                                                          
010500     03  WS-DAGENS-AAAAMMDD      PIC 9(8).                                
010600     03  WS-JMFR-AAAAMMDD        PIC 9(8).                                
010700     03  FILLER REDEFINES WS-JMFR-AAAAMMDD.                               
010800        05 FILLER                PIC 9(2).                                
010900        05 WS-JMFR-AA            PIC 9(2).                                
011000        05 FILLER                PIC 9(4).                                
011100     03  WS-FLAGGA-Q-KAMP        PIC X(1)    VALUE SPACE.                 
011200     03  WS-FLAGGA-W-S-KAMP      PIC X(1)    VALUE SPACE.                 
011300     03  WS-KVLS-REM             PIC S9(7)   VALUE ZERO COMP-3.           
011400     03  WS-ANTAL-KAMP           PIC 9(7)    VALUE ZERO.                  
011500                                                                          
011600     03  WS-TEMFSINF             PIC X(42)   VALUE SPACE.                 
011700     03  WS-TEMFSINF-KAMP        PIC X(10)   VALUE SPACE.                 
011800     03  WS-TEMFSINF-SPLIT       PIC X(3)    VALUE '-- '.                 
011900                                                                          
012000*      --- VALID IDDC CODES                                               
012100*                                                                         
012200*01    -COPY WWDCKONS                                                     
012300*01    -COPY WWDC99                                                       
012400*01    -COPY WWLEV05                                                      
012500       EJECT                                                              
012600 01  WS-IDSEKVNR                 PIC S9(3) VALUE ZERO COMP-3.             
012700                                                                          
012800 01  W-IDLOGLOP                  PIC S9(1) VALUE ZERO.                    
012900 01  WS-IDARTNR-TILLK            PIC X(9) VALUE ZERO.                     
013000 01  IDARTNR-TILLK-WS REDEFINES                                           
013100          WS-IDARTNR-TILLK       PIC 9(9).                                
013110 01  WS-DIERS-TILLK-X            PIC X(7) VALUE ZERO.                     
013120 01  WS-DIERS-TILLK   REDEFINES                                           
013130            WS-DIERS-TILLK-X     PIC Z(2)9.9(3).                          
013200 01  WS-PPMS-IDARTNR-TILLK       PIC X(9) VALUE ZERO.                     
013300 01  PPMS-IDARTNR-TILLK-WS       PIC 9(9) VALUE ZERO.                     
013400 01  W-IDARTNR-8                 PIC 9(8) VALUE ZERO.                     
013500                                                                          
013510 01  WS-KDERS-TEMP               PIC 9(2)   VALUE ZERO.                   
013520 01  WS-TIERSDAT-TEMP            PIC 9(5)   VALUE ZERO.                   
013530 01  WS-IDARTNR-TEMP             PIC 9(9)   VALUE ZERO.                   
013600     SKIP2                                                                
013700 01  WS-KDERS                    PIC 9(2)   VALUE ZERO.                   
013800 01  FILLER REDEFINES WS-KDERS.                                           
013900     03  WS-KDERS-1              PIC 9.                                   
014000     03  WS-KDERS-2              PIC 9.                                   
014100     SKIP2                                                                
014200 01  NY-KDERS-C1                 PIC 9(2)    VALUE ZERO.                  
014300     SKIP2                                                                
014400 01  WS-KVBR-TOT                 PIC 9(7)   VALUE ZERO.                   
014500 01  WS-KVBR                     PIC 9(7)   VALUE ZERO.                   
014600     SKIP2                                                                
014700 01  WS-IDANSK-KOLL              PIC 9(3).                                
014800 01  FILLER REDEFINES WS-IDANSK-KOLL.                                     
014900     03  WS-IDANSK-POS1-2        PIC 9(2).                                
015000     03  FILLER                  PIC 9.                                   
015100     SKIP2                                                                
015200 01  W-IDAVTAL-RED               PIC 9(13).                               
015300 01  W-IDAVTAL REDEFINES W-IDAVTAL-RED.                                   
015400     03  FILLER                  PIC X.                                   
015500     03  W-PREFIX                PIC X(3).                                
015600     03  W-AVTALSNR              PIC X(6).                                
015700     03  W-SUFFIX                PIC X(3).                                
015800     SKIP2                                                                
015900 01  W-PREFIX-NUM                PIC 9(3).                                
016000 01  RAKNARE.                                                             
016100     03  B-RAKN                  PIC S9     COMP-3 VALUE ZERO.            
016200     03  R-RAKN                  PIC S9     COMP-3 VALUE ZERO.            
016300     03  WS-RAKNARE              PIC S9(5)  COMP-3 VALUE ZERO.            
016400     03  NOLL-RAKNARE            PIC S9(5)  COMP-3 VALUE ZERO.            
016500     SKIP2                                                                
016600 01  SWITCHAR.                                                            
016700     03  SW-TIKO                 PIC X             VALUE 'N'.             
016900     SKIP2                                                                
017000 01  SPARADE-IDAO.                                                        
017100     03  SPARAD-IDAO OCCURS 5.                                            
017200        05  IDAO-POS-1           PIC X.                                   
017300        05  IDAO-RESTEN          PIC X(9).                                
017400     SKIP2                                                                
017500 01  NYA-IDAO.                                                            
017600     03  NY-IDAO                 PIC X(10) OCCURS 5.                      
017700     SKIP2                                                                
017800 01  WS-TIFINLV-KOLL             PIC 9(5).                                
017900 01  FILLER REDEFINES WS-TIFINLV-KOLL.                                    
018000     03  WS-TIFINLV-AAVV         PIC 9(4).                                
018100     03  FILLER                  PIC 9.                                   
018200     SKIP2                                                                
018300 01  WS-TIFINLV                  PIC 9(5).                                
018400 01  FILLER REDEFINES WS-TIFINLV.                                         
018500     03  WS-AAVV                 PIC 9(4).                                
018600     03  FILLER                  PIC 9.                                   
018700     SKIP2                                                                
018800 01  WS-TIFINLV-SW               PIC X.                                   
018900     88   TIFINLV-OK             VALUE 'J'.                               
019000     SKIP2                                                                
019100 01  WS-TIERSDAT-PREL            PIC 9(5) VALUE ZERO.                     
019200 01  FILLER REDEFINES WS-TIERSDAT-PREL.                                   
019300     03  FILLER                  PIC 9(2).                                
019400     03  WS-VECKA-PREL           PIC 9(2).                                
019500     03  WS-DAG-PREL             PIC 9.                                   
019600     SKIP2                                                                
019700 01  XX-TIERSDAT-PREL            PIC X(5) VALUE SPACE.                    
019800 01  FILLER REDEFINES XX-TIERSDAT-PREL.                                   
019900     03  FILLER                  PIC X(2).                                
020000     03  XX-VECKA                PIC X(2).                                
020100     03  XX-DAG                  PIC X.                                   
020200     SKIP2                                                                
020300 01  WS-TISTODAT                 PIC S9(7) COMP-3 VALUE ZERO.             
020400     EJECT                                                                
020500 01  FILLER                      PIC X(11) VALUE 'ERSATT-AREA'.           
020600 01  ERSATT-AREA.                                                         
020700     03   ERSATT-KDERS-UTG       PIC S9(3)   COMP-3 VALUE ZERO.           
020800     03   ERSATT-FLERS           PIC X       VALUE SPACE.                 
020900     03   ERSATT-FLIART          PIC X       VALUE SPACE.                 
021000     03   ERSATT-KDKSP           PIC S9      COMP-3 VALUE ZERO.           
021100     03   ERSATT-IDLEVNR         PIC X(5)    VALUE SPACE.                 
021200     03   ERSATT-FLLSRDEL        PIC X       VALUE SPACE.                 
021300     03   ERSATT-KDPRODSL        PIC 9(2)    VALUE ZERO.                  
021400     03   ERSATT-KDAVT           PIC 9(1)    VALUE ZERO.                  
021500     03   ERSATT-IDINK           PIC 9(3)    VALUE ZERO.                  
021600     03   ERSATT-KDBPSR          PIC 9(1)    VALUE ZERO.                  
021700     03   ERSATT-PRARTSTD        PIC S9(7)V9(2)  VALUE ZERO.              
021800                                                                          
021900     03   ERSATT-KDERS-C1        PIC 9(2)    VALUE ZERO.                  
022000     03   FILLER REDEFINES ERSATT-KDERS-C1.                               
022100          05 ERSATT-KDERS-C1-1   PIC 9.                                   
022200          05 ERSATT-KDERS-C1-2   PIC 9.                                   
022300     SKIP3                                                                
022400 01  TABELL.                                                              
022500     03 TAB-RAD OCCURS 990.                                               
022600        05   TAB-IDRADNR         PIC 9(3).                                
022700        05   TAB-IDARTNR-TILLK   PIC 9(9).                                
022800        05   TAB-DIERS-TILLK     PIC 9(4)V9(3).                           
022900        05   TAB-BEERS           PIC X(20).                               
023000     SKIP3                                                                
023100 01  WS-STR-TABELL.                                                       
023200     03 WS-STR-TILLKART          PIC S9(9) COMP-3 VALUE ZERO              
023300                                         OCCURS 99.                       
023400     SKIP3                                                                
023500 01  ALARM-SSCODE-SW             PIC X   VALUE 'N'.                       
023600     88 ALARM-SSCODE-JA                  VALUE 'J'.                       
023700     SKIP3                                                                
023800 01  DYNAMISKA-SUBPROGRAM.                                                
023900     03  WDECEDIT                PIC X(8)       VALUE 'WDECEDIT'.         
024000     03  WDATKONV                PIC X(8)       VALUE 'WDATKONV'.         
024100     03  CBLTDLI                 PIC X(8)       VALUE 'CBLTDLI '.         
024200     03  FELLOG                  PIC X(8)       VALUE 'FELLOG  '.         
024300     03  W005INIT                PIC X(8)       VALUE 'W005INIT'.         
024400     EJECT                                                                
024500     EJECT                                                                
024600*01  -COPY W10111                                                         
024700     EJECT                                                                
024800*01  AREA   -COPY W092W001     -PRE W092-.                                
024900     EJECT                                                                
025000*01         -COPY W111PPMS     -PRE PPMS-.                                
025100     EJECT                                                                
025200*01         -COPY A310TB65     -PRE A310-                                 
025300     EJECT                                                                
025400*01  -COPY WDECAREA                                                       
025500     EJECT                                                                
025600*01  -COPY WDATAREA                                                       
025700     EJECT                                                                
025800                                                                          
026100 01  NYCKLAR-TILL-DLI.                                                    
026200     03  W-WDD901KY-X.                                                    
026300         05  W-IDARTNR-D9         PIC S9(9) COMP-3 VALUE ZERO.            
026400         05  W-IDDC-D9            PIC X(2)  VALUE SPACE.                  
026500     03  W-IDARTNR-X.                                                     
026600         05  W-IDARTNR            PIC S9(9) COMP-3 VALUE ZERO.            
026700     03  W-IDDC-X.                                                        
026800         05  W-IDDC               PIC X(2)  VALUE SPACE.                  
026900     03  W-KDSEGKEY-X.                                                    
027000         05  W-KDSEGKEY           PIC X(1)  VALUE '1'.                    
027100     03  W-1116-IDARTNR-X.                                                
027200         05  W-1116-IDARTNR       PIC S9(9) COMP-3 VALUE ZERO.            
027300     03  W-1158-IDARTNR-X.                                                
027400         05  W-1158-IDARTNR       PIC S9(9) COMP-3 VALUE ZERO.            
027500     03  W-IDARTNR-TILLK-X.                                               
027600         05  W-IDARTNR-TILLK      PIC S9(9) COMP-3 VALUE ZERO.            
027700     03  W-IDSKYLT-X.                                                     
027800         05  W-IDSKYLT            PIC X(3)  VALUE 'S  '.                  
027900     03  W-IDKORTNR-X.                                                    
028000         05  W-IDKORTNR           PIC S9(3) COMP-3 VALUE ZERO.            
028100     03  W-1111-KEY-X.                                                    
028200         05  FILLER               PIC X(4)  VALUE '1111'.                 
028300         05  W-1111-IDARTNR       PIC S9(9) COMP-3 VALUE ZERO.            
028400         05  FILLER               PIC X(21) VALUE LOW-VALUE.              
028500     03  W-1112-KEY-X.                                                    
028600         05  W-1112-IDARTNR       PIC S9(9) COMP-3 VALUE ZERO.            
028700         05  FILLER               PIC X(5)  VALUE LOW-VALUE.              
028800     03  W-1114-KEY-X.                                                    
028900         05  W-1114-IDKORTNR      PIC S9(3) COMP-3 VALUE ZERO.            
029000         05  FILLER               PIC X(8)  VALUE LOW-VALUE.              
029100     03  W-2203-KEY-X.                                                    
029200         05  FILLER               PIC X(4)  VALUE '2203'.                 
029300         05  W-2203-IDDC          PIC X(2).                               
029400         05  FILLER               PIC X(24) VALUE LOW-VALUE.              
029500     03  W-2303-KEY-X.                                                    
029600         05  FILLER               PIC X(4)  VALUE '2303'.                 
029700         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
029800                                                                          
029900*    03  W-2301-KEY-X.  GAMLA NYCKELN FÖR XXBT  NYA ÄR 2303               
030000*        05  FILLER               PIC X(4)  VALUE '2301'.                 
030100*        05  FILLER               PIC X(26) VALUE LOW-VALUE.              
030200                                                                          
030300     03  W-9101-KEY-X.                                                    
030400         05  FILLER               PIC X(4)  VALUE '9101'.                 
030500         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
030600     03  W-1139-KEY-X.                                                    
030700         05  FILLER               PIC X(4)  VALUE '1139'.                 
030800         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
030900     03  W-1140-KEY-X.                                                    
031000         05  FILLER               PIC X(1)  VALUE '1'.                    
031100     03  W-1141-KEY-X.                                                    
031200         05  FILLER               PIC X(4)  VALUE '1141'.                 
031300         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
031400     03  W-1142-KEY-X.                                                    
031500         05  FILLER               PIC X     VALUE SPACE.                  
031600     03  W-1115-KEY-X.                                                    
031700         05  FILLER               PIC X(4)  VALUE '1115'.                 
031800         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
031900     03  W-1157-KEY-X.                                                    
032000         05  FILLER               PIC X(4)  VALUE '1157'.                 
032100         05  FILLER               PIC X(26) VALUE LOW-VALUE.              
032200     03  W-IDARTNR-ERS-LOW-X.                                             
032300         05  W-IDARTNR-ERS-LOW   PIC S9(9) COMP-3  VALUE ZERO.            
032400     03  W-IDARTNR-ERS-HIGH-X.                                            
032500       05  W-IDARTNR-ERS-HIGH  PIC S9(9) COMP-3  VALUE +999999999.        
032600     03  W-IDKORTNR-LOW-X.                                                
032700         05  W-IDKORTNR-LOW      PIC S9(3) COMP-3  VALUE ZERO.            
032800     03  W-IDKORTNR-HIGH-X.                                               
032900         05  W-IDKORTNR-HIGH     PIC S9(3) COMP-3  VALUE +999.            
033000     03  W-IDAO-X.                                                        
033100         05  W-IDAO              PIC X(10) VALUE SPACE.                   
033200     03  W-WDJ1CSEQ-X.                                                    
033300         05  W-IDLEVNR-S         PIC X(5)  VALUE SPACE.                   
033400         05  W-BELEVART-S        PIC X(30) VALUE SPACE.                   
033500         05  W-IDARTNR-S         PIC S9(9) COMP-3 VALUE ZERO.             
033600     03  W-WDJ111KY-X.                                                    
033700         05  W-KDSTRRAD          PIC X VALUE SPACE.                       
033800         05  W-IDRADNR           PIC S9(5) COMP-3 VALUE ZERO.             
033900     03  W-WDGX2223-X.                                                    
034000         05  W-IDHTYP-2223       PIC X(4)  VALUE '2223'.                  
034100         05  W-IDANSK-2223       PIC S9(3) VALUE ZERO COMP-3.             
034200         05  W-VALFRI-2225       PIC X(24) VALUE LOW-VALUE.               
034300                                                                          
034400     03  W-WDGX2224-X.                                                    
034500         05  W-TISENBEK-DAG-2224 PIC S9(7) VALUE ZERO COMP-3.             
034600         05  W-TISENBEK-KL-2224  PIC S9(7) VALUE ZERO COMP-3.             
034700         05  W-KDLARM-2224       PIC S9(3) VALUE ZERO COMP-3.             
034800                                                                          
034900     03  W-WDGX2231-X.                                                    
035000         05  W-IDHTYP-2231       PIC X(4)  VALUE '2231'.                  
035100         05  W-VALFRI-2231       PIC X(26) VALUE LOW-VALUE.               
035200                                                                          
035300     03  W-WDGX2232-X.                                                    
035400         05  W-IDANSK-2232       PIC S9(3) VALUE ZERO COMP-3.             
035500         05  W-LOW-VALUE-2232    PIC X(3)  VALUE LOW-VALUE.               
035600 01  W-2227KEY-X.                                                         
035700     03 W-IDHTYP        PIC X(4)    VALUE '2227'.                         
035800     03 FILLER          PIC X(26)   VALUE LOW-VALUE.                      
035900*                                                                         
036000     EJECT                                                                
036100 01  MEDDELANDE.                                                          
036200                                                                          
036300     03  W-FEL-1.                                                         
036400         05  FILLER              PIC X(40)  VALUE                         
036500            'ARTIKELNUMMER EJ NUMERISKT              '.                   
036600         05  FILLER              PIC X(40)  VALUE                         
036700            'PARTNUMBER NOT NUMERIC                  '.                   
036800     03  FILLER REDEFINES W-FEL-1.                                        
036900         05  FEL-1               PIC X(40)  OCCURS 2.                     
037000                                                                          
037100     03  W-FEL-2.                                                         
037200         05  FILLER              PIC X(40)  VALUE                         
037300            'ARTIKELNUMRET SAKNAS PÅ ARTIKLEREGISTRET'.                   
037400         05  FILLER              PIC X(40)  VALUE                         
037500            'PARTNUMBER IS MISSING IN PARTS FILE     '.                   
037600     03  FILLER REDEFINES W-FEL-2.                                        
037700         05  FEL-2               PIC X(40)  OCCURS 2.                     
037800                                                                          
037900     03  W-FEL-3.                                                         
038000         05  FILLER              PIC X(40)  VALUE                         
038100            'KORRIGERA UPPLYSTA FÄLT                 '.                   
038200         05  FILLER              PIC X(40)  VALUE                         
038300            'CORRECT HIGHLIGHTED FIELDS              '.                   
038400     03  FILLER REDEFINES W-FEL-3.                                        
038500         05  FEL-3               PIC X(40)  OCCURS 2.                     
038600                                                                          
038700     03  W-FEL-4.                                                         
038800         05  FILLER              PIC X(40)  VALUE                         
038900            'TRYCK PF11 FÖR UPPDATERING              '.                   
039000         05  FILLER              PIC X(40)  VALUE                         
039100            'PRESS PF11 FOR UPDATING                 '.                   
039200     03  FILLER REDEFINES W-FEL-4.                                        
039300         05  FEL-4               PIC X(40)  OCCURS 2.                     
039400                                                                          
039500     03  W-FEL-8.                                                         
039600         05  FILLER              PIC X(40)  VALUE                         
039700            'OBEHÖRIG ANVÄNDARE                      '.                   
039800         05  FILLER              PIC X(40)  VALUE                         
039900            'USER NOT AUTHORIZED                     '.                   
040000     03  FILLER REDEFINES W-FEL-8.                                        
040100         05  FEL-8               PIC X(40)  OCCURS 2.                     
040200                                                                          
040300     03  W-MED-1.                                                         
040400         05  FILLER              PIC X(40)  VALUE                         
040500            'ANTAL SAKNAS                            '.                   
040600         05  FILLER              PIC X(40)  VALUE                         
040700            'QUANTITY IS MISSING                     '.                   
040800     03  FILLER REDEFINES W-MED-1.                                        
040900         05  MED-1               PIC X(40)  OCCURS 2.                     
041000                                                                          
041100     03  W-MED-2.                                                         
041200         05  FILLER              PIC X(40)  VALUE                         
041300            'ARTIKEL ÄR TILLKOMMANDE I ANNAN ERS    '.                    
041400         05  FILLER              PIC X(40)  VALUE                         
041500            'PARTNO IS ADDED IN ANOTHER SUPERSESSION'.                    
041600     03  FILLER REDEFINES W-MED-2.                                        
041700         05  MED-2               PIC X(40)  OCCURS 2.                     
041800                                                                          
041900     03  W-MED-3.                                                         
042000         05  FILLER              PIC X(40)  VALUE                         
042100              'EJ GODKÄND ERSÄTTNINGSKOD             '.                   
042200         05  FILLER              PIC X(40)  VALUE                         
042300              'NOT A VALID SUPERSESSION CODE         '.                   
042400     03  FILLER REDEFINES W-MED-3.                                        
042500         05  MED-3               PIC X(40)  OCCURS 2.                     
042600                                                                          
042700     03  W-MED-4.                                                         
042800         05  FILLER              PIC X(40)  VALUE                         
042900              'ERSÄTTNINGSDATUM SAKNAS               '.                   
043000         05  FILLER              PIC X(40)  VALUE                         
043100              'DATE OF SUPERSESSION IS MISSING       '.                   
043200     03  FILLER REDEFINES W-MED-4.                                        
043300         05  MED-4               PIC X(40)  OCCURS 2.                     
043400                                                                          
043500     03  W-MED-5.                                                         
043600         05  FILLER              PIC X(40)  VALUE                         
043700              'ÄNDRA ERSÄTTNINGSDATUM                '.                   
043800         05  FILLER              PIC X(40)  VALUE                         
043900              'CHANGE DATE OF SUPERSESSION           '.                   
044000     03  FILLER REDEFINES W-MED-5.                                        
044100         05  MED-5               PIC X(40)  OCCURS 2.                     
044200                                                                          
044300     03  W-MED-6.                                                         
044400         05  FILLER              PIC X(40)  VALUE                         
044500              'TILLKOMMANDE ARTIKEL SAKNAS PÅ ART.REG'.                   
044600         05  FILLER              PIC X(40)  VALUE                         
044700              'ADDING PART NO IS MISSING IN PARTS FILE'.                  
044800     03  FILLER REDEFINES W-MED-6.                                        
044900         05  MED-6               PIC X(40)  OCCURS 2.                     
045000                                                                          
045100     03  W-MED-7.                                                         
045200         05  FILLER              PIC X(40)  VALUE                         
045300              'TILLKOMMANDE ARTIKLAR FÅR EJ FINNAS   '.                   
045400         05  FILLER              PIC X(40)  VALUE                         
045500              'ADDING PART NO IS NOT ALLOWED         '.                   
045600     03  FILLER REDEFINES W-MED-7.                                        
045700         05  MED-7               PIC X(40)  OCCURS 2.                     
045800                                                                          
045900                                                                          
046000                                                                          
046100                                                                          
046200                                                                          
046300                                                                          
046400     03  W-MED-9.                                                         
046500         05  FILLER              PIC X(40)  VALUE                         
046600              'ARTIKEL INGÅR I SATS                  '.                   
046700         05  FILLER              PIC X(40)  VALUE                         
046800              'PART NO IS INCLUDED IN A KIT          '.                   
046900     03  FILLER REDEFINES W-MED-9.                                        
047000         05  MED-9               PIC X(40)  OCCURS 2.                     
047100                                                                          
047200     03  W-MED-10.                                                        
047300         05  FILLER              PIC X(40)  VALUE                         
047400              'EJ ENT ERS MÅSTE BÖRJA MED TEXT       '.                   
047500         05  FILLER              PIC X(40)  VALUE                         
047600              'ALT SUPERSESSION MUST START WITH TEXT '.                   
047700     03  FILLER REDEFINES W-MED-10.                                       
047800         05  MED-10              PIC X(40)  OCCURS 2.                     
047900                                                                          
048000     03  W-MED-11.                                                        
048100         05  FILLER              PIC X(40)  VALUE                         
048200              'ÄO-SAKNAS                             '.                   
048300         05  FILLER              PIC X(40)  VALUE                         
048400              'DCN-NO IS MISSING                     '.                   
048500     03  FILLER REDEFINES W-MED-11.                                       
048600         05  MED-11              PIC X(40)  OCCURS 2.                     
048700                                                                          
048800                                                                          
048900                                                                          
049000                                                                          
049100                                                                          
049200                                                                          
049300     03  W-MED-13.                                                        
049400         05  FILLER              PIC X(40)  VALUE                         
049500              'TILLKOMMANDE ARTIKELNUMMER ÄR ERSATT  '.                   
049600         05  FILLER              PIC X(40)  VALUE                         
049700              'ADDING PART NO IS SUPERSEDED          '.                   
049800     03  FILLER REDEFINES W-MED-13.                                       
049900         05  MED-13              PIC X(40)  OCCURS 2.                     
050000                                                                          
050100                                                                          
050200                                                                          
050300                                                                          
050400                                                                          
050500     03  W-MED-15.                                                        
050600         05  FILLER              PIC X(40)  VALUE                         
050700              'UPPDATERING UTFÖRD                    '.                   
050800         05  FILLER              PIC X(40)  VALUE                         
050900              'FIELDS ARE UPDATED                    '.                   
051000     03  FILLER REDEFINES W-MED-15.                                       
051100         05  MED-15              PIC X(40)  OCCURS 2.                     
051200                                                                          
051300     03  W-MED-16.                                                        
051400         05  FILLER              PIC X(40)  VALUE                         
051500              'FLER RADER FINNS                      '.                   
051600         05  FILLER              PIC X(40)  VALUE                         
051700              'MORE LINES                            '.                   
051800     03  FILLER REDEFINES W-MED-16.                                       
051900         05  MED-16              PIC X(40)  OCCURS 2.                     
052000                                                                          
052100     03  W-MED-17.                                                        
052200         05  FILLER              PIC X(40)  VALUE                         
052300              'ARTIKEL RENSAD                        '.                   
052400         05  FILLER              PIC X(40)  VALUE                         
052500              'PART NO IS DELETED                    '.                   
052600     03  FILLER REDEFINES W-MED-17.                                       
052700         05  MED-17              PIC X(40)  OCCURS 2.                     
052800                                                                          
052900     03  W-MED-18.                                                        
053000         05  FILLER              PIC X(40)  VALUE                         
053100              'UPPDATERAS AV ANNAN ANVÄNDARE'.                            
053200         05  FILLER              PIC X(40)  VALUE                         
053300              'IS BEING UPDATED BY ANOTHER USER    '.                     
053400     03  FILLER REDEFINES W-MED-18.                                       
053500         05  MED-18              PIC X(40)  OCCURS 2.                     
053600                                                                          
053700                                                                          
053800                                                                          
053900                                                                          
054000                                                                          
054100                                                                          
054200     03  W-MED-20.                                                        
054300         05  FILLER              PIC X(40)  VALUE                         
054400              'ERSÄTTNINGEN BACKAS FÖRE UPPDATERING'.                     
054500         05  FILLER              PIC X(40)  VALUE                         
054600              'THE SUP. MUST BE REVERSED BEFORE UPDATE'.                  
054700     03  FILLER REDEFINES W-MED-20.                                       
054800         05  MED-20              PIC X(40)  OCCURS 2.                     
054900                                                                          
055000     03  W-MED-21.                                                        
055100         05  FILLER              PIC X(40)  VALUE                         
055200              'ÄNDRA TIFINLV PÅ TILLK ARTIKEL       '.                    
055300         05  FILLER              PIC X(40)  VALUE                         
055400              'CHANGE PUBL. WEEK ON ADDING PART NO   '.                   
055500     03  FILLER REDEFINES W-MED-21.                                       
055600         05  MED-21              PIC X(40)  OCCURS 2.                     
055700                                                                          
055800     03  W-MED-22.                                                        
055900         05  FILLER              PIC X(40)  VALUE                         
056000              'UPPDATERING EJ GJORD                 '.                    
056100         05  FILLER              PIC X(40)  VALUE                         
056200              'UPDATE IS NOT DONE                    '.                   
056300     03  FILLER REDEFINES W-MED-22.                                       
056400         05  MED-22              PIC X(40)  OCCURS 2.                     
056500                                                                          
056600     03  W-MED-23.                                                        
056700         05  FILLER              PIC X(40)  VALUE                         
056800              'ERSÄTTNINGEN FÅR EJ UPPDATERAS       '.                    
056900         05  FILLER              PIC X(40)  VALUE                         
057000              'THE SUP. IS NOT ALLOWED TO BE UPDATED'.                    
057100     03  FILLER REDEFINES W-MED-23.                                       
057200         05  MED-23              PIC X(40)  OCCURS 2.                     
057300                                                                          
057400     03  W-MED-24.                                                        
057500         05  FILLER              PIC X(40)  VALUE                         
057600              'COMMON PART                          '.                    
057700         05  FILLER              PIC X(40)  VALUE                         
057800              'COMMON PART                           '.                   
057900     03  FILLER REDEFINES W-MED-24.                                       
058000         05  MED-24              PIC X(40)  OCCURS 2.                     
058100                                                                          
058200     03  W-MED-25.                                                        
058300         05  FILLER              PIC X(40)  VALUE                         
058400              'ARTIKEL INGÅR I 1002-SATS            '.                    
058500         05  FILLER              PIC X(40)  VALUE                         
058600              'PART NO IS INCLUDED IN A 1002-KIT     '.                   
058700     03  FILLER REDEFINES W-MED-25.                                       
058800         05  MED-25              PIC X(40)  OCCURS 2.                     
058900                                                                          
059000     03  W-MED-26.                                                        
059100         05  FILLER              PIC X(40)  VALUE                         
059200              'ARTIKEL INGÅR I EJ 1002-SATS         '.                    
059300         05  FILLER              PIC X(40)  VALUE                         
059400              'PART NO NOT INCLUDED IN A 1002-KIT    '.                   
059500     03  FILLER REDEFINES W-MED-26.                                       
059600         05  MED-26              PIC X(40)  OCCURS 2.                     
059700                                                                          
059800     03  W-MED-27.                                                        
059900         05  FILLER              PIC X(40)  VALUE                         
060000              'SATSEN ÄR DEF ERSATT                 '.                    
060100         05  FILLER              PIC X(40)  VALUE                         
060200              'THE KIT IS DEFINITIVELY SUPERSEDED    '.                   
060300     03  FILLER REDEFINES W-MED-27.                                       
060400         05  MED-27              PIC X(40)  OCCURS 2.                     
060500                                                                          
060600     03  W-MED-28.                                                        
060700         05  FILLER              PIC X(40)  VALUE                         
060800              'ALTERNATIV ERSÄTTN - UPPDATERA RASA  '.                    
060900         05  FILLER              PIC X(40)  VALUE                         
061000              'ALTERNATIVE SUPERSESSION - UPDATE RASA'.                   
061100     03  FILLER REDEFINES W-MED-28.                                       
061200         05  MED-28              PIC X(40)  OCCURS 2.                     
061300                                                                          
061400     03  W-MED-29.                                                        
061500         05  FILLER              PIC X(40)  VALUE                         
061600              'ARTIKEL INGÅR I STRUKTUR             '.                    
061700         05  FILLER              PIC X(40)  VALUE                         
061800              'PART NO IS INCLUDED IN A STRUCTURE '.                      
061900     03  FILLER REDEFINES W-MED-29.                                       
062000         05  MED-29              PIC X(40)  OCCURS 2.                     
062100     03  W-MED-30.                                                        
062200         05  FILLER              PIC X(40)  VALUE                         
062300              'GEMENSAM PV/LV                     '.                      
062400         05  FILLER              PIC X(40)  VALUE                         
062500              'COMMON CP/TP                       '.                      
062600     03  FILLER REDEFINES W-MED-30.                                       
062700         05  MED-30              PIC X(40)  OCCURS 2.                     
062800     03  W-MED-31.                                                        
062900         05  FILLER              PIC X(40)  VALUE                         
063000              'UPPDATERING EJ TILLÅTEN              '.                    
063100         05  FILLER              PIC X(40)  VALUE                         
063200              'UPDATE NOT ALLOWED                 '.                      
063300     03  FILLER REDEFINES W-MED-31.                                       
063400         05  MED-31              PIC X(40)  OCCURS 2.                     
063500                                                                          
063600                                                                          
063700****           MED-91 KAN KOMBINERAS MED MEDDELANDENA                     
063800****                  MED-2, MED-16, MED-18, MED-30 VID LÄS-TRANS         
063900     03    W-MED-91.                                                      
064000         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
064100         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
064200     03    FILLER REDEFINES W-MED-91.                                     
064300         05    MED-91        PIC X(10) OCCURS 2.                          
064400                                                                          
064500****           MED-92 KAN KOMBINERAS MED MEDDELANDENA                     
064600****                  MED-2, MED-16, MED-18, MED-30 VID LÄS-TRANS         
064700     03    W-MED-92.                                                      
064800         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
064900         05    FILLER        PIC X(10)   VALUE 'CAMPAIGN  '.              
065000     03    FILLER REDEFINES W-MED-92.                                     
065100         05    MED-92        PIC X(10) OCCURS 2.                          
065200                                                                          
065400     EJECT                                                                
065410*                        ****    MFS OCH SKÄRMHANTERING                   
065420 01  FILLER              PIC X(16)   VALUE 'MFS-WS'.                      
065492*01  -COPY WMFSAREA                                                       
065493     EJECT                                                                
065500******************************************************************        
065600*ERROR FIELDS FOR DISPACTHER ENTY FROM W11181600              **          
065700******************************************************************        
065800 01  MSG-KOM-MESSAGE-CODES.                                               
065900     03  FEL-ERR-FIELD           PIC X(3)    VALUE '020'.                 
066000     03  FEL-ERR-IDARTNR         PIC X(3)    VALUE '768'.                 
066100     03  FEL-ERR-IDKORTNR        PIC X(3)    VALUE '98A'.                 
066200     03  FEL-ERR-DIERS           PIC X(3)    VALUE '98B'.                 
066300     03  FEL-ERR-KDERS           PIC X(3)    VALUE '98C'.                 
066400     03  FEL-ERR-IDAO            PIC X(3)    VALUE '98D'.                 
066500     03  FEL-ERR-TIERSDAT        PIC X(3)    VALUE '98E'.                 
066600     03  FEL-ERR-BEERS           PIC X(3)    VALUE '98F'.                 
066610     03  FEL-ERR-FLKLAR          PIC X(3)    VALUE '98G'.                 
066620     03  FEL-ERR-TEARTNOT        PIC X(3)    VALUE '96U'.                 
066630     03  OK-GODKANT-FEL          PIC X(3)    VALUE '114'.                 
066640     03  OK-BEHANDLAD            PIC X(3)    VALUE '101'.                 
066700******************************************************************        
066800*                                                                         
066900*        ARBETS-AREOR TILL DB2-SEKTIONERNA                                
067000*                                                                         
067100 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
067200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
067300                                                                          
067400 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
067500 01  DB2-WS.                                                              
067600     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
067700         88  CURSOR-OK                      VALUE 000.                    
067800         88  LINES-FOUND                    VALUE 000.                    
067900         88  LINES-MISSING                  VALUE 100.                    
068000         88  RESOURCE-WRONG                 VALUE 904.                    
068100     03  GOOD-SQLCODECODES.                                               
068200         05  GOOD-SQLCODE OCCURS 5                                        
068300             INDEXED BY SQLCODE-IX PIC 9(3).                              
068400     EJECT                                                                
068500******************************************************************        
068600*****                                                                     
068700*****    ARBETS-AREOR TILL IMS-SEKTIONERNA                                
068800*****                                                                     
068900 01  IMS-WS.                                                              
069000     03  FILLER                  PIC X(16)   VALUE ' IMS-WS '.            
069100     SKIP3                                                                
069200*****                    **** STATUS-KOD FRÅN IMS                         
069300     03  STATUS-WS               PIC X(2).                                
069400         88  SEGMENT-FINNS                   VALUE '  '.                  
069500         88  SEGMENT-SAKNAS                  VALUE 'GE'.                  
069600         88  BASEN-SLUT                      VALUE 'GB'.                  
069700         88  SEGMENT-FINNS-REDAN             VALUE 'II'.                  
069800     SKIP3                                                                
069900     03  GODK-STATUSKODER.                                                
070000         05  GODK-STATUS OCCURS 2 INDEXED BY STATUS-IX PIC XX.            
070100     SKIP3                                                                
070200 01  SSA1                        PIC X(128).                              
070300 01  SSA2                        PIC X(128).                              
070400 01  SSA3                        PIC X(128).                              
070500     EJECT                                                                
070600*                            IMS FUNKTIONSKODER                           
070700*01  -COPY W0003                                                          
070800     EJECT                                                                
070900*                            DLI INPUT-OUTPUT AREA                        
071000 01  DLI-IO-AREA.                                                         
071100     03  IO-AREA                 PIC X(928)  VALUE SPACE.                 
071200     SKIP3                                                                
071300*    03  WLERSA  -COPY WDD701   -PRE ERSA01-  -RED IO-AREA.               
071400     EJECT                                                                
071500*    03  WLERSA  -COPY WDD702   -PRE ERSA11-  -RED IO-AREA.               
071600     EJECT                                                                
071700*    03  WLERSA  -COPY WDD704   -PRE ERSA13-  -RED IO-AREA.               
071800     EJECT                                                                
071900*    03  WLINLB  -COPY WDD902   -PRE INLB11-  -RED IO-AREA.               
072000     EJECT                                                                
072100*    03  WLARTC  -COPY WDK601                 -RED IO-AREA.               
072200     EJECT                                                                
072300*    03  WLARTC  -COPY WDK611                 -RED IO-AREA.               
072400     EJECT                                                                
072500*    03  WLARTC  -COPY WDK623                 -RED IO-AREA.               
072600     EJECT                                                                
072700*    03  WLBENA  -COPY WDD311   -PRE BENA-    -RED IO-AREA.               
072800     EJECT                                                                
072900*    03  WDGX2304 -COPY WDGX2304 -PRE 2303-  -RED IO-AREA.                
073000     EJECT                                                                
073100*    03  WLXXBJ  -COPY WDGX2204             -RED IO-AREA.                 
073200     EJECT                                                                
073300*    03  WLXXID  -COPY WDG39101 -PRE XXID-  -RED IO-AREA.                 
073400     EJECT                                                                
073500*    03  WLXXAW  -COPY WDGX1116 -PRE XXAW-  -RED IO-AREA.                 
073600     EJECT                                                                
073700*    03  WLXXCW  -COPY WDGX1158 -PRE XXCW-  -RED IO-AREA.                 
073800     EJECT                                                                
073900*    03  WLZZAC  -COPY WDGZ01   -PRE ZZAC-  -RED IO-AREA.                 
074000     EJECT                                                                
074100*    03  WLXXAV  -COPY WDGX1142 -PRE XXAV-  -RED IO-AREA.                 
074200     EJECT                                                                
074300 01  DLI-IO-AREA-2.                                                       
074400     03  IO-AREA-2               PIC X(200)  VALUE SPACE.                 
074500     SKIP3                                                                
074600*    03  WLXXAN  -COPY WDGX1111 -PRE XXAN-  -RED IO-AREA-2.               
074700     EJECT                                                                
074800*    03  WLXXAN  -COPY WDGX1112 -PRE XXAN-  -RED IO-AREA-2.               
074900     EJECT                                                                
075000*    03  WLXXAN  -COPY WDGX1114 -PRE XXAN-  -RED IO-AREA-2.               
075100     EJECT                                                                
075200 01  DLI-IO-AREA-3.                                                       
075300     03  IO-AREA-3               PIC X(600)  VALUE SPACE.                 
075400     SKIP3                                                                
075500*    03  WLARTG  -COPY WDD201   -PRE ARTG-  -RED IO-AREA-3.               
075600     EJECT                                                                
075700 01  DLI-IO-AREA-5.                                                       
075800     03  IO-AREA-5               PIC X(500)  VALUE SPACE.                 
075900     03  WDJ1CSEQ  REDEFINES IO-AREA-5.                                   
076000*       05  WLSATB11  -COPY WDJ111  -PRE SATB-                            
076100*       05  WLSATB01  -COPY WDJ101  -PRE SATB-                            
076200     EJECT                                                                
076300 01  DLI-IO-AREA-6.                                                       
076400     03  IO-AREA-6               PIC X(20)  VALUE SPACE.                  
076500     SKIP3                                                                
076600*    03  WLXXBW01 INGEN COPYTEXT TOM ROT                                  
076700*    03  WLXXBW11  -COPY WDGX2228     -RED IO-AREA-6.                     
076800     EJECT                                                                
076900 01  DLI-IO-AREA-7.                                                       
077000*    03  AREA    -COPY WDR301   -PRE FILC-                                
077100     EJECT                                                                
077200 01  DLI-IO-AREA-8.                                                       
077300*    03  -COPY WDB601                                                     
077400     EJECT                                                                
077500 01  FILLER                      PIC X(16)   VALUE 'WDK701-AREA'.         
077600 01  DLI-IO-AREA-WDK701.                                                  
077700*    03  -COPY WDK701                                                     
077800     EJECT                                                                
077900 01  FILLER                      PIC X(16)   VALUE 'WDK711-AREA'.         
078000 01  DLI-IO-AREA-WDK711.                                                  
078100*    03  -COPY WDK711                                                     
078200     EJECT                                                                
078300 01  FILLER                      PIC X(16)   VALUE 'WDK722-AREA'.         
078400 01  DLI-IO-AREA-WDK722.                                                  
078500*    03  -COPY WDK722                                                     
078600     EJECT                                                                
078700 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDR220'.        
078800 01  DLI-IO-AREA-2232.                                                    
078900*    03  -COPY WDGX2232   -PRE WDR220-                                    
079000     EJECT                                                                
079100 01  FILLER                      PIC X(16)  VALUE 'WDR501-AREA'.          
079200 01  DLI-IO-AREA-2223.                                                    
079300*    03  -COPY WDGX2223   -PRE WDR501-                                    
079400     EJECT                                                                
079500 01  FILLER                      PIC X(16)  VALUE 'WDR550-AREA'.          
079600 01  DLI-IO-AREA-2224.                                                    
079700*    03  -COPY WDGX2224   -PRE WDR550-                                    
079800     EJECT                                                                
079900                                                                          
080000*    --------------- DB2 INPUT-OUTPUT AREA ---------------                
080100                                                                          
080200 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
080300*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
080400     EJECT                                                                
080500 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
080600*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
080700     EJECT                                                                
080800     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
080900     EJECT                                                                
081000     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
081100     EJECT                                                                
081200                                                                          
081300 LINKAGE SECTION.                                                         
081400     SKIP2                                                                
081410 01  REQU-AREA.                                                           
081420*    03 -COPY WZ01REQU                                                    
081430*    03 -COPY W111ERIN                                                    
081440*                                                                         
081450 01  RESP-AREA.                                                           
081471*    03 -COPY WZ01RESP                                                    
081472*    03 -COPY W111ERUT                                                    
081480*                                                                         
081900*01  -COPY W0008     -PRE ERSA-                                           
082000         05  FILLER              PIC X.                                   
082100     EJECT                                                                
082200*01  -COPY W0008     -PRE ERSB-                                           
082300         05  FILLER              PIC X.                                   
082400     EJECT                                                                
082500*01  -COPY W0008     -PRE ARTC-                                           
082600         05  FILLER              PIC X.                                   
082700     EJECT                                                                
082800*01  -COPY W0008     -PRE BENA-                                           
082900         05  FILLER              PIC X.                                   
083000     EJECT                                                                
083100*01  -COPY W0008     -PRE INLB-                                           
083200         05  FILLER              PIC X.                                   
083300     EJECT                                                                
083400*01  -COPY W0008     -PRE XXAN-                                           
083500         05  FILLER              PIC X.                                   
083600     EJECT                                                                
083700*01  -COPY W0008     -PRE 2303-                                           
083800         05  FILLER              PIC X.                                   
083900     EJECT                                                                
084000*01  -COPY W0008     -PRE XXBJ-                                           
084100         05  FILLER              PIC X.                                   
084200     EJECT                                                                
084300*01  -COPY W0008     -PRE XXID-                                           
084400         05  FILLER              PIC X.                                   
084500     EJECT                                                                
084600*01  -COPY W0008     -PRE ZZAC-                                           
084700         05  FILLER              PIC X.                                   
084800     EJECT                                                                
084900*01  -COPY W0008     -PRE ART2-                                           
085000         05  FILLER              PIC X.                                   
085100     EJECT                                                                
085200*01  -COPY W0008     -PRE XXAV-                                           
085300         05  FILLER              PIC X.                                   
085400     EJECT                                                                
085500*01  -COPY W0008     -PRE ARTG-                                           
085600         05  FILLER              PIC X.                                   
085700     EJECT                                                                
085800*01  -COPY W0008     -PRE XXAW-                                           
085900         05  FILLER              PIC X.                                   
086000     EJECT                                                                
086100*01  -COPY W0008     -PRE SATE-                                           
086200         05  FILLER              PIC X.                                   
086300     EJECT                                                                
086400*01  -COPY W0008     -PRE SATB-                                           
086500         05  FILLER              PIC X.                                   
086600     EJECT                                                                
086700*01  -COPY W0008     -PRE XXBW-                                           
086800         05  FILLER              PIC X.                                   
086900     EJECT                                                                
087000*01  -COPY W0008     -PRE XXCW-                                           
087100         05  FILLER              PIC X.                                   
087200     EJECT                                                                
087300*01  -COPY W0008     -PRE FILC-                                           
087400         05  FILLER              PIC X.                                   
087500     EJECT                                                                
087600*01  -COPY W0008     -PRE WDK7-                                           
087700     05  FILLER                  PIC X.                                   
087800     EJECT                                                                
087900*01  -COPY W0008     -PRE WDB6-                                           
088000     05  FILLER                  PIC X.                                   
088100     EJECT                                                                
088200*01  -COPY W0008     -PRE WDR2-                                           
088300     05  FILLER                  PIC X.                                   
088400     EJECT                                                                
088500*01  -COPY W0008     -PRE WDR5-                                           
088600     05  FILLER                  PIC X.                                   
088700     EJECT                                                                
088800 PROCEDURE DIVISION USING REQU-AREA RESP-AREA                             
088900                          ARTC-PCB ERSA-PCB ERSB-PCB                      
089000                          INLB-PCB BENA-PCB XXAN-PCB                      
089100                          2303-PCB XXBJ-PCB                               
089200                          XXID-PCB ZZAC-PCB ART2-PCB                      
089300                          XXAV-PCB ARTG-PCB XXAW-PCB                      
089400                          SATE-PCB SATB-PCB                               
089500                          XXBW-PCB XXCW-PCB                               
089600                          FILC-PCB WDK7-PCB WDB6-PCB                      
089700                          WDR2-PCB WDR5-PCB.                              
089800                                                                          
089900     ENTRY 'DLITCBL' USING REQU-AREA RESP-AREA                            
090000                           ARTC-PCB ERSA-PCB ERSB-PCB                     
090100                           INLB-PCB BENA-PCB XXAN-PCB                     
090200                           2303-PCB XXBJ-PCB                              
090300                           XXID-PCB ZZAC-PCB ART2-PCB                     
090400                           XXAV-PCB ARTG-PCB XXAW-PCB                     
090500                           SATE-PCB SATB-PCB                              
090600                           XXBW-PCB XXCW-PCB                              
090700                           FILC-PCB WDK7-PCB WDB6-PCB                     
090800                           WDR2-PCB WDR5-PCB.                             
090900                                                                          
091000     PERFORM A-INIT                                                       
091100                                                                          
091200     IF REQU-IDARTNR-KEY NUMERIC                                          
091300       IF REQU-UPDATE                                                     
091400         PERFORM C-KOLLA-ACTION-FILE                                      
091500         IF UPPDATERING-TILLATEN = NEJ                                    
091600           PERFORM S22-ROER-EJ-FAELT                                      
091700         ELSE                                                             
091800           PERFORM H-LAS-ERSATT-ARTIKEL                                   
091900           IF INPUT-RETT = JA                                             
092000            PERFORM B-KOLLA-INDATA                                        
092100            IF INPUT-RETT = JA                                            
092200               IF REQU-KDERS = ALL '+'                                    
092300                MOVE REQU-IDARTNR-KEY TO W-IDARTNR                        
092400                PERFORM IMS-GET-ERSA01                                    
092500                IF SEGMENT-FINNS                                          
092600                  IF UPPDAT-TIERSDAT-TEARTNOT = JA                        
092700                     IF REQU-FLKLAR = JA OR YES                           
092800                        PERFORM K-UPPDAT-TIERSDAT-TEARTNOT                
092900                     ELSE                                                 
093000                        MOVE MFS-ADD-SAETT-CURSOR TO                      
093100                        RESP-FLKLAR-ATTR                                  
093200                        MOVE MED-22(SPRAK-IX) TO RESP-TEMFSINF            
093300                     END-IF                                               
093400                  ELSE                                                    
093500                     IF REQU-FLKLAR = NEJ                                 
093600                       PERFORM S17-UPPDAT-TILLK-ART-ACTION                
093700                     ELSE                                                 
093800                       PERFORM I-UPPDAT-TILLK-ART-ERS-REG                 
093900                       IF INPUT-RETT = JA                                 
094000                          MOVE ERSATT-KDERS-C1 TO WS-KDERS                
094100                          PERFORM S16-SKAPA-VR-TRANS                      
094200                       ELSE                                               
094300                         MOVE FEL-3(SPRAK-IX) TO RESP-TEMFSFEL            
094400                       END-IF                                             
094500                     END-IF                                               
094600                  END-IF                                                  
094700                END-IF                                                    
094800               ELSE                                                       
094900                  IF REQU-KDERS = ZERO                                    
095000                     IF REQU-FLKLAR = JA OR YES                           
095100                        PERFORM D-RIVNING-AV-ERSAETTNING                  
095200                     ELSE                                                 
095300                        MOVE MED-22(SPRAK-IX) TO RESP-TEMFSINF            
095400                        MOVE MFS-ADD-SAETT-CURSOR                         
095500                                      TO RESP-FLKLAR-ATTR                 
095600                     END-IF                                               
095700                  ELSE                                                    
095800                     PERFORM F-KONTROLL-REG-UPPDAT-AV-ERS                 
095900                     IF INPUT-RETT = JA                                   
096000                        IF REQU-FLKLAR = JA OR YES                        
096100                         PERFORM S16-SKAPA-VR-TRANS                       
096200                         PERFORM S24-TRANS-TILL-BASL                      
096300                         PERFORM S25-TRANS-TILL-PPMS                      
096400                         PERFORM S27-TRANS-TILL-WDR5                      
096500                         PERFORM S28-SKAPA-B65-TRANS                      
096600                        END-IF                                            
096700                     ELSE                                                 
096800                        MOVE FEL-3(SPRAK-IX) TO RESP-TEMFSFEL             
096900                     END-IF                                               
097000                  END-IF                                                  
097100               END-IF                                                     
097200             ELSE                                                         
097300                MOVE FEL-3(SPRAK-IX) TO RESP-TEMFSFEL                     
097400             END-IF                                                       
097500          ELSE                                                            
097600             MOVE FEL-3(SPRAK-IX) TO RESP-TEMFSFEL                        
097700          END-IF                                                          
097800          PERFORM S20-ROER-EJ-FAELT                                       
097900        END-IF                                                            
098000       ELSE                                                               
098100          IF REQU-QUERY                                                   
098200             PERFORM G-LAS                                                
098300          END-IF                                                          
098400       END-IF                                                             
098500     ELSE                                                                 
098600       MOVE FEL-1(SPRAK-IX) TO RESP-TEMFSFEL                              
098700       MOVE FEL-ERR-IDARTNR TO RESP-IDMSG-ERROR                           
098710       MOVE 'PART NUM INVALID '                                           
098720                            TO RESP-FELTEXT                               
098800       MOVE NEJ             TO INPUT-RETT                                 
098900     END-IF                                                               
099000                                                                          
099100     IF INPUT-RETT = NEJ                                                  
099200        MOVE FEL TO RESP-KDSVAR                                           
099300     END-IF                                                               
099400                                                                          
099500     MOVE ZERO TO RETURN-CODE                                             
099600     GOBACK                                                               
099700     .                                                                    
099800     EJECT                                                                
099900 A-INIT SECTION.                                                          
100000                                                                          
100100     MOVE LOW-VALUE                   TO RESP-W111ERUT                    
100110     MOVE 001                         TO RESP-IDMSGVER                    
100111     MOVE SPACE                       TO RESP-KDSVAR                      
100112                                         RESP-IDMSG-ERROR                 
100113                                         RESP-IDMSG-INFO                  
100114                                         RESP-IDELMT-ERROR                
100115                                         RESP-TEMFSFEL                    
100116                                         RESP-TEMFSINF                    
100117                                         RESP-FELTEXT                     
100118                                                                          
100120*SET ROW COUNT                                                            
100121     MOVE REQU-KVRADER-MAX9           TO RESP-KVRADER-MAX9                
100122                                         MAX-RAD                          
100123     PERFORM MFS-FORM-ATTR                                                
100124                                                                          
100139*INITIALIZE FLAGS                                                         
100140     MOVE JA                          TO INPUT-RETT                       
100141                                         LAS-VIDARE                       
100150                                         UPPDATERING-TILLATEN             
100170                                         RADER-UPPDATERADE                
100180                                                                          
100200     MOVE NEJ                         TO TEXT-FINNS                       
100220                                         FLFORTS                          
100240                                         FLTCPLM                          
100260                                         FINNS-PA-ACTION-FILE             
100280                                         UPPDAT-TIERSDAT-TEARTNOT         
100291                                         WS-EXTERN-SATS                   
100293                                         IDAO-FINNS                       
100295                                         SW-IDAO-FIXAD                    
100297                                         SW-TOMMA-IDAO-FINNS              
100299                                         SW-SUPPLIER-OK                   
100301                                         PPMS-TRANS                       
100303                                         WS-FLGEMART                      
100305                                         FL-NYPONART                      
100306                                         SW-TIKO                          
100307* TO BE USED FOR VALIDATIONS LATER IN THE PROGRAM                         
100308     MOVE REQU-IDDC                   TO WS-IDDC                          
100309*                                                                         
100310     IF REQU-KDARTSYS  = 'TC'                                             
100400        MOVE JA                       TO FLTCPLM                          
100500     END-IF                                                               
100600                                                                          
100630     ACCEPT DAGENS-DATUM FROM DATE                                        
100800     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-AAAAMMDD               
100900                                                                          
101000     MOVE SPACE                       TO RESP-TEMFSFEL                    
101100                                         RESP-TEMFSINF                    
101300                                                                          
101400     IF REQU-IDSPRAK    = 'EN'                                            
101500       MOVE +2    TO SPRAK-IX                                             
101600       MOVE 'GB ' TO W-IDSKYLT                                            
101700     ELSE                                                                 
101800       MOVE +1    TO SPRAK-IX                                             
101900       MOVE 'S  ' TO W-IDSKYLT                                            
102000     END-IF                                                               
102100     .                                                                    
102200     EJECT                                                                
102210 MFS-FORM-ATTR SECTION.                                                   
102220                                                                          
102231     MOVE MFS-FORMAT-DEFAULT-ATTR     TO RESP-DIERS-ERS-ATTR              
102232                                         RESP-KDERS-ATTR                  
102233                                         RESP-IDAO-ATTR                   
102234                                         RESP-TIERSDAT-PREL-ATTR          
102235                                         RESP-TEARTNOT-ATTR               
102236                                         RESP-FLKLAR-ATTR                 
102237     MOVE +1 TO RAD-IX                                                    
102238     PERFORM UNTIL RAD-IX > RESP-KVRADER-MAX9                             
102239        MOVE MFS-FORMAT-DEFAULT-ATTR  TO                                  
102240                                   RESP-IDKORTNR-ATTR(RAD-IX)             
102241                                   RESP-IDARTNR-TILLK-ATTR(RAD-IX)        
102242                                   RESP-DIERS-TILLK-ATTR(RAD-IX)          
102243                                   RESP-BEERS-ATTR(RAD-IX)                
102244        ADD +1 TO RAD-IX                                                  
102245     END-PERFORM                                                          
102294     .                                                                    
102295     SKIP2                                                                
102300 B-KOLLA-INDATA SECTION.                                                  
102400     SKIP2                                                                
102500     MOVE NEJ                TO RADER-UPPDATERADE                         
102600                                UPPDAT-TIERSDAT-TEARTNOT                  
102700     MOVE REQU-KDERS         TO WS-KDERS                                  
102800     MOVE REQU-TIERSDAT-PREL TO XX-TIERSDAT-PREL                          
102900     MOVE '+'                TO XX-DAG                                    
103000     MOVE XX-TIERSDAT-PREL   TO REQU-TIERSDAT-PREL                        
103100                                                                          
103200     IF REQU-KDERS = ALL '+'                                              
103300       IF REQU-TIERSDAT-PREL = ALL '+'                                    
103400          AND REQU-DIERS-ERS = ALL '+'                                    
103500          AND REQU-IDAO = ALL '+' AND REQU-TEARTNOT = ALL '+'             
103600          PERFORM BA-KOLLA-RADER                                          
103700       ELSE                                                               
103800          IF REQU-DIERS-ERS = ALL '+' AND REQU-IDAO = ALL '+'             
103900             AND REQU-TEARTNOT = ALL '+'                                  
104000             MOVE +1 TO RAD-IX                                            
104100             PERFORM UNTIL RAD-IX > MAX-RAD                               
104200                IF REQU-IDARTNR-TILLK(RAD-IX) = ALL '+' AND               
104300                   REQU-BEERS(RAD-IX) = ALL '+' AND                       
104400                   REQU-DIERS-TILLK(RAD-IX) = ALL '+'                     
104500                   CONTINUE                                               
104600                ELSE                                                      
104700                   MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR             
104800                   MOVE NEJ TO INPUT-RETT                                 
104900                   MOVE '001' TO RESP-IDMSG-ERROR                         
104940                   MOVE 'REPL-CODE IS NEEDED'                             
104970                              TO RESP-FELTEXT                             
105000                END-IF                                                    
105100                ADD +1 TO RAD-IX                                          
105200             END-PERFORM                                                  
105300             PERFORM BC-KOLLA-TIERSDAT-PREL                               
105400          ELSE                                                            
105500             IF REQU-DIERS-ERS = ALL '+' AND REQU-IDAO = ALL '+'          
105600                AND REQU-TIERSDAT-PREL = ALL '+'                          
105700                MOVE +1 TO RAD-IX                                         
105800                PERFORM UNTIL RAD-IX > MAX-RAD                            
105900                   IF REQU-IDARTNR-TILLK(RAD-IX) = ALL '+' AND            
106000                      REQU-BEERS(RAD-IX) = ALL '+' AND                    
106100                      REQU-DIERS-TILLK(RAD-IX) = ALL '+'                  
106200                      CONTINUE                                            
106300                   ELSE                                                   
106400                      MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR          
106500                      MOVE NEJ TO INPUT-RETT                              
106600                      MOVE '002' TO RESP-IDMSG-ERROR                      
106640                      MOVE 'REPL CODE IS NEEDED'                          
106670                                 TO RESP-FELTEXT                          
106700                   END-IF                                                 
106800                   ADD +1 TO RAD-IX                                       
106900                END-PERFORM                                               
107000                IF REQU-TEARTNOT = ALL '+' OR SPACE                       
107100                   MOVE SPACE TO RESP-TEARTNOT                            
107200                   MOVE NEJ TO INPUT-RETT                                 
107300                   MOVE '003' TO RESP-IDMSG-ERROR                         
107310                   MOVE 'REPL NOTE IS NEEDED'                             
107320                              TO RESP-FELTEXT                             
107400                ELSE                                                      
107500                   MOVE MFS-ALFA-FAELT-RAETT TO                           
107600                                      RESP-TEARTNOT-ATTR                  
107700                END-IF                                                    
107800                IF INPUT-RETT = JA                                        
107900                   MOVE JA TO UPPDAT-TIERSDAT-TEARTNOT                    
108000                END-IF                                                    
108100             ELSE                                                         
108200                MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR                
108300                MOVE NEJ TO INPUT-RETT                                    
108400                MOVE '004' TO RESP-IDMSG-ERROR                            
108500                MOVE MED-3(SPRAK-IX) TO RESP-TEMFSINF                     
108510                MOVE 'INVALID REPLACE-CODE'                               
108520                                     TO RESP-FELTEXT                      
108600             END-IF                                                       
108700          END-IF                                                          
108800       END-IF                                                             
108900                                                                          
109000     ELSE                                                                 
109100**** OM REQU-KDERS IFYLLT                                                 
109200       IF REQU-KDERS NUMERIC                                              
109300          MOVE REQU-KDERS TO WS-KDERS                                     
109400          IF REQU-KDERS = '01' OR '02' OR '03' OR '04' OR '05'            
109500                          OR '06' OR '07' OR '08' OR  '09' OR             
109600                         '21' OR '22' OR '23' OR '24' OR '25' OR          
109700                         '26' OR '27' OR '28' OR '29' OR                  
109800                         '00' OR '52'                                     
109900            MOVE MFS-ALFA-FAELT-RAETT TO RESP-KDERS-ATTR                  
110000                                                                          
110100            IF REQU-KDERS = ZERO                                          
110200                                                                          
110300              IF ERSATT-KDERS-UTG > 0                                     
110400                 MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR               
110500                 MOVE NEJ TO INPUT-RETT                                   
110600                 MOVE '005' TO RESP-IDMSG-ERROR                           
110700                 MOVE MED-17(SPRAK-IX)   TO RESP-TEMFSINF                 
110710                 MOVE 'DELETED PART NUM' TO RESP-FELTEXT                  
110800              ELSE                                                        
110900                                                                          
111000                 IF REQU-TIERSDAT-PREL = ALL '+'                          
111100                   AND REQU-TEARTNOT = ALL '+' AND                        
111200                   REQU-DIERS-ERS = ALL '+'                               
111300                    MOVE +1 TO RAD-IX                                     
111400                    PERFORM UNTIL RAD-IX > MAX-RAD                        
111500                     IF REQU-IDARTNR-TILLK(RAD-IX) = ALL '+' AND          
111600                        REQU-BEERS(RAD-IX) = ALL '+' AND                  
111700                        REQU-DIERS-TILLK(RAD-IX) = ALL '+'                
111800                        CONTINUE                                          
111900                     ELSE                                                 
112000                      MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR          
112100                      MOVE NEJ TO INPUT-RETT                              
112200                      MOVE '006' TO RESP-IDMSG-ERROR                      
112240                      MOVE 'REPL PART NOT VALID FOR CODE 0'               
112270                                 TO RESP-FELTEXT                          
112300                     END-IF                                               
112400                     ADD +1 TO RAD-IX                                     
112500                    END-PERFORM                                           
112600                    IF REQU-IDAO = ALL '+' OR SPACE                       
112700                       MOVE SPACE TO RESP-IDAO                            
112800                    ELSE                                                  
112900                       MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDAO-ATTR        
113000                    END-IF                                                
113100                 ELSE                                                     
113200                  MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR              
113300                  MOVE NEJ TO INPUT-RETT                                  
113400                  MOVE '007' TO RESP-IDMSG-ERROR                          
113410                  MOVE 'CLEAR REPLACE DATE/QUANTITY'                      
113420                             TO RESP-FELTEXT                              
113500                 END-IF                                                   
113600              END-IF                                                      
113700                                                                          
113800            ELSE                                                          
113900*********   OM REQU-KDERS > 00                                            
114000              IF WS-KDERS-2 = 3 OR 6                                      
114100                IF REQU-TIERSDAT-PREL NOT = ALL '+'                       
114200                 MOVE SPACE             TO XX-DAG                         
114300                 MOVE XX-TIERSDAT-PREL  TO REQU-TIERSDAT-PREL             
114400                 IF REQU-TIERSDAT-PREL NOT = SPACE                        
114500                  MOVE '1'              TO XX-DAG                         
114600                  MOVE XX-TIERSDAT-PREL  TO REQU-TIERSDAT-PREL            
114700                  IF REQU-TIERSDAT-PREL NUMERIC                           
114800                    MOVE REQU-TIERSDAT-PREL TO WS-TIERSDAT-PREL           
114900                    IF WS-VECKA-PREL > 00 AND < 54                        
115000                       MOVE 'AAVVD  ' TO DAT-KDDATFORM                    
115100                       MOVE REQU-TIERSDAT-PREL TO DAT-I-TIDATUM           
115200                       CALL WDATKONV USING DAT-KDDATFORM                  
115300                                           DAT-I-TIDATUM                  
115400                                           DAT-O-TIDATUM                  
115500                                           DAT-KDSVAR                     
115600                       IF DAT-KDSVAR-OK                                   
115700                         MOVE DAT-TIAAMMDD   TO TMP1-YYMMDD               
115800                         MOVE DAGENS-DATUM   TO TMP2-YYMMDD               
115900                         PERFORM WY2000P1                                 
116000                         IF TMP1-YYMMDD > TMP2-YYMMDD                     
116100                            MOVE MFS-ALFA-FAELT-RAETT                     
116200                                      TO RESP-TIERSDAT-PREL-ATTR          
116300                         ELSE                                             
116400                            MOVE MFS-ALFA-FAELT-FEL TO                    
116500                                RESP-TIERSDAT-PREL-ATTR                   
116600                            MOVE NEJ TO INPUT-RETT                        
116700                            MOVE MED-5(SPRAK-IX) TO RESP-TEMFSINF         
116710                            MOVE 'INVALID REPL DATE'                      
116720                                                 TO RESP-FELTEXT          
116800                            MOVE '008' TO RESP-IDMSG-ERROR                
116900                         END-IF                                           
117000                      ELSE                                                
117100                         MOVE MFS-ALFA-FAELT-FEL TO                       
117200                             RESP-TIERSDAT-PREL-ATTR                      
117300                         MOVE NEJ TO INPUT-RETT                           
117400                         MOVE MED-5(SPRAK-IX) TO RESP-TEMFSINF            
117410                         MOVE 'REPL-DATE INVALID'                         
117420                                              TO RESP-FELTEXT             
117500                         MOVE '009' TO RESP-IDMSG-ERROR                   
117600                      END-IF                                              
117700                    ELSE                                                  
117800                      MOVE MFS-ALFA-FAELT-FEL                             
117900                           TO RESP-TIERSDAT-PREL-ATTR                     
118000                      MOVE NEJ TO INPUT-RETT                              
118100                      MOVE MED-5(SPRAK-IX) TO RESP-TEMFSINF               
118110                      MOVE 'REPLACE DATE INVALID'                         
118120                                           TO RESP-FELTEXT                
118200                      MOVE '010' TO RESP-IDMSG-ERROR                      
118300                    END-IF                                                
118400                  ELSE                                                    
118500                    MOVE MFS-ALFA-FAELT-FEL                               
118600                         TO RESP-TIERSDAT-PREL-ATTR                       
118700                    MOVE NEJ TO INPUT-RETT                                
118800                    MOVE MED-4(SPRAK-IX) TO RESP-TEMFSINF                 
118810                    MOVE 'REPLACE-DATE INVALID'                           
118820                                         TO RESP-FELTEXT                  
118900                    MOVE '011' TO RESP-IDMSG-ERROR                        
119000                  END-IF                                                  
119100                 ELSE                                                     
119200                   MOVE MFS-ALFA-FAELT-FEL                                
119300                        TO RESP-TIERSDAT-PREL-ATTR                        
119400                   MOVE NEJ TO INPUT-RETT                                 
119500                   MOVE MED-4(SPRAK-IX) TO RESP-TEMFSINF                  
119510                   MOVE 'INVALID REPL DATE'                               
119520                                        TO RESP-FELTEXT                   
119600                   MOVE '012' TO RESP-IDMSG-ERROR                         
119700                 END-IF                                                   
119800               ELSE                                                       
119900                 MOVE MFS-ALFA-FAELT-FEL                                  
120000                        TO RESP-TIERSDAT-PREL-ATTR                        
120100                 MOVE NEJ TO INPUT-RETT                                   
120200                 MOVE MED-4(SPRAK-IX) TO RESP-TEMFSINF                    
120210                 MOVE 'REPLACE DATE MISSING'                              
120220                                      TO RESP-FELTEXT                     
120300                 MOVE '013' TO RESP-IDMSG-ERROR                           
120400               END-IF                                                     
120500           ELSE                                                           
120600              IF REQU-TIERSDAT-PREL = ALL '+'                             
120700                 CONTINUE                                                 
120800              ELSE                                                        
120900                MOVE SPACE TO XX-DAG                                      
121000                MOVE XX-TIERSDAT-PREL TO REQU-TIERSDAT-PREL               
121100                IF REQU-TIERSDAT-PREL = SPACE                             
121200                   CONTINUE                                               
121300                ELSE                                                      
121400                   MOVE MFS-ALFA-FAELT-FEL                                
121500                               TO RESP-TIERSDAT-PREL-ATTR                 
121600                   MOVE NEJ TO INPUT-RETT                                 
121610                   MOVE 'CLEAR REPLACE DATE'                              
121620                                        TO RESP-FELTEXT                   
121700                   MOVE '014' TO RESP-IDMSG-ERROR                         
121800                END-IF                                                    
121900              END-IF                                                      
122000           END-IF                                                         
122100                                                                          
122200           IF REQU-KDERS = '09' OR '29' OR '52'                           
122300              IF ERSATT-FLIART = JA                                       
122400                 MOVE REQU-IDARTNR-KEY TO W-IDARTNR-S                     
122500                 MOVE SPACE TO W-IDLEVNR-S                                
122600                               W-BELEVART-S                               
122700                 PERFORM IMS-GET-SATB-CSEQ                                
122800                 PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT               
122900                  IF SATB-STR-IDLEVNR = '1002 '                           
123000                   IF SATB-STR-IDARTNR < 100000000                        
123100                    IF SATB-STR-TIBORT = 0                                
123200                     MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD              
123300                     MOVE DAGENS-DATUM        TO TMP2-YYMMDD              
123400                     PERFORM WY2000P1                                     
123500                     IF TMP1-YYMMDD > TMP2-YYMMDD                         
123600                       MOVE SATB-STR-IDARTNR TO W-IDARTNR                 
123700                       PERFORM IMS-GET-ARTC01                             
123800                       PERFORM IMS-GET-ARTC11                             
123900                       IF SEGMENT-FINNS                                   
124000                          IF CLAG-KDERS < 09                              
124100                            MOVE NEJ TO INPUT-RETT                        
124200                            MOVE MFS-ALFA-FAELT-FEL TO                    
124300                                 RESP-KDERS-ATTR                          
124400                            MOVE MED-9(SPRAK-IX) TO RESP-TEMFSINF         
124410                            MOVE 'PART NUM IS INCLUDED IN A KIT'          
124420                                                 TO RESP-FELTEXT          
124500                            MOVE '015' TO RESP-IDMSG-ERROR                
124600                          END-IF                                          
124700                       END-IF                                             
124800                     END-IF                                               
124900                    END-IF                                                
125000                   END-IF                                                 
125100                  END-IF                                                  
125200                  PERFORM IMS-GET-SATB-CSEQ                               
125300                 END-PERFORM                                              
125400              END-IF                                                      
125500           END-IF                                                         
125600                                                                          
125700           IF ERSATT-KDERS-UTG > 0                                        
125800              IF WS-KDERS-1 = 2                                           
125900                 CONTINUE                                                 
126000              ELSE                                                        
126100                 MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR               
126200                 MOVE NEJ TO INPUT-RETT                                   
126210                 MOVE 'REPL CODE SHOULD START WITH 2'                     
126220                          TO RESP-FELTEXT                                 
126300                 MOVE '016' TO RESP-IDMSG-ERROR                           
126400              END-IF                                                      
126500           END-IF                                                         
126600                                                                          
126700           IF REQU-IDAO = ALL '+' OR SPACE                                
126800              IF ERSATT-KDERS-C1 = 00                                     
126900                 AND REQU-KDERS = '09'                                    
127000                 MOVE SPACE TO RESP-IDAO                                  
127100              ELSE                                                        
127200                 IF (ERSATT-KDERS-C1 = 21 OR 22 OR 23 OR 24 OR 25         
127300                 OR 26 OR 27 OR 28 OR 29)                                 
127400                    MOVE SPACE TO RESP-IDAO                               
127500                 ELSE                                                     
127600                    MOVE MFS-ALFA-FAELT-FEL TO RESP-IDAO-ATTR             
127700                    MOVE NEJ TO INPUT-RETT                                
127710                    MOVE 'REPL-CODE SHOULD START WITH 2'                  
127720                             TO RESP-FELTEXT                              
127800                    MOVE MED-11(SPRAK-IX) TO RESP-TEMFSINF                
127900                    MOVE '017' TO RESP-IDMSG-ERROR                        
128000                 END-IF                                                   
128100              END-IF                                                      
128200           ELSE                                                           
128300              MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDAO-ATTR                 
128400           END-IF                                                         
128500                                                                          
128600           IF REQU-DIERS-ERS = ALL '+' OR SPACE                           
128700              MOVE MFS-NUM-FAELT-FEL TO RESP-DIERS-ERS-ATTR               
128800              MOVE NEJ TO INPUT-RETT                                      
128810              MOVE 'REPLACED PART QUANTITY MISSING'                       
128820                                   TO RESP-FELTEXT                        
128900              MOVE MED-1(SPRAK-IX) TO RESP-TEMFSINF                       
129000              MOVE '018' TO RESP-IDMSG-ERROR                              
129100           ELSE                                                           
129200              MOVE REQU-DIERS-ERS TO DEC-IDFRIDATA                        
129300              MOVE 3             TO DEC-KVHELTAL                          
129400              MOVE 3             TO DEC-KVDECIMAL                         
129500              CALL WDECEDIT USING DEC-WDECAREA                            
129600              IF DEC-KDSVAR-OK                                            
129700                 IF DEC-IDEDITDATA = ZERO                                 
129800                    MOVE MFS-NUM-FAELT-FEL TO RESP-DIERS-ERS-ATTR         
129900                    MOVE NEJ TO INPUT-RETT                                
129910                    MOVE 'REPLACED PART QUANTITY INVALID'                 
129920                                         TO RESP-FELTEXT                  
130000                    MOVE '019' TO RESP-IDMSG-ERROR                        
130100                 ELSE                                                     
130200                    MOVE MFS-NUM-FAELT-RAETT                              
130300                                           TO RESP-DIERS-ERS-ATTR         
130400                 END-IF                                                   
130500              ELSE                                                        
130600                 MOVE MFS-NUM-FAELT-FEL TO                                
130700                      RESP-DIERS-ERS-ATTR                                 
130800                 MOVE NEJ TO INPUT-RETT                                   
130810                 MOVE 'INVALID REPLACED PART QUANTITY'                    
130820                                      TO RESP-FELTEXT                     
130900                 MOVE '020' TO RESP-IDMSG-ERROR                           
131000              END-IF                                                      
131100           END-IF                                                         
131200                                                                          
131300            IF REQU-TEARTNOT = ALL '+' OR SPACE                           
131400               MOVE SPACE TO RESP-TEARTNOT                                
131500            ELSE                                                          
131600               MOVE MFS-ALFA-FAELT-RAETT TO RESP-TEARTNOT-ATTR            
131700            END-IF                                                        
131800                                                                          
131900            PERFORM BA-KOLLA-RADER                                        
132000            END-IF                                                        
132100          ELSE                                                            
132200             MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR                   
132300             MOVE NEJ TO INPUT-RETT                                       
132310             MOVE 'INVALID REPL CODE'                                     
132320                                  TO RESP-FELTEXT                         
132400             MOVE MED-3(SPRAK-IX) TO RESP-TEMFSINF                        
132500             MOVE '021' TO RESP-IDMSG-ERROR                               
132600          END-IF                                                          
132700       ELSE                                                               
132800         MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR                       
132900         MOVE NEJ TO INPUT-RETT                                           
132910         MOVE 'INVALID REPL-CODE'                                         
132920                              TO RESP-FELTEXT                             
133000         MOVE MED-3(SPRAK-IX) TO RESP-TEMFSINF                            
133100         MOVE '022' TO RESP-IDMSG-ERROR                                   
133200       END-IF                                                             
133300     END-IF                                                               
133400                                                                          
133500     IF REQU-KDERS = ALL '+'                                              
133600        PERFORM BB-KOLLA-PREL-ERSKOD                                      
133700     END-IF                                                               
133800                                                                          
133900     IF REQU-FLKLAR = JA OR YES OR NEJ                                    
134000        MOVE MFS-ALFA-FAELT-RAETT TO RESP-FLKLAR-ATTR                     
134100     ELSE                                                                 
134200        MOVE MFS-ALFA-FAELT-FEL TO RESP-FLKLAR-ATTR                       
134300        MOVE NEJ TO INPUT-RETT                                            
134400        MOVE '023' TO RESP-IDMSG-ERROR                                    
134500     END-IF                                                               
134600     .                                                                    
134700     EJECT                                                                
134800 BA-KOLLA-RADER SECTION.                                                  
134900     SKIP2                                                                
135000     IF REQU-KDERS = ALL '+'                                              
135100        MOVE ERSATT-KDERS-C1 TO WS-KDERS                                  
135200     ELSE                                                                 
135300        MOVE REQU-KDERS TO WS-KDERS                                       
135400     END-IF                                                               
135500                                                                          
135600     MOVE +1 TO RAD-IX                                                    
135700     PERFORM UNTIL RAD-IX > MAX-RAD                                       
135800      MOVE 'F' TO DEC-KDSVAR                                              
135900                                                                          
136000      IF REQU-IDARTNR-TILLK(RAD-IX) = ALL '+' AND                         
136100         REQU-DIERS-TILLK(RAD-IX) = ALL '+' AND                           
136200         REQU-BEERS(RAD-IX) = ALL '+'                                     
136300         CONTINUE                                                         
136400      ELSE                                                                
136500       IF REQU-IDKORTNR(RAD-IX) = SPACE                                   
136600          CONTINUE                                                        
136700       ELSE                                                               
136800         INSPECT REQU-IDKORTNR(RAD-IX) REPLACING ALL SPACE                
136900         BY ZERO                                                          
137000       END-IF                                                             
137100       EVALUATE TRUE                                                      
137200       WHEN REQU-IDKORTNR(RAD-IX) = ALL '+'                               
137300          IF (REQU-IDARTNR-TILLK(RAD-IX) = ALL '+' OR                     
137400              SPACE) AND (REQU-DIERS-TILLK(RAD-IX) = SPACE                
137500              OR ALL '+') AND (REQU-BEERS(RAD-IX) = SPACE                 
137600              OR ALL '+')                                                 
137700            MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDKORTNR-ATTR               
137800                         (RAD-IX)                                         
137900          ELSE                                                            
138000            MOVE MFS-ALFA-FAELT-FEL TO RESP-IDKORTNR-ATTR(RAD-IX)         
138100            MOVE NEJ TO INPUT-RETT                                        
138200            MOVE '024' TO RESP-IDMSG-ERROR                                
138300          END-IF                                                          
138400       WHEN REQU-IDKORTNR(RAD-IX) = SPACE                                 
138500          IF (REQU-IDARTNR-TILLK(RAD-IX) = ALL '+' OR                     
138600              SPACE) AND (REQU-DIERS-TILLK(RAD-IX) = SPACE                
138700              OR ALL '+') AND (REQU-BEERS(RAD-IX) = SPACE                 
138800              OR ALL '+')                                                 
138900            MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDKORTNR-ATTR               
139000                         (RAD-IX)                                         
139100          ELSE                                                            
139200            MOVE MFS-ALFA-FAELT-FEL TO RESP-IDKORTNR-ATTR                 
139300                         (RAD-IX)                                         
139400            MOVE NEJ TO INPUT-RETT                                        
139500            MOVE '025' TO RESP-IDMSG-ERROR                                
139600          END-IF                                                          
139700       WHEN REQU-IDKORTNR(RAD-IX) NUMERIC                                 
139800          IF REQU-IDKORTNR(RAD-IX) = ZERO                                 
139900             IF (REQU-IDARTNR-TILLK(RAD-IX) = ALL '+' OR                  
140000                 SPACE) AND (REQU-DIERS-TILLK(RAD-IX) = SPACE             
140100                 OR ALL '+') AND (REQU-BEERS(RAD-IX) = SPACE              
140200                 OR ALL '+')                                              
140300                 MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDKORTNR-ATTR          
140400                              (RAD-IX)                                    
140500             ELSE                                                         
140600                 MOVE MFS-ALFA-FAELT-FEL TO RESP-IDKORTNR-ATTR            
140700                              (RAD-IX)                                    
140800                 MOVE NEJ TO INPUT-RETT                                   
140900                 MOVE '026' TO RESP-IDMSG-ERROR                           
141000             END-IF                                                       
141100          ELSE                                                            
141200             MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDKORTNR-ATTR              
141300                             (RAD-IX)                                     
141400          END-IF                                                          
141500*      ELSE                                                               
141600       WHEN OTHER                                                         
141700          MOVE MFS-ALFA-FAELT-FEL TO RESP-IDKORTNR-ATTR                   
141800                          (RAD-IX)                                        
141900          MOVE NEJ TO INPUT-RETT                                          
142000          MOVE '027' TO RESP-IDMSG-ERROR                                  
142100       END-EVALUATE                                                       
142200                                                                          
142300       IF REQU-IDARTNR-TILLK(RAD-IX) = SPACE                              
142400          CONTINUE                                                        
142500       ELSE                                                               
142600          INSPECT REQU-IDARTNR-TILLK(RAD-IX)                              
142700              REPLACING ALL SPACE BY ZERO                                 
142800       END-IF                                                             
142900                                                                          
143000       IF REQU-DIERS-TILLK(RAD-IX) = SPACE                                
143100          CONTINUE                                                        
143200       ELSE                                                               
143300          INSPECT REQU-DIERS-TILLK(RAD-IX)                                
143400              REPLACING ALL SPACE BY ZERO                                 
143500       END-IF                                                             
143600       IF (WS-KDERS-2 = 1 OR 2 OR 3 OR 7) AND (WS-KDERS NOT               
143700         = 52)                                                            
143800        EVALUATE TRUE                                                     
143900         WHEN REQU-IDARTNR-TILLK(RAD-IX) = SPACE                          
144000           MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDARTNR-TILLK-ATTR           
144100                                        (RAD-IX)                          
144200           IF REQU-DIERS-TILLK(RAD-IX) = SPACE OR ZERO                    
144300             MOVE MFS-ALFA-FAELT-RAETT TO RESP-DIERS-TILLK-ATTR           
144400                                     (RAD-IX)                             
144500           ELSE                                                           
144600             MOVE MFS-ALFA-FAELT-FEL                                      
144700                  TO RESP-DIERS-TILLK-ATTR(RAD-IX)                        
144800             MOVE NEJ TO INPUT-RETT                                       
144900             MOVE '028' TO RESP-IDMSG-ERROR                               
144960             MOVE 'NEW PART NUM MISSING'                                  
144980                        TO RESP-FELTEXT                                   
145000           END-IF                                                         
145100         WHEN REQU-IDARTNR-TILLK(RAD-IX) = ALL '+'                        
145200           IF REQU-DIERS-TILLK(RAD-IX) = ALL '+'                          
145300              MOVE MFS-ALFA-FAELT-RAETT TO RESP-DIERS-TILLK-ATTR          
145400                     (RAD-IX)                                             
145500           ELSE                                                           
145600              IF REQU-DIERS-TILLK(RAD-IX) = SPACE                         
145700                 MOVE MFS-ALFA-FAELT-RAETT TO                             
145800                        RESP-DIERS-TILLK-ATTR(RAD-IX)                     
145900              ELSE                                                        
146000                 MOVE MFS-ALFA-FAELT-FEL                                  
146100                             TO RESP-DIERS-TILLK-ATTR(RAD-IX)             
146200                 MOVE NEJ TO INPUT-RETT                                   
146300                 MOVE '029' TO RESP-IDMSG-ERROR                           
146340                 MOVE 'NEW PART-NUM MISSING'                              
146360                            TO RESP-FELTEXT                               
146400              END-IF                                                      
146500           END-IF                                                         
146600         WHEN REQU-IDARTNR-TILLK(RAD-IX) NOT NUMERIC                      
146700           MOVE MFS-ALFA-FAELT-FEL TO RESP-IDARTNR-TILLK-ATTR             
146800                                        (RAD-IX)                          
146900           MOVE NEJ TO INPUT-RETT                                         
147000           MOVE '030' TO RESP-IDMSG-ERROR                                 
147010*                                                                         
147040           STRING 'INVALID PART NUM:'   DELIMITED BY SIZE                 
147050             REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE                 
147060                    INTO RESP-FELTEXT                                     
147080                                                                          
147100           IF REQU-DIERS-TILLK(RAD-IX) = ALL '+'                          
147200              MOVE MFS-ALFA-FAELT-FEL TO RESP-DIERS-TILLK-ATTR            
147300                        (RAD-IX)                                          
147400              MOVE NEJ TO INPUT-RETT                                      
147500              MOVE '031' TO RESP-IDMSG-ERROR                              
147540              STRING 'QUANTITY MISSING FOR:' DELIMITED BY SIZE            
147550                  REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE            
147560                       INTO RESP-FELTEXT                                  
147600           ELSE                                                           
147700              PERFORM BAB-KOLLA-DIERS-TILLK                               
147800                                                                          
147900              IF DEC-KDSVAR-OK                                            
148000                IF DEC-IDEDITDATA = ZERO                                  
148100                  MOVE MFS-ALFA-FAELT-FEL TO RESP-DIERS-TILLK-ATTR        
148200                                             (RAD-IX)                     
148300                  MOVE NEJ TO INPUT-RETT                                  
148400                  MOVE '032' TO RESP-IDMSG-ERROR                          
148440                  STRING 'INVALID QUANTITY FOR '                          
148450                                               DELIMITED BY SIZE          
148460                    REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE          
148470                           INTO RESP-FELTEXT                              
148500                ELSE                                                      
148600                  MOVE MFS-ALFA-FAELT-RAETT                               
148700                          TO RESP-DIERS-TILLK-ATTR(RAD-IX)                
148800                END-IF                                                    
148900             ELSE                                                         
149000                MOVE MFS-ALFA-FAELT-FEL TO                                
149100                      RESP-DIERS-TILLK-ATTR(RAD-IX)                       
149200                MOVE NEJ TO INPUT-RETT                                    
149300                MOVE '033' TO RESP-IDMSG-ERROR                            
149340                STRING 'QUANTITY INVALID FOR:'                            
149350                                             DELIMITED BY SIZE            
149360                  REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE            
149370                         INTO RESP-FELTEXT                                
149400             END-IF                                                       
149500           END-IF                                                         
149600         WHEN REQU-IDARTNR-TILLK(RAD-IX) NUMERIC                          
149700           MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDARTNR-TILLK-ATTR           
149800                                       (RAD-IX)                           
149900           IF REQU-DIERS-TILLK(RAD-IX) = ALL '+'                          
150000              MOVE MFS-ALFA-FAELT-FEL TO RESP-DIERS-TILLK-ATTR            
150100                        (RAD-IX)                                          
150200              MOVE NEJ TO INPUT-RETT                                      
150300              MOVE '034' TO RESP-IDMSG-ERROR                              
150340              STRING 'WRONG QUANTITY FOR:'  DELIMITED BY SIZE             
150350                 REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE             
150360                       INTO RESP-FELTEXT                                  
150400           ELSE                                                           
150500              PERFORM BAB-KOLLA-DIERS-TILLK                               
150600                                                                          
150700              IF DEC-KDSVAR-OK                                            
150800                IF DEC-IDEDITDATA = ZERO                                  
150900                  MOVE MFS-ALFA-FAELT-FEL TO RESP-DIERS-TILLK-ATTR        
151000                                             (RAD-IX)                     
151100                  MOVE NEJ TO INPUT-RETT                                  
151200                  MOVE '035' TO RESP-IDMSG-ERROR                          
151240                  STRING 'INVALID-QUANTITY FOR:'                          
151241                                               DELIMITED BY SIZE          
151250                    REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE          
151260                           INTO RESP-FELTEXT                              
151300                ELSE                                                      
151400                  MOVE MFS-ALFA-FAELT-RAETT                               
151500                          TO RESP-DIERS-TILLK-ATTR(RAD-IX)                
151600                END-IF                                                    
151700             ELSE                                                         
151800                MOVE MFS-ALFA-FAELT-FEL TO                                
151900                      RESP-DIERS-TILLK-ATTR(RAD-IX)                       
152000                MOVE NEJ TO INPUT-RETT                                    
152100                MOVE '036' TO RESP-IDMSG-ERROR                            
152140                STRING 'WRONG-QUANTITY FOR:'                              
152141                                             DELIMITED BY SIZE            
152150                  REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE            
152160                         INTO RESP-FELTEXT                                
152200             END-IF                                                       
152300           END-IF                                                         
152400        END-EVALUATE                                                      
152500                                                                          
152600         IF REQU-BEERS(RAD-IX) = ALL '+'                                  
152700             MOVE MFS-ALFA-FAELT-RAETT TO RESP-BEERS-ATTR(RAD-IX)         
152800         ELSE                                                             
152900            IF REQU-BEERS(RAD-IX) = SPACE                                 
153000               MOVE MFS-ALFA-FAELT-RAETT TO                               
153100                                    RESP-BEERS-ATTR(RAD-IX)               
153200            ELSE                                                          
153300               MOVE MFS-ALFA-FAELT-FEL TO                                 
153400                               RESP-BEERS-ATTR(RAD-IX)                    
153500               MOVE NEJ TO INPUT-RETT                                     
153600               MOVE '037' TO RESP-IDMSG-ERROR                             
153640               MOVE 'REPL TEXT MUST BE EMPTY:'                            
153650                          TO RESP-FELTEXT                                 
153700            END-IF                                                        
153800         END-IF                                                           
153900                                                                          
154000         PERFORM BAA-KOLLA-TILLK-ARTIKEL                                  
154100        END-IF                                                            
154200                                                                          
154300                                                                          
154400        IF WS-KDERS-2 = 4 OR 5 OR 6 OR 8                                  
154500         EVALUATE TRUE                                                    
154600          WHEN REQU-IDARTNR-TILLK(RAD-IX) = SPACE                         
154700           MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDARTNR-TILLK-ATTR           
154800                                        (RAD-IX)                          
154900           IF REQU-BEERS(RAD-IX) = ALL '+'                                
155000             MOVE MFS-ALFA-FAELT-RAETT TO RESP-BEERS-ATTR(RAD-IX)         
155100           ELSE                                                           
155200              IF REQU-BEERS(RAD-IX) = SPACE                               
155300                 MOVE MFS-ALFA-FAELT-RAETT TO                             
155400                                 RESP-BEERS-ATTR(RAD-IX)                  
155500              ELSE                                                        
155600                 MOVE MFS-ALFA-FAELT-FEL TO                               
155700                                 RESP-BEERS-ATTR(RAD-IX)                  
155800                 MOVE NEJ TO INPUT-RETT                                   
155900                 MOVE '038' TO RESP-IDMSG-ERROR                           
155910                 MOVE 'REPL TEXT IS MISSING'                              
155920                            TO RESP-FELTEXT                               
156000              END-IF                                                      
156100           END-IF                                                         
156200           IF REQU-DIERS-TILLK(RAD-IX) = SPACE OR ZERO                    
156300             MOVE MFS-ALFA-FAELT-RAETT TO RESP-DIERS-TILLK-ATTR           
156400                                          (RAD-IX)                        
156500           ELSE                                                           
156600             MOVE MFS-ALFA-FAELT-FEL                                      
156700                  TO RESP-DIERS-TILLK-ATTR(RAD-IX)                        
156800             MOVE NEJ TO INPUT-RETT                                       
156900             MOVE '039' TO RESP-IDMSG-ERROR                               
156910             MOVE 'REMOVE QUANTITY '                                      
156920                        TO RESP-FELTEXT                                   
157000           END-IF                                                         
157100          WHEN REQU-IDARTNR-TILLK(RAD-IX) = ALL '+'                       
157200             IF REQU-BEERS(RAD-IX) = ALL '+'                              
157300                IF REQU-FLTEXT(RAD-IX) = NEJ                              
157400                   MOVE MFS-ALFA-FAELT-RAETT                              
157500                         TO RESP-BEERS-ATTR(RAD-IX)                       
157600                ELSE                                                      
157700                   MOVE MFS-ALFA-FAELT-FEL                                
157800                         TO RESP-BEERS-ATTR(RAD-IX)                       
157900                   MOVE NEJ TO INPUT-RETT                                 
158000                   MOVE '040' TO RESP-IDMSG-ERROR                         
158040                   MOVE 'CLEAR REPL-TEXT'                                 
158070                              TO RESP-FELTEXT                             
158100                END-IF                                                    
158200             ELSE                                                         
158300                MOVE MFS-ALFA-FAELT-RAETT                                 
158400                      TO RESP-BEERS-ATTR(RAD-IX)                          
158500             END-IF                                                       
158600             IF REQU-DIERS-TILLK(RAD-IX) = ALL '+'                        
158700                MOVE MFS-ALFA-FAELT-RAETT TO RESP-DIERS-TILLK-ATTR        
158800                          (RAD-IX)                                        
158900             ELSE                                                         
159000                IF REQU-DIERS-TILLK(RAD-IX) = SPACE                       
159100                   MOVE MFS-ALFA-FAELT-RAETT TO                           
159200                       RESP-DIERS-TILLK-ATTR(RAD-IX)                      
159300                ELSE                                                      
159400                   MOVE MFS-ALFA-FAELT-FEL TO                             
159500                           RESP-DIERS-TILLK-ATTR(RAD-IX)                  
159600                   MOVE NEJ TO INPUT-RETT                                 
159700                   MOVE '041' TO RESP-IDMSG-ERROR                         
159740                   MOVE 'CLEAR QUANTITY'                                  
159770                              TO RESP-FELTEXT                             
159800                END-IF                                                    
159900             END-IF                                                       
160000          WHEN REQU-IDARTNR-TILLK(RAD-IX) NUMERIC                         
160100           MOVE MFS-ALFA-FAELT-RAETT TO RESP-IDARTNR-TILLK-ATTR           
160200                                        (RAD-IX)                          
160300           PERFORM BAB-KOLLA-DIERS-TILLK                                  
160400           IF DEC-KDSVAR-OK                                               
160500             IF DEC-IDEDITDATA = ZERO                                     
160600                MOVE MFS-ALFA-FAELT-FEL TO RESP-DIERS-TILLK-ATTR          
160700                                           (RAD-IX)                       
160800                MOVE NEJ TO INPUT-RETT                                    
160900                MOVE '042' TO RESP-IDMSG-ERROR                            
160940                STRING 'INVALID QUANTITY FOR-'                            
160950                                             DELIMITED BY SIZE            
160951                  REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE            
160960                         INTO RESP-FELTEXT                                
161000             ELSE                                                         
161100                MOVE MFS-ALFA-FAELT-RAETT TO RESP-DIERS-TILLK-ATTR        
161200                                             (RAD-IX)                     
161300             END-IF                                                       
161400          ELSE                                                            
161500              MOVE MFS-ALFA-FAELT-FEL TO                                  
161600                    RESP-DIERS-TILLK-ATTR(RAD-IX)                         
161700              MOVE NEJ TO INPUT-RETT                                      
161800              MOVE '043' TO RESP-IDMSG-ERROR                              
161840              STRING 'INVALID QUANTITY FOR_'                              
161850                                           DELIMITED BY SIZE              
161860                REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE              
161870                       INTO RESP-FELTEXT                                  
161900          END-IF                                                          
162000                                                                          
162100          IF REQU-BEERS(RAD-IX) = ALL '+'                                 
162200              MOVE MFS-ALFA-FAELT-RAETT TO RESP-BEERS-ATTR(RAD-IX)        
162300          ELSE                                                            
162400             IF REQU-BEERS(RAD-IX) = SPACE                                
162500                MOVE MFS-ALFA-FAELT-RAETT TO                              
162600                                 RESP-BEERS-ATTR(RAD-IX)                  
162700             ELSE                                                         
162800                MOVE MFS-ALFA-FAELT-FEL TO                                
162900                              RESP-BEERS-ATTR(RAD-IX)                     
163000                MOVE NEJ TO INPUT-RETT                                    
163100                MOVE '044' TO RESP-IDMSG-ERROR                            
163110                MOVE 'CLEAR REPL_TEXT'                                    
163120                           TO RESP-FELTEXT                                
163200             END-IF                                                       
163300          END-IF                                                          
163400                                                                          
163500         WHEN REQU-IDARTNR-TILLK(RAD-IX) NOT NUMERIC                      
163600           MOVE MFS-ALFA-FAELT-FEL TO RESP-IDARTNR-TILLK-ATTR             
163700                                        (RAD-IX)                          
163800           MOVE NEJ TO INPUT-RETT                                         
163900           MOVE '045' TO RESP-IDMSG-ERROR                                 
163940           STRING 'INVALID PART-NUM:'   DELIMITED BY SIZE                 
163950             REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE                 
163960                    INTO RESP-FELTEXT                                     
163980                                                                          
164000           IF REQU-BEERS(RAD-IX) = ALL '+'                                
164100              MOVE MFS-ALFA-FAELT-RAETT TO RESP-BEERS-ATTR(RAD-IX)        
164200           ELSE                                                           
164300              IF REQU-BEERS(RAD-IX) = SPACE                               
164400                 MOVE MFS-ALFA-FAELT-RAETT TO                             
164500                                RESP-BEERS-ATTR(RAD-IX)                   
164600              ELSE                                                        
164700                 MOVE MFS-ALFA-FAELT-FEL TO                               
164800                                RESP-BEERS-ATTR(RAD-IX)                   
164900                 MOVE NEJ TO INPUT-RETT                                   
165000                 MOVE '046' TO RESP-IDMSG-ERROR                           
165040                 MOVE 'CLEAR REPL TEXT'                                   
165050                            TO RESP-FELTEXT                               
165100              END-IF                                                      
165200           END-IF                                                         
165300           IF REQU-DIERS-TILLK(RAD-IX) = ALL '+'                          
165400             MOVE MFS-ALFA-FAELT-FEL                                      
165500                  TO RESP-DIERS-TILLK-ATTR(RAD-IX)                        
165600             MOVE NEJ TO INPUT-RETT                                       
165700             MOVE '047' TO RESP-IDMSG-ERROR                               
165740             STRING 'MISSING QUANTITY FOR:'  DELIMITED BY SIZE            
165750               REQU-IDARTNR-TILLK(RAD-IX)    DELIMITED BY SIZE            
165760                      INTO RESP-FELTEXT                                   
165780                                                                          
165800           ELSE                                                           
165900             PERFORM BAB-KOLLA-DIERS-TILLK                                
166000             IF DEC-KDSVAR-OK                                             
166100               IF DEC-IDEDITDATA = ZERO                                   
166200                  MOVE MFS-ALFA-FAELT-FEL TO RESP-DIERS-TILLK-ATTR        
166300                                             (RAD-IX)                     
166400                  MOVE NEJ TO INPUT-RETT                                  
166500                  MOVE '048' TO RESP-IDMSG-ERROR                          
166510                                                                          
166540                  STRING 'QUANTITY INVALID FOR '                          
166550                                               DELIMITED BY SIZE          
166560                    REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE          
166570                           INTO RESP-FELTEXT                              
166590                                                                          
166600               ELSE                                                       
166700                  MOVE MFS-ALFA-FAELT-RAETT                               
166800                       TO RESP-DIERS-TILLK-ATTR(RAD-IX)                   
166900               END-IF                                                     
167000             ELSE                                                         
167100               MOVE MFS-ALFA-FAELT-FEL TO                                 
167200                     RESP-DIERS-TILLK-ATTR(RAD-IX)                        
167300               MOVE NEJ TO INPUT-RETT                                     
167400               MOVE '049' TO RESP-IDMSG-ERROR                             
167410                                                                          
167440               STRING 'INVALID QUANTITY FOR:'                             
167450                                            DELIMITED BY SIZE             
167460                 REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE             
167470                        INTO RESP-FELTEXT                                 
167490                                                                          
167500             END-IF                                                       
167600         END-IF                                                           
167700        END-EVALUATE                                                      
167800        PERFORM BAA-KOLLA-TILLK-ARTIKEL                                   
167900      END-IF                                                              
168000                                                                          
168100        IF WS-KDERS = '09' OR '29' OR '52'                                
168200          MOVE MFS-ALFA-FAELT-FEL TO RESP-BEERS-ATTR(RAD-IX)              
168300                                     RESP-IDARTNR-TILLK-ATTR              
168400                                         (RAD-IX)                         
168500                                     RESP-DIERS-TILLK-ATTR                
168600                                         (RAD-IX)                         
168700          MOVE NEJ TO INPUT-RETT                                          
168800          MOVE '050' TO RESP-IDMSG-ERROR                                  
168900          MOVE MED-7(SPRAK-IX) TO RESP-TEMFSINF                           
168940          MOVE 'ADDING PART NUM IS NOT ALLOWED:'                          
168960                               TO RESP-FELTEXT                            
169000        END-IF                                                            
169100       END-IF                                                             
169200       ADD +1 TO RAD-IX                                                   
169300     END-PERFORM                                                          
169400     .                                                                    
169500     EJECT                                                                
169600 BAA-KOLLA-TILLK-ARTIKEL SECTION.                                         
169700                                                                          
169800     IF REQU-IDARTNR-TILLK(RAD-IX) NUMERIC                                
169900      IF REQU-IDARTNR-TILLK(RAD-IX) = REQU-IDARTNR-KEY                    
170000         MOVE NEJ TO INPUT-RETT                                           
170100         MOVE '051' TO RESP-IDMSG-ERROR                                   
170200         MOVE MFS-ALFA-FAELT-FEL TO RESP-IDARTNR-TILLK-ATTR               
170300         (RAD-IX)                                                         
170340         MOVE 'SAME PART NUM CANT BE USED'                                
170360                    TO RESP-FELTEXT                                       
170400      END-IF                                                              
170500      MOVE REQU-IDARTNR-TILLK(RAD-IX) TO WS-IDARTNR-TILLK                 
170600                                                                          
170700      MOVE IDARTNR-TILLK-WS TO W-IDARTNR                                  
170800      PERFORM IMS-GET-ARTC01                                              
170900      IF SEGMENT-FINNS                                                    
171000         MOVE ART-TIFINLV  TO WS-TIFINLV-KOLL                             
171100         MOVE ART-KDPRODSL TO TEST-KDPRODSL                               
171200         IF ART-KDERS-UTG > ZERO                                          
171300           MOVE NEJ TO INPUT-RETT                                         
171400           MOVE '052' TO RESP-IDMSG-ERROR                                 
171500           MOVE MFS-ALFA-FAELT-FEL TO RESP-IDARTNR-TILLK-ATTR             
171600                                        (RAD-IX)                          
171700           MOVE MED-17(SPRAK-IX) TO RESP-TEMFSINF                         
171740           STRING 'DELETED PARTNUM:'    DELIMITED BY SIZE                 
171750             REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE                 
171760                               INTO RESP-FELTEXT                          
171800         ELSE                                                             
171900           IF KDPRODSL-LOCAL                                              
172000              MOVE ERSATT-KDPRODSL   TO TEST-KDPRODSL                     
172100              IF KDPRODSL-LOCAL                                           
172200                 PERFORM IMS-GET-ARTC11                                   
172300                 IF SEGMENT-FINNS                                         
172400                    IF CLAG-KDERS > ZERO                                  
172500                       MOVE MFS-ALFA-FAELT-FEL TO                         
172600                                   RESP-IDARTNR-TILLK-ATTR(RAD-IX)        
172700                       MOVE MED-13(SPRAK-IX) TO RESP-TEMFSINF             
172800                       MOVE NEJ TO INPUT-RETT                             
172900                       MOVE '053' TO RESP-IDMSG-ERROR                     
172940                       STRING 'REPLACED PARTNUM:'                         
172950                                               DELIMITED BY SIZE          
172960                         REQU-IDARTNR-TILLK(RAD-IX)                       
172970                                               DELIMITED BY SIZE          
172980                                INTO RESP-FELTEXT                         
172993                                                                          
173000                    END-IF                                                
173100                 END-IF                                                   
173200              ELSE                                                        
173300                 MOVE MFS-ALFA-FAELT-FEL TO                               
173400                               RESP-IDARTNR-TILLK-ATTR(RAD-IX)            
173500                 MOVE NEJ TO INPUT-RETT                                   
173600                 MOVE '054' TO RESP-IDMSG-ERROR                           
173640                 MOVE 'ADD PART NOT VALID,PG NONLOCAL'                    
173660                            TO RESP-FELTEXT                               
173700              END-IF                                                      
173800           ELSE                                                           
173900              PERFORM IMS-GET-ARTC11                                      
174000              IF SEGMENT-FINNS                                            
174100                 IF CLAG-KDERS > ZERO                                     
174200                   MOVE MFS-ALFA-FAELT-FEL TO                             
174300                               RESP-IDARTNR-TILLK-ATTR(RAD-IX)            
174400                   MOVE MED-13(SPRAK-IX) TO RESP-TEMFSINF                 
174500                   MOVE NEJ TO INPUT-RETT                                 
174600                   MOVE '055' TO RESP-IDMSG-ERROR                         
174640                   STRING 'REPLACED PARTNUM '                             
174650                                                DELIMITED BY SIZE         
174660                     REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE         
174670                            INTO RESP-FELTEXT                             
174700                 END-IF                                                   
174800               END-IF                                                     
174900            END-IF                                                        
175000         END-IF                                                           
175100                                                                          
175200         IF ERSATT-KDERS-C1 = 25 AND WS-KDERS = 05                        
175300            PERFORM BAAA-KOLLA-TIFINLV                                    
175400            IF TIFINLV-OK                                                 
175500               CONTINUE                                                   
175600            ELSE                                                          
175700               MOVE MFS-ALFA-FAELT-FEL TO RESP-IDARTNR-TILLK-ATTR         
175800                                         (RAD-IX)                         
175900               MOVE MED-21(SPRAK-IX) TO RESP-TEMFSINF                     
176000               MOVE NEJ TO INPUT-RETT                                     
176100               MOVE '056' TO RESP-IDMSG-ERROR                             
176140               STRING 'CHG PUBWEEK OF PART:'                              
176150                                            DELIMITED BY SIZE             
176160                 REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE             
176170                        INTO RESP-FELTEXT                                 
176200            END-IF                                                        
176300         ELSE                                                             
176400            IF ERSATT-KDERS-C1 = 22 AND WS-KDERS = 02                     
176500               PERFORM BAAA-KOLLA-TIFINLV                                 
176600               IF TIFINLV-OK                                              
176700                  CONTINUE                                                
176800               ELSE                                                       
176900                  MOVE MFS-ALFA-FAELT-FEL TO                              
177000                          RESP-IDARTNR-TILLK-ATTR(RAD-IX)                 
177100                  MOVE MED-21(SPRAK-IX) TO RESP-TEMFSINF                  
177200                  MOVE NEJ TO INPUT-RETT                                  
177300                  MOVE '057' TO RESP-IDMSG-ERROR                          
177340                  STRING 'CHG PUBWEEK OF PART '                           
177350                                               DELIMITED BY SIZE          
177360                    REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE          
177370                           INTO RESP-FELTEXT                              
177400               END-IF                                                     
177500            END-IF                                                        
177600         END-IF                                                           
177700                                                                          
177800      ELSE                                                                
177900          MOVE MFS-ALFA-FAELT-FEL TO RESP-IDARTNR-TILLK-ATTR              
178000                                     (RAD-IX)                             
178100          MOVE NEJ TO INPUT-RETT                                          
178200          MOVE MED-6(SPRAK-IX) TO RESP-TEMFSINF                           
178300          MOVE '058' TO RESP-IDMSG-ERROR                                  
178341          STRING 'PARTNUM MISS IN FILE:'                                  
178350                                       DELIMITED BY SIZE                  
178360            REQU-IDARTNR-TILLK(RAD-IX) DELIMITED BY SIZE                  
178370                   INTO RESP-FELTEXT                                      
178390                                                                          
178400      END-IF                                                              
178500     END-IF                                                               
178600     .                                                                    
178700     EJECT                                                                
178800 BAAA-KOLLA-TIFINLV SECTION.                                              
178900     SKIP2                                                                
179000     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
179100     CALL WDATKONV USING DAT-KDDATFORM                                    
179200                         DAT-I-TIDATUM                                    
179300                         DAT-O-TIDATUM                                    
179400                         DAT-KDSVAR                                       
179500     IF DAT-KDSVAR-OK                                                     
179600        MOVE DAT-TIAAVVD TO WS-TIFINLV                                    
179700        MOVE WS-TIFINLV-AAVV   TO TMP1-YYWW                               
179800        MOVE WS-AAVV           TO TMP2-YYWW                               
179900        PERFORM WY2000P3                                                  
180000        IF TMP1-YYWW  > TMP2-YYWW                                         
180100           MOVE JA TO WS-TIFINLV-SW                                       
180200        ELSE                                                              
180300           MOVE NEJ TO WS-TIFINLV-SW                                      
180400        END-IF                                                            
180500     ELSE                                                                 
180600        MOVE NEJ TO WS-TIFINLV-SW                                         
180700     END-IF                                                               
180800     .                                                                    
180900     EJECT                                                                
181000 BAB-KOLLA-DIERS-TILLK SECTION.                                           
181100     SKIP2                                                                
181200     MOVE REQU-DIERS-TILLK(RAD-IX) TO DEC-IDFRIDATA                       
181300     MOVE 3                        TO DEC-KVHELTAL                        
181400     MOVE 3                        TO DEC-KVDECIMAL                       
181500     CALL WDECEDIT USING DEC-WDECAREA                                     
181600     .                                                                    
181700     EJECT                                                                
181800 BB-KOLLA-PREL-ERSKOD SECTION.                                            
181900     SKIP2                                                                
182000     IF ERSATT-KDERS-C1 = 11 OR 14 OR 17 OR 18 OR 19                      
182100        MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR                        
182200        MOVE NEJ TO INPUT-RETT                                            
182300        MOVE '059' TO RESP-IDMSG-ERROR                                    
182400        MOVE MED-23(SPRAK-IX) TO RESP-TEMFSINF                            
182410        MOVE 'REPLACE CODE IS NOT ALLOWED'                                
182420                              TO RESP-FELTEXT                             
182500     END-IF                                                               
182600     .                                                                    
182700     EJECT                                                                
182800 BC-KOLLA-TIERSDAT-PREL SECTION.                                          
182900     SKIP2                                                                
183000     IF ERSATT-KDERS-C1 = 03 OR 06                                        
183100        MOVE REQU-TIERSDAT-PREL TO XX-TIERSDAT-PREL                       
183200        MOVE '1' TO XX-DAG                                                
183300        MOVE XX-TIERSDAT-PREL TO REQU-TIERSDAT-PREL                       
183400        IF REQU-TIERSDAT-PREL NUMERIC                                     
183500           MOVE REQU-TIERSDAT-PREL TO WS-TIERSDAT-PREL                    
183600           IF WS-VECKA-PREL > 00 AND < 54                                 
183700              MOVE MFS-ALFA-FAELT-RAETT TO                                
183800                  RESP-TIERSDAT-PREL-ATTR                                 
183900           ELSE                                                           
184000              MOVE MFS-ALFA-FAELT-FEL TO                                  
184100                  RESP-TIERSDAT-PREL-ATTR                                 
184200              MOVE NEJ TO INPUT-RETT                                      
184300              MOVE '060' TO RESP-IDMSG-ERROR                              
184310              MOVE 'REPL_DATE INVALID'                                    
184320                         TO RESP-FELTEXT                                  
184400           END-IF                                                         
184500        ELSE                                                              
184600           MOVE MFS-ALFA-FAELT-FEL TO                                     
184700               RESP-TIERSDAT-PREL-ATTR                                    
184800           MOVE NEJ TO INPUT-RETT                                         
184900           MOVE '061' TO RESP-IDMSG-ERROR                                 
184910           MOVE 'REPL DATE-INVALID'                                       
184920                      TO RESP-FELTEXT                                     
185000        END-IF                                                            
185100     ELSE                                                                 
185200        MOVE MFS-ALFA-FAELT-FEL TO                                        
185300            RESP-TIERSDAT-PREL-ATTR                                       
185400        MOVE NEJ TO INPUT-RETT                                            
185500        MOVE '062' TO RESP-IDMSG-ERROR                                    
185510        MOVE 'CLEAR REPL DATE'                                            
185520                   TO RESP-FELTEXT                                        
185600     END-IF                                                               
185700     IF INPUT-RETT = JA                                                   
185800         MOVE JA TO UPPDAT-TIERSDAT-TEARTNOT                              
185900     END-IF                                                               
186000     .                                                                    
186100     EJECT                                                                
186110 C-KOLLA-ACTION-FILE SECTION.                                             
186120     SKIP2                                                                
186130     MOVE NEJ TO FINNS-PA-ACTION-FILE                                     
186140     MOVE JA  TO UPPDATERING-TILLATEN                                     
186150                                                                          
186160     MOVE REQU-IDARTNR-KEY         TO W-1111-IDARTNR                      
186170                                      W-1112-IDARTNR                      
186180     PERFORM IMS-GET-XXAN01                                               
186190     IF SEGMENT-FINNS                                                     
186191        PERFORM IMS-GET-XXAN11                                            
186192        IF XXAN-1112-TIUPPDAT = DAGENS-DATUM                              
186193           IF XXAN-1112-IDUSER = REQU-IDUSER                              
186194             MOVE JA TO FINNS-PA-ACTION-FILE                              
186195                        UPPDATERING-TILLATEN                              
186196           ELSE                                                           
186197             MOVE NEJ TO UPPDATERING-TILLATEN                             
186199             MOVE MED-18(SPRAK-IX) TO RESP-TEMFSINF                       
186200           END-IF                                                         
186201        ELSE                                                              
186202          MOVE JA TO UPPDATERING-TILLATEN                                 
186203          PERFORM IMS-GET-XXAN01                                          
186204          PERFORM IMS-DLET-XXAN                                           
186205        END-IF                                                            
186206     END-IF                                                               
186207     MOVE REQU-IDARTNR-KEY    TO W-IDARTNR                                
186208     PERFORM IMS-GET-ARTC01                                               
186209     IF SEGMENT-FINNS                                                     
186210       IF  REQU-KDARBTYP-SEC-IDLEV = ART-IDLEVNR                          
186211       OR (REQU-KDARBTYP-SEC-IDLEV = SPACE OR LOW-VALUE)                  
186212         CONTINUE                                                         
186213       ELSE                                                               
186214         MOVE NEJ TO UPPDATERING-TILLATEN                                 
186216         MOVE FEL-8(SPRAK-IX) TO RESP-TEMFSFEL                            
186217       END-IF                                                             
186218     END-IF                                                               
186219                                                                          
186220     IF UPPDATERING-TILLATEN = JA                                         
186221       MOVE JA               TO SW-SUPPLIER-OK                            
186222                                                                          
186223       MOVE 1                TO LEV05-IX                                  
186224       PERFORM UNTIL LEV05-IX > 2                                         
186225         IF REQU-KDARBTYP-SEC-IDLEV = TAB-KDARBTYP-LEV(LEV05-IX)          
186226           MOVE NEJ          TO SW-SUPPLIER-OK                            
186227           IF ART-KDPRODSL = TAB-KDPRODSL (LEV05-IX)                      
186228             MOVE JA         TO SW-SUPPLIER-OK                            
186229             MOVE 2          TO LEV05-IX                                  
186230           END-IF                                                         
186231         END-IF                                                           
186232         ADD 1               TO LEV05-IX                                  
186233       END-PERFORM                                                        
186234                                                                          
186235       IF SW-SUPPLIER-OK = NEJ                                            
186236         MOVE NEJ             TO UPPDATERING-TILLATEN                     
186238         MOVE FEL-8(SPRAK-IX) TO RESP-TEMFSFEL                            
186239       END-IF                                                             
186240     END-IF                                                               
186241     .                                                                    
186242     EJECT                                                                
186250 D-RIVNING-AV-ERSAETTNING SECTION.                                        
186300     SKIP2                                                                
186400     MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                   
186500     PERFORM IMS-GET-ERSA01                                               
186600     IF SEGMENT-FINNS                                                     
186700        PERFORM S06-RIV-TILLK-ART                                         
186800        MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                
186900        PERFORM IMS-GET-ERSA01                                            
187000        IF SEGMENT-FINNS                                                  
187100           PERFORM IMS-DLET-ERSA                                          
187200        END-IF                                                            
187300        PERFORM DA-UPPDATERA-REGISTER                                     
187400     ELSE                                                                 
187500        IF ERSATT-KDERS-C1 = 29 OR 52                                     
187600           PERFORM DA-UPPDATERA-REGISTER                                  
187700        END-IF                                                            
187800     END-IF                                                               
187900     .                                                                    
188000     EJECT                                                                
188100 DA-UPPDATERA-REGISTER SECTION.                                           
188200     SKIP2                                                                
188300     MOVE REQU-IDARTNR-KEY TO W-1111-IDARTNR                              
188400     PERFORM IMS-GET-XXAN01                                               
188500     IF SEGMENT-FINNS                                                     
188600        PERFORM IMS-DLET-XXAN                                             
188700     END-IF                                                               
188800                                                                          
188900     IF (ERSATT-FLIART = JA )                                             
189000     OR (ERSATT-IDLEVNR  = '1002 ')                                       
189100        PERFORM S12-SKAPA-SATSTRANS                                       
189200     END-IF                                                               
189300                                                                          
189400     MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                   
189500     PERFORM IMS-GET-ARTC01                                               
189600     MOVE ART-KDPRODSL TO WS-KDPRODSL                                     
189700                                                                          
189800     IF (ERSATT-KDERS-C1 > 20)                                            
189900        MOVE ZERO TO ART-TIERSDAT                                         
190000        PERFORM IMS-REPL-ARTC                                             
190100     END-IF                                                               
190200                                                                          
190300     PERFORM S10-LAGG-UPP-IDAO                                            
190400                                                                          
190500                                                                          
190600     PERFORM IMS-GET-ARTC11                                               
190700     IF  CLAG-KDERS        > ZERO                                         
190800     AND CLAG-KDERS    NOT = 09                                           
190900        MOVE CLAG-IDANSK  TO WS-IDANSK-ALARM                              
191000        MOVE WC-CDC-SE    TO WS-IDDC-ALARM                                
191100        SET ALARM-SSCODE-JA                                               
191200                          TO TRUE                                         
191300     END-IF                                                               
191400     MOVE ZERO TO CLAG-KDERS                                              
191500     IF ERSATT-KDKSP = +4 OR +1                                           
191600        MOVE ZERO TO CLAG-KDKSP                                           
191700     END-IF                                                               
191800     PERFORM IMS-REPL-ARTC                                                
191900     IF ALARM-SSCODE-JA                                                   
192000        PERFORM S29-ALARM-KDERS-UPD                                       
192100     END-IF                                                               
192200                                                                          
192300     PERFORM S16-SKAPA-VR-TRANS                                           
192400                                                                          
192500     MOVE 'R'             TO KDP-KDUART                                   
192600     MOVE REQU-IDARTNR-KEY  TO W-IDARTNR                                  
192700     PERFORM S21-SKAPA-KDP-TRANS                                          
192800     PERFORM S23-TRANS-TIKO-RSLISTA                                       
192900     PERFORM S24-TRANS-TILL-BASL                                          
193000     PERFORM S25-TRANS-TILL-PPMS                                          
193100     MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF                               
193300     .                                                                    
193400     EJECT                                                                
193500 F-KONTROLL-REG-UPPDAT-AV-ERS SECTION.                                    
193600     SKIP2                                                                
193700     IF  ERSATT-KDERS-C1 = 00                                             
193800        IF WS-KDERS-1 = 0                                                 
193900        OR WS-KDERS   = 52                                                
194000           IF REQU-FLKLAR = NEJ                                           
194100              IF FINNS-PA-ACTION-FILE = NEJ                               
194200                 MOVE REQU-IDARTNR-KEY TO W-1111-IDARTNR                  
194300                                          W-1112-IDARTNR                  
194400                                          W-IDARTNR                       
194600                                          XXAN-1111-IDARTNR               
194700                 MOVE '1111'           TO XXAN-1111-IDHTYP                
194800                 MOVE LOW-VALUE        TO XXAN-1111-LOWVALUE              
194900                 PERFORM IMS-ISRT-XXAN01                                  
195000              END-IF                                                      
195100                                                                          
195200              PERFORM IMS-GU-XXAN11                                       
195300              MOVE REQU-IDARTNR-KEY  TO XXAN-1112-IDARTNR                 
195400              MOVE LOW-VALUE         TO XXAN-1112-LOWVALUE                
195500              MOVE REQU-IDUSER       TO XXAN-1112-IDUSER                  
195600              MOVE DAGENS-DATUM      TO XXAN-1112-TIUPPDAT                
195700              IF SEGMENT-FINNS                                            
195800                 PERFORM IMS-REPL-XXAN                                    
195900              ELSE                                                        
196000                 PERFORM IMS-ISRT-XXAN11                                  
196100              END-IF                                                      
196200              PERFORM S03-UPPDATERA-RADER                                 
196300           ELSE                                                           
196400              PERFORM FA-REGISTRERA-ERSAETTNING                           
196500              IF ERSATT-FLIART = JA                                       
196600                IF WS-KDERS-2 = 4 OR 5 OR 6 OR 8                          
196700                    MOVE MED-28(SPRAK-IX) TO RESP-TEMFSFEL                
196800                END-IF                                                    
196900              END-IF                                                      
197000           END-IF                                                         
197100        ELSE                                                              
197200           MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR                     
197300           MOVE NEJ TO INPUT-RETT                                         
197400           MOVE MED-3(SPRAK-IX) TO RESP-TEMFSINF                          
197500           MOVE '063' TO RESP-IDMSG-ERROR                                 
197510           MOVE 'REPLACE CODE INVALID'                                    
197520                      TO RESP-FELTEXT                                     
197600        END-IF                                                            
197700     ELSE                                                                 
197800        IF ((ERSATT-KDERS-C1 = 04 OR 05 OR 06 OR 08)                      
197900        AND (WS-KDERS        = 04 OR 05 OR 06 OR 08))                     
198000        OR                                                                
198100           ((ERSATT-KDERS-C1 = 01 OR 02 OR 03 OR 07)                      
198200        AND (WS-KDERS        = 01 OR 02 OR 03 OR 07))                     
198300        OR                                                                
198400           ((ERSATT-KDERS-C1 = 24 OR 25 OR 26 OR 28)                      
198500        AND (WS-KDERS        = 24 OR 25 OR 26 OR 28))                     
198600        OR                                                                
198700           ((ERSATT-KDERS-C1 = 21 OR 22 OR 23 OR 27)                      
198800        AND (WS-KDERS        = 21 OR 22 OR 23 OR 27))                     
198900***** 021218                                                              
199000*****   OR                                                                
199100*****      (ERSATT-KDERS-C1 = 24 AND WS-KDERS = 29)                       
199200***** 021218                                                              
199300           IF REQU-FLKLAR = NEJ                                           
199400              PERFORM S17-UPPDAT-TILLK-ART-ACTION                         
199500              MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF                      
199600           ELSE                                                           
199700              PERFORM FB-UPPDATERA-ERSAETTNING                            
199800           END-IF                                                         
199900        ELSE                                                              
200000           IF (ERSATT-KDERS-C1 = 21 OR 22 OR 23 OR 24 OR 25               
200100                              OR 26 OR 27 OR 28)                          
200200           AND (WS-KDERS       = 01 OR 02 OR 03 OR 04 OR 05               
200300                              OR 06 OR 07 OR 08)                          
200400              IF ERSATT-KDERS-C1-2 = WS-KDERS-2                           
200500                 IF REQU-FLKLAR = NEJ                                     
200600                    PERFORM S17-UPPDAT-TILLK-ART-ACTION                   
200700                    MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF                
200800                 ELSE                                                     
200900                    PERFORM FB-UPPDATERA-ERSAETTNING                      
201000                 END-IF                                                   
201100              ELSE                                                        
201200                 MOVE MED-20(SPRAK-IX) TO RESP-TEMFSINF                   
201300                 MOVE NEJ TO INPUT-RETT                                   
201400                 MOVE '064' TO RESP-IDMSG-ERROR                           
201410                 MOVE 'RESET REPLACE BEFORE UPDATE'                       
201420                            TO RESP-FELTEXT                               
201500              END-IF                                                      
201600           ELSE                                                           
201700              IF ((ERSATT-KDERS-C1 = 01 OR 02 OR 03 OR 07)                
201800              AND (WS-KDERS    = 27))                                     
201900              OR                                                          
202000                 ((ERSATT-KDERS-C1 = 04 OR 05 OR 06 OR 08)                
202100              AND (WS-KDERS    = 28))                                     
202200                 IF REQU-FLKLAR = NEJ                                     
202300                    PERFORM S17-UPPDAT-TILLK-ART-ACTION                   
202400                    MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF                
202500                 ELSE                                                     
202600                    PERFORM FB-UPPDATERA-ERSAETTNING                      
202700                 END-IF                                                   
202800              ELSE                                                        
202900                 IF (ERSATT-KDERS-C1 = 11 OR 14 OR 17 OR 18 OR 19)        
203000                 AND (WS-KDERS       = 01 OR 04 OR 07 OR 08 OR 09)        
203100                    IF ERSATT-KDERS-C1-2 = WS-KDERS-2                     
203200                       IF REQU-FLKLAR = NEJ                               
203300                         PERFORM S17-UPPDAT-TILLK-ART-ACTION              
203400                         MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF           
203500                       ELSE                                               
203600                          PERFORM FB-UPPDATERA-ERSAETTNING                
203700                       END-IF                                             
203800                    ELSE                                                  
203900                       MOVE MED-20(SPRAK-IX) TO RESP-TEMFSINF             
204000                       MOVE NEJ TO INPUT-RETT                             
204100                       MOVE '065' TO RESP-IDMSG-ERROR                     
204110                       MOVE 'RESET REPL BEFORE UPDATE'                    
204120                                  TO RESP-FELTEXT                         
204200                    END-IF                                                
204300                 ELSE                                                     
204400                    MOVE MED-20(SPRAK-IX) TO RESP-TEMFSINF                
204500                    MOVE NEJ TO INPUT-RETT                                
204600                    MOVE '066' TO RESP-IDMSG-ERROR                        
204610                    MOVE 'CANCELLATION NOT POSSIBLE'                      
204620                               TO RESP-FELTEXT                            
204700                 END-IF                                                   
204800              END-IF                                                      
204900           END-IF                                                         
205000        END-IF                                                            
205100     END-IF                                                               
205200                                                                          
205400     .                                                                    
205500     EJECT                                                                
205600 FA-REGISTRERA-ERSAETTNING SECTION.                                       
205700     SKIP2                                                                
205800     MOVE JA TO INPUT-RETT                                                
205900                                                                          
206000     MOVE +1 TO TAB-IX                                                    
206100     PERFORM UNTIL TAB-IX > MAX-TAB                                       
206200        MOVE ZERO  TO TAB-IDRADNR      (TAB-IX)                           
206300                      TAB-IDARTNR-TILLK(TAB-IX)                           
206400                      TAB-DIERS-TILLK  (TAB-IX)                           
206500        MOVE SPACE TO TAB-BEERS        (TAB-IX)                           
206600        ADD +1 TO TAB-IX                                                  
206700     END-PERFORM                                                          
206800                                                                          
206900     IF FINNS-PA-ACTION-FILE = JA                                         
207000        PERFORM S03-UPPDATERA-RADER                                       
207100        PERFORM S01-LAS-ACTIONFILE-TILL-TAB                               
207200     ELSE                                                                 
207300        PERFORM S05-UPPDATERA-TABELL                                      
207400     END-IF                                                               
207500                                                                          
207600     PERFORM S18-RAKNA-KVKORT                                             
207700     IF WS-RAKNARE = ZERO                                                 
207800        IF REQU-KDERS = '09' OR '29' OR '52'                              
207900           CONTINUE                                                       
208000        ELSE                                                              
208100           MOVE NEJ TO INPUT-RETT                                         
208200           MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR                     
208300           MOVE '067' TO RESP-IDMSG-ERROR                                 
208310           MOVE 'ADD PART NUM TO REPL OLD PART'                           
208320                      TO RESP-FELTEXT                                     
208400        END-IF                                                            
208500     END-IF                                                               
208600     IF WS-KDERS-2 = 4 OR 5 OR 6 OR 8                                     
208700        IF TEXT-FINNS = NEJ                                               
208800           MOVE NEJ TO INPUT-RETT                                         
208900           MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR                     
209000           MOVE MED-10(SPRAK-IX) TO RESP-TEMFSINF                         
209100           MOVE '068' TO RESP-IDMSG-ERROR                                 
209110           MOVE 'REPLACE TEXT MISSING'                                    
209120                      TO RESP-FELTEXT                                     
209200        END-IF                                                            
209300     END-IF                                                               
209400                                                                          
209500     IF INPUT-RETT = JA                                                   
209600                                                                          
209700       MOVE REQU-IDARTNR-KEY TO W-1111-IDARTNR                            
209800       PERFORM IMS-GET-XXAN01                                             
209900       IF SEGMENT-FINNS                                                   
210000          PERFORM IMS-DLET-XXAN                                           
210100       END-IF                                                             
210200                                                                          
210300       IF REQU-IDAO = ALL '+' OR SPACE                                    
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
212000          MOVE REQU-IDARTNR-KEY TO W-IDARTNR-D9                           
212100          MOVE REQU-IDDC        TO W-IDDC-D9                              
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
214300          MOVE REQU-IDARTNR-KEY TO W-IDARTNR                              
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
217400          IF NY-KDERS-C1 = 3 OR 6 OR 13 OR 16 OR 23 OR 26                 
217500            MOVE WS-TIERSDAT-PREL    TO CLAG-TISTOREF                     
217600          END-IF                                                          
217700          MOVE NY-KDERS-C1 TO CLAG-KDERS                                  
217800          PERFORM IMS-REPL-ARTC                                           
217900                                                                          
218000       ELSE                                                               
218100                                                                          
218200          MOVE REQU-IDARTNR-KEY TO W-IDARTNR                              
218300          PERFORM IMS-GET-ARTC01                                          
218400          MOVE WS-KDERS TO ART-KDERS-UTG                                  
218500          PERFORM IMS-REPL-ARTC                                           
218600                                                                          
218700       END-IF                                                             
218800       MOVE SPACE TO WS-TEARTNOT                                          
218900       PERFORM S07-SKAPA-ERSATT-ART-TO-ERSREG                             
219000       IF WS-KDERS = '09' OR '29' OR '52'                                 
219100          MOVE +1 TO ERSA01-KVKORT                                        
219200       ELSE                                                               
219300          MOVE WS-RAKNARE TO ERSA01-KVKORT                                
219400       END-IF                                                             
219500       PERFORM IMS-ISRT-ERSA01                                            
219600       PERFORM S08-SKAPA-TILLK-ART-TO-ERSREG                              
219700       PERFORM S19-SKAPA-BEVAKNINGS-SEGMENT                               
219800       MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                 
219900       PERFORM IMS-ISRT-ERSA13                                            
220000       IF WS-KDERS = '09' OR '19' OR '29' OR '52'                         
220100          MOVE 'U' TO KDP-KDUART                                          
220200       ELSE                                                               
220300          MOVE 'E' TO KDP-KDUART                                          
220400       END-IF                                                             
220500       MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                 
220600       PERFORM IMS-GET-ARTC01                                             
220700       MOVE ART-KDPRODSL TO WS-KDPRODSL                                   
220800       PERFORM S21-SKAPA-KDP-TRANS                                        
220900       PERFORM S23-TRANS-TIKO-RSLISTA                                     
221000       MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF                             
221200     END-IF                                                               
221300     .                                                                    
221400     EJECT                                                                
221500 FB-UPPDATERA-ERSAETTNING SECTION.                                        
221600     SKIP2                                                                
221700     MOVE JA TO INPUT-RETT                                                
221800                                                                          
221900     MOVE +1 TO TAB-IX                                                    
222000     PERFORM UNTIL TAB-IX > MAX-TAB                                       
222100        MOVE ZERO  TO TAB-IDRADNR      (TAB-IX)                           
222200                      TAB-IDARTNR-TILLK(TAB-IX)                           
222300                      TAB-DIERS-TILLK  (TAB-IX)                           
222400        MOVE SPACE TO TAB-BEERS        (TAB-IX)                           
222500        ADD +1 TO TAB-IX                                                  
222600     END-PERFORM                                                          
222700                                                                          
222800     IF FINNS-PA-ACTION-FILE = JA                                         
222900        PERFORM S03-UPPDATERA-RADER                                       
223000        PERFORM S01-LAS-ACTIONFILE-TILL-TAB                               
223100     ELSE                                                                 
223200        PERFORM S04-LAS-ERSREG-TILL-TABELL                                
223300        PERFORM S05-UPPDATERA-TABELL                                      
223400     END-IF                                                               
223500************021218                                                        
223600**** IF (ERSATT-KDERS-C1 = 24 AND WS-KDERS = 29)                          
223700****     MOVE JA TO RADER-UPPDATERADE                                     
223800****       MOVE +1 TO TAB-IX                                              
223900****       PERFORM UNTIL TAB-IX > MAX-TAB                                 
224000****          MOVE ZERO  TO TAB-IDRADNR      (TAB-IX)                     
224100****                        TAB-IDARTNR-TILLK(TAB-IX)                     
224200****                        TAB-DIERS-TILLK  (TAB-IX)                     
224300****          MOVE SPACE TO TAB-BEERS        (TAB-IX)                     
224400****          ADD +1 TO TAB-IX                                            
224500****       END-PERFORM                                                    
224600**** END-IF                                                               
224700************021218                                                        
224800                                                                          
224900     IF RADER-UPPDATERADE = NEJ AND FINNS-PA-ACTION-FILE = NEJ            
225000        CONTINUE                                                          
225100     ELSE                                                                 
225200        PERFORM S18-RAKNA-KVKORT                                          
225300        IF WS-RAKNARE = ZERO                                              
225400           IF REQU-KDERS = '09' OR '29' OR '52'                           
225500              CONTINUE                                                    
225600           ELSE                                                           
225700              MOVE NEJ TO INPUT-RETT                                      
225800              MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR                  
225900              MOVE '069' TO RESP-IDMSG-ERROR                              
225910              MOVE 'ADD PART-NUM TO REPL OLD PART'                        
225920                         TO RESP-FELTEXT                                  
226000           END-IF                                                         
226100        END-IF                                                            
226200        IF WS-KDERS-2 = 4 OR 5 OR 6 OR 8                                  
226300           IF TEXT-FINNS = NEJ                                            
226400              MOVE NEJ TO INPUT-RETT                                      
226500              MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR                  
226600              MOVE MED-10(SPRAK-IX) TO RESP-TEMFSINF                      
226700              MOVE '070' TO RESP-IDMSG-ERROR                              
226710              MOVE 'REPL TEXT MISSING'                                    
226720                         TO RESP-FELTEXT                                  
226800           END-IF                                                         
226900        END-IF                                                            
227000     END-IF                                                               
227100                                                                          
227200     IF INPUT-RETT = JA                                                   
227300       MOVE REQU-IDARTNR-KEY TO W-1111-IDARTNR                            
227400       PERFORM IMS-GET-XXAN01                                             
227500       IF SEGMENT-FINNS                                                   
227600          PERFORM IMS-DLET-XXAN                                           
227700       END-IF                                                             
227800                                                                          
227900       IF REQU-IDAO = ALL '+' OR SPACE                                    
228000          CONTINUE                                                        
228100       ELSE                                                               
228200          PERFORM S10-LAGG-UPP-IDAO                                       
228300       END-IF                                                             
228400                                                                          
228500       IF ERSATT-KDERS-UTG = 0                                            
228600                                                                          
228700          IF (ERSATT-FLIART = JA)                                         
228800          OR (ERSATT-IDLEVNR = '1002 ')                                   
228900             PERFORM S12-SKAPA-SATSTRANS                                  
229000          END-IF                                                          
229100                                                                          
229200          MOVE WS-KDERS TO NY-KDERS-C1                                    
229300                                                                          
229400          MOVE REQU-IDARTNR-KEY TO W-IDARTNR                              
229500          PERFORM IMS-GET-ARTC01                                          
229600          IF WS-KDERS > 20                                                
229700             MOVE 'IDAG  ' TO DAT-KDDATFORM                               
229800             CALL WDATKONV USING DAT-KDDATFORM                            
229900                                 DAT-I-TIDATUM                            
230000                                 DAT-O-TIDATUM                            
230100                                 DAT-KDSVAR                               
230200             MOVE DAT-TIAAVVD    TO ART-TIERSDAT                          
230300          ELSE                                                            
230400             MOVE ZERO           TO ART-TIERSDAT                          
230500          END-IF                                                          
230600          PERFORM IMS-REPL-ARTC                                           
230700          PERFORM IMS-GET-ARTC11                                          
230800                                                                          
230900          IF (NY-KDERS-C1 > 0 AND < +10)                                  
231000          AND (ERSATT-KDKSP = +0)                                         
231100             MOVE +4 TO CLAG-KDKSP                                        
231200          ELSE                                                            
231300             IF (NY-KDERS-C1 > +19)                                       
231400             AND (ERSATT-KDKSP = +0 OR +4)                                
231500                MOVE +1 TO CLAG-KDKSP                                     
231600             ELSE                                                         
231700                IF (ERSATT-KDERS-C1 > +9)                                 
231800                AND (NY-KDERS-C1 > +0 AND < +10)                          
231900                AND (ERSATT-KDKSP = +1)                                   
232000                   MOVE +4 TO CLAG-KDKSP                                  
232100                END-IF                                                    
232200             END-IF                                                       
232300          END-IF                                                          
232400                                                                          
232500          IF NY-KDERS-C1 = 3 OR 6 OR 13 OR 16 OR 23 OR 26                 
232600            MOVE WS-TIERSDAT-PREL    TO CLAG-TISTOREF                     
232700          END-IF                                                          
232800          MOVE NY-KDERS-C1 TO CLAG-KDERS                                  
232900          PERFORM IMS-REPL-ARTC                                           
233000                                                                          
233100       ELSE                                                               
233200                                                                          
233300          MOVE REQU-IDARTNR-KEY TO W-IDARTNR                              
233400          PERFORM IMS-GET-ARTC01                                          
233500          MOVE WS-KDERS TO ART-KDERS-UTG                                  
233600          PERFORM IMS-REPL-ARTC                                           
233700                                                                          
233800       END-IF                                                             
233900                                                                          
234000       IF (RADER-UPPDATERADE = JA)                                        
234100       OR (RADER-UPPDATERADE = NEJ                                        
234200                          AND FINNS-PA-ACTION-FILE = JA)                  
234300          PERFORM S06-RIV-TILLK-ART                                       
234400          MOVE REQU-IDARTNR-KEY TO W-IDARTNR                              
234500          PERFORM IMS-GET-ERSA01                                          
234600          MOVE ERSA01-TEARTNOT TO WS-TEARTNOT                             
234700          IF SEGMENT-FINNS                                                
234800             PERFORM IMS-DLET-ERSA                                        
234900          END-IF                                                          
235000          PERFORM S07-SKAPA-ERSATT-ART-TO-ERSREG                          
235100          IF WS-KDERS = '09' OR '29' OR '52'                              
235200             MOVE +1 TO ERSA01-KVKORT                                     
235300          ELSE                                                            
235400             MOVE WS-RAKNARE TO ERSA01-KVKORT                             
235500          END-IF                                                          
235600          PERFORM IMS-ISRT-ERSA01                                         
235700          PERFORM S08-SKAPA-TILLK-ART-TO-ERSREG                           
235800          MOVE REQU-IDARTNR-KEY TO W-IDARTNR                              
235900          PERFORM S19-SKAPA-BEVAKNINGS-SEGMENT                            
236000          PERFORM IMS-ISRT-ERSA13                                         
236100          MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF                          
236200     ELSE                                                                 
236300        IF RADER-UPPDATERADE = NEJ                                        
236400        AND FINNS-PA-ACTION-FILE = NEJ                                    
236500          PERFORM IMS-GET-ERSA01                                          
236600          MOVE ERSA01-TEARTNOT TO WS-TEARTNOT                             
236700          PERFORM S07-SKAPA-ERSATT-ART-TO-ERSREG                          
236800          PERFORM IMS-REPL-ERSA                                           
236900          MOVE +1 TO TAB-IX                                               
237000          PERFORM UNTIL TAB-IX > MAX-TAB                                  
237100           IF TAB-IDARTNR-TILLK(TAB-IX) > ZERO                            
237200              MOVE TAB-IDARTNR-TILLK(TAB-IX) TO WS-IDARTNR-TILLK          
237300              MOVE IDARTNR-TILLK-WS TO W-IDARTNR                          
237400              PERFORM IMS-GET-ARTC01                                      
237500              MOVE ART-TIFINLV      TO TMP1-YYWWD                         
237600              MOVE WS-TIFINLV-MAX   TO TMP2-YYWWD                         
237700              PERFORM WY2000P2                                            
237800              IF TMP1-YYWWD > TMP2-YYWWD                                  
237900                 MOVE ART-TIFINLV TO WS-TIFINLV-MAX                       
238000              END-IF                                                      
238100           END-IF                                                         
238200           ADD +1 TO TAB-IX                                               
238300          END-PERFORM                                                     
238400          MOVE REQU-IDARTNR-KEY TO W-IDARTNR                              
238500          PERFORM IMS-GET-ERSA01                                          
238600          PERFORM IMS-GET-ERSA13                                          
238700          PERFORM S19-SKAPA-BEVAKNINGS-SEGMENT                            
238800          IF SEGMENT-FINNS                                                
238900             PERFORM IMS-REPL-ERSA                                        
239000          ELSE                                                            
239100            PERFORM IMS-ISRT-ERSA13                                       
239200          END-IF                                                          
239300          MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF                          
239400        END-IF                                                            
239500       END-IF                                                             
239600                                                                          
239700       MOVE NEJ TO FL-NYPONART                                            
239800       MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                 
239900       PERFORM IMS-GET-ARTG01                                             
240000       IF SEGMENT-FINNS                                                   
240100          MOVE JA TO FL-NYPONART                                          
240200       END-IF                                                             
240300                                                                          
240400     END-IF                                                               
240500     .                                                                    
240600     EJECT                                                                
240700 G-LAS SECTION.                                                           
240800     SKIP2                                                                
240900***** I SLUTET AV DENNA SEKTION SÄTTS RESP-TEMFSINF IHOP FRÅN             
241000*****                       WS-TEMFSINF-KAMP   OCH   WS-TEMFSINF          
241100     MOVE SPACE TO RESP-KDERS-C2                                          
241110                   RESP-DIERS-ERS                                         
241200                   RESP-KDERS                                             
241210                   RESP-TIERSDAT-PREL                                     
241300                   RESP-IDAO                                              
241500                   RESP-TEARTNOT                                          
241600                   RESP-FLKLAR                                            
241700                                                                          
241800     MOVE NEJ TO FLFORTS                                                  
241900     MOVE REQU-IDARTNR-KEY TO W-1111-IDARTNR                              
242000                        W-1112-IDARTNR                                    
242100                        W-IDARTNR                                         
242200     MOVE ZERO       TO W-1114-IDKORTNR                                   
242300                        W-IDKORTNR                                        
242400                                                                          
242500     IF REQU-IDKORTNR-SPAR1 = ZERO AND REQU-IDKORTNR-SPAR2 = ZERO         
242600         AND REQU-IDKORTNR-SPAR3 = ZERO                                   
242700                                                                          
242800         PERFORM IMS-GET-ARTC01                                           
242900         IF SEGMENT-FINNS                                                 
243000                                                                          
243100*          --- KOLLAR OM REG. ARTIKEL ÄR KAMPANJARTIKEL                   
243200           PERFORM GC-KOLLA-KAMPANJ                                       
243300                                                                          
243400           MOVE ART-TIERSDAT        TO WS-TIERSDAT-TEMP                   
243410           MOVE WS-TIERSDAT-TEMP    TO RESP-TIERSDAT                      
243500           IF ART-FLIART = JA                                             
243600              MOVE MED-25(SPRAK-IX) TO RESP-TEMFSFEL                      
243700           END-IF                                                         
243800           PERFORM S26-KOLLA-RASA                                         
243900           IF WS-EXTERN-SATS = JA                                         
244000               MOVE MED-26(SPRAK-IX) TO RESP-TEMFSFEL                     
244100           END-IF                                                         
244200           IF ART-FLIART = JA                                             
244300           AND WS-EXTERN-SATS = JA                                        
244400              MOVE MED-29(SPRAK-IX) TO RESP-TEMFSFEL                      
244500           END-IF                                                         
244600           IF ART-FLERS = JA                                              
244700              MOVE MED-2(SPRAK-IX) TO WS-TEMFSINF                         
244800           END-IF                                                         
244900           PERFORM IMS-GET-ARTC11                                         
245000           IF SEGMENT-FINNS                                               
245100              IF CLAG-FLGEMART = JA                                       
245200                 MOVE JA TO WS-FLGEMART                                   
245300                 MOVE MED-30(SPRAK-IX) TO WS-TEMFSINF                     
245400              END-IF                                                      
245500              MOVE CLAG-KDERS       TO WS-KDERS-TEMP                      
245501              MOVE WS-KDERS-TEMP    TO RESP-KDERS-C1                      
245600           ELSE                                                           
245700              MOVE SPACE            TO RESP-KDERS-C1                      
245800           END-IF                                                         
245900                                                                          
246000           PERFORM IMS-GET-ERSA01                                         
246100           MOVE SPACES              TO RESP-TIERSDAT-PREL-C2              
246200           IF SEGMENT-FINNS                                               
246300              MOVE ERSA01-TEARTNOT  TO RESP-TEARTNOT                      
246400              MOVE ERSA01-IDUSER    TO RESP-IDUSER                        
246500              PERFORM IMS-GET-ERSA13                                      
246600              IF SEGMENT-FINNS                                            
246700                MOVE ERSA13-TIERSDAT-PREL-C1 TO                           
246800                            WS-TIERSDAT-TEMP                              
246810                MOVE WS-TIERSDAT-TEMP        TO                           
246820                            RESP-TIERSDAT-PREL-C1                         
246900                MOVE ERSA13-TIERSDAT-REG     TO                           
247000                            WS-TIERSDAT-TEMP                              
247010                MOVE WS-TIERSDAT-TEMP        TO                           
247020                            RESP-TIERSDAT-REG                             
247100              ELSE                                                        
247200                MOVE SPACES  TO RESP-TIERSDAT-PREL-C1                     
247300                                RESP-TIERSDAT-REG                         
247400              END-IF                                                      
247500                                                                          
247600              PERFORM IMS-GET-XXAN01                                      
247700              IF SEGMENT-FINNS                                            
247800                PERFORM IMS-GET-XXAN11                                    
247900                IF XXAN-1112-TIUPPDAT = DAGENS-DATUM                      
248000                  IF XXAN-1112-IDUSER = REQU-IDUSER                       
248100                     PERFORM GA-LAS-ACTIONFILE                            
248200                  ELSE                                                    
248300                     PERFORM GB-LAS-ERSREG                                
248400                     MOVE +1 TO RAD-IX                                    
248500                     PERFORM UNTIL RAD-IX > MAX-RAD                       
248600                       MOVE MFS-STAENG-FAELT TO                           
248700                           RESP-IDARTNR-TILLK-ATTR(RAD-IX)                
248800                           RESP-DIERS-TILLK-ATTR(RAD-IX)                  
248900                           RESP-BEERS-ATTR(RAD-IX)                        
249000                           RESP-IDKORTNR-ATTR(RAD-IX)                     
249100                       ADD +1 TO RAD-IX                                   
249200                    END-PERFORM                                           
249300                    MOVE MFS-STAENG-FAELT TO RESP-DIERS-ERS-ATTR          
249400                                             RESP-IDAO-ATTR               
249500                                             RESP-TEARTNOT-ATTR           
249600                                             RESP-KDERS-ATTR              
249700                    MOVE MFS-STAENG-FAELT TO                              
249800                                     RESP-TIERSDAT-PREL-ATTR              
249900                    MOVE MED-18(SPRAK-IX) TO WS-TEMFSINF                  
250000                  END-IF                                                  
250100                ELSE                                                      
250200                  PERFORM GB-LAS-ERSREG                                   
250300                END-IF                                                    
250400             ELSE                                                         
250500                PERFORM GB-LAS-ERSREG                                     
250600             END-IF                                                       
250700           ELSE                                                           
250800             PERFORM IMS-GET-XXAN01                                       
250900             IF SEGMENT-FINNS                                             
251000                PERFORM IMS-GET-XXAN11                                    
251100                IF XXAN-1112-TIUPPDAT = DAGENS-DATUM                      
251200                  IF XXAN-1112-IDUSER = REQU-IDUSER                       
251300                     PERFORM GA-LAS-ACTIONFILE                            
251400                  ELSE                                                    
251500                    MOVE MED-18(SPRAK-IX) TO WS-TEMFSINF                  
251600                    MOVE +1 TO RAD-IX                                     
251700                    PERFORM UNTIL RAD-IX > MAX-RAD                        
251800                       MOVE MFS-STAENG-FAELT TO                           
251900                                     RESP-IDKORTNR-ATTR(RAD-IX)           
252000                       MOVE MFS-STAENG-FAELT TO                           
252100                                      RESP-IDARTNR-TILLK-ATTR             
252200                                      (RAD-IX)                            
252300                       MOVE MFS-STAENG-FAELT TO                           
252400                                      RESP-DIERS-TILLK-ATTR               
252500                                      (RAD-IX)                            
252600                       MOVE MFS-STAENG-FAELT TO                           
252700                                      RESP-BEERS-ATTR(RAD-IX)             
252800                       ADD +1 TO RAD-IX                                   
252900                    END-PERFORM                                           
253000                    MOVE MFS-STAENG-FAELT TO RESP-DIERS-ERS-ATTR          
253100                                             RESP-IDAO-ATTR               
253200                                             RESP-TEARTNOT-ATTR           
253300                                             RESP-KDERS-ATTR              
253400                    MOVE MFS-STAENG-FAELT TO                              
253500                                     RESP-TIERSDAT-PREL-ATTR              
253600                  END-IF                                                  
253700               ELSE                                                       
253800                MOVE +1 TO RAD-IX                                         
253900                PERFORM UNTIL RAD-IX > MAX-RAD                            
254000                 MOVE SPACES TO RESP-IDKORTNR(RAD-IX)                     
254200                                RESP-BEART(RAD-IX)                        
254300                                RESP-BEERS(RAD-IX)                        
254510                                RESP-IDARTNR-TILLK(RAD-IX)                
254550                                RESP-DIERS-TILLK(RAD-IX)                  
254600                 ADD +1 TO RAD-IX                                         
254700                END-PERFORM                                               
254800              END-IF                                                      
254900             ELSE                                                         
255000               MOVE +1 TO RAD-IX                                          
255100               PERFORM UNTIL RAD-IX > MAX-RAD                             
255200                MOVE SPACES TO RESP-IDKORTNR(RAD-IX)                      
255300                               RESP-BEART(RAD-IX)                         
255400                               RESP-BEERS(RAD-IX)                         
255610                               RESP-DIERS-TILLK(RAD-IX)                   
255640                               RESP-IDARTNR-TILLK(RAD-IX)                 
255700                ADD +1 TO RAD-IX                                          
255800               END-PERFORM                                                
255900             END-IF                                                       
256000             MOVE SPACES TO RESP-IDUSER                                   
256100                            RESP-TEARTNOT                                 
256400                            RESP-TIERSDAT-PREL-C1                         
256450                            RESP-TIERSDAT-PREL-C2                         
256460                            RESP-TIERSDAT-REG                             
256500           END-IF                                                         
256600         ELSE                                                             
256700            MOVE FEL-2(SPRAK-IX) TO RESP-TEMFSFEL                         
256800            MOVE SPACES  TO RESP-TEARTNOT                                 
257000                            RESP-IDUSER                                   
257510                            RESP-KDERS-C1                                 
257521                            RESP-KDERS-C2                                 
257530                            RESP-TIERSDAT-PREL-C1                         
257540                            RESP-TIERSDAT-PREL-C2                         
257550                            RESP-TIERSDAT-REG                             
257560                            RESP-TIERSDAT                                 
257600            MOVE +1 TO RAD-IX                                             
257700            PERFORM UNTIL RAD-IX > MAX-RAD                                
257800              MOVE SPACES TO RESP-IDKORTNR(RAD-IX)                        
257900                             RESP-BEART(RAD-IX)                           
258000                             RESP-BEERS(RAD-IX)                           
258210                             RESP-IDARTNR-TILLK(RAD-IX)                   
258220                             RESP-DIERS-TILLK(RAD-IX)                     
258300              ADD +1 TO RAD-IX                                            
258400            END-PERFORM                                                   
258500         END-IF                                                           
258600     ELSE                                                                 
258700        MOVE ALL '+'           TO RESP-TEARTNOT                           
258800                                  RESP-IDUSER                             
258910                                  RESP-KDERS-C1                           
258920                                  RESP-KDERS-C2                           
258930                                  RESP-TIERSDAT-REG                       
258940                                  RESP-TIERSDAT-PREL-C1                   
258950                                  RESP-TIERSDAT-PREL-C2                   
258960                                  RESP-TIERSDAT                           
259000                                                                          
259500        IF REQU-IDKORTNR-SPAR1 > ZERO                                     
259600           MOVE REQU-IDKORTNR-SPAR1 TO W-IDKORTNR                         
259700           PERFORM GB-LAS-ERSREG                                          
259800           PERFORM IMS-GET-XXAN01                                         
259900           IF SEGMENT-FINNS                                               
260000              PERFORM IMS-GET-XXAN11                                      
260100              IF XXAN-1112-TIUPPDAT = DAGENS-DATUM                        
260200                  MOVE +1 TO RAD-IX                                       
260300                  PERFORM UNTIL RAD-IX > MAX-RAD                          
260400                    MOVE MFS-STAENG-FAELT TO                              
260500                         RESP-IDARTNR-TILLK-ATTR(RAD-IX)                  
260600                         RESP-DIERS-TILLK-ATTR(RAD-IX)                    
260700                         RESP-BEERS-ATTR(RAD-IX)                          
260800                         RESP-IDKORTNR-ATTR(RAD-IX)                       
260900                    ADD +1 TO RAD-IX                                      
261000                  END-PERFORM                                             
261100                  MOVE MED-18(SPRAK-IX) TO WS-TEMFSINF                    
261200              END-IF                                                      
261300           END-IF                                                         
261400        ELSE                                                              
261500           IF REQU-IDKORTNR-SPAR2 > ZERO                                  
261600              MOVE REQU-IDKORTNR-SPAR2 TO W-1114-IDKORTNR                 
261700              MOVE JA TO FLFORTS                                          
261800              PERFORM GA-LAS-ACTIONFILE                                   
261900           ELSE                                                           
262000              MOVE +1 TO RAD-IX                                           
262100              PERFORM UNTIL RAD-IX > MAX-RAD                              
262200                 MOVE SPACES TO                                           
262500                      RESP-BEERS(RAD-IX)                                  
262520                      RESP-IDARTNR-TILLK(RAD-IX)                          
262530                      RESP-DIERS-TILLK(RAD-IX)                            
262600                 ADD +1 TO RAD-IX                                         
262700              END-PERFORM                                                 
262800              MOVE MFS-ADD-SAETT-CURSOR TO                                
262900                               RESP-IDARTNR-TILLK-ATTR(1)                 
263000              MOVE SPACE TO RESP-IDKORTNR-SPAR1                           
263100                                      RESP-IDKORTNR-SPAR2                 
263200                                      RESP-IDKORTNR-SPAR3                 
263300           END-IF                                                         
263400        END-IF                                                            
263500     END-IF                                                               
263600                                                                          
263700*    ---- SÄTT IHOP EV. MEDDELANDEN PÅ RAD 23 ------------                
263800     IF WS-TEMFSINF = SPACE  AND WS-TEMFSINF-KAMP = SPACE                 
263900         CONTINUE                                                         
264000     ELSE                                                                 
264100       IF WS-TEMFSINF = SPACE                                             
264200           MOVE WS-TEMFSINF-KAMP TO RESP-TEMFSINF                         
264300       ELSE                                                               
264400         IF WS-TEMFSINF-KAMP = SPACE                                      
264500             MOVE WS-TEMFSINF TO RESP-TEMFSINF                            
264600         ELSE                                                             
264700*            --- OBS MAX. 55 TECKEN                                       
264800             STRING WS-TEMFSINF-KAMP  DELIMITED BY SIZE                   
264900                    WS-TEMFSINF-SPLIT DELIMITED BY SIZE                   
265000                    WS-TEMFSINF       DELIMITED BY SIZE                   
265100             INTO RESP-TEMFSINF                                           
265200         END-IF                                                           
265300       END-IF                                                             
265400     END-IF                                                               
265500                                                                          
265600     .                                                                    
265700     EJECT                                                                
265800 GA-LAS-ACTIONFILE SECTION.                                               
265900     SKIP2                                                                
266000     MOVE +1 TO RAD-IX                                                    
266100     MOVE SPACE TO RESP-IDKORTNR-SPAR1                                    
266200                             RESP-IDKORTNR-SPAR2                          
266300                             RESP-IDKORTNR-SPAR3                          
266400     MOVE JA TO LAS-VIDARE                                                
266500                                                                          
266600     IF FLFORTS = JA                                                      
266700        PERFORM IMS-GU-XXAN11                                             
266800        PERFORM IMS-GNP-XXAN21                                            
266900     ELSE                                                                 
267000        PERFORM IMS-GET-XXAN21                                            
267100     END-IF                                                               
267200     PERFORM UNTIL SEGMENT-SAKNAS OR LAS-VIDARE = NEJ                     
267300        IF RAD-IX = MAX-RAD                                               
267400           MOVE NEJ TO LAS-VIDARE                                         
267500           MOVE XXAN-1114-IDKORTNR TO RESP-IDKORTNR-SPAR2                 
267600           MOVE MED-16(SPRAK-IX) TO WS-TEMFSINF                           
267700        ELSE                                                              
267800         IF LAS-VIDARE = JA                                               
267900           MOVE XXAN-1114-IDKORTNR TO RESP-IDKORTNR(RAD-IX)               
268000           IF XXAN-1114-FLTEXT = NEJ                                      
268100              MOVE NEJ TO RESP-FLTEXT(RAD-IX)                             
268200              MOVE XXAN-1114-IDARTNR-TILLK                                
268300                        TO WS-IDARTNR-TEMP                                
268310              MOVE WS-IDARTNR-TEMP                                        
268320                        TO RESP-IDARTNR-TILLK(RAD-IX)                     
268400              MOVE XXAN-1114-DIERS-TILLK TO WS-DIERS-TILLK                
268500              MOVE WS-DIERS-TILLK-X TO RESP-DIERS-TILLK(RAD-IX)           
268600              MOVE XXAN-1114-IDARTNR-TILLK TO W-IDARTNR-TILLK             
268700              IF W-IDARTNR-TILLK > ZERO                                   
268800                 PERFORM IMS-GET-BENA11                                   
268900                 IF SEGMENT-FINNS                                         
269000                    MOVE BENA-TEXT-BEART TO RESP-BEART(RAD-IX)            
269100                 ELSE                                                     
269200                    MOVE SPACE TO RESP-BEART(RAD-IX)                      
269300                 END-IF                                                   
269400              END-IF                                                      
269500              MOVE SPACE TO RESP-BEERS(RAD-IX)                            
269600              MOVE MFS-STAENG-FAELT TO RESP-BEERS-ATTR(RAD-IX)            
269700           ELSE                                                           
269800              MOVE XXAN-1114-BEERS  TO RESP-BEERS(RAD-IX)                 
269900              MOVE JA               TO RESP-FLTEXT(RAD-IX)                
270000              MOVE SPACES           TO RESP-BEART(RAD-IX)                 
270200                                       RESP-DIERS-TILLK(RAD-IX)           
270210                                      RESP-IDARTNR-TILLK(RAD-IX)          
270300              MOVE MFS-STAENG-FAELT TO RESP-IDARTNR-TILLK-ATTR            
270400                                      (RAD-IX)                            
270500              MOVE MFS-STAENG-FAELT TO RESP-DIERS-TILLK-ATTR              
270600                                      (RAD-IX)                            
270700           END-IF                                                         
270800         END-IF                                                           
270900           MOVE MFS-STAENG-FAELT TO RESP-IDKORTNR-ATTR(RAD-IX)            
271000           ADD +1 TO RAD-IX                                               
271100        END-IF                                                            
271200        PERFORM IMS-GET-XXAN21                                            
271300     END-PERFORM                                                          
271400                                                                          
271500     IF SEGMENT-FINNS                                                     
271600        CONTINUE                                                          
271700     ELSE                                                                 
271800        IF RAD-IX < MAX-RAD                                               
271900           PERFORM UNTIL RAD-IX > MAX-RAD                                 
272000              MOVE SPACES TO RESP-IDKORTNR(RAD-IX)                        
272100                             RESP-BEERS(RAD-IX)                           
272200                             RESP-BEART(RAD-IX)                           
272300                             RESP-FLTEXT(RAD-IX)                          
272510                             RESP-IDARTNR-TILLK(RAD-IX)                   
272520                             RESP-DIERS-TILLK(RAD-IX)                     
272600              ADD +1 TO RAD-IX                                            
272700           END-PERFORM                                                    
272800        ELSE                                                              
272900           IF RAD-IX = MAX-RAD                                            
273000              MOVE TIO TO REQU-IDKORTNR-SPAR3                             
273100              MOVE REQU-IDKORTNR-SPAR3 TO RESP-IDKORTNR-SPAR3             
273200           END-IF                                                         
273300        END-IF                                                            
273400     END-IF                                                               
273500     .                                                                    
273600     EJECT                                                                
273700 GB-LAS-ERSREG SECTION.                                                   
273800     SKIP2                                                                
273900     MOVE +1 TO RAD-IX                                                    
274000     MOVE SPACE TO RESP-IDKORTNR-SPAR1                                    
274100                             RESP-IDKORTNR-SPAR2                          
274200                             RESP-IDKORTNR-SPAR3                          
274300     MOVE JA TO LAS-VIDARE                                                
274400                                                                          
274500     PERFORM IMS-GET-ERSA01                                               
274600     PERFORM IMS-GNP-ERSA11                                               
274700     PERFORM UNTIL SEGMENT-SAKNAS OR LAS-VIDARE = NEJ                     
274800        IF RAD-IX = MAX-RAD                                               
274900           MOVE NEJ TO LAS-VIDARE                                         
275000           MOVE ERSA11-IDKORTNR TO REQU-IDKORTNR-SPAR1                    
275100           MOVE REQU-IDKORTNR-SPAR1 TO RESP-IDKORTNR-SPAR1                
275200           MOVE MED-16(SPRAK-IX) TO WS-TEMFSINF                           
275300        ELSE                                                              
275400         IF LAS-VIDARE = JA                                               
275500           IF ERSA11-FLTEXT = NEJ                                         
275600              MOVE NEJ TO RESP-FLTEXT(RAD-IX)                             
275700              MOVE ERSA11-IDARTNR-TILLK                                   
275800                        TO WS-IDARTNR-TEMP                                
275810              MOVE WS-IDARTNR-TEMP                                        
275820                        TO RESP-IDARTNR-TILLK(RAD-IX)                     
275900              MOVE ERSA11-DIERS-TILLK TO WS-DIERS-TILLK                   
276000              MOVE WS-DIERS-TILLK-X TO RESP-DIERS-TILLK(RAD-IX)           
276100              MOVE ERSA11-IDARTNR-TILLK TO W-IDARTNR-TILLK                
276200              MULTIPLY ERSA11-IDKORTNR BY TIO GIVING WS-IDKORTNR          
276300              MOVE WS-IDKORTNR TO RESP-IDKORTNR(RAD-IX)                   
276400              MOVE MFS-STAENG-FAELT TO RESP-IDKORTNR-ATTR(RAD-IX)         
276500                                       RESP-BEERS-ATTR(RAD-IX)            
276600              MOVE SPACE TO RESP-BEERS(RAD-IX)                            
276700              PERFORM IMS-GET-BENA11                                      
276800              IF SEGMENT-FINNS                                            
276900                 MOVE BENA-TEXT-BEART TO RESP-BEART(RAD-IX)               
277000              ELSE                                                        
277100                 MOVE SPACE TO RESP-BEART(RAD-IX)                         
277200              END-IF                                                      
277300           ELSE                                                           
277400              MOVE ERSA11-BEERS    TO RESP-BEERS(RAD-IX)                  
277500              MOVE JA              TO RESP-FLTEXT(RAD-IX)                 
277600              MULTIPLY ERSA11-IDKORTNR BY TIO GIVING WS-IDKORTNR          
277700              MOVE WS-IDKORTNR TO RESP-IDKORTNR(RAD-IX)                   
277800              MOVE MFS-STAENG-FAELT TO RESP-IDKORTNR-ATTR(RAD-IX)         
277900                                       RESP-IDARTNR-TILLK-ATTR            
278000                                       (RAD-IX)                           
278100              MOVE MFS-STAENG-FAELT TO RESP-DIERS-TILLK-ATTR              
278200                                       (RAD-IX)                           
278300              MOVE SPACES           TO RESP-BEART(RAD-IX)                 
278400                                       RESP-IDARTNR-TILLK(RAD-IX)         
278500                                       RESP-DIERS-TILLK(RAD-IX)           
278600           END-IF                                                         
278700         END-IF                                                           
278800           ADD +1 TO RAD-IX                                               
278900        END-IF                                                            
279000        PERFORM IMS-GNP-ERSA11                                            
279100     END-PERFORM                                                          
279200                                                                          
279300     IF SEGMENT-FINNS                                                     
279400        CONTINUE                                                          
279500     ELSE                                                                 
279600        IF RAD-IX < MAX-RAD                                               
279700           PERFORM UNTIL RAD-IX > MAX-RAD                                 
279800              MOVE SPACES TO RESP-IDARTNR-TILLK(RAD-IX)                   
279900                             RESP-DIERS-TILLK(RAD-IX)                     
280000                             RESP-BEART(RAD-IX)                           
280100                             RESP-BEERS(RAD-IX)                           
280200                             RESP-IDKORTNR(RAD-IX)                        
280300                             RESP-FLTEXT(RAD-IX)                          
280400              ADD +1 TO RAD-IX                                            
280500           END-PERFORM                                                    
280600        ELSE                                                              
280700           IF RAD-IX = MAX-RAD                                            
280800              MOVE TIO TO REQU-IDKORTNR-SPAR3                             
280900              MOVE REQU-IDKORTNR-SPAR3 TO RESP-IDKORTNR-SPAR3             
281000           END-IF                                                         
281100        END-IF                                                            
281200     END-IF                                                               
281300     .                                                                    
281400     EJECT                                                                
281500 GC-KOLLA-KAMPANJ   SECTION.                                              
281600     SKIP2                                                                
281700     PERFORM DB2-DCL-OPN-TP1ARTK-CRS                                      
281800     IF SQLCODE-WS = ZERO                                                 
281900       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
282000     END-IF                                                               
282100                                                                          
282200     MOVE ZERO               TO WS-ANTAL-KAMP                             
282300     MOVE NEJ                TO WS-FLAGGA-Q-KAMP                          
282400                                WS-FLAGGA-W-S-KAMP                        
282500     PERFORM UNTIL SQLCODE > ZERO                                         
282600       IF TP1KAMP-TISTODAT-KAMP > ZERO                                    
282700         MOVE TP1KAMP-TISTODAT-KAMP                                       
282800                             TO WS-JMFR-AAAAMMDD                          
282900       ELSE                                                               
283000         MOVE TP1KAMP-TISTADAT-KAMP                                       
283100                             TO WS-JMFR-AAAAMMDD                          
283200       END-IF                                                             
283300       IF WS-JMFR-AA > 50                                                 
283400         MOVE 19             TO WS-JMFR-AAAAMMDD (1:2)                    
283500       ELSE                                                               
283600         MOVE 20             TO WS-JMFR-AAAAMMDD (1:2)                    
283700       END-IF                                                             
283800       IF TP1KAMP-TISTODAT-KAMP = ZERO                                    
283900*    LÄGG TILL 5 ÅR                                                       
284000         ADD 50000           TO WS-JMFR-AAAAMMDD                          
284100       END-IF                                                             
284200       IF WS-JMFR-AAAAMMDD >= WS-DAGENS-AAAAMMDD                          
284300         IF TP1KAMP-KDKAMP = 'Q'                                          
284400           MOVE JA           TO WS-FLAGGA-Q-KAMP                          
284500         END-IF                                                           
284600         IF TP1KAMP-KDKAMP = 'W'                                          
284700         OR TP1KAMP-KDKAMP = 'S'                                          
284800           MOVE JA           TO WS-FLAGGA-W-S-KAMP                        
284900         END-IF                                                           
285000       END-IF                                                             
285100       ADD 1                 TO WS-ANTAL-KAMP                             
285200       PERFORM DB2-FETCH-TP1ARTK-CRS                                      
285300     END-PERFORM                                                          
285400                                                                          
285500     IF  WS-FLAGGA-Q-KAMP   = JA                                          
285600     AND WS-FLAGGA-W-S-KAMP = NEJ                                         
285700       MOVE MED-92 (SPRAK-IX)    TO WS-TEMFSINF-KAMP                      
285800*            SM ETC                                                       
285900     ELSE                                                                 
286000       IF WS-FLAGGA-W-S-KAMP = JA                                         
286100       MOVE MED-91 (SPRAK-IX)    TO WS-TEMFSINF-KAMP                      
286200*            CAMPAIGN                                                     
286300       END-IF                                                             
286400     END-IF                                                               
286500*    MOVE WS-ANTAL-KAMP      TO WS-TEMFSINF-KAMP                          
286600     PERFORM DB2-CLOSE-TP1ARTK-CRS                                        
286700     .                                                                    
286800     EJECT                                                                
286900 H-LAS-ERSATT-ARTIKEL SECTION.                                            
287000     SKIP2                                                                
287100     MOVE    JA              TO INPUT-RETT                                
287200                                                                          
287300     MOVE REQU-IDARTNR-KEY   TO W-IDARTNR                                 
287400     PERFORM IMS-GET-ARTC01                                               
287500     IF SEGMENT-SAKNAS                                                    
287600        MOVE FEL-2(SPRAK-IX) TO RESP-TEMFSINF                             
287700        MOVE NEJ             TO INPUT-RETT                                
287800        MOVE '071'           TO RESP-IDMSG-ERROR                          
287810        MOVE 'PART IS MISSING IN PARTS FILE'                              
287820                             TO RESP-FELTEXT                              
287900     ELSE                                                                 
288000        MOVE ART-KDERS-UTG   TO ERSATT-KDERS-UTG                          
288100        MOVE ART-FLERS       TO ERSATT-FLERS                              
288200        MOVE ART-IDLEVNR     TO ERSATT-IDLEVNR                            
288300        MOVE ART-FLIART      TO ERSATT-FLIART                             
288400        MOVE ART-KDPRODSL    TO ERSATT-KDPRODSL                           
288500                                TEST-KDPRODSL                             
288600        PERFORM IMS-GET-ARTC11                                            
288700        IF SEGMENT-FINNS                                                  
288800           MOVE CLAG-IDPROJ  TO SPAR-IDPROJ                               
288900           MOVE CLAG-KDKSP   TO ERSATT-KDKSP                              
289000           MOVE CLAG-FLLSRDEL                                             
289100                             TO ERSATT-FLLSRDEL                           
289200           MOVE CLAG-KDERS   TO ERSATT-KDERS-C1                           
289300           MOVE CLAG-KDAVT   TO ERSATT-KDAVT                              
289400           MOVE CLAG-KDBPSR  TO ERSATT-KDBPSR                             
289500           MOVE CLAG-PRARTSTD                                             
289600                             TO ERSATT-PRARTSTD                           
289700           IF CLAG-IDINK (1:1) NOT NUMERIC                                
289800              MOVE ZERO      TO ERSATT-IDINK                              
289900           ELSE                                                           
290000              IF CLAG-IDINK(1:3) NUMERIC                                  
290100                 MOVE CLAG-IDINK(1:3) TO ERSATT-IDINK                     
290200              ELSE                                                        
290300                 MOVE ZERO            TO ERSATT-IDINK                     
290400              END-IF                                                      
290500           END-IF                                                         
290600        ELSE                                                              
290700           MOVE ERSATT-KDERS-UTG      TO ERSATT-KDERS-C1                  
290800        END-IF                                                            
290900        IF KDPRODSL-VOLVO-BIMA                                            
291000           IF CDC OR SDC                                                  
291100              CONTINUE                                                    
291200           ELSE                                                           
291300              MOVE MED-31(SPRAK-IX)   TO RESP-TEMFSINF                    
291400              MOVE NEJ                TO INPUT-RETT                       
291500              MOVE '072'              TO RESP-IDMSG-ERROR                 
291510                                                                          
291540              STRING 'BIMA UPD NOT ALLOWED FOR DC:'                       
291550                                  DELIMITED BY SIZE                       
291560                     WS-IDDC      DELIMITED BY SIZE                       
291570                                    INTO RESP-FELTEXT                     
291590                                                                          
291600           END-IF                                                         
291700        END-IF                                                            
291800     END-IF                                                               
291900     .                                                                    
292000     EJECT                                                                
292100 I-UPPDAT-TILLK-ART-ERS-REG SECTION.                                      
292200     SKIP2                                                                
292300     MOVE JA TO INPUT-RETT                                                
292400     MOVE REQU-IDARTNR-KEY TO W-1111-IDARTNR                              
292500                        W-1112-IDARTNR                                    
292600                        W-IDARTNR                                         
292700                                                                          
292800     MOVE +1 TO TAB-IX                                                    
292900     PERFORM UNTIL TAB-IX > MAX-TAB                                       
293000        MOVE ZERO TO TAB-IDRADNR(TAB-IX)                                  
293100                     TAB-IDARTNR-TILLK(TAB-IX)                            
293200                     TAB-DIERS-TILLK(TAB-IX)                              
293300        MOVE SPACE TO TAB-BEERS(TAB-IX)                                   
293400        ADD +1 TO TAB-IX                                                  
293500     END-PERFORM                                                          
293600                                                                          
293700     IF FINNS-PA-ACTION-FILE = NEJ                                        
293800        PERFORM S04-LAS-ERSREG-TILL-TABELL                                
293900        PERFORM S05-UPPDATERA-TABELL                                      
294000     ELSE                                                                 
294100        PERFORM S03-UPPDATERA-RADER                                       
294200        PERFORM S01-LAS-ACTIONFILE-TILL-TAB                               
294300     END-IF                                                               
294400                                                                          
294500     IF FINNS-PA-ACTION-FILE = NEJ AND RADER-UPPDATERADE = NEJ            
294600        CONTINUE                                                          
294700     ELSE                                                                 
294800        PERFORM S18-RAKNA-KVKORT                                          
294900        IF WS-RAKNARE = ZERO                                              
295000           MOVE NEJ TO INPUT-RETT                                         
295100           MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR                     
295200           MOVE '073' TO RESP-IDMSG-ERROR                                 
295210           MOVE 'ADD PART NUM TO REPL OLD-PART'                           
295220                      TO RESP-FELTEXT                                     
295300        END-IF                                                            
295400        IF WS-KDERS-2 = 4 OR 5 OR 6 OR 8                                  
295500           IF TEXT-FINNS = NEJ                                            
295600              MOVE NEJ TO INPUT-RETT                                      
295700              MOVE MFS-ALFA-FAELT-FEL TO RESP-KDERS-ATTR                  
295800              MOVE MED-10(SPRAK-IX) TO RESP-TEMFSINF                      
295900              MOVE '074' TO RESP-IDMSG-ERROR                              
295910              MOVE 'REPL-TEXT MISSING'                                    
295920                         TO RESP-FELTEXT                                  
296000           END-IF                                                         
296100        END-IF                                                            
296200                                                                          
296300        IF INPUT-RETT = JA                                                
296400           PERFORM IMS-GET-XXAN01                                         
296500           IF SEGMENT-FINNS                                               
296600              PERFORM IMS-DLET-XXAN                                       
296700           END-IF                                                         
296800           PERFORM S06-RIV-TILLK-ART                                      
296900           PERFORM S08-SKAPA-TILLK-ART-TO-ERSREG                          
297000           MOVE REQU-IDARTNR-KEY TO W-IDARTNR                             
297100           PERFORM IMS-GET-ERSA01                                         
297200           MOVE WS-RAKNARE TO ERSA01-KVKORT                               
297300           PERFORM IMS-REPL-ERSA                                          
297400           PERFORM IMS-GET-ERSA01                                         
297500           PERFORM IMS-GET-ERSA13                                         
297600           IF SEGMENT-FINNS                                               
297700              IF ERSATT-KDERS-C1 = 01 OR 02 OR 04 OR 05                   
297800                 MOVE WS-TIFINLV-MAX TO ERSA13-TIERSDAT-PREL-C1           
297900                 PERFORM IMS-REPL-ERSA                                    
298000              END-IF                                                      
298100          END-IF                                                          
298200          MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF                          
298300       END-IF                                                             
298400     END-IF                                                               
298500     .                                                                    
298600     EJECT                                                                
298700 K-UPPDAT-TIERSDAT-TEARTNOT SECTION.                                      
298800     SKIP2                                                                
298900     IF REQU-TEARTNOT = ALL '+' OR SPACE                                  
299000        CONTINUE                                                          
299100     ELSE                                                                 
299200        MOVE REQU-TEARTNOT TO ERSA01-TEARTNOT                             
299300        PERFORM IMS-REPL-ERSA                                             
299400        MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF                            
299500     END-IF                                                               
299600                                                                          
299700     IF REQU-TIERSDAT-PREL = ALL '+' OR SPACE                             
299800        CONTINUE                                                          
299900     ELSE                                                                 
300000        PERFORM IMS-GET-ERSA13                                            
300100        IF SEGMENT-FINNS                                                  
300200           MOVE REQU-TIERSDAT-PREL TO ERSA13-TIERSDAT-PREL-C1             
300300           PERFORM IMS-REPL-ERSA                                          
300400           IF ERSATT-FLIART = JA                                          
300500              PERFORM KA-UPPDATERA-RASA                                   
300600           END-IF                                                         
300700           MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF                         
300800        END-IF                                                            
300900     END-IF                                                               
301000     .                                                                    
301100     EJECT                                                                
301200 KA-UPPDATERA-RASA SECTION.                                               
301300     SKIP2                                                                
301400     MOVE REQU-IDARTNR-KEY TO W-IDARTNR-S                                 
301500                        W-IDARTNR                                         
301600     MOVE SPACE TO W-IDLEVNR-S                                            
301700     MOVE SPACE TO W-BELEVART-S                                           
301800     PERFORM IMS-GET-SATB-CSEQ                                            
301900     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
302000        IF SATB-STR-IDARTNR < 100000000                                   
302100          IF SATB-STR-IDLEVNR = '1002 '                                   
302200           IF SATB-RAD-KDISATS = 'E'                                      
302300              MOVE SATB-RAD-TISTODAT TO WS-TISTODAT                       
302400              MOVE SATB-RAD-IDRADNR  TO W-IDRADNR                         
302500              MOVE SATB-RAD-KDSTRRAD TO W-KDSTRRAD                        
302600                                                                          
302700              MOVE SATB-STR-IDARTNR TO W-IDARTNR                          
302800              PERFORM IMS-GET-SATB01                                      
302900              PERFORM IMS-GHNP-SATB11                                     
303000              MOVE REQU-TIERSDAT-PREL TO DAT-I-TIDATUM                    
303100              MOVE 'AAVVD '          TO DAT-KDDATFORM                     
303200              CALL WDATKONV USING DAT-KDDATFORM                           
303300                                  DAT-I-TIDATUM                           
303400                                  DAT-O-TIDATUM                           
303500                                  DAT-KDSVAR                              
303600              MOVE DAT-TIAAMMDD TO SATB-RAD-TISTODAT                      
303700              PERFORM IMS-REPL-SATB                                       
303800                                                                          
303900              MOVE +1 TO STR-IX                                           
304000              PERFORM UNTIL STR-IX > STR-IX-MAX                           
304100                 MOVE ZERO TO WS-STR-TILLKART(STR-IX)                     
304200                 ADD +1 TO STR-IX                                         
304300              END-PERFORM                                                 
304400                                                                          
304500              MOVE +1 TO STR-IX                                           
304600              MOVE REQU-IDARTNR-KEY TO W-IDARTNR                          
304700              PERFORM IMS-GET-ERSA01                                      
304800              PERFORM IMS-GET-ERSA11                                      
304900              PERFORM UNTIL SEGMENT-SAKNAS                                
305000                IF ERSA11-FLTEXT = NEJ                                    
305100                  MOVE ERSA11-IDARTNR-TILLK TO                            
305200                     WS-STR-TILLKART(STR-IX)                              
305300                     ADD +1 TO STR-IX                                     
305400                END-IF                                                    
305500                PERFORM IMS-GET-ERSA11                                    
305600              END-PERFORM                                                 
305700                                                                          
305800              PERFORM IMS-GET-SATB11-FIRST                                
305900              PERFORM UNTIL SEGMENT-SAKNAS                                
306000                IF SATB-RAD-KDISATS = 'T'                                 
306100                  IF SATB-RAD-TISTADAT = WS-TISTODAT                      
306200                    MOVE +1 TO STR-IX                                     
306300                    PERFORM UNTIL STR-IX > STR-IX-MAX                     
306400                      IF SATB-RAD-IDARTNR =                               
306500                         WS-STR-TILLKART(STR-IX)                          
306600                         MOVE DAT-TIAAMMDD TO SATB-RAD-TISTADAT           
306700                         PERFORM IMS-REPL-SATB                            
306800                         MOVE +99 TO STR-IX                               
306900                      END-IF                                              
307000                      ADD +1 TO STR-IX                                    
307100                    END-PERFORM                                           
307200                   END-IF                                                 
307300                  END-IF                                                  
307400                  PERFORM IMS-GET-SATB11                                  
307500                END-PERFORM                                               
307600             END-IF                                                       
307700          END-IF                                                          
307800        END-IF                                                            
307900        PERFORM IMS-GET-SATB-CSEQ                                         
308000     END-PERFORM                                                          
308100     .                                                                    
308200     EJECT                                                                
308300 S01-LAS-ACTIONFILE-TILL-TAB SECTION.                                     
308400     SKIP2                                                                
308500     MOVE REQU-IDARTNR-KEY TO W-1111-IDARTNR                              
308600                        W-1112-IDARTNR                                    
308700     PERFORM IMS-GET-XXAN01                                               
308800     PERFORM IMS-GET-XXAN11                                               
308900     PERFORM IMS-GET-XXAN21                                               
309000     PERFORM UNTIL SEGMENT-SAKNAS                                         
309100        MOVE XXAN-1114-IDKORTNR TO TAB-IX                                 
309200        MOVE XXAN-1114-IDKORTNR TO TAB-IDRADNR(TAB-IX)                    
309300        IF XXAN-1114-FLTEXT = JA                                          
309400           MOVE XXAN-1114-BEERS TO TAB-BEERS(TAB-IX)                      
309500        ELSE                                                              
309600           MOVE XXAN-1114-IDARTNR-TILLK TO TAB-IDARTNR-TILLK              
309700                                       (TAB-IX)                           
309800           MOVE XXAN-1114-DIERS-TILLK TO TAB-DIERS-TILLK                  
309900                                        (TAB-IX)                          
310000        END-IF                                                            
310100        PERFORM IMS-GET-XXAN21                                            
310200     END-PERFORM                                                          
310300     .                                                                    
310400     EJECT                                                                
310500 S02-LAS-ERSREG-TILL-ACTIONFILE SECTION.                                  
310600     SKIP2                                                                
310700     MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                   
310800                        W-1111-IDARTNR                                    
310900                        W-1112-IDARTNR                                    
311000     MOVE ZERO       TO W-IDKORTNR                                        
311100     PERFORM IMS-GET-ERSA01                                               
311200                                                                          
311300     MOVE REQU-IDARTNR-KEY TO XXAN-1111-IDARTNR                           
311400     MOVE '1111'     TO XXAN-1111-IDHTYP                                  
311500     MOVE LOW-VALUE  TO XXAN-1111-LOWVALUE                                
311600     PERFORM IMS-ISRT-XXAN01                                              
311700                                                                          
311800     MOVE REQU-IDARTNR-KEY  TO XXAN-1112-IDARTNR                          
311900     MOVE LOW-VALUE         TO XXAN-1112-LOWVALUE                         
312000     MOVE REQU-IDUSER TO XXAN-1112-IDUSER                                 
312100     MOVE DAGENS-DATUM      TO XXAN-1112-TIUPPDAT                         
312200     PERFORM IMS-ISRT-XXAN11                                              
312300                                                                          
312400     PERFORM IMS-GNP-ERSA11                                               
312500     PERFORM UNTIL SEGMENT-SAKNAS                                         
312600        MULTIPLY ERSA11-IDKORTNR  BY TIO GIVING                           
312700                                       XXAN-1114-IDKORTNR                 
312800        MOVE LOW-VALUE              TO XXAN-1114-LOWVALUE                 
312900        MOVE ERSA11-FLTEXT          TO XXAN-1114-FLTEXT                   
313000        IF ERSA11-FLTEXT = NEJ                                            
313100           MOVE ERSA11-IDARTNR-TILLK TO XXAN-1114-IDARTNR-TILLK           
313200           MOVE ERSA11-DIERS-TILLK   TO XXAN-1114-DIERS-TILLK             
313300        ELSE                                                              
313400           MOVE ERSA11-BEERS        TO XXAN-1114-BEERS                    
313500        END-IF                                                            
313600        PERFORM IMS-ISRT-XXAN21                                           
313700        MOVE SPACE TO XXAN-1114-BEERS                                     
313800        PERFORM IMS-GNP-ERSA11                                            
313900     END-PERFORM                                                          
314000     .                                                                    
314100     EJECT                                                                
314200  S03-UPPDATERA-RADER SECTION.                                            
314300     SKIP2                                                                
314400     MOVE REQU-IDARTNR-KEY TO W-1111-IDARTNR                              
314500                        W-1112-IDARTNR                                    
314600                                                                          
314700     MOVE +1 TO RAD-IX                                                    
314800     PERFORM UNTIL RAD-IX > MAX-RAD                                       
314900      IF REQU-IDKORTNR(RAD-IX) NUMERIC                                    
315000        MOVE REQU-IDKORTNR(RAD-IX) TO WS-IDKORTNR                         
315100        MOVE WS-IDKORTNR          TO W-1114-IDKORTNR                      
315200                                                                          
315300        IF REQU-IDARTNR-TILLK(RAD-IX) = ALL '+' AND                       
315400           REQU-DIERS-TILLK(RAD-IX) = ALL '+' AND                         
315500           REQU-BEERS(RAD-IX) = ALL '+'                                   
315600           CONTINUE                                                       
315700        ELSE                                                              
315800          IF REQU-IDARTNR-TILLK(RAD-IX) = SPACE                           
315900             PERFORM IMS-GHU-XXAN21                                       
316000             IF SEGMENT-FINNS                                             
316100               IF XXAN-1114-FLTEXT = NEJ                                  
316200                  MOVE ZERO TO XXAN-1114-IDARTNR-TILLK                    
316300                            XXAN-1114-DIERS-TILLK                         
316400                  PERFORM IMS-REPL-XXAN                                   
316500                  MOVE NEJ TO RESP-FLTEXT(RAD-IX)                         
316600                  MOVE JA TO RADER-UPPDATERADE                            
316700                  MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF                  
316800               END-IF                                                     
316900             END-IF                                                       
317000          ELSE                                                            
317100            IF (REQU-IDARTNR-TILLK(RAD-IX) NUMERIC) AND                   
317200               (REQU-DIERS-TILLK(RAD-IX) NOT = SPACE)                     
317300               MOVE REQU-IDARTNR-TILLK(RAD-IX) TO                         
317400                                  WS-IDARTNR-TILLK                        
317500               MOVE REQU-DIERS-TILLK(RAD-IX) TO DEC-IDFRIDATA             
317600               MOVE 3                       TO DEC-KVHELTAL               
317700               MOVE 3                       TO DEC-KVDECIMAL              
317800               CALL WDECEDIT USING DEC-WDECAREA                           
317900               PERFORM IMS-GHU-XXAN21                                     
318000               IF SEGMENT-FINNS                                           
318100                 IF XXAN-1114-FLTEXT = NEJ                                
318200                    MOVE IDARTNR-TILLK-WS  TO                             
318300                               XXAN-1114-IDARTNR-TILLK                    
318400                    MOVE DEC-IDEDITDATA    TO                             
318500                               XXAN-1114-DIERS-TILLK                      
318600                    PERFORM IMS-REPL-XXAN                                 
318700                    MOVE NEJ         TO RESP-FLTEXT(RAD-IX)               
318800                    MOVE JA TO RADER-UPPDATERADE                          
318900                    MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF                
319000                 END-IF                                                   
319100               ELSE                                                       
319200                 MOVE WS-IDKORTNR  TO XXAN-1114-IDKORTNR                  
319300                 MOVE LOW-VALUE    TO XXAN-1114-LOWVALUE                  
319400                 MOVE NEJ          TO XXAN-1114-FLTEXT                    
319500                 MOVE DEC-IDEDITDATA  TO                                  
319600                                XXAN-1114-DIERS-TILLK                     
319700                 MOVE IDARTNR-TILLK-WS TO                                 
319800                                XXAN-1114-IDARTNR-TILLK                   
319900                 PERFORM IMS-ISRT-XXAN21                                  
320000                 MOVE JA TO RADER-UPPDATERADE                             
320100                 MOVE NEJ  TO  RESP-FLTEXT(RAD-IX)                        
320200                 MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF                   
320300               END-IF                                                     
320400            ELSE                                                          
320500              IF REQU-BEERS(RAD-IX) = SPACE                               
320600                PERFORM IMS-GHU-XXAN21                                    
320700                IF SEGMENT-FINNS                                          
320800                  IF XXAN-1114-FLTEXT = JA                                
320900                     MOVE SPACE TO XXAN-1114-BEERS                        
321000                     PERFORM IMS-REPL-XXAN                                
321100                     MOVE JA TO RESP-FLTEXT(RAD-IX)                       
321200                     MOVE JA TO RADER-UPPDATERADE                         
321300                     MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF               
321400                  END-IF                                                  
321500                END-IF                                                    
321600              ELSE                                                        
321700                IF REQU-BEERS(RAD-IX) NOT = SPACE                         
321800                  IF REQU-BEERS(RAD-IX) NOT = ALL '+'                     
321900                     PERFORM IMS-GHU-XXAN21                               
322000                     IF SEGMENT-FINNS                                     
322100                        IF XXAN-1114-FLTEXT = JA                          
322200                           MOVE REQU-BEERS(RAD-IX) TO                     
322300                                   XXAN-1114-BEERS                        
322400                           PERFORM IMS-REPL-XXAN                          
322500                           MOVE JA  TO RESP-FLTEXT(RAD-IX)                
322600                           MOVE JA TO RADER-UPPDATERADE                   
322700                           MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF         
322800                        END-IF                                            
322900                     ELSE                                                 
323000                       MOVE WS-IDKORTNR TO XXAN-1114-IDKORTNR             
323100                       MOVE LOW-VALUE   TO XXAN-1114-LOWVALUE             
323200                       MOVE JA          TO XXAN-1114-FLTEXT               
323300                                           RESP-FLTEXT(RAD-IX)            
323400                       MOVE JA TO RADER-UPPDATERADE                       
323500                       MOVE REQU-BEERS(RAD-IX) TO XXAN-1114-BEERS         
323600                       PERFORM IMS-ISRT-XXAN21                            
323700                       MOVE MED-15(SPRAK-IX) TO RESP-TEMFSINF             
323800                     END-IF                                               
323900                  END-IF                                                  
324000                END-IF                                                    
324100              END-IF                                                      
324200            END-IF                                                        
324300          END-IF                                                          
324400****************************                                              
324500        END-IF                                                            
324600       END-IF                                                             
324700       ADD +1 TO RAD-IX                                                   
324800     END-PERFORM                                                          
324900     .                                                                    
325000     EJECT                                                                
325100  S04-LAS-ERSREG-TILL-TABELL SECTION.                                     
325200     SKIP2                                                                
325300     MOVE ZERO TO TAB-IX                                                  
325400     MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                   
325500     MOVE ZERO       TO W-IDKORTNR                                        
325600     PERFORM IMS-GET-ERSA01                                               
325700                                                                          
325800     PERFORM IMS-GNP-ERSA11                                               
325900     PERFORM UNTIL SEGMENT-SAKNAS                                         
326000       MULTIPLY ERSA11-IDKORTNR BY TIO GIVING TAB-IX                      
326100       MULTIPLY ERSA11-IDKORTNR BY TIO GIVING TAB-IDRADNR(TAB-IX)         
326200       IF ERSA11-FLTEXT = NEJ                                             
326300         MOVE ERSA11-IDARTNR-TILLK TO TAB-IDARTNR-TILLK(TAB-IX)           
326400         MOVE ERSA11-DIERS-TILLK   TO TAB-DIERS-TILLK(TAB-IX)             
326500       ELSE                                                               
326600         MOVE ERSA11-BEERS         TO TAB-BEERS(TAB-IX)                   
326700       END-IF                                                             
326800       PERFORM IMS-GNP-ERSA11                                             
326900     END-PERFORM                                                          
327000     .                                                                    
327100     EJECT                                                                
327200 S05-UPPDATERA-TABELL SECTION.                                            
327300     SKIP2                                                                
327400     MOVE +1 TO RAD-IX                                                    
327500     PERFORM UNTIL RAD-IX > MAX-RAD                                       
327600        IF REQU-IDKORTNR(RAD-IX) NUMERIC                                  
327700           MOVE REQU-IDKORTNR(RAD-IX) TO WS-IDKORTNR                      
327800           MOVE WS-IDKORTNR TO TAB-IX                                     
327900                                                                          
328000           IF  REQU-IDARTNR-TILLK(RAD-IX) = ALL '+'                       
328100           AND REQU-DIERS-TILLK(RAD-IX) = ALL '+'                         
328200           AND REQU-BEERS(RAD-IX) = ALL '+'                               
328300              CONTINUE                                                    
328400           ELSE                                                           
328500              IF REQU-IDARTNR-TILLK(RAD-IX) = SPACE                       
328600                 MOVE ZERO TO TAB-IDARTNR-TILLK(TAB-IX)                   
328700                 MOVE NEJ TO RESP-FLTEXT(RAD-IX)                          
328800                 MOVE JA TO RADER-UPPDATERADE                             
328900              ELSE                                                        
329000                 IF  (REQU-IDARTNR-TILLK(RAD-IX) NUMERIC)                 
329100                 AND (REQU-DIERS-TILLK(RAD-IX) NOT = SPACE)               
329200                    MOVE REQU-IDARTNR-TILLK(RAD-IX)                       
329300                                TO  WS-IDARTNR-TILLK                      
329400                    MOVE WS-IDARTNR-TILLK                                 
329500                                TO TAB-IDARTNR-TILLK(TAB-IX)              
329600                    MOVE REQU-DIERS-TILLK(RAD-IX)                         
329700                                TO DEC-IDFRIDATA                          
329800                    MOVE 3      TO DEC-KVHELTAL                           
329900                    MOVE 3      TO DEC-KVDECIMAL                          
330000                    CALL WDECEDIT USING DEC-WDECAREA                      
330100                    MOVE DEC-IDEDITDATA                                   
330200                                TO TAB-DIERS-TILLK(TAB-IX)                
330300                    MOVE NEJ    TO RESP-FLTEXT(RAD-IX)                    
330400                    MOVE JA     TO RADER-UPPDATERADE                      
330500                 ELSE                                                     
330600                    IF REQU-BEERS(RAD-IX) = SPACE                         
330700                       MOVE SPACE TO TAB-BEERS(TAB-IX)                    
330800                       MOVE JA   TO RESP-FLTEXT(RAD-IX)                   
330900                       MOVE JA   TO RADER-UPPDATERADE                     
331000                    ELSE                                                  
331100                       IF REQU-BEERS(RAD-IX) NOT = SPACE                  
331200                          IF REQU-BEERS(RAD-IX) NOT = ALL '+'             
331300                             MOVE REQU-BEERS(RAD-IX)                      
331400                                     TO TAB-BEERS(TAB-IX)                 
331500                             MOVE JA TO RESP-FLTEXT(RAD-IX)               
331600                             MOVE JA TO RADER-UPPDATERADE                 
331700                          END-IF                                          
331800                       END-IF                                             
331900                    END-IF                                                
332000                 END-IF                                                   
332100              END-IF                                                      
332200           END-IF                                                         
332300        END-IF                                                            
332400        ADD +1 TO RAD-IX                                                  
332500     END-PERFORM                                                          
332600     .                                                                    
332700     EJECT                                                                
332800 S06-RIV-TILLK-ART SECTION.                                               
332900     SKIP2                                                                
333000     MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                   
333100     PERFORM IMS-GET-ERSA01                                               
333200     IF SEGMENT-FINNS                                                     
333300         PERFORM IMS-GET-ERSA11                                           
333400         PERFORM UNTIL SEGMENT-SAKNAS                                     
333500            IF ERSA11-FLTEXT = 'N'                                        
333600               MOVE ERSA11-IDARTNR-TILLK TO W-IDARTNR                     
333700               PERFORM IMS-DLET-ERSA                                      
333800               PERFORM IMS-GET-ERSB01                                     
333900               IF SEGMENT-SAKNAS                                          
334000                  PERFORM IMS-GET-ARTC01                                  
334100                  IF SEGMENT-FINNS                                        
334200                     MOVE NEJ TO ART-FLERS                                
334300                     PERFORM IMS-REPL-ARTC                                
334400                  END-IF                                                  
334500               END-IF                                                     
334600            ELSE                                                          
334700               PERFORM IMS-DLET-ERSA                                      
334800            END-IF                                                        
334900            PERFORM IMS-GET-ERSA11                                        
335000         END-PERFORM                                                      
335100     END-IF                                                               
335200     .                                                                    
335300     EJECT                                                                
335400 S07-SKAPA-ERSATT-ART-TO-ERSREG SECTION.                                  
335500     SKIP2                                                                
335600     MOVE REQU-IDARTNR-KEY   TO ERSA01-IDARTNR                            
335700     MOVE REQU-DIERS-ERS     TO DEC-IDFRIDATA                             
335800     MOVE 3                  TO DEC-KVHELTAL                              
335900     MOVE 3                  TO DEC-KVDECIMAL                             
336000     CALL WDECEDIT USING DEC-WDECAREA                                     
336100     MOVE DEC-IDEDITDATA     TO ERSA01-DIERS-ERS                          
336200     IF WS-KDERS =  07 OR 08 OR 09 OR 27 OR 28                            
336300         OR 29 OR 52                                                      
336400         MOVE NEJ            TO ERSA01-FLPUB                              
336500     ELSE                                                                 
336600         MOVE JA             TO ERSA01-FLPUB                              
336700     END-IF                                                               
336800     MOVE REQU-IDUSER        TO ERSA01-IDUSER                             
336900     IF REQU-TEARTNOT = ALL '+' OR SPACE                                  
337000        IF WS-TEARTNOT = SPACE                                            
337100           MOVE SPACE        TO ERSA01-TEARTNOT                           
337200        ELSE                                                              
337300           MOVE WS-TEARTNOT  TO ERSA01-TEARTNOT                           
337400        END-IF                                                            
337500     ELSE                                                                 
337600        MOVE REQU-TEARTNOT   TO ERSA01-TEARTNOT                           
337700     END-IF                                                               
337800     .                                                                    
337900     EJECT                                                                
338000 S08-SKAPA-TILLK-ART-TO-ERSREG SECTION.                                   
338100     SKIP2                                                                
338200     MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                   
338300     PERFORM IMS-GET-ARTC01                                               
338400     PERFORM IMS-GET-ARTC11                                               
338500     IF SEGMENT-FINNS                                                     
338600        MOVE CLAG-IDANSK TO WS-IDANSK                                     
338700     ELSE                                                                 
338800        MOVE ZERO        TO WS-IDANSK                                     
338900     END-IF                                                               
339000     MOVE ZERO TO WS-RAKNARE                                              
339100     MOVE +1 TO TAB-IX                                                    
339200     PERFORM UNTIL TAB-IX > MAX-TAB                                       
339300        IF TAB-IDARTNR-TILLK(TAB-IX) > ZERO                               
339400           PERFORM S09-LAS-TILLK-ART                                      
339500           ADD +1 TO WS-RAKNARE                                           
339600           MOVE SPACE                     TO ERSA11-BEERS                 
339700           MOVE TAB-IDARTNR-TILLK(TAB-IX) TO ERSA11-IDARTNR-TILLK         
339800           MOVE TAB-DIERS-TILLK(TAB-IX)   TO ERSA11-DIERS-TILLK           
339900           MOVE WS-RAKNARE                TO ERSA11-IDKORTNR              
340000           MOVE NEJ                       TO ERSA11-FLTEXT                
340100           MOVE REQU-IDARTNR-KEY          TO W-IDARTNR                    
340200           PERFORM IMS-ISRT-ERSA11                                        
340300        ELSE                                                              
340400           IF TAB-BEERS(TAB-IX) NOT = SPACE                               
340500              MOVE SPACE        TO ERSA11-BEERS                           
340600              ADD +1 TO WS-RAKNARE                                        
340700              MOVE TAB-BEERS(TAB-IX)  TO ERSA11-BEERS                     
340800              MOVE WS-RAKNARE         TO ERSA11-IDKORTNR                  
340900              MOVE JA                 TO ERSA11-FLTEXT                    
341000              MOVE REQU-IDARTNR-KEY TO W-IDARTNR                          
341100              PERFORM IMS-ISRT-ERSA11                                     
341200           END-IF                                                         
341300        END-IF                                                            
341400        ADD +1 TO TAB-IX                                                  
341500     END-PERFORM                                                          
341600     .                                                                    
341700     EJECT                                                                
341800 S09-LAS-TILLK-ART SECTION.                                               
341900     SKIP2                                                                
342000     MOVE TAB-IDARTNR-TILLK(TAB-IX) TO WS-IDARTNR-TILLK                   
342100     MOVE IDARTNR-TILLK-WS TO W-IDARTNR                                   
342200     PERFORM IMS-GET-ARTC01                                               
342300     MOVE ART-TIFINLV      TO TMP1-YYWWD                                  
342400     MOVE WS-TIFINLV-MAX   TO TMP2-YYWWD                                  
342500     PERFORM WY2000P2                                                     
342600     IF TMP1-YYWWD > TMP2-YYWWD                                           
342700        MOVE ART-TIFINLV TO WS-TIFINLV-MAX                                
342800     END-IF                                                               
342900     MOVE JA TO ART-FLERS                                                 
343000     PERFORM IMS-REPL-ARTC                                                
343100     .                                                                    
343200     EJECT                                                                
343300 S10-LAGG-UPP-IDAO SECTION.                                               
343400     SKIP2                                                                
343500                                                                          
343600     MOVE SPACE TO SPARADE-IDAO                                           
343700                   NYA-IDAO                                               
343800     MOVE NEJ   TO IDAO-FINNS                                             
343900                   SW-IDAO-FIXAD                                          
344000                   SW-TOMMA-IDAO-FINNS                                    
344100                                                                          
344200     IF REQU-IDAO = ALL '+' OR SPACE                                      
344300        CONTINUE                                                          
344400     ELSE                                                                 
344500        MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                
344600        PERFORM IMS-GET-ARTC01                                            
344700        MOVE +1 TO IX                                                     
344800        PERFORM UNTIL IX > 5                                              
344900           IF ART-IDAO(IX) = SPACE                                        
345000              MOVE +6 TO IX                                               
345100           ELSE                                                           
345200              IF REQU-IDAO = ART-IDAO(IX)                                 
345300                 MOVE JA TO IDAO-FINNS                                    
345400                 MOVE +6 TO IX                                            
345500              ELSE                                                        
345600                 MOVE ART-IDAO(IX) TO SPARAD-IDAO(IX)                     
345700******************************************************************        
345800*                OM ÄO:T HAR "A" I 1:A POSITION, SKA MAN BYTA UT          
345900*                DETTA ÄO:T I FÖRSTA HAND.                                
346000*                (FÖRUTSATT ATT DET  REDAN FINNS 5 REG ÄO, ALLA           
346100*                 PLATSER FULLA!)                                         
346200*                OBS, MAN KONTROLLERAR ALDRIG DET 1:A ÄO:T, DET SK        
346300*                MAN ALDRIG RÖRA.                                         
346400******************************************************************        
346500                 IF (IX > 1) AND ( IDAO-POS-1(IX) = 'A' )                 
346600                    IF ART-IDAO(5) > SPACE                                
346700                       MOVE REQU-IDAO TO ART-IDAO(IX)                     
346800                       MOVE MAX-IX-VAERDE TO IX                           
346900                       MOVE JA TO SW-IDAO-FIXAD                           
347000                    END-IF                                                
347100                 END-IF                                                   
347200                 ADD +1 TO IX                                             
347300              END-IF                                                      
347400           END-IF                                                         
347500        END-PERFORM                                                       
347600                                                                          
347700******************************************************************        
347800*       ARTC01-IDAO(1) SKA ALDRIG RÖRAS *** ÄT-BEREDNINGEN 8906***        
347900*       DET 2:A ÄO:T 'PUTTAS ÖVER KANTEN ', DET NYA KOMMER IN             
348000*       PÅ PLATS 5.                                                       
348100******************************************************************        
348200        IF (IDAO-FINNS = NEJ) AND (SW-IDAO-FIXAD = NEJ)                   
348300           MOVE +5 TO IX                                                  
348400           PERFORM UNTIL (IX < 1) OR (SW-IDAO-FIXAD = JA)                 
348500              IF SPARAD-IDAO(IX) = SPACE                                  
348600                 MOVE JA TO SW-TOMMA-IDAO-FINNS                           
348700                 SUBTRACT 1 FROM IX                                       
348800              ELSE                                                        
348900                 IF SW-TOMMA-IDAO-FINNS = JA                              
349000                    COMPUTE                                               
349100                    IX-PLUS-1 = IX + 1                                    
349200                    MOVE REQU-IDAO TO ART-IDAO(IX-PLUS-1)                 
349300                    MOVE JA TO SW-IDAO-FIXAD                              
349400                 ELSE                                                     
349500                    IF IX < 2                                             
349600                       CONTINUE                                           
349700                    ELSE                                                  
349800                       COMPUTE                                            
349900                       IX-MINUS-1 = IX - 1                                
350000                       MOVE SPARAD-IDAO(IX) TO                            
350100                                    ART-IDAO(IX-MINUS-1)                  
350200                    END-IF                                                
350300                    SUBTRACT 1 FROM IX                                    
350400                 END-IF                                                   
350500              END-IF                                                      
350600           END-PERFORM                                                    
350700           IF SW-TOMMA-IDAO-FINNS = NEJ                                   
350800              MOVE REQU-IDAO TO ART-IDAO(5)                               
350900           END-IF                                                         
351000        END-IF                                                            
351100******* ENDAST OM ÄO SAKNAS UPPDATERAS ÄO-IND(1)     ************         
351200        IF SPARAD-IDAO(1) = SPACE                                         
351300           MOVE REQU-IDAO TO ART-IDAO(1)                                  
351400        ELSE                                                              
351500           MOVE  SPARAD-IDAO(1) TO ART-IDAO(1)                            
351600        END-IF                                                            
351700        PERFORM IMS-REPL-ARTC                                             
351800     END-IF                                                               
351900     .                                                                    
352000     EJECT                                                                
352100 S12-SKAPA-SATSTRANS SECTION.                                             
352200     SKIP2                                                                
352300     MOVE LOW-VALUE         TO IO-AREA                                    
352400     MOVE ZERO              TO 2303-2304-IDARTNR-ING                      
352500                               2303-2304-IDARTNR-SATS                     
352600     IF ERSATT-FLIART = JA                                                
352700        MOVE REQU-IDARTNR-KEY  TO 2303-2304-IDARTNR-ING                   
352800     END-IF                                                               
352900     IF ERSATT-IDLEVNR = '1002 '                                          
353000        MOVE REQU-IDARTNR-KEY  TO 2303-2304-IDARTNR-SATS                  
353100     END-IF                                                               
353200     MOVE WS-KDERS          TO 2303-2304-KDERS-NEW                        
353300     MOVE ERSATT-KDERS-C1   TO 2303-2304-KDERS-OLD                        
353400                                                                          
353500     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
353600     CALL WDATKONV USING DAT-KDDATFORM                                    
353700                         DAT-I-TIDATUM                                    
353800                         DAT-O-TIDATUM                                    
353900                         DAT-KDSVAR                                       
354000     MOVE DAT-TIAAVVD       TO 2303-2304-TIERSDAT-PREL                    
354100                                                                          
354200     PERFORM IMS-ISRT-2304                                                
354300     .                                                                    
354400     EJECT                                                                
354500 S15-SKAPA-LEV-PLANTRANS SECTION.                                         
354600     SKIP2                                                                
354700     MOVE LOW-VALUE         TO IO-AREA                                    
354800     MOVE WC-CDC-SE         TO W-2203-IDDC                                
354900     MOVE REQU-IDARTNR-KEY  TO 2204-IDARTNR                               
355000     MOVE +21               TO 2204-KDLPORS                               
355100                                                                          
355200     PERFORM IMS-ISRT-XXBJ                                                
355300     .                                                                    
355400     EJECT                                                                
355500 S16-SKAPA-VR-TRANS SECTION.                                              
355600     SKIP2                                                                
355700*****************************************************************         
355800*  9101-TRANS SKAPAS VID                                        *         
355900*  -  ERSÄTTNING UPPÅT TILL NY EK > 10                          *         
356000*  -  (RIVNING AV ERSÄTTNING FRÅN EK > 10 TILL 0)               *         
356100*  -  ÄT FEBR 93 VR VILL HA SAMTLIGA RIVNINGAR TILL 00          *         
356200*                OBEROENDE AV TIDIGARE EK                       *         
356300*                VIPS VILL HA RIVNINGAR > 20 TILL < 10          *         
356400*                BEHANDLAS I VIPS SOM RIVNING TILL 00           *         
356500*  -  BYTE EK FRÅN EK > 20                                      *         
356600*  -  UPPDATERING AV TILLK ARTIKLAR VID EK > 20                 *         
356700*  -  (UPPDAT AV TILLK ART / BYTE EK VID PREL EK EJ TILLÅTEN)   *         
356800*****************************************************************         
356900     IF (WS-KDERS < 10 AND > ZERO) AND (ERSATT-KDERS-C1 < 20)             
357000        CONTINUE                                                          
357100     ELSE                                                                 
357200        MOVE LOW-VALUE TO    IO-AREA                                      
357300        MOVE REQU-IDARTNR-KEY  TO XXID-IDARTNR                            
357400        MOVE ERSATT-KDERS-C1 TO XXID-KDERS-OLD                            
357500        MOVE WS-KDERS        TO XXID-KDERS-NEW                            
357600                                                                          
357700        PERFORM IMS-ISRT-XXID                                             
357800     END-IF                                                               
357900     .                                                                    
358000     EJECT                                                                
358100 S17-UPPDAT-TILLK-ART-ACTION SECTION.                                     
358200     SKIP2                                                                
358300     IF FINNS-PA-ACTION-FILE = NEJ                                        
358400        PERFORM S02-LAS-ERSREG-TILL-ACTIONFILE                            
358500        PERFORM S03-UPPDATERA-RADER                                       
358600     ELSE                                                                 
358700        PERFORM S03-UPPDATERA-RADER                                       
358800     END-IF                                                               
358900     .                                                                    
359000     EJECT                                                                
359100 S18-RAKNA-KVKORT SECTION.                                                
359200     SKIP2                                                                
359300     MOVE NEJ TO TEXT-FINNS                                               
359400     MOVE ZERO TO WS-RAKNARE                                              
359500     MOVE +1 TO TAB-IX                                                    
359600     PERFORM UNTIL TAB-IX > MAX-TAB                                       
359700        IF TAB-IDARTNR-TILLK(TAB-IX) > ZERO                               
359800           ADD +1 TO WS-RAKNARE                                           
359900        ELSE                                                              
360000           IF TAB-BEERS(TAB-IX) NOT = SPACE                               
360100              ADD +1 TO WS-RAKNARE                                        
360200              IF WS-RAKNARE = 1                                           
360300                 MOVE JA TO TEXT-FINNS                                    
360400              END-IF                                                      
360500           END-IF                                                         
360600        END-IF                                                            
360700        ADD +1 TO TAB-IX                                                  
360800     END-PERFORM                                                          
360900     .                                                                    
361000     EJECT                                                                
361100 S19-SKAPA-BEVAKNINGS-SEGMENT SECTION.                                    
361200     SKIP2                                                                
361300     IF WS-KDERS = '01' OR '02' OR '04' OR '05'                           
361400        MOVE 2                 TO ERSA13-KDSTATUS-C1                      
361500        MOVE WS-TIFINLV-MAX    TO ERSA13-TIERSDAT-PREL-C1                 
361600     ELSE                                                                 
361700        IF WS-KDERS = '03' OR '06'                                        
361800           MOVE 1              TO ERSA13-KDSTATUS-C1                      
361900           MOVE REQU-TIERSDAT-PREL TO ERSA13-TIERSDAT-PREL-C1             
362000        ELSE                                                              
362100           IF WS-KDERS = '07' OR '08'                                     
362200              MOVE 6           TO ERSA13-KDSTATUS-C1                      
362300              MOVE ZERO        TO ERSA13-TIERSDAT-PREL-C1                 
362400           ELSE                                                           
362500              IF WS-KDERS = '09'                                          
362600                 MOVE 3        TO ERSA13-KDSTATUS-C1                      
362700                 MOVE ZERO     TO ERSA13-TIERSDAT-PREL-C1                 
362800              ELSE                                                        
362900                 MOVE ZERO     TO ERSA13-KDSTATUS-C1                      
363000                                  ERSA13-TIERSDAT-PREL-C1                 
363100              END-IF                                                      
363200           END-IF                                                         
363300        END-IF                                                            
363400     END-IF                                                               
363500                                                                          
363600     MOVE ZERO                    TO ERSA13-KDSTATUS-C2                   
363700                                     ERSA13-TIERSDAT-PREL-C2              
363800                                                                          
363900     MOVE 'IDAG  '    TO DAT-KDDATFORM                                    
364000     CALL WDATKONV USING DAT-KDDATFORM                                    
364100                         DAT-I-TIDATUM                                    
364200                         DAT-O-TIDATUM                                    
364300                         DAT-KDSVAR                                       
364400     MOVE DAT-TIAAVVD TO ERSA13-TIERSDAT-REG                              
364500     .                                                                    
364600     EJECT                                                                
364700 S20-ROER-EJ-FAELT SECTION.                                               
364800     SKIP2                                                                
364900     MOVE ALL '+'              TO RESP-IDUSER                             
365100                                  RESP-KDERS-C1                           
365200                                  RESP-KDERS-C2                           
365300                                  RESP-TIERSDAT-REG                       
365400                                  RESP-TIERSDAT-PREL-C1                   
365500                                  RESP-TIERSDAT-PREL-C2                   
365510                                  RESP-TIERSDAT                           
365520                                                                          
365600     IF REQU-DIERS-ERS = ALL '+'                                          
365700        MOVE SPACE             TO RESP-DIERS-ERS                          
365800     ELSE                                                                 
365900        MOVE ALL '+'           TO RESP-DIERS-ERS                          
366000     END-IF                                                               
366010                                                                          
366100     IF REQU-IDAO = ALL '+'                                               
366200        MOVE SPACE TO RESP-IDAO                                           
366300     ELSE                                                                 
366400        MOVE ALL '+'           TO RESP-IDAO                               
366500     END-IF                                                               
366600     IF REQU-KDERS = ALL '+'                                              
366700        MOVE SPACE             TO RESP-KDERS                              
366800     ELSE                                                                 
366900        MOVE ALL '+'           TO RESP-KDERS                              
367000     END-IF                                                               
367100     IF REQU-TIERSDAT-PREL = ALL '+'                                      
367200        MOVE SPACE             TO RESP-TIERSDAT-PREL                      
367300     ELSE                                                                 
367400        MOVE ALL '+'           TO RESP-TIERSDAT-PREL                      
367500     END-IF                                                               
367600     IF REQU-TEARTNOT = ALL '+'                                           
367700        MOVE SPACE TO RESP-TEARTNOT                                       
367800     ELSE                                                                 
367900        MOVE ALL '+'           TO RESP-TEARTNOT                           
368000     END-IF                                                               
368100     MOVE SPACE                TO RESP-FLKLAR                             
368200                                                                          
368300     MOVE +1 TO RAD-IX                                                    
368400     PERFORM UNTIL RAD-IX > MAX-RAD                                       
368500       IF REQU-IDARTNR-TILLK(RAD-IX) = ALL '+' AND                        
368600          REQU-DIERS-TILLK(RAD-IX) = ALL '+' AND                          
368700          REQU-BEERS(RAD-IX) = ALL '+'                                    
368800          MOVE ALL '+'           TO RESP-IDARTNR-TILLK(RAD-IX)            
368900                                    RESP-DIERS-TILLK(RAD-IX)              
369000                                    RESP-BEERS(RAD-IX)                    
369100                                    RESP-IDKORTNR(RAD-IX)                 
369200       ELSE                                                               
369300          IF REQU-IDARTNR-TILLK(RAD-IX) = ALL '+'                         
369400             MOVE SPACES           TO RESP-IDARTNR-TILLK(RAD-IX)          
369500          ELSE                                                            
369600             MOVE ALL '+'                                                 
369610                                   TO RESP-IDARTNR-TILLK(RAD-IX)          
369700          END-IF                                                          
369800          IF REQU-DIERS-TILLK(RAD-IX) = ALL '+'                           
369900             IF REQU-IDARTNR-TILLK(RAD-IX) = ALL '+'                      
370000                MOVE SPACES        TO RESP-DIERS-TILLK(RAD-IX)            
370410             ELSE                                                         
370420                MOVE ALL '+'       TO RESP-IDARTNR-TILLK(RAD-IX)          
370430                                      RESP-DIERS-TILLK(RAD-IX)            
370500             END-IF                                                       
370600          ELSE                                                            
370700             MOVE    ALL '+'       TO RESP-IDARTNR-TILLK(RAD-IX)          
370800                                      RESP-DIERS-TILLK(RAD-IX)            
370900          END-IF                                                          
371000          IF REQU-BEERS(RAD-IX) = ALL '+'                                 
371100             MOVE SPACES            TO RESP-BEERS(RAD-IX)                 
371200          ELSE                                                            
371300             MOVE ALL '+'           TO RESP-BEERS(RAD-IX)                 
371400          END-IF                                                          
371500          IF REQU-IDKORTNR(RAD-IX) = ALL '+'                              
371600             MOVE SPACE             TO RESP-IDKORTNR(RAD-IX)              
371700          ELSE                                                            
371800             MOVE ALL '+'           TO RESP-IDKORTNR(RAD-IX)              
371900          END-IF                                                          
372000       END-IF                                                             
372100       MOVE ALL '+'                 TO RESP-BEART(RAD-IX)                 
372200                                       RESP-FLTEXT(RAD-IX)                
372300                                                                          
372400       IF REQU-IDKORTNR(RAD-IX) = ALL '+' OR SPACE                        
372500          CONTINUE                                                        
372600       ELSE                                                               
372700          IF REQU-IDKORTNR(RAD-IX) NUMERIC                                
372800             IF REQU-IDKORTNR(RAD-IX) NOT = ZERO                          
372900                IF REQU-FLTEXT(RAD-IX) = JA OR NEJ                        
373000                   MOVE MFS-STAENG-FAELT                                  
373100                                 TO RESP-IDKORTNR-ATTR(RAD-IX)            
373200                END-IF                                                    
373300             END-IF                                                       
373400          END-IF                                                          
373500       END-IF                                                             
373600                                                                          
373700       IF REQU-FLTEXT(RAD-IX) = NEJ                                       
373800          MOVE MFS-STAENG-FAELT TO RESP-BEERS-ATTR(RAD-IX)                
373900       ELSE                                                               
374000          IF REQU-FLTEXT(RAD-IX) = JA                                     
374100             MOVE MFS-STAENG-FAELT                                        
374200                           TO RESP-IDARTNR-TILLK-ATTR(RAD-IX)             
374300             MOVE MFS-STAENG-FAELT                                        
374400                           TO RESP-DIERS-TILLK-ATTR(RAD-IX)               
374500          END-IF                                                          
374600       END-IF                                                             
374700       ADD +1 TO RAD-IX                                                   
374800     END-PERFORM                                                          
374900     .                                                                    
375000     EJECT                                                                
375100 S21-SKAPA-KDP-TRANS SECTION.                                             
375200     SKIP2                                                                
375300     MOVE WS-KDPRODSL        TO TEST-KDPRODSL                             
375400     IF KDPRODSL-VCBV                                                     
375500        CONTINUE                                                          
375600     ELSE                                                                 
375700        MOVE LOW-VALUE       TO IO-AREA                                   
375800        ACCEPT ZZAC-TIKLOCK  FROM TIME                                    
375900        ACCEPT ZZAC-TIAAMMDD FROM DATE                                    
376000        ADD +1               TO W-IDLOGLOP                                
376100        MOVE W-IDLOGLOP      TO ZZAC-IDLOGLOP                             
376200        MOVE 'RZU'           TO KDP-IDPTYP                                
376300        MOVE REQU-IDARTNR-KEY  TO KDP-IDARTNR                             
376400        MOVE REQU-IDARTNR-KEY  TO W092-SORTBGP                            
376500        MOVE KDP-W10111      TO ZZAC-LOGGPOST                             
376600        MOVE W092-AREA       TO ZZAC-SORTPOST                             
376700        PERFORM IMS-ISRT-ZZAC                                             
376800        PERFORM UNTIL SEGMENT-FINNS                                       
376900           IF W-IDLOGLOP = 9                                              
377000              MOVE ZERO TO W-IDLOGLOP                                     
377100              ACCEPT ZZAC-TIKLOCK  FROM TIME                              
377200           END-IF                                                         
377300           ADD +1            TO W-IDLOGLOP                                
377400           MOVE W-IDLOGLOP   TO ZZAC-IDLOGLOP                             
377500           PERFORM IMS-ISRT-ZZAC                                          
377600        END-PERFORM                                                       
377700     END-IF                                                               
377800     .                                                                    
377900     EJECT                                                                
378000 S22-ROER-EJ-FAELT SECTION.                                               
378100     SKIP2                                                                
378200     MOVE MFS-STAENG-FAELT  TO RESP-DIERS-ERS-ATTR                        
378300                               RESP-IDAO-ATTR                             
378400                               RESP-TIERSDAT-PREL-ATTR                    
378500                               RESP-TEARTNOT-ATTR                         
378600                               RESP-FLKLAR-ATTR                           
378700                               RESP-KDERS-ATTR                            
378800     MOVE ALL '+'           TO RESP-TEARTNOT                              
378900                               RESP-IDUSER                                
379700                               RESP-IDAO                                  
379900                               RESP-TEARTNOT                              
380000                               RESP-KDERS-C1                              
380010                               RESP-KDERS-C2                              
380020                               RESP-TIERSDAT-REG                          
380030                               RESP-TIERSDAT-PREL-C1                      
380040                               RESP-TIERSDAT-PREL-C2                      
380050                               RESP-TIERSDAT                              
380060                               RESP-DIERS-ERS                             
380070                               RESP-TIERSDAT-PREL                         
380080                               RESP-KDERS                                 
380091                                                                          
380093                                                                          
380100     MOVE +1 TO RAD-IX                                                    
380200     PERFORM UNTIL RAD-IX > MAX-RAD                                       
380300        MOVE ALL '+'           TO RESP-IDARTNR-TILLK(RAD-IX)              
380400                                  RESP-DIERS-TILLK(RAD-IX)                
380500                                  RESP-BEERS(RAD-IX)                      
380600                                  RESP-BEART(RAD-IX)                      
380700                                  RESP-IDKORTNR(RAD-IX)                   
380800                                  RESP-FLTEXT(RAD-IX)                     
380900        MOVE MFS-STAENG-FAELT  TO RESP-IDARTNR-TILLK-ATTR                 
381000                                  (RAD-IX)                                
381100        MOVE MFS-STAENG-FAELT  TO RESP-DIERS-TILLK-ATTR(RAD-IX)           
381200                                  RESP-BEERS-ATTR(RAD-IX)                 
381300                                  RESP-IDKORTNR-ATTR(RAD-IX)              
381400        ADD +1 TO RAD-IX                                                  
381500     END-PERFORM                                                          
381600     .                                                                    
381700     EJECT                                                                
381800 S23-TRANS-TIKO-RSLISTA SECTION.                                          
381900     SKIP2                                                                
382000     MOVE NEJ TO FL-NYPONART                                              
382100                                                                          
382200     MOVE REQU-IDARTNR-KEY TO W-IDARTNR                                   
382300     PERFORM IMS-GET-ARTG01                                               
382400     IF SEGMENT-FINNS                                                     
382500        MOVE JA TO FL-NYPONART                                            
382600        IF WS-KDERS = '00'                                                
382700           IF ERSATT-PRARTSTD = ZERO                                      
382800              IF ERSATT-KDBPSR = 8 AND ERSATT-FLLSRDEL = NEJ              
382900                 CONTINUE                                                 
383000              ELSE                                                        
383100                 MOVE '1' TO ARTG-ART-KDANSKQ                             
383200              END-IF                                                      
383300           END-IF                                                         
383400           MOVE 'R' TO ARTG-ART-KDRESBED                                  
383500           PERFORM IMS-REPL-ARTG                                          
383600        ELSE                                                              
383700           IF WS-KDERS = '09' OR '19' OR '29' OR '52'                     
383800              MOVE 'U' TO ARTG-ART-KDRESBED                               
383900           ELSE                                                           
384000              MOVE 'E' TO ARTG-ART-KDRESBED                               
384100           END-IF                                                         
384200           MOVE ARTG-ART-KDANSKQ  TO SPAR-KDANSKQ                         
384300                                                                          
384400           IF WS-KDERS > 20                                               
384500             IF ARTG-ART-KDANSKQ NOT = 9                                  
384600               MOVE +0 TO ARTG-ART-KDANSKQ                                
384700             END-IF                                                       
384800           END-IF                                                         
384900                                                                          
385000           IF WS-KDERS > 0 AND WS-KDERS < 20                              
385100           OR WS-KDERS = 52                                               
385200              IF SPAR-KDANSKQ = '2' OR '4'                                
385300               MOVE '0' TO ARTG-ART-KDANSKQ                               
385400               MOVE 0 TO ARTG-ART-KVPROG                                  
385500              END-IF                                                      
385600           END-IF                                                         
385700                                                                          
385800           PERFORM IMS-REPL-ARTG                                          
385900                                                                          
386000           PERFORM IMS-GET-ARTC01                                         
386100           MOVE ART-KDPRODSL   TO WS-KDPRODSL                             
386200                                  TEST-KDPRODSL                           
386300           PERFORM IMS-GET-ARTC11                                         
386400           MOVE CLAG-KDBPSR    TO WS-KDBPSR                               
386500*          *********************************************                  
386600*          ********** KDPRODSL-PV-SKALL-KÖPA ***********                  
386700*          *********************************************                  
386800           IF KDPRODSL-UTAN-EMB                                           
386900           OR KDPRODSL-LOCAL                                              
387000              IF SPAR-IDPROJ = '9501' OR '9502'                           
387100              OR ERSATT-KDAVT > 0                                         
387200                 CONTINUE                                                 
387300              ELSE                                                        
387400                 IF ARTG-ART-IDPROJK = SPACE                              
387500                    CONTINUE                                              
387600                 ELSE                                                     
387700                    MOVE JA TO SW-TIKO                                    
387800                    IF SW-TIKO = JA                                       
387900                       IF SPAR-KDANSKQ = '2'                              
388000                       AND ARTG-ART-KVLEVBEG > 0                          
388100                          CONTINUE                                        
388200                       ELSE                                               
388300                          IF SPAR-KDANSKQ = '4'                           
388400                          AND ARTG-ART-KVLEVBEG > 0                       
388500                             MOVE '4' TO W-1142-KEY-X                     
388600                             PERFORM IMS-GHU-XXAV11-GE                    
388700                             IF SEGMENT-FINNS                             
388800                                PERFORM IMS-DLET-XXAV11                   
388900                             END-IF                                       
389000                          END-IF                                          
389100                          MOVE NEJ TO SW-TIKO                             
389200                       END-IF                                             
389300                    END-IF                                                
389400                                                                          
389500                    IF SW-TIKO = JA                                       
389600                      MOVE NEJ      TO SW-TIKO                            
389700                      IF (WS-KDBPSR = 8 AND ERSATT-FLLSRDEL = NEJ)        
389800                        PERFORM IMS-GU-XXAV01                             
389900                        MOVE '1'        TO W-1142-KEY-X                   
390000                        PERFORM IMS-GNP-XXAV11                            
390100                                                                          
390200                        PERFORM UNTIL SW-TIKO = JA                        
390300                        OR SEGMENT-SAKNAS                                 
390400                           IF REQU-IDARTNR-KEY = XXAV-1142-IDARTNR        
390500                              MOVE JA     TO SW-TIKO                      
390600                           ELSE                                           
390700                              PERFORM IMS-GNP-XXAV11                      
390800                           END-IF                                         
390900                        END-PERFORM                                       
391000                                                                          
391100                        IF SW-TIKO = NEJ                                  
391200                          MOVE SPACE   TO XXAV-1142-WDGX1142              
391300                          MOVE +1      TO XXAV-1142-KDSEGKEY              
391400                          MOVE REQU-IDARTNR-KEY TO                        
391500                                           XXAV-1142-IDARTNR              
391600                          PERFORM IMS-ISRT-XXAV                           
391700                        END-IF                                            
391800                      ELSE                                                
391900                        PERFORM IMS-GU-XXAV01                             
392000                        MOVE '1'        TO W-1142-KEY-X                   
392100                        PERFORM IMS-GNP-XXAV11                            
392200                                                                          
392300                        PERFORM UNTIL SW-TIKO = JA                        
392400                        OR SEGMENT-SAKNAS                                 
392500                          IF REQU-IDARTNR-KEY = XXAV-1142-IDARTNR         
392600                             MOVE JA     TO SW-TIKO                       
392700                          ELSE                                            
392800                             PERFORM IMS-GNP-XXAV11                       
392900                          END-IF                                          
393000                        END-PERFORM                                       
393100                                                                          
393200                        IF SW-TIKO = NEJ                                  
393300                           MOVE SPACE      TO XXAV-1142-WDGX1142          
393400                           MOVE REQU-IDARTNR-KEY                          
393410                                           TO XXAV-1142-IDARTNR           
393500                           MOVE +1         TO XXAV-1142-KDSEGKEY          
393600                           PERFORM IMS-ISRT-XXAV                          
393700                        END-IF                                            
393800                      END-IF                                              
393900                    END-IF                                                
394000                 END-IF                                                   
394100              END-IF                                                      
394200           END-IF                                                         
394300                                                                          
394400        END-IF                                                            
394500     END-IF                                                               
394600     .                                                                    
394700     EJECT                                                                
394800  S24-TRANS-TILL-BASL SECTION.                                            
394900*****************************************************************         
395000*  ÄT NOV 92  TRANS 1158 TILL ERSÄTTNINGSBEVAKNING BASLAGER     *         
395100*             VID NY EK > 10. TRANSEN LÄSES OCH DELEATAS I      *         
395200*             I W115D1.                                         *         
395300*****************************************************************         
395400     SKIP2                                                                
395500     IF FL-NYPONART = JA                                                  
395600        IF ERSATT-KDERS-C1 = 00 OR WS-KDERS = 0                           
395700           MOVE REQU-IDARTNR-KEY  TO W-1116-IDARTNR                       
395800           PERFORM IMS-GET-XXAW11-GHU                                     
395900           IF SEGMENT-FINNS                                               
396000              MOVE ERSATT-KDERS-C1 TO XXAW-1116-KDERS-OLD                 
396100              MOVE WS-KDERS        TO XXAW-1116-KDERS-NEW                 
396200              MOVE DAGENS-DATUM    TO XXAW-1116-TIREGDAT                  
396300              PERFORM IMS-REPL-XXAW                                       
396400           ELSE                                                           
396500              MOVE LOW-VALUE       TO IO-AREA                             
396600              MOVE REQU-IDARTNR-KEY  TO XXAW-1116-IDARTNR                 
396700              MOVE ERSATT-KDERS-C1 TO XXAW-1116-KDERS-OLD                 
396800              MOVE WS-KDERS        TO XXAW-1116-KDERS-NEW                 
396900              MOVE DAGENS-DATUM    TO XXAW-1116-TIREGDAT                  
397000              PERFORM IMS-ISRT-XXAW                                       
397100           END-IF                                                         
397200        END-IF                                                            
397300                                                                          
397400        IF (ERSATT-KDERS-C1 < 10 AND WS-KDERS > 10) OR                    
397500           (ERSATT-KDERS-C1 > 10 AND WS-KDERS = ZERO)                     
397600            MOVE REQU-IDARTNR-KEY TO W-1158-IDARTNR                       
397700            PERFORM IMS-GET-XXCW11                                        
397800            IF SEGMENT-FINNS                                              
397900               IF WS-KDERS > 10                                           
398000                  MOVE WS-KDERS      TO XXCW-1158-KDERS                   
398100                  MOVE DAGENS-DATUM  TO XXCW-1158-TIREGDAT                
398200                  PERFORM IMS-REPL-XXCW                                   
398300               ELSE                                                       
398400                  PERFORM IMS-DLET-XXCW                                   
398500               END-IF                                                     
398600            ELSE                                                          
398700               IF WS-KDERS > 10                                           
398800                  MOVE LOW-VALUE TO IO-AREA                               
398900                  MOVE REQU-IDARTNR-KEY  TO XXCW-1158-IDARTNR             
399000                  MOVE WS-KDERS      TO XXCW-1158-KDERS                   
399100                  MOVE DAGENS-DATUM  TO XXCW-1158-TIREGDAT                
399200                  PERFORM IMS-ISRT-XXCW                                   
399300               END-IF                                                     
399400            END-IF                                                        
399500         END-IF                                                           
399600     END-IF                                                               
399700     .                                                                    
399800     EJECT                                                                
399900 S25-TRANS-TILL-PPMS SECTION.                                             
400000     SKIP2                                                                
400100     MOVE NEJ TO PPMS-TRANS                                               
400200                                                                          
400300     EVALUATE TRUE                                                        
400400     WHEN ERSATT-KDERS-C1 < 10 AND WS-KDERS > 10                          
400500        MOVE JA TO PPMS-TRANS                                             
400600     WHEN ERSATT-KDERS-C1 > 10 AND WS-KDERS < 10                          
400700        MOVE JA TO PPMS-TRANS                                             
400800     WHEN ERSATT-KDERS-C1 > 10 AND WS-KDERS > 10                          
400900        MOVE JA TO PPMS-TRANS                                             
401000     WHEN OTHER                                                           
401100        CONTINUE                                                          
401200     END-EVALUATE                                                         
401300                                                                          
401400     IF PPMS-TRANS = JA                                                   
401500        MOVE SPACE              TO PPMS-W111PPMS                          
401600        MOVE REQU-IDARTNR-KEY   TO PPMS-IDARTNR                           
401700        IF WS-KDERS > 10                                                  
401800           MOVE WS-KDERS        TO PPMS-KDERS                             
401900        ELSE                                                              
402000           MOVE ZERO            TO PPMS-KDERS                             
402100        END-IF                                                            
402200                                                                          
402300        EVALUATE TRUE                                                     
402400        WHEN WS-KDERS = ZERO                                              
402500           CONTINUE                                                       
402600        WHEN WS-KDERS = 09 OR 19 OR 29 OR 52                              
402700           CONTINUE                                                       
402800        WHEN WS-KDERS-2 =  4 OR 5 OR 6 OR 8                               
402900           MOVE 'FLERA'         TO PPMS-ANMARKNING                        
403000        WHEN OTHER                                                        
403100           MOVE REQU-IDARTNR-KEY TO W-IDARTNR                             
403200           PERFORM IMS-GET-ERSA01                                         
403300           IF ERSA01-KVKORT > 1                                           
403400              MOVE 'FLERA'          TO PPMS-ANMARKNING                    
403500           ELSE                                                           
403600              PERFORM IMS-GET-ERSA11                                      
403700              MOVE ERSA11-IDARTNR-TILLK TO                                
403800                      PPMS-IDARTNR-TILLK-WS                               
403900              MOVE PPMS-IDARTNR-TILLK-WS TO                               
404000                    WS-PPMS-IDARTNR-TILLK                                 
404100              INSPECT WS-PPMS-IDARTNR-TILLK TALLYING                      
404200                    NOLL-RAKNARE FOR LEADING ZERO                         
404300              ADD +1 TO NOLL-RAKNARE                                      
404400              UNSTRING WS-PPMS-IDARTNR-TILLK INTO                         
404500                   PPMS-ANMARKNING WITH POINTER                           
404600                   NOLL-RAKNARE                                           
404700           END-IF                                                         
404800        END-EVALUATE                                                      
404900                                                                          
405000       MOVE 'RPP'       TO PPMS-IDPTYP                                    
405100                                                                          
405200       MOVE SPACE                  TO FILC-AREA                           
405300       MOVE PROGRAM-NAMN           TO FILC-FIL-IDPGM                      
405400       ACCEPT FILC-FIL-TIREGDAT    FROM DATE                              
405500       ACCEPT FILC-FIL-TIKLOCK     FROM TIME                              
405600       ADD +1                      TO WS-IDSEKVNR                         
405700       MOVE WS-IDSEKVNR            TO FILC-FIL-IDSEKVNR                   
405800       MOVE 'W111RPP '             TO FILC-FIL-IDCPYTXT                   
405900       MOVE PPMS-W111PPMS          TO FILC-FIL-WDR301-DATA                
406000                                                                          
406100       PERFORM IMS-ISRT-FILC-TRANS                                        
406200       PERFORM UNTIL SEGMENT-FINNS                                        
406300         ACCEPT FILC-FIL-TIREGDAT    FROM DATE                            
406400         ACCEPT FILC-FIL-TIKLOCK     FROM TIME                            
406500         ADD +1                      TO WS-IDSEKVNR                       
406600         MOVE WS-IDSEKVNR            TO FILC-FIL-IDSEKVNR                 
406700                                                                          
406800         PERFORM IMS-ISRT-FILC-TRANS                                      
406900       END-PERFORM                                                        
407000     END-IF                                                               
407100     .                                                                    
407200     EJECT                                                                
407300 S26-KOLLA-RASA SECTION.                                                  
407400     SKIP2                                                                
407500     MOVE NEJ TO WS-EXTERN-SATS                                           
407600     MOVE REQU-IDARTNR-KEY TO W-IDARTNR-S                                 
407700     MOVE SPACE TO W-IDLEVNR-S                                            
407800     MOVE SPACE TO W-BELEVART-S                                           
407900     PERFORM IMS-GET-SATB-CSEQ                                            
408000     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
408100        OR WS-EXTERN-SATS = JA                                            
408200        IF SATB-STR-IDLEVNR = SPACE OR LOW-VALUE                          
408300           IF SATB-STR-IDARTNR < 100000000                                
408400             MOVE SATB-RAD-TISTODAT   TO TMP1-YYMMDD                      
408500             MOVE DAGENS-DATUM        TO TMP2-YYMMDD                      
408600             PERFORM WY2000P1                                             
408700             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
408800               MOVE JA TO WS-EXTERN-SATS                                  
408900             END-IF                                                       
409000           END-IF                                                         
409100        END-IF                                                            
409200        PERFORM IMS-GET-SATB-CSEQ                                         
409300     END-PERFORM                                                          
409400     .                                                                    
409500 S27-TRANS-TILL-WDR5 SECTION.                                             
409600     SKIP2                                                                
409700     IF (ERSATT-KDERS-C1 < 10 AND REQU-KDERS > 10                         
409800        OR ERSATT-KDERS-C1 > 10 AND REQU-KDERS < 10)                      
409900        MOVE W-IDARTNR  TO     2228-IDARTNR                               
410000        MOVE LOW-VALUE  TO     2228-LOW-VALUE                             
410100        MOVE SPACE      TO     2228-FILLER                                
410200        PERFORM IMS-ISRT-2228                                             
410300      END-IF                                                              
410400     .                                                                    
410500     EJECT                                                                
410600 S28-SKAPA-B65-TRANS SECTION.                                             
410700                                                                          
410800     IF WS-KDERS = 52                                                     
410900        IF ERSATT-KDAVT = 1                                               
411000           MOVE REQU-IDARTNR-KEY TO W-IDARTNR                             
411100           PERFORM IMS-GET-ARTC01                                         
411200           PERFORM IMS-GET-ARTC23                                         
411300                                                                          
411400           PERFORM UNTIL SEGMENT-SAKNAS                                   
411500              MOVE AVT-IDAVTAL TO W-IDAVTAL-RED                           
411600              MOVE W-PREFIX TO W-PREFIX-NUM                               
411700***               ÄVEN NAP-AVTAL MED, PREFIX = 004                        
411800              IF (W-PREFIX-NUM > 99 AND W-PREFIX-NUM < 790) OR            
411900              (W-PREFIX-NUM > 799 AND W-PREFIX-NUM < 987) OR              
412000              (W-PREFIX-NUM > 987 AND W-PREFIX-NUM < 1000) OR             
412100              (W-PREFIX-NUM = 004)                                        
412200                                                                          
412300                 IF AVT-IDLEVNR-AVT (5:1) = SPACE                         
412400                    MOVE AVT-IDLEVNR-AVT TO IDLEVNR-ALFA                  
412500                    MOVE ZERO TO TALLY                                    
412600                    INSPECT IDLEVNR-ALFA TALLYING TALLY FOR               
412700                        CHARACTERS BEFORE INITIAL SPACE                   
412800                    IF TALLY = ZERO                                       
412900                       MOVE ZERO TO WS-IDLEVNR-NUM                        
413000                    ELSE                                                  
413100                       MOVE IDLEVNR-ALFA(1:TALLY)                         
413200                                  TO WS-IDLEVNR-NUM                       
413300                    END-IF                                                
413400                    MOVE WS-IDLEVNR-NUM  TO A310-LEVNUM                   
413500                 ELSE                                                     
413600                    MOVE AVT-IDLEVNR-AVT TO A310-LEVNUM                   
413700                 END-IF                                                   
413800                 MOVE AVT-IDAVTAL     TO W-IDAVTAL-RED                    
413900                 MOVE W-PREFIX        TO A310-BESTPREF                    
414000                 MOVE W-AVTALSNR      TO A310-BESTLNR                     
414100                 MOVE W-SUFFIX        TO A310-BESTSUFF                    
414200                 MOVE SPACE           TO A310-LEVNUM-GODSM                
414300                                         A310-ANT-BESTANN                 
414400                 MOVE 'RY2'           TO A310-KT                          
414500                 MOVE DAGENS-DATUM    TO A310-DATUM-UTSKR                 
414600                 MOVE REQU-IDARTNR-KEY  TO W-IDARTNR-8                    
414700                 MOVE W-IDARTNR-8     TO A310-ARTNR                       
414800                                         W092-SORTBGP                     
414900                                                                          
415000                 ACCEPT ZZAC-TIKLOCK  FROM TIME                           
415100                 ACCEPT ZZAC-TIAAMMDD FROM DATE                           
415200                 ADD +1 TO W-IDLOGLOP                                     
415300                 MOVE W-IDLOGLOP     TO ZZAC-IDLOGLOP                     
415400                 MOVE A310-A310B65   TO ZZAC-LOGGPOST                     
415500                 MOVE W092-AREA      TO ZZAC-SORTPOST                     
415600                 PERFORM IMS-ISRT-ZZAC                                    
415700                 PERFORM UNTIL SEGMENT-FINNS                              
415800                    IF W-IDLOGLOP = 9                                     
415900                       MOVE ZERO TO W-IDLOGLOP                            
416000                       ACCEPT ZZAC-TIKLOCK  FROM TIME                     
416100                    END-IF                                                
416200                    ADD +1            TO W-IDLOGLOP                       
416300                    MOVE W-IDLOGLOP   TO ZZAC-IDLOGLOP                    
416400                    PERFORM IMS-ISRT-ZZAC                                 
416500                 END-PERFORM                                              
416600***           ELSE                                                        
416700***              IF (W-PREFIX-NUM > 639 AND W-PREFIX-NUM < 660)           
416800***                MOVE SPACE      TO XXAV-1142-WDGX1142                  
416900***                MOVE REQU-IDARTNR-KEY TO XXAV-1142-IDARTNR             
417000***                MOVE +2         TO XXAV-1142-KDSEGKEY                  
417100***                MOVE 'A'        TO XXAV-1142-KDSVAR                    
417200***                PERFORM IMS-ISRT-XXAV                                  
417300***              END-IF                                                   
417400              END-IF                                                      
417500                                                                          
417600              PERFORM IMS-GET-ARTC23                                      
417700           END-PERFORM                                                    
417800        END-IF                                                            
417900     END-IF                                                               
418000     .                                                                    
418100     EJECT                                                                
418200 S29-ALARM-KDERS-UPD SECTION.                                             
418300*    CREATE ALARM WHEN SS CODE IS UPDATED TO ZERO                         
418400     MOVE WS-IDANSK-ALARM     TO W-IDANSK-2232                            
418500     PERFORM IMS-GU-R220                                                  
418600     IF SEGMENT-FINNS                                                     
418700       MOVE WDR220-2232-IDANSK-LARM                                       
418800                              TO W-IDANSK-2223                            
418900     ELSE                                                                 
419000       MOVE ZERO              TO W-IDANSK-2223                            
419100     END-IF                                                               
419200     MOVE '2223'              TO WDR501-2223-IDHTYP                       
419300     MOVE W-IDANSK-2223       TO WDR501-2223-IDANSK                       
419400     MOVE LOW-VALUE           TO WDR501-2223-LOW-VALUE                    
419500     PERFORM IMS-ISRT-R501                                                
419600     PERFORM IMS-GHU-R501                                                 
419700     MOVE FUNCTION CURRENT-DATE(3:6)                                      
419800                              TO WDR550-2224-TISENBEK-DAG                 
419900     MOVE FUNCTION CURRENT-DATE(11:6)                                     
420000                              TO WDR550-2224-TISENBEK-KL                  
420100     MOVE 610                 TO WDR550-2224-KDLARM                       
420200     MOVE W-IDARTNR           TO WDR550-2224-IDARTNR                      
420300     MOVE WS-IDDC-ALARM       TO WDR550-2224-IDDC                         
420400     MOVE JA                  TO WDR550-2224-FLNYLARM                     
420500     MOVE ZERO                TO WDR550-2224-IDDISTR                      
420600                                 WDR550-2224-IDKUNDNR                     
420700     MOVE '0000000   '        TO WDR550-2224-IDKUNDRF                     
420800     MOVE 1                   TO WDR550-2224-IDLOPNR                      
420900     MOVE DAGENS-DATUM        TO WDR550-2224-TIREGDAT                     
421000     MOVE SPACE               TO WDR550-2224-IDTRANS                      
421100                                 WDR550-2224-KDMFSFOR                     
421200     MOVE ZERO                TO WDR550-2224-IDKR                         
421300     MOVE SPACE               TO WDR550-2224-IDLEVNR                      
421400                                                                          
421500     PERFORM IMS-ISRT-R550                                                
421600*                                                                         
421700*    IF PART LOCALLY PROCURED IN CN OR US, CREATE ALARM                   
421800     PERFORM S29A-CHK-PART-SOURCING                                       
421900     .                                                                    
422000     EJECT                                                                
422100 S29A-CHK-PART-SOURCING SECTION.                                          
422200                                                                          
422300     PERFORM IMS-GU-WDK701                                                
422400     IF SEGMENT-FINNS                                                     
422500        PERFORM IMS-GN-WDK711                                             
422600        IF SEGMENT-FINNS                                                  
422700          PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                      
422800*           CHECK IF PART IS PROCURED IN US OR CHINA                      
422900            IF SLAG-IDDC-REF = SPACES                                     
423000               MOVE SLAG-IDDC                TO W-IDDC                    
423100               PERFORM IMS-GU-WDB601                                      
423200               IF SEGMENT-FINNS                                           
423300                  IF DCS-NDC-NA OR DCS-NDC-CN                             
423400                     MOVE ZERO               TO WS-IDANSK-ALARM           
423500                     MOVE SLAG-IDDC          TO WS-IDDC-ALARM             
423600                     PERFORM IMS-GNP-WDK722                               
423700                     IF SEGMENT-FINNS                                     
423800                        MOVE XLAG-IDANSK     TO WS-IDANSK-ALARM           
423900                     END-IF                                               
424000                     PERFORM S29AA-CREATE-SS-ALARM-LOCAL                  
424100                  END-IF                                                  
424200               END-IF                                                     
424300            END-IF                                                        
424400            PERFORM IMS-GN-WDK711                                         
424500          END-PERFORM                                                     
424600        END-IF                                                            
424700     END-IF                                                               
424800     .                                                                    
424900     EJECT                                                                
425000 S29AA-CREATE-SS-ALARM-LOCAL  SECTION.                                    
425100                                                                          
425200     MOVE WS-IDANSK-ALARM     TO W-IDANSK-2232                            
425300     PERFORM IMS-GU-R220                                                  
425400     IF SEGMENT-FINNS                                                     
425500       MOVE WDR220-2232-IDANSK-LARM                                       
425600                              TO W-IDANSK-2223                            
425700     ELSE                                                                 
425800       MOVE ZERO              TO W-IDANSK-2223                            
425900     END-IF                                                               
426000     MOVE '2223'              TO WDR501-2223-IDHTYP                       
426100     MOVE W-IDANSK-2223       TO WDR501-2223-IDANSK                       
426200     MOVE LOW-VALUE           TO WDR501-2223-LOW-VALUE                    
426300     PERFORM IMS-ISRT-R501                                                
426400     PERFORM IMS-GHU-R501                                                 
426500     MOVE FUNCTION CURRENT-DATE(3:6)                                      
426600                              TO WDR550-2224-TISENBEK-DAG                 
426700     MOVE FUNCTION CURRENT-DATE(11:6)                                     
426800                              TO WDR550-2224-TISENBEK-KL                  
426900     MOVE 610                 TO WDR550-2224-KDLARM                       
427000     MOVE W-IDARTNR           TO WDR550-2224-IDARTNR                      
427100     MOVE WS-IDDC-ALARM       TO WDR550-2224-IDDC                         
427200     MOVE JA                  TO WDR550-2224-FLNYLARM                     
427300     MOVE ZERO                TO WDR550-2224-IDDISTR                      
427400                                 WDR550-2224-IDKUNDNR                     
427500     MOVE '0000000   '        TO WDR550-2224-IDKUNDRF                     
427600     MOVE 1                   TO WDR550-2224-IDLOPNR                      
427700     MOVE DAGENS-DATUM        TO WDR550-2224-TIREGDAT                     
427800     MOVE SPACE               TO WDR550-2224-IDTRANS                      
427900                                 WDR550-2224-KDMFSFOR                     
428000     MOVE ZERO                TO WDR550-2224-IDKR                         
428100     MOVE SPACE               TO WDR550-2224-IDLEVNR                      
428200                                                                          
428300     PERFORM IMS-ISRT-R550                                                
428400     .                                                                    
428500     EJECT                                                                
428600*** IMS SEKTIONER                                                         
428700     SKIP3                                                                
428800 IMS-GET-BENA11 SECTION.                                                  
428900     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-TILLK-X ')'                   
429000             DELIMITED BY SIZE INTO SSA1                                  
429100     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
429200              DELIMITED BY SIZE INTO SSA2                                 
429300     MOVE '  GE' TO GODK-STATUSKODER                                      
429400     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
429500     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
429600     PERFORM IMS-STATUS-KONTROLL                                          
429700     .                                                                    
429800     EJECT                                                                
429900 IMS-GET-ARTC01 SECTION.                                                  
430000     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
430100            DELIMITED BY SIZE INTO SSA1                                   
430200     MOVE '  GE' TO GODK-STATUSKODER                                      
430300     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA SSA1                     
430400     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
430500     PERFORM IMS-STATUS-KONTROLL                                          
430600     .                                                                    
430700     SKIP3                                                                
430800 IMS-GET-ARTC11 SECTION.                                                  
430900     MOVE  'WLARTC11 ' TO  SSA1                                           
431000     MOVE '  GE' TO GODK-STATUSKODER                                      
431100     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA SSA1                    
431200     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
431300     PERFORM IMS-STATUS-KONTROLL                                          
431400     .                                                                    
431500     EJECT                                                                
431600 IMS-GET-ARTC23 SECTION.                                                  
431700     MOVE  'WLARTC11 ' TO  SSA1                                           
431800     MOVE  'WLARTC23 ' TO  SSA2                                           
431900     MOVE '  GE' TO GODK-STATUSKODER                                      
432000     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA SSA1 SSA2                
432100     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
432200     PERFORM IMS-STATUS-KONTROLL                                          
432300     .                                                                    
432400     SKIP2                                                                
432500 IMS-REPL-ARTC SECTION.                                                   
432600     MOVE '  ' TO GODK-STATUSKODER                                        
432700     CALL CBLTDLI USING REPL    ARTC-PCB DLI-IO-AREA                      
432800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
432900     PERFORM IMS-STATUS-KONTROLL                                          
433000     .                                                                    
433100     EJECT                                                                
433200 IMS-GET-ERSB01 SECTION.                                                  
433300     STRING 'WLERSB01(WDD7A1KY >' W-IDARTNR-X                             
433400                                  W-IDARTNR-ERS-LOW-X                     
433500                                  W-IDKORTNR-LOW-X                        
433600                    '&WDD7A1KY <' W-IDARTNR-X                             
433700                                  W-IDARTNR-ERS-HIGH-X                    
433800                                  W-IDKORTNR-HIGH-X                       
433900                            ')'                                           
434000            DELIMITED BY SIZE INTO SSA1                                   
434100     MOVE '  GE' TO GODK-STATUSKODER                                      
434200     CALL CBLTDLI USING GU ERSB-PCB DLI-IO-AREA SSA1                      
434300     MOVE ERSB-STATUS-CODE TO STATUS-WS                                   
434400     PERFORM IMS-STATUS-KONTROLL                                          
434500     .                                                                    
434600     EJECT                                                                
434700 IMS-GET-ERSA01 SECTION.                                                  
434800     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
434900            DELIMITED BY SIZE INTO SSA1                                   
435000     MOVE '  GE' TO GODK-STATUSKODER                                      
435100     CALL CBLTDLI USING GHU ERSA-PCB DLI-IO-AREA SSA1                     
435200     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
435300     PERFORM IMS-STATUS-KONTROLL                                          
435400     .                                                                    
435500     SKIP3                                                                
435600 IMS-GET-ERSA11 SECTION.                                                  
435700     MOVE 'WLERSA11 ' TO SSA1                                             
435800     MOVE '  GE' TO GODK-STATUSKODER                                      
435900     CALL CBLTDLI USING GHNP ERSA-PCB DLI-IO-AREA SSA1                    
436000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
436100     PERFORM IMS-STATUS-KONTROLL                                          
436200     .                                                                    
436300     SKIP3                                                                
436400 IMS-GNP-ERSA11 SECTION.                                                  
436500     STRING 'WLERSA11(IDKORTNR>=' W-IDKORTNR-X ')'                        
436600            DELIMITED BY SIZE INTO SSA1                                   
436700     MOVE '  GE' TO GODK-STATUSKODER                                      
436800     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
436900     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
437000     PERFORM IMS-STATUS-KONTROLL                                          
437100     .                                                                    
437200     SKIP3                                                                
437300 IMS-GET-ERSA13 SECTION.                                                  
437400     MOVE 'WLERSA13 ' TO SSA1                                             
437500     MOVE '  GE' TO GODK-STATUSKODER                                      
437600     CALL CBLTDLI USING GHNP ERSA-PCB DLI-IO-AREA SSA1                    
437700     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
437800     PERFORM IMS-STATUS-KONTROLL                                          
437900     .                                                                    
438000     EJECT                                                                
438100 IMS-ISRT-ERSA01 SECTION.                                                 
438200     MOVE 'WLERSA01' TO SSA1                                              
438300     MOVE '  ' TO GODK-STATUSKODER                                        
438400     CALL CBLTDLI USING ISRT ERSA-PCB DLI-IO-AREA SSA1                    
438500     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
438600     PERFORM IMS-STATUS-KONTROLL                                          
438700     .                                                                    
438800     SKIP3                                                                
438900 IMS-ISRT-ERSA11 SECTION.                                                 
439000     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
439100            DELIMITED BY SIZE INTO SSA1                                   
439200     MOVE  'WLERSA11'  TO SSA2                                            
439300     MOVE '  ' TO GODK-STATUSKODER                                        
439400     CALL CBLTDLI USING ISRT ERSA-PCB DLI-IO-AREA SSA1 SSA2               
439500     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
439600     PERFORM IMS-STATUS-KONTROLL                                          
439700     .                                                                    
439800     SKIP3                                                                
439900 IMS-ISRT-ERSA13 SECTION.                                                 
440000     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
440100            DELIMITED BY SIZE INTO SSA1                                   
440200     MOVE  'WLERSA13'  TO SSA2                                            
440300     MOVE '  ' TO GODK-STATUSKODER                                        
440400     CALL CBLTDLI USING ISRT ERSA-PCB DLI-IO-AREA SSA1 SSA2               
440500     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
440600     PERFORM IMS-STATUS-KONTROLL                                          
440700     .                                                                    
440800     EJECT                                                                
440900 IMS-DLET-ERSA SECTION.                                                   
441000     MOVE '  ' TO GODK-STATUSKODER                                        
441100     CALL CBLTDLI USING DLET    ERSA-PCB DLI-IO-AREA                      
441200     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
441300     PERFORM IMS-STATUS-KONTROLL                                          
441400     .                                                                    
441500     SKIP3                                                                
441600 IMS-REPL-ERSA SECTION.                                                   
441700     MOVE '  ' TO GODK-STATUSKODER                                        
441800     CALL CBLTDLI USING REPL ERSA-PCB DLI-IO-AREA                         
441900     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
442000     PERFORM IMS-STATUS-KONTROLL                                          
442100     .                                                                    
442200     SKIP3                                                                
442300 IMS-GET-INLB01 SECTION.                                                  
442400     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
442500            DELIMITED BY SIZE INTO SSA1                                   
442600     MOVE '  GE' TO GODK-STATUSKODER                                      
442700     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1                      
442800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
442900     PERFORM IMS-STATUS-KONTROLL                                          
443000     .                                                                    
443100     SKIP3                                                                
443200 IMS-GET-INLB11 SECTION.                                                  
443300     MOVE 'WLINLB11*F ' TO SSA1                                           
443400     MOVE '  GE' TO GODK-STATUSKODER                                      
443500     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
443600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
443700     PERFORM IMS-STATUS-KONTROLL                                          
443800     .                                                                    
443900     EJECT                                                                
444000 IMS-GET-INLB11-NAESTA SECTION.                                           
444100     MOVE 'WLINLB11 ' TO SSA1                                             
444200     MOVE '  GE' TO GODK-STATUSKODER                                      
444300     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1                     
444400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
444500     PERFORM IMS-STATUS-KONTROLL                                          
444600     .                                                                    
444700     SKIP3                                                                
444800 IMS-GET-XXAN01 SECTION.                                                  
444900     STRING 'WLXXAN01(WDGXKEY  =' W-1111-KEY-X ')'                        
445000             DELIMITED BY SIZE INTO SSA1                                  
445100     MOVE 'GE   ' TO GODK-STATUSKODER                                     
445200     CALL CBLTDLI USING GHU XXAN-PCB DLI-IO-AREA-2 SSA1                   
445300     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
445400     PERFORM IMS-STATUS-KONTROLL                                          
445500     .                                                                    
445600     SKIP3                                                                
445700 IMS-GET-XXAN11 SECTION.                                                  
445800     STRING 'WLXXAN11(WDGXKEY  =' W-1112-KEY-X ')'                        
445900             DELIMITED BY SIZE INTO SSA1                                  
446000     MOVE 'GE   ' TO GODK-STATUSKODER                                     
446100     CALL CBLTDLI USING GNP XXAN-PCB DLI-IO-AREA-2 SSA1                   
446200     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
446300     PERFORM IMS-STATUS-KONTROLL                                          
446400     .                                                                    
446500     EJECT                                                                
446600 IMS-GU-XXAN11 SECTION.                                                   
446700     STRING 'WLXXAN01(WDGXKEY  =' W-1111-KEY-X ')'                        
446800             DELIMITED BY SIZE INTO SSA1                                  
446900     STRING 'WLXXAN11(WDGXKEY  =' W-1112-KEY-X ')'                        
447000             DELIMITED BY SIZE INTO SSA2                                  
447100     MOVE 'GE   ' TO GODK-STATUSKODER                                     
447200     CALL CBLTDLI USING GHU XXAN-PCB DLI-IO-AREA-2 SSA1 SSA2              
447300     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
447400     PERFORM IMS-STATUS-KONTROLL                                          
447500     .                                                                    
447600     SKIP3                                                                
447700 IMS-GNP-XXAN21 SECTION.                                                  
447800     STRING 'WLXXAN21(WDGXKEY  =' W-1114-KEY-X ')'                        
447900             DELIMITED BY SIZE INTO SSA1                                  
448000     MOVE 'GE   ' TO GODK-STATUSKODER                                     
448100     CALL CBLTDLI USING GNP XXAN-PCB DLI-IO-AREA-2 SSA1                   
448200     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
448300     PERFORM IMS-STATUS-KONTROLL                                          
448400     .                                                                    
448500     SKIP3                                                                
448600 IMS-GHU-XXAN21 SECTION.                                                  
448700     STRING 'WLXXAN01(WDGXKEY  =' W-1111-KEY-X ')'                        
448800             DELIMITED BY SIZE INTO SSA1                                  
448900     STRING 'WLXXAN11(WDGXKEY  =' W-1112-KEY-X ')'                        
449000             DELIMITED BY SIZE INTO SSA2                                  
449100     STRING 'WLXXAN21(WDGXKEY  =' W-1114-KEY-X ')'                        
449200             DELIMITED BY SIZE INTO SSA3                                  
449300     MOVE 'GE   ' TO GODK-STATUSKODER                                     
449400     CALL CBLTDLI USING GHU XXAN-PCB DLI-IO-AREA-2                        
449500                SSA1 SSA2 SSA3                                            
449600     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
449700     PERFORM IMS-STATUS-KONTROLL                                          
449800     .                                                                    
449900     SKIP3                                                                
450000 IMS-GET-XXAN21 SECTION.                                                  
450100     MOVE 'WLXXAN21 ' TO SSA1                                             
450200     MOVE 'GE   ' TO GODK-STATUSKODER                                     
450300     CALL CBLTDLI USING GNP XXAN-PCB DLI-IO-AREA-2 SSA1                   
450400     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
450500     PERFORM IMS-STATUS-KONTROLL                                          
450600     .                                                                    
450700     SKIP3                                                                
450800 IMS-DLET-XXAN SECTION.                                                   
450900     MOVE '  ' TO GODK-STATUSKODER                                        
451000     CALL CBLTDLI USING DLET XXAN-PCB DLI-IO-AREA-2                       
451100     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
451200     PERFORM IMS-STATUS-KONTROLL                                          
451300     .                                                                    
451400     EJECT                                                                
451500 IMS-REPL-XXAN SECTION.                                                   
451600     MOVE '  ' TO GODK-STATUSKODER                                        
451700     CALL CBLTDLI USING REPL XXAN-PCB DLI-IO-AREA-2                       
451800     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
451900     PERFORM IMS-STATUS-KONTROLL                                          
452000     .                                                                    
452100     SKIP3                                                                
452200 IMS-ISRT-XXAN01 SECTION.                                                 
452300     MOVE 'WLXXAN01 ' TO SSA1                                             
452400     MOVE '  ' TO GODK-STATUSKODER                                        
452500     CALL CBLTDLI USING ISRT XXAN-PCB DLI-IO-AREA-2 SSA1                  
452600     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
452700     PERFORM IMS-STATUS-KONTROLL                                          
452800     .                                                                    
452900     SKIP3                                                                
453000 IMS-ISRT-XXAN11 SECTION.                                                 
453100     STRING 'WLXXAN01(WDGXKEY  =' W-1111-KEY-X ')'                        
453200              DELIMITED BY SIZE INTO SSA1                                 
453300     MOVE 'WLXXAN11 ' TO SSA2                                             
453400     MOVE '  ' TO GODK-STATUSKODER                                        
453500     CALL CBLTDLI USING ISRT XXAN-PCB DLI-IO-AREA-2 SSA1 SSA2             
453600     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
453700     PERFORM IMS-STATUS-KONTROLL                                          
453800     .                                                                    
453900     EJECT                                                                
454000 IMS-ISRT-XXAN21 SECTION.                                                 
454100     STRING 'WLXXAN01(WDGXKEY  =' W-1111-KEY-X ')'                        
454200              DELIMITED BY SIZE INTO SSA1                                 
454300     STRING 'WLXXAN11(WDGXKEY  =' W-1112-KEY-X ')'                        
454400              DELIMITED BY SIZE INTO SSA2                                 
454500     MOVE 'WLXXAN21 ' TO SSA3                                             
454600     MOVE '  ' TO GODK-STATUSKODER                                        
454700     CALL CBLTDLI USING ISRT XXAN-PCB DLI-IO-AREA-2 SSA1                  
454800                               SSA2 SSA3                                  
454900     MOVE XXAN-STATUS-CODE TO STATUS-WS                                   
455000     PERFORM IMS-STATUS-KONTROLL                                          
455100     .                                                                    
455200     EJECT                                                                
455300 IMS-ISRT-2304 SECTION.                                                   
455400     STRING 'WDG301  (WDG3KEY  =' W-2303-KEY-X ')'                        
455500              DELIMITED BY SIZE INTO SSA1                                 
455600     MOVE 'WDGX2304*L' TO SSA2                                            
455700     MOVE '  ' TO GODK-STATUSKODER                                        
455800     CALL CBLTDLI USING ISRT 2303-PCB DLI-IO-AREA SSA1 SSA2               
455900     MOVE 2303-STATUS-CODE TO STATUS-WS                                   
456000     PERFORM IMS-STATUS-KONTROLL                                          
456100     .                                                                    
456200     SKIP3                                                                
456300 IMS-ISRT-XXBJ SECTION.                                                   
456400     STRING 'WLXXBJ01(WDG3KEY  =' W-2203-KEY-X ')'                        
456500              DELIMITED BY SIZE INTO SSA1                                 
456600     MOVE 'WLXXBJ11*L' TO SSA2                                            
456700     MOVE '  ' TO GODK-STATUSKODER                                        
456800     CALL CBLTDLI USING ISRT XXBJ-PCB DLI-IO-AREA SSA1 SSA2               
456900     MOVE XXBJ-STATUS-CODE TO STATUS-WS                                   
457000     PERFORM IMS-STATUS-KONTROLL                                          
457100     .                                                                    
457200     EJECT                                                                
457300 IMS-ISRT-XXID SECTION.                                                   
457400     STRING 'WLXXID01(WDG3KEY  =' W-9101-KEY-X ')'                        
457500              DELIMITED BY SIZE INTO SSA1                                 
457600     MOVE 'WLXXID11*L' TO SSA2                                            
457700     MOVE '  ' TO GODK-STATUSKODER                                        
457800     CALL CBLTDLI USING ISRT XXID-PCB DLI-IO-AREA SSA1 SSA2               
457900     MOVE XXID-STATUS-CODE TO STATUS-WS                                   
458000     PERFORM IMS-STATUS-KONTROLL                                          
458100     .                                                                    
458200     SKIP3                                                                
458300 IMS-ISRT-ZZAC SECTION.                                                   
458400     MOVE 'WLZZAC01 ' TO SSA1                                             
458500     MOVE '  II' TO GODK-STATUSKODER                                      
458600     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
458700     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
458800     PERFORM IMS-STATUS-KONTROLL                                          
458900     .                                                                    
459000     EJECT                                                                
459100 IMS-GET-ARTG01 SECTION.                                                  
459200     STRING 'WLARTG01(IDARTNR  =' W-IDARTNR-X ')'                         
459300             DELIMITED BY SIZE INTO SSA1                                  
459400     MOVE '  GE' TO GODK-STATUSKODER                                      
459500     CALL CBLTDLI USING GHU ARTG-PCB DLI-IO-AREA-3 SSA1                   
459600     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
459700     PERFORM IMS-STATUS-KONTROLL                                          
459800     .                                                                    
459900     SKIP3                                                                
460000 IMS-REPL-ARTG SECTION.                                                   
460100     MOVE '  ' TO GODK-STATUSKODER                                        
460200     CALL CBLTDLI USING REPL ARTG-PCB DLI-IO-AREA-3                       
460300     MOVE ARTG-STATUS-CODE TO STATUS-WS                                   
460400     PERFORM IMS-STATUS-KONTROLL                                          
460500     .                                                                    
460600     EJECT                                                                
460700 IMS-GU-XXAV01      SECTION.                                              
460800     STRING 'WLXXAV01(WDGXKEY  =' W-1141-KEY-X ')'                        
460900              DELIMITED BY SIZE INTO SSA1                                 
461000     MOVE '  '   TO GODK-STATUSKODER                                      
461100     CALL CBLTDLI USING GU    XXAV-PCB DLI-IO-AREA SSA1                   
461200     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
461300     PERFORM IMS-STATUS-KONTROLL                                          
461400     .                                                                    
461500     SKIP2                                                                
461600 IMS-GNP-XXAV11      SECTION.                                             
461700     STRING 'WLXXAV11(KDSEGKEY =' W-1142-KEY-X ')'                        
461800              DELIMITED BY SIZE INTO SSA1                                 
461900     MOVE '  GE'   TO GODK-STATUSKODER                                    
462000     CALL CBLTDLI USING GNP XXAV-PCB DLI-IO-AREA SSA1                     
462100     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
462200     PERFORM IMS-STATUS-KONTROLL                                          
462300     .                                                                    
462400     SKIP2                                                                
462500 IMS-ISRT-XXAV SECTION.                                                   
462600     STRING 'WLXXAV01(WDGXKEY  =' W-1141-KEY-X ')'                        
462700              DELIMITED BY SIZE INTO SSA1                                 
462800     MOVE 'WLXXAV11 ' TO SSA2                                             
462900     MOVE '  '   TO GODK-STATUSKODER                                      
463000     CALL CBLTDLI USING ISRT XXAV-PCB DLI-IO-AREA SSA1 SSA2               
463100     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
463200     PERFORM IMS-STATUS-KONTROLL                                          
463300     .                                                                    
463400     EJECT                                                                
463500 IMS-ISRT-XXAW SECTION.                                                   
463600     STRING 'WLXXAW01(WDGXKEY  =' W-1115-KEY-X ')'                        
463700              DELIMITED BY SIZE INTO SSA1                                 
463800     MOVE 'WLXXAW11 ' TO SSA2                                             
463900     MOVE '  II' TO GODK-STATUSKODER                                      
464000     CALL CBLTDLI USING ISRT XXAW-PCB DLI-IO-AREA SSA1 SSA2               
464100     MOVE XXAW-STATUS-CODE TO STATUS-WS                                   
464200     PERFORM IMS-STATUS-KONTROLL                                          
464300     .                                                                    
464400     SKIP2                                                                
464500 IMS-GET-XXAW11-GHU  SECTION.                                             
464600     STRING 'WLXXAW01(WDGXKEY  =' W-1115-KEY-X ')'                        
464700              DELIMITED BY SIZE INTO SSA1                                 
464800     STRING 'WLXXAW11(WDGXKEY  =' W-1116-IDARTNR-X ')'                    
464900              DELIMITED BY SIZE INTO SSA2                                 
465000     MOVE '  GE' TO GODK-STATUSKODER                                      
465100     CALL CBLTDLI USING GHU XXAW-PCB DLI-IO-AREA SSA1 SSA2                
465200     MOVE XXAW-STATUS-CODE TO STATUS-WS                                   
465300     PERFORM IMS-STATUS-KONTROLL                                          
465400     .                                                                    
465500     SKIP2                                                                
465600 IMS-REPL-XXAW SECTION.                                                   
465700     MOVE '  ' TO GODK-STATUSKODER                                        
465800     CALL CBLTDLI USING REPL XXAW-PCB DLI-IO-AREA                         
465900     MOVE XXAW-STATUS-CODE TO STATUS-WS                                   
466000     PERFORM IMS-STATUS-KONTROLL                                          
466100     .                                                                    
466200     SKIP2                                                                
466300 IMS-GET-SATB-CSEQ SECTION.                                               
466400     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
466500             DELIMITED BY SIZE INTO SSA1                                  
466600     MOVE 'WLSATB01 ' TO SSA2                                             
466700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
466800     CALL CBLTDLI USING GN SATE-PCB DLI-IO-AREA-5                         
466900                SSA1 SSA2                                                 
467000     MOVE SATE-STATUS-CODE TO STATUS-WS                                   
467100     PERFORM IMS-STATUS-KONTROLL                                          
467200     .                                                                    
467300     SKIP3                                                                
467400 IMS-GET-SATB01 SECTION.                                                  
467500     STRING 'WLSATB01(IDARTNR  =' W-IDARTNR-X ')'                         
467600            DELIMITED BY SIZE INTO SSA1                                   
467700     MOVE '  ' TO GODK-STATUSKODER                                        
467800     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA-5 SSA1                    
467900     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
468000     PERFORM IMS-STATUS-KONTROLL                                          
468100     .                                                                    
468200     SKIP3                                                                
468300 IMS-GET-SATB11 SECTION.                                                  
468400     MOVE 'WLSATB11 ' TO SSA1                                             
468500     MOVE '  GE' TO GODK-STATUSKODER                                      
468600     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA-5 SSA1                  
468700     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
468800     PERFORM IMS-STATUS-KONTROLL                                          
468900     .                                                                    
469000 IMS-GET-SATB11-FIRST SECTION.                                            
469100     MOVE 'WLSATB11*F' TO SSA1                                            
469200     MOVE '  GE' TO GODK-STATUSKODER                                      
469300     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA-5 SSA1                  
469400     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
469500     PERFORM IMS-STATUS-KONTROLL                                          
469600     .                                                                    
469700 IMS-GHNP-SATB11 SECTION.                                                 
469800     STRING 'WLSATB11(WDJ111KY =' W-WDJ111KY-X ')'                        
469900            DELIMITED BY SIZE INTO SSA1                                   
470000     MOVE '  GE' TO GODK-STATUSKODER                                      
470100     CALL CBLTDLI USING GHNP SATB-PCB DLI-IO-AREA-5 SSA1                  
470200     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
470300     PERFORM IMS-STATUS-KONTROLL                                          
470400     .                                                                    
470500 IMS-REPL-SATB SECTION.                                                   
470600     MOVE '  ' TO GODK-STATUSKODER                                        
470700     CALL CBLTDLI USING REPL SATB-PCB DLI-IO-AREA-5                       
470800     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
470900     PERFORM IMS-STATUS-KONTROLL                                          
471000     .                                                                    
471100     SKIP3                                                                
471200                                                                          
471300 IMS-ISRT-2228 SECTION.                                                   
471400     STRING 'WLXXBW01(WDGXKEY  =' W-2227KEY-X ')'                         
471500            DELIMITED BY SIZE INTO SSA1                                   
471600     MOVE   'WLXXBW11 ' TO SSA2                                           
471700     MOVE '  II' TO GODK-STATUSKODER                                      
471800     CALL CBLTDLI USING ISRT XXBW-PCB DLI-IO-AREA-6 SSA1 SSA2             
471900     MOVE XXBW-STATUS-CODE TO STATUS-WS                                   
472000     PERFORM IMS-STATUS-KONTROLL                                          
472100     .                                                                    
472200     EJECT                                                                
472300 IMS-GET-XXCW11 SECTION.                                                  
472400     STRING 'WLXXCW01(WDG3KEY  =' W-1157-KEY-X ')'                        
472500              DELIMITED BY SIZE INTO SSA1                                 
472600     STRING 'WLXXCW11(WDGXKEY  =' W-1158-IDARTNR-X ')'                    
472700              DELIMITED BY SIZE INTO SSA2                                 
472800     MOVE '  GE' TO GODK-STATUSKODER                                      
472900     CALL CBLTDLI USING GHU XXCW-PCB DLI-IO-AREA SSA1 SSA2                
473000     MOVE XXCW-STATUS-CODE TO STATUS-WS                                   
473100     PERFORM IMS-STATUS-KONTROLL                                          
473200     .                                                                    
473300     SKIP2                                                                
473400 IMS-ISRT-XXCW SECTION.                                                   
473500     STRING 'WLXXCW01(WDG3KEY  =' W-1157-KEY-X ')'                        
473600              DELIMITED BY SIZE INTO SSA1                                 
473700     MOVE 'WLXXCW11 ' TO SSA2                                             
473800     MOVE '  ' TO GODK-STATUSKODER                                        
473900     CALL CBLTDLI USING ISRT XXCW-PCB DLI-IO-AREA SSA1 SSA2               
474000     MOVE XXCW-STATUS-CODE TO STATUS-WS                                   
474100     PERFORM IMS-STATUS-KONTROLL                                          
474200     .                                                                    
474300     SKIP2                                                                
474400 IMS-REPL-XXCW SECTION.                                                   
474500     MOVE '  ' TO GODK-STATUSKODER                                        
474600     CALL CBLTDLI USING REPL XXCW-PCB DLI-IO-AREA                         
474700     MOVE XXCW-STATUS-CODE TO STATUS-WS                                   
474800     PERFORM IMS-STATUS-KONTROLL                                          
474900     .                                                                    
475000     SKIP2                                                                
475100 IMS-DLET-XXCW SECTION.                                                   
475200     MOVE '  ' TO GODK-STATUSKODER                                        
475300     CALL CBLTDLI USING DLET XXCW-PCB DLI-IO-AREA                         
475400     MOVE XXCW-STATUS-CODE TO STATUS-WS                                   
475500     PERFORM IMS-STATUS-KONTROLL                                          
475600     .                                                                    
475700     EJECT                                                                
475800 IMS-ISRT-FILC-TRANS  SECTION.                                            
475900     MOVE 'WLFILC01 ' TO SSA1                                             
476000     MOVE '  II' TO GODK-STATUSKODER                                      
476100     CALL CBLTDLI USING ISRT FILC-PCB DLI-IO-AREA-7  SSA1                 
476200     MOVE FILC-STATUS-CODE TO STATUS-WS                                   
476300     PERFORM IMS-STATUS-KONTROLL                                          
476400     .                                                                    
476500     EJECT                                                                
476600 IMS-GHU-XXAV11-GE  SECTION.                                              
476700     STRING 'WLXXAV01(WDGXKEY  =' W-1141-KEY-X ')'                        
476800             DELIMITED BY SIZE INTO SSA1                                  
476900     STRING 'WLXXAV11(KDSEGKEY =' W-1142-KEY-X                            
477000                    '&IDARTNR  =' W-IDARTNR-X ')'                         
477100             DELIMITED BY SIZE INTO SSA2                                  
477200     MOVE '  GE' TO GODK-STATUSKODER                                      
477300     CALL CBLTDLI USING GHU XXAV-PCB DLI-IO-AREA SSA1 SSA2                
477400     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
477500     PERFORM IMS-STATUS-KONTROLL                                          
477600     .                                                                    
477700     SKIP3                                                                
477800 IMS-DLET-XXAV11 SECTION.                                                 
477900     MOVE '  '   TO GODK-STATUSKODER                                      
478000     CALL CBLTDLI USING DLET XXAV-PCB DLI-IO-AREA                         
478100     MOVE XXAV-STATUS-CODE TO STATUS-WS                                   
478200     PERFORM IMS-STATUS-KONTROLL                                          
478300     .                                                                    
478400     EJECT                                                                
478500 IMS-GU-WDB601    SECTION.                                                
478600     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
478700                    DELIMITED BY SIZE INTO SSA1                           
478800     MOVE '  GE' TO GODK-STATUSKODER                                      
478900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-8 SSA1                    
479000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
479100     PERFORM IMS-STATUS-KONTROLL                                          
479200     .                                                                    
479300     EJECT                                                                
479400 IMS-GU-WDK701 SECTION.                                                   
479500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
479600          DELIMITED BY SIZE INTO SSA1                                     
479700     MOVE '  GE' TO GODK-STATUSKODER                                      
479800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK701 SSA1               
479900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
480000     PERFORM IMS-STATUS-KONTROLL                                          
480100     .                                                                    
480200     EJECT                                                                
480300 IMS-GN-WDK711 SECTION.                                                   
480400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
480500          DELIMITED BY SIZE INTO SSA1                                     
480600     MOVE   'WDK711   ' TO SSA2                                           
480700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
480800     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-WDK711 SSA1  SSA2         
480900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
481000     PERFORM IMS-STATUS-KONTROLL                                          
481100     .                                                                    
481200     EJECT                                                                
481300 IMS-GNP-WDK722 SECTION.                                                  
481400     MOVE 'WDK722 '     TO SSA1                                           
481500     MOVE '  GE' TO GODK-STATUSKODER                                      
481600     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK722 SSA1              
481700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
481800     PERFORM IMS-STATUS-KONTROLL                                          
481900     .                                                                    
482000     EJECT                                                                
482100 IMS-GU-R220 SECTION.                                                     
482200     STRING 'WDR201  (WDGXKEY  =' W-WDGX2231-X ')'                        
482300          DELIMITED BY SIZE INTO SSA1                                     
482400     STRING 'WDR220  (WDGXKEY  =' W-WDGX2232-X ')'                        
482500          DELIMITED BY SIZE INTO SSA2                                     
482600     MOVE '  GE' TO GODK-STATUSKODER                                      
482700     CALL CBLTDLI USING GU WDR2-PCB DLI-IO-AREA-2232 SSA1 SSA2            
482800     MOVE WDR2-STATUS-CODE TO STATUS-WS                                   
482900     PERFORM IMS-STATUS-KONTROLL                                          
483000     .                                                                    
483100     EJECT                                                                
483200 IMS-GHU-R501 SECTION.                                                    
483300     STRING 'WDR501  (WDGXKEY  =' W-WDGX2223-X ')'                        
483400          DELIMITED BY SIZE INTO SSA1                                     
483500     MOVE '  GE'           TO GODK-STATUSKODER                            
483600     CALL CBLTDLI USING GHU WDR5-PCB DLI-IO-AREA-2223 SSA1                
483700     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
483800     PERFORM IMS-STATUS-KONTROLL                                          
483900     .                                                                    
484000     EJECT                                                                
484100 IMS-ISRT-R501 SECTION.                                                   
484200     MOVE 'WDR501   '      TO SSA1                                        
484300     MOVE '  II'           TO GODK-STATUSKODER                            
484400     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-AREA-2223 SSA1               
484500     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
484600     PERFORM IMS-STATUS-KONTROLL                                          
484700     .                                                                    
484800     EJECT                                                                
484900 IMS-ISRT-R550 SECTION.                                                   
485000     MOVE 'WDR550   '      TO SSA1                                        
485100     MOVE '  II'           TO GODK-STATUSKODER                            
485200     CALL CBLTDLI USING ISRT WDR5-PCB DLI-IO-AREA-2224 SSA1               
485300     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
485400     PERFORM IMS-STATUS-KONTROLL                                          
485500     .                                                                    
485600     EJECT                                                                
485700                                                                          
485800 IMS-STATUS-KONTROLL SECTION.                                             
485900     SET STATUS-IX TO 1                                                   
486000     SEARCH GODK-STATUS AT END CALL FELLOG                                
486100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
486200     END-SEARCH.                                                          
486300     EJECT                                                                
486400* DB2 SEKTIONER                                                           
486500     SKIP3                                                                
486600                                                                          
486700 DB2-DCL-OPN-TP1ARTK-CRS  SECTION.                                        
486800     MOVE 'DB2-DCL-OPN-TP1ARTK   ' TO  WS-DB2-SEKTION                     
486900                                                                          
487000     MOVE 000100  TO GOOD-SQLCODECODES                                    
487100                                                                          
487200     EXEC SQL                                                             
487300         DECLARE TP1ARTK-CRS CURSOR FOR                                   
487400           SELECT  A.IDKAMP                                               
487500                  ,A.IDARTNR                                              
487600                  ,B.TISTADAT_KAMP                                        
487700                  ,B.TISTODAT_KAMP                                        
487800                  ,B.KDKAMP                                               
487900                                                                          
488000           FROM    TP1ARTK A                                              
488100                  ,TP1KAMP B                                              
488200                                                                          
488300           WHERE   A.IDARTNR = :W-IDARTNR                                 
488400               AND A.IDKAMP  =  B.IDKAMP                                  
488500                                                                          
488600           ORDER BY A.IDARTNR                                             
488700     END-EXEC                                                             
488800                                                                          
488900     MOVE 000100  TO GOOD-SQLCODECODES                                    
489000     EXEC SQL OPEN TP1ARTK-CRS END-EXEC                                   
489100     .                                                                    
489200     SKIP3                                                                
489300 DB2-FETCH-TP1ARTK-CRS  SECTION.                                          
489400     MOVE 'DB2-FETCH-TP1ARTK   ' TO  WS-DB2-SEKTION                       
489500     SKIP2                                                                
489600     MOVE 000100  TO GOOD-SQLCODECODES                                    
489700     EXEC SQL                                                             
489800         FETCH TP1ARTK-CRS INTO                                           
489900                    :TP1KAMP-IDKAMP                                       
490000                   ,:TP1ARTK-IDARTNR                                      
490100                   ,:TP1KAMP-TISTADAT-KAMP                                
490200                   ,:TP1KAMP-TISTODAT-KAMP                                
490300                   ,:TP1KAMP-KDKAMP                                       
490400     END-EXEC                                                             
490500                                                                          
490510                                                                          
490600     MOVE SQLCODE TO SQLCODE-WS                                           
490700     PERFORM DB2-STATUS-CHECK                                             
490800     .                                                                    
490900     SKIP3                                                                
491000 DB2-CLOSE-TP1ARTK-CRS  SECTION.                                          
491100     MOVE 'DB2-CLOSE-TP1ARTK   ' TO  WS-DB2-SEKTION                       
491200                                                                          
491300     EXEC SQL CLOSE TP1ARTK-CRS END-EXEC                                  
491400     .                                                                    
491500     EJECT                                                                
491600 DB2-STATUS-CHECK  SECTION.                                               
491700                                                                          
491800     SET SQLCODE-IX TO 1                                                  
491900     SEARCH GOOD-SQLCODE                                                  
492000       AT END                                                             
492100*         STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
492200*         DELIMITED BY SIZE INTO ERROR-TEXT                               
492300          CALL FELLOG                                                     
492400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
492500     END-SEARCH                                                           
492600     .                                                                    
492700     EJECT                                                                
492800*    -COPY WY2000P3                                                       
492900     EJECT                                                                
493000*    -COPY WY2000P1                                                       
493100     EJECT                                                                
493200*    -COPY WY2000P2                                                       
