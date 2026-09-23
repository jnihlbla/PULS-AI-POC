000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W2711600.                                                
000400 AUTHOR.         NIHLBLAD JOHAN.                                          
000500 DATE-WRITTEN.   06/11/15.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAM FÖR ATT TA HAND OM AUTOMATRETURER                        
001100*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700*    CHANGE LOG:                                                          
001800*                                                                         
001900*      28/01/14 - DATTA ARUP      - ENABLE TO START AREA 88               
002000*                                   ORDERS UPTO 5 DAYS.                   
002100*                                   SCR 10200572                          
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- UTFIL TILL W2711100 PGM MED AUTOMATRETURER                 
003100     SELECT W27117                     ASSIGN TO W27116D1.                
003200     SELECT W27116                     ASSIGN TO W27116D2.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W27117                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  POST -COPY W27111 -PRE  UT-  -L.                                     
004300     SKIP3                                                                
004400 FD  W27116                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  POST -COPY W2712D -PRE  UT98-  -L.                                   
004900     EJECT                                                                
005000 WORKING-STORAGE SECTION.                                                 
005100*    -COPY WY2000W1                                                       
005200                                                                          
005300 77  IDPGM                       PIC X(8)    VALUE 'W2711600'.            
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600 77  WS-IDDC-IX                  PIC 9(3)    VALUE ZERO.                  
005700 77  MAX-IDDC-IX                 PIC 9(3)    VALUE ZERO.                  
005800 77  WS-OMRAKTAL-21              PIC 9(1)V9(2) VALUE 0.10.                
005900 77  WS-OMRAKTAL-22              PIC 9(1)V9(2) VALUE 0.04.                
006000 77  WS-OMRAKTAL-3A              PIC 9(1)V9(2) VALUE 0.04.                
006100 77  WS-OMRAKTAL-24              PIC 9(1)V9(2) VALUE 0.02.                
006200 77  WS-OMRAKTAL-25              PIC 9(1)V9(2) VALUE 0.04.                
006300 77  WS-OMRAKTAL-26              PIC 9(1)V9(2) VALUE 0.04.                
006400 77  WS-PROGNOS               PIC S9(6)V9(1) VALUE ZERO COMP-3.           
006500 01  CHKP-VAR.                                                            
006600     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
006700     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
006800     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
006900     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
007000     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
007100     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
007200     EJECT                                                                
007300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007400 01  FILLER REDEFINES DAGENS-DATUM.                                       
007500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007800     EJECT                                                                
007900 01  WS-DATUM-YEAR-1.                                                     
008000     03  WS-DATUM-YY-1           PIC 9(2).                                
008100     03  WS-DATUM-MM             PIC 9(2).                                
008200     03  WS-DATUM-DD             PIC 9(2).                                
008300     EJECT                                                                
008400 01  DAGENS-TIAAVVD          PIC  9(5)   VALUE ZERO.                      
008500 01  FILLER REDEFINES DAGENS-TIAAVVD.                                     
008600     03 DAGENS-TIAAVVD-AA    PIC  9(2).                                   
008700     03 DAGENS-TIAAVVD-VV    PIC  9(2).                                   
008800     03 DAGENS-TIAAVVD-D     PIC  9(1).                                   
008900                                                                          
009000                                                                          
009100 01  ARBETSAREA.                                                          
009200     03 WS-VECKOTYP                 PIC X(5)    VALUE SPACE.              
009300     03 WS-RETUR-ADLAGOMR           PIC 9(2)    VALUE ZERO.               
009400     03 WS-RETUR-ADGANG             PIC 9(2)    VALUE ZERO.               
009500     03 WS-TRANS-ADLAGOMR           PIC 9(2)    VALUE ZERO.               
009600     03 WS-TIAAVVD                  PIC 9(5)    VALUE ZERO.               
009700     03 WS-LAGERSALDO-DC            PIC 9(6)    VALUE ZERO.               
009800     03 WS-KVPB-TOT-30V-SEND-DC     PIC 9(8)    VALUE ZERO.               
009900     03 WS-DAGENS-AAVV-MINUS-26V    PIC 9(4)    VALUE ZERO.               
010000     03 WS-DAGENS-AAAAVV-MINUS-26V  PIC 9(6)    VALUE ZERO.               
010100     03 DAGENS-AAMMDD-MINUS-90DAG   PIC 9(6)    VALUE ZERO.               
010200     03 DAGENS-AAMMDD-MINUS-365DAG  PIC 9(6)    VALUE ZERO.               
010300     03 DAGENS-AAAAMMDD-MINUS-365DAG  PIC 9(8)    VALUE ZERO.             
010400     03 WS-DATUM-MINUS-18MONTH      PIC 9(6)    VALUE ZERO.               
010500     03 WS-OVERLAGER                PIC S9(7)   VALUE ZERO COMP-3.        
010600     03 WS-ANTAL-RETUR              PIC S9(6)   VALUE ZERO COMP-3.        
010700     03 WS-ANTAL-KVAR               PIC S9(6)   VALUE ZERO COMP-3.        
010800     03 WS-KVBEART                  PIC S9(7)   VALUE ZERO COMP-3.        
010900     03 WS-KVSKROT                  PIC S9(7)   VALUE ZERO COMP-3.        
011000     03 WS-KVBEART-SKROT            PIC S9(7)   VALUE ZERO COMP-3.        
011100     03 WS-VALUE-KVSKROT       PIC S9(9)V9(2)   VALUE ZERO COMP-3.        
011200     03 WS-KVLS                     PIC S9(7)   VALUE ZERO COMP-3.        
011300     03 WS-KVPB-TOT-CDC             PIC 9(6)V9(2)  VALUE ZERO.            
011400     03 WS-KVPB-TOT-XDC             PIC 9(6)V9(2)  VALUE ZERO.            
011500     03 WS-KVPB-TOT-CDC-XDC         PIC 9(6)V9(2)  VALUE ZERO.            
011600     03 WS-KVPB-TOT-1V-SEND-DC      PIC 9(6)V9(2)  VALUE ZERO.            
011700     03 WS-TIFINLV                  PIC 9(7)    VALUE ZERO.               
011800     03 WS-TIFINLV-AAAAVV           PIC 9(6)    VALUE ZERO.               
011900     03 ARTWS-TIFINLV               PIC 9(5)  VALUE ZERO.                 
012000     03 W-TILLG-SDC                 PIC S9(7)   COMP-3.                   
012100     03 W-OVERLAGER-SDC             PIC S9(7)   COMP-3.                   
012200     03 WS-ANTAL-QX                 PIC S9(7) VALUE ZERO COMP-3.          
012300     03 WS-QX-BRYTNING              PIC  9(2)   VALUE ZERO.               
012400     03 WS-TIREFEFT-6               PIC  9(6)   VALUE ZERO.               
012500     03 WS-TIREFEFT-8               PIC  9(8)   VALUE ZERO.               
012600     03 WS-VARDE-RETUR              PIC  9(9)   VALUE ZERO.               
012700     03 WS-DATUM-KNTRL              PIC S9(7)   VALUE ZERO COMP-3.        
012800     03 WS-TIINLINL                 PIC 9(6)   VALUE ZERO.                
012900                                                                          
013000     03 WS-KVANT-QX          PIC  9(5)V9(2) VALUE ZERO.                   
013100     03 WS-KVANT-QX-DELAR    REDEFINES WS-KVANT-QX.                       
013200        05 WS-KVANT-QX-HELTAL PIC 9(5).                                   
013300        05 WS-KVANT-QX-DECTAL PIC 9(2).                                   
013400     03 WS-PRIS              PIC S9(7)V9(2) VALUE ZERO COMP-3.            
013410     03 IX-L611              PIC 9(9)    VALUE ZERO.                      
013420     03 IX-K611              PIC 9(9)   VALUE ZERO.                       
013430     03 IX-UTIL              PIC 9(9)   VALUE ZERO.                       
013500                                                                          
013600                                                                          
013700 01    WS-IDDC-TABELL.                                                    
013800    03 WS-VALID-IDDC  OCCURS 300.                                         
013900       05 TAB-IDDC            PIC X(2).                                   
014000       05 TAB-KDDC            PIC X(2).                                   
014100       05 TAB-REQXBRYT        PIC S9(2).                                  
014200       05 TAB-TID-RET98       PIC X(2).                                   
014300       05 TAB-PRARTSTD-S98    PIC S9(3).                                  
014400       05 TAB-TID-RETOS       PIC X(2).                                   
014500       05 TAB-IDLANDX2        PIC X(2).                                   
014600       05 TAB-FLTRLO88-MAN    PIC X(1).                                   
014700       05 TAB-FLTRLO88-TIS    PIC X(1).                                   
014800       05 TAB-FLTRLO88-ONS    PIC X(1).                                   
014900       05 TAB-FLTRLO88-TOR    PIC X(1).                                   
015000       05 TAB-FLTRLO88-FRE    PIC X(1).                                   
015100       05 TAB-IDDISTR-SKROT   PIC 9(4).                                   
015200       05 TAB-IDKUNDNR-SKROT  PIC 9(6).                                   
015300       05 TAB-IDDISTR-QSKROT  PIC 9(4).                                   
015400       05 TAB-IDKUNDNR-QSKROT PIC 9(6).                                   
015500       05 TAB-IDDISTR-RSKROT  PIC 9(4).                                   
015600       05 TAB-IDKUNDNR-RSKROT PIC 9(6).                                   
015700                                                                          
015800 01  WS-FLTRLO88-NUM.                                                     
015900     03 WS-FLTRLO88-MAN-NUM      PIC 9(1)    VALUE ZERO.                  
016000     03 WS-FLTRLO88-TIS-NUM      PIC 9(1)    VALUE ZERO.                  
016100     03 WS-FLTRLO88-ONS-NUM      PIC 9(1)    VALUE ZERO.                  
016200     03 WS-FLTRLO88-TOR-NUM      PIC 9(1)    VALUE ZERO.                  
016300     03 WS-FLTRLO88-FRE-NUM      PIC 9(1)    VALUE ZERO.                  
016400                                                                          
016500 01 NYCKLAR-TP4TRAN.                                                      
016600     03 WS-IDDC-SEND             PIC X(2)    VALUE SPACE.                 
016700     03 WS-IDDC-REC              PIC X(2)    VALUE SPACE.                 
016800*                                                                         
016900 77  BEHANDLA-SW                 PIC X       VALUE 'J'.                   
017000     88  BEHANDLA                            VALUE 'J'.                   
017100     88  BEHANDLA-EJ                         VALUE 'N'.                   
017200*                                                                         
017300 77  RETUR-SW                    PIC X       VALUE 'N'.                   
017400     88  RETUR-OK                            VALUE 'J'.                   
017500*                                                                         
017600 77  SKROT-SW                    PIC X       VALUE 'N'.                   
017700     88  SKROT-OK                            VALUE 'J'.                   
017800*                                                                         
017900 01  SW-AREA88-TRANSF            PIC X(1).                                
018000     88 AREA88-TRANSF-JA                     VALUE 'Y'.                   
018100     88 AREA88-TRANSF-NEJ                    VALUE 'N'.                   
018200*                                                                         
018300 01  SW-FLSLSDTE-RET             PIC X(1).                                
018400     88 FLSLSDTE-RET-JA                      VALUE 'J'.                   
018500     88 FLSLSDTE-RET-NEJ                     VALUE 'N'.                   
018510*                                                                         
018520 01  SW-FLINLINL-RET             PIC X(1).                                
018530     88 FLINLINL-RET-JA                      VALUE 'J'.                   
018540     88 FLINLINL-RET-NEJ                     VALUE 'N'.                   
018600*                                                                         
018700*01  -COPY WWPRODSL                                                       
018800*                                                                         
018900*      --- VALID IDDC CODES                                               
019000*01    -COPY WWDC99                                                       
019100*01    -COPY WWDC99        -PRE REF-                                      
019200*                                                                         
019300 01  DYNAMISKA-SUBPROGRAM.                                                
019400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
019500     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
019600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
019700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
019800     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY'.             
019900     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
020000     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
020100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
020200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
020300     03  W005WDK7                PIC X(8)    VALUE 'W005WDK7'.            
020400     03  W271UTIL                PIC X(8)    VALUE 'W271UTIL'.            
020500     SKIP2                                                                
020600*    --- PARAMETRAR TILL ABEND                                            
020700                                                                          
020800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
020900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
021000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
021100     SKIP2                                                                
021200 01  FELTEXT.                                                             
021300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
021400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
021500     EJECT                                                                
021600*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
021700 01 FILLER                       PIC X(8)    VALUE 'W005WDK7'.            
021800*   -COPY W005WDK7                                                        
021900     EJECT                                                                
022000*    --- PARAMETRAR TILL W271UTIL                                         
022100*01 -COPY W271UTIL                                                        
022200     EJECT                                                                
022300                                                                          
022400*    --- PARAMETRAR TILL DATKORT                                          
022500*                                                                         
022600 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27116'.              
022700     SKIP2                                                                
022800 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
022900     SKIP2                                                                
023000*01  -COPY WDATKORT                                                       
023100     EJECT                                                                
023200*    --- PARAMETRAR TILL WORKDAY                                          
023300*                                                                         
023400*01  -COPY WORKAREA                                                       
023500     EJECT                                                                
023600*    --- PARAMETRAR TILL VECKOADD                                         
023700                                                                          
023800 01  W009VADD-AREA.                                                       
023900     03 VADD-DATUM-AAVV          PIC S9(5) VALUE ZERO COMP-3.             
024000     03 VADD-ANTAL               PIC S9(3) VALUE ZERO COMP-3.             
024100                                                                          
024200*    --- PARAMETRAR TILL POSTSUM                                          
024300*                                                                         
024400*01  -COPY W0005   -PRE  POSTSUM-                                         
024500     EJECT                                                                
024600*01  -COPY WDATAREA                                                       
024700     EJECT                                                                
024800*01  -COPY WZ20DAYS                                                       
024900     EJECT                                                                
025000*01    -COPY WWBYT03                                                      
025100     EJECT                                                                
025200 01  UT-AREA-START               PIC X(24)   VALUE                        
025300                                 'UT-AREA-START  '.                       
025400     SKIP2                                                                
025500                                                                          
025600*01  AREA -COPY W27111     -PRE UT-                                       
025700     EJECT                                                                
025800 01  UT98-AREA-START             PIC X(24)   VALUE                        
025900                                 'UT98-AREA-START'.                       
026000     SKIP2                                                                
026100                                                                          
026200*01  AREA -COPY W2712D     -PRE UT98-                                     
026300     EJECT                                                                
026400*******************************************                               
026500 01  NYCKLAR-TILL-DLI.                                                    
026600     03  W-IDDC-X.                                                        
026700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
026800     03  W-IDARTNR-X.                                                     
026900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
027000     03  W-KDSEGKEY-X.                                                    
027100         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
027200     03  W-IDDC-TRANSF-X.                                                 
027300         05  W-IDDC-TRANSF       PIC X(2)    VALUE SPACE.                 
027400     03  W-IDDC-INT-X.                                                    
027500         05  W-IDDC-INT          PIC X(2)    VALUE SPACE.                 
027600     03  W-IDDC-B616-X.                                                   
027700         05  W-IDDC-B616         PIC X(2)   VALUE SPACE.                  
027800     03  W-IDLAND-X.                                                      
027900         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
028000     03  W-WDQ4B1KY-MAX-X.                                                
028100         05  W-IDARTNR-Q4B1-MAX  PIC S9(9)    COMP-3.                     
028200         05  FILLER              PIC X(32)    VALUE HIGH-VALUE.           
028300                                                                          
028400     03  W-WDQ4B1KY-MIN-X.                                                
028500         05  W-IDARTNR-Q4B1-MIN  PIC S9(9)    COMP-3.                     
028600         05  FILLER              PIC X(32)    VALUE LOW-VALUE.            
028700                                                                          
028800     03  W-IDDISTR-Q4B1-X.                                                
028900         05  W-IDDISTR-Q4B1      PIC S9(5)  VALUE ZERO COMP-3.            
029000                                                                          
029100     03  W-IDKUNDNR-Q4B1-X.                                               
029200         05  W-IDKUNDNR-Q4B1     PIC S9(7)  VALUE ZERO COMP-3.            
029300                                                                          
029400     03  W-IDDISTR-Q4B1-Q-X.                                              
029500         05  W-IDDISTR-Q4B1-Q    PIC S9(5)  VALUE ZERO COMP-3.            
029600                                                                          
029700     03  W-IDKUNDNR-Q4B1-Q-X.                                             
029800         05  W-IDKUNDNR-Q4B1-Q   PIC S9(7)  VALUE ZERO COMP-3.            
029900                                                                          
030000     03  W-IDDISTR-Q4B1-R-X.                                              
030100         05  W-IDDISTR-Q4B1-R    PIC S9(5)  VALUE ZERO COMP-3.            
030200                                                                          
030300     03  W-IDKUNDNR-Q4B1-R-X.                                             
030400         05  W-IDKUNDNR-Q4B1-R   PIC S9(7)  VALUE ZERO COMP-3.            
030500                                                                          
030600                                                                          
030700*******************************************                               
030800*    --- STATUS-KOD FRÅN IMS                                              
030900 01  STATUS-WS                   PIC XX.                                  
031000     88  SEGMENT-FINNS                       VALUE '  '.                  
031100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
031200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
031300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
031400     SKIP2                                                                
031500 01  GODK-STATUSKODER.                                                    
031600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
031700     SKIP3                                                                
031800 01  SSA1                        PIC X(384).                              
031900 01  SSA2                        PIC X(128).                              
032000     EJECT                                                                
032100********DB2                                                               
032200 01  FILLER                      PIC X(16)  VALUE 'SQLCA-AREA'.           
032300       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
032400                                                                          
032500 01  FILLER                      PIC X(16)  VALUE 'SQLCODE-WS'.           
032600 01  DB2-WS.                                                              
032700     03  SQLCODE-WS              PIC 9(3)   VALUE ZERO.                   
032800         88  CURSOR-OK                      VALUE 000.                    
032900         88  LINES-FOUND                    VALUE 000.                    
033000         88  LINES-MISSING                  VALUE 100.                    
033100         88  MULTIPLE-ROWS-FOUND            VALUE 811.                    
033200         88  RESOURCE-WRONG                 VALUE 904.                    
033300     03  GOOD-SQLCODECODES.                                               
033400         05  GOOD-SQLCODE OCCURS 5                                        
033500             INDEXED BY SQLCODE-IX PIC 9(3).                              
033600 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
033700     EJECT                                                                
033800*******************************************                               
033900*    --- IMS FUNKTIONSKODER                                               
034000*01  -COPY W0003                                                          
034100     EJECT                                                                
034200 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
034300 01   DLI-IO-AREA-B601.                                                   
034400*     03  -COPY WDB601                                                    
034500                                                                          
034600     EJECT                                                                
034700 01  FILLER               PIC X(16)   VALUE 'WDB616 AREA'.                
034800 01   DLI-IO-AREA-B616.                                                   
034900*     03  -COPY WDB616       -PRE B6-                                     
035000     EJECT                                                                
035100                                                                          
035200 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK601'.                      
035300     SKIP3                                                                
035400 01  DLI-IO-AREA-WDK601.                                                  
035500*    03  -COPY WDK601                                                     
035600     EJECT                                                                
035700                                                                          
035800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK611'.                      
035900     SKIP3                                                                
036000 01  DLI-IO-AREA-WDK611.                                                  
036100*    03  -COPY WDK611                                                     
036200     EJECT                                                                
036300                                                                          
036400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711-INTERN'.               
036500 01  DLI-IO-WDK711-INTERN.                                                
036600*    03  -COPY WDK711   -PRE  INT-                                        
036700     EJECT                                                                
036800                                                                          
036900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK712'.                      
037000 01  DLI-IO-WDK712.                                                       
037100*    03  -COPY WDK712                                                     
037200     EJECT                                                                
037300                                                                          
037400 01  FILLER         PIC X(16)   VALUE 'DLI-IO-AREA-K7'.                   
037500*                                                                         
037600 01  DLI-IO-AREA-K7.                                                      
037700     03 IO-AREA-K7   PIC X(300)  VALUE SPACE.                             
037800     03 DLI-IO-WDK701 REDEFINES IO-AREA-K7.                               
037900*       05  -COPY WDK701                                                  
038000                                                                          
038100     03 DLI-IO-WDK711 REDEFINES IO-AREA-K7.                               
038200*       05  -COPY WDK711                                                  
038300                                                                          
038400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL701'.                      
038500 01  DLI-IO-WDL701.                                                       
038600*    03  -COPY WDL701                                                     
038700     EJECT                                                                
038800                                                                          
038900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL711'.                      
039000 01  DLI-IO-WDL711.                                                       
039100*    03  -COPY WDL711                                                     
039200     EJECT                                                                
039300                                                                          
039400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK711-TRANSF'.               
039500 01  DLI-IO-WDK711-TRANSF.                                                
039600*    03  -COPY WDK711   -PRE  TRANSF-                                     
039700     EJECT                                                                
039800                                                                          
039900 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL601'.                      
040000     SKIP3                                                                
040100 01  DLI-IO-AREA-WDL601.                                                  
040200*    03  -COPY WDL601                                                     
040300     EJECT                                                                
040400                                                                          
040500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL611'.                      
040600     SKIP3                                                                
040700 01  DLI-IO-AREA-WDL611.                                                  
040800*    03  -COPY WDL611                                                     
040900     EJECT                                                                
041000                                                                          
041100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ4B1'.                      
041200 01  DLI-IO-WDQ4B1.                                                       
041300*    03  -COPY WDQ4B1                                                     
041400     EJECT                                                                
041500                                                                          
075200*****DB2 AREOR*******************                                         
075300 01  FILLER                      PIC X(16)  VALUE 'TP1KAMP-AREA'.         
075500*01  -COPY TP1KAMP -PRE TP1KAMP-                                          
076000     EJECT                                                                
076001                                                                          
076002 01  FILLER                      PIC X(16)  VALUE 'TP1ARTK-AREA'.         
076003*01  -COPY TP1ARTK -PRE TP1ARTK-                                          
076004     EJECT                                                                
076005                                                                          
076006 01  FILLER                      PIC X(16)  VALUE 'TP4TRAN-AREA'.         
076007*01  -COPY TP4TRAN -PRE TP4TRAN-                                          
076008     EJECT                                                                
076009                                                                          
076100     EXEC SQL INCLUDE TP1KAMP END-EXEC.                                   
076200     EJECT                                                                
076300     EXEC SQL INCLUDE TP1ARTK END-EXEC.                                   
076400     EJECT                                                                
076500     EXEC SQL INCLUDE TP4TRAN END-EXEC.                                   
076600     EJECT                                                                
076700                                                                          
076800*******************************************                               
076900 LINKAGE SECTION.                                                         
077000                                                                          
077100*01  -COPY W0008      -PRE MSG-                                           
077200     05  FILLER                  PIC X.                                   
077300                                                                          
077400*01  -COPY W0008      -PRE WDB6-                                          
077500     05  FILLER                  PIC X.                                   
077600     EJECT                                                                
077700*01  -COPY W0008      -PRE WDK6-                                          
077800     05  FILLER                  PIC X.                                   
077900     EJECT                                                                
078000*01  -COPY W0008      -PRE WDK7-                                          
078100     05  WDK7-KEY-FB-AREA-IDARTNR       PIC S9(9) COMP-3.                 
078200     EJECT                                                                
078300*01  -COPY W0008      -PRE WDK7I-                                         
078400     05  FILLER                  PIC X.                                   
078500     EJECT                                                                
078600*01  -COPY W0008      -PRE WDL7-                                          
078700     05  FILLER                  PIC X.                                   
078800     EJECT                                                                
078900*01  -COPY W0008      -PRE WDL6-                                          
079000     05  FILLER                  PIC X.                                   
079100     EJECT                                                                
079200*01  -COPY W0008      -PRE WDQ4B-                                         
079300     05  FILLER                  PIC X.                                   
079400     EJECT                                                                
079500*01  -COPY W0008      -PRE UTIL-WDK6-                                     
079600     05  FILLER                  PIC X.                                   
079700     EJECT                                                                
079800*01  -COPY W0008      -PRE UTIL-WDK7-                                     
079900     05  FILLER                  PIC X.                                   
080000     EJECT                                                                
080100*01  -COPY W0008      -PRE UTIL-WDB6-                                     
080200     05  FILLER                  PIC X.                                   
080300     EJECT                                                                
080400*******************************************                               
080500 PROCEDURE DIVISION  USING MSG-PCB WDB6-PCB WDK6-PCB WDK7-PCB             
080600                           WDK7I-PCB WDL7-PCB WDL6-PCB WDQ4B-PCB          
080700                           UTIL-WDK6-PCB UTIL-WDK7-PCB                    
080800                           UTIL-WDB6-PCB.                                 
080900                                                                          
081000 MAIN SECTION.                                                            
081100     ENTRY 'DLITCBL' USING MSG-PCB WDB6-PCB WDK6-PCB WDK7-PCB             
081200                           WDK7I-PCB WDL7-PCB WDL6-PCB WDQ4B-PCB          
081300                           UTIL-WDK6-PCB UTIL-WDK7-PCB                    
081400                           UTIL-WDB6-PCB.                                 
081500                                                                          
081600     PERFORM A-INIT                                                       
081700     PERFORM B-KOLLA-DATUM                                                
081800     PERFORM IMS-GN-WDK7                                                  
081900     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
082000       EVALUATE WDK7-SEG-NAME-FB                                          
082100         WHEN 'WDK701  '                                                  
082200           MOVE WDK7-KEY-FB-AREA-IDARTNR TO W-IDARTNR                     
082300                                            BYT03-IDARTNR                 
082400         WHEN 'WDK711  '                                                  
082500           MOVE SLAG-IDDC    TO W-IDDC                                    
082600                                WS-IDDC-SEND                              
082700                                WS-IDDC                                   
082800           MOVE +1 TO WS-IDDC-IX                                          
082900           PERFORM UNTIL (WS-IDDC-IX > MAX-IDDC-IX)                       
083000             IF SLAG-IDDC = TAB-IDDC(WS-IDDC-IX)                          
083100               IF SLAG-IDDC-REF NOT = SPACES                              
083200*******LAGEROMRÅDE     98 RETURER**************                           
083300                 IF SLAG-IDDC-REF NOT = '11'                              
083400                   PERFORM S27-KOLLA-SEND-DC                              
083500                 END-IF                                                   
083600                 IF BEHANDLA                                              
083700                   IF SLAG-KVAKS-PAV > ZERO                               
083800                   OR SLAG-KVBEART > ZERO                                 
083900                     CONTINUE                                             
084000                   ELSE                                                   
084100                     IF TAB-TID-RET98(WS-IDDC-IX) = ZERO                  
084200                       CONTINUE                                           
084300                     ELSE                                                 
084400                       IF TAB-TID-RET98(WS-IDDC-IX) =                     
084500                          DAGENS-TIAAVVD-D                                
084600                          PERFORM C-LAGOMR98-RETUR                        
084700                       END-IF                                             
084800                     END-IF                                               
084900                   END-IF                                                 
085000*******ÖVERLAGER             RETURER*******************                   
085100                   IF SLAG-KVAKS-PAV > ZERO                               
085200                   OR SLAG-KVBEART > ZERO                                 
085300                     CONTINUE                                             
085400                   ELSE                                                   
085500                     IF TAB-TID-RETOS(WS-IDDC-IX) = ZERO                  
085600                       CONTINUE                                           
085700                     ELSE                                                 
085800                       IF TAB-TID-RETOS(WS-IDDC-IX) =                     
085900                          DAGENS-TIAAVVD-D                                
086000                         IF SLAG-ADLAGOMR = 98                            
086100                         AND TAB-TID-RET98(WS-IDDC-IX) NOT = ZERO         
086200                           CONTINUE                                       
086300                         ELSE                                             
086400                           PERFORM D-OVERLAGER-RETUR                      
086500                         END-IF                                           
086600                       END-IF                                             
086700                     END-IF                                               
086800                   END-IF                                                 
086900                 END-IF                                                   
087000*******LAGEROMRÅDE       88 TRANSFER*************                         
087100                 PERFORM F-CHECK-AREA88-TRANSF-DAY                        
087200                                                                          
087300                 IF AREA88-TRANSF-NEJ                                     
087400                   CONTINUE                                               
087500                 ELSE                                                     
087600                   IF AREA88-TRANSF-JA                                    
087700                     PERFORM E-LAGOMR88-TRANSF                            
087800                   END-IF                                                 
087900                 END-IF                                                   
088000               END-IF                                                     
088100             END-IF                                                       
088200             ADD  +1 TO WS-IDDC-IX                                        
088300           END-PERFORM                                                    
088400           MOVE JA   TO BEHANDLA-SW                                       
088500       END-EVALUATE                                                       
088600       PERFORM IMS-GN-WDK7                                                
088700     END-PERFORM                                                          
088800     DISPLAY 'DAG I VECKA :' DAGENS-TIAAVVD-D                             
088810     DISPLAY 'GNP WDL611  :' IX-L611                                      
088820     DISPLAY 'GNP WDK611  :' IX-K611                                      
088830     DISPLAY 'ANTAL UTIL  :' IX-UTIL                                      
088900                                                                          
089000                                                                          
089100     PERFORM Z-FINIT                                                      
089200                                                                          
089300     MOVE ZERO TO RETURN-CODE                                             
089400     GOBACK                                                               
089500     .                                                                    
089600     EJECT                                                                
089700 A-INIT SECTION.                                                          
089800                                                                          
089900     OPEN OUTPUT W27117 W27116                                            
090000                                                                          
090100     SKIP2                                                                
090200     ACCEPT DAGENS-DATUM    FROM DATE                                     
090300     PERFORM AA-LADDA-DC-TABELL                                           
090310     MOVE ZERO      TO IX-L611                                            
090320                       IX-K611                                            
090330                       IX-UTIL                                            
090400     .                                                                    
090500     EJECT                                                                
090600 AA-LADDA-DC-TABELL SECTION.                                              
090700                                                                          
090800     INITIALIZE WS-IDDC-TABELL                                            
090900     MOVE +1 TO WS-IDDC-IX                                                
091000                MAX-IDDC-IX                                               
091100     PERFORM IMS-GN-WDB601                                                
091200     PERFORM UNTIL SEGMENT-SLUT                                           
091300        IF DCS-KDDC = 'S '                                                
091400        OR DCS-KDDC = 'NC'                                                
091500        OR DCS-KDDC = 'NA'                                                
091510        OR DCS-KDDC = 'NP'                                                
091600          MOVE DCS-IDDC           TO TAB-IDDC(WS-IDDC-IX)                 
091700          MOVE DCS-KDDC           TO TAB-KDDC(WS-IDDC-IX)                 
091800          MOVE DCS-REQXBRYT       TO TAB-REQXBRYT(WS-IDDC-IX)             
091900          MOVE DCS-TID-RET98      TO TAB-TID-RET98(WS-IDDC-IX)            
092000          IF DCS-PRARTSTD-SKRLO98 NUMERIC                                 
092100          MOVE DCS-PRARTSTD-SKRLO98                                       
092200                                  TO TAB-PRARTSTD-S98(WS-IDDC-IX)         
092300          ELSE                                                            
092400          MOVE ZERO               TO TAB-PRARTSTD-S98(WS-IDDC-IX)         
092500          END-IF                                                          
092600          MOVE DCS-TID-RETOS      TO TAB-TID-RETOS(WS-IDDC-IX)            
092700          MOVE DCS-IDLANDX2       TO TAB-IDLANDX2(WS-IDDC-IX)             
092800          MOVE DCS-FLTRLO88-MAN   TO TAB-FLTRLO88-MAN(WS-IDDC-IX)         
092900          MOVE DCS-FLTRLO88-TIS   TO TAB-FLTRLO88-TIS(WS-IDDC-IX)         
093000          MOVE DCS-FLTRLO88-ONS   TO TAB-FLTRLO88-ONS(WS-IDDC-IX)         
093100          MOVE DCS-FLTRLO88-TOR   TO TAB-FLTRLO88-TOR(WS-IDDC-IX)         
093200          MOVE DCS-FLTRLO88-FRE   TO TAB-FLTRLO88-FRE(WS-IDDC-IX)         
093300          MOVE DCS-IDDISTR-SKROT  TO TAB-IDDISTR-SKROT(WS-IDDC-IX)        
093400          MOVE DCS-IDKUNDNR-SKROT TO                                      
093500                                  TAB-IDKUNDNR-SKROT(WS-IDDC-IX)          
093600          MOVE DCS-IDDISTR-QSKROT TO                                      
093700                                  TAB-IDDISTR-QSKROT(WS-IDDC-IX)          
093800          MOVE DCS-IDKUNDNR-QSKROT TO                                     
093900                                  TAB-IDKUNDNR-QSKROT(WS-IDDC-IX)         
094000          MOVE DCS-IDDISTR-RSKROT TO                                      
094100                                  TAB-IDDISTR-RSKROT(WS-IDDC-IX)          
094200          MOVE DCS-IDKUNDNR-RSKROT TO                                     
094300                                  TAB-IDKUNDNR-RSKROT(WS-IDDC-IX)         
094400          ADD +1 TO WS-IDDC-IX                                            
094500                    MAX-IDDC-IX                                           
094600          IF WS-IDDC-IX > 300                                             
094700             MOVE 'DC-TABELLEN FULL' TO FELTEXT                           
094800             CALL FELLOG                                                  
094900          END-IF                                                          
095000        END-IF                                                            
095100        PERFORM IMS-GN-WDB601                                             
095200     END-PERFORM                                                          
095300     .                                                                    
095400     EJECT                                                                
095500 B-KOLLA-DATUM SECTION.                                                   
095600     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
095700     MOVE DAGENS-DATUM      TO DAT-I-TIDATUM                              
095800                                                                          
095900     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
096000                     DAT-O-TIDATUM DAT-KDSVAR                             
096100                                                                          
096200     IF DAT-KDSVAR-OK                                                     
096300        MOVE DAT-TIAAVVD    TO WS-TIAAVVD                                 
096400                               DAGENS-TIAAVVD                             
096500     ELSE                                                                 
096600        DISPLAY 'FEL FRÅN DATKONV I B-KOLLA-DATUM'                        
096700        CALL FELLOG                                                       
096800     END-IF                                                               
096900                                                                          
097000     IF DAGENS-TIAAVVD-VV = 01 OR 03 OR 05 OR 07 OR 09 OR 11 OR           
097100                            13 OR 15 OR 17 OR 19 OR 21 OR 23 OR           
097200                            25 OR 27 OR 29 OR 31 OR 33 OR 35 OR           
097300                            37 OR 39 OR 41 OR 43 OR 45 OR 47 OR           
097400                            49 OR 51 OR 53                                
097500        MOVE 'OJAMN'        TO WS-VECKOTYP                                
097600     ELSE                                                                 
097700        MOVE ' JAMN'        TO WS-VECKOTYP                                
097800     END-IF                                                               
097810*****FÖR SIMULERA TEST MOT OLIKA DAGAR                                    
097820**   MOVE 7 TO DAGENS-TIAAVVD-D                                           
097830*****                                                                     
097900*                                                                         
098000*    CALCULATIONS FOR DATE 18 MONTHS BACK                                 
098100*                                                                         
098200     MOVE  DAGENS-DATUM        TO WS-DATUM-YEAR-1                         
098300     SUBTRACT 1 FROM DAGENS-DATUM-AAR GIVING WS-DATUM-YY-1                
098400                                                                          
098500     MOVE  'YYMMDD'            TO DAYS-KDDATFMT1                          
098600     MOVE  'YYMMDD'            TO DAYS-KDDATFMT2                          
098700     MOVE  WS-DATUM-YEAR-1     TO DAYS-TIDATE1                            
098800     MOVE  -180                TO DAYS-KVDAYS                             
098900     MOVE  SPACE               TO DAYS-TIDATE2                            
099000                                  DAYS-IDCALEND                           
099100     CALL  WZ20DAYS USING DAYS-WZ20DAYS                                   
099200                                                                          
099300     MOVE  DAYS-TIDATE2(1:6)   TO WS-DATUM-MINUS-18MONTH                  
099400     .                                                                    
099500     EJECT                                                                
099600 C-LAGOMR98-RETUR SECTION.                                                
099700                                                                          
099800     PERFORM IMS-GU-WDK601                                                
099810     IF SEGMENT-FINNS                                                     
099900       PERFORM IMS-GNP-WDK611                                             
100000       IF SEGMENT-FINNS                                                   
100100          IF CLAG-KDLEVSP = 20 OR 21 OR 22                                
100200             CONTINUE                                                     
100300          ELSE                                                            
100400             MOVE 98    TO WS-RETUR-ADLAGOMR                              
100500             IF WS-VECKOTYP = ' JAMN'                                     
100600               MOVE 1   TO WS-RETUR-ADGANG                                
100700             ELSE                                                         
100800               MOVE 2   TO WS-RETUR-ADGANG                                
100900             END-IF                                                       
101000             IF SLAG-ADLAGOMR = WS-RETUR-ADLAGOMR                         
101100               IF SLAG-ADGANG = WS-RETUR-ADGANG                           
101200                 IF SLAG-KVLS > ZERO                                      
101300                   IF SLAG-FLSKROT-BEORD = 'N'                            
101400                     MOVE W-IDARTNR       TO UT-IDARTNR                   
101500                     MOVE SLAG-IDDC       TO UT-IDDC                      
101600                     MOVE ZERO            TO WS-KVBEART                   
101700                     COMPUTE WS-KVBEART = SLAG-KVLS                       
101800                                        - SLAG-KVOKS-BULK                 
101900                                        - SLAG-KVOKS-DAG                  
102000                                                                          
102100                     MOVE WS-KVBEART      TO UT-KVBEART                   
102200                     MOVE SLAG-ADLAGOMR   TO UT-ADLAGOMR-CDC              
102300                     MOVE SLAG-ADGANG     TO UT-ADGANG-CDC                
102400                     MOVE SLAG-ADPLATS    TO UT-ADPLATS-CDC               
102500                     MOVE SLAG-IDLEVNR    TO UT-IDLEVNR                   
102600                     MOVE SLAG-IDPERSON-BUY TO UT-IDPERSON-BUY            
102700                     MOVE 'R'             TO UT-KDREFTYP                  
102800                     MOVE 'U'             TO UT-KDREFORS                  
102810                     MOVE SLAG-IDDC-REF   TO UT-IDDC-REF                  
102900                     IF SLAG-IDDC-REF = '11'                              
103000                       MOVE CLAG-ADLAGOMR TO UT-ADLAGOMR-SDC              
103100                       MOVE CLAG-ADGANG   TO UT-ADGANG-SDC                
103200                       MOVE CLAG-ADPLATS  TO UT-ADPLATS-SDC               
103210                       MOVE '11'          TO UT-IDDC-REF                  
103300                     ELSE                                                 
103400                       MOVE INT-SLAG-ADLAGOMR TO UT-ADLAGOMR-SDC          
103500                       MOVE INT-SLAG-ADGANG   TO UT-ADGANG-SDC            
103600                       MOVE INT-SLAG-ADPLATS  TO UT-ADPLATS-SDC           
103700                     END-IF                                               
103800                     MOVE ZERO            TO UT-IDDISTR                   
103900                                               UT-ADLAGOMR-CD             
104000                                               UT-ADGANG-CD               
104100                                               UT-ADPLATS-CD              
104200                                               UT-KVBEART-CD              
104300                                               UT-KDREFTXT                
104400                                               UT-KDFRAKT                 
104500                                               UT-IDKUNDNR                
104700                     IF WS-KVBEART > ZERO                                 
104800                        PERFORM CA-KOLLA-RETUR                            
104900                        IF RETUR-OK                                       
105000                           PERFORM S11-SKRIV-W27117                       
105100                        ELSE                                              
105200                            PERFORM CB-KOLLA-SKROT                        
105300                            IF SKROT-OK                                   
105400                               PERFORM CC-REDIGERA-SKRIV-W27116           
105500                            END-IF                                        
105600                        END-IF                                            
105700                     END-IF                                               
105800                   END-IF                                                 
105900                 END-IF                                                   
106000               END-IF                                                     
106100             END-IF                                                       
106200          END-IF                                                          
106300       ELSE                                                               
106400         DISPLAY 'ARTIKEL SAKNAS PÅ WDK6'                                 
106500         CALL FELLOG                                                      
106600       END-IF                                                             
106610     END-IF                                                               
106700     .                                                                    
106800     EJECT                                                                
106900                                                                          
107000 CA-KOLLA-RETUR SECTION.                                                  
107100     MOVE NEJ TO RETUR-SW                                                 
107200     IF CLAG-KDERS < 11                                                   
107300        IF SLAG-IDDC (1:1) = '7' OR '4' OR '5'                            
107400           IF SLAG-IDDC (1:1) = '7'                                       
107500             MOVE 'CN' TO W-IDLAND                                        
107600           END-IF                                                         
107700           IF SLAG-IDDC (1:1) = '4'                                       
107800             MOVE 'US' TO W-IDLAND                                        
107900           END-IF                                                         
108000           IF SLAG-IDDC (1:1) = '5'                                       
108100             MOVE 'CA' TO W-IDLAND                                        
108200           END-IF                                                         
108300           PERFORM IMS-GU-WDK712                                          
108400           IF SEGMENT-FINNS                                               
108500              MOVE LART-PRMATRL TO WS-PRIS                                
108600           ELSE                                                           
108700              MOVE ZERO         TO WS-PRIS                                
108800           END-IF                                                         
108900        ELSE                                                              
109000           MOVE CLAG-PRARTSTD   TO WS-PRIS                                
109100        END-IF                                                            
109200        IF CLAG-KVROS > ZERO OR                                           
109300           WS-PRIS > TAB-PRARTSTD-S98 (WS-IDDC-IX)                        
109400           MOVE JA TO RETUR-SW                                            
109500        END-IF                                                            
109600     END-IF                                                               
109700     .                                                                    
109800     EJECT                                                                
109900                                                                          
110000 CB-KOLLA-SKROT SECTION.                                                  
110100                                                                          
110200     MOVE NEJ  TO SKROT-SW                                                
110300     MOVE ZERO TO WS-KVSKROT                                              
110400                                                                          
110500     PERFORM CBA-KOLLA-UNDANTAG                                           
110600     IF SKROT-OK                                                          
110700        PERFORM CBB-BERAKNA-ANTAL-ATT-SKROTA                              
110800        IF WS-KVSKROT = ZERO                                              
110900           MOVE NEJ TO SKROT-SW                                           
111000        END-IF                                                            
111100     END-IF                                                               
111200     .                                                                    
111300     EJECT                                                                
111400                                                                          
111500 CBA-KOLLA-UNDANTAG SECTION.                                              
111600                                                                          
111700     MOVE JA TO SKROT-SW                                                  
111800                                                                          
111900     IF (SLAG-KDLEVSP = 20 OR 21 OR 22)                                   
112000     OR SLAG-KVSPARR-KVAL > ZERO                                          
112100     OR SLAG-KVBEART      > ZERO                                          
112200     OR SLAG-KVAKS-PAV    > ZERO                                          
112300     OR SLAG-KVAKS-SDC    > ZERO                                          
112400        MOVE NEJ TO SKROT-SW                                              
112500     END-IF                                                               
112600                                                                          
112700     IF SLAG-FLSKROT-BEORD = JA                                           
112800     OR SLAG-TISKROT-AUTO  > DAGENS-DATUM                                 
112900        MOVE NEJ TO SKROT-SW                                              
113000     END-IF                                                               
113100                                                                          
113200     MOVE ART-KDPRODSL TO TEST-KDPRODSL                                   
113300     IF KDPRODSL-BYTES                                                    
113400     OR KDPRODSL-VOLVO-EMB                                                
113500     OR KDPRODSL-LOCAL-EMB                                                
113600     OR ART-IDFKNGRP      = 8616                                          
113700        MOVE NEJ TO SKROT-SW                                              
113800     END-IF                                                               
113900                                                                          
114000     IF CLAG-IDPROJ   = 'OBJ'                                             
114100     OR CLAG-PRARTSJK = ZERO                                              
114200        MOVE NEJ TO SKROT-SW                                              
114300     END-IF                                                               
114400                                                                          
114500     .                                                                    
114600     EJECT                                                                
114700                                                                          
114800 CBB-BERAKNA-ANTAL-ATT-SKROTA SECTION.                                    
114900                                                                          
115000     COMPUTE WS-KVSKROT = SLAG-KVLS                                       
115100                        - SLAG-KVOKS-BULK                                 
115200                        - SLAG-KVOKS-DAG                                  
115300                        - SLAG-KVRESS                                     
115400                        - SLAG-KVUTRS                                     
115500                        - SLAG-KVROS-BULK                                 
115600                        - SLAG-KVROS-DAG                                  
115700                                                                          
115800     IF WS-KVSKROT > ZERO                                                 
115900       MOVE ZERO         TO WS-KVBEART-SKROT                              
116000                                                                          
116100       MOVE LOW-VALUE    TO W-WDQ4B1KY-MIN-X                              
116200       MOVE HIGH-VALUE   TO W-WDQ4B1KY-MAX-X                              
116300       MOVE W-IDARTNR    TO W-IDARTNR-Q4B1-MIN                            
116400                            W-IDARTNR-Q4B1-MAX                            
116500                                                                          
116600       MOVE TAB-IDDISTR-SKROT(WS-IDDC-IX)   TO W-IDDISTR-Q4B1             
116700       MOVE TAB-IDKUNDNR-SKROT(WS-IDDC-IX)  TO W-IDKUNDNR-Q4B1            
116800       MOVE TAB-IDDISTR-QSKROT(WS-IDDC-IX)  TO W-IDDISTR-Q4B1-Q           
116900       MOVE TAB-IDKUNDNR-QSKROT(WS-IDDC-IX) TO W-IDKUNDNR-Q4B1-Q          
117000       MOVE TAB-IDDISTR-RSKROT(WS-IDDC-IX)  TO W-IDDISTR-Q4B1-R           
117100       MOVE TAB-IDKUNDNR-RSKROT(WS-IDDC-IX) TO W-IDKUNDNR-Q4B1-R          
117200                                                                          
117300       PERFORM IMS-GU-WDQ4B1                                              
117400       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
117500                                                                          
117600          ADD SEQB-KVBEART-Q TO WS-KVBEART-SKROT                          
117700                                                                          
117800          PERFORM IMS-GN-WDQ4B1                                           
117900       END-PERFORM                                                        
118000       IF WS-KVBEART-SKROT > ZERO                                         
118100          COMPUTE WS-KVSKROT = WS-KVSKROT - WS-KVBEART-SKROT              
118200       END-IF                                                             
118300     END-IF                                                               
118400     .                                                                    
118500     EJECT                                                                
118600                                                                          
118700 CC-REDIGERA-SKRIV-W27116 SECTION.                                        
118800                                                                          
118900     PERFORM CCA-BERAKNA-SKROTVARDE                                       
119000                                                                          
119100     IF   WS-KVSKROT > ZERO                                               
119200       MOVE W-IDARTNR       TO TP1ARTK-IDARTNR                            
119300       PERFORM DB2-SELECT-TP1KAMP-TP1ARTK                                 
119400       IF LINES-MISSING                                                   
119500        MOVE '16'                           TO UT98-IDPTYP                
119600        MOVE W-IDARTNR                      TO UT98-IDARTNR               
119700        MOVE W-IDDC                         TO UT98-IDDC                  
119800        MOVE FUNCTION CURRENT-DATE (1:8)    TO UT98-DADATUM               
119900        MOVE TAB-IDDISTR-SKROT(WS-IDDC-IX)  TO UT98-IDDISTR               
120000        MOVE TAB-IDKUNDNR-SKROT(WS-IDDC-IX) TO UT98-IDKUNDNR              
120100        MOVE WS-VALUE-KVSKROT               TO UT98-SUARTSTD              
120200        MOVE WS-KVSKROT                     TO UT98-KVSKROT               
120300        MOVE TAB-KDDC(WS-IDDC-IX)           TO UT98-KDDC                  
120400                                                                          
120500        PERFORM S12-SKRIV-W27116                                          
120600       ELSE                                                               
120700         MOVE SPACE      TO POSTSUM-TRANSTYP                              
120800         MOVE 'W27116'   TO POSTSUM-FDNAMN                                
120900         MOVE 'SUPPRESD' TO POSTSUM-DDNAMN2                               
121000         CALL POSTSUM USING POSTSUM-PARM                                  
121100       END-IF                                                             
121200     END-IF                                                               
121300     .                                                                    
121400     EJECT                                                                
121500                                                                          
121600 CCA-BERAKNA-SKROTVARDE SECTION.                                          
121700                                                                          
121800     MOVE ZERO     TO WS-VALUE-KVSKROT                                    
121900                                                                          
122000     IF SLAG-IDDC-REF(1:1) = '7'                                          
122100     OR SLAG-IDDC (1:1)    = '7'                                          
122200     OR SLAG-IDDC-REF(1:1) = '4'                                          
122300     OR SLAG-IDDC (1:1)    = '4'                                          
122400     OR SLAG-IDDC-REF(1:1) = '5'                                          
122500     OR SLAG-IDDC (1:1)    = '5'                                          
122510     OR SLAG-IDDC-REF(1:1) = '6'                                          
122520     OR SLAG-IDDC (1:1)    = '6'                                          
122600        IF SLAG-IDDC-REF(1:1) = '7'                                       
122700        OR SLAG-IDDC (1:1)    = '7'                                       
122800          MOVE 'CN'        TO W-IDLAND                                    
122900        END-IF                                                            
123000        IF SLAG-IDDC-REF(1:1) = '4'                                       
123100        OR SLAG-IDDC (1:1)    = '4'                                       
123200          MOVE 'US'        TO W-IDLAND                                    
123300        END-IF                                                            
123400        IF SLAG-IDDC-REF(1:1) = '5'                                       
123500        OR SLAG-IDDC (1:1)    = '5'                                       
123600          MOVE 'CA'        TO W-IDLAND                                    
123700        END-IF                                                            
123701        IF SLAG-IDDC-REF = '61'                                           
123702        OR SLAG-IDDC-REF = '6A'                                           
123703        OR SLAG-IDDC   = '61'                                             
123704        OR SLAG-IDDC   = '6A'                                             
123730          MOVE 'JP'        TO W-IDLAND                                    
123740        END-IF                                                            
123800        PERFORM IMS-GU-WDK712                                             
123900        IF SEGMENT-FINNS                                                  
124000           COMPUTE WS-VALUE-KVSKROT ROUNDED =                             
124100                   LART-PRMATRL * WS-KVSKROT                              
124200        END-IF                                                            
124300     ELSE                                                                 
124400        COMPUTE WS-VALUE-KVSKROT ROUNDED =                                
124500                CLAG-PRARTSTD * WS-KVSKROT                                
124600     END-IF                                                               
124700     .                                                                    
124800     EJECT                                                                
124900                                                                          
125000 D-OVERLAGER-RETUR SECTION.                                               
125100     PERFORM IMS-GU-WDK601                                                
125110     IF SEGMENT-FINNS                                                     
125200       PERFORM IMS-GNP-WDK611                                             
125300       IF SEGMENT-FINNS                                                   
125400         IF CLAG-KDERS = ZERO                                             
125500           PERFORM DB-CHECK-LSLSDTE                                       
125600           MOVE ART-KDPRODSL        TO TEST-KDPRODSL                      
125700           IF CLAG-KDLEVSP = 20 OR 21 OR 22                               
125800           OR CLAG-KDFARLIG = 4                                           
125900           OR SLAG-KVSPARR-KVAL > ZERO                                    
126000           OR SLAG-FLSKROT-BEORD = 'J'                                    
126100           OR SLAG-FLREFBEO = 'S'                                         
126200           OR SLAG-KVLS < 1                                               
126300           OR ART-IDFKNGRP = 8616                                         
126400           OR KDPRODSL-VOLVO-EMB                                          
126500           OR KDPRODSL-CHEMICAL                                           
126600           OR (W-IDDC = '91' AND BYT03-OBJEKT)                            
126700           OR FLSLSDTE-RET-NEJ                                            
126800           OR CLAG-REDIRLEV = 1.00                                        
126900             CONTINUE                                                     
127000           ELSE                                                           
127100************KOLLAR     OM ÖVERLAGER SDC/LDC***********                    
127200             PERFORM S25-BERAKNA-OVERLAGER                                
127300             IF W-TILLG-SDC    > SLAG-KVREFOVL                            
127400************KOLLAR  OM SALDO SEND DC ÄR MINDRE ÄN 30 VECKORS LAGER        
127500               IF SLAG-IDDC-REF = '11'                                    
127600                 PERFORM S21-BERAKNA-30V-LAGERSALDO-CDC                   
127700                 MOVE CLAG-KVLS     TO WS-KVLS                            
127800               ELSE                                                       
127900                 PERFORM S21-BERAKNA-30V-LAGERSALDO-NDC                   
128000                 MOVE INT-SLAG-KVLS TO WS-KVLS                            
128100               END-IF                                                     
128200               IF WS-KVLS < WS-KVPB-TOT-30V-SEND-DC                       
128300*KOLLAR  OM FÖRSTA INLEV TILL CDC VAR FÖR MER ÄN 6 MÅNADER SEDAN*         
128400                 PERFORM S22-BERAKNA-6MANADER-SEDAN                       
128500                IF WS-DAGENS-AAAAVV-MINUS-26V > WS-TIFINLV-AAAAVV         
128600*KOLLAR  SÅ ATT INTE ARTIKELN ÄR REFILLAD INOM 90 DAGAR**********         
128700                   PERFORM S23-BERAKNA-MINUS-90DAGAR                      
128800*                  IF SLAG-IDDC-REF = '71' OR '72' OR '73'                
128900                   IF SLAG-IDDC-REF NOT = '11'                            
128910                     IF SW-FLINLINL-RET = NEJ                             
129001                       MOVE WS-TIINLINL TO WS-DATUM-KNTRL                 
129002                     ELSE                                                 
129003                       PERFORM S28-KOLLA-SENASTE-INL                      
129004                       MOVE WS-TIINLINL TO WS-DATUM-KNTRL                 
129010                     END-IF                                               
129100***OM   INTERN REFILL KINA KOLLA MOT SENASTE INLEVERANS                   
129300                   ELSE                                                   
129400***OM   REFILL FRÅN CDC KOLLA MOT SENAST REFILLAD                         
129500                     MOVE SLAG-TIORDREG TO WS-DATUM-KNTRL                 
129600                   END-IF                                                 
129700                   IF DAGENS-AAMMDD-MINUS-90DAG > WS-DATUM-KNTRL          
129800                     PERFORM DA-KOLLA-ANTAL-RETUR                         
129900                     IF WS-ANTAL-RETUR < +1                               
130000                       CONTINUE                                           
130100                     ELSE                                                 
130200                       MOVE W-IDARTNR     TO UT-IDARTNR                   
130300                       MOVE SLAG-IDDC     TO UT-IDDC                      
130400                       MOVE WS-ANTAL-RETUR TO UT-KVBEART                  
130500                       MOVE SLAG-ADLAGOMR TO UT-ADLAGOMR-CDC              
130600                       MOVE SLAG-ADGANG   TO UT-ADGANG-CDC                
130700                       MOVE SLAG-ADPLATS  TO UT-ADPLATS-CDC               
130800                       MOVE SLAG-IDLEVNR  TO UT-IDLEVNR                   
130900                       MOVE SLAG-IDPERSON-BUY TO UT-IDPERSON-BUY          
131000                       MOVE 'R'           TO UT-KDREFTYP                  
131100                       MOVE 'U'           TO UT-KDREFORS                  
131110                       MOVE SLAG-IDDC-REF TO UT-IDDC-REF                  
131200                       IF SLAG-IDDC-REF = '11'                            
131300                         MOVE CLAG-ADLAGOMR TO UT-ADLAGOMR-SDC            
131400                         MOVE CLAG-ADGANG TO UT-ADGANG-SDC                
131500                         MOVE CLAG-ADPLATS TO UT-ADPLATS-SDC              
131510                         MOVE '11'          TO UT-IDDC-REF                
131600                       ELSE                                               
131700                        MOVE INT-SLAG-ADLAGOMR TO UT-ADLAGOMR-SDC         
131800                         MOVE INT-SLAG-ADGANG TO UT-ADGANG-SDC            
131900                         MOVE INT-SLAG-ADPLATS TO UT-ADPLATS-SDC          
132000                       END-IF                                             
132100                       MOVE ZERO          TO UT-IDDISTR                   
132200                                                 UT-ADLAGOMR-CD           
132300                                                 UT-ADGANG-CD             
132400                                                 UT-ADPLATS-CD            
132500                                                 UT-KVBEART-CD            
132600                                                 UT-KDREFTXT              
132700                                                 UT-KDFRAKT               
132800                                                 UT-IDKUNDNR              
132900                       IF WS-ANTAL-RETUR > ZERO                           
133000****OM     STD PRIS UNDER 10 SEK SÅ MÅSTE TOTALA RETURVÄRDET              
133100****VARA     MER ÄN 200 SEK FÖR ATT RETUNERAS                             
133200                         IF SLAG-IDDC (1:1) = '7' OR '4' OR '5'           
133300                           IF SLAG-IDDC (1:1) = '7'                       
133400                             MOVE 'CN' TO W-IDLAND                        
133500                           END-IF                                         
133600                           IF SLAG-IDDC (1:1) = '4'                       
133700                             MOVE 'US' TO W-IDLAND                        
133800                           END-IF                                         
133900                           IF SLAG-IDDC (1:1) = '5'                       
134000                             MOVE 'CA' TO W-IDLAND                        
134100                           END-IF                                         
134200                           PERFORM IMS-GU-WDK712                          
134300                           IF SEGMENT-FINNS                               
134400                             MOVE LART-PRMATRL  TO WS-PRIS                
134500                           ELSE                                           
134600                             MOVE 'WDK712 SAKNAS'                         
134700                                       TO FELTEXT-STR                     
134800                             CALL FELLOG                                  
134900                           END-IF                                         
135000                         ELSE                                             
135100                           MOVE CLAG-PRARTSTD   TO WS-PRIS                
135200                         END-IF                                           
135300                         IF WS-PRIS < 10                                  
135400                           COMPUTE WS-VARDE-RETUR =                       
135500                                   WS-ANTAL-RETUR * WS-PRIS               
135600                                                                          
135700                           IF WS-VARDE-RETUR > 200                        
135800                             PERFORM S11-SKRIV-W27117                     
135900                           END-IF                                         
136000                         ELSE                                             
136100                           PERFORM S11-SKRIV-W27117                       
136200                         END-IF                                           
136300                       END-IF                                             
136400                     END-IF                                               
136500                   END-IF                                                 
136600                 END-IF                                                   
136700               END-IF                                                     
136800             END-IF                                                       
136900           END-IF                                                         
137000         END-IF                                                           
137100       END-IF                                                             
137110     END-IF                                                               
137200                                                                          
137300     .                                                                    
137400     EJECT                                                                
137500                                                                          
137600 DA-KOLLA-ANTAL-RETUR SECTION.                                            
137700                                                                          
137800     MOVE ZERO       TO WS-ANTAL-RETUR                                    
137900                        W-OVERLAGER-SDC                                   
138000                        WS-ANTAL-RETUR                                    
138100                        WS-ANTAL-KVAR                                     
138200                        WS-ANTAL-QX                                       
138300                        WS-TIREFEFT-6                                     
138400                                                                          
138500****GENERELLT GÄLLER ATT ÖVERLAGER + 1 REFILLKVANT RETUNERAS              
138600     COMPUTE W-OVERLAGER-SDC = W-TILLG-SDC - SLAG-KVREFOVL                
138700     IF W-OVERLAGER-SDC > ZERO                                            
138800       COMPUTE WS-ANTAL-RETUR = W-OVERLAGER-SDC + SLAG-KVREFBER           
138900       COMPUTE WS-ANTAL-KVAR  = W-TILLG-SDC - WS-ANTAL-RETUR              
139000****OM KVQPACK-1 INTE ÄR NOLL SÅ SKALL MAN RUNDA AV TILL NÄRMASTE         
139100**** KVQPACK-1 SOM SKALL LIGGA KVAR PÅ SDC/LDC.                           
139200       IF CLAG-KVQPACK-1 > ZERO                                           
139300*      AND SLAG-IDDC-REF(1:1) NOT = '7'                                   
139400         PERFORM S26-KVQPACK-1                                            
139500           COMPUTE WS-ANTAL-RETUR = W-TILLG-SDC - WS-ANTAL-QX             
139600       END-IF                                                             
139700       PERFORM S24-BERAKNA-MINUS-365DAG                                   
139800       PERFORM IMS-GU-WDL711                                              
139900*                                                                         
140000*****                                                                     
140100       IF SEGMENT-SAKNAS                                                  
140200         MOVE +999999 TO DC-TIREFEFT                                      
140300       END-IF                                                             
140400       MOVE DC-TIREFEFT      TO WS-TIREFEFT-6                             
140500       IF WS-TIREFEFT-6(1:2) < 50                                         
140600         COMPUTE WS-TIREFEFT-8 = 20000000 + WS-TIREFEFT-6                 
140700       ELSE                                                               
140800         COMPUTE WS-TIREFEFT-8 = 19000000 + WS-TIREFEFT-6                 
140900       END-IF                                                             
141000*****                                                                     
141100*                                                                         
141200       IF DAGENS-AAAAMMDD-MINUS-365DAG < WS-TIREFEFT-8                    
141300****OM   SENASTE ORDERDATUM ÄR FÖR MINDRE ÄN 12 MÅNADER SEDAN             
141400****MÅSTE   MINST 1 FINNAS KVAR SOM SALDO DÅ SDC/LDC                      
141500*        IF CLAG-KVQPACK-1 > ZERO                                         
141600*        AND SLAG-IDDC-REF(1:1) NOT = '7'                                 
141700*          CONTINUE                                                       
141800*        ELSE                                                             
141900*          IF WS-ANTAL-RETUR >= W-TILLG-SDC                               
142000*            MOVE W-TILLG-SDC TO WS-ANTAL-RETUR                           
142100*            COMPUTE WS-ANTAL-RETUR = WS-ANTAL-RETUR - 1                  
142200*          END-IF                                                         
142300*        END-IF                                                           
142400*      ELSE                                                               
142500*****OM SENASTE ORDERDATUM ÄR FÖR MER ÄN 12 MÅNADER SEDAN                 
142600*****KAN MAN RETUNERA ALLT OM ANTAL RETUR ÄR MER ÄN TILLGÅNGEN DC         
142700         IF CLAG-KVQPACK-1 > ZERO                                         
142800*        AND SLAG-IDDC-REF(1:1) NOT = '7'                                 
142900           CONTINUE                                                       
143000         ELSE                                                             
143100           IF WS-ANTAL-RETUR >= W-TILLG-SDC                               
143200             MOVE W-TILLG-SDC TO WS-ANTAL-RETUR                           
143300           END-IF                                                         
143400         END-IF                                                           
143500       END-IF                                                             
143600     END-IF                                                               
143700     .                                                                    
143800     EJECT                                                                
143900                                                                          
144000 DB-CHECK-LSLSDTE  SECTION.                                               
144100                                                                          
144200     MOVE JA TO SW-FLSLSDTE-RET                                           
144210                SW-FLINLINL-RET                                           
144300                                                                          
144400     IF SLAG-KVREFOVL = 0                                                 
144500        PERFORM S29-GET-LSLSDTE                                           
144700        IF    WS-TIREFEFT-6  >  ZERO                                      
144800          IF  WS-TIREFEFT-6  < DAGENS-DATUM                               
144900          AND WS-TIREFEFT-6  > WS-DATUM-MINUS-18MONTH                     
145000              MOVE NEJ      TO SW-FLSLSDTE-RET                            
145100          END-IF                                                          
145200        ELSE                                                              
145210          PERFORM S28-KOLLA-SENASTE-INL                                   
145300          IF  WS-TIINLINL    > ZERO                                       
145400          AND WS-TIINLINL    < DAGENS-DATUM                               
145500          AND WS-TIINLINL    > WS-DATUM-MINUS-18MONTH                     
145600              MOVE NEJ      TO SW-FLSLSDTE-RET                            
145610                               SW-FLINLINL-RET                            
145700          END-IF                                                          
145800        END-IF                                                            
145900     END-IF                                                               
146000     .                                                                    
146100     EJECT                                                                
146200                                                                          
146300 E-LAGOMR88-TRANSF SECTION.                                               
146400******HANTERING AV BUMPERS, TRANSFER SKER ALLTID TILL DC21                
146500     MOVE 88              TO WS-TRANS-ADLAGOMR                            
146600     MOVE '21'            TO WS-IDDC-REC                                  
146700                             W-IDDC-TRANSF                                
146800     IF SLAG-ADLAGOMR = WS-TRANS-ADLAGOMR                                 
146900       IF SLAG-KVLS > ZERO                                                
147000         PERFORM IMS-GU-WDK601                                            
147010         IF SEGMENT-FINNS                                                 
147100           PERFORM IMS-GNP-WDK611                                         
147200           IF SEGMENT-FINNS                                               
147300****KOLLAR   OM GODKÄND TRANSFER VÄG SE BILD 2349                         
147400             PERFORM DB2-SELECT-TP4TRAN                                   
147500             IF LINES-FOUND                                               
147600****HÄMTAR   ARTIKELINFO PÅ MOTTAGANDE DC (DC21)                          
147700               PERFORM IMS-GU-WDK711-TRANSF                               
147800               IF SEGMENT-FINNS                                           
147900                 CONTINUE                                                 
148000               ELSE                                                       
148100                 MOVE ALL '+'      TO WDK7-W005WDK7                       
148200                 MOVE 'WDK711'     TO WDK7-IDSEGM                         
148300                 MOVE W-IDARTNR    TO WDK7-IDARTNR-KFB                    
148400                 MOVE W-IDDC-TRANSF TO WDK7-IDDC-KFB                      
148500                                        WDK7-IDDC                         
148600                 PERFORM S01-INDATA-WDK711                                
148700                                                                          
148800                 CALL W005WDK7 USING WDK7-W005WDK7 WDB6-PCB               
148900                                                   WDK6-PCB               
149000                                                   WDK7I-PCB              
149100                 MOVE WDK7-WDK711 TO TRANSF-SLAG-WDK711                   
149200                 ADD +1 TO CHKP-ANT                                       
149300               END-IF                                                     
149400*                                                                         
149500               MOVE W-IDARTNR             TO UT-IDARTNR                   
149600               MOVE WS-IDDC-REC           TO UT-IDDC                      
149700               MOVE SLAG-KVLS             TO UT-KVBEART                   
149800               MOVE TRANSF-SLAG-IDLEVNR   TO UT-IDLEVNR                   
149900               MOVE TRANSF-SLAG-IDPERSON-BUY TO UT-IDPERSON-BUY           
150000               MOVE TRANSF-SLAG-ADLAGOMR  TO UT-ADLAGOMR-SDC              
150100               MOVE TRANSF-SLAG-ADGANG    TO UT-ADGANG-SDC                
150200               MOVE TRANSF-SLAG-ADPLATS   TO UT-ADPLATS-SDC               
150300               MOVE CLAG-ADLAGOMR         TO UT-ADLAGOMR-CDC              
150400               MOVE CLAG-ADGANG           TO UT-ADGANG-CDC                
150500               MOVE CLAG-ADPLATS          TO UT-ADPLATS-CDC               
150600               MOVE 'T'                   TO UT-KDREFTYP                  
150700               MOVE 'O'                   TO UT-KDREFORS                  
150800               MOVE TP4TRAN-IDDISTR       TO UT-IDDISTR                   
150900               MOVE TP4TRAN-IDKUNDNR      TO UT-IDKUNDNR                  
150910               MOVE TP4TRAN-IDDC-SEND     TO UT-IDDC-REF                  
151000               MOVE ZERO                  TO UT-ADLAGOMR-CD               
151100                                               UT-ADGANG-CD               
151200                                               UT-ADPLATS-CD              
151300                                               UT-KVBEART-CD              
151400                                               UT-KDREFTXT                
151500                                               UT-KDFRAKT                 
151600               PERFORM S11-SKRIV-W27117                                   
151700             END-IF                                                       
151800           ELSE                                                           
151900             DISPLAY 'ARTIKEL SAKNAS PÅ WDK6'                             
152000             CALL FELLOG                                                  
152100           END-IF                                                         
152110         END-IF                                                           
152200       END-IF                                                             
152300     END-IF                                                               
152400     .                                                                    
152500     EJECT                                                                
152600 F-CHECK-AREA88-TRANSF-DAY SECTION.                                       
152700                                                                          
152800     MOVE ZERO TO WS-FLTRLO88-MAN-NUM                                     
152900                  WS-FLTRLO88-TIS-NUM                                     
153000                  WS-FLTRLO88-ONS-NUM                                     
153100                  WS-FLTRLO88-TOR-NUM                                     
153200                  WS-FLTRLO88-FRE-NUM                                     
153300                                                                          
153400     IF TAB-FLTRLO88-MAN(WS-IDDC-IX) = 'Y'                                
153500        MOVE 7         TO WS-FLTRLO88-MAN-NUM                             
153600     END-IF                                                               
153700     IF TAB-FLTRLO88-TIS(WS-IDDC-IX) = 'Y'                                
153800        MOVE 2         TO WS-FLTRLO88-TIS-NUM                             
153900     END-IF                                                               
154000     IF TAB-FLTRLO88-ONS(WS-IDDC-IX) = 'Y'                                
154100        MOVE 3         TO WS-FLTRLO88-ONS-NUM                             
154200     END-IF                                                               
154300     IF TAB-FLTRLO88-TOR(WS-IDDC-IX) = 'Y'                                
154400        MOVE 4         TO WS-FLTRLO88-TOR-NUM                             
154500     END-IF                                                               
154600     IF TAB-FLTRLO88-FRE(WS-IDDC-IX) = 'Y'                                
154700        MOVE 5         TO WS-FLTRLO88-FRE-NUM                             
154800     END-IF                                                               
154900                                                                          
155000* CHECK IF TRANSFER DAY IS TODAY                                          
155100     MOVE 'N'                   TO SW-AREA88-TRANSF                       
155200                                                                          
155300     IF WS-FLTRLO88-MAN-NUM     =  DAGENS-TIAAVVD-D                       
155400        MOVE 'Y'                TO SW-AREA88-TRANSF                       
155500     ELSE                                                                 
155600      IF WS-FLTRLO88-TIS-NUM    =  DAGENS-TIAAVVD-D                       
155700         MOVE 'Y'               TO SW-AREA88-TRANSF                       
155800      ELSE                                                                
155900       IF WS-FLTRLO88-ONS-NUM   =  DAGENS-TIAAVVD-D                       
156000          MOVE 'Y'              TO SW-AREA88-TRANSF                       
156100       ELSE                                                               
156200        IF WS-FLTRLO88-TOR-NUM  =  DAGENS-TIAAVVD-D                       
156300           MOVE 'Y'             TO SW-AREA88-TRANSF                       
156400        ELSE                                                              
156500         IF WS-FLTRLO88-FRE-NUM =  DAGENS-TIAAVVD-D                       
156600            MOVE 'Y'            TO SW-AREA88-TRANSF                       
156700         END-IF                                                           
156800        END-IF                                                            
156900       END-IF                                                             
157000      END-IF                                                              
157100     END-IF                                                               
157200     .                                                                    
157300     EJECT                                                                
157400                                                                          
157500 Z-FINIT SECTION.                                                         
157600     CLOSE W27117 W27116                                                  
157700     SKIP2                                                                
157800     MOVE 'S' TO POSTSUM-OPKOD                                            
157900     CALL POSTSUM USING POSTSUM-PARM                                      
158000     .                                                                    
158100     EJECT                                                                
158200                                                                          
158300 S01-INDATA-WDK711 SECTION.                                               
158400     MOVE 'A'             TO WDK7-KDREFSTA                                
158500     MOVE DAGENS-DATUM    TO WDK7-TIREFSTA                                
158600                             WDK7-TIREFMPB                                
158700     COMPUTE WS-PROGNOS = CLAG-KVPB-SEP * 0.73                            
158800     EVALUATE TRUE                                                        
158900        WHEN SDC-NL                                                       
159000           COMPUTE WDK7-KVPB-REF = WS-PROGNOS * WS-OMRAKTAL-21            
159100        WHEN LDC-GB-3A                                                    
159200           COMPUTE WDK7-KVPB-REF = WS-PROGNOS * WS-OMRAKTAL-3A            
159300        WHEN SDC-ES                                                       
159400           COMPUTE WDK7-KVPB-REF = WS-PROGNOS * WS-OMRAKTAL-24            
159500        WHEN SDC-IT                                                       
159600           COMPUTE WDK7-KVPB-REF = WS-PROGNOS * WS-OMRAKTAL-25            
159700        WHEN SDC-AT                                                       
159800           COMPUTE WDK7-KVPB-REF = WS-PROGNOS * WS-OMRAKTAL-26            
159900     END-EVALUATE                                                         
160000     .                                                                    
160100     EJECT                                                                
160200                                                                          
160300 S11-SKRIV-W27117 SECTION.                                                
160400     WRITE UT-POST FROM UT-AREA                                           
160500                                                                          
160600     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
160700     MOVE 'W27117' TO POSTSUM-FDNAMN                                      
160800     MOVE 'W27116D1' TO POSTSUM-DDNAMN2                                   
160900     CALL POSTSUM USING POSTSUM-PARM                                      
161000     .                                                                    
161100     EJECT                                                                
161200 S12-SKRIV-W27116 SECTION.                                                
161300     WRITE UT98-POST FROM UT98-AREA                                       
161400                                                                          
161500     MOVE SPACE     TO POSTSUM-TRANSTYP                                   
161600     MOVE 'W27116' TO POSTSUM-FDNAMN                                      
161700     MOVE 'W27116D2' TO POSTSUM-DDNAMN2                                   
161800     CALL POSTSUM USING POSTSUM-PARM                                      
161900     .                                                                    
162000     EJECT                                                                
162100 S21-BERAKNA-30V-LAGERSALDO-CDC SECTION.                                  
162200     COMPUTE WS-KVPB-TOT-CDC = CLAG-KVPB-SEP                              
162300                             + CLAG-KVPB-SATS                             
162400                             + CLAG-KVPB-TPO                              
162500                                                                          
162600     PERFORM S21A-CALC-PROGNOS-REFILLED-XDC                               
162700                                                                          
162800     COMPUTE WS-KVPB-TOT-CDC-XDC                                          
162900                             = WS-KVPB-TOT-CDC                            
163000                             + WS-KVPB-TOT-XDC                            
163100                                                                          
163200     COMPUTE WS-KVPB-TOT-1V-SEND-DC                                       
163300                             = WS-KVPB-TOT-CDC-XDC / 4.33                 
163400                                                                          
163500     COMPUTE WS-KVPB-TOT-30V-SEND-DC ROUNDED =                            
163600                                 WS-KVPB-TOT-1V-SEND-DC * 30              
163700     .                                                                    
163800     EJECT                                                                
163900                                                                          
164000 S21A-CALC-PROGNOS-REFILLED-XDC SECTION.                                  
164100*  ----  CALC TOTAL FORECAST OF DCS REFILLED FROM CDC                     
164200*  ----  FOR FORECAST OF ALL DC CALL W271UTIL WITH KDCAL 002              
164300                                                                          
164400     INITIALIZE UTIL-W271UTIL                                             
164500     MOVE ALL ZERO              TO WS-KVPB-TOT-XDC                        
164600     MOVE 002                   TO UTIL-KDCALL                            
164700     MOVE W-IDARTNR             TO UTIL-IDARTNR                           
164800     MOVE W-IDDC                TO UTIL-IDDC                              
164900     MOVE SLAG-IDDC-REF         TO UTIL-IDDC-REF                          
165000                                                                          
165100     CALL W271UTIL USING UTIL-W271UTIL                                    
165200                         UTIL-WDK6-PCB                                    
165300                         UTIL-WDK7-PCB                                    
165400                         UTIL-WDB6-PCB                                    
165500                                                                          
165600     IF UTIL-KDSVAR-OK                                                    
165700        MOVE UTIL-KVPB-TOT      TO WS-KVPB-TOT-XDC                        
165800     ELSE                                                                 
165900        MOVE 'FEL FRÅN W271UTIL '                                         
166000                                TO FELTEXT-STR                            
166100        DISPLAY FELTEXT                                                   
166200        PERFORM S99-ABEND                                                 
166300     END-IF                                                               
166310     ADD +1     TO IX-UTIL                                                
166400     .                                                                    
166500     EJECT                                                                
166600                                                                          
166700 S21-BERAKNA-30V-LAGERSALDO-NDC SECTION.                                  
166800     COMPUTE WS-KVPB-TOT-1V-SEND-DC = (INT-SLAG-KVPB-REF +                
166900                                      INT-SLAG-KVPBREOI) / 4.33           
167000                                                                          
167100     COMPUTE WS-KVPB-TOT-30V-SEND-DC ROUNDED =                            
167200                                 WS-KVPB-TOT-1V-SEND-DC * 30              
167300     .                                                                    
167400     EJECT                                                                
167500                                                                          
167600 S22-BERAKNA-6MANADER-SEDAN SECTION.                                      
167700     IF SLAG-IDDC-REF = '11'                                              
167800       MOVE ART-TIFINLV TO WS-TIFINLV                                     
167900                           ARTWS-TIFINLV                                  
168000     ELSE                                                                 
168100********LÄS WDK712 OCH GÖR OM AAAAMMDD TILL AAVVD                         
168200       MOVE TAB-IDLANDX2(WS-IDDC-IX) TO W-IDLAND                          
168300       PERFORM IMS-GU-WDK712                                              
168400       IF SEGMENT-FINNS                                                   
168500          IF LART-DAPUBL > ZERO                                           
168600             MOVE 'AAMMDD'          TO DAT-KDDATFORM                      
168700             MOVE LART-DAPUBL       TO DAT-I-TIDATUM                      
168800             CALL WDATKONV USING       DAT-KDDATFORM                      
168900                                   DAT-I-TIDATUM                          
169000                                   DAT-O-TIDATUM                          
169100                                   DAT-KDSVAR                             
169200                                                                          
169300             IF DAT-KDSVAR-OK                                             
169400                MOVE DAT-TIAAVVD TO WS-TIFINLV                            
169500                                    ARTWS-TIFINLV                         
169600             ELSE                                                         
169700                MOVE 'FEL FRÅN WDATKONV I S22-' TO                        
169800                                      FELTEXT-STR                         
169900                DISPLAY FELTEXT                                           
170000                PERFORM S99-ABEND                                         
170100             END-IF                                                       
170200          ELSE                                                            
170300            MOVE ART-TIFINLV TO WS-TIFINLV                                
170400                                ARTWS-TIFINLV                             
170500          END-IF                                                          
170600       ELSE                                                               
170700         MOVE ART-TIFINLV TO WS-TIFINLV                                   
170800                             ARTWS-TIFINLV                                
170900       END-IF                                                             
171000     END-IF                                                               
171100                                                                          
171200     IF ARTWS-TIFINLV(1:2) > 50                                           
171300       COMPUTE WS-TIFINLV = WS-TIFINLV + 1900000                          
171400     ELSE                                                                 
171500       COMPUTE WS-TIFINLV = WS-TIFINLV + 2000000                          
171600     END-IF                                                               
171700     DIVIDE WS-TIFINLV BY 10 GIVING WS-TIFINLV-AAAAVV                     
171800                                                                          
171900                                                                          
172000     MOVE DAGENS-TIAAVVD (1:4) TO VADD-DATUM-AAVV                         
172100     MOVE -26                  TO VADD-ANTAL                              
172200     CALL W009VADD USING VADD-DATUM-AAVV VADD-ANTAL                       
172300                                                                          
172400     MOVE VADD-DATUM-AAVV      TO WS-DAGENS-AAVV-MINUS-26V                
172500     IF DAGENS-TIAAVVD-AA > 50                                            
172600       COMPUTE WS-DAGENS-AAAAVV-MINUS-26V =                               
172700               WS-DAGENS-AAVV-MINUS-26V + 190000                          
172800     ELSE                                                                 
172900       COMPUTE WS-DAGENS-AAAAVV-MINUS-26V =                               
173000               WS-DAGENS-AAVV-MINUS-26V + 200000                          
173100     END-IF                                                               
173200     .                                                                    
173300     EJECT                                                                
173400                                                                          
173500 S23-BERAKNA-MINUS-90DAGAR SECTION.                                       
173600     MOVE DAGENS-DATUM   TO WORK-TIAAMMDD-TOM                             
173700     MOVE 3              TO WORK-KDCALL                                   
173800**** 65 ARBETSDAGR MOTSVARAR CA 90 KALENDERDAGAR                          
173900     MOVE 65             TO WORK-KVWORKD                                  
174000     MOVE W-IDDC         TO WORK-IDDC                                     
174100                                                                          
174200     CALL WORKDAY USING                                                   
174300          WORK-KDCALL                                                     
174400          WORK-DATE-AREA                                                  
174500          WORK-KDSVAR                                                     
174600                                                                          
174700     IF WORK-KDSVAR-OK                                                    
174800        MOVE WORK-TIAAMMDD-FOM TO DAGENS-AAMMDD-MINUS-90DAG               
174900     ELSE                                                                 
175000       DISPLAY '*** S23 W27116, FEL I WORKDAY '                           
175100       CALL FELLOG                                                        
175200     END-IF                                                               
175300     .                                                                    
175400     EJECT                                                                
175500                                                                          
175600 S24-BERAKNA-MINUS-365DAG  SECTION.                                       
175700     MOVE DAGENS-DATUM   TO DAGENS-AAMMDD-MINUS-365DAG                    
175800     COMPUTE DAGENS-AAMMDD-MINUS-365DAG =                                 
175900             DAGENS-AAMMDD-MINUS-365DAG - 10000                           
176000     COMPUTE DAGENS-AAAAMMDD-MINUS-365DAG =                               
176100             20000000 + DAGENS-AAMMDD-MINUS-365DAG                        
176200     .                                                                    
176300     EJECT                                                                
176400                                                                          
176500 S25-BERAKNA-OVERLAGER SECTION.                                           
176600     MOVE ZERO                 TO   W-TILLG-SDC                           
176700                                    W-OVERLAGER-SDC                       
176800     COMPUTE W-TILLG-SDC = SLAG-KVLS                                      
176900                         + SLAG-KVBEART                                   
177000                         + SLAG-KVAKS-SDC                                 
177100                         + SLAG-KVAKS-PAV                                 
177200                         - SLAG-KVOKS-BULK                                
177300                         - SLAG-KVOKS-DAG                                 
177400                         - SLAG-KVRESS                                    
177500                                                                          
177600     .                                                                    
177700     EJECT                                                                
177800                                                                          
177900 S26-KVQPACK-1 SECTION.                                                   
178000     MOVE TAB-REQXBRYT(WS-IDDC-IX) TO WS-QX-BRYTNING                      
178100     COMPUTE WS-KVANT-QX =                                                
178200             WS-ANTAL-KVAR / CLAG-KVQPACK-1                               
178300     IF WS-KVANT-QX-DECTAL < WS-QX-BRYTNING                               
178400       IF WS-KVANT-QX-HELTAL < 1                                          
178500         COMPUTE WS-ANTAL-QX = 1 * CLAG-KVQPACK-1                         
178600       ELSE                                                               
178700         COMPUTE WS-ANTAL-QX =                                            
178800                 WS-KVANT-QX-HELTAL * CLAG-KVQPACK-1                      
178900       END-IF                                                             
179000     ELSE                                                                 
179100       COMPUTE WS-KVANT-QX-HELTAL =                                       
179200               WS-KVANT-QX-HELTAL + 1                                     
179300       COMPUTE WS-ANTAL-QX =                                              
179400               WS-KVANT-QX-HELTAL * CLAG-KVQPACK-1                        
179500     END-IF                                                               
179600     .                                                                    
179700     EJECT                                                                
179800                                                                          
179900 S27-KOLLA-SEND-DC SECTION.                                               
180000                                                                          
180100     MOVE SLAG-IDDC-REF    TO REF-WS-IDDC                                 
180200                                                                          
180300*    RETURNS BETWEEN US AND CN NOT ALLOWED                                
180400*                                                                         
180500     IF (NDC-CN AND REF-NDC-US)                                           
180600     OR (NDC-US AND REF-NDC-CN)                                           
180700        MOVE NEJ           TO BEHANDLA-SW                                 
180800     ELSE                                                                 
180900        MOVE SLAG-IDDC-REF TO W-IDDC-INT                                  
181000*       PERFORM IMS-GU-WDB616                                             
181100*       IF SEGMENT-FINNS                                                  
181200          PERFORM IMS-GU-WDK711-INTERN                                    
181300          IF SEGMENT-SAKNAS                                               
181400            MOVE NEJ TO BEHANDLA-SW                                       
181500          END-IF                                                          
181600*       ELSE                                                              
181700*         MOVE NEJ TO BEHANDLA-SW                                         
181800*       END-IF                                                            
181900     END-IF                                                               
182000     .                                                                    
182100     EJECT                                                                
182200                                                                          
182300 S28-KOLLA-SENASTE-INL SECTION.                                           
182400                                                                          
182500     MOVE ZERO                         TO WS-TIINLINL                     
182600     PERFORM IMS-GU-WDL601                                                
182700     IF SEGMENT-FINNS                                                     
182800       PERFORM IMS-GNP-WDL611                                             
182900       PERFORM UNTIL SEGMENT-SAKNAS                                       
183000         IF SLAG-IDDC-REF = INL-IDDC                                      
183100           MOVE INL-TIINLINL                                              
183200                         TO TMP1-YYMMDD                                   
183300           MOVE WS-TIINLINL                                               
183400                         TO TMP2-YYMMDD                                   
183500           PERFORM WY2000P1                                               
183600           IF TMP1-YYMMDD > TMP2-YYMMDD                                   
183700             MOVE INL-TIINLINL                                            
183800                         TO WS-TIINLINL                                   
183900           END-IF                                                         
184000         END-IF                                                           
184100         PERFORM IMS-GNP-WDL611                                           
184200       END-PERFORM                                                        
184300     END-IF                                                               
184400     .                                                                    
184500     EJECT                                                                
184600                                                                          
184700 S29-GET-LSLSDTE SECTION.                                                 
184800                                                                          
184900     MOVE ALL ZERO         TO WS-TIREFEFT-6                               
185000                                                                          
185100     PERFORM IMS-GU-WDL711                                                
185200*                                                                         
185300     IF SEGMENT-SAKNAS                                                    
185400        MOVE +000000       TO DC-TIREFEFT                                 
185500     END-IF                                                               
185600     MOVE DC-TIREFEFT      TO WS-TIREFEFT-6                               
185700     .                                                                    
185800     EJECT                                                                
185900                                                                          
186000 S99-ABEND SECTION.                                                       
186100     SKIP2                                                                
186200     MOVE 'S' TO POSTSUM-OPKOD                                            
186300     CALL POSTSUM USING POSTSUM-PARM                                      
186400     CALL ABEND USING RKOD-ABEND                                          
186500     .                                                                    
186600* --- IMS SEKTIONER ---                                                   
186700                                                                          
186800 IMS-GN-WDB601    SECTION.                                                
186900     MOVE 'WDB601  ' TO SSA1                                              
187000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
187100     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1                 
187200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
187300     PERFORM IMS-STATUSKONTROLL                                           
187400     .                                                                    
187500     EJECT                                                                
187600 IMS-GU-WDB616    SECTION.                                                
187700     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
187800          DELIMITED BY SIZE INTO SSA1                                     
187900     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
188000          DELIMITED BY SIZE INTO SSA2                                     
188100     MOVE '  ' TO GODK-STATUSKODER                                        
188200     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
188300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
188400     PERFORM IMS-STATUSKONTROLL                                           
188500     .                                                                    
188600     EJECT                                                                
188700 IMS-GU-WDK711-INTERN SECTION.                                            
188800                                                                          
188900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
189000          DELIMITED BY SIZE INTO SSA1                                     
189100     STRING 'WDK711  (IDDC     =' W-IDDC-INT-X ')'                        
189200          DELIMITED BY SIZE INTO SSA2                                     
189300     MOVE '  GE' TO GODK-STATUSKODER                                      
189400     CALL CBLTDLI USING GU WDK7I-PCB DLI-IO-WDK711-INTERN                 
189500          SSA1 SSA2                                                       
189600     MOVE WDK7I-STATUS-CODE TO STATUS-WS                                  
189700     PERFORM IMS-STATUSKONTROLL                                           
189800     .                                                                    
189900     EJECT                                                                
190000                                                                          
190100 IMS-GU-WDK712 SECTION.                                                   
190200                                                                          
190300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
190400          DELIMITED BY SIZE INTO SSA1                                     
190500     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
190600          DELIMITED BY SIZE INTO SSA2                                     
190700     MOVE '  GE' TO GODK-STATUSKODER                                      
190800     CALL CBLTDLI USING GU WDK7I-PCB DLI-IO-WDK712                        
190900          SSA1 SSA2                                                       
191000     MOVE WDK7I-STATUS-CODE TO STATUS-WS                                  
191100     PERFORM IMS-STATUSKONTROLL                                           
191200     .                                                                    
191300     EJECT                                                                
191400                                                                          
191500 IMS-GU-WDL711 SECTION.                                                   
191600     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
191700          DELIMITED BY SIZE INTO SSA1                                     
191800     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
191900          DELIMITED BY SIZE INTO SSA2                                     
192000     MOVE '  GE' TO GODK-STATUSKODER                                      
192100     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-WDL711 SSA1 SSA2               
192200     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
192300     PERFORM IMS-STATUSKONTROLL                                           
192400     .                                                                    
192500     SKIP3                                                                
192600 IMS-GU-WDK601 SECTION.                                                   
192700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
192800          DELIMITED BY SIZE INTO SSA1                                     
192900     MOVE '  GE' TO GODK-STATUSKODER                                      
193000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
193100     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
193200     PERFORM IMS-STATUSKONTROLL                                           
193300     .                                                                    
193400     EJECT                                                                
193500                                                                          
193600                                                                          
193700 IMS-GNP-WDK611 SECTION.                                                  
193800     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY ')'                          
193900          DELIMITED BY SIZE INTO SSA1                                     
194000     MOVE '  GE' TO GODK-STATUSKODER                                      
194100     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK611 SSA1              
194200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
194300     PERFORM IMS-STATUSKONTROLL                                           
194310     ADD +1    TO IX-K611                                                 
194400     .                                                                    
194500     EJECT                                                                
194600                                                                          
194700 IMS-GN-WDK7 SECTION.                                                     
194800     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-AREA-K7                        
194900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
195000     MOVE '  GEGBGAGK' TO GODK-STATUSKODER                                
195100     PERFORM IMS-STATUSKONTROLL                                           
195200     .                                                                    
195300     EJECT                                                                
195400                                                                          
195500 IMS-GU-WDK711-TRANSF SECTION.                                            
195600     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
195700          DELIMITED BY SIZE INTO SSA1                                     
195800     STRING 'WDK711  (IDDC     =' W-IDDC-TRANSF-X ')'                     
195900          DELIMITED BY SIZE INTO SSA2                                     
196000     MOVE '  GE' TO GODK-STATUSKODER                                      
196100     CALL CBLTDLI USING GU WDK7I-PCB DLI-IO-WDK711-TRANSF                 
196200          SSA1 SSA2                                                       
196300     MOVE WDK7I-STATUS-CODE TO STATUS-WS                                  
196400     PERFORM IMS-STATUSKONTROLL                                           
196500     .                                                                    
196600     EJECT                                                                
196700 IMS-GU-WDL601 SECTION.                                                   
196800     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
196900          DELIMITED BY SIZE INTO SSA1                                     
197000     MOVE '  GE' TO GODK-STATUSKODER                                      
197100     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-AREA-WDL601 SSA1               
197200     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
197300     PERFORM IMS-STATUSKONTROLL                                           
197400     .                                                                    
197500     SKIP2                                                                
197600 IMS-GNP-WDL611 SECTION.                                                  
197810     MOVE 'WDL611  '         TO SSA1                                      
197900     MOVE '  GE' TO GODK-STATUSKODER                                      
198000     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-AREA-WDL611 SSA1              
198100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
198200     PERFORM IMS-STATUSKONTROLL                                           
198210     ADD +1    TO IX-L611                                                 
198300     .                                                                    
198400     SKIP2                                                                
198500 IMS-GU-WDQ4B1 SECTION.                                                   
198600     STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
198700                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
198800                    '&IDDISTR  =' W-IDDISTR-Q4B1-X                        
198900                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-X                       
199000                    '!WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
199100                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
199200                    '&IDDISTR  =' W-IDDISTR-Q4B1-Q-X                      
199300                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-Q-X                     
199400                    '!WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
199500                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
199600                    '&IDDISTR  =' W-IDDISTR-Q4B1-R-X                      
199700                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-R-X ')'                 
199800                                                                          
199900          DELIMITED BY SIZE INTO SSA1                                     
200000     MOVE '  GE' TO GODK-STATUSKODER                                      
200100     CALL CBLTDLI USING GU WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
200200     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
200300     PERFORM IMS-STATUSKONTROLL                                           
200400     .                                                                    
200500     SKIP3                                                                
200600 IMS-GN-WDQ4B1 SECTION.                                                   
200700     STRING 'WDQ4B1  (WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
200800                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
200900                    '&IDDISTR  =' W-IDDISTR-Q4B1-X                        
201000                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-X                       
201100                    '!WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
201200                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
201300                    '&IDDISTR  =' W-IDDISTR-Q4B1-Q-X                      
201400                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-Q-X                     
201500                    '!WDQ4B1KY>=' W-WDQ4B1KY-MIN-X                        
201600                    '&WDQ4B1KY<=' W-WDQ4B1KY-MAX-X                        
201700                    '&IDDISTR  =' W-IDDISTR-Q4B1-R-X                      
201800                    '&IDKUNDNR =' W-IDKUNDNR-Q4B1-R-X ')'                 
201900                                                                          
202000                                                                          
202100          DELIMITED BY SIZE INTO SSA1                                     
202200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
202300     CALL CBLTDLI USING GN WDQ4B-PCB DLI-IO-WDQ4B1 SSA1                   
202400     MOVE WDQ4B-STATUS-CODE TO STATUS-WS                                  
202500     PERFORM IMS-STATUSKONTROLL                                           
202600     .                                                                    
202700     SKIP3                                                                
202800     EJECT                                                                
202900                                                                          
203000 DB2-SELECT-TP1KAMP-TP1ARTK  SECTION.                                     
203100     MOVE 000100811  TO GOOD-SQLCODECODES                                 
203200                                                                          
203300     EXEC SQL                                                             
203400           SELECT  K.IDKAMP                                               
203500                                                                          
203600           INTO   :TP1KAMP-IDKAMP                                         
203700                                                                          
203800           FROM    TP1KAMP K                                              
203900                  ,TP1ARTK A                                              
204000           WHERE K.IDKAMP = A.IDKAMP                                      
204100             AND IDARTNR  = :TP1ARTK-IDARTNR                              
204200             AND (  (   TISTODAT_KAMP < 990000                            
204300                    AND TISTODAT_KAMP >                                   
204400                          INT(REPLACE(SUBSTR(CHAR(CURRENT_DATE)           
204500                                             ,3,8),'-',''))               
204600                    )                                                     
204700                 OR (   TISTODAT_KAMP = 0                                 
204800                    AND TISTADAT_KAMP < 940000                            
204900                    AND TISTADAT_KAMP +  50000 >                          
205000                          INT(REPLACE(SUBSTR(CHAR(CURRENT_DATE)           
205100                                             ,3,8),'-',''))               
205200                    )                                                     
205300                 )                                                        
205400     END-EXEC                                                             
205500                                                                          
205600     MOVE SQLCODE TO SQLCODE-WS                                           
205700     PERFORM DB2-STATUS-CHECK                                             
205800     .                                                                    
205900     EJECT                                                                
206000                                                                          
206100 DB2-SELECT-TP4TRAN     SECTION.                                          
206200     MOVE 000100  TO GOOD-SQLCODECODES                                    
206300                                                                          
206400     EXEC SQL                                                             
206500           SELECT  IDDC_SEND                                              
206600                  ,IDDC_REC                                               
206700                  ,IDDISTR                                                
206800                  ,IDKUNDNR                                               
206900                                                                          
207000           INTO   :TP4TRAN-IDDC-SEND                                      
207100                 ,:TP4TRAN-IDDC-REC                                       
207200                 ,:TP4TRAN-IDDISTR                                        
207300                 ,:TP4TRAN-IDKUNDNR                                       
207400                                                                          
207500           FROM    TP4TRAN                                                
207600                                                                          
207700           WHERE IDDC_SEND = :WS-IDDC-SEND                                
207800           AND   IDDC_REC  = :WS-IDDC-REC                                 
207900           AND   KDARBTYP  = 'ESC'                                        
208000     END-EXEC                                                             
208100                                                                          
208200     MOVE SQLCODE TO SQLCODE-WS                                           
208300     PERFORM DB2-STATUS-CHECK                                             
208400     .                                                                    
208500     EJECT                                                                
208600                                                                          
208700 DB2-STATUS-CHECK  SECTION.                                               
208800     SET SQLCODE-IX TO 1                                                  
208900     SEARCH GOOD-SQLCODE                                                  
209000       AT END                                                             
209100          CALL ABEND USING RKOD-ABEND-DB2                                 
209200       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
209300     END-SEARCH                                                           
209400     .                                                                    
209500     EJECT                                                                
209600                                                                          
209700 IMS-STATUSKONTROLL SECTION.                                              
209800     SET STATUS-IX TO 1                                                   
209900     SEARCH GODK-STATUS                                                   
210000       AT END                                                             
210100         MOVE 'XXXXXXXXXX' TO FELTEXT-STR                                 
210200         DISPLAY FELTEXT                                                  
210300         CALL FELLOG                                                      
210400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
210500         CONTINUE                                                         
210600     END-SEARCH                                                           
210700     .                                                                    
210800     EJECT                                                                
210900*    -COPY WY2000P1                                                       
211000                                                                          
