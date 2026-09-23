000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W3018310.                                                
000400 AUTHOR.         INGVAR SKJELBRED.                                        
000500 DATE-WRITTEN.   96/09/26.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        SKAPA PROFORMAFAKTUROR FÖR ATT TRANSPORTERA OBJEKT               
001000*        MELLAN NDC:ER OCH MAASTRICHT                                     
001100*                                                                         
001200*        PROGRAMMET LÄSER      TABELL BYART                               
001300*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001400*        PROGRAMMET LÄSER      WLARTS (WDK7)                              
001500*        PROGRAMMET LÄSER      WLXXLD                                     
001600*        PROGRAMMET LÄSER      WL3165 (HÄNDELSEDATABASEN)                 
001700*        PROGRAMMET LOGGAR SALDO FÖRÄNDRINGAR PÅ                          
001800*                              WLLOGA (WDL9)                              
001900*                                                                         
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSAKTION: W3T183                                              
002300*        MID:         W3I18301                                            
002400*                                                                         
002500*    UTDATA.                                                              
002600*        MOD:         W3O18301                                            
002700*                                                                         
002800*    CHANGE LOG:                                                          
002900*      14/04/24 - REDDY RAHUL     - NEW PROG WITH BUSINES LOGIC           
003000*                                   COMMON FOR CLASSIC (W3018300)         
003100*                                   AND WEB (W3W18300) INTERFACE.         
003200*                                   ETRACKER 10228590 - TORONTO.          
003300*                                                                         
003400                                                                          
003500     SKIP3                                                                
003600 ENVIRONMENT DIVISION.                                                    
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900 WORKING-STORAGE SECTION.                                                 
004000*    -- CHECKED BY WY2000                                                 
004100     SKIP3                                                                
004200 77  IDPGM                       PIC X(08)     VALUE 'W3018310'.          
004300                                                                          
004400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004500 77  FELTEXT                     PIC X(80)     VALUE SPACE.               
004600                                                                          
004700 77  FELTEXT2                    PIC X(80)     VALUE SPACE.               
004800                                                                          
004900 77  JA                          PIC X         VALUE 'J'.                 
005000 77  NEJ                         PIC X         VALUE 'N'.                 
005100 01  ALL-SPACE.                                                           
005200     03 FILLER                   PIC X(50)     VALUE SPACE.               
005300 01  ALL-PLUS.                                                            
005400     03 FILLER                   PIC X(50)     VALUE ALL '+'.             
005500 01  ALL-UTF8-SPACE.                                                      
005600     03 FILLER                   PIC X(50)     VALUE ALL X'20'.           
005700 01  ALL-UTF8-PLUS.                                                       
005800     03 FILLER                   PIC X(50)     VALUE ALL X'2B'.           
005900                                                                          
006000 77  UPPDATE-DONE                PIC X         VALUE 'N'.                 
006100                                                                          
006200 77  WS-VKORDBTO                 PIC 9(6)V9(1) VALUE ZERO.                
006300 77  WS-VLORDBTO                 PIC 9(5)V9(3) VALUE ZERO.                
006400 77  WS-VLORDBTO-INP             PIC 9(6)V9(1) VALUE ZERO.                
006500                                                                          
006600 77  WS-VKORDBTO-UTAN-DEC        PIC 9(6)      VALUE ZERO.                
006700 77  WS-VLORDBTO-UTAN-DEC        PIC 9(6)      VALUE ZERO.                
006800                                                                          
006900 77  ARTIKEL-SW                  PIC X         VALUE 'N'.                 
007000     88  ARTIKEL-SAKNAS                        VALUE 'N'.                 
007100     88  ARTIKEL-FINNS                         VALUE 'J'.                 
007200                                                                          
007300 77  TABRAD-SW                   PIC X         VALUE 'N'.                 
007400     88  TABRAD-SAKNAS                         VALUE 'N'.                 
007500     88  TABRAD-FINNS                          VALUE 'J'.                 
007600                                                                          
007700 77  KOLLI-SW                    PIC X         VALUE 'N'.                 
007800     88  KOLLI-SAKNAS                          VALUE 'N'.                 
007900     88  KOLLI-FINNS                           VALUE 'J'.                 
008000                                                                          
008100 77  FAKTURA-SW                  PIC X         VALUE 'N'.                 
008200     88  FAKTURA-EJ-SKRIVEN                    VALUE 'N'.                 
008300     88  FAKTURA-SKRIVEN                       VALUE 'J'.                 
008400                                                                          
008500 77  LAGERSALDO-SW               PIC X         VALUE 'J'.                 
008600     88  LAGERSALDO-OK                         VALUE 'J'.                 
008700     88  LAGERSALDO-FEL                        VALUE 'N'.                 
008800                                                                          
008900 77  INDATA-SW                   PIC X         VALUE 'J'.                 
009000     88  INDATA-OK                             VALUE 'J'.                 
009100     88  INDATA-FEL                            VALUE 'N'.                 
009200                                                                          
009300*    --- INDEX FÖR BLÄDDRINGSRADER                                        
009400 77  INDX                        PIC S9(4) VALUE +0 COMP SYNC.            
009500                                                                          
009600*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
009700                                                                          
009800 77  SPAR-IDARTNR                PIC S9(9) VALUE ZERO.                    
009900                                                                          
010000 77  SPAR-KVLS                   PIC S9(7) VALUE ZERO COMP-3.             
010100                                                                          
010200 77  WS-KVLS                     PIC S9(7) VALUE ZERO COMP-3.             
010300                                                                          
010400 77  WS-KVLEVART                 PIC S9(7) VALUE ZERO COMP-3.             
010500                                                                          
010600 77  WS-KVRETUR                  PIC S9(7) VALUE ZERO COMP-3.             
010700                                                                          
010800 77  W-TOT-KVLEVART              PIC S9(7) VALUE ZERO COMP-3.             
010900                                                                          
011000 77  FEL-IDARTNR                 PIC S9(9) VALUE ZERO COMP-3.             
011100                                                                          
011200 77  NYCKLAR-SW                  PIC X     VALUE 'J'.                     
011300     88  NYCKLAR-OK                        VALUE 'J'.                     
011400     88  NYCKLAR-FEL                       VALUE 'N'.                     
011500                                                                          
011600 01  FILLER                      PIC X(16) VALUE 'WS-SEKTION'.            
011700 01  WS-SEKTION                  PIC X(30) VALUE SPACE.                   
011800 01  FILLER                      PIC X(16) VALUE 'WS-IMS-SEKTION'.        
011900 01  WS-IMS-SEKTION              PIC X(30) VALUE SPACE.                   
012000                                                                          
012100*                                                                         
012200 01  FILLER                      PIC X(16) VALUE 'INDEX TABELL'.          
012300 01  TABELL.                                                              
012400   03  IDARTNR-TABELL OCCURS 1000.                                        
012500     05  TAB-IDARTNR             PIC S9(9).                               
012600     05  TAB-KVLS                PIC S9(7).                               
012700*                                                                         
012800 01  TABINDX                     PIC S9(4) VALUE ZERO.                    
012900 01  WS-TABINDX                  PIC S9(4) VALUE ZERO COMP-3.             
013000*                                                                         
013100 01  FILLER                      PIC X(16) VALUE 'DATUM FÄLT'.            
013200 01  DAGENS-DATUM                PIC S9(6) VALUE ZERO.                    
013300 01  DAGENS-DATUM1  REDEFINES DAGENS-DATUM.                               
013400   03  WS-YY                     PIC 9(2).                                
013500   03  WS-MM                     PIC 9(2).                                
013600   03  WS-DD                     PIC 9(2).                                
013700 01  DAGENS-DATUM2               PIC 9(8)  VALUE ZERO.                    
013800 01  DAGENS-DATUM3  REDEFINES DAGENS-DATUM2.                              
013900   03  WS-SS                     PIC 9(2).                                
014000   03  WS-AA                     PIC 9(2).                                
014100   03  WS-MA                     PIC 9(2).                                
014200   03  WS-DA                     PIC 9(2).                                
014300 01    TRANS-TID                 PIC S9(9) VALUE ZERO.                    
014400 01    LOGG-DATUM                PIC S9(8) VALUE ZERO.                    
014500                                                                          
014600 01  FILLER                      PIC X(16) VALUE 'DB2-NYCKLAR'.           
014700*                                                                         
014800 77  W-IDARTNR-BYT               PIC S9(9) VALUE ZERO COMP-3.             
014900*                                                                         
015000 01  RED-IDBYTKOL                PIC  9(3) VALUE ZERO.                    
015100*                                                                         
015200 01  WS-IDBYTKOL                 PIC  X(3) VALUE SPACE.                   
015300*                                                                         
015400 01  WS-IDBYTKOL-1               PIC  9(3) VALUE ZERO.                    
015500*                                                                         
015600 01  WS-IDARTNR                  PIC  X(9) VALUE SPACE.                   
015700 01  WS-IDARTNR-NUM              PIC  9(9) VALUE ZERO.                    
015800*                                                                         
015900     EJECT                                                                
016000 01  FILLER                      PIC X(16) VALUE 'BYTES-ART'.             
016100 01  TEST-IDARTNR                PIC 9(9)  COMP-3.                        
016200*01  FILLER -COPY WWBYT03   -RED TEST-IDARTNR                             
016300                                                                          
016400     EJECT                                                                
016500*    --- VALID IDDC CODES                                                 
016600*                                                                         
016700 01  FILLER                      PIC X(16) VALUE 'IDDC CODES'.            
016800     SKIP3                                                                
016900*01 -COPY WWDC99                                                          
016910                                                                          
016920     EJECT                                                                
016930 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
016940*01  FILLER -COPY WWDIS134    -RED TEST-IDDISTR.                          
016950                                                                          
017000     SKIP3                                                                
017100 01  FILLER                      PIC X(16) VALUE 'WWOMVAND '.             
017200*   -COPY WWOMVAND                                                        
017300     SKIP3                                                                
017400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
017500 01  FILLER                   PIC X(16) VALUE 'GENERELLA-SUBPGM'.         
017600 01  GENERELLA-SUBPROGRAM.                                                
017700     03  WDECEDIT                PIC X(8)  VALUE 'WDECEDIT'.              
017800     03  CBLTDLI                 PIC X(8)  VALUE 'CBLTDLI '.              
017900     03  FELLOG                  PIC X(8)  VALUE 'FELLOG  '.              
018000     03  WTRAUTF8                PIC X(8)  VALUE 'WTRAUTF8'.              
018100     EJECT                                                                
018200*    --- PARAMETRAR TILL SUBPROGRAM WDECAREA                              
018300 01  FILLER                      PIC X(16) VALUE 'WDECAREA'.              
018400*01 -COPY WDECAREA                                                        
018500     SKIP3                                                                
018600*    --- PARAMETRAR TILL SUBPROGRAM WMSGSOP                               
018700 01  FILLER                      PIC X(16) VALUE 'WMSGSOP '.              
018800 01  PROG-TO-PROG-SW.                                                     
018900*03  -COPY WMSGSOP                                                        
019000     EJECT                                                                
019100 01  FILLER                      PIC X(16) VALUE 'MFS-AREA'.              
019200     SKIP3                                                                
019300*01  -COPY WMFSAREA                                                       
019400     EJECT                                                                
019500 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
019600*01  -COPY WTRAUTF8                                                       
019700                                                                          
019800 01  WS-IDSKYLT-SE               PIC X(3)  VALUE 'S  '.                   
019900 01  WS-IDSKYLT-GB               PIC X(3)  VALUE 'GB '.                   
020000 01  WS-IDSKYLT-CN               PIC X(3)  VALUE 'RCN'.                   
020100 01  WS-CP-UNICODE               PIC X(4)  VALUE 'UTF8'.                  
020200 01  WS-CP-EBCDIC                PIC X(3)  VALUE '278'.                   
020300                                                                          
020400     EJECT                                                                
020500 01  FILLER                      PIC X(16) VALUE 'MESSAGE-CODES'.         
020600 01  MESSAGE-CODES.                                                       
020700     03  ERR-CORR-HILITE-FLDS    PIC X(3)  VALUE '020'.                   
020800     03  INF-PRESS-PF11          PIC X(3)  VALUE '013'.                   
020900     03  ERR-INFO-MISSING        PIC X(3)  VALUE '025'.                   
021000     03  INF-FIRST-PAGE          PIC X(3)  VALUE '010'.                   
021100     03  ERR-UPPDATE             PIC X(3)  VALUE '007'.                   
021200     03  PF11-AND-NO-CHANGE      PIC X(3)  VALUE '014'.                   
021300     03  MISSING-PARTNO          PIC X(3)  VALUE '025'.                   
021400     03  ERR-NO-UPPDATE-DONE     PIC X(3)  VALUE '004'.                   
021500     03  INF-UPDATE-DONE         PIC X(3)  VALUE '001'.                   
021600     03  INF-MORE-INFO-EXISTS    PIC X(3)  VALUE '011'.                   
021700     03  THIS-IS-THE-LAST-PAGE   PIC X(3)  VALUE '012'.                   
021800     03  PRINTING-OK             PIC X(3)  VALUE '376'.                   
021900     03  WRONG-AMOUNT            PIC X(3)  VALUE '330'.                   
022000     03  NOT-A-CORE-NO           PIC X(3)  VALUE '023'.                   
022100     03  MISSING-INVOICE         PIC X(3)  VALUE '025'.                   
022200     03  PARTNO-IS-SCRAP         PIC X(3)  VALUE '413'.                   
022300     03  BASE-ON-HAND-TO-LOW     PIC X(3)  VALUE '414'.                   
022400     03  ERR-WRONG-KEY           PIC X(3)  VALUE '022'.                   
022500     03  USER-NOT-ALLOWED        PIC X(3)  VALUE '00A'.                   
022600     03  KOMP-KOLLI-UPPG         PIC X(3)  VALUE '226'.                   
022700     03  MISSING-KOLLI           PIC X(3)  VALUE '041'.                   
022800     03  PARTNO-MISSING          PIC X(3)  VALUE '025'.                   
022900     EJECT                                                                
023000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
023100*                                                                         
023200     EJECT                                                                
023300 01  FILLER                      PIC X(16) VALUE 'IMS-WS'.                
023400     SKIP3                                                                
023500 01  NYCKLAR-TILL-BLAEDDRING.                                             
023600     03  W-3168-MIN-X.                                                    
023700         05  W-IDBYTKOL-MIN      PIC 9(3)  VALUE ZERO.                    
023800         05  W-IDARTNR-MIN       PIC S9(9) VALUE ZERO COMP-3.             
023900     03  W-3168-MAX-X.                                                    
024000         05  W-IDBYTKOL-MAX      PIC 9(3)  VALUE ZERO.                    
024100         05  W-IDARTNR-MAX       PIC S9(9) VALUE ZERO COMP-3.             
024200     SKIP3                                                                
024300     03  W-3168-MIN-X-F.                                                  
024400         05  W-IDBYTKOL-MIN-F    PIC 9(3)  VALUE ZERO.                    
024500         05  W-IDARTNR-MIN-F     PIC S9(9) VALUE ZERO COMP-3.             
024600     03  W-3168-MAX-X-F.                                                  
024700         05  W-IDBYTKOL-MAX-F    PIC 9(3)  VALUE ZERO.                    
024800         05  W-IDARTNR-MAX-F     PIC S9(9) VALUE ZERO COMP-3.             
024900     SKIP3                                                                
025000     03  W-KEY3168-X.                                                     
025100         05  W-IDBYTKOL-3168     PIC 9(3)  VALUE ZERO.                    
025200         05  W-IDARTNR-3168      PIC S9(9) VALUE ZERO COMP-3.             
025300     SKIP3                                                                
025400 01  NYCKLAR-TILL-DLI.                                                    
025500                                                                          
025600     03  W-IDSKYLT-X.                                                     
025700         05  W-IDSKYLT           PIC X(3)  VALUE SPACE.                   
025800                                                                          
025900     03  W-WDGXKEY-X.                                                     
026000         05  FILLER              PIC X(4)  VALUE '3165'.                  
026100         05  FILLER              PIC X(26) VALUE LOW-VALUE.               
026200     03  W-IDARTNR-X.                                                     
026300         05  W-IDARTNR           PIC S9(9) VALUE ZERO COMP-3.             
026400     03  W-PARTNUM-X.                                                     
026500         05  W-PARTNUM           PIC S9(9) VALUE ZERO COMP-3.             
026600     03  W-IDARTNR-ART-X.                                                 
026700         05  W-IDARTNR-ART       PIC S9(9) VALUE ZERO COMP-3.             
026800     03  W-IDBYTFAK-X.                                                    
026900         05  W-IDBYTFAK          PIC 9(4)  VALUE ZERO.                    
027000     03  W-WDGX4741-X.                                                    
027100         05  W-IDHTYP-4741       PIC X(4)  VALUE '4741'.                  
027200         05  FILLER              PIC X(26) VALUE  LOW-VALUE.              
027300     03  W-WDGX4742-X.                                                    
027400         05  W-KDSEGKEY-4742     PIC X(1)  VALUE '1'.                     
027500     03  W-IDDC-X.                                                        
027600         05 W-IDDC               PIC X(2).                                
027700     03  W-IDDISTR-X.                                                     
027800         05  W-IDDISTR           PIC S9(5) VALUE ZERO COMP-3.             
027900     03  W-IDBYTKOL-X.                                                    
028000         05  W-IDBYTKOL          PIC 9(3)  VALUE ZERO.                    
028100     03  W-KDBYTFAK-X.                                                    
028200         05  W-KDBYTFAK          PIC X     VALUE '1'.                     
028300     03  W-IDARTNR-KEY-X.                                                 
028400         05  W-IDARTNR-KEY       PIC S9(9) VALUE ZERO COMP-3.             
028500     03  W-KDSEGKEY-X.                                                    
028600         05  FILLER              PIC X(1)  VALUE '1'  .                   
           03  W-IDDC-B6-X.                                                     
               05 W-IDDC-B6            PIC X(2).                                
028700     SKIP2                                                                
028800*    --- STATUS-KOD FRÅN IMS                                              
028900 01  STATUS-WS                   PIC XX.                                  
029000     88  SEGMENT-FINNS                     VALUE '  '.                    
029100     88  SEGMENT-FINNS-REDAN               VALUE 'II'.                    
029200     88  SEGMENT-SAKNAS                    VALUE 'GE'.                    
029300     88  SEGMENT-SLUT                      VALUE 'GB'.                    
029400     SKIP2                                                                
029500 01  GODK-STATUSKODER.                                                    
029600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
029700     SKIP3                                                                
029800 01  SSA1                        PIC X(128).                              
029900 01  SSA2                        PIC X(128).                              
030000 01  SSA3                        PIC X(128).                              
030100     EJECT                                                                
030200*    --- IMS FUNKTIONSKODER                                               
030300*01  -COPY W0003                                                          
030400     EJECT                                                                
030500 01  FILLER                      PIC X(16) VALUE 'WS-DB2    '.            
030600 01  FILLER -COPY BYART -PRE BYART-                                       
030700     EJECT                                                                
030800 01  FILLER                      PIC X(16) VALUE 'BYART-AREA'.            
030900       EXEC SQL INCLUDE BYART END-EXEC.                                   
031000     EJECT                                                                
031100 01  FILLER                      PIC X(16) VALUE 'SQLCA-AREA'.            
031200       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
031300                                                                          
031400 01  FILLER                      PIC X(16) VALUE 'SQLCODE-WS'.            
031500 01  DB2-WS.                                                              
031600     03  SQLCODE-WS              PIC 9(3)  VALUE ZERO.                    
031700         88  CURSOR-OK                     VALUE 000.                     
031800         88  RADER-FINNS                   VALUE 000.                     
031900         88  RADER-SAKNAS                  VALUE 100.                     
032000         88  ATKOMST-FEL                   VALUE 904.                     
032100     03  GODK-SQLCODEKODER.                                               
032200         05  GODK-SQLCODE OCCURS 5                                        
032300             INDEXED BY SQLCODE-IX PIC 9(3).                              
032400     EJECT                                                                
032500*    ---  DLI INPUT-OUTPUT AREA                                           
032600 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA'.           
032700 01  DLI-IO-AREA.                                                         
032800     03  IO-AREA                 PIC X(150) VALUE SPACE.                  
032900     SKIP3                                                                
033000     03  WLBENA11 REDEFINES IO-AREA.                                      
033100*        05  -COPY WDD311  -PRE BENA-                                     
033200     SKIP3                                                                
033300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA2'.          
033400 01  DLI-IO-AREA2.                                                        
033500     03  IO-AREA2                PIC X(1500) VALUE SPACE.                 
033600     SKIP3                                                                
033700     03  WLXXLD01 REDEFINES IO-AREA2.                                     
033800*        05  -COPY WDGX01  -PRE XXLD-                                     
033900     SKIP3                                                                
034000     03  WLXXLD11 REDEFINES IO-AREA2.                                     
034100*        05  -COPY WDGX4742  -PRE XXLD-                                   
034200     SKIP3                                                                
034300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA4'.          
034400 01  DLI-IO-AREA4.                                                        
034500     03  IO-AREA4                PIC X(300) VALUE SPACE.                  
034600     SKIP3                                                                
034700     03  WLARTS01 REDEFINES IO-AREA4.                                     
034800*        05  -COPY WDK701  -PRE ARTS-                                     
034900     SKIP3                                                                
035000     03  WLARTS11 REDEFINES IO-AREA4.                                     
035100*        05  -COPY WDK711  -PRE ARTS-                                     
035200     SKIP3                                                                
035300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA5'.          
035400 01  DLI-IO-AREA5.                                                        
035500     03  IO-AREA5                PIC X(150) VALUE SPACE.                  
035600     SKIP3                                                                
035700     03  WL316501 REDEFINES IO-AREA5.                                     
035800*        05  -COPY WDGX01   -PRE 3165-                                    
035900     SKIP3                                                                
036000     03  WL316511 REDEFINES IO-AREA5.                                     
036100*        05  -COPY WDGX3166 -PRE 3165-                                    
036200     SKIP3                                                                
036300 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA6'.          
036400 01  DLI-IO-AREA6.                                                        
036500     03  IO-AREA6                PIC X(150) VALUE SPACE.                  
036600     SKIP3                                                                
036700     03  WL316521 REDEFINES IO-AREA6.                                     
036800*        05  -COPY WDGX3168 -PRE 3165-                                    
036900 01  FILLER                      PIC X(16) VALUE 'WLLOGA01'.              
037000*01 WLLOGA01 -COPY WDL901                                                 
037100                                                                          
037200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC01'.                    
037300 01  DLI-IO-WLARTC01.                                                     
037400*    03  -COPY WDK601                                                     
037500     EJECT                                                                
037600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTC11'.                    
037700 01  DLI-IO-WLARTC11.                                                     
037800*    03  -COPY WDK611                                                     
037900                                                                          
038000     EJECT                                                                
       01  FILLER                      PIC X(16)   VALUE 'WDB601 AREA'.         
       01   DLI-IO-AREA-B601.                                                   
      *     03  -COPY WDB601                                                    
           EJECT                                                                
038100 LINKAGE SECTION.                                                         
038200 01  REQU-AREA.                                                           
038300*    03 -COPY WZ01REQU                                                    
038400*    03 -COPY W30183I1                                                    
038500     EJECT                                                                
038600 01  RESP-AREA.                                                           
038700*    03 -COPY WZ01RESP                                                    
038800*    03 -COPY W30183O1                                                    
038900     EJECT                                                                
039000 01  MAX-KVRADER                 PIC S9(4) COMP.                          
039100*01  -COPY W0009   -PRE ALT-                                              
039200     EJECT                                                                
039300*01  -COPY W0008  -PRE BENA-                                              
039400     05  FILLER                  PIC X.                                   
039500     EJECT                                                                
039600*01  -COPY W0008  -PRE XXLD-                                              
039700     05  FILLER                  PIC X.                                   
039800     EJECT                                                                
039900*01  -COPY W0008  -PRE ARTS-                                              
040000     05  FILLER                  PIC X.                                   
040100     EJECT                                                                
040200*01  -COPY W0008  -PRE 3165-                                              
040300     05  FILLER                  PIC X.                                   
040400     EJECT                                                                
040500*01  -COPY W0008  -PRE LOGA-                                              
040600     05  FILLER                  PIC X.                                   
040700     EJECT                                                                
040800*01  -COPY W0008  -PRE ARTC-                                              
040900     05  FILLER                  PIC X.                                   
041000     EJECT                                                                
040800*01  -COPY W0008  -PRE WDB6-                                              
040900     05  FILLER                  PIC X.                                   
041000     EJECT                                                                
041100 PROCEDURE DIVISION  USING                                                
041200     REQU-AREA RESP-AREA MAX-KVRADER ALT-PCB                              
041300     BENA-PCB  XXLD-PCB  ARTS-PCB                                         
041400     3165-PCB  LOGA-PCB  ARTC-PCB WDB6-PCB.                               
041500 MAIN SECTION.                                                            
041600     ENTRY 'DLITCBL' USING                                                
041700     REQU-AREA RESP-AREA MAX-KVRADER ALT-PCB                              
041800     BENA-PCB  XXLD-PCB  ARTS-PCB                                         
041900     3165-PCB  LOGA-PCB  ARTC-PCB WDB6-PCB.                               
042000                                                                          
042100     PERFORM A-INIT                                                       
042200     PERFORM B-KOLLA-NYCKLAR                                              
042300     IF NYCKLAR-OK                                                        
042400        IF REQU-UPDATE                                                    
042500           PERFORM G-KOLLA-INPUT                                          
042600           IF INDATA-OK                                                   
042700              PERFORM H-UPPDATERA                                         
042800           END-IF                                                         
042900        ELSE                                                              
043000           IF REQU-FIRST                                                  
043100              PERFORM C-FOERSTA-SIDA                                      
043200           ELSE                                                           
043300              IF REQU-NEXT                                                
043400                 PERFORM D-NAESTA-SIDA                                    
043500              ELSE                                                        
043600                 PERFORM E-SAMMA-SIDA                                     
043700              END-IF                                                      
043800           END-IF                                                         
043900        END-IF                                                            
044000        PERFORM F-LAES-VISA-INFO                                          
044100     END-IF                                                               
044200                                                                          
044300     GOBACK                                                               
044400     .                                                                    
044500     EJECT                                                                
044600 A-INIT SECTION.                                                          
044700     MOVE 'A-INIT'               TO WS-SEKTION                            
044800                                                                          
044900     MOVE ALL '+'                TO RESP-W30183O1                         
045000     MOVE LOW-VALUE              TO RESP-IDARTNR-OBJ-BORT-ATTR            
045100                                    RESP-KVRETUR-BORT-ATTR                
045200                                    RESP-IDBYTKOL-BORT-ATTR               
045300                                    RESP-GODK-FAKT-ATTR                   
045400                                    RESP-IDARTNR-OBJ-UPPD-ATTR            
045500                                    RESP-KVRETUR-UPPD-ATTR                
045600                                    RESP-FLBYTKNR-UPPD-ATTR               
045700                                    RESP-VKORDBTO-FAKT-UPD-ATTR           
045800                                    RESP-VLORDBTO-FAKT-UPD-ATTR           
045900                                                                          
046000     MOVE 001                    TO RESP-IDMSGVER                         
046100     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
046200                                    RESP-IDMSG-INFO                       
046300                                    RESP-IDELMT-ERROR                     
046400                                                                          
046500     MOVE REQU-KVRADER           TO RESP-KVRADER                          
046600                                                                          
046700     INITIALIZE GODK-SQLCODEKODER                                         
046800                                                                          
046900     MOVE ZERO                   TO W-IDARTNR-MIN                         
047000     MOVE 999999999              TO W-IDARTNR-MAX                         
047100     MOVE ZERO                   TO W-IDBYTKOL-MIN                        
047200     MOVE 999                    TO W-IDBYTKOL-MAX                        
047300                                                                          
047400     MOVE ZERO                   TO RESP-IDBYTKOL-START                   
047500                                    RESP-IDARTNR-OBJ-START                
047510                                    RESP-IDDISTR-START                    
047600                                    RESP-IDBYTKOL-NEXT                    
047700                                    RESP-IDARTNR-OBJ-NEXT                 
047710                                    RESP-IDDISTR-NEXT                     
047800                                                                          
047900     ACCEPT DAGENS-DATUM       FROM DATE                                  
048000                                                                          
048100     IF WS-YY > 60                                                        
048200        MOVE 19                  TO WS-SS                                 
048300     ELSE                                                                 
048400        MOVE 20                  TO WS-SS                                 
048500     END-IF                                                               
048600     MOVE WS-YY                  TO WS-AA                                 
048700     MOVE WS-MM                  TO WS-MA                                 
048800     MOVE WS-DD                  TO WS-DA                                 
048900                                                                          
049000     .                                                                    
049100     EJECT                                                                
049200 B-KOLLA-NYCKLAR SECTION.                                                 
049300     MOVE 'B-KOLLA-NYCKLAR'      TO WS-SEKTION                            
049400                                                                          
049500     IF REQU-IDSPRAK = 'SV'                                               
049600       MOVE 'S  '                TO W-IDSKYLT                             
049700     ELSE                                                                 
049800       MOVE 'GB '                TO W-IDSKYLT                             
049900     END-IF                                                               
050000                                                                          
050100     MOVE JA                     TO NYCKLAR-SW                            
050200     MOVE NEJ                    TO ARTIKEL-SW                            
050300     MOVE NEJ                    TO KOLLI-SW                              
050400                                                                          
050500*    -- IDDC                                                              
050600                                                                          
050700     MOVE REQU-IDDC              TO W-IDDC                                
050800                                    WS-IDDC                               
                                          W-IDDC-B6                             
050900                                                                          
051000*    -- KONTROLL AV IDBYTKOL                                              
051100                                                                          
051200     IF REQU-IDBYTKOL-KEY NUMERIC AND                                     
051300        REQU-IDBYTKOL-KEY > ZERO                                          
051400        MOVE REQU-IDBYTKOL-KEY   TO WS-IDBYTKOL                           
051500        MOVE WS-IDBYTKOL         TO W-IDBYTKOL                            
051600                                    W-IDBYTKOL-MIN                        
051700                                    W-IDBYTKOL-MAX                        
051800        MOVE JA                  TO KOLLI-SW                              
051900     ELSE                                                                 
052000        MOVE ZERO                TO REQU-IDBYTKOL-KEY                     
052000        MOVE NEJ                 TO KOLLI-SW                              
052100     END-IF                                                               
052200                                                                          
052300*    -- KONTROLL AV IDARTNR                                               
052400                                                                          
052500     MOVE REQU-IDARTNR-OBJ-KEY   TO WS-IDARTNR                            
052600                                                                          
052700     IF WS-IDARTNR NUMERIC                                                
052800        IF WS-IDARTNR  > ZERO                                             
052900           MOVE WS-IDARTNR       TO W-IDARTNR                             
053000                                    W-IDARTNR-MIN                         
053100                                    W-IDARTNR-MAX                         
053200                                    W-IDARTNR-KEY                         
053300           MOVE JA               TO ARTIKEL-SW                            
053400        END-IF                                                            
053500     ELSE                                                                 
053600        MOVE NEJ                 TO NYCKLAR-SW                            
053700        MOVE 'IDARTNR-OBJ'       TO RESP-IDELMT-ERROR                     
053800     END-IF                                                               
053810                                                                          
053820*    -- KONTROLL AV IDDISTR                                               
053830                                                                          
053840     IF REQU-IDDISTR-KEY NUMERIC AND                                      
053850        REQU-IDDISTR-KEY > ZERO                                           
053860        MOVE REQU-IDDISTR-KEY TO TEST-IDDISTR                             
053861        IF DIS134-BYTESREN-NA OR                                          
053862           REQU-IDDISTR-KEY = 9927 OR                                     
053862           REQU-IDDISTR-KEY = 9993                                        
053870           MOVE REQU-IDDISTR-KEY TO W-IDDISTR                             
053871        ELSE                                                              
053872           MOVE NEJ              TO NYCKLAR-SW                            
053873           MOVE 'IDDISTR'        TO RESP-IDELMT-ERROR                     
053880        END-IF                                                            
053892     ELSE                                                                 
053893        MOVE NEJ                 TO NYCKLAR-SW                            
053894        MOVE 'IDDISTR'           TO RESP-IDELMT-ERROR                     
053896     END-IF                                                               
053900                                                                          
054000     IF SDC OR CDC                                                        
054100        MOVE NEJ                 TO NYCKLAR-SW                            
054200        MOVE USER-NOT-ALLOWED    TO RESP-IDMSG-INFO                       
054300     END-IF                                                               
054400                                                                          
054500     IF NYCKLAR-FEL                                                       
054600        MOVE ERR-WRONG-KEY       TO RESP-IDMSG-ERROR                      
054700        MOVE ZERO                TO RESP-KVRADER                          
054800        PERFORM MFS-RENSA-FAELT-IN                                        
054900     END-IF                                                               
055000     .                                                                    
055100     EJECT                                                                
055200 C-FOERSTA-SIDA SECTION.                                                  
055300     MOVE 'C-FOERSTA-SIDA '      TO WS-SEKTION                            
055400                                                                          
055500     MOVE INF-FIRST-PAGE         TO RESP-IDMSG-INFO                       
055600                                                                          
055700     PERFORM MFS-RENSA-FAELT-IN                                           
055800     .                                                                    
055900     EJECT                                                                
056000 D-NAESTA-SIDA SECTION.                                                   
056100     MOVE 'D-NAESTA-SIDA '       TO WS-SEKTION                            
056200                                                                          
056300     MOVE REQU-IDARTNR-OBJ-START TO W-IDARTNR-MIN                         
056400                                    W-IDARTNR-KEY                         
056500     MOVE REQU-IDBYTKOL-START    TO W-IDBYTKOL-MIN                        
056510     MOVE REQU-IDDISTR-START     TO W-IDDISTR                             
056600     PERFORM MFS-RENSA-FAELT-IN                                           
056700     .                                                                    
056800     EJECT                                                                
056900 E-SAMMA-SIDA SECTION.                                                    
057000     MOVE 'E-SAMMA-SIDA '        TO WS-SEKTION                            
057100                                                                          
057200     IF REQU-IDARTNR-OBJ-START NUMERIC                                    
057300        MOVE REQU-IDARTNR-OBJ-START                                       
057400                                 TO W-IDARTNR-MIN                         
057500                                    W-IDARTNR-KEY                         
057600     ELSE                                                                 
057700        MOVE ZERO                TO W-IDARTNR-MIN                         
057800                                    W-IDARTNR-KEY                         
057900     END-IF                                                               
058000     IF REQU-IDBYTKOL-START NUMERIC                                       
058100        MOVE REQU-IDBYTKOL-START TO W-IDBYTKOL-MIN                        
058200     ELSE                                                                 
058300        MOVE ZERO                TO W-IDBYTKOL-MIN                        
058400     END-IF                                                               
058410     IF REQU-IDDISTR-START NUMERIC                                        
058420        MOVE REQU-IDDISTR-START TO W-IDDISTR                              
058430     ELSE                                                                 
058440        MOVE ZERO               TO W-IDDISTR                              
058450     END-IF                                                               
058500     IF REQU-NYRAD = ALL '+' AND                                          
058600        REQU-BORT  = ALL '+'                                              
058700        PERFORM MFS-RENSA-FAELT-IN                                        
058800     ELSE                                                                 
058900        MOVE INF-PRESS-PF11      TO RESP-IDMSG-ERROR                      
059000        PERFORM EA-MID-INDATA-TILL-MOD                                    
059100     END-IF                                                               
059200     .                                                                    
059300     EJECT                                                                
059400 EA-MID-INDATA-TILL-MOD SECTION.                                          
059500     MOVE 'EA-MID-INDATA-TILL-MOD'                                        
059600                                 TO WS-SEKTION                            
059700                                                                          
059800     IF REQU-IDARTNR-OBJ-UPPD = ALL '+'                                   
059900        MOVE ALL-SPACE           TO RESP-IDARTNR-OBJ-UPPD                 
060000        MOVE MFS-NUM-FAELT-RAETT                                          
060100                                 TO RESP-IDARTNR-OBJ-UPPD-ATTR            
060200     ELSE                                                                 
060300        MOVE ALL-PLUS            TO RESP-IDARTNR-OBJ-UPPD                 
060400        MOVE MFS-ADD-LAES-IN-FAELT                                        
060500                                 TO RESP-IDARTNR-OBJ-UPPD-ATTR            
060600     END-IF                                                               
060700                                                                          
060800     IF REQU-KVRETUR-UPPD = ALL '+'                                       
060900        MOVE ALL-SPACE           TO RESP-KVRETUR-UPPD                     
061000        MOVE MFS-NUM-FAELT-RAETT                                          
061100                                 TO RESP-KVRETUR-UPPD-ATTR                
061200     ELSE                                                                 
061300        MOVE ALL-PLUS            TO RESP-KVRETUR-UPPD                     
061400        MOVE MFS-ADD-LAES-IN-FAELT                                        
061500                                 TO RESP-KVRETUR-UPPD-ATTR                
061600     END-IF                                                               
061700                                                                          
061800     IF REQU-FLBYTKNR-UPPD = ALL '+'                                      
061900        MOVE ALL-SPACE           TO RESP-FLBYTKNR-UPPD                    
062000        MOVE MFS-ALFA-FAELT-RAETT                                         
062100                                 TO RESP-FLBYTKNR-UPPD-ATTR               
062200     ELSE                                                                 
062300        MOVE ALL-PLUS            TO RESP-FLBYTKNR-UPPD                    
062400        MOVE MFS-ADD-LAES-IN-FAELT                                        
062500                                 TO RESP-FLBYTKNR-UPPD-ATTR               
062600     END-IF                                                               
062700                                                                          
062800     IF REQU-IDARTNR-OBJ-BORT = ALL '+'                                   
062900        MOVE ALL-SPACE           TO RESP-IDARTNR-OBJ-BORT                 
063000        MOVE MFS-NUM-FAELT-RAETT                                          
063100                                 TO RESP-IDARTNR-OBJ-BORT-ATTR            
063200     ELSE                                                                 
063300        MOVE ALL-PLUS            TO RESP-IDARTNR-OBJ-BORT                 
063400        MOVE MFS-ADD-LAES-IN-FAELT                                        
063500                                 TO RESP-IDARTNR-OBJ-BORT-ATTR            
063600     END-IF                                                               
063700                                                                          
063800     IF REQU-KVRETUR-BORT = ALL '+'                                       
063900        MOVE ALL-SPACE           TO RESP-KVRETUR-BORT                     
064000        MOVE MFS-NUM-FAELT-RAETT                                          
064100                                 TO RESP-KVRETUR-BORT-ATTR                
064200     ELSE                                                                 
064300        MOVE ALL-PLUS            TO RESP-KVRETUR-BORT                     
064400        MOVE MFS-ADD-LAES-IN-FAELT                                        
064500                                 TO RESP-KVRETUR-BORT-ATTR                
064600     END-IF                                                               
064700                                                                          
064800     IF REQU-IDBYTKOL-BORT = ALL '+'                                      
064900        MOVE ALL-SPACE           TO RESP-IDBYTKOL-BORT                    
065000        MOVE MFS-NUM-FAELT-RAETT                                          
065100                                 TO RESP-IDBYTKOL-BORT-ATTR               
065200     ELSE                                                                 
065300        MOVE ALL-PLUS            TO RESP-IDBYTKOL-BORT                    
065400        MOVE MFS-ADD-LAES-IN-FAELT                                        
065500                                 TO RESP-IDBYTKOL-BORT-ATTR               
065600     END-IF                                                               
065700                                                                          
065800     IF REQU-GODK-FAKT = ALL '+'                                          
065900        MOVE ALL-SPACE           TO RESP-GODK-FAKT                        
066000        MOVE MFS-ALFA-FAELT-RAETT                                         
066100                                 TO RESP-GODK-FAKT-ATTR                   
066200     ELSE                                                                 
066300        MOVE ALL-PLUS            TO RESP-GODK-FAKT                        
066400        MOVE MFS-ADD-LAES-IN-FAELT                                        
066500                                 TO RESP-GODK-FAKT-ATTR                   
066600     END-IF                                                               
066700                                                                          
066800     IF REQU-VKORDBTO-FAKT-UPD = ALL '+'                                  
066900        MOVE ALL-SPACE           TO RESP-VKORDBTO-FAKT-UPD                
067000        MOVE MFS-NUM-FAELT-RAETT TO RESP-VKORDBTO-FAKT-UPD-ATTR           
067100                                                                          
067200     ELSE                                                                 
067300        MOVE ALL-PLUS            TO RESP-VKORDBTO-FAKT-UPD                
067400        MOVE MFS-ADD-LAES-IN-FAELT                                        
067500                                 TO RESP-VKORDBTO-FAKT-UPD-ATTR           
067600     END-IF                                                               
067700                                                                          
067800     IF REQU-VLORDBTO-FAKT-UPD = ALL '+'                                  
067900        MOVE ALL-SPACE           TO RESP-VLORDBTO-FAKT-UPD                
068000        MOVE MFS-NUM-FAELT-RAETT                                          
068100                                 TO RESP-VLORDBTO-FAKT-UPD-ATTR           
068200     ELSE                                                                 
068300        MOVE ALL-PLUS            TO RESP-VLORDBTO-FAKT-UPD                
068400        MOVE MFS-ADD-LAES-IN-FAELT                                        
068500                                 TO RESP-VLORDBTO-FAKT-UPD-ATTR           
068600     END-IF                                                               
068700                                                                          
068800     .                                                                    
068900     EJECT                                                                
069000 F-LAES-VISA-INFO SECTION.                                                
069100     MOVE 'F-LAES-VISA-INFO'     TO WS-SEKTION                            
069200                                                                          
069300     MOVE ZERO                   TO RESP-KVRADER                          
069310     MOVE W-IDDISTR              TO RESP-IDDISTR-START                    
069320                                    RESP-IDDISTR-NEXT                     
069400                                                                          
069500     PERFORM FA-LAES-GRUNDDATA                                            
069600                                                                          
069700     IF SEGMENT-SAKNAS                                                    
069800        IF FAKTURA-SKRIVEN                                                
069900           MOVE PRINTING-OK      TO RESP-IDMSG-ERROR                      
070000        ELSE                                                              
070100           IF UPPDATE-DONE = NEJ                                          
070200              MOVE ERR-INFO-MISSING                                       
070300                                 TO RESP-IDMSG-ERROR                      
070400              MOVE 'KEY'         TO RESP-IDELMT-ERROR                     
070500           END-IF                                                         
070600        END-IF                                                            
070700     ELSE                                                                 
070800        MOVE +1                  TO INDX                                  
070900        MOVE 3165-3166-IDBYTFAK  TO RESP-IDBYTFAK                         
071000        PERFORM UNTIL INDX > MAX-KVRADER                                  
071100           IF SEGMENT-FINNS                                               
071200              MOVE 3165-3168-IDARTNR-OBJ                                  
071300                                 TO RESP-IDARTNR-OBJ-LINE (INDX)          
071400                                    W-IDARTNR                             
071500                                                                          
071600              MOVE 3165-3168-IDBYTKOL                                     
071700                                 TO RESP-IDBYTKOL-LINE (INDX)             
071800                                                                          
071900              MOVE 3165-3168-KVLEVART                                     
072000                                 TO RESP-KVRETUR-LINE (INDX)              
072100                                                                          
072200*    -- SELECT LANGUAGE TO FETCH AND CORRESPONDING CODE-PAGE              
072300              EVALUATE REQU-IDSPRAK                                       
072400                WHEN 'SV'                                                 
072500                  MOVE WS-IDSKYLT-SE                                      
072600                                 TO W-IDSKYLT-X                           
072700                  MOVE WS-CP-EBCDIC                                       
072800                                 TO TRAUTF8-KDCP                          
072900                WHEN 'ZH'                                                 
073000                  MOVE WS-IDSKYLT-CN                                      
073100                                 TO W-IDSKYLT-X                           
073200                  MOVE WS-CP-UNICODE                                      
073300                                 TO TRAUTF8-KDCP                          
073400                WHEN OTHER                                                
073500                  MOVE WS-IDSKYLT-GB                                      
073600                                 TO W-IDSKYLT-X                           
073700                  MOVE WS-CP-EBCDIC                                       
073800                                 TO TRAUTF8-KDCP                          
073900              END-EVALUATE                                                
                    PERFORM IMS-GU-WDB601                                       
                                                                                
                    MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                     
                    IF DCS-UNICODE-IDSKYLT                                      
                       MOVE 'UTF8'             TO TRAUTF8-KDCP                  
                    ELSE                                                        
                       MOVE '278 '             TO TRAUTF8-KDCP                  
                    END-IF                                                      
074000              PERFORM IMS-GET-BENA                                        
074100              IF SEGMENT-FINNS                                            
074200                 MOVE BENA-TEXT-BEART                                     
074300                                 TO TRAUTF8-TECONV-FROM                   
074400              ELSE                                                        
074500                 MOVE SPACE      TO TRAUTF8-TECONV-FROM                   
074600                                    BENA-TEXT-BEART                       
074700                 MOVE WS-CP-EBCDIC                                        
074800                                 TO TRAUTF8-KDCP                          
074900              END-IF                                                      
                    IF TRAUTF8-TECONV-FROM = SPACES                             
                     MOVE 'GB'  TO W-IDSKYLT                                    
                     MOVE '278' TO TRAUTF8-KDCP                                 
                     PERFORM IMS-GET-BENA                                       
                     MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM             
                    END-IF                                                      
075000              IF REQU-IDMSGVER = '001'                                    
075100*              CALL FROM WEB                                              
075200*              CONVERT TO UNICODE IF NOT ALREADY SO,                      
075300*                 STRIP TRAILING SPACE                                    
075400                 CALL WTRAUTF8 USING TRAUTF8-AREA                         
075500                 MOVE TRAUTF8-TECONV-TO                                   
075600                                 TO RESP-BEART-LINE (INDX)                
075700              ELSE                                                        
075800*                CALL FROM 3270 SCREEN. RETURN AS-IS (EBCDIC)             
075900                 MOVE BENA-TEXT-BEART                                     
076000                                 TO RESP-BEART-LINE (INDX)                
076100              END-IF                                                      
076200              ADD 1              TO INDX                                  
076300                                    RESP-KVRADER                          
076400           ELSE                                                           
076500              MOVE 'GB'          TO STATUS-WS                             
076600              COMPUTE INDX = MAX-KVRADER + 1                              
076700           END-IF                                                         
076800           IF SEGMENT-FINNS                                               
076900              IF ARTIKEL-FINNS AND                                        
077000                 KOLLI-SAKNAS                                             
077100                 PERFORM IMS-GET-WL316521-2                               
077200              ELSE                                                        
077300                 PERFORM IMS-GET-WL316521                                 
077400              END-IF                                                      
077500           END-IF                                                         
077600        END-PERFORM                                                       
077700                                                                          
077800        IF SEGMENT-FINNS                                                  
077900           PERFORM FC-SKAPA-NEXT-KEY                                      
078000           IF UPPDATE-DONE = NEJ                                          
078100              IF INDATA-OK                                                
078200                 MOVE INF-MORE-INFO-EXISTS                                
078300                                 TO RESP-IDMSG-INFO                       
078400              END-IF                                                      
078500           END-IF                                                         
078600        ELSE                                                              
078700           MOVE RESP-IDBYTKOL-START                                       
078800                                 TO RESP-IDBYTKOL-NEXT                    
078900           MOVE RESP-IDARTNR-OBJ-START                                    
079000                                 TO RESP-IDARTNR-OBJ-NEXT                 
079100           IF UPPDATE-DONE = NEJ                                          
079200              IF INDATA-OK                                                
079300                 MOVE THIS-IS-THE-LAST-PAGE                               
079400                                 TO RESP-IDMSG-INFO                       
079500              END-IF                                                      
079600           END-IF                                                         
079700        END-IF                                                            
079800*       MOVE MFS-ADD-SAETT-CURSOR                                         
079900*                                TO RESP-IDARTNR-OBJ-UPPD-ATTR            
080000     END-IF                                                               
080100                                                                          
080200     .                                                                    
080300     EJECT                                                                
080400 FA-LAES-GRUNDDATA SECTION.                                               
080500     MOVE 'FA-LAES-GRUNDDATA'    TO WS-SEKTION                            
080600     SKIP3                                                                
080700     PERFORM IMS-GET-WL316501                                             
080800     IF SEGMENT-FINNS                                                     
080900        PERFORM IMS-GET-WL316511                                          
081000        IF SEGMENT-FINNS                                                  
081100           MOVE 3165-3166-IDBYTFAK                                        
081200                                 TO W-IDBYTFAK                            
081300           IF 3165-3166-VLORDBTO-FAKT > ZERO                              
081400             IF NDC-NA                                                    
081500               COMPUTE WS-VLORDBTO-UTAN-DEC ROUNDED =                     
081600                        3165-3166-VLORDBTO-FAKT * CONV-M3-TO-FT3          
081700               MOVE WS-VLORDBTO-UTAN-DEC                                  
081800                                  TO RESP-VLORDBTO-FAKT-UT                
081900             ELSE                                                         
082000               MOVE 3165-3166-VLORDBTO-FAKT                               
082100                                  TO WS-VLORDBTO                          
082200               MOVE WS-VLORDBTO   TO RESP-VLORDBTO-FAKT-UT                
082300             END-IF                                                       
082400           END-IF                                                         
082500           IF 3165-3166-VKORDBTO-FAKT > ZERO                              
082600             IF NDC-NA                                                    
082700               COMPUTE WS-VKORDBTO-UTAN-DEC ROUNDED =                     
082800                       3165-3166-VKORDBTO-FAKT * CONV-KG-TO-LB            
082900               MOVE WS-VKORDBTO-UTAN-DEC                                  
083000                                 TO RESP-VKORDBTO-FAKT-UT                 
083100             ELSE                                                         
083200               MOVE 3165-3166-VKORDBTO-FAKT                               
083300                                 TO WS-VKORDBTO                           
083400               MOVE WS-VKORDBTO  TO RESP-VKORDBTO-FAKT-UT                 
083500             END-IF                                                       
083600                                                                          
083700           END-IF                                                         
083800           IF ARTIKEL-FINNS AND KOLLI-SAKNAS                              
083900              PERFORM IMS-GET-WL316521-2                                  
084000              IF W-IDBYTKOL-MIN > ZERO                                    
084100                 PERFORM                                                  
084200                   UNTIL SEGMENT-SAKNAS OR                                
084300                         3165-3168-IDBYTKOL = W-IDBYTKOL-MIN              
084400                    PERFORM IMS-GET-WL316521-2                            
084500                 END-PERFORM                                              
084600              END-IF                                                      
084700              IF SEGMENT-FINNS                                            
084800                 MOVE 3165-3168-IDBYTKOL                                  
084900                                 TO RESP-IDBYTKOL-START                   
085000                 MOVE 3165-3168-IDARTNR-OBJ                               
085100                                 TO WS-IDARTNR-NUM                        
085200                 MOVE WS-IDARTNR-NUM                                      
085300                                 TO RESP-IDARTNR-OBJ-START                
085400              ELSE                                                        
085500                 MOVE W-IDBYTKOL-MIN                                      
085600                                 TO RESP-IDBYTKOL-START                   
085700                 MOVE ZERO       TO RESP-IDARTNR-OBJ-START                
085800              END-IF                                                      
085900           ELSE                                                           
086000******************************************************************        
086100*** LÄS FRAM TILL DET KOLLIT ARTIKLNR VAR FÖRUT ******************        
086200******************************************************************        
086300              PERFORM IMS-GET-WL316521                                    
086400              IF SEGMENT-FINNS                                            
086500                 MOVE 3165-3168-IDBYTKOL                                  
086600                                 TO RESP-IDBYTKOL-START                   
086700                 MOVE 3165-3168-IDARTNR-OBJ                               
086800                                 TO WS-IDARTNR-NUM                        
086900                 MOVE WS-IDARTNR-NUM                                      
087000                                 TO RESP-IDARTNR-OBJ-START                
087100              ELSE                                                        
087200                 MOVE W-IDBYTKOL-MIN                                      
087300                                 TO RESP-IDBYTKOL-START                   
087400                 MOVE W-IDARTNR-MIN                                       
087500                                 TO WS-IDARTNR-NUM                        
087600                 MOVE WS-IDARTNR-NUM                                      
087700                                 TO RESP-IDARTNR-OBJ-START                
087800              END-IF                                                      
087900           END-IF                                                         
088000        ELSE                                                              
088100           MOVE ZERO             TO RESP-IDBYTKOL-START                   
088200                                    RESP-IDARTNR-OBJ-START                
088300        END-IF                                                            
088400     ELSE                                                                 
088500        MOVE ZERO                TO RESP-IDBYTKOL-START                   
088600                                    RESP-IDARTNR-OBJ-START                
088700     END-IF                                                               
088800     MOVE ZERO                   TO RESP-IDBYTKOL-NEXT                    
088900                                    RESP-IDARTNR-OBJ-NEXT                 
089000                                                                          
089100     .                                                                    
089200     EJECT                                                                
089300 FC-SKAPA-NEXT-KEY SECTION.                                               
089400     MOVE 'FC-SKAPA-NEXT-KEY'    TO WS-SEKTION                            
089500                                                                          
089600     MOVE 3165-3168-IDBYTKOL     TO RESP-IDBYTKOL-NEXT                    
089700     MOVE 3165-3168-IDARTNR-OBJ  TO WS-IDARTNR-NUM                        
089800     MOVE WS-IDARTNR-NUM         TO RESP-IDARTNR-OBJ-NEXT                 
089900     .                                                                    
090000     EJECT                                                                
090100 G-KOLLA-INPUT SECTION.                                                   
090200     MOVE 'G-KOLLA-INPUT'        TO WS-SEKTION                            
090300                                                                          
090400     MOVE JA                     TO INDATA-SW                             
090500                                                                          
090600     IF REQU-NYRAD NOT = ALL '+'                                          
090700        IF REQU-GODKRAD NOT = ALL '+'                                     
090800           MOVE MFS-ALFA-FAELT-FEL                                        
090900                                 TO RESP-GODK-FAKT-ATTR                   
091000           MOVE MFS-NUM-FAELT-FEL                                         
091100                                 TO RESP-VKORDBTO-FAKT-UPD-ATTR           
091200                                    RESP-VLORDBTO-FAKT-UPD-ATTR           
091300           MOVE ERR-UPPDATE      TO RESP-IDMSG-INFO                       
091400           PERFORM MFS-ROER-EJ-FAELT-UT                                   
091500           MOVE NEJ              TO INDATA-SW                             
091600        ELSE                                                              
091700           PERFORM GA-KOLLA-REQU-NYRAD                                    
091800        END-IF                                                            
091900     END-IF                                                               
092000                                                                          
092100     IF INDATA-OK                                                         
092200        IF REQU-BORT NOT = ALL '+'                                        
092300           IF REQU-GODKRAD NOT = ALL '+'                                  
092400              MOVE MFS-ALFA-FAELT-FEL                                     
092500                                 TO RESP-GODK-FAKT-ATTR                   
092600              MOVE MFS-NUM-FAELT-FEL                                      
092700                                 TO RESP-VKORDBTO-FAKT-UPD-ATTR           
092800                                    RESP-VLORDBTO-FAKT-UPD-ATTR           
092900              MOVE ERR-UPPDATE   TO RESP-IDMSG-INFO                       
093000              PERFORM MFS-ROER-EJ-FAELT-UT                                
093100              MOVE NEJ           TO INDATA-SW                             
093200           ELSE                                                           
093300              PERFORM GB-KOLLA-REQU-BORT                                  
093400           END-IF                                                         
093500        ELSE                                                              
093600           IF REQU-VKORDBTO-FAKT-UPD NOT = ALL '+'                        
093700              PERFORM GC-KOLLA-VKORDBTO                                   
093800           END-IF                                                         
093900           IF INDATA-OK                                                   
094000              IF REQU-VLORDBTO-FAKT-UPD NOT = ALL '+'                     
094100                 PERFORM S08-REDIGERA-VLORDBTO                            
094200              END-IF                                                      
094300           END-IF                                                         
094400           IF INDATA-OK                                                   
094500              IF REQU-GODK-FAKT NOT = ALL '+'                             
094600                 PERFORM GD-KOLLA-GODK-FAKT                               
094700              END-IF                                                      
094800           END-IF                                                         
094900        END-IF                                                            
095000     END-IF                                                               
095100     IF INDATA-FEL                                                        
095200        MOVE ERR-UPPDATE         TO RESP-IDMSG-INFO                       
095300        MOVE NEJ                 TO INDATA-SW                             
095400     END-IF                                                               
095500     .                                                                    
095600     EJECT                                                                
095700 GA-KOLLA-REQU-NYRAD SECTION.                                             
095800     MOVE 'GA-KOLLA-REQU-NYRAD'  TO WS-SEKTION                            
095900                                                                          
096000     IF REQU-IDARTNR-OBJ-UPPD NOT = ALL '+'                               
096100        INSPECT REQU-IDARTNR-OBJ-UPPD REPLACING                           
096200           LEADING SPACE BY ZERO                                          
096300        MOVE REQU-IDARTNR-OBJ-UPPD                                        
096400                                 TO TEST-IDARTNR                          
096500                                    W-PARTNUM                             
096600        PERFORM IMS-GET-ARTC-C11                                          
096700        IF SEGMENT-FINNS AND                                              
096800           CLAG-PRARTSJK > ZERO                                           
096900           IF BYT03-OBJEKT                                                
097000              MOVE REQU-IDARTNR-OBJ-UPPD                                  
097100                                 TO W-IDARTNR-BYT                         
097200              PERFORM DB2-SELECT-BYART                                    
097300              IF RADER-FINNS                                              
097400                 IF BYART-IDDISTR-RENOV = 0082                            
097500***************************                                               
097600**** ARTIKELN ÄR SKROT ****                                               
097700***************************                                               
097800                    MOVE MFS-NUM-FAELT-FEL                                
097900                                 TO RESP-IDARTNR-OBJ-UPPD-ATTR            
098000                    MOVE MFS-NUM-FAELT-RAETT                              
098100                                 TO RESP-KVRETUR-UPPD-ATTR                
098200                    MOVE MFS-ALFA-FAELT-RAETT                             
098300                                 TO RESP-FLBYTKNR-UPPD-ATTR               
098400                    MOVE PARTNO-IS-SCRAP                                  
098500                                 TO RESP-IDMSG-ERROR                      
098600                    MOVE NEJ     TO INDATA-SW                             
098700                 ELSE                                                     
098800                    PERFORM GAA-KOLLA-NYRAD                               
098900                 END-IF                                                   
099000              ELSE                                                        
099100****************************************                                  
099200**** ARTIKELN SAKNAS I DB2 TABELLEN ****                                  
099300****************************************                                  
099400                 PERFORM GAA-KOLLA-NYRAD                                  
099500              END-IF                                                      
099600           ELSE                                                           
099700              MOVE MFS-NUM-FAELT-FEL                                      
099800                                 TO RESP-IDARTNR-OBJ-UPPD-ATTR            
099900              MOVE MFS-NUM-FAELT-RAETT                                    
100000                                 TO RESP-KVRETUR-UPPD-ATTR                
100100              MOVE MFS-ALFA-FAELT-RAETT                                   
100200                                 TO RESP-FLBYTKNR-UPPD-ATTR               
100300              PERFORM MFS-ROER-EJ-FAELT-2                                 
100400              MOVE NOT-A-CORE-NO TO RESP-IDMSG-ERROR                      
100500              MOVE 'IDARTNR-OBJ' TO RESP-IDELMT-ERROR                     
100600              MOVE NEJ           TO INDATA-SW                             
100700           END-IF                                                         
100800        ELSE                                                              
100900           MOVE MFS-NUM-FAELT-FEL                                         
101000                                 TO RESP-IDARTNR-OBJ-UPPD-ATTR            
101100           MOVE MFS-NUM-FAELT-RAETT                                       
101200                                 TO RESP-KVRETUR-UPPD-ATTR                
101300           MOVE MFS-ALFA-FAELT-RAETT                                      
101400                                 TO RESP-FLBYTKNR-UPPD-ATTR               
101500           PERFORM MFS-ROER-EJ-FAELT-2                                    
101600           MOVE PARTNO-MISSING   TO RESP-IDMSG-ERROR                      
101700           MOVE 'IDARTNR'        TO RESP-IDELMT-ERROR                     
101800           MOVE NEJ              TO INDATA-SW                             
101900        END-IF                                                            
102000     END-IF                                                               
102100                                                                          
102200     .                                                                    
102300     EJECT                                                                
102400 GAA-KOLLA-NYRAD SECTION.                                                 
102500     MOVE 'GAA-KOLLA-NYRAD'      TO WS-SEKTION                            
102600                                                                          
102700     MOVE MFS-NUM-FAELT-RAETT    TO RESP-IDARTNR-OBJ-UPPD-ATTR            
102800                                                                          
102900     IF REQU-KVRETUR-UPPD NOT = ALL '+'                                   
103000        INSPECT REQU-KVRETUR-UPPD REPLACING                               
103100           LEADING SPACE BY ZERO                                          
103200        IF REQU-KVRETUR-UPPD NUMERIC                                      
103300           MOVE MFS-NUM-FAELT-RAETT                                       
103400                                 TO RESP-KVRETUR-UPPD-ATTR                
103500        ELSE                                                              
103600           MOVE MFS-NUM-FAELT-FEL                                         
103700                                 TO RESP-KVRETUR-UPPD-ATTR                
103800           MOVE ERR-CORR-HILITE-FLDS                                      
103900                                 TO RESP-IDMSG-ERROR                      
104000           PERFORM MFS-ROER-EJ-FAELT-2                                    
104100           MOVE NEJ              TO INDATA-SW                             
104200        END-IF                                                            
104300************************                                                  
104400*** VALIDERA IDBYTKOL***                                                  
104500************************                                                  
104600        IF REQU-FLBYTKNR-UPPD NOT = ALL '+'                               
104700           IF REQU-FLBYTKNR-UPPD = 'J' OR 'Y'                             
104800              MOVE MFS-ALFA-FAELT-RAETT                                   
104900                                 TO RESP-FLBYTKNR-UPPD-ATTR               
105000           ELSE                                                           
105100              MOVE MFS-ALFA-FAELT-FEL                                     
105200                                 TO RESP-FLBYTKNR-UPPD-ATTR               
105300              MOVE ERR-CORR-HILITE-FLDS                                   
105400                                 TO RESP-IDMSG-ERROR                      
105500              PERFORM MFS-ROER-EJ-FAELT-2                                 
105600              MOVE NEJ           TO INDATA-SW                             
105700           END-IF                                                         
105800        ELSE                                                              
105900           MOVE 'N'              TO REQU-FLBYTKNR-UPPD                    
106000           MOVE MFS-ALFA-FAELT-RAETT                                      
106100                                 TO RESP-FLBYTKNR-UPPD-ATTR               
106200        END-IF                                                            
106300     ELSE                                                                 
106400        MOVE MFS-NUM-FAELT-FEL   TO RESP-KVRETUR-UPPD-ATTR                
106500        MOVE MFS-ALFA-FAELT-RAETT                                         
106600                                 TO RESP-FLBYTKNR-UPPD-ATTR               
106700        MOVE ERR-CORR-HILITE-FLDS                                         
106800                                 TO RESP-IDMSG-ERROR                      
106900        PERFORM MFS-ROER-EJ-FAELT-2                                       
107000        MOVE NEJ                 TO INDATA-SW                             
107100     END-IF                                                               
107200     .                                                                    
107300     EJECT                                                                
107400 GB-KOLLA-REQU-BORT SECTION.                                              
107500     MOVE 'GB-KOLLA-REQU-BORT'   TO WS-SEKTION                            
107600                                                                          
107700     IF REQU-GODK-FAKT NOT = ALL '+'                                      
107800        MOVE MFS-ALFA-FAELT-FEL  TO RESP-GODK-FAKT-ATTR                   
107900        MOVE NEJ                 TO INDATA-SW                             
108000        MOVE ERR-UPPDATE         TO RESP-IDMSG-INFO                       
108100        PERFORM MFS-ROER-EJ-FAELT-UT                                      
108200     ELSE                                                                 
108300        IF REQU-IDARTNR-OBJ-BORT NOT = ALL '+'                            
108400           INSPECT REQU-IDARTNR-OBJ-BORT REPLACING                        
108500              LEADING SPACE BY ZERO                                       
108600           MOVE REQU-IDARTNR-OBJ-BORT                                     
108700                                 TO TEST-IDARTNR                          
108800           IF BYT03-OBJEKT                                                
108900              MOVE MFS-NUM-FAELT-RAETT                                    
109000                                 TO RESP-IDBYTKOL-BORT-ATTR               
109100                                                                          
109200              MOVE REQU-IDARTNR-OBJ-BORT                                  
109300                                 TO W-IDARTNR-BYT                         
109400              PERFORM DB2-SELECT-BYART                                    
109500              IF RADER-FINNS                                              
109600                 IF BYART-IDDISTR-RENOV = 0082                            
109700**********************                                                    
109800* ARTIKELN ÄR SKROT **                                                    
109900**********************                                                    
110000                    MOVE MFS-NUM-FAELT-FEL                                
110100                                 TO RESP-IDARTNR-OBJ-BORT-ATTR            
110200                    MOVE MFS-NUM-FAELT-RAETT                              
110300                                 TO RESP-KVRETUR-BORT-ATTR                
110400                                    RESP-IDBYTKOL-BORT-ATTR               
110500                    MOVE MISSING-PARTNO                                   
110600                                 TO RESP-IDMSG-ERROR                      
110700                    MOVE 'IDARTNR'                                        
110800                                 TO RESP-IDELMT-ERROR                     
110900                    MOVE NEJ     TO INDATA-SW                             
111000                    PERFORM MFS-ROER-EJ-FAELT-3                           
111100                 ELSE                                                     
111200                    PERFORM GBA-KOLLA-REQU-BORT                           
111300                 END-IF                                                   
111400              ELSE                                                        
111500***********************************                                       
111600*ARTIKELN SAKNAS I DB2 TABELLEN ***                                       
111700***********************************                                       
111800                 PERFORM GBA-KOLLA-REQU-BORT                              
111900              END-IF                                                      
112000           ELSE                                                           
112100              MOVE MFS-NUM-FAELT-FEL                                      
112200                                 TO RESP-IDARTNR-OBJ-BORT-ATTR            
112300                                    RESP-KVRETUR-BORT-ATTR                
112400                                    RESP-IDBYTKOL-BORT-ATTR               
112500              MOVE NOT-A-CORE-NO TO RESP-IDMSG-ERROR                      
112600              MOVE 'IDARTNR-OBJ' TO RESP-IDELMT-ERROR                     
112700              MOVE NEJ           TO INDATA-SW                             
112800              PERFORM MFS-ROER-EJ-FAELT-3                                 
112900           END-IF                                                         
113000        ELSE                                                              
113100           MOVE MFS-NUM-FAELT-FEL                                         
113200                                 TO RESP-IDARTNR-OBJ-BORT-ATTR            
113300           MOVE MFS-NUM-FAELT-RAETT                                       
113400                                 TO RESP-KVRETUR-BORT-ATTR                
113500                                    RESP-IDBYTKOL-BORT-ATTR               
113600           MOVE NOT-A-CORE-NO    TO RESP-IDMSG-ERROR                      
113700           MOVE 'IDARTNR-OBJ'    TO RESP-IDELMT-ERROR                     
113800           MOVE NEJ              TO INDATA-SW                             
113900           PERFORM MFS-ROER-EJ-FAELT-3                                    
114000        END-IF                                                            
114100     END-IF                                                               
114200     .                                                                    
114300     EJECT                                                                
114400 GBA-KOLLA-REQU-BORT SECTION.                                             
114500     MOVE 'GBA-KOLLA-REQU-BORT'  TO WS-SEKTION                            
114600                                                                          
114700     MOVE MFS-NUM-FAELT-RAETT    TO RESP-IDARTNR-OBJ-BORT-ATTR            
114800                                                                          
114900     IF REQU-KVRETUR-BORT NOT = ALL '+'                                   
115000        INSPECT REQU-KVRETUR-BORT REPLACING                               
115100           LEADING SPACE BY ZERO                                          
115200        IF REQU-KVRETUR-BORT NUMERIC                                      
115300           MOVE MFS-NUM-FAELT-RAETT                                       
115400                                 TO RESP-KVRETUR-BORT-ATTR                
115500        ELSE                                                              
115600           MOVE MFS-NUM-FAELT-FEL                                         
115700                                 TO RESP-KVRETUR-BORT-ATTR                
115800           MOVE WRONG-AMOUNT                                              
115900                                 TO RESP-IDMSG-ERROR                      
116000           MOVE NEJ              TO INDATA-SW                             
116100           PERFORM MFS-ROER-EJ-FAELT-3                                    
116200        END-IF                                                            
116300     ELSE                                                                 
116400        MOVE MFS-NUM-FAELT-FEL   TO RESP-KVRETUR-BORT-ATTR                
116500        MOVE WRONG-AMOUNT        TO RESP-IDMSG-ERROR                      
116600        MOVE NEJ                 TO INDATA-SW                             
116700        PERFORM MFS-ROER-EJ-FAELT-3                                       
116800     END-IF                                                               
116900*******************                                                       
117000*ALIDERA IDBYTKOL**                                                       
117100*******************                                                       
117200     IF REQU-IDBYTKOL-BORT NOT = ALL '+'                                  
117300        INSPECT REQU-IDBYTKOL-BORT                                        
117400           REPLACING LEADING SPACE BY ZERO                                
117500        IF REQU-IDBYTKOL-BORT NUMERIC                                     
117600           MOVE MFS-NUM-FAELT-RAETT                                       
117700                                 TO RESP-IDBYTKOL-BORT-ATTR               
117800        ELSE                                                              
117900           MOVE MFS-NUM-FAELT-FEL                                         
118000                                 TO RESP-IDBYTKOL-BORT-ATTR               
118100                                                                          
118200           MOVE KOMP-KOLLI-UPPG  TO RESP-IDMSG-ERROR                      
118300           MOVE NEJ              TO INDATA-SW                             
118400           PERFORM MFS-ROER-EJ-FAELT-3                                    
118500        END-IF                                                            
118600     ELSE                                                                 
118700        MOVE MFS-NUM-FAELT-FEL   TO RESP-IDBYTKOL-BORT-ATTR               
118800        MOVE KOMP-KOLLI-UPPG     TO RESP-IDMSG-ERROR                      
118900        PERFORM MFS-ROER-EJ-FAELT-3                                       
119000        MOVE NEJ                 TO INDATA-SW                             
119100     END-IF                                                               
119200     .                                                                    
119300     EJECT                                                                
119400 GC-KOLLA-VKORDBTO SECTION.                                               
119500     MOVE 'GC-KOLLA-VKORDBTO'    TO WS-SEKTION                            
119600     SKIP2                                                                
119700     PERFORM S10-REDIGERA-VIKT                                            
119800     IF INDATA-OK                                                         
119900        IF WS-VKORDBTO > ZERO                                             
120000           MOVE MFS-NUM-FAELT-RAETT                                       
120100                                 TO RESP-VKORDBTO-FAKT-UPD-ATTR           
120200        ELSE                                                              
120300           MOVE MFS-NUM-FAELT-FEL                                         
120400                                 TO RESP-VKORDBTO-FAKT-UPD-ATTR           
120500           MOVE MFS-NUM-FAELT-RAETT                                       
120600                                 TO RESP-VLORDBTO-FAKT-UPD-ATTR           
120700           MOVE MFS-ALFA-FAELT-RAETT                                      
120800                                 TO RESP-GODK-FAKT-ATTR                   
120900           MOVE ERR-CORR-HILITE-FLDS                                      
121000                                 TO RESP-IDMSG-ERROR                      
121100           MOVE NEJ              TO INDATA-SW                             
121200           PERFORM MFS-ROER-EJ-FAELT-4                                    
121300        END-IF                                                            
121400     ELSE                                                                 
121500        MOVE MFS-NUM-FAELT-FEL   TO RESP-VKORDBTO-FAKT-UPD-ATTR           
121600                                                                          
121700*       MOVE MFS-ADD-SAETT-CURSOR                                         
121800*                                TO RESP-VKORDBTO-FAKT-UPD-ATTR           
121900        MOVE MFS-NUM-FAELT-RAETT                                          
122000                                 TO RESP-VLORDBTO-FAKT-UPD-ATTR           
122100        MOVE MFS-ALFA-FAELT-RAETT                                         
122200                                 TO RESP-GODK-FAKT-ATTR                   
122300        MOVE ERR-CORR-HILITE-FLDS                                         
122400                                 TO RESP-IDMSG-ERROR                      
122500        MOVE NEJ                 TO INDATA-SW                             
122600        PERFORM MFS-ROER-EJ-FAELT-4                                       
122700     END-IF                                                               
122800     .                                                                    
122900     EJECT                                                                
123000 GD-KOLLA-GODK-FAKT SECTION.                                              
123100     MOVE 'GD-KOLLA-GODK-FAKT'   TO WS-SEKTION                            
123200     SKIP2                                                                
123300     IF REQU-GODK-FAKT = 'J' OR 'Y'                                       
123400        PERFORM IMS-GET-WL316501                                          
123500        PERFORM IMS-GET-WL316511                                          
123600        IF SEGMENT-FINNS                                                  
123700           IF 3165-3166-VKORDBTO-FAKT  > ZERO AND                         
123800              3165-3166-VLORDBTO-FAKT > ZERO                              
123900              MOVE MFS-ALFA-FAELT-RAETT                                   
124000                                 TO RESP-GODK-FAKT-ATTR                   
124100           ELSE                                                           
124200              IF 3165-3166-VKORDBTO-FAKT  > ZERO                          
124300                 CONTINUE                                                 
124400              ELSE                                                        
124500                 MOVE MFS-NUM-FAELT-FEL                                   
124600                                 TO RESP-VKORDBTO-FAKT-UPD-ATTR           
124700              END-IF                                                      
124800              IF 3165-3166-VLORDBTO-FAKT  > ZERO                          
124900                 CONTINUE                                                 
125000              ELSE                                                        
125100                 MOVE MFS-NUM-FAELT-FEL                                   
125200                                 TO RESP-VLORDBTO-FAKT-UPD-ATTR           
125300              END-IF                                                      
125400              MOVE MFS-ALFA-FAELT-FEL                                     
125500                                 TO RESP-GODK-FAKT-ATTR                   
125600              MOVE ERR-CORR-HILITE-FLDS                                   
125700                                 TO RESP-IDMSG-ERROR                      
125800              MOVE NEJ           TO INDATA-SW                             
125900              PERFORM MFS-ROER-EJ-FAELT-4                                 
126000              MOVE ALL-SPACE     TO RESP-GODK-FAKT                        
126100           END-IF                                                         
126200        ELSE                                                              
126300           MOVE MFS-ALFA-FAELT-FEL                                        
126400                                 TO RESP-GODK-FAKT-ATTR                   
126500           MOVE ERR-CORR-HILITE-FLDS                                      
126600                                 TO RESP-IDMSG-ERROR                      
126700           MOVE NEJ              TO INDATA-SW                             
126800           PERFORM MFS-ROER-EJ-FAELT-4                                    
126900        END-IF                                                            
127000     ELSE                                                                 
127100        MOVE MFS-ALFA-FAELT-FEL  TO RESP-GODK-FAKT-ATTR                   
127200*       MOVE MFS-ADD-SAETT-CURSOR                                         
127300*                                TO RESP-GODK-FAKT-ATTR                   
127400        MOVE ERR-CORR-HILITE-FLDS                                         
127500                                 TO RESP-IDMSG-ERROR                      
127600        MOVE NEJ                 TO INDATA-SW                             
127700        PERFORM MFS-ROER-EJ-FAELT-4                                       
127800     END-IF                                                               
127900     .                                                                    
128000     EJECT                                                                
128100 H-UPPDATERA SECTION.                                                     
128200     MOVE 'H-UPPDATERA'          TO WS-SEKTION                            
128300     MOVE 'NEJ'                  TO FAKTURA-SW                            
128400     PERFORM S03-LAS-FAKTURA                                              
128500                                                                          
128600     IF REQU-NYRAD = ALL '+' AND                                          
128700        REQU-BORT = ALL '+'  AND                                          
128800        REQU-GODKRAD = ALL '+'                                            
128900        MOVE NEJ                 TO INDATA-SW                             
129000        MOVE PF11-AND-NO-CHANGE  TO RESP-IDMSG-ERROR                      
129100     END-IF                                                               
129200     IF INDATA-OK                                                         
129300        IF REQU-GODKRAD NOT = ALL '+'                                     
129400           PERFORM IMS-GET-WL316501                                       
129500           PERFORM IMS-GET-WL316511                                       
129600           MOVE 3165-3166-IDBYTFAK                                        
129700                                 TO W-IDBYTFAK                            
129800           IF REQU-VKORDBTO-FAKT-UPD NOT = ALL '+'                        
129900              PERFORM IMS-GHU-WL316511                                    
130000              IF NDC-NA                                                   
130100                 COMPUTE WS-VKORDBTO ROUNDED =                            
130200                         WS-VKORDBTO * CONV-LB-TO-KG                      
130300              END-IF                                                      
130400              MOVE WS-VKORDBTO   TO 3165-3166-VKORDBTO-FAKT               
130500                                                                          
130600              PERFORM IMS-REPL-WL316611                                   
130700              MOVE JA            TO UPPDATE-DONE                          
130800           END-IF                                                         
130900           IF REQU-VLORDBTO-FAKT-UPD NOT = ALL '+'                        
131000              PERFORM IMS-GHU-WL316511                                    
131100              IF NDC-NA                                                   
131200                 COMPUTE WS-VLORDBTO ROUNDED =                            
131300                         WS-VLORDBTO-INP * CONV-FT3-TO-M3                 
131310                 IF WS-VLORDBTO > 9999.999                                
131320                    MOVE 'WS-VLORDBTO > 9999.999' TO FELTEXT              
131330                    CALL FELLOG                                           
131340                 END-IF                                                   
131400                 MOVE WS-VLORDBTO                                         
131500                                 TO 3165-3166-VLORDBTO-FAKT               
131600              ELSE                                                        
131700                 MOVE WS-VLORDBTO-INP                                     
131800                                 TO 3165-3166-VLORDBTO-FAKT               
131900              END-IF                                                      
132000                                                                          
132100              PERFORM IMS-REPL-WL316611                                   
132200              MOVE JA            TO UPPDATE-DONE                          
132300           END-IF                                                         
132400           IF REQU-GODK-FAKT NOT = ALL '+'                                
132500              IF REQU-GODK-FAKT = 'J' OR 'Y'                              
132600                 MOVE ZERO       TO SPAR-IDARTNR                          
132700                                    W-TOT-KVLEVART                        
132800                 MOVE +1         TO TABINDX                               
132900                 PERFORM S04-NOLLSTALL-TABELL                             
133000                 PERFORM IMS-GET-WL316521-F                               
133100                 PERFORM UNTIL SEGMENT-SAKNAS                             
133200                    MOVE 3165-3168-IDARTNR-OBJ                            
133300                                 TO SPAR-IDARTNR                          
133400                    PERFORM S05-SKAPA-TABELL                              
133500                    PERFORM IMS-GET-WL316521                              
133600                 END-PERFORM                                              
133700                                                                          
133800                 MOVE ZERO       TO SPAR-KVLS                             
133900                 PERFORM S06-HAMTA-TABELL                                 
134000************** OBS TILLFÄLLIG ÄNDRING *******************                 
134100****************IF LAGERSALDO-FEL                                         
134200************* LAGERSALDO ÄR FÖR LITET **********                          
134300****************   MOVE FEL-IDARTNR       TO W-IDARTNR-MIN                
134400****************                             W-IDARTNR-MAX                
134500****************                             W-IDARTNR-KEY                
134600****************   MOVE MFS-ALFA-FAELT-FEL TO                             
134700****************                        RESP-GODK-FAKT-ATTR               
134800****************   MOVE BASE-ON-HAND-TO-LOW TO MED-IDMFSFEL               
134900****************   MOVE NEJ TO INDATA-SW                                  
135000****************   CALL WMEDKONV USING MED-WMEDAREA                       
135100****************   MOVE MED-MFSFEL TO MOD-TEMFSFEL                        
135200****************   PERFORM MFS-ROER-EJ-FAELT-UT                           
135300****************ELSE                                                      
135400                 PERFORM IMS-GU-FAKTURAN                                  
135500                 IF SEGMENT-FINNS                                         
135600                    PERFORM S07-UPPDAT-LAGERSALDO                         
135700                    PERFORM IMS-GHU-WL316511                              
135800                    IF SEGMENT-FINNS                                      
135900                       MOVE '2'  TO 3165-3166-KDBYTFAK                    
136000                       MOVE DAGENS-DATUM2                                 
136100                                 TO 3165-3166-DAFAKT                      
136200                       MOVE REQU-IDUSER                                   
136300                                 TO 3165-3166-IDUSER                      
136400                       PERFORM IMS-REPL-WL316611                          
136500                       PERFORM S01-STARTA-NYTT-JOBB-VIA-SOP               
136600                       MOVE 'JA' TO FAKTURA-SW                            
136700                       MOVE JA   TO UPPDATE-DONE                          
136800                    ELSE                                                  
136900                       MOVE MFS-ALFA-FAELT-FEL                            
137000                                 TO RESP-GODK-FAKT-ATTR                   
137100                       MOVE MISSING-INVOICE                               
137200                                 TO RESP-IDMSG-ERROR                      
137300                       MOVE 'IDFAKT'                                      
137400                                 TO RESP-IDELMT-ERROR                     
137500                       MOVE NEJ  TO INDATA-SW                             
137600                       PERFORM MFS-ROER-EJ-FAELT-UT                       
137700                    END-IF                                                
137800                 ELSE                                                     
137900                    MOVE MFS-ALFA-FAELT-FEL                               
138000                                 TO RESP-GODK-FAKT-ATTR                   
138100                    MOVE MISSING-INVOICE                                  
138200                                 TO RESP-IDMSG-ERROR                      
138300                    MOVE 'IDFAKT'                                         
138400                                 TO RESP-IDELMT-ERROR                     
138500                    MOVE NEJ     TO INDATA-SW                             
138600                    PERFORM MFS-ROER-EJ-FAELT-UT                          
138700                END-IF                                                    
138800****************END-IF                                                    
138900             ELSE                                                         
139000                MOVE MFS-ALFA-FAELT-FEL                                   
139100                                 TO  RESP-GODK-FAKT-ATTR                  
139200                MOVE ERR-CORR-HILITE-FLDS                                 
139300                                 TO RESP-IDMSG-ERROR                      
139400                MOVE NEJ         TO INDATA-SW                             
139500                PERFORM MFS-ROER-EJ-FAELT-UT                              
139600             END-IF                                                       
139700          END-IF                                                          
139800       END-IF                                                             
139900     END-IF                                                               
140000                                                                          
140100     IF REQU-IDARTNR-OBJ-UPPD = ALL '+' AND                               
140200        REQU-KVRETUR-UPPD = ALL '+'                                       
140300        CONTINUE                                                          
140400     ELSE                                                                 
140500        IF INDATA-OK                                                      
140600           PERFORM HA-UPPDATERA                                           
140700        END-IF                                                            
140800     END-IF                                                               
140900                                                                          
141000     IF INDATA-OK                                                         
141100        IF REQU-IDARTNR-OBJ-BORT = ALL '+' AND                            
141200           REQU-KVRETUR-BORT    = ALL '+' AND                             
141300           REQU-IDBYTKOL-BORT   = ALL '+'                                 
141400           CONTINUE                                                       
141500        ELSE                                                              
141600           PERFORM HB-UPPDATERA                                           
141700        END-IF                                                            
141800     END-IF                                                               
141900                                                                          
142000     IF INDATA-OK                                                         
142100        MOVE INF-UPDATE-DONE     TO RESP-IDMSG-INFO                       
142200        PERFORM MFS-FORM-ATTR                                             
142300        PERFORM MFS-RENSA-FAELT-IN                                        
142400     ELSE                                                                 
142500        MOVE ERR-NO-UPPDATE-DONE TO RESP-IDMSG-INFO                       
142600        PERFORM MFS-ROER-EJ-FAELT-UT                                      
142700        MOVE NEJ                 TO INDATA-SW                             
142800     END-IF                                                               
142900                                                                          
143000                                                                          
143100     .                                                                    
143200     EJECT                                                                
143300 HA-UPPDATERA SECTION.                                                    
143400     MOVE 'HA-UPPDATERA'         TO WS-SEKTION                            
143500                                                                          
143600     MOVE REQU-IDARTNR-OBJ-UPPD  TO W-IDARTNR-ART                         
143700                                    W-IDARTNR                             
143800                                    W-IDARTNR-3168                        
143900     MOVE ZERO                   TO WS-KVLS                               
144000     MOVE ZERO                   TO WS-IDBYTKOL-1                         
144100     MOVE ZERO                   TO WS-KVLEVART                           
144200     MOVE ZERO                   TO WS-KVRETUR                            
144300     MOVE REQU-KVRETUR-UPPD      TO WS-KVLEVART                           
144400     ADD WS-KVLEVART             TO WS-KVLS                               
144500     PERFORM IMS-GET-ARTS-WLARTS11                                        
144600     IF SEGMENT-FINNS                                                     
144700        MOVE ARTS-SLAG-KVLS      TO RESP-KVLS                             
144800        PERFORM IMS-GET-WL316501                                          
144900        PERFORM IMS-GET-WL316511                                          
145000        IF SEGMENT-FINNS                                                  
145100           MOVE 3165-3166-IDBYTFAK                                        
145200                                 TO W-IDBYTFAK                            
145300***********************************************************               
145400*** BERÄKNAR ANTALET ARTIKLAR FÖR ETT SPECIELLT ***********               
145500*** ARTIKELNR PÅ EN PROFORMA FAKTURA            ***********               
145600***********************************************************               
145700           PERFORM IMS-GNP-WL316521                                       
145800           PERFORM UNTIL SEGMENT-SAKNAS                                   
145900              IF W-IDARTNR = 3165-3168-IDARTNR-OBJ                        
146000                 ADD 3165-3168-KVLEVART                                   
146100                                 TO WS-KVLS                               
146200              END-IF                                                      
146300              MOVE 3165-3168-IDBYTKOL                                     
146400                                 TO WS-IDBYTKOL-1                         
146500              PERFORM IMS-GNP-WL316521                                    
146600           END-PERFORM                                                    
146700                                                                          
146800           IF ARTS-SLAG-KVLS < WS-KVLS                                    
146900              MOVE MFS-NUM-FAELT-FEL                                      
147000                                 TO RESP-KVRETUR-UPPD-ATTR                
147100              MOVE WRONG-AMOUNT  TO RESP-IDMSG-ERROR                      
147200              MOVE NEJ           TO INDATA-SW                             
147300              PERFORM MFS-ROER-EJ-FAELT-2                                 
147400           ELSE                                                           
147500              IF REQU-FLBYTKNR-UPPD = 'J' OR 'Y'                          
147600                 ADD +1          TO WS-IDBYTKOL-1                         
147700                 MOVE WS-IDBYTKOL-1                                       
147800                                 TO 3165-3168-IDBYTKOL                    
147900                 MOVE REQU-IDARTNR-OBJ-UPPD                               
148000                                 TO 3165-3168-IDARTNR-OBJ                 
148100                 MOVE REQU-KVRETUR-UPPD                                   
148200                                 TO 3165-3168-KVLEVART                    
148300                 PERFORM IMS-ISRT-WL316521                                
148400                 MOVE JA         TO UPPDATE-DONE                          
148500                 MOVE RESP-IDARTNR-OBJ-START                              
148600                                 TO WS-IDARTNR-NUM                        
148700                 MOVE WS-IDARTNR-NUM                                      
148800                                 TO W-IDARTNR-MIN                         
148900                                    W-IDARTNR-KEY                         
149000                 MOVE WS-IDBYTKOL-1                                       
149100                                 TO W-IDBYTKOL-MIN                        
149200*                                                                         
149300              ELSE                                                        
149400                 IF KOLLI-FINNS                                           
149500                    PERFORM S09-KOLLA-REQU-IDBYTKOL                       
149600                    IF SEGMENT-FINNS                                      
149700                       MOVE  W-IDBYTKOL                                   
149800                                 TO W-IDBYTKOL-3168                       
149900                       MOVE  W-IDARTNR                                    
150000                                 TO W-IDARTNR-3168                        
150100                       PERFORM IMS-GHU-WL316521                           
150200                       IF SEGMENT-FINNS                                   
150300                          MOVE REQU-KVRETUR-UPPD                          
150400                                 TO WS-KVRETUR                            
150500                          ADD WS-KVRETUR                                  
150600                                 TO 3165-3168-KVLEVART                    
150700                          PERFORM IMS-REPL-WL3168                         
150800                          MOVE JA                                         
150900                                 TO UPPDATE-DONE                          
151000*                                                                         
151100                          MOVE W-IDARTNR                                  
151200                                 TO W-IDARTNR-MIN                         
151300                                    W-IDARTNR-KEY                         
151400                          MOVE WS-IDBYTKOL                                
151500                                 TO W-IDBYTKOL-MIN                        
151600*                                                                         
151700                       ELSE                                               
151800                          MOVE W-IDBYTKOL                                 
151900                                 TO 3165-3168-IDBYTKOL                    
152000                          MOVE REQU-IDARTNR-OBJ-UPPD                      
152100                                 TO 3165-3168-IDARTNR-OBJ                 
152200                          MOVE REQU-KVRETUR-UPPD                          
152300                                 TO 3165-3168-KVLEVART                    
152400                          PERFORM IMS-ISRT-WL316521                       
152500                          MOVE JA                                         
152600                                 TO UPPDATE-DONE                          
152700*                                                                         
152800                          MOVE REQU-IDARTNR-OBJ-UPPD                      
152900                                 TO W-IDARTNR-MIN                         
153000                                    W-IDARTNR-KEY                         
153100                          MOVE W-IDBYTKOL                                 
153200                                 TO W-IDBYTKOL-MIN                        
153300*                                                                         
153400                       END-IF                                             
153500                    ELSE                                                  
153600                       MOVE MFS-ALFA-FAELT-FEL                            
153700                                 TO RESP-FLBYTKNR-UPPD-ATTR               
153800                       MOVE MISSING-KOLLI                                 
153900                                 TO RESP-IDMSG-ERROR                      
154000                       MOVE 'IDKOLLI'                                     
154100                                 TO RESP-IDELMT-ERROR                     
154200                       PERFORM MFS-ROER-EJ-FAELT-2                        
154300                       MOVE NEJ  TO INDATA-SW                             
154400                    END-IF                                                
154500                 ELSE                                                     
154600*************************************************************             
154700****** KOLLI SAKNAS DVS DET ÄR NOLL *************************             
154800                    IF WS-IDBYTKOL-1 > ZERO                               
154900                       CONTINUE                                           
155000                    ELSE                                                  
155100                       ADD +1    TO WS-IDBYTKOL-1                         
155200                    END-IF                                                
155300                    MOVE WS-IDBYTKOL-1                                    
155400                                 TO W-IDBYTKOL-3168                       
155500                    MOVE REQU-IDARTNR-OBJ-UPPD                            
155600                                 TO W-IDARTNR-3168                        
155700                    PERFORM IMS-GHU-WL316521                              
155800                    IF SEGMENT-FINNS                                      
155900                       MOVE REQU-KVRETUR-UPPD                             
156000                                 TO WS-KVRETUR                            
156100                       ADD WS-KVRETUR                                     
156200                                 TO  3165-3168-KVLEVART                   
156300                       PERFORM IMS-REPL-WL3168                            
156400                       MOVE JA   TO UPPDATE-DONE                          
156500                    ELSE                                                  
156600                       MOVE WS-IDBYTKOL-1                                 
156700                                 TO 3165-3168-IDBYTKOL                    
156800                       MOVE REQU-IDARTNR-OBJ-UPPD                         
156900                                 TO 3165-3168-IDARTNR-OBJ                 
157000                       MOVE REQU-KVRETUR-UPPD                             
157100                                 TO 3165-3168-KVLEVART                    
157200                       PERFORM IMS-ISRT-WL316521                          
157300                       MOVE JA   TO UPPDATE-DONE                          
157400                    END-IF                                                
157500*                                                                         
157600                    MOVE W-IDARTNR                                        
157700                                 TO W-IDARTNR-MIN                         
157800                                    W-IDARTNR-KEY                         
157900                    MOVE WS-IDBYTKOL-1                                    
158000                                 TO W-IDBYTKOL-MIN                        
158100*                                                                         
158200                 END-IF                                                   
158300              END-IF                                                      
158400           END-IF                                                         
158500        ELSE                                                              
158600           MOVE MFS-NUM-FAELT-FEL                                         
158700                                 TO RESP-IDARTNR-OBJ-UPPD-ATTR            
158800           MOVE MISSING-INVOICE  TO RESP-IDMSG-ERROR                      
158900           MOVE 'IDFAKT'         TO RESP-IDELMT-ERROR                     
159000           PERFORM MFS-ROER-EJ-FAELT-2                                    
159100           MOVE NEJ              TO INDATA-SW                             
159200        END-IF                                                            
159300     ELSE                                                                 
159400        MOVE MFS-NUM-FAELT-FEL   TO RESP-IDARTNR-OBJ-UPPD-ATTR            
159500        MOVE PARTNO-MISSING      TO RESP-IDMSG-ERROR                      
159600        MOVE 'IDARTNR'           TO RESP-IDELMT-ERROR                     
159700        PERFORM MFS-ROER-EJ-FAELT-2                                       
159800        MOVE NEJ                 TO INDATA-SW                             
159900     END-IF                                                               
160000                                                                          
160100     .                                                                    
160200     EJECT                                                                
160300 HB-UPPDATERA SECTION.                                                    
160400     MOVE 'HB-UPPDATERA'         TO WS-SEKTION                            
160500                                                                          
160600     MOVE REQU-IDARTNR-OBJ-BORT  TO W-IDARTNR-ART                         
160700                                    W-IDARTNR                             
160800                                    W-IDARTNR-3168                        
160900     MOVE REQU-IDBYTKOL-BORT     TO W-IDBYTKOL-3168                       
161000                                                                          
161100     MOVE ZERO                   TO WS-KVRETUR                            
161200                                                                          
161300     MOVE REQU-KVRETUR-BORT      TO WS-KVRETUR                            
161400                                                                          
161500     PERFORM IMS-GET-ARTS-WLARTS11                                        
161600     IF SEGMENT-FINNS                                                     
161700        MOVE ARTS-SLAG-KVLS      TO RESP-KVLS                             
161800        PERFORM IMS-GET-WL316501                                          
161900        PERFORM IMS-GET-WL316511                                          
162000        MOVE 3165-3166-IDBYTFAK  TO W-IDBYTFAK                            
162100***********************************************************               
162200        PERFORM IMS-GHU-WL316521                                          
162300        IF SEGMENT-FINNS                                                  
162400           IF WS-KVRETUR > 3165-3168-KVLEVART                             
162500              MOVE MFS-NUM-FAELT-FEL                                      
162600                                 TO RESP-KVRETUR-BORT-ATTR                
162700              MOVE WRONG-AMOUNT  TO RESP-IDMSG-ERROR                      
162800              MOVE NEJ           TO INDATA-SW                             
162900              PERFORM MFS-ROER-EJ-FAELT-3                                 
163000           ELSE                                                           
163100              IF WS-KVRETUR = 3165-3168-KVLEVART                          
163200                 PERFORM IMS-DLET-WL3168                                  
163300                 MOVE JA         TO UPPDATE-DONE                          
163400              ELSE                                                        
163500                 COMPUTE 3165-3168-KVLEVART =                             
163600                    3165-3168-KVLEVART - WS-KVRETUR                       
163700                 PERFORM IMS-REPL-WL3168                                  
163800                 MOVE JA         TO UPPDATE-DONE                          
163900              END-IF                                                      
164000           END-IF                                                         
164100        ELSE                                                              
164200           MOVE MFS-NUM-FAELT-FEL                                         
164300                                 TO RESP-IDBYTKOL-BORT-ATTR               
164400           MOVE MISSING-KOLLI    TO RESP-IDMSG-ERROR                      
164500           MOVE 'IDKOLLI'        TO RESP-IDELMT-ERROR                     
164600           MOVE NEJ              TO INDATA-SW                             
164700        END-IF                                                            
164800     ELSE                                                                 
164900        MOVE MFS-NUM-FAELT-FEL   TO RESP-IDARTNR-OBJ-BORT-ATTR            
165000        MOVE PARTNO-MISSING      TO RESP-IDMSG-ERROR                      
165100        MOVE 'IDARTNR'           TO RESP-IDELMT-ERROR                     
165200        MOVE NEJ                 TO INDATA-SW                             
165300     END-IF                                                               
165400     .                                                                    
165500     EJECT                                                                
165600 S01-STARTA-NYTT-JOBB-VIA-SOP SECTION.                                    
165700     MOVE 'S01-STARTA-NYTT-JOBB-VIA-SOP'                                  
165800                                 TO WS-SEKTION                            
165900******************************************************************        
166000*               AKTIVERING AV RUTIN W371S5 I SOP                 *        
166100*          IDUSER SKICKAS MED SOM SYMBOLISK PARAMETER            *        
166200******************************************************************        
166300                                                                          
166400     MOVE '3183'                 TO MSGSOP-IDTRANS                        
166500     IF REQU-IDSPRAK = 'SV'                                               
166600       MOVE '1'                  TO MSGSOP-KDMFSFOR                       
166700     ELSE                                                                 
166800       MOVE '2'                  TO MSGSOP-KDMFSFOR                       
166900     END-IF                                                               
167000     MOVE 'W371S5'               TO MSGSOP-IDPROCESS                      
167100     MOVE 'O'                    TO MSGSOP-KDSOPFUNK                      
167200                                                                          
167300     STRING 'IDUSER(' REQU-IDUSER ')'                                     
167400            'IDDC(' REQU-IDDC ')'                                         
167500        DELIMITED BY SIZE INTO MSGSOP-TESYMBV                             
167600                                                                          
167700     PERFORM IMS-INSERT-ALTMSG                                            
167800     .                                                                    
167900     EJECT                                                                
168000 S02-LAS-FAKTNR-SEG SECTION.                                              
168100     MOVE 'S02-LAS-FAKTNR-SEG'   TO WS-SEKTION                            
168200                                                                          
168300     PERFORM IMS-GET-INVOICE                                              
168400*    INDEX 05  -  NDC  -   PROFORMOR                                      
168500*    FAKTURANR INTERVALLET GÅR MELLLAN 1 OCH 999                          
168600     ADD +1                      TO XXLD-4742-IDFAKT-AKT (05)             
168700     IF XXLD-4742-IDFAKT-AKT (05) > +0000999                              
168800        MOVE +0000001            TO XXLD-4742-IDFAKT-AKT (05)             
168900     END-IF                                                               
169000     PERFORM IMS-REPL-INVOICE-NO                                          
169100     MOVE XXLD-4742-IDFAKT-AKT (05)                                       
169200                                 TO 3165-3166-IDBYTFAK                    
169300     MOVE W-IDDISTR              TO 3165-3166-IDDISTR                     
169400     MOVE W-IDDC                 TO 3165-3166-IDDC                        
169500     MOVE '1'                    TO 3165-3166-KDBYTFAK                    
169600     MOVE DAGENS-DATUM2          TO 3165-3166-DAREGDAT                    
169700     MOVE DAGENS-DATUM2          TO 3165-3166-DAFAKT                      
169800     MOVE REQU-IDUSER            TO 3165-3166-IDUSER                      
169900     MOVE ZERO                   TO 3165-3166-VKORDBTO-FAKT               
170000     MOVE ZERO                   TO 3165-3166-VLORDBTO-FAKT               
170100     PERFORM IMS-ISRT-WL316511                                            
170200     .                                                                    
170300     EJECT                                                                
170400 S03-LAS-FAKTURA SECTION.                                                 
170500     MOVE 'S03-LAS-FAKTURA'      TO WS-SEKTION                            
170600                                                                          
170700     PERFORM IMS-GET-WL316501                                             
170800     PERFORM IMS-GET-WL316511                                             
170900     IF SEGMENT-FINNS                                                     
171000        CONTINUE                                                          
171100     ELSE                                                                 
171200        PERFORM S02-LAS-FAKTNR-SEG                                        
171300     END-IF                                                               
171400                                                                          
171500     .                                                                    
171600     EJECT                                                                
171700 S04-NOLLSTALL-TABELL SECTION.                                            
171800     MOVE 'S04-NOLLSTALL-TABELL' TO WS-SEKTION                            
171900                                                                          
172000     MOVE +1                     TO TABINDX                               
172100     PERFORM UNTIL TABINDX > 1000                                         
172200        MOVE 999999999           TO TAB-IDARTNR (TABINDX)                 
172300        MOVE ZERO                TO TAB-KVLS    (TABINDX)                 
172400        ADD +1                   TO TABINDX                               
172500     END-PERFORM                                                          
172600     .                                                                    
172700     EJECT                                                                
172800 S05-SKAPA-TABELL SECTION.                                                
172900     MOVE 'S05-SKAPA-TABELL'     TO WS-SEKTION                            
173000******************************************                                
173100*** TABRAD FINNS LÄGG UPPDATERA SALDO  ***                                
173200******************************************                                
173300     MOVE 'N'                    TO TABRAD-SW                             
173400     MOVE +1                     TO TABINDX                               
173500     PERFORM                                                              
173600       UNTIL TABINDX > 1000 OR                                            
173700             TAB-IDARTNR (TABINDX) = 999999999 OR                         
173800             TABRAD-FINNS                                                 
173900        IF TAB-IDARTNR (TABINDX) = SPAR-IDARTNR                           
174000           ADD 3165-3168-KVLEVART                                         
174100                                 TO TAB-KVLS(TABINDX)                     
174200           MOVE 'J'              TO TABRAD-SW                             
174300           MOVE TABINDX          TO WS-TABINDX                            
174400        END-IF                                                            
174500        ADD +1                   TO TABINDX                               
174600     END-PERFORM                                                          
174700                                                                          
174800     IF TABRAD-SAKNAS                                                     
174900**********************************                                        
175000*** TABRAD SAKNAS LÄGG NY RAD  ***                                        
175100**********************************                                        
175200        MOVE +1                  TO TABINDX                               
175300        PERFORM                                                           
175400          UNTIL TABINDX > 1000 OR                                         
175500                TAB-IDARTNR (TABINDX) = 999999999                         
175600           ADD +1                TO TABINDX                               
175700        END-PERFORM                                                       
175800        IF TABINDX > 1000                                                 
175900           MOVE 'TABINDX > 1000 UTÖKA TABELLEN'                           
176000                                 TO FELTEXT                               
176100           CALL FELLOG                                                    
176200        ELSE                                                              
176300           MOVE TABINDX          TO WS-TABINDX                            
176400           MOVE SPAR-IDARTNR     TO TAB-IDARTNR(TABINDX)                  
176500           MOVE 3165-3168-KVLEVART                                        
176600                                 TO TAB-KVLS(TABINDX)                     
176700        END-IF                                                            
176800     END-IF                                                               
176900                                                                          
177000     .                                                                    
177100     EJECT                                                                
177200 S06-HAMTA-TABELL SECTION.                                                
177300     MOVE 'S06-HAMTA-TABELL'     TO WS-SEKTION                            
177400                                                                          
177500     MOVE JA                     TO LAGERSALDO-SW                         
177600     MOVE ZERO                   TO FEL-IDARTNR                           
177700     MOVE +1                     TO TABINDX                               
177800     PERFORM                                                              
177900       UNTIL TABINDX > 1000 OR                                            
178000             TAB-IDARTNR (TABINDX) = 999999999 OR                         
178100             LAGERSALDO-FEL                                               
178200        MOVE TAB-IDARTNR (TABINDX)                                        
178300                                 TO W-IDARTNR-ART                         
178400        PERFORM IMS-GET-ARTS-WLARTS11                                     
178500        MOVE TAB-KVLS (TABINDX)  TO SPAR-KVLS                             
178600        IF ARTS-SLAG-KVLS < SPAR-KVLS                                     
178700           MOVE NEJ              TO LAGERSALDO-SW                         
178800           MOVE TAB-IDARTNR (TABINDX)                                     
178900                                 TO FEL-IDARTNR                           
179000        END-IF                                                            
179100        ADD +1                   TO TABINDX                               
179200     END-PERFORM                                                          
179300                                                                          
179400     .                                                                    
179500     EJECT                                                                
179600 S07-UPPDAT-LAGERSALDO SECTION.                                           
179700     MOVE 'S07-UPPDAT-LAGERSALDO'                                         
179800                                 TO WS-SEKTION                            
179900                                                                          
180000     PERFORM IMS-GNP-FAKTURARAD-FIRST                                     
180100     PERFORM                                                              
180200       UNTIL SEGMENT-SAKNAS                                               
180300        MOVE 3165-3168-IDARTNR-OBJ                                        
180400                                 TO W-IDARTNR-ART                         
180500                                                                          
180600        PERFORM IMS-GET-ARTS-WLARTS11                                     
180700        IF SEGMENT-FINNS                                                  
180800           MOVE 3165-3168-KVLEVART                                        
180900                                 TO SPAR-KVLS                             
181000           COMPUTE ARTS-SLAG-KVLS =                                       
181100                   ARTS-SLAG-KVLS - SPAR-KVLS                             
181200           PERFORM IMS-REPL-ARTS                                          
181300           PERFORM S071-SKAPA-SALDOLOGG                                   
181400        END-IF                                                            
181500        PERFORM IMS-GNP-FAKTURARAD                                        
181600     END-PERFORM                                                          
181700                                                                          
181800     .                                                                    
181900     EJECT                                                                
182000 S071-SKAPA-SALDOLOGG SECTION.                                            
182100     MOVE 'S071-SKAPA-SALDOLOGG' TO WS-SEKTION                            
182200                                                                          
182300                                                                          
182400     MOVE W-IDARTNR-ART          TO LOGG-IDARTNR                          
182500     MOVE 9                      TO LOGG-IDSEKVNR                         
182600     MOVE W-IDDC                 TO LOGG-IDDC                             
182700     MOVE 'EXCH'                 TO LOGG-IDHUVTYP                         
182800     MOVE 'OBJ'                  TO LOGG-IDSUBTYP                         
182900     MOVE 'W3018300'             TO LOGG-IDPGM                            
183000     MOVE '3183'                 TO LOGG-IDTRANS                          
183100     MOVE REQU-IDUSER            TO LOGG-IDUSER                           
183200     MOVE SPACE                  TO LOGG-REF                              
183300     MOVE W-IDDISTR              TO LOGG-IDDISTR                          
183400     MOVE REQU-IDBYTKOL-KEY      TO LOGG-IDKOLLI                          
183500     MOVE SPACE                  TO LOGG-IDTECKEN-KVAKS                   
183600     MOVE SPACE                  TO LOGG-IDTECKEN-KVAKS-PAV               
183700     MOVE SPACE                  TO LOGG-IDTECKEN-KVEFRS                  
183800     MOVE '-'                    TO LOGG-IDTECKEN-KVLS                    
183900     MOVE SPAR-KVLS              TO LOGG-KVART-SALDO                      
184000     MOVE ARTS-SLAG-KVAKS-SDC    TO LOGG-KVAKS                            
184100     MOVE ARTS-SLAG-KVAKS-PAV    TO LOGG-KVAKS-PAV                        
184200     MOVE ARTS-SLAG-KVEFRS       TO LOGG-KVEFRS                           
184300     MOVE ARTS-SLAG-KVLS         TO LOGG-KVLS                             
184400     MOVE ZERO                   TO LOGG-DAREGDAT-LADD                    
184500     MOVE FUNCTION CURRENT-DATE(1:8)                                      
184600                                 TO LOGG-DATUM                            
184700     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
184800     ACCEPT TRANS-TID          FROM TIME                                  
184900     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - TRANS-TID                  
185000     PERFORM IMS-ISRT-WDL9                                                
185100     IF SEGMENT-FINNS-REDAN                                               
185200        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
185300           ADD -1                TO LOGG-IDSEKVNR                         
185400           PERFORM IMS-ISRT-WDL9                                          
185500        END-PERFORM                                                       
185600     END-IF                                                               
185700     .                                                                    
185800     EJECT                                                                
185900                                                                          
186000 S08-REDIGERA-VLORDBTO SECTION.                                           
186100     MOVE 'S08-REDIGERA-VLORDBTO'                                         
186200                                 TO WS-SEKTION                            
186300                                                                          
186400     IF REQU-VLORDBTO-FAKT-UPD NOT = ALL '+'                              
186500        MOVE REQU-VLORDBTO-FAKT-UPD                                       
186600                                 TO DEC-IDFRIDATA                         
186700        MOVE +6                  TO DEC-KVHELTAL                          
186800        MOVE +1                  TO DEC-KVDECIMAL                         
186900                                                                          
187000        CALL WDECEDIT USING DEC-WDECAREA                                  
187100                                                                          
187200        IF  DEC-KDSVAR-OK                                                 
187300           MOVE DEC-IDEDITDATA   TO WS-VLORDBTO-INP                       
187400           MOVE MFS-NUM-FAELT-RAETT                                       
187500                                 TO RESP-VLORDBTO-FAKT-UPD-ATTR           
187600        ELSE                                                              
187700           MOVE MFS-NUM-FAELT-FEL                                         
187800                                 TO RESP-VLORDBTO-FAKT-UPD-ATTR           
187900           MOVE MFS-ALFA-FAELT-RAETT                                      
188000                                 TO RESP-GODK-FAKT-ATTR                   
188100           MOVE ERR-CORR-HILITE-FLDS                                      
188200                                 TO RESP-IDMSG-ERROR                      
188300           MOVE NEJ              TO INDATA-SW                             
188400           PERFORM MFS-ROER-EJ-FAELT-4                                    
188500        END-IF                                                            
188600     END-IF                                                               
188700     .                                                                    
188800     EJECT                                                                
188900 S09-KOLLA-REQU-IDBYTKOL SECTION.                                         
189000     MOVE 'S09-KOLLA-REQU-IDBYTKOL'                                       
189100                                 TO WS-SEKTION                            
189200     SKIP2                                                                
189300     MOVE  W-IDBYTKOL            TO W-IDBYTKOL-MIN-F                      
189400                                    W-IDBYTKOL-MAX-F                      
189500     MOVE  ZERO                  TO W-IDARTNR-MIN-F                       
189600     MOVE  +999999999            TO W-IDARTNR-MAX-F                       
189700     PERFORM IMS-GET-WL316521-FIRST                                       
189800     .                                                                    
189900     EJECT                                                                
190000 S10-REDIGERA-VIKT SECTION.                                               
190100     MOVE 'S10-REDIGERA-VIKT'    TO WS-SEKTION                            
190200                                                                          
190300     IF REQU-VKORDBTO-FAKT-UPD NOT = ALL '+'                              
190400        MOVE REQU-VKORDBTO-FAKT-UPD                                       
190500                                 TO DEC-IDFRIDATA                         
190600        MOVE +6                  TO DEC-KVHELTAL                          
190700        MOVE +1                  TO DEC-KVDECIMAL                         
190800                                                                          
190900        CALL WDECEDIT USING DEC-WDECAREA                                  
191000                                                                          
191100        IF DEC-KDSVAR-OK                                                  
191200           MOVE DEC-IDEDITDATA   TO WS-VKORDBTO                           
191300           MOVE MFS-NUM-FAELT-RAETT                                       
191400                                 TO RESP-VKORDBTO-FAKT-UPD-ATTR           
191500        ELSE                                                              
191600           MOVE MFS-NUM-FAELT-FEL                                         
191700                                 TO RESP-VKORDBTO-FAKT-UPD-ATTR           
191800           MOVE ERR-CORR-HILITE-FLDS                                      
191900                                 TO RESP-IDMSG-ERROR                      
192000           MOVE NEJ              TO INDATA-SW                             
192100           PERFORM MFS-ROER-EJ-FAELT-4                                    
192200        END-IF                                                            
192300     END-IF                                                               
192400     .                                                                    
192500     EJECT                                                                
192600 MFS-RENSA-FAELT-IN SECTION.                                              
192700     MOVE 'MFS-RENSA-FAELT-IN'   TO WS-SEKTION                            
192800                                                                          
192900     MOVE +1 TO INDX                                                      
193000     PERFORM                                                              
193100     VARYING INDX FROM +1 BY +1                                           
193200       UNTIL INDX > MAX-KVRADER                                           
193300        MOVE ALL-SPACE           TO RESP-KVRETUR-LINE     (INDX)          
193400                                    RESP-IDBYTKOL-LINE    (INDX)          
193500                                    RESP-IDARTNR-OBJ-LINE (INDX)          
193600                                    RESP-BEART-LINE       (INDX)          
193700        IF REQU-IDMSGVER = '001'                                          
193800           MOVE ALL-UTF8-SPACE   TO RESP-BEART-LINE       (INDX)          
193900        END-IF                                                            
194000     END-PERFORM                                                          
194100                                                                          
194200     MOVE ALL-SPACE              TO RESP-IDARTNR-OBJ-UPPD                 
194300                                    RESP-KVRETUR-UPPD                     
194400                                    RESP-FLBYTKNR-UPPD                    
194500                                    RESP-KVLS                             
194600                                    RESP-IDBYTFAK                         
194700                                    RESP-GODK-FAKT                        
194800                                    RESP-IDARTNR-OBJ-BORT                 
194900                                    RESP-KVRETUR-BORT                     
195000                                    RESP-IDBYTKOL-BORT                    
195100                                    RESP-VKORDBTO-FAKT-UPD                
195200                                    RESP-VLORDBTO-FAKT-UPD                
195300                                    RESP-VKORDBTO-FAKT-UT                 
195400                                    RESP-VLORDBTO-FAKT-UT                 
195500     .                                                                    
195600     EJECT                                                                
195700 MFS-ROER-EJ-FAELT-UT  SECTION.                                           
195800     MOVE 'MFS-ROER-EJ-FAELT-UT' TO WS-SEKTION                            
195900                                                                          
196000*    --- ALLA UTDATA-FÄLT                                                 
196100     MOVE ALL-PLUS               TO RESP-IDARTNR-OBJ-UPPD                 
196200                                    RESP-KVRETUR-UPPD                     
196300                                    RESP-FLBYTKNR-UPPD                    
196400                                    RESP-IDARTNR-OBJ-BORT                 
196500                                    RESP-KVRETUR-BORT                     
196600                                    RESP-IDBYTKOL-BORT                    
196700                                    RESP-GODK-FAKT                        
196800                                    RESP-VKORDBTO-FAKT-UPD                
196900                                    RESP-VLORDBTO-FAKT-UPD                
197000     .                                                                    
197100     EJECT                                                                
197200 MFS-ROER-EJ-FAELT-2   SECTION.                                           
197300     MOVE 'MFS-ROER-EJ-FAELT-2'  TO WS-SEKTION                            
197400                                                                          
197500*    --- ALLA UTDATA-FÄLT                                                 
197600     MOVE ALL-PLUS               TO RESP-IDARTNR-OBJ-UPPD                 
197700                                    RESP-KVRETUR-UPPD                     
197800                                    RESP-FLBYTKNR-UPPD                    
197900     .                                                                    
198000     EJECT                                                                
198100 MFS-ROER-EJ-FAELT-3   SECTION.                                           
198200     MOVE 'MFS-ROER-EJ-FAELT-3'  TO WS-SEKTION                            
198300                                                                          
198400*    --- ALLA UTDATA-FÄLT                                                 
198500     MOVE ALL-PLUS               TO RESP-IDARTNR-OBJ-BORT                 
198600                                    RESP-KVRETUR-BORT                     
198700                                    RESP-IDBYTKOL-BORT                    
198800                                    RESP-GODK-FAKT                        
198900     .                                                                    
199000     EJECT                                                                
199100 MFS-ROER-EJ-FAELT-4   SECTION.                                           
199200     MOVE 'MFS-ROER-EJ-FAELT-4'  TO WS-SEKTION                            
199300                                                                          
199400*    --- ALLA UTDATA-FÄLT                                                 
199500     MOVE ALL-PLUS               TO RESP-GODK-FAKT                        
199600                                    RESP-VKORDBTO-FAKT-UPD                
199700                                    RESP-VLORDBTO-FAKT-UPD                
199800     .                                                                    
199900     EJECT                                                                
200000 MFS-FORM-ATTR SECTION.                                                   
200100     MOVE 'MFS-FORM-ATTR'        TO WS-SEKTION                            
200200                                                                          
200300*    --- ALLA INDATA-FÄLT                                                 
200400                                                                          
200500     MOVE MFS-FORMATETS-ATTR     TO RESP-IDARTNR-OBJ-BORT-ATTR            
200600                                    RESP-KVRETUR-BORT-ATTR                
200700                                    RESP-IDBYTKOL-BORT-ATTR               
200800                                    RESP-GODK-FAKT-ATTR                   
200900                                    RESP-IDARTNR-OBJ-UPPD-ATTR            
201000                                    RESP-KVRETUR-UPPD-ATTR                
201100                                    RESP-FLBYTKNR-UPPD-ATTR               
201200                                    RESP-VKORDBTO-FAKT-UPD-ATTR           
201300                                    RESP-VLORDBTO-FAKT-UPD-ATTR           
201400     .                                                                    
201500     SKIP2                                                                
201600* --- IMS SEKTIONER ---                                                   
201700     SKIP3                                                                
201800 IMS-GET-BENA SECTION.                                                    
201900     MOVE 'IMS-GET-BENA'          TO WS-IMS-SEKTION                       
202000     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
202100          DELIMITED BY SIZE INTO SSA1                                     
202200     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
202300          DELIMITED BY SIZE INTO SSA2                                     
202400     MOVE '  GE' TO GODK-STATUSKODER                                      
202500     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
202600     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
202700     PERFORM IMS-STATUSKONTROLL                                           
202800     .                                                                    
202900     SKIP2                                                                
203000 IMS-GET-INVOICE SECTION.                                                 
203100     MOVE 'IMS-GET-INVOICE'       TO WS-IMS-SEKTION                       
203200                                                                          
203300     STRING 'WLXXLD01(WDGXKEY  =' W-WDGX4741-X ')'                        
203400          DELIMITED BY SIZE INTO SSA1                                     
203500     MOVE 'WLXXLD11 ' TO SSA2                                             
203600     MOVE '  GE' TO GODK-STATUSKODER                                      
203700     CALL CBLTDLI USING GHU XXLD-PCB DLI-IO-AREA2 SSA1 SSA2               
203800     MOVE XXLD-STATUS-CODE TO STATUS-WS                                   
203900     PERFORM IMS-STATUSKONTROLL                                           
204000     .                                                                    
204100     SKIP3                                                                
204200 IMS-REPL-INVOICE-NO SECTION.                                             
204300     MOVE 'IMS-REPL-INVOICE-NO'    TO WS-IMS-SEKTION                      
204400                                                                          
204500     MOVE '  ' TO GODK-STATUSKODER                                        
204600     CALL CBLTDLI USING REPL XXLD-PCB DLI-IO-AREA2                        
204700     MOVE XXLD-STATUS-CODE TO STATUS-WS                                   
204800     PERFORM IMS-STATUSKONTROLL                                           
204900     .                                                                    
205000     EJECT                                                                
205100 IMS-GET-WL316501 SECTION.                                                
205200     MOVE 'IMS-GET-WL316501'      TO WS-IMS-SEKTION                       
205300     STRING 'WL316501(WDGXKEY  =' W-WDGXKEY-X ')'                         
205400          DELIMITED BY SIZE INTO SSA1                                     
205500     MOVE '  ' TO GODK-STATUSKODER                                        
205600     CALL CBLTDLI USING GU 3165-PCB DLI-IO-AREA5 SSA1                     
205700     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
205800     PERFORM IMS-STATUSKONTROLL                                           
205900     .                                                                    
206000     EJECT                                                                
206100 IMS-GET-WL316511 SECTION.                                                
206200     MOVE 'IMS-GET-WL316511'      TO WS-IMS-SEKTION                       
206300     STRING 'WL316511(IDDISTR  =' W-IDDISTR-X                             
206400                   '&IDDC     =' W-IDDC-X                                 
206500                   '&KDBYTFAK =' W-KDBYTFAK-X ')'                         
206600          DELIMITED BY SIZE INTO SSA1                                     
206700     MOVE '  GE' TO GODK-STATUSKODER                                      
206800     CALL CBLTDLI USING GNP 3165-PCB DLI-IO-AREA5 SSA1                    
206900     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
207000     PERFORM IMS-STATUSKONTROLL                                           
207100     .                                                                    
207200     EJECT                                                                
207300 IMS-GHU-WL316511 SECTION.                                                
207400     MOVE 'IMS-GHU-WL316511'      TO WS-IMS-SEKTION                       
207500     STRING 'WL316501(WDGXKEY  =' W-WDGXKEY-X ')'                         
207600          DELIMITED BY SIZE INTO SSA1                                     
207700     STRING 'WL316511(IDBYTFAK =' W-IDBYTFAK-X ')'                        
207800          DELIMITED BY SIZE INTO SSA2                                     
207900     MOVE '  ' TO GODK-STATUSKODER                                        
208000     CALL CBLTDLI USING GHU 3165-PCB DLI-IO-AREA5 SSA1 SSA2               
208100     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
208200     PERFORM IMS-STATUSKONTROLL                                           
208300     .                                                                    
208400     EJECT                                                                
208500 IMS-GU-FAKTURAN SECTION.                                                 
208600     MOVE 'IMS-GU-FAKTURAN '      TO WS-IMS-SEKTION                       
208700     STRING 'WL316501(WDGXKEY  =' W-WDGXKEY-X ')'                         
208800          DELIMITED BY SIZE INTO SSA1                                     
208900     STRING 'WL316511(IDBYTFAK =' W-IDBYTFAK-X ')'                        
209000          DELIMITED BY SIZE INTO SSA2                                     
209100     MOVE '  ' TO GODK-STATUSKODER                                        
209200     CALL CBLTDLI USING GU 3165-PCB DLI-IO-AREA5 SSA1 SSA2                
209300     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
209400     PERFORM IMS-STATUSKONTROLL                                           
209500     .                                                                    
209600     EJECT                                                                
209700 IMS-GNP-FAKTURARAD-FIRST SECTION.                                        
209800     MOVE 'IMS-GNP-FAKTURARAD-FIRST' TO WS-IMS-SEKTION                    
209900                                                                          
210000     STRING 'WL316511(IDBYTFAK =' W-IDBYTFAK-X ')'                        
210100          DELIMITED BY SIZE INTO SSA1                                     
210200     MOVE 'WL316521*F' TO SSA2                                            
210300     MOVE '  GE' TO GODK-STATUSKODER                                      
210400     CALL CBLTDLI USING GNP 3165-PCB DLI-IO-AREA6 SSA1 SSA2               
210500     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
210600     PERFORM IMS-STATUSKONTROLL                                           
210700     .                                                                    
210800     EJECT                                                                
210900 IMS-GNP-FAKTURARAD SECTION.                                              
211000     MOVE 'IMS-GNP-FAKTURARAD' TO WS-IMS-SEKTION                          
211100                                                                          
211200     STRING 'WL316511(IDBYTFAK =' W-IDBYTFAK-X ')'                        
211300          DELIMITED BY SIZE INTO SSA1                                     
211400     MOVE 'WL316521  ' TO SSA2                                            
211500     MOVE '  GE' TO GODK-STATUSKODER                                      
211600     CALL CBLTDLI USING GNP 3165-PCB DLI-IO-AREA6 SSA1 SSA2               
211700     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
211800     PERFORM IMS-STATUSKONTROLL                                           
211900     .                                                                    
212000     EJECT                                                                
212100 IMS-GET-WL316521 SECTION.                                                
212200     MOVE 'IMS-GET-WL316521'      TO WS-IMS-SEKTION                       
212300                                                                          
212400     STRING 'WL316501(WDGXKEY  =' W-WDGXKEY-X ')'                         
212500          DELIMITED BY SIZE INTO SSA1                                     
212600     STRING 'WL316511(IDBYTFAK =' W-IDBYTFAK-X ')'                        
212700          DELIMITED BY SIZE INTO SSA2                                     
212800     STRING 'WL316521(WDGX3168=>' W-3168-MIN-X                            
212900                   '&WDGX3168=<' W-3168-MAX-X ')'                         
213000          DELIMITED BY SIZE INTO SSA3                                     
213100     MOVE '  GE' TO GODK-STATUSKODER                                      
213200     CALL CBLTDLI USING GNP 3165-PCB DLI-IO-AREA6 SSA1 SSA2 SSA3          
213300     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
213400     PERFORM IMS-STATUSKONTROLL                                           
213500     .                                                                    
213600     EJECT                                                                
213700 IMS-GET-WL316521-F SECTION.                                              
213800     MOVE 'IMS-GET-WL316521-F' TO WS-IMS-SEKTION                          
213900                                                                          
214000     STRING 'WL316501(WDGXKEY  =' W-WDGXKEY-X ')'                         
214100          DELIMITED BY SIZE INTO SSA1                                     
214200     STRING 'WL316511(IDBYTFAK =' W-IDBYTFAK-X ')'                        
214300          DELIMITED BY SIZE INTO SSA2                                     
214400     STRING 'WL316521*F(WDGX3168=>' W-3168-MIN-X                          
214500                   '&WDGX3168=<' W-3168-MAX-X ')'                         
214600          DELIMITED BY SIZE INTO SSA3                                     
214700     MOVE '  GE' TO GODK-STATUSKODER                                      
214800     CALL CBLTDLI USING GNP 3165-PCB DLI-IO-AREA6 SSA1 SSA2 SSA3          
214900     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
215000     PERFORM IMS-STATUSKONTROLL                                           
215100     .                                                                    
215200     EJECT                                                                
215300 IMS-GET-WL316521-FIRST SECTION.                                          
215400     MOVE 'IMS-GET-WL316521-FIRST' TO WS-IMS-SEKTION                      
215500                                                                          
215600     STRING 'WL316501(WDGXKEY  =' W-WDGXKEY-X ')'                         
215700          DELIMITED BY SIZE INTO SSA1                                     
215800     STRING 'WL316511(IDBYTFAK =' W-IDBYTFAK-X ')'                        
215900          DELIMITED BY SIZE INTO SSA2                                     
216000     STRING 'WL316521*F(WDGX3168=>' W-3168-MIN-X-F                        
216100                   '&WDGX3168=<' W-3168-MAX-X-F ')'                       
216200          DELIMITED BY SIZE INTO SSA3                                     
216300     MOVE '  GE' TO GODK-STATUSKODER                                      
216400     CALL CBLTDLI USING GNP 3165-PCB DLI-IO-AREA6 SSA1 SSA2 SSA3          
216500     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
216600     PERFORM IMS-STATUSKONTROLL                                           
216700     .                                                                    
216800     EJECT                                                                
216900 IMS-GET-WL316521-2 SECTION.                                              
217000     MOVE 'IMS-GET-WL316521-2' TO WS-IMS-SEKTION                          
217100                                                                          
217200     STRING 'WL316501(WDGXKEY  =' W-WDGXKEY-X ')'                         
217300          DELIMITED BY SIZE INTO SSA1                                     
217400     STRING 'WL316511(IDBYTFAK =' W-IDBYTFAK-X ')'                        
217500          DELIMITED BY SIZE INTO SSA2                                     
217600     STRING 'WL316521(IDARTNRO =' W-IDARTNR-KEY-X ')'                     
217700          DELIMITED BY SIZE INTO SSA3                                     
217800     MOVE '  GE' TO GODK-STATUSKODER                                      
217900     CALL CBLTDLI USING GNP 3165-PCB DLI-IO-AREA6 SSA1 SSA2 SSA3          
218000     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
218100     PERFORM IMS-STATUSKONTROLL                                           
218200     .                                                                    
218300     EJECT                                                                
218400 IMS-GNP-WL316521 SECTION.                                                
218500     MOVE 'IMS-GNP-WL316521' TO WS-IMS-SEKTION                            
218600                                                                          
218700     STRING 'WL316511(IDBYTFAK =' W-IDBYTFAK-X ')'                        
218800          DELIMITED BY SIZE INTO SSA1                                     
218900     MOVE 'WL316521 '         TO SSA2                                     
219000     MOVE '  GE' TO GODK-STATUSKODER                                      
219100     CALL CBLTDLI USING GHNP 3165-PCB DLI-IO-AREA6 SSA1 SSA2              
219200     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
219300     PERFORM IMS-STATUSKONTROLL                                           
219400     .                                                                    
219500     EJECT                                                                
219600 IMS-GHU-WL316521 SECTION.                                                
219700     MOVE 'IMS-GHU-WL316521' TO WS-IMS-SEKTION                            
219800                                                                          
219900     STRING 'WL316511(IDBYTFAK =' W-IDBYTFAK-X ')'                        
220000          DELIMITED BY SIZE INTO SSA1                                     
220100     STRING 'WL316521(WDGX3168 =' W-KEY3168-X ')'                         
220200          DELIMITED BY SIZE INTO SSA2                                     
220300     MOVE '  GE' TO GODK-STATUSKODER                                      
220400     CALL CBLTDLI USING GHU 3165-PCB DLI-IO-AREA6 SSA1 SSA2               
220500     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
220600     PERFORM IMS-STATUSKONTROLL                                           
220700     .                                                                    
220800     EJECT                                                                
220900 IMS-ISRT-WL316511 SECTION.                                               
221000     MOVE 'IMS-ISRT-WL316511' TO WS-IMS-SEKTION                           
221100                                                                          
221200     MOVE 'WL316511 ' TO SSA1                                             
221300     MOVE '  II' TO GODK-STATUSKODER                                      
221400     CALL CBLTDLI USING ISRT 3165-PCB DLI-IO-AREA5 SSA1                   
221500     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
221600     PERFORM IMS-STATUSKONTROLL                                           
221700     .                                                                    
221800     SKIP3                                                                
221900 IMS-ISRT-WL316521 SECTION.                                               
222000     MOVE 'IMS-ISRT-WL316521' TO WS-IMS-SEKTION                           
222100                                                                          
222200     MOVE 'WL316521 ' TO SSA1                                             
222300     MOVE '  II' TO GODK-STATUSKODER                                      
222400     CALL CBLTDLI USING ISRT 3165-PCB DLI-IO-AREA6 SSA1                   
222500     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
222600     PERFORM IMS-STATUSKONTROLL                                           
222700     .                                                                    
222800     SKIP3                                                                
222900 IMS-REPL-WL316611 SECTION.                                               
223000     MOVE 'IMS-REPL-WL316611' TO WS-IMS-SEKTION                           
223100                                                                          
223200     MOVE '  ' TO GODK-STATUSKODER                                        
223300     CALL CBLTDLI USING REPL 3165-PCB DLI-IO-AREA5                        
223400     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
223500     PERFORM IMS-STATUSKONTROLL                                           
223600     .                                                                    
223700     SKIP3                                                                
223800 IMS-REPL-WL3168 SECTION.                                                 
223900     MOVE 'IMS-REPL-WL3168'   TO WS-IMS-SEKTION                           
224000                                                                          
224100     MOVE '  ' TO GODK-STATUSKODER                                        
224200     CALL CBLTDLI USING REPL 3165-PCB DLI-IO-AREA6                        
224300     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
224400     PERFORM IMS-STATUSKONTROLL                                           
224500     .                                                                    
224600     SKIP3                                                                
224700 IMS-DLET-WL3168 SECTION.                                                 
224800     MOVE 'IMS-DLET-WL3168'   TO WS-IMS-SEKTION                           
224900                                                                          
225000     MOVE '  ' TO GODK-STATUSKODER                                        
225100     CALL CBLTDLI USING DLET 3165-PCB DLI-IO-AREA6                        
225200     MOVE 3165-STATUS-CODE TO STATUS-WS                                   
225300     PERFORM IMS-STATUSKONTROLL                                           
225400     .                                                                    
225500     EJECT                                                                
225600 IMS-GET-ARTS-WLARTS11 SECTION.                                           
225700     MOVE 'IMS-GET-ARTS-WLARTS11'   TO WS-IMS-SEKTION                     
225800     SKIP2                                                                
225900     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-ART-X ')'                     
226000           DELIMITED BY SIZE INTO SSA1                                    
226100     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
226200           DELIMITED BY SIZE INTO SSA2                                    
226300     MOVE '  GE' TO GODK-STATUSKODER                                      
226400     CALL CBLTDLI USING GHU ARTS-PCB IO-AREA4 SSA1 SSA2                   
226500     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
226600     PERFORM IMS-STATUSKONTROLL                                           
226700     .                                                                    
226800     SKIP3                                                                
226900 IMS-REPL-ARTS SECTION.                                                   
227000     MOVE 'IMS-REPL-ARTS'   TO WS-IMS-SEKTION                             
227100                                                                          
227200     MOVE '  ' TO GODK-STATUSKODER                                        
227300     CALL CBLTDLI USING REPL ARTS-PCB DLI-IO-AREA4                        
227400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
227500     PERFORM IMS-STATUSKONTROLL                                           
227600                                                                          
227700     .                                                                    
227800 IMS-ISRT-WDL9 SECTION.                                                   
227900     MOVE 'IMS-ISRT-WDL9'   TO WS-IMS-SEKTION                             
228000                                                                          
228100     MOVE 'WLLOGA01 ' TO SSA1                                             
228200     MOVE '  II' TO GODK-STATUSKODER                                      
228300     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
228400     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
228500     PERFORM IMS-STATUSKONTROLL                                           
228600     .                                                                    
228700     SKIP3                                                                
228800 IMS-GET-ARTC-C11 SECTION.                                                
228900                                                                          
229000     STRING 'WLARTC01(IDARTNR  =' W-PARTNUM-X ')'                         
229100          DELIMITED BY SIZE INTO SSA1                                     
229200     STRING 'WLARTC11(KDSEGKEY =' W-KDSEGKEY-X ')'                        
229300          DELIMITED BY SIZE INTO SSA2                                     
229400     MOVE '  GE' TO GODK-STATUSKODER                                      
229500     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WLARTC11 SSA1  SSA2            
229600     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
229700     PERFORM IMS-STATUSKONTROLL                                           
229800     .                                                                    
229900     EJECT                                                                
       IMS-GU-WDB601    SECTION.                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  GE' TO GODK-STATUSKODER                                      
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUSKONTROLL                                           
           .                                                                    
           EJECT                                                                
230000 IMS-INSERT-ALTMSG SECTION.                                               
230100     MOVE 'IMS-INSERT-ALTMSG'   TO WS-IMS-SEKTION                         
230200                                                                          
230300     MOVE '  '  TO GODK-STATUSKODER                                       
230400     CALL CBLTDLI USING ISRT ALT-PCB PROG-TO-PROG-SW                      
230500     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
230600     PERFORM IMS-STATUSKONTROLL                                           
230700     .                                                                    
230800     EJECT                                                                
230900     SKIP3                                                                
231000 IMS-STATUSKONTROLL SECTION.                                              
231100                                                                          
231200     SET STATUS-IX TO 1                                                   
231300     SEARCH GODK-STATUS                                                   
231400       AT END                                                             
231500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
231600         DELIMITED BY SIZE INTO FELTEXT                                   
231700         CALL FELLOG                                                      
231800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
231900         CONTINUE                                                         
232000     END-SEARCH                                                           
232100     .                                                                    
232200     EJECT                                                                
232300 DB2-SELECT-BYART SECTION.                                                
232400     MOVE 'DB2-SELECT-BYART'    TO WS-IMS-SEKTION                         
232500                                                                          
232600     MOVE 000100  TO GODK-SQLCODEKODER                                    
232700     EXEC SQL SELECT                                                      
232800                  IDARTNR_BYT,                                            
232900                  IDDISTR_RENOV                                           
233000              INTO                                                        
233100                  :BYART-IDARTNR-BYT,                                     
233200                  :BYART-IDDISTR-RENOV                                    
233300            FROM BYART                                                    
233400            WHERE IDARTNR_BYT = :W-IDARTNR-BYT                            
233500     END-EXEC                                                             
233600     MOVE SQLCODE           TO SQLCODE-WS                                 
233700     PERFORM DB2-STATUSKONTROLL                                           
233800     .                                                                    
233900     EJECT                                                                
234000 DB2-STATUSKONTROLL  SECTION.                                             
234100                                                                          
234200     SET SQLCODE-IX TO 1                                                  
234300     SEARCH GODK-SQLCODE                                                  
234400       AT END CALL FELLOG                                                 
234500       WHEN GODK-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
234600     END-SEARCH                                                           
234700     .                                                                    
