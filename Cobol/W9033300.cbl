000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9033300.                                                
000400 AUTHOR.         GERRY CARMICHAEL.                                        
000500 DATE-WRITTEN.   JANUARI 98.                                              
000600*                                                                         
000700*REMARKS.                                                                 
000800*                                                                         
000900*                                                                         
001000*    FUNKTION.                                                            
001100*        FRÅGA PÅ ORDERBEKRÄFTELSE REGISTRET                              
001200*        VIA VDI SYSTEMET.                                                
001300*        PROGRAMMET HANTERAR OLIKA VERSIONER I VDI                        
001400*        ENLIGT IDVTYP.                                                   
001500*                                                                         
001600*        PROGRAMMET LÄSER POSTER PÅ WDQ1 BEROENDA AV                      
001700*        NYCKLAR I MID'EN.                                                
001800*                                                                         
001900*        PROGRAMMET ANROPAR W218ETA FÖR HÄMTNING                          
002000*        AV ETA-DATUM/NDC-LAGER.                                          
002100*       (ESTIMATED TIME AVAILABLE)                                        
002200*                                                                         
002300*    INDATA.                                                              
002400*        TRANSAKTION: W90333T                                             
002500*        MID:         W9I33301                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W9O33301                                            
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500*    -COPY WY2000W1                                                       
003600     SKIP3                                                                
003700 77  IDPGM                       PIC X(8)    VALUE 'W9033300'.            
003800 77  WS-MODNAMN                  PIC X(8)    VALUE 'W9O33301'.            
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004300 77  W-SPAR-KDORDBEK             PIC  9(2).                               
004400 77  MAX-RAD                     PIC S9(7)   VALUE +13 COMP-3.            
004500 77  SPAR-IDORDER-NEXT           PIC 9(7)    VALUE ZERO.                  
004600 77  SPAR-IDORDNR7-NEXT          PIC 9(7)    VALUE ZERO.                  
004700 77  SPAR-IDARTNR-NEXT           PIC 9(9)    VALUE ZERO.                  
004800 77  SPAR-IDLOPNR-NEXT           PIC 9(3)    VALUE ZERO.                  
004900 77  SPAR-IDSEKVNR-NEXT          PIC 9(3)    VALUE ZERO.                  
005000 77  SPAR-IDDC-NEXT              PIC X(2)    VALUE SPACE.                 
005100 77  SPAR-KDORDBEK-NEXT          PIC 9(2)    VALUE ZERO.                  
005200 77  SPAR-KDORDKL-NEXT           PIC 9(1)    VALUE ZERO.                  
005300 77  SPAR-KDFRAKT-NEXT           PIC 9(2)    VALUE ZERO.                  
005400 77  SPAR-TIORDREG-NEXT          PIC 9(6)    VALUE ZERO.                  
005500 77  MAX-TAB                     PIC S9(7)   VALUE +5 COMP-3.             
005600 77  MAX-MOD-LANGD               PIC S9(7)   VALUE +0 COMP-3.             
005700 77  DAGENS-DATUM                PIC  9(6).                               
005800 77  W-TIDISPIN                  PIC 9(6).                                
005900                                                                          
006000     EJECT                                                                
006100*      --- VALID IDDC CODES                                               
006200*                                                                         
006300*01    -COPY WWDC99                                                       
006400       EJECT                                                              
006500*  --- FÖR ATT FÅ RÄTT SEKEL VID ANROP TILL W218ETA                       
006600 01  WS-ETA-DATUM                PIC 9(6).                                
006700 01  FILLER REDEFINES WS-ETA-DATUM.                                       
006800     03  WS-ETA-DATUM-AAR        PIC 9(2).                                
006900     03  WS-ETA-DATUM-MAANAD     PIC 9(2).                                
007000     03  WS-ETA-DATUM-DAG        PIC 9(2).                                
007100     EJECT                                                                
007200* ----- DYNAMISKA SUBPROGRAM                                              
007300 01  DYNAMISKA-SUBPGM.                                                    
007400     03 CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.            
007500     03 W009KSIF                 PIC X(8)    VALUE 'W009KSIF'.            
007600     03 FELLOG                   PIC X(8)    VALUE 'FELLOG  '.            
007700     03 W218ETA                  PIC X(8)    VALUE 'W218ETA '.            
007800                                                                          
007900 01 FILLER                       PIC  X(8)   VALUE 'LETA'.                
008000*   -COPY W218LETA -PRE ETA-.                                             
008100     EJECT                                                                
008200 01  W009KSIF-PARM.                                                       
008300     03 FLT                    PIC 9(9).                                  
008400     03 LGD                    PIC 9(1).                                  
008500     03 KSIFF                  PIC 9(1).                                  
008600     EJECT                                                                
008700* ----- INDEXFÄLT                                                         
008800 77  IX-RAD                      PIC S9(4)   VALUE +0  COMP SYNC.         
008900 77  IX-TAB                      PIC S9(4)   VALUE +0  COMP SYNC.         
009000 77  IX-SPAR                     PIC S9(4)   VALUE +0  COMP SYNC.         
009100 77  TAB-INDX                    PIC S9(4)   VALUE +0  COMP SYNC.         
009200                                                                          
009300* ----- SWITCHAR                                                          
009400 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009500     88  NYCKLAR-OK                          VALUE 'J'.                   
009600                                                                          
009700                                                                          
009800 77  ORDER-SAKNAS-SW             PIC X       VALUE 'N'.                   
009900     88  ORDER-SAKNAS                        VALUE 'J'.                   
010000                                                                          
010100 77  ORDER-EJ-KLAR-SW            PIC X       VALUE 'N'.                   
010200     88  ORDER-EJ-KLAR                       VALUE 'J'.                   
010300                                                                          
010400 01  WS-TITIORDD-9KOMPL          PIC 9(9).                                
010500                                                                          
010600 01  W-SATB-TABELL.                                                       
010700     03 W-SATB-IDARTNR           PIC S9(9)   COMP-3 OCCURS 5.             
010800     03 W-SATB-REKSIFFR          PIC S9(1)   COMP-3 OCCURS 5.             
010900                                                                          
011000 01  WS-TITIORDD                 PIC 9(8).                                
011100 01  FILLER    REDEFINES  WS-TITIORDD.                                    
011200     05  WS-SEKEL-TAL            PIC 9(2).                                
011300     05  WS-AAMMDD               PIC 9(6).                                
011400     05  FILLER REDEFINES WS-AAMMDD.                                      
011500        07  WS-AA                PIC 9(2).                                
011600        07  FILLER               PIC 9(4).                                
011700                                                                          
011800     EJECT                                                                
011900 01    FILLER                    PIC X(16)   VALUE 'MID-AREA'.            
012000     SKIP3                                                                
012100*01    MID -COPY W9I33301.                                                
012200     EJECT                                                                
012300 01    FILLER                    PIC X(16)   VALUE 'MSG-AREA'.            
012400     SKIP3                                                                
012500*01    -COPY WMSGAREA                                                     
012600     EJECT                                                                
012700*  03    MOD -COPY W9O33301  -RED MSG-AREA.                               
012800     EJECT                                                                
012900*PIE VERSION MOD VDI                                                      
013000 01    FILLER                   PIC X(16)   VALUE 'IMS-WS     '.          
013100                                                                          
013200 01    NYCKLAR-TILL-DLI.                                                  
013300                                                                          
013400   03    W-WDQ1B1KY-MIN-X.                                                
013500     05    W-Q1B-IDDISTR-MIN      PIC S9(5)   VALUE ZERO  COMP-3.         
013600     05    W-Q1B-IDKUNDNR-MIN     PIC S9(7)   VALUE ZERO  COMP-3.         
013700     05    W-Q1B-TITIOWDD9-MIN    PIC S9(9)   VALUE ZERO  COMP-3.         
013800     05    W-Q1B-KDFRAKT-MIN      PIC S9(3)   VALUE ZERO  COMP-3.         
013900     05    W-Q1B-KDORDKL-MIN      PIC S9(1)   VALUE ZERO  COMP-3.         
014000     05    W-Q1B-IDKUNDRF-MIN     PIC X(10)   VALUE SPACE.                
014100     05    W-Q1B-IDARTNR-MIN      PIC S9(9)   VALUE ZERO  COMP-3.         
014200     05    W-Q1B-IDLOPNR-MIN      PIC S9(3)   VALUE ZERO  COMP-3.         
014300     05    W-Q1B-IDSEKVNR-MIN     PIC S9(3)   VALUE ZERO  COMP-3.         
014400     05    W-Q1B-IDDC-MIN         PIC  X(2)   VALUE SPACE.                
014500     05    W-Q1B-IDORDER-MIN      PIC S9(7)   VALUE ZERO  COMP-3.         
014600     05    W-Q1B-KDORDBEK-MIN     PIC  9(2)   VALUE ZERO.                 
014700                                                                          
014800   03  W-WDQ1B1KY-MAX-X.                                                  
014900     05  W-Q1B-IDDISTR-MAX      PIC S9(5) VALUE ZERO     COMP-3.          
015000     05  W-Q1B-IDKUNDNR-MAX     PIC S9(7) VALUE ZERO     COMP-3.          
015100     05  W-Q1B-TITIOWDD9-MAX    PIC S9(9) VALUE ZERO     COMP-3.          
015200     05  W-Q1B-KDFRAKT-MAX      PIC S9(3) VALUE ZERO     COMP-3.          
015300     05  W-Q1B-KDORDKL-MAX      PIC S9(1) VALUE ZERO     COMP-3.          
015400     05  W-Q1B-IDKUNDRF-MAX     PIC X(10) VALUE SPACE.                    
015500     05  W-Q1B-IDARTNR-MAX      PIC S9(9) VALUE ZERO     COMP-3.          
015600     05  W-Q1B-IDLOPNR-MAX      PIC S9(3) VALUE ZERO     COMP-3.          
015700     05  W-Q1B-IDSEKVNR-MAX     PIC S9(3) VALUE ZERO     COMP-3.          
015800     05  W-Q1B-IDDC-MAX         PIC  X(2) VALUE SPACE.                    
015900     05  W-Q1B-IDORDER-MAX      PIC S9(7) VALUE ZERO     COMP-3.          
016000     05  W-Q1B-KDORDBEK-MAX     PIC  9(2) VALUE ZERO.                     
016100                                                                          
016200   03    W-WDQ101KY-X.                                                    
016300     05    W-Q1-IDORDER        PIC S9(7)   VALUE ZERO  COMP-3.            
016400     05    W-Q1-IDARTNR        PIC S9(9)   VALUE ZERO  COMP-3.            
016500     05    W-Q1-IDLOPNR        PIC S9(3)   VALUE ZERO  COMP-3.            
016600     05    W-Q1-IDSEKVNR       PIC S9(3)   VALUE ZERO  COMP-3.            
016700     05    W-Q1-IDDC           PIC  X(2)   VALUE SPACE.                   
016800     05    W-Q1-KDORDBEK       PIC  9(2)   VALUE ZERO.                    
016900     EJECT                                                                
017000                                                                          
017100   03    W-KDORDKL-X.                                                     
017200     05    W-KDORDKL             PIC S9(1)   VALUE ZERO  COMP-3.          
017300     SKIP2                                                                
017400   03    W-IDKUNDRF-X.                                                    
017500     05    W-IDKUNDRF            PIC X(10)   VALUE SPACE.                 
017600     SKIP2                                                                
017700   03    W-IDARTNR-Q1-X.                                                  
017800     05    W-IDARTNR-Q1          PIC S9(9)   VALUE ZERO  COMP-3.          
017900     SKIP2                                                                
018000   03  W-WDQ2CSEQ-X.                                                      
018100       05  W-Q2CSEQ-IDDISTR         PIC S9(05) VALUE ZERO COMP-3.         
018200       05  W-Q2CSEQ-IDKUNDNR        PIC S9(07) VALUE ZERO COMP-3.         
018300       05  W-Q2CSEQ-IDKUNDRF        PIC  X(10) VALUE SPACE.               
018400     SKIP2                                                                
018500   03  W-WDJ1CSEQ-X.                                                      
018600     05  W-IDLEVNR                 PIC X(5)   VALUE SPACE.                
018700     05  FILLER                    PIC X(30)  VALUE SPACE.                
018800     05  W-IDARTNR                 PIC S9(9)  VALUE +0   COMP-3.          
018900                                                                          
019000   03  W-IDLEVNR-X.                                                       
019100     05  W-IDLEVNR                  PIC X(5)  VALUE '1002'.               
019200                                                                          
019300   03  W-IDARTNR-K6-X.                                                    
019400     05  W-IDARTNR-K6               PIC S9(9) VALUE ZERO COMP-3.          
019401                                                                          
019410   03  W-IDDC-X.                                                          
019420     05 W-IDDC                      PIC X(2).                             
019500                                                                          
019600     EJECT                                                                
019700*                        **** STATUS-KOD FRÅN IMS                         
019800   03    STATUS-WS               PIC XX.                                  
019900     88    SEGMENT-FINNS                     VALUE '  '.                  
020000     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
020100     88    SEGMENT-SLUT                      VALUE 'GB'.                  
020200     SKIP3                                                                
020300   03    WDQ1-STATUS-WS          PIC XX.                                  
020400     88    WDQ1-SEGMENT-FINNS                VALUE '  '.                  
020500     88    WDQ1-SEGMENT-SAKNAS               VALUE 'GE'.                  
020600     88    WDQ1-SEGMENT-SLUT                 VALUE 'GB'.                  
020700     SKIP3                                                                
020800   03    GODK-STATUSKODER.                                                
020900     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
021000     SKIP3                                                                
021100 01    SSA1                      PIC X(164).                              
021200 01    SSA2                      PIC X(64).                               
021300     EJECT                                                                
021400*                            IMS FUNKTIONSKODER                           
021500*01    -COPY W0003                                                        
021600     EJECT                                                                
021700*                            DLI INPUT-OUTPUT AREA                        
021800 01    DLI-IO-AREA.                                                       
021900   03    IO-AREA                 PIC X(700)  VALUE SPACE.                 
022000     SKIP3                                                                
022100*  03    WLORQO01  -COPY WDQ1B1                    -RED IO-AREA           
022200     EJECT                                                                
022300*  03    WLORQM01  -COPY WDQ101                    -RED IO-AREA           
022400     EJECT                                                                
022700 01    DLI-IO-AREA2.                                                      
022800   03    IO-AREA2                PIC X(500)  VALUE SPACE.                 
022900     SKIP3                                                                
023000     03  WLSATB01 REDEFINES IO-AREA2.                                     
023100*        05  -COPY WDJ111      -PRE SATB-                                 
023200*        05  -COPY WDJ101      -PRE SATB-                                 
023300     EJECT                                                                
023400 01  DLI-IO-WDK611.                                                       
023500*    03  WDK611      -COPY WDK611                                         
023600     EJECT                                                                
023610 01  DLI-IO-WDQ201.                                                       
023620*    03  WDQ201      -COPY WDQ201                                         
023630     EJECT                                                                
023640 01  DLI-IO-WDQ212.                                                       
023650*    03  WDQ212      -COPY WDQ212                                         
023660     EJECT                                                                
023700 01  FILLER                 PIC X(12) VALUE 'DUMMY-ARTC'.                 
023800 01  ETA-ARTC-PCB           PIC X.                                        
023900 01  FILLER                 PIC X(12) VALUE 'DUMMY-LEVA'.                 
024000 01  ETA-LEVA-PCB           PIC X.                                        
024100 LINKAGE SECTION.                                                         
024200*01    -COPY W0009     -PRE MSG-                                          
024300                                                                          
024400*01    -COPY W0008     -PRE ORQO-                                         
024500     05  FILLER                  PIC X.                                   
024600                                                                          
024700*01    -COPY W0008     -PRE ORQM-                                         
024800     05  FILLER                  PIC X.                                   
024900                                                                          
025000*01    -COPY W0008     -PRE SATB-                                         
025100     05  FILLER                  PIC X.                                   
025200                                                                          
025300*01    -COPY W0008     -PRE ORQI-                                         
025400     05  FILLER                  PIC X.                                   
025500                                                                          
025600*01    -COPY W0008     -PRE WDK6-                                         
025700     05  FILLER                  PIC X.                                   
025800                                                                          
025900     EJECT                                                                
026000 01  ETA-WDK7-PCB           PIC X.                                        
026100 01  ETA-INLC-PCB           PIC X.                                        
026200 01  ETA-WDB6-PCB           PIC X.                                        
026300 01  ETA-WDD9-PCB           PIC X.                                        
026400     EJECT                                                                
026500                                                                          
026600 PROCEDURE DIVISION  USING MSG-PCB ORQO-PCB ORQM-PCB                      
026700                                   SATB-PCB ORQI-PCB                      
026800                                   WDK6-PCB                               
026900                                   ETA-WDK7-PCB ETA-INLC-PCB              
027000                                   ETA-WDB6-PCB ETA-WDD9-PCB.             
027100     ENTRY 'DLITCBL' USING MSG-PCB ORQO-PCB ORQM-PCB                      
027200                                   SATB-PCB ORQI-PCB                      
027300                                   WDK6-PCB                               
027400                                   ETA-WDK7-PCB ETA-INLC-PCB              
027500                                   ETA-WDB6-PCB ETA-WDD9-PCB.             
027600                                                                          
027700 STYR      SECTION.                                                       
027800                                                                          
027900     PERFORM IMS-GET-MSG                                                  
028000     IF SEGMENT-FINNS                                                     
028100        PERFORM A-INIT                                                    
028200        PERFORM B-KONTROLLERA-NYCKLAR                                     
028300        IF NYCKLAR-OK                                                     
028400           PERFORM C-BEHANDLA-RADER                                       
028500           IF ORDER-SAKNAS OR  ORDER-EJ-KLAR                              
028600              CONTINUE                                                    
028700           ELSE                                                           
028800              PERFORM E-KONTROLLERA-OM-TOM-SIDA                           
028900           END-IF                                                         
029000        END-IF                                                            
029100        PERFORM F-BERAKNA-MAX-MOD-LANGD                                   
029200        MOVE MAX-MOD-LANGD        TO MSG-KVLL                             
029300        PERFORM IMS-INSERT-MSG                                            
029400     END-IF                                                               
029500     MOVE ZERO                 TO RETURN-CODE                             
029600     GOBACK                                                               
029700     .                                                                    
029800     EJECT                                                                
029900                                                                          
030000 A-INIT     SECTION.                                                      
030100                                                                          
030200     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I33301                    
030300     MOVE LOW-VALUE                    TO MSG-AREA                        
030400     MOVE '9333'                       TO MOD-IDTRANS                     
030500     MOVE ZERO                         TO MOD-IDMFSFEL                    
030600     MOVE MID-TIORDREG                 TO MOD-TIORDREG                    
030700     MOVE ZERO                         TO MOD-IDARTNR-NEXT                
030800                                          MOD-IDORDNR7-NEXT               
030900                                          MOD-IDLOPNR-NEXT                
031000                                          MOD-IDSEKVNR-NEXT               
031100                                          MOD-IDORDER-NEXT                
031200                                          MOD-TIORDREG-NEXT               
031300                                          MOD-KDFRAKT-NEXT                
031400                                          MOD-KDORDKL-NEXT                
031500                                          MOD-KDORDBEK-NEXT               
031600                                                                          
031700     MOVE SPACE                        TO MOD-IDDC-NEXT                   
031800                                                                          
031900     MOVE +1                   TO  IX-RAD                                 
032000     PERFORM UNTIL             IX-RAD > MAX-RAD                           
032100         MOVE ZERO             TO  MOD-IDORDNR7-RAD(IX-RAD)               
032200                                   MOD-IDARTNR-RAD(IX-RAD)                
032300                                   MOD-KVBEART(IX-RAD)                    
032400                                   MOD-KVAVBART(IX-RAD)                   
032500                                   MOD-KDORDBEK(IX-RAD)                   
032600                                   MOD-TIDISPIN(IX-RAD)                   
032700                                   MOD-IDORDNR7-RO(IX-RAD)                
032800         MOVE SPACE            TO  MOD-BEERS(IX-RAD)                      
032900                                   MOD-IDBIL(IX-RAD)                      
033000                                   MOD-IDDC    (IX-RAD)                   
033100         ADD +1                TO  IX-RAD                                 
033200     END-PERFORM                                                          
033300                                                                          
033400     MOVE ZERO                 TO  W-SPAR-KDORDBEK                        
033500                                                                          
033600     MOVE +1                   TO  IX-TAB                                 
033700     PERFORM UNTIL             IX-TAB > MAX-TAB                           
033800         MOVE ZERO             TO  W-SATB-IDARTNR(IX-TAB)                 
033900         ADD +1                TO  IX-TAB                                 
034000     END-PERFORM                                                          
034100                                                                          
034200     MOVE NEJ                  TO ORDER-SAKNAS-SW                         
034300                                  ORDER-EJ-KLAR-SW                        
034400     ACCEPT DAGENS-DATUM FROM DATE                                        
034500     .                                                                    
034600     EJECT                                                                
034700                                                                          
034800 B-KONTROLLERA-NYCKLAR    SECTION.                                        
034900                                                                          
035000     IF MID-IDDISTR            NUMERIC AND                                
035100        MID-IDKUNDNR           NUMERIC                                    
035200       IF MID-IDARTNR      NOT NUMERIC OR                                 
035300          MID-TIORDREG     NOT NUMERIC                                    
035400         MOVE NEJ              TO NYCKLAR-SW                              
035500         MOVE 'B10'            TO MOD-IDMFSFEL                            
035600       END-IF                                                             
035700     ELSE                                                                 
035800       MOVE NEJ                TO NYCKLAR-SW                              
035900       MOVE 'B01'              TO MOD-IDMFSFEL                            
036000     END-IF                                                               
036100     .                                                                    
036200     EJECT                                                                
036300                                                                          
036400 C-BEHANDLA-RADER         SECTION.                                        
036500                                                                          
036600     IF MID-IDORDNR7-NEXT      >  ZERO                                    
036700         PERFORM CA-SKAPA-BLADDRINGS-NYCKEL                               
036800         PERFORM IMS-GU-WLORQO01-KVAL                                     
036900         MOVE STATUS-WS    TO  WDQ1-STATUS-WS                             
037000         IF SEGMENT-FINNS                                                 
037100            PERFORM S03-LAES-WDQ101-UNIK                                  
037200         END-IF                                                           
037300     ELSE                                                                 
037400         IF MID-IDORDNR7       >  ZERO                                    
037500             PERFORM CB-KOLLA-OHUV                                        
037600         END-IF                                                           
037700         IF ORDER-SAKNAS OR ORDER-EJ-KLAR                                 
037800             CONTINUE                                                     
037900         ELSE                                                             
038000             PERFORM S01-SKAPA-OKVAL-NYCKEL                               
038100             PERFORM S02-LAES-WDQ1B-OKVAL                                 
038200             MOVE STATUS-WS    TO  WDQ1-STATUS-WS                         
038300             IF SEGMENT-FINNS                                             
038400                PERFORM S03-LAES-WDQ101-UNIK                              
038500             END-IF                                                       
038600         END-IF                                                           
038700     END-IF                                                               
038800                                                                          
038900     MOVE +1                   TO IX-RAD                                  
039000                                                                          
039100     IF ORDER-SAKNAS OR ORDER-EJ-KLAR                                     
039200         CONTINUE                                                         
039300     ELSE                                                                 
039400         PERFORM UNTIL WDQ1-SEGMENT-SAKNAS       OR                       
039500                       WDQ1-SEGMENT-SLUT         OR                       
039600                       IX-RAD  > MAX-RAD                                  
039700                                                                          
039800             PERFORM CC-REDIGERA-ORDERBEK-RAD                             
039900             ADD +1       TO  IX-RAD                                      
040000     EJECT                                                                
040100                                                                          
040200             IF IX-RAD         =   +16                                    
040300                 CONTINUE                                                 
040400*--------------- SIDBRYTNING MITT I EN SATSARTIKEL LÄS EJ                 
040500*--------------- NÄSTA PÅ OBKR                                            
040600             ELSE                                                         
040700                 PERFORM S05-SKAPA-OKVAL-POSITION                         
040800                 PERFORM S02-LAES-WDQ1B-OKVAL                             
040900                 MOVE STATUS-WS TO WDQ1-STATUS-WS                         
041000                 IF SEGMENT-FINNS                                         
041100                    PERFORM S03-LAES-WDQ101-UNIK                          
041200                 END-IF                                                   
041300             END-IF                                                       
041400                                                                          
041500             IF WDQ1-SEGMENT-FINNS           AND                          
041600                IX-RAD         >  MAX-RAD                                 
041700                IF OBKR-KDORDBEK = 61                                     
041800                  PERFORM CE-BACKA-BILDEN-KOD61                           
041900                ELSE                                                      
042000                  PERFORM CD-SPARA-NYCKLAR                                
042100                END-IF                                                    
042200             END-IF                                                       
042300         END-PERFORM                                                      
042400     END-IF                                                               
042500     .                                                                    
042600     EJECT                                                                
042700                                                                          
042800 CA-SKAPA-BLADDRINGS-NYCKEL   SECTION.                                    
042900                                                                          
043000     MOVE LOW-VALUE            TO W-WDQ1B1KY-MIN-X                        
043100                                                                          
043200     MOVE MID-IDDC-NEXT        TO WS-IDDC                                 
043300     IF NOT CDC-SE                                                        
043400       MOVE MID-IDDISTR        TO W-Q2CSEQ-IDDISTR                        
043500       MOVE MID-IDKUNDNR       TO W-Q2CSEQ-IDKUNDNR                       
043600       MOVE MID-IDORDNR7-NEXT  TO W-Q2CSEQ-IDKUNDRF                       
043700                                                                          
043800       PERFORM IMS-GU-WLORQI01-KVAL                                       
043900       IF SEGMENT-FINNS                                                   
044000         MOVE MID-IDDC-NEXT      TO W-Q1B-IDDC-MIN                        
044100       ELSE                                                               
044200         MOVE JA               TO ORDER-SAKNAS-SW                         
044300         MOVE 'B15'            TO MOD-IDMFSFEL                            
044400       END-IF                                                             
044500     ELSE                                                                 
044600       MOVE MID-IDDC-NEXT      TO  W-Q1B-IDDC-MIN                         
044700     END-IF                                                               
044800                                                                          
044900     MOVE MID-IDDISTR          TO  W-Q1B-IDDISTR-MIN                      
045000     MOVE MID-IDKUNDNR         TO  W-Q1B-IDKUNDNR-MIN                     
045100                                                                          
045200     MOVE MID-TIORDREG-NEXT TO TMP1-YYMMDD                                
045300     MOVE ZERO              TO TMP2-YYMMDD                                
045400     PERFORM WY2000P1                                                     
045500     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
045600       MOVE MID-TIORDREG-NEXT  TO  WS-AAMMDD                              
045700       IF WS-AA < 50                                                      
045800         MOVE 20               TO  WS-SEKEL-TAL                           
045900       ELSE                                                               
046000         MOVE 19               TO  WS-SEKEL-TAL                           
046100       END-IF                                                             
046200     END-IF                                                               
046300                                                                          
046400     COMPUTE WS-TITIORDD-9KOMPL =  999999999 - WS-TITIORDD                
046500     MOVE WS-TITIORDD-9KOMPL   TO  W-Q1B-TITIOWDD9-MIN                    
046600                                                                          
046700     MOVE MID-KDFRAKT-NEXT     TO  W-Q1B-KDFRAKT-MIN                      
046800     MOVE MID-KDORDKL-NEXT     TO  W-Q1B-KDORDKL-MIN                      
046900     MOVE MID-IDORDNR7-NEXT    TO  W-Q1B-IDKUNDRF-MIN                     
047000     MOVE MID-IDARTNR-NEXT     TO  W-Q1B-IDARTNR-MIN                      
047100     MOVE MID-IDLOPNR-NEXT     TO  W-Q1B-IDLOPNR-MIN                      
047200     MOVE MID-IDSEKVNR-NEXT    TO  W-Q1B-IDSEKVNR-MIN                     
047300     MOVE MID-IDORDER-NEXT     TO  W-Q1B-IDORDER-MIN                      
047400     MOVE MID-KDORDBEK-NEXT    TO  W-Q1B-KDORDBEK-MIN                     
047500     .                                                                    
047600     EJECT                                                                
047700                                                                          
047800 CB-KOLLA-OHUV                SECTION.                                    
047900                                                                          
048000     MOVE MID-IDDISTR          TO W-Q2CSEQ-IDDISTR                        
048100     MOVE MID-IDKUNDNR         TO W-Q2CSEQ-IDKUNDNR                       
048200     MOVE MID-IDORDNR7         TO W-Q2CSEQ-IDKUNDRF                       
048300                                                                          
048400     PERFORM IMS-GU-WLORQI01-KVAL                                         
048500     IF SEGMENT-FINNS                                                     
048520        PERFORM IMS-GNP-WLORQI12                                          
048600        IF SEGMENT-FINNS                                                  
048610           IF ARB-KDORDSTA = 'E'                                          
048700              MOVE JA                          TO ORDER-EJ-KLAR-SW        
048800              MOVE 'B30'                       TO MOD-IDMFSFEL            
048900           END-IF                                                         
048940        END-IF                                                            
048950     END-IF                                                               
049000     IF NOT SEGMENT-FINNS                                                 
049100         MOVE JA               TO ORDER-SAKNAS-SW                         
049200         MOVE 'B15'            TO MOD-IDMFSFEL                            
049300     END-IF                                                               
049400     .                                                                    
049500     EJECT                                                                
049600                                                                          
049700 CC-REDIGERA-ORDERBEK-RAD    SECTION.                                     
049800                                                                          
049900     IF OBKR-KDORDBEK           =  40 OR 41 OR 61                         
050000         IF OBKR-IDSEKVNR       = 1 OR                                    
050100            (OBKR-KDORDBEK      NOT = W-SPAR-KDORDBEK AND                 
050200             IX-RAD             > 1)                                      
050300             MOVE OBKR-KDORDBEK TO W-SPAR-KDORDBEK                        
050400                                   MOD-KDORDBEK     (IX-RAD)              
050500             MOVE OBKR-IDARTNR  TO MOD-IDARTNR-RAD  (IX-RAD)              
050600         ELSE                                                             
050700             MOVE OBKR-KDORDBEK     TO W-SPAR-KDORDBEK                    
050800             MOVE ZERO              TO MOD-KDORDBEK (IX-RAD)              
050900             IF OBKR-IDARTNR-TILLK  =  ZERO                               
051000                 MOVE OBKR-BEERS    TO MOD-BEERS (IX-RAD)                 
051100                 MOVE ZERO          TO MOD-IDARTNR-RAD(IX-RAD)            
051200             ELSE                                                         
051300                MOVE OBKR-IDARTNR-TILLK TO MOD-IDARTNR-RAD(IX-RAD)        
051400                MOVE OBKR-BEVOLREF  TO MOD-BEERS(IX-RAD)                  
051500             END-IF                                                       
051600         END-IF                                                           
051700         IF OBKR-KDORDBEK        =  61 AND OBKR-IDSEKVNR = +1             
051800           AND IX-RAD > +1                                                
051900           MOVE IX-RAD            TO IX-SPAR                              
052000           MOVE OBKR-IDORDER      TO SPAR-IDORDER-NEXT                    
052100           MOVE OBKR-IDORDNR7     TO SPAR-IDORDNR7-NEXT                   
052200           MOVE OBKR-TIORDREG     TO SPAR-TIORDREG-NEXT                   
052300           MOVE OBKR-IDARTNR      TO SPAR-IDARTNR-NEXT                    
052400           MOVE OBKR-IDLOPNR      TO SPAR-IDLOPNR-NEXT                    
052500           MOVE OBKR-IDSEKVNR     TO SPAR-IDSEKVNR-NEXT                   
052600           MOVE OBKR-IDDC         TO SPAR-IDDC-NEXT                       
052700           MOVE OBKR-KDORDBEK     TO SPAR-KDORDBEK-NEXT                   
052800           MOVE OBKR-KDFRAKT      TO SPAR-KDFRAKT-NEXT                    
052900           MOVE OBKR-KDORDKL      TO SPAR-KDORDKL-NEXT                    
053000         END-IF                                                           
053100     ELSE                                                                 
053200         MOVE OBKR-BEVOLREF       TO  MOD-BEERS (IX-RAD)                  
053300         IF OBKR-IDARTNR-TILLK    >   ZERO                                
053400             MOVE OBKR-IDARTNR-TILLK  TO  MOD-IDARTNR-RAD(IX-RAD)         
053500         ELSE                                                             
053600             MOVE OBKR-IDARTNR        TO  MOD-IDARTNR-RAD(IX-RAD)         
053700         END-IF                                                           
053800         MOVE OBKR-KDORDBEK           TO  MOD-KDORDBEK (IX-RAD)           
053900     END-IF                                                               
054000                                                                          
054100     MOVE OBKR-IDORDNR7        TO  MOD-IDORDNR7-RAD(IX-RAD)               
054200     IF OBKR-IDKUNDRF-RO       NOT =   '0000000   '                       
054300         MOVE OBKR-IDKUNDRF-RO(1:7) TO MOD-IDORDNR7-RO(IX-RAD)            
054400     ELSE                                                                 
054500         MOVE OBKR-IDKUNDRF(1:7)    TO MOD-IDORDNR7-RO(IX-RAD)            
054600     END-IF                                                               
054700                                                                          
054800     MOVE OBKR-IDDC            TO MOD-IDDC(IX-RAD)                        
054900     MOVE OBKR-IDBIL           TO MOD-IDBIL(IX-RAD)                       
055000     PERFORM CCA-BESTAEM-ANTAL                                            
055100     PERFORM CCB-BESTAEM-TIDISPIN                                         
055200     MOVE W-TIDISPIN           TO  MOD-TIDISPIN(IX-RAD)                   
055300     IF OBKR-KDORDBEK          =   +57                                    
055400         PERFORM CCC-BEHANDLA-SATSARTIKLAR                                
055500     END-IF                                                               
055600     .                                                                    
055700     EJECT                                                                
055800                                                                          
055900 CCA-BESTAEM-ANTAL            SECTION.                                    
056000                                                                          
056100     EVALUATE TRUE                                                        
056200                                                                          
056300     WHEN OBKR-KDORDBEK        =  +10 OR +15 OR +16 OR +56 OR             
056400                                  +70 OR +95 OR +96                       
056500          MOVE OBKR-KVBEART-Q  TO  MOD-KVAVBART(IX-RAD)                   
056600          MOVE OBKR-KVBEART-Q  TO  MOD-KVBEART(IX-RAD)                    
056700                                                                          
056800     WHEN OBKR-KDORDBEK        =  +41 OR +61 OR +40                       
056900          IF OBKR-FLTILLK      =  JA                                      
057000            IF OBKR-KDORDBEK   = +61                                      
057100              MOVE ZERO                TO MOD-KVAVBART(IX-RAD)            
057200            ELSE                                                          
057300              MOVE OBKR-KVBEART-TILLK  TO MOD-KVAVBART(IX-RAD)            
057400            END-IF                                                        
057500            MOVE OBKR-KVBEART-TILLK    TO MOD-KVBEART(IX-RAD)             
057600          ELSE                                                            
057700            MOVE OBKR-KVBEART-Q        TO MOD-KVBEART(IX-RAD)             
057800            MOVE ZERO                  TO MOD-KVAVBART(IX-RAD)            
057900          END-IF                                                          
058000                                                                          
058100     WHEN OBKR-KDORDBEK        =  +43 OR +44                              
058200          MOVE OBKR-KVBEART    TO MOD-KVBEART(IX-RAD)                     
058300          MOVE OBKR-KVBEART-Q  TO MOD-KVAVBART(IX-RAD)                    
058400                                                                          
058500                                                                          
058600     WHEN OBKR-KDORDBEK        =  +30 OR +31 OR +32 OR +33 OR             
058700                                  +34                                     
058800          MOVE OBKR-KVANNANT   TO MOD-KVBEART(IX-RAD)                     
058900          MOVE ZERO            TO MOD-KVAVBART(IX-RAD)                    
059000                                                                          
059100     WHEN OBKR-KDORDBEK        =  +20 OR +21 OR +22 OR                    
059200                                  +52 OR +53 OR +54 OR +55 OR             
059300                                  +57 OR +58 OR +59 OR +66 OR             
059400                                  +67 OR +80 OR +82 OR +98                
059500          MOVE OBKR-KVBEART    TO MOD-KVBEART(IX-RAD)                     
059600          MOVE ZERO            TO MOD-KVAVBART(IX-RAD)                    
059700                                                                          
059800     WHEN OBKR-KDORDBEK        =  +71 OR +72 OR +73 OR +74 OR             
059900                                  +75 OR +76 OR +77                       
060000          MOVE OBKR-KVBEART-Q  TO MOD-KVBEART(IX-RAD)                     
060100          MOVE ZERO            TO MOD-KVAVBART(IX-RAD)                    
060200                                                                          
060300     WHEN OBKR-KDORDBEK        =  +80 OR +81 OR +93                       
060400          MOVE OBKR-KVBEART-Q  TO MOD-KVBEART(IX-RAD)                     
060500          COMPUTE MOD-KVAVBART(IX-RAD) = OBKR-KVBEART-Q -                 
060600                                         OBKR-KVANNANT                    
060700                                                                          
060800     WHEN OBKR-KDORDBEK        =  +83 OR +85 OR +87                       
060900          MOVE OBKR-KVANNANT   TO MOD-KVBEART(IX-RAD)                     
061000          MOVE ZERO            TO MOD-KVAVBART(IX-RAD)                    
061100                                                                          
061200     WHEN OBKR-KDORDBEK        =  +90 OR +91                              
061300          MOVE OBKR-KVBEART-Q  TO MOD-KVBEART(IX-RAD)                     
061400          COMPUTE MOD-KVAVBART(IX-RAD) =  OBKR-KVBEART-Q -                
061500                                          OBKR-KVRO                       
061600                                                                          
061700     WHEN OBKR-KDORDBEK        =  +92                                     
061800          MOVE OBKR-KVBEART-Q  TO MOD-KVBEART(IX-RAD)                     
061900          MOVE OBKR-KVPREAVB   TO MOD-KVAVBART(IX-RAD)                    
062000                                                                          
062100     WHEN OBKR-KDORDBEK        =  +99                                     
062200          MOVE OBKR-KVBEART-Q  TO MOD-KVBEART(IX-RAD)                     
062300          COMPUTE MOD-KVAVBART(IX-RAD) =  OBKR-KVBEART-Q -                
062400                                          OBKR-KVPRERO                    
062500                                                                          
062600     END-EVALUATE                                                         
062700     .                                                                    
062800     EJECT                                                                
062900 CCB-BESTAEM-TIDISPIN         SECTION.                                    
063000                                                                          
063100     MOVE OBKR-IDDC             TO WS-IDDC                                
063300     IF NDC                                                               
063400        EVALUATE TRUE                                                     
063500        WHEN OBKR-KDORDBEK        =  +70 OR +90 OR +91 OR +99             
063600             MOVE '612'         TO ETA-KDCALL                             
063700             MOVE OBKR-IDDC     TO ETA-IDDC-REC                           
063800             MOVE OBKR-IDARTNR  TO ETA-IDARTNR                            
063900             MOVE SPACE         TO ETA-IDLEVNR                            
064000             MOVE ZERO          TO ETA-KDFRAKT                            
064100             MOVE OBKR-TIREGDAT TO ETA-TIAAMMDD-ANROP                     
064200                                   WS-ETA-DATUM                           
064300             IF WS-ETA-DATUM-AAR > 50                                     
064400                MOVE 19         TO ETA-TISEKEL-ANROP                      
064500             ELSE                                                         
064600                MOVE 20         TO ETA-TISEKEL-ANROP                      
064700             END-IF                                                       
064800                                                                          
064900             CALL W218ETA  USING ETA-W218LETA                             
065000                           ETA-ARTC-PCB ETA-WDK7-PCB                      
065100                           ETA-INLC-PCB ETA-LEVA-PCB                      
065200                           ETA-WDB6-PCB ETA-WDD9-PCB                      
065300                                                                          
065400             IF ETA-SVAR-OK = JA                                          
065600               IF NDC-NA OR NDC-CN                                        
065700                 MOVE ETA-TIAAMMDD-SVAR                                   
065800                                  TO W-TIDISPIN                           
065900               ELSE                                                       
066000                 IF ETA-KVAVIS-ETA > +0                                   
066100                   MOVE ETA-TIAAMMDD-SVAR                                 
066200                                  TO W-TIDISPIN                           
066300                 ELSE                                                     
066400                   MOVE ZERO      TO W-TIDISPIN                           
066500                 END-IF                                                   
066600               END-IF                                                     
066700             ELSE                                                         
066800                MOVE ZERO         TO W-TIDISPIN                           
066900             END-IF                                                       
067000                                                                          
067100        WHEN OBKR-KDORDBEK        =  +96                                  
067200             MOVE OBKR-TIDISPIN   TO W-TIDISPIN                           
067300                                                                          
067400        WHEN OTHER                                                        
067500             MOVE ZERO            TO W-TIDISPIN                           
067600                                                                          
067700        END-EVALUATE                                                      
067800                                                                          
067900     ELSE                                                                 
068000        EVALUATE TRUE                                                     
068100        WHEN OBKR-KDORDBEK        =  +70 OR +71 OR +77                    
068200             MOVE OBKR-TITPO      TO W-TIDISPIN                           
068300                                                                          
068400        WHEN OBKR-KDORDBEK        =  +96                                  
068500             MOVE OBKR-TIDISPIN   TO W-TIDISPIN                           
068600                                                                          
068700        WHEN OBKR-KDORDBEK        =  +90 OR +91                           
068800                                         OR +99                           
068900             MOVE OBKR-TIDISPIN   TO W-TIDISPIN                           
069000                                                                          
069100             IF MID-IDARTNR            >   ZERO                           
069200               MOVE MID-IDARTNR   TO  W-IDARTNR-K6                        
069300                                                                          
069400               PERFORM IMS-GU-WDK611                                      
069500               IF SEGMENT-FINNS                                           
069600                 IF CLAG-TIDISPIN > ZERO                                  
069700                   MOVE CLAG-TIDISPIN                                     
069800                                  TO W-TIDISPIN                           
069900                 END-IF                                                   
070000               END-IF                                                     
070100             END-IF                                                       
070200                                                                          
070300        WHEN OTHER                                                        
070400             MOVE ZERO            TO W-TIDISPIN                           
070500                                                                          
070600        END-EVALUATE                                                      
070700     END-IF                                                               
070800     .                                                                    
070900     EJECT                                                                
071000                                                                          
071100 CCC-BEHANDLA-SATSARTIKLAR    SECTION.                                    
071200                                                                          
071300     MOVE OBKR-IDARTNR         TO W-IDARTNR                               
071400                                                                          
071500     PERFORM IMS-GU-SATB01                                                
071600     MOVE +1                   TO IX-TAB                                  
071700     ADD  +1                   TO IX-RAD                                  
071800     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
071900                   IX-TAB      > 5                                        
072000       MOVE DAGENS-DATUM      TO TMP1-YYMMDD                              
072100       MOVE SATB-RAD-TISTADAT TO TMP2-YYMMDD                              
072200       MOVE SATB-RAD-TISTODAT TO TMP3-YYMMDD                              
072300       PERFORM WY2000Q1                                                   
072400       IF SATB-STR-IDARTNR     < 100000000         AND                    
072500          SATB-STR-TIBORT      = 0                 AND                    
072600          TMP2-YYMMDD     NOT > TMP1-YYMMDD   AND                         
072700          TMP3-YYMMDD     NOT < TMP1-YYMMDD                               
072800            MOVE SATB-STR-IDARTNR TO W-SATB-IDARTNR(IX-TAB)               
072900                                     FLT                                  
073000            MOVE 9             TO LGD                                     
073100            CALL W009KSIF      USING FLT LGD KSIFF                        
073200            MOVE KSIFF         TO W-SATB-REKSIFFR(IX-TAB)                 
073300            ADD +1             TO IX-TAB                                  
073400       END-IF                                                             
073500       PERFORM IMS-GN-SATB01                                              
073600     END-PERFORM                                                          
073700                                                                          
073800     SUBTRACT 1                FROM IX-TAB                                
073900     COMPUTE TAB-INDX          = MAX-RAD - IX-RAD                         
074000     IF TAB-INDX               < IX-TAB                                   
074100         SUBTRACT 1            FROM IX-RAD                                
074200         PERFORM S04-BLANKA-RAD                                           
074300         MOVE +15              TO IX-RAD                                  
074400     ELSE                                                                 
074500         MOVE +1               TO IX-TAB                                  
074600         PERFORM UNTIL IX-TAB  > 5  OR                                    
074700                       W-SATB-IDARTNR(IX-TAB) = ZERO                      
074800             MOVE W-SATB-IDARTNR(IX-TAB)                                  
074900                                      TO MOD-IDARTNR-RAD(IX-RAD)          
075000             MOVE OBKR-IDORDNR7       TO MOD-IDORDNR7-RAD (IX-RAD)        
075100             MOVE SPACE               TO MOD-BEERS        (IX-RAD)        
075200                                         MOD-IDBIL        (IX-RAD)        
075300             MOVE ZERO                TO MOD-KDORDBEK     (IX-RAD)        
075400                                         MOD-KVBEART      (IX-RAD)        
075500                                         MOD-KVAVBART     (IX-RAD)        
075600                                         MOD-TIDISPIN     (IX-RAD)        
075700             IF OBKR-IDKUNDRF-RO       NOT =   '0000000   '               
075800                 MOVE OBKR-IDKUNDRF-RO(1:7)                               
075900                                       TO MOD-IDORDNR7-RO(IX-RAD)         
076000             ELSE                                                         
076100                 MOVE OBKR-IDKUNDRF(1:7)                                  
076200                                       TO MOD-IDORDNR7-RO(IX-RAD)         
076300             END-IF                                                       
076400             ADD +1                   TO IX-TAB                           
076500                                         IX-RAD                           
076600       END-PERFORM                                                        
076700       SUBTRACT 1 FROM IX-RAD                                             
076800     END-IF                                                               
076900     .                                                                    
077000     EJECT                                                                
077100                                                                          
077200 CD-SPARA-NYCKLAR SECTION.                                                
077300                                                                          
077400     MOVE OBKR-IDORDNR7        TO  MOD-IDORDNR7-NEXT                      
077500     MOVE OBKR-IDARTNR         TO  MOD-IDARTNR-NEXT                       
077600     MOVE OBKR-IDLOPNR         TO  MOD-IDLOPNR-NEXT                       
077700     MOVE OBKR-IDSEKVNR        TO  MOD-IDSEKVNR-NEXT                      
077800     MOVE OBKR-IDORDER         TO  MOD-IDORDER-NEXT                       
077900     MOVE OBKR-TIORDREG        TO  MOD-TIORDREG-NEXT                      
078000     MOVE OBKR-KDFRAKT         TO  MOD-KDFRAKT-NEXT                       
078100     MOVE OBKR-KDORDKL         TO  MOD-KDORDKL-NEXT                       
078200     MOVE OBKR-IDDC            TO  MOD-IDDC-NEXT                          
078300     MOVE OBKR-KDORDBEK        TO  MOD-KDORDBEK-NEXT                      
078400     .                                                                    
078500     EJECT                                                                
078600 CE-BACKA-BILDEN-KOD61 SECTION.                                           
078700     IF IX-SPAR > +1                                                      
078800       MOVE IX-SPAR TO IX-RAD                                             
078900       PERFORM UNTIL IX-RAD > MAX-RAD                                     
079000          PERFORM S04-BLANKA-RAD                                          
079100          ADD +1 TO IX-RAD                                                
079200       END-PERFORM                                                        
079300       MOVE SPAR-IDORDER-NEXT         TO MOD-IDORDER-NEXT                 
079400       MOVE SPAR-IDORDNR7-NEXT        TO MOD-IDORDNR7-NEXT                
079500       MOVE SPAR-IDARTNR-NEXT         TO MOD-IDARTNR-NEXT                 
079600       MOVE SPAR-IDLOPNR-NEXT         TO MOD-IDLOPNR-NEXT                 
079700       MOVE SPAR-IDSEKVNR-NEXT        TO MOD-IDSEKVNR-NEXT                
079800       MOVE SPAR-IDDC-NEXT            TO MOD-IDDC-NEXT                    
079900       MOVE SPAR-KDORDBEK-NEXT        TO MOD-KDORDBEK-NEXT                
080000       MOVE SPAR-KDORDKL-NEXT         TO MOD-KDORDKL-NEXT                 
080100       MOVE SPAR-KDFRAKT-NEXT         TO MOD-KDFRAKT-NEXT                 
080200       MOVE SPAR-TIORDREG-NEXT        TO MOD-TIORDREG-NEXT                
080300     END-IF                                                               
080400     .                                                                    
080500     EJECT                                                                
080600                                                                          
080700 E-KONTROLLERA-OM-TOM-SIDA  SECTION.                                      
080800                                                                          
080900     IF IX-RAD                 = 1                                        
081000         MOVE 'B10'            TO MOD-IDMFSFEL                            
081100     END-IF                                                               
081200     .                                                                    
081300     EJECT                                                                
081400                                                                          
081500 F-BERAKNA-MAX-MOD-LANGD    SECTION.                                      
081600                                                                          
081700                                                                          
081800     COMPUTE MAX-MOD-LANGD = LENGTH OF MOD-W9O33301 + 4                   
081900     MOVE 13                   TO  IX-RAD                                 
082000     PERFORM UNTIL IX-RAD      = 0                                        
082100         IF MOD-IDORDNR7-RAD(IX-RAD) = ZERO                               
082200             SUBTRACT +78      FROM MAX-MOD-LANGD                         
082300             SUBTRACT +1       FROM IX-RAD                                
082400         ELSE                                                             
082500             MOVE ZERO         TO IX-RAD                                  
082600         END-IF                                                           
082700     END-PERFORM                                                          
082800     .                                                                    
082900     EJECT                                                                
083000                                                                          
083100 S01-SKAPA-OKVAL-NYCKEL       SECTION.                                    
083200                                                                          
083300     MOVE LOW-VALUE            TO  W-WDQ1B1KY-MIN-X                       
083400     MOVE HIGH-VALUE           TO  W-WDQ1B1KY-MAX-X                       
083500                                                                          
083600     MOVE MID-IDDISTR          TO  W-Q1B-IDDISTR-MIN                      
083700                                   W-Q1B-IDDISTR-MAX                      
083800     MOVE MID-IDKUNDNR         TO  W-Q1B-IDKUNDNR-MIN                     
083900                                   W-Q1B-IDKUNDNR-MAX                     
084000                                                                          
084100     MOVE MID-TIORDREG    TO TMP1-YYMMDD                                  
084200     MOVE ZERO            TO TMP2-YYMMDD                                  
084300     PERFORM WY2000P1                                                     
084400     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
084500         MOVE MID-TIORDREG       TO WS-AAMMDD                             
084600         IF WS-AA < 50                                                    
084700           MOVE 20               TO WS-SEKEL-TAL                          
084800         ELSE                                                             
084900           MOVE 19               TO WS-SEKEL-TAL                          
085000         END-IF                                                           
085100         COMPUTE WS-TITIORDD-9KOMPL = 999999999 - WS-TITIORDD             
085200         MOVE WS-TITIORDD-9KOMPL TO W-Q1B-TITIOWDD9-MIN                   
085300                                    W-Q1B-TITIOWDD9-MAX                   
085400     END-IF                                                               
085500                                                                          
085600     IF MID-KDORDKL            > SPACE                                    
085700         MOVE MID-KDORDKL     TO  W-KDORDKL                               
085800     END-IF                                                               
085900                                                                          
086000     IF MID-IDORDNR7           >   ZERO                                   
086100         MOVE MID-IDORDNR7     TO  W-IDKUNDRF                             
086200     END-IF                                                               
086300                                                                          
086400     IF MID-IDARTNR            >   ZERO                                   
086500         MOVE MID-IDARTNR      TO  W-IDARTNR-Q1                           
086600     END-IF                                                               
086700     .                                                                    
086800     EJECT                                                                
086900                                                                          
087000 S02-LAES-WDQ1B-OKVAL SECTION.                                            
087100                                                                          
087200     IF MID-KDORDKL NOT = SPACE                                           
087300        IF MID-IDORDNR7 > ZERO                                            
087400           IF MID-IDARTNR > ZERO                                          
087500              PERFORM IMS-GN-Q1B-KL-ORDNR-ARTNR                           
087600           ELSE                                                           
087700              PERFORM IMS-GN-Q1B-KL-ORDNR                                 
087800           END-IF                                                         
087900        ELSE                                                              
088000           IF MID-IDARTNR > ZERO                                          
088100              PERFORM IMS-GN-Q1B-KL-ARTNR                                 
088200           ELSE                                                           
088300              PERFORM IMS-GN-Q1B-KL                                       
088400           END-IF                                                         
088500        END-IF                                                            
088600     ELSE                                                                 
088700        IF MID-IDORDNR7 > ZERO                                            
088800           IF MID-IDARTNR > ZERO                                          
088900              PERFORM IMS-GN-Q1B-ORDNR-ARTNR                              
089000           ELSE                                                           
089100              PERFORM IMS-GN-Q1B-ORDNR                                    
089200           END-IF                                                         
089300        ELSE                                                              
089400           IF MID-IDARTNR > ZERO                                          
089500              PERFORM IMS-GN-Q1B-ARTNR                                    
089600           ELSE                                                           
089700              PERFORM IMS-GN-WLORQO01-OKVAL                               
089800           END-IF                                                         
089900        END-IF                                                            
090000     END-IF                                                               
090100     .                                                                    
090200     EJECT                                                                
090300                                                                          
090400 S03-LAES-WDQ101-UNIK SECTION.                                            
090500                                                                          
090600     MOVE  SEQB-IDORDER        TO W-Q1-IDORDER                            
090700     MOVE  SEQB-IDARTNR        TO W-Q1-IDARTNR                            
090800     MOVE  SEQB-IDLOPNR        TO W-Q1-IDLOPNR                            
090900     MOVE  SEQB-IDSEKVNR       TO W-Q1-IDSEKVNR                           
091000     MOVE  SEQB-IDDC           TO W-Q1-IDDC                               
091100     MOVE  SEQB-KDORDBEK       TO W-Q1-KDORDBEK                           
091200                                                                          
091300     PERFORM IMS-GU-WLORQM01                                              
091400     .                                                                    
091500     EJECT                                                                
091600                                                                          
091700 S04-BLANKA-RAD   SECTION.                                                
091800                                                                          
091900     MOVE ZERO                 TO  MOD-IDORDNR7-RAD    (IX-RAD)           
092000                                   MOD-IDARTNR-RAD     (IX-RAD)           
092100                                   MOD-KVBEART         (IX-RAD)           
092200                                   MOD-KVAVBART        (IX-RAD)           
092300                                   MOD-KDORDBEK        (IX-RAD)           
092400                                   MOD-TIDISPIN        (IX-RAD)           
092500                                   MOD-IDORDNR7-RO     (IX-RAD)           
092600     MOVE SPACE                TO  MOD-BEERS           (IX-RAD)           
092700                                   MOD-IDDC            (IX-RAD)           
092800                                   MOD-IDBIL           (IX-RAD)           
092900     .                                                                    
093000     EJECT                                                                
093100                                                                          
093200 S05-SKAPA-OKVAL-POSITION     SECTION.                                    
093300                                                                          
093400     MOVE LOW-VALUE            TO  W-WDQ1B1KY-MIN-X                       
093500     MOVE HIGH-VALUE           TO  W-WDQ1B1KY-MAX-X                       
093600                                                                          
093700     MOVE MID-IDDISTR          TO  W-Q1B-IDDISTR-MIN                      
093800                                   W-Q1B-IDDISTR-MAX                      
093900                                                                          
094000     MOVE MID-IDKUNDNR         TO  W-Q1B-IDKUNDNR-MIN                     
094100                                   W-Q1B-IDKUNDNR-MAX                     
094200                                                                          
094300     MOVE OBKR-TITIORDD-9KOMPL TO  W-Q1B-TITIOWDD9-MIN                    
094400                                   W-Q1B-TITIOWDD9-MAX                    
094500                                                                          
094600     IF MID-KDORDKL            > SPACE                                    
094700         MOVE MID-KDORDKL     TO  W-KDORDKL                               
094800     END-IF                                                               
094900                                                                          
095000     IF MID-IDORDNR7           >   ZERO                                   
095100         MOVE MID-IDORDNR7     TO  W-IDKUNDRF                             
095200     END-IF                                                               
095300                                                                          
095400     IF MID-IDARTNR            >   ZERO                                   
095500         MOVE MID-IDARTNR      TO  W-IDARTNR-Q1                           
095600     END-IF                                                               
095700     .                                                                    
095800     EJECT                                                                
095900                                                                          
096000* IMS SEKTIONER                                                           
096100     SKIP2                                                                
096200 IMS-GET-MSG SECTION.                                                     
096300     MOVE '  QC' TO GODK-STATUSKODER                                      
096400     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
096500     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
096600     PERFORM IMS-STATUSKONTROLL                                           
096700     .                                                                    
096800     SKIP2                                                                
096900 IMS-INSERT-MSG SECTION.                                                  
097000     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
097100     MOVE SPACE TO GODK-STATUSKODER                                       
097200     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA WS-MODNAMN               
097300     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
097400     PERFORM IMS-STATUSKONTROLL                                           
097500     .                                                                    
097600     EJECT                                                                
097700 IMS-GU-WLORQM01              SECTION.                                    
097800     STRING 'WLORQM01(WDQ101KY =' W-WDQ101KY-X ')'                        
097900            DELIMITED BY SIZE INTO SSA1                                   
098000     MOVE '  '    TO GODK-STATUSKODER                                     
098100     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA SSA1                      
098200     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
098300     PERFORM IMS-STATUSKONTROLL                                           
098400     .                                                                    
098500     SKIP2                                                                
098600 IMS-GU-WLORQO01-KVAL         SECTION.                                    
098700     STRING 'WLORQO01(WDQ1B1KY =' W-WDQ1B1KY-MIN-X ')'                    
098800            DELIMITED BY SIZE INTO SSA1                                   
098900     MOVE '  GE'  TO GODK-STATUSKODER                                     
099000     CALL CBLTDLI USING GU ORQO-PCB DLI-IO-AREA SSA1                      
099100     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
099200     PERFORM IMS-STATUSKONTROLL                                           
099300     .                                                                    
099400     SKIP2                                                                
099500 IMS-GN-WLORQO01-OKVAL         SECTION.                                   
099600     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
099700                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X ')'                    
099800            DELIMITED BY SIZE INTO SSA1                                   
099900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
100000     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
100100     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
100200     PERFORM IMS-STATUSKONTROLL                                           
100300     .                                                                    
100400     EJECT                                                                
100500 IMS-GN-Q1B-KL-ORDNR-ARTNR SECTION.                                       
100600     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
100700                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
100800                    '&KDORDKL  =' W-KDORDKL-X                             
100900                    '&IDKUNDRF =' W-IDKUNDRF-X                            
101000                    '&IDARTNR  =' W-IDARTNR-Q1-X ')'                      
101100            DELIMITED BY SIZE INTO SSA1                                   
101200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
101300     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
101400     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
101500     PERFORM IMS-STATUSKONTROLL                                           
101600     .                                                                    
101700     SKIP2                                                                
101800 IMS-GN-Q1B-KL-ORDNR SECTION.                                             
101900     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
102000                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
102100                    '&KDORDKL  =' W-KDORDKL-X                             
102200                    '&IDKUNDRF =' W-IDKUNDRF-X ')'                        
102300            DELIMITED BY SIZE INTO SSA1                                   
102400     MOVE '  GEGB' TO GODK-STATUSKODER                                    
102500     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
102600     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
102700     PERFORM IMS-STATUSKONTROLL                                           
102800     .                                                                    
102900     SKIP2                                                                
103000 IMS-GN-Q1B-KL-ARTNR SECTION.                                             
103100     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
103200                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
103300                    '&KDORDKL  =' W-KDORDKL-X                             
103400                    '&IDARTNR  =' W-IDARTNR-Q1-X ')'                      
103500            DELIMITED BY SIZE INTO SSA1                                   
103600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
103700     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
103800     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
103900     PERFORM IMS-STATUSKONTROLL                                           
104000     .                                                                    
104100     SKIP2                                                                
104200 IMS-GN-Q1B-KL SECTION.                                                   
104300     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
104400                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
104500                    '&KDORDKL  =' W-KDORDKL-X ')'                         
104600            DELIMITED BY SIZE INTO SSA1                                   
104700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
104800     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
104900     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
105000     PERFORM IMS-STATUSKONTROLL                                           
105100     .                                                                    
105200     SKIP2                                                                
105300 IMS-GN-Q1B-ORDNR-ARTNR SECTION.                                          
105400     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
105500                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
105600                    '&IDKUNDRF =' W-IDKUNDRF-X                            
105700                    '&IDARTNR  =' W-IDARTNR-Q1-X ')'                      
105800            DELIMITED BY SIZE INTO SSA1                                   
105900     MOVE '  GEGB' TO GODK-STATUSKODER                                    
106000     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
106100     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
106200     PERFORM IMS-STATUSKONTROLL                                           
106300     .                                                                    
106400     SKIP2                                                                
106500 IMS-GN-Q1B-ORDNR SECTION.                                                
106600     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
106700                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
106800                    '&IDKUNDRF =' W-IDKUNDRF-X ')'                        
106900            DELIMITED BY SIZE INTO SSA1                                   
107000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
107100     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
107200     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
107300     PERFORM IMS-STATUSKONTROLL                                           
107400     .                                                                    
107500     SKIP2                                                                
107600 IMS-GN-Q1B-ARTNR SECTION.                                                
107700     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
107800                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
107900                    '&IDARTNR  =' W-IDARTNR-Q1-X ')'                      
108000            DELIMITED BY SIZE INTO SSA1                                   
108100     MOVE '  GEGB' TO GODK-STATUSKODER                                    
108200     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
108300     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
108400     PERFORM IMS-STATUSKONTROLL                                           
108500     .                                                                    
108600     EJECT                                                                
108700 IMS-GU-SATB01 SECTION.                                                   
108800     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
108900          DELIMITED BY SIZE INTO SSA1                                     
109000     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
109100          DELIMITED BY SIZE INTO SSA2                                     
109200     MOVE '  GE' TO GODK-STATUSKODER                                      
109300     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA2 SSA1 SSA2                
109400     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
109500     PERFORM IMS-STATUSKONTROLL                                           
109600     .                                                                    
109700     SKIP2                                                                
109800 IMS-GN-SATB01 SECTION.                                                   
109900     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
110000          DELIMITED BY SIZE INTO SSA1                                     
110100     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
110200          DELIMITED BY SIZE INTO SSA2                                     
110300     MOVE '  GE' TO GODK-STATUSKODER                                      
110400     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA2 SSA1 SSA2                
110500     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
110600     PERFORM IMS-STATUSKONTROLL                                           
110700     .                                                                    
110800     EJECT                                                                
110900 IMS-GU-WLORQI01-KVAL SECTION.                                            
111000     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
111100          DELIMITED BY SIZE INTO SSA1                                     
111200     MOVE '  GE' TO GODK-STATUSKODER                                      
111300     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-WDQ201 SSA1                    
111400     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
111500     PERFORM IMS-STATUSKONTROLL                                           
111600     .                                                                    
111700     EJECT                                                                
111710 IMS-GNP-WLORQI12      SECTION.                                           
111720                                                                          
111730     MOVE 'WLORQI12' TO SSA1                                              
111750     MOVE '  GE' TO GODK-STATUSKODER                                      
111760     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-WDQ212 SSA1                   
111770     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
111780     PERFORM IMS-STATUSKONTROLL                                           
111790     .                                                                    
111791     EJECT                                                                
111800 IMS-GU-WDK611 SECTION.                                                   
111900     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
112000          DELIMITED BY SIZE INTO SSA1                                     
112100     MOVE 'WDK611   '      TO SSA2                                        
112200     MOVE '  GE'           TO GODK-STATUSKODER                            
112300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
112400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
112500     PERFORM IMS-STATUSKONTROLL                                           
112600     .                                                                    
112700     EJECT                                                                
112800 IMS-STATUSKONTROLL SECTION.                                              
112900     SET STATUS-IX TO 1                                                   
113000     SEARCH GODK-STATUS                                                   
113100       AT END                                                             
113200         CALL FELLOG                                                      
113300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
113400         CONTINUE                                                         
113500     END-SEARCH                                                           
113600     .                                                                    
113700     EJECT                                                                
113800*    -COPY WY2000Q1                                                       
113900     EJECT                                                                
114000*    -COPY WY2000P1                                                       
