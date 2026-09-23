000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W9034300.                                                
000400 AUTHOR.         GERRY CARMICHAEL.                                        
000500 DATE-WRITTEN.   JANUARI 98.                                              
000600*                                                                         
000700*                                                                         
000800*REMARKS.                                                                 
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
002400*        TRANSAKTION: W90343T                                             
002500*        MID:         W9I34301                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W9O34301                                            
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP3                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500*    -COPY WY2000W1                                                       
003600     SKIP3                                                                
003700 77  IDPGM                       PIC X(8)    VALUE 'W9034300'.            
003800 77  WS-MODNAMN                  PIC X(8)    VALUE 'W9O34301'.            
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
005900 01  W-IDKUNDRF-RO               PIC 9(7)   VALUE ZERO.                   
006000 01  FILLER REDEFINES W-IDKUNDRF-RO.                                      
006100     03  FILLER                  PIC 9(2).                                
006200     03  W-IDKUNDRF-RO-3--7      PIC 9(5).                                
006300                                                                          
006400     EJECT                                                                
006500*      --- VALID IDDC CODES                                               
006600*                                                                         
006700*01    -COPY WWDC99                                                       
006800       EJECT                                                              
006900*  --- FÖR ATT FÅ RÄTT SEKEL VID ANROP TILL W218ETA                       
007000 01  WS-ETA-DATUM                PIC 9(6).                                
007100 01  FILLER REDEFINES WS-ETA-DATUM.                                       
007200     03  WS-ETA-DATUM-AAR        PIC 9(2).                                
007300     03  WS-ETA-DATUM-MAANAD     PIC 9(2).                                
007400     03  WS-ETA-DATUM-DAG        PIC 9(2).                                
007500     EJECT                                                                
007600* ----- DYNAMISKA SUBPROGRAM                                              
007700 01  DYNAMISKA-SUBPGM.                                                    
007800     03 CBLTDLI                  PIC X(8)    VALUE 'CBLTDLI '.            
007900     03 W009KSIF                 PIC X(8)    VALUE 'W009KSIF'.            
008000     03 FELLOG                   PIC X(8)    VALUE 'FELLOG  '.            
008100     03 W218ETA                  PIC X(8)    VALUE 'W218ETA '.            
008200                                                                          
008300 01 FILLER                       PIC  X(8)   VALUE 'LETA'.                
008400*   -COPY W218LETA -PRE ETA-.                                             
008500     EJECT                                                                
008600 01  W009KSIF-PARM.                                                       
008700     03 FLT                    PIC 9(9).                                  
008800     03 LGD                    PIC 9(1).                                  
008900     03 KSIFF                  PIC 9(1).                                  
009000     EJECT                                                                
009100* ----- INDEXFÄLT                                                         
009200 77  IX-RAD                      PIC S9(4)   VALUE +0  COMP SYNC.         
009300 77  IX-TAB                      PIC S9(4)   VALUE +0  COMP SYNC.         
009400 77  IX-SPAR                     PIC S9(4)   VALUE +0  COMP SYNC.         
009500 77  TAB-INDX                    PIC S9(4)   VALUE +0  COMP SYNC.         
009600                                                                          
009700* ----- SWITCHAR                                                          
009800 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009900     88  NYCKLAR-OK                          VALUE 'J'.                   
010000                                                                          
010100                                                                          
010200 77  ORDER-SAKNAS-SW             PIC X       VALUE 'N'.                   
010300     88  ORDER-SAKNAS                        VALUE 'J'.                   
010400                                                                          
010500 77  ORDER-EJ-KLAR-SW            PIC X       VALUE 'N'.                   
010600     88  ORDER-EJ-KLAR                       VALUE 'J'.                   
010700                                                                          
010800 01  WS-TITIORDD-9KOMPL          PIC 9(9).                                
010900                                                                          
011000 01  W-SATB-TABELL.                                                       
011100     03 W-SATB-IDARTNR           PIC S9(9)   COMP-3 OCCURS 5.             
011200     03 W-SATB-REKSIFFR          PIC S9(1)   COMP-3 OCCURS 5.             
011300                                                                          
011400 01  WS-TITIORDD                 PIC 9(8).                                
011500 01  FILLER    REDEFINES  WS-TITIORDD.                                    
011600     05  WS-SEKEL-TAL            PIC 9(2).                                
011700     05  WS-AAMMDD               PIC 9(6).                                
011800     05  FILLER REDEFINES WS-AAMMDD.                                      
011900        07  WS-AA                PIC 9(2).                                
012000        07  FILLER               PIC 9(4).                                
012100                                                                          
012200     EJECT                                                                
012300 01    FILLER                    PIC X(16)   VALUE 'MID-AREA'.            
012400     SKIP3                                                                
012500*01    MID -COPY W9I34301.                                                
012600     EJECT                                                                
012700 01    FILLER                    PIC X(16)   VALUE 'MSG-AREA'.            
012800     SKIP3                                                                
012900*01    -COPY WMSGAREA                                                     
013000     EJECT                                                                
013100*  03    MOD -COPY W9O34301  -RED MSG-AREA.                               
013200     EJECT                                                                
013300*PIE VERSION MOD VDI                                                      
013400 01    FILLER                   PIC X(16)   VALUE 'IMS-WS     '.          
013500                                                                          
013600 01    NYCKLAR-TILL-DLI.                                                  
013700                                                                          
013800   03    W-WDQ1B1KY-MIN-X.                                                
013900     05    W-Q1B-IDDISTR-MIN      PIC S9(5)   VALUE ZERO  COMP-3.         
014000     05    W-Q1B-IDKUNDNR-MIN     PIC S9(7)   VALUE ZERO  COMP-3.         
014100     05    W-Q1B-TITIOWDD9-MIN    PIC S9(9)   VALUE ZERO  COMP-3.         
014200     05    W-Q1B-KDFRAKT-MIN      PIC S9(3)   VALUE ZERO  COMP-3.         
014300     05    W-Q1B-KDORDKL-MIN      PIC S9(1)   VALUE ZERO  COMP-3.         
014400     05    W-Q1B-IDKUNDRF-MIN     PIC X(10)   VALUE SPACE.                
014500     05    W-Q1B-IDARTNR-MIN      PIC S9(9)   VALUE ZERO  COMP-3.         
014600     05    W-Q1B-IDLOPNR-MIN      PIC S9(3)   VALUE ZERO  COMP-3.         
014700     05    W-Q1B-IDSEKVNR-MIN     PIC S9(3)   VALUE ZERO  COMP-3.         
014800     05    W-Q1B-IDDC-MIN         PIC  X(2)   VALUE SPACE.                
014900     05    W-Q1B-IDORDER-MIN      PIC S9(7)   VALUE ZERO  COMP-3.         
015000     05    W-Q1B-KDORDBEK-MIN     PIC  9(2)   VALUE ZERO.                 
015100                                                                          
015200   03  W-WDQ1B1KY-MAX-X.                                                  
015300     05  W-Q1B-IDDISTR-MAX      PIC S9(5) VALUE ZERO     COMP-3.          
015400     05  W-Q1B-IDKUNDNR-MAX     PIC S9(7) VALUE ZERO     COMP-3.          
015500     05  W-Q1B-TITIOWDD9-MAX    PIC S9(9) VALUE ZERO     COMP-3.          
015600     05  W-Q1B-KDFRAKT-MAX      PIC S9(3) VALUE ZERO     COMP-3.          
015700     05  W-Q1B-KDORDKL-MAX      PIC S9(1) VALUE ZERO     COMP-3.          
015800     05  W-Q1B-IDKUNDRF-MAX     PIC X(10) VALUE SPACE.                    
015900     05  W-Q1B-IDARTNR-MAX      PIC S9(9) VALUE ZERO     COMP-3.          
016000     05  W-Q1B-IDLOPNR-MAX      PIC S9(3) VALUE ZERO     COMP-3.          
016100     05  W-Q1B-IDSEKVNR-MAX     PIC S9(3) VALUE ZERO     COMP-3.          
016200     05  W-Q1B-IDDC-MAX         PIC  X(2) VALUE SPACE.                    
016300     05  W-Q1B-IDORDER-MAX      PIC S9(7) VALUE ZERO     COMP-3.          
016400     05  W-Q1B-KDORDBEK-MAX     PIC  9(2) VALUE ZERO.                     
016500                                                                          
016600   03    W-WDQ101KY-X.                                                    
016700     05    W-Q1-IDORDER        PIC S9(7)   VALUE ZERO  COMP-3.            
016800     05    W-Q1-IDARTNR        PIC S9(9)   VALUE ZERO  COMP-3.            
016900     05    W-Q1-IDLOPNR        PIC S9(3)   VALUE ZERO  COMP-3.            
017000     05    W-Q1-IDSEKVNR       PIC S9(3)   VALUE ZERO  COMP-3.            
017100     05    W-Q1-IDDC           PIC  X(2)   VALUE SPACE.                   
017200     05    W-Q1-KDORDBEK       PIC  9(2)   VALUE ZERO.                    
017300     EJECT                                                                
017400                                                                          
017500   03    W-KDORDKL-X.                                                     
017600     05    W-KDORDKL             PIC S9(1)   VALUE ZERO  COMP-3.          
017700     SKIP2                                                                
017800   03    W-IDKUNDRF-X.                                                    
017900     05    W-IDKUNDRF            PIC X(10)   VALUE SPACE.                 
018000     SKIP2                                                                
018100   03    W-IDARTNR-Q1-X.                                                  
018200     05    W-IDARTNR-Q1          PIC S9(9)   VALUE ZERO  COMP-3.          
018300     SKIP2                                                                
018400   03  W-WDQ2CSEQ-X.                                                      
018500       05  W-Q2CSEQ-IDDISTR         PIC S9(05) VALUE ZERO COMP-3.         
018600       05  W-Q2CSEQ-IDKUNDNR        PIC S9(07) VALUE ZERO COMP-3.         
018700       05  W-Q2CSEQ-IDKUNDRF        PIC  X(10) VALUE SPACE.               
018800     SKIP2                                                                
018900   03  W-WDJ1CSEQ-X.                                                      
019000     05  W-IDLEVNR                 PIC X(5)   VALUE SPACE.                
019100     05  FILLER                    PIC X(30)  VALUE SPACE.                
019200     05  W-IDARTNR                 PIC S9(9)  VALUE +0   COMP-3.          
019300                                                                          
019400   03  W-IDLEVNR-X.                                                       
019500     05  W-IDLEVNR                  PIC X(5)  VALUE '1002'.               
019600                                                                          
019700   03  W-IDARTNR-K6-X.                                                    
019800     05  W-IDARTNR-K6           PIC S9(9) VALUE ZERO COMP-3.              
019900                                                                          
019910   03  W-IDDC-X.                                                          
019920     05 W-IDDC                  PIC X(2).                                 
020000     EJECT                                                                
020100*                        **** STATUS-KOD FRÅN IMS                         
020200   03    STATUS-WS               PIC XX.                                  
020300     88    SEGMENT-FINNS                     VALUE '  '.                  
020400     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
020500     88    SEGMENT-SLUT                      VALUE 'GB'.                  
020600     SKIP3                                                                
020700   03    WDQ1-STATUS-WS          PIC XX.                                  
020800     88    WDQ1-SEGMENT-FINNS                VALUE '  '.                  
020900     88    WDQ1-SEGMENT-SAKNAS               VALUE 'GE'.                  
021000     88    WDQ1-SEGMENT-SLUT                 VALUE 'GB'.                  
021100     SKIP3                                                                
021200   03    GODK-STATUSKODER.                                                
021300     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
021400     SKIP3                                                                
021500 01    SSA1                      PIC X(164).                              
021600 01    SSA2                      PIC X(64).                               
021700     EJECT                                                                
021800*                            IMS FUNKTIONSKODER                           
021900*01    -COPY W0003                                                        
022000     EJECT                                                                
022100*                            DLI INPUT-OUTPUT AREA                        
022200 01    DLI-IO-AREA.                                                       
022300   03    IO-AREA                 PIC X(700)  VALUE SPACE.                 
022400     SKIP3                                                                
022500*  03    WLORQO01  -COPY WDQ1B1                    -RED IO-AREA           
022600     EJECT                                                                
022700*  03    WLORQM01  -COPY WDQ101                    -RED IO-AREA           
022800     EJECT                                                                
023100 01    DLI-IO-AREA2.                                                      
023200   03    IO-AREA2                PIC X(500)  VALUE SPACE.                 
023300     SKIP3                                                                
023400     03  WLSATB01 REDEFINES IO-AREA2.                                     
023500*        05  -COPY WDJ111      -PRE SATB-                                 
023600*        05  -COPY WDJ101      -PRE SATB-                                 
023700     EJECT                                                                
023800 01  DLI-IO-WDK611.                                                       
023900*    03  WDK611      -COPY WDK611                                         
024000     EJECT                                                                
024010 01  DLI-IO-WDQ201.                                                       
024020*    03  WDQ201      -COPY WDQ201                                         
024030     EJECT                                                                
024040 01  DLI-IO-WDQ212.                                                       
024050*    03  WDQ212      -COPY WDQ212                                         
024060     EJECT                                                                
024100 01  DLI-IO-WDQ201-RO.                                                    
024200*    03  -COPY WDQ201 -PRE RO-                                            
024300     EJECT                                                                
024400 01  FILLER                 PIC X(12) VALUE 'DUMMY-ARTC'.                 
024500 01  ETA-ARTC-PCB           PIC X.                                        
024600 01  FILLER                 PIC X(12) VALUE 'DUMMY-LEVA'.                 
024700 01  ETA-LEVA-PCB           PIC X.                                        
024800 LINKAGE SECTION.                                                         
024900*01    -COPY W0009     -PRE MSG-                                          
025000                                                                          
025100*01    -COPY W0008     -PRE ORQO-                                         
025200     05  FILLER                  PIC X.                                   
025300                                                                          
025400*01    -COPY W0008     -PRE ORQM-                                         
025500     05  FILLER                  PIC X.                                   
025600                                                                          
025700*01    -COPY W0008     -PRE SATB-                                         
025800     05  FILLER                  PIC X.                                   
025900                                                                          
026000*01    -COPY W0008     -PRE ORQI-                                         
026100     05  FILLER                  PIC X.                                   
026200                                                                          
026300*01    -COPY W0008     -PRE WDK6-                                         
026400     05  FILLER                  PIC X.                                   
026500                                                                          
026600     EJECT                                                                
026700 01  ETA-WDK7-PCB           PIC X.                                        
026800 01  ETA-INLC-PCB           PIC X.                                        
026900 01  ETA-WDB6-PCB           PIC X.                                        
027000 01  ETA-WDD9-PCB           PIC X.                                        
027100     EJECT                                                                
027200                                                                          
027300 PROCEDURE DIVISION  USING MSG-PCB ORQO-PCB ORQM-PCB                      
027400                                   SATB-PCB ORQI-PCB                      
027500                                   WDK6-PCB                               
027600                                   ETA-WDK7-PCB ETA-INLC-PCB              
027700                                   ETA-WDB6-PCB ETA-WDD9-PCB.             
027800     ENTRY 'DLITCBL' USING MSG-PCB ORQO-PCB ORQM-PCB                      
027900                                   SATB-PCB ORQI-PCB                      
028000                                   WDK6-PCB                               
028100                                   ETA-WDK7-PCB ETA-INLC-PCB              
028200                                   ETA-WDB6-PCB ETA-WDD9-PCB.             
028300                                                                          
028400 STYR      SECTION.                                                       
028500                                                                          
028600     PERFORM IMS-GET-MSG                                                  
028700     IF SEGMENT-FINNS                                                     
028800        PERFORM A-INIT                                                    
028900        PERFORM B-KONTROLLERA-NYCKLAR                                     
029000        IF NYCKLAR-OK                                                     
029100           PERFORM C-BEHANDLA-RADER                                       
029200           IF ORDER-SAKNAS OR  ORDER-EJ-KLAR                              
029300              CONTINUE                                                    
029400           ELSE                                                           
029500              PERFORM E-KONTROLLERA-OM-TOM-SIDA                           
029600           END-IF                                                         
029700        END-IF                                                            
029800        PERFORM F-BERAKNA-MAX-MOD-LANGD                                   
029900        MOVE MAX-MOD-LANGD        TO MSG-KVLL                             
030000        PERFORM IMS-INSERT-MSG                                            
030100     END-IF                                                               
030200     MOVE ZERO                 TO RETURN-CODE                             
030300     GOBACK                                                               
030400     .                                                                    
030500     EJECT                                                                
030600                                                                          
030700 A-INIT     SECTION.                                                      
030800                                                                          
030900     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W9I34301                    
031000     MOVE LOW-VALUE                    TO MSG-AREA                        
031100     MOVE '9343'                       TO MOD-IDTRANS                     
031200     MOVE ZERO                         TO MOD-IDMFSFEL                    
031300     MOVE MID-TIORDREG                 TO MOD-TIORDREG                    
031400     MOVE ZERO                         TO MOD-IDARTNR-NEXT                
031500                                          MOD-IDORDNR7-NEXT               
031600                                          MOD-IDLOPNR-NEXT                
031700                                          MOD-IDSEKVNR-NEXT               
031800                                          MOD-IDORDER-NEXT                
031900                                          MOD-TIORDREG-NEXT               
032000                                          MOD-KDFRAKT-NEXT                
032100                                          MOD-KDORDKL-NEXT                
032200                                          MOD-KDORDBEK-NEXT               
032300                                                                          
032400     MOVE SPACE                        TO MOD-IDDC-NEXT                   
032500                                                                          
032600     MOVE +1                   TO  IX-RAD                                 
032700     PERFORM UNTIL             IX-RAD > MAX-RAD                           
032800         MOVE ZERO             TO  MOD-IDORDNR7-RAD(IX-RAD)               
032900                                   MOD-IDARTNR-RAD(IX-RAD)                
033000                                   MOD-KVBEART(IX-RAD)                    
033100                                   MOD-KVAVBART(IX-RAD)                   
033200                                   MOD-KDORDBEK(IX-RAD)                   
033300                                   MOD-TIDISPIN(IX-RAD)                   
033400                                   MOD-IDORDNR7-RO(IX-RAD)                
033500         MOVE SPACE            TO  MOD-BEERS(IX-RAD)                      
033600                                   MOD-IDBIL(IX-RAD)                      
033700                                   MOD-IDDC    (IX-RAD)                   
033800                                   MOD-BERADREF(IX-RAD)                   
033900         ADD +1                TO  IX-RAD                                 
034000     END-PERFORM                                                          
034100                                                                          
034200     MOVE ZERO                 TO  W-SPAR-KDORDBEK                        
034300                                                                          
034400     MOVE +1                   TO  IX-TAB                                 
034500     PERFORM UNTIL             IX-TAB > MAX-TAB                           
034600         MOVE ZERO             TO  W-SATB-IDARTNR(IX-TAB)                 
034700         ADD +1                TO  IX-TAB                                 
034800     END-PERFORM                                                          
034900                                                                          
035000     MOVE NEJ                  TO ORDER-SAKNAS-SW                         
035100                                  ORDER-EJ-KLAR-SW                        
035200     ACCEPT DAGENS-DATUM FROM DATE                                        
035300     .                                                                    
035400     EJECT                                                                
035500                                                                          
035600 B-KONTROLLERA-NYCKLAR    SECTION.                                        
035700                                                                          
035800     IF MID-IDDISTR            NUMERIC AND                                
035900        MID-IDKUNDNR           NUMERIC                                    
036000       IF MID-IDARTNR      NOT NUMERIC OR                                 
036100          MID-TIORDREG     NOT NUMERIC                                    
036200         MOVE NEJ              TO NYCKLAR-SW                              
036300         MOVE 'B10'            TO MOD-IDMFSFEL                            
036400       END-IF                                                             
036500     ELSE                                                                 
036600       MOVE NEJ                TO NYCKLAR-SW                              
036700       MOVE 'B01'              TO MOD-IDMFSFEL                            
036800     END-IF                                                               
036900     .                                                                    
037000     EJECT                                                                
037100                                                                          
037200 C-BEHANDLA-RADER         SECTION.                                        
037300                                                                          
037400     IF MID-IDORDNR7-NEXT      >  ZERO                                    
037500         PERFORM CA-SKAPA-BLADDRINGS-NYCKEL                               
037600         PERFORM IMS-GU-WLORQO01-KVAL                                     
037700         MOVE STATUS-WS    TO  WDQ1-STATUS-WS                             
037800         IF SEGMENT-FINNS                                                 
037900            PERFORM S03-LAES-WDQ101-UNIK                                  
038000         END-IF                                                           
038100     ELSE                                                                 
038200         IF MID-IDORDNR7       >  ZERO                                    
038300             PERFORM CB-KOLLA-OHUV                                        
038400         END-IF                                                           
038500         IF ORDER-SAKNAS OR ORDER-EJ-KLAR                                 
038600             CONTINUE                                                     
038700         ELSE                                                             
038800             PERFORM S01-SKAPA-OKVAL-NYCKEL                               
038900             PERFORM S02-LAES-WDQ1B-OKVAL                                 
039000             MOVE STATUS-WS    TO  WDQ1-STATUS-WS                         
039100             IF SEGMENT-FINNS                                             
039200                PERFORM S03-LAES-WDQ101-UNIK                              
039300             END-IF                                                       
039400         END-IF                                                           
039500     END-IF                                                               
039600                                                                          
039700     MOVE +1                   TO IX-RAD                                  
039800                                                                          
039900     IF ORDER-SAKNAS OR ORDER-EJ-KLAR                                     
040000         CONTINUE                                                         
040100     ELSE                                                                 
040200         PERFORM UNTIL WDQ1-SEGMENT-SAKNAS       OR                       
040300                       WDQ1-SEGMENT-SLUT         OR                       
040400                       IX-RAD  > MAX-RAD                                  
040500                                                                          
040600             PERFORM CC-REDIGERA-ORDERBEK-RAD                             
040700             ADD +1       TO  IX-RAD                                      
040800     EJECT                                                                
040900                                                                          
041000             IF IX-RAD         =   +16                                    
041100                 CONTINUE                                                 
041200*--------------- SIDBRYTNING MITT I EN SATSARTIKEL LÄS EJ                 
041300*--------------- NÄSTA PÅ OBKR                                            
041400             ELSE                                                         
041500                 PERFORM S05-SKAPA-OKVAL-POSITION                         
041600                 PERFORM S02-LAES-WDQ1B-OKVAL                             
041700                 MOVE STATUS-WS TO WDQ1-STATUS-WS                         
041800                 IF SEGMENT-FINNS                                         
041900                    PERFORM S03-LAES-WDQ101-UNIK                          
042000                 END-IF                                                   
042100             END-IF                                                       
042200                                                                          
042300             IF WDQ1-SEGMENT-FINNS           AND                          
042400                IX-RAD         >  MAX-RAD                                 
042500                IF OBKR-KDORDBEK = 61                                     
042600                  PERFORM CE-BACKA-BILDEN-KOD61                           
042700                ELSE                                                      
042800                  PERFORM CD-SPARA-NYCKLAR                                
042900                END-IF                                                    
043000             END-IF                                                       
043100         END-PERFORM                                                      
043200     END-IF                                                               
043300     .                                                                    
043400     EJECT                                                                
043500                                                                          
043600 CA-SKAPA-BLADDRINGS-NYCKEL   SECTION.                                    
043700                                                                          
043800     MOVE LOW-VALUE            TO W-WDQ1B1KY-MIN-X                        
043900                                                                          
044000     MOVE MID-IDDC-NEXT        TO WS-IDDC                                 
044100     IF NOT CDC-SE                                                        
044200       MOVE MID-IDDISTR        TO W-Q2CSEQ-IDDISTR                        
044300       MOVE MID-IDKUNDNR       TO W-Q2CSEQ-IDKUNDNR                       
044400       MOVE MID-IDORDNR7-NEXT  TO W-Q2CSEQ-IDKUNDRF                       
044500                                                                          
044600       PERFORM IMS-GU-WLORQI01-KVAL                                       
044700       IF SEGMENT-FINNS                                                   
044800         MOVE MID-IDDC-NEXT      TO W-Q1B-IDDC-MIN                        
044900       ELSE                                                               
045000         MOVE JA               TO ORDER-SAKNAS-SW                         
045100         MOVE 'B15'            TO MOD-IDMFSFEL                            
045200       END-IF                                                             
045300     ELSE                                                                 
045400       MOVE MID-IDDC-NEXT      TO  W-Q1B-IDDC-MIN                         
045500     END-IF                                                               
045600                                                                          
045700     MOVE MID-IDDISTR          TO  W-Q1B-IDDISTR-MIN                      
045800     MOVE MID-IDKUNDNR         TO  W-Q1B-IDKUNDNR-MIN                     
045900                                                                          
046000     MOVE MID-TIORDREG-NEXT TO TMP1-YYMMDD                                
046100     MOVE ZERO              TO TMP2-YYMMDD                                
046200     PERFORM WY2000P1                                                     
046300     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
046400       MOVE MID-TIORDREG-NEXT  TO  WS-AAMMDD                              
046500       IF WS-AA < 50                                                      
046600         MOVE 20               TO  WS-SEKEL-TAL                           
046700       ELSE                                                               
046800         MOVE 19               TO  WS-SEKEL-TAL                           
046900       END-IF                                                             
047000     END-IF                                                               
047100                                                                          
047200     COMPUTE WS-TITIORDD-9KOMPL =  999999999 - WS-TITIORDD                
047300     MOVE WS-TITIORDD-9KOMPL   TO  W-Q1B-TITIOWDD9-MIN                    
047400                                                                          
047500     MOVE MID-KDFRAKT-NEXT     TO  W-Q1B-KDFRAKT-MIN                      
047600     MOVE MID-KDORDKL-NEXT     TO  W-Q1B-KDORDKL-MIN                      
047700     MOVE MID-IDORDNR7-NEXT    TO  W-Q1B-IDKUNDRF-MIN                     
047800     MOVE MID-IDARTNR-NEXT     TO  W-Q1B-IDARTNR-MIN                      
047900     MOVE MID-IDLOPNR-NEXT     TO  W-Q1B-IDLOPNR-MIN                      
048000     MOVE MID-IDSEKVNR-NEXT    TO  W-Q1B-IDSEKVNR-MIN                     
048100     MOVE MID-IDORDER-NEXT     TO  W-Q1B-IDORDER-MIN                      
048200     MOVE MID-KDORDBEK-NEXT    TO  W-Q1B-KDORDBEK-MIN                     
048300     .                                                                    
048400     EJECT                                                                
048500                                                                          
048600 CB-KOLLA-OHUV                SECTION.                                    
048700                                                                          
048800     MOVE MID-IDDISTR          TO W-Q2CSEQ-IDDISTR                        
048900     MOVE MID-IDKUNDNR         TO W-Q2CSEQ-IDKUNDNR                       
049000     MOVE MID-IDORDNR7         TO W-Q2CSEQ-IDKUNDRF                       
049100                                                                          
049200     PERFORM IMS-GU-WLORQI01-KVAL                                         
049300     IF SEGMENT-FINNS                                                     
049500        PERFORM IMS-GNP-WLORQI12                                          
049600        IF SEGMENT-FINNS                                                  
049700           IF ARB-KDORDSTA = 'E'                                          
049800              MOVE JA                          TO ORDER-EJ-KLAR-SW        
049900              MOVE 'B30'                       TO MOD-IDMFSFEL            
049910           END-IF                                                         
049950        END-IF                                                            
049960     END-IF                                                               
050000     IF NOT SEGMENT-FINNS                                                 
050100         MOVE JA               TO ORDER-SAKNAS-SW                         
050200         MOVE 'B15'            TO MOD-IDMFSFEL                            
050300     END-IF                                                               
050400     .                                                                    
050500     EJECT                                                                
050600                                                                          
050700 CC-REDIGERA-ORDERBEK-RAD    SECTION.                                     
050800                                                                          
050900     IF OBKR-KDORDBEK           =  40 OR 41 OR 61                         
051000         IF OBKR-IDSEKVNR       = 1 OR                                    
051100            (OBKR-KDORDBEK      NOT = W-SPAR-KDORDBEK AND                 
051200             IX-RAD             > 1)                                      
051300             MOVE OBKR-KDORDBEK TO W-SPAR-KDORDBEK                        
051400                                   MOD-KDORDBEK     (IX-RAD)              
051500             MOVE OBKR-IDARTNR  TO MOD-IDARTNR-RAD  (IX-RAD)              
051600         ELSE                                                             
051700             MOVE OBKR-KDORDBEK     TO W-SPAR-KDORDBEK                    
051800             MOVE ZERO              TO MOD-KDORDBEK (IX-RAD)              
051900             IF OBKR-IDARTNR-TILLK  =  ZERO                               
052000                 MOVE OBKR-BEERS    TO MOD-BEERS (IX-RAD)                 
052100                 MOVE ZERO          TO MOD-IDARTNR-RAD(IX-RAD)            
052200             ELSE                                                         
052300                MOVE OBKR-IDARTNR-TILLK TO MOD-IDARTNR-RAD(IX-RAD)        
052400                MOVE OBKR-BEVOLREF  TO MOD-BEERS(IX-RAD)                  
052500             END-IF                                                       
052600         END-IF                                                           
052700         IF OBKR-KDORDBEK        =  61 AND OBKR-IDSEKVNR = +1             
052800           AND IX-RAD > +1                                                
052900           MOVE IX-RAD            TO IX-SPAR                              
053000           MOVE OBKR-IDORDER      TO SPAR-IDORDER-NEXT                    
053100           MOVE OBKR-IDORDNR7     TO SPAR-IDORDNR7-NEXT                   
053200           MOVE OBKR-TIORDREG     TO SPAR-TIORDREG-NEXT                   
053300           MOVE OBKR-IDARTNR      TO SPAR-IDARTNR-NEXT                    
053400           MOVE OBKR-IDLOPNR      TO SPAR-IDLOPNR-NEXT                    
053500           MOVE OBKR-IDSEKVNR     TO SPAR-IDSEKVNR-NEXT                   
053600           MOVE OBKR-IDDC         TO SPAR-IDDC-NEXT                       
053700           MOVE OBKR-KDORDBEK     TO SPAR-KDORDBEK-NEXT                   
053800           MOVE OBKR-KDFRAKT      TO SPAR-KDFRAKT-NEXT                    
053900           MOVE OBKR-KDORDKL      TO SPAR-KDORDKL-NEXT                    
054000         END-IF                                                           
054100     ELSE                                                                 
054200         MOVE OBKR-BEVOLREF       TO  MOD-BEERS (IX-RAD)                  
054300         IF OBKR-IDARTNR-TILLK    >   ZERO                                
054400             MOVE OBKR-IDARTNR-TILLK  TO  MOD-IDARTNR-RAD(IX-RAD)         
054500         ELSE                                                             
054600             MOVE OBKR-IDARTNR        TO  MOD-IDARTNR-RAD(IX-RAD)         
054700         END-IF                                                           
054800         MOVE OBKR-KDORDBEK           TO  MOD-KDORDBEK (IX-RAD)           
054900     END-IF                                                               
055000                                                                          
055100     MOVE OBKR-IDORDNR7        TO  MOD-IDORDNR7-RAD(IX-RAD)               
055200                                                                          
055300*    LITE FIX FÖR ATT FÅ RÄTT ORDERREFERENS VID                           
055400*    TVINGANDE TILLÄGG FRÅN VIPS                                          
055500     MOVE MID-IDDISTR          TO W-Q2CSEQ-IDDISTR                        
055600     MOVE MID-IDKUNDNR         TO W-Q2CSEQ-IDKUNDNR                       
055700     MOVE MID-IDORDNR7         TO W-Q2CSEQ-IDKUNDRF                       
055800                                                                          
055900     PERFORM IMS-GU-WLORQI01-RO                                           
056000     IF RO-OHUV-BEKUNDRF(1:2) = 'OC'                                      
056100                                                                          
056200        MOVE RO-OHUV-BEKUNDRF(4:5)    TO W-IDKUNDRF-RO-3--7               
056300        MOVE W-IDKUNDRF-RO            TO MOD-IDORDNR7-RO(IX-RAD)          
056400     ELSE                                                                 
056500        IF OBKR-IDKUNDRF-RO     NOT =   '0000000   '                      
056600           MOVE OBKR-IDKUNDRF-RO(1:7) TO MOD-IDORDNR7-RO(IX-RAD)          
056700        ELSE                                                              
056800           MOVE OBKR-IDKUNDRF(1:7)    TO MOD-IDORDNR7-RO(IX-RAD)          
056900        END-IF                                                            
057000     END-IF                                                               
057100                                                                          
057200     MOVE OBKR-IDDC            TO MOD-IDDC(IX-RAD)                        
057300     MOVE OBKR-BERADREF        TO MOD-BERADREF(IX-RAD)                    
057400     MOVE OBKR-IDBIL           TO MOD-IDBIL(IX-RAD)                       
057500     PERFORM CCA-BESTAEM-ANTAL                                            
057600     PERFORM CCB-BESTAEM-TIDISPIN                                         
057700     MOVE W-TIDISPIN           TO  MOD-TIDISPIN(IX-RAD)                   
057800     IF OBKR-KDORDBEK          =   +57                                    
057900         PERFORM CCC-BEHANDLA-SATSARTIKLAR                                
058000     END-IF                                                               
058100     .                                                                    
058200     EJECT                                                                
058300                                                                          
058400 CCA-BESTAEM-ANTAL            SECTION.                                    
058500                                                                          
058600     EVALUATE TRUE                                                        
058700                                                                          
058800     WHEN OBKR-KDORDBEK        =  +10 OR +15 OR +16 OR +56 OR             
058900                                  +70 OR +95 OR +96                       
059000          MOVE OBKR-KVBEART-Q  TO  MOD-KVAVBART(IX-RAD)                   
059100          MOVE OBKR-KVBEART-Q  TO  MOD-KVBEART(IX-RAD)                    
059200                                                                          
059300     WHEN OBKR-KDORDBEK        =  +41 OR +61 OR +40                       
059400          IF OBKR-FLTILLK      =  JA                                      
059500            IF OBKR-KDORDBEK   = +61                                      
059600              MOVE ZERO                TO MOD-KVAVBART(IX-RAD)            
059700            ELSE                                                          
059800              MOVE OBKR-KVBEART-TILLK  TO MOD-KVAVBART(IX-RAD)            
059900            END-IF                                                        
060000            MOVE OBKR-KVBEART-TILLK    TO MOD-KVBEART(IX-RAD)             
060100          ELSE                                                            
060200            MOVE OBKR-KVBEART-Q        TO MOD-KVBEART(IX-RAD)             
060300            MOVE ZERO                  TO MOD-KVAVBART(IX-RAD)            
060400          END-IF                                                          
060500                                                                          
060600     WHEN OBKR-KDORDBEK        =  +43 OR +44                              
060700          MOVE OBKR-KVBEART    TO MOD-KVBEART(IX-RAD)                     
060800          MOVE OBKR-KVBEART-Q  TO MOD-KVAVBART(IX-RAD)                    
060900                                                                          
061000                                                                          
061100     WHEN OBKR-KDORDBEK        =  +30 OR +31 OR +32 OR +33 OR             
061200                                  +34                                     
061300          MOVE OBKR-KVANNANT   TO MOD-KVBEART(IX-RAD)                     
061400          MOVE ZERO            TO MOD-KVAVBART(IX-RAD)                    
061500                                                                          
061600     WHEN OBKR-KDORDBEK        =  +20 OR +21 OR +22 OR                    
061700                                  +52 OR +53 OR +54 OR +55 OR             
061800                                  +57 OR +58 OR +59 OR +66 OR             
061900                                  +67 OR +80 OR +82 OR +98 OR             
062000                                  +84                                     
062100          MOVE OBKR-KVBEART    TO MOD-KVBEART(IX-RAD)                     
062200          MOVE ZERO            TO MOD-KVAVBART(IX-RAD)                    
062300                                                                          
062400     WHEN OBKR-KDORDBEK        =  +71 OR +72 OR +73 OR +74 OR             
062500                                  +75 OR +76 OR +77                       
062600          MOVE OBKR-KVBEART-Q  TO MOD-KVBEART(IX-RAD)                     
062700          MOVE ZERO            TO MOD-KVAVBART(IX-RAD)                    
062800                                                                          
062900     WHEN OBKR-KDORDBEK        =  +80 OR +81 OR +93                       
063000          MOVE OBKR-KVBEART-Q  TO MOD-KVBEART(IX-RAD)                     
063100          COMPUTE MOD-KVAVBART(IX-RAD) = OBKR-KVBEART-Q -                 
063200                                         OBKR-KVANNANT                    
063300                                                                          
063400     WHEN OBKR-KDORDBEK        =  +83 OR +85 OR +87                       
063500          MOVE OBKR-KVANNANT   TO MOD-KVBEART(IX-RAD)                     
063600          MOVE ZERO            TO MOD-KVAVBART(IX-RAD)                    
063700                                                                          
063800     WHEN OBKR-KDORDBEK        =  +90 OR +91                              
063900          MOVE OBKR-KVBEART-Q  TO MOD-KVBEART(IX-RAD)                     
064000          COMPUTE MOD-KVAVBART(IX-RAD) =  OBKR-KVBEART-Q -                
064100                                          OBKR-KVRO                       
064200                                                                          
064300     WHEN OBKR-KDORDBEK        =  +92                                     
064400          MOVE OBKR-KVBEART-Q  TO MOD-KVBEART(IX-RAD)                     
064500          MOVE OBKR-KVPREAVB   TO MOD-KVAVBART(IX-RAD)                    
064600                                                                          
064700     WHEN OBKR-KDORDBEK        =  +99                                     
064800          MOVE OBKR-KVBEART-Q  TO MOD-KVBEART(IX-RAD)                     
064900          COMPUTE MOD-KVAVBART(IX-RAD) =  OBKR-KVBEART-Q -                
065000                                          OBKR-KVPRERO                    
065100                                                                          
065200     END-EVALUATE                                                         
065300     .                                                                    
065400     EJECT                                                                
065500 CCB-BESTAEM-TIDISPIN         SECTION.                                    
065600                                                                          
065700     MOVE OBKR-IDDC             TO WS-IDDC                                
065800     IF NDC                                                               
065900        EVALUATE TRUE                                                     
066000        WHEN OBKR-KDORDBEK        =  +70 OR +90 OR +91 OR +99             
066100             MOVE '612'         TO ETA-KDCALL                             
066200             MOVE OBKR-IDDC     TO ETA-IDDC-REC                           
066300             MOVE OBKR-IDARTNR  TO ETA-IDARTNR                            
066400             MOVE SPACE         TO ETA-IDLEVNR                            
066500             MOVE ZERO          TO ETA-KDFRAKT                            
066600             MOVE OBKR-TIREGDAT TO ETA-TIAAMMDD-ANROP                     
066700                                   WS-ETA-DATUM                           
066800             IF WS-ETA-DATUM-AAR > 50                                     
066900                MOVE 19         TO ETA-TISEKEL-ANROP                      
067000             ELSE                                                         
067100                MOVE 20         TO ETA-TISEKEL-ANROP                      
067200             END-IF                                                       
067300                                                                          
067400             CALL W218ETA  USING ETA-W218LETA                             
067500                           ETA-ARTC-PCB ETA-WDK7-PCB                      
067600                           ETA-INLC-PCB ETA-LEVA-PCB                      
067700                           ETA-WDB6-PCB ETA-WDD9-PCB                      
067800                                                                          
067900             IF ETA-SVAR-OK = JA                                          
068000               IF NDC-NA OR NDC-CN                                        
068100                 MOVE ETA-TIAAMMDD-SVAR                                   
068200                                  TO W-TIDISPIN                           
068300               ELSE                                                       
068400                 IF ETA-KVAVIS-ETA > +0                                   
068500                   MOVE ETA-TIAAMMDD-SVAR                                 
068600                                  TO W-TIDISPIN                           
068700                 ELSE                                                     
068800                   MOVE ZERO      TO W-TIDISPIN                           
068900                 END-IF                                                   
069000               END-IF                                                     
069100             ELSE                                                         
069200                MOVE ZERO         TO W-TIDISPIN                           
069300             END-IF                                                       
069400                                                                          
069500        WHEN OBKR-KDORDBEK        =  +96                                  
069600             MOVE OBKR-TIDISPIN   TO W-TIDISPIN                           
069700                                                                          
069800        WHEN OTHER                                                        
069900             MOVE ZERO            TO W-TIDISPIN                           
070000                                                                          
070100        END-EVALUATE                                                      
070200                                                                          
070300     ELSE                                                                 
070400        EVALUATE TRUE                                                     
070500        WHEN OBKR-KDORDBEK        =  +70 OR +71 OR +77                    
070600             MOVE OBKR-TITPO      TO W-TIDISPIN                           
070700                                                                          
070800        WHEN OBKR-KDORDBEK        =  +96                                  
070900             MOVE OBKR-TIDISPIN   TO W-TIDISPIN                           
071000                                                                          
071100        WHEN OBKR-KDORDBEK        =  +90 OR +91                           
071200                                         OR +99                           
071300             MOVE OBKR-TIDISPIN   TO W-TIDISPIN                           
071400                                                                          
071500             IF MID-IDARTNR            >   ZERO                           
071600               MOVE MID-IDARTNR   TO  W-IDARTNR-K6                        
071700                                                                          
071800               PERFORM IMS-GU-WDK611                                      
071900               IF SEGMENT-FINNS                                           
072000                 IF CLAG-TIDISPIN > ZERO                                  
072100                   MOVE CLAG-TIDISPIN                                     
072200                                  TO W-TIDISPIN                           
072300                 END-IF                                                   
072400               END-IF                                                     
072500             END-IF                                                       
072600                                                                          
072700        WHEN OTHER                                                        
072800             MOVE ZERO            TO W-TIDISPIN                           
072900                                                                          
073000        END-EVALUATE                                                      
073100     END-IF                                                               
073200     .                                                                    
073300     EJECT                                                                
073400                                                                          
073500 CCC-BEHANDLA-SATSARTIKLAR    SECTION.                                    
073600                                                                          
073700     MOVE OBKR-IDARTNR         TO W-IDARTNR                               
073800                                                                          
073900     PERFORM IMS-GU-SATB01                                                
074000     MOVE +1                   TO IX-TAB                                  
074100     ADD  +1                   TO IX-RAD                                  
074200     PERFORM UNTIL SEGMENT-SAKNAS  OR                                     
074300                   IX-TAB      > 5                                        
074400       MOVE DAGENS-DATUM      TO TMP1-YYMMDD                              
074500       MOVE SATB-RAD-TISTADAT TO TMP2-YYMMDD                              
074600       MOVE SATB-RAD-TISTODAT TO TMP3-YYMMDD                              
074700       PERFORM WY2000Q1                                                   
074800       IF SATB-STR-IDARTNR     < 100000000         AND                    
074900          SATB-STR-TIBORT      = 0                 AND                    
075000          TMP2-YYMMDD     NOT > TMP1-YYMMDD   AND                         
075100          TMP3-YYMMDD     NOT < TMP1-YYMMDD                               
075200            MOVE SATB-STR-IDARTNR TO W-SATB-IDARTNR(IX-TAB)               
075300                                     FLT                                  
075400            MOVE 9             TO LGD                                     
075500            CALL W009KSIF      USING FLT LGD KSIFF                        
075600            MOVE KSIFF         TO W-SATB-REKSIFFR(IX-TAB)                 
075700            ADD +1             TO IX-TAB                                  
075800       END-IF                                                             
075900       PERFORM IMS-GN-SATB01                                              
076000     END-PERFORM                                                          
076100                                                                          
076200     SUBTRACT 1                FROM IX-TAB                                
076300     COMPUTE TAB-INDX          = MAX-RAD - IX-RAD                         
076400     IF TAB-INDX               < IX-TAB                                   
076500         SUBTRACT 1            FROM IX-RAD                                
076600         PERFORM S04-BLANKA-RAD                                           
076700         MOVE +15              TO IX-RAD                                  
076800     ELSE                                                                 
076900         MOVE +1               TO IX-TAB                                  
077000         PERFORM UNTIL IX-TAB  > 5  OR                                    
077100                       W-SATB-IDARTNR(IX-TAB) = ZERO                      
077200             MOVE W-SATB-IDARTNR(IX-TAB)                                  
077300                                      TO MOD-IDARTNR-RAD(IX-RAD)          
077400             MOVE OBKR-IDORDNR7       TO MOD-IDORDNR7-RAD (IX-RAD)        
077500             MOVE SPACE               TO MOD-BEERS        (IX-RAD)        
077600                                         MOD-IDBIL        (IX-RAD)        
077700                                         MOD-BERADREF     (IX-RAD)        
077800             MOVE ZERO                TO MOD-KDORDBEK     (IX-RAD)        
077900                                         MOD-KVBEART      (IX-RAD)        
078000                                         MOD-KVAVBART     (IX-RAD)        
078100                                         MOD-TIDISPIN     (IX-RAD)        
078200             IF OBKR-IDKUNDRF-RO       NOT =   '0000000   '               
078300                 MOVE OBKR-IDKUNDRF-RO(1:7)                               
078400                                       TO MOD-IDORDNR7-RO(IX-RAD)         
078500             ELSE                                                         
078600                 MOVE OBKR-IDKUNDRF(1:7)                                  
078700                                       TO MOD-IDORDNR7-RO(IX-RAD)         
078800             END-IF                                                       
078900             ADD +1                   TO IX-TAB                           
079000                                         IX-RAD                           
079100       END-PERFORM                                                        
079200       SUBTRACT 1 FROM IX-RAD                                             
079300     END-IF                                                               
079400     .                                                                    
079500     EJECT                                                                
079600                                                                          
079700 CD-SPARA-NYCKLAR SECTION.                                                
079800                                                                          
079900     MOVE OBKR-IDORDNR7        TO  MOD-IDORDNR7-NEXT                      
080000     MOVE OBKR-IDARTNR         TO  MOD-IDARTNR-NEXT                       
080100     MOVE OBKR-IDLOPNR         TO  MOD-IDLOPNR-NEXT                       
080200     MOVE OBKR-IDSEKVNR        TO  MOD-IDSEKVNR-NEXT                      
080300     MOVE OBKR-IDORDER         TO  MOD-IDORDER-NEXT                       
080400     MOVE OBKR-TIORDREG        TO  MOD-TIORDREG-NEXT                      
080500     MOVE OBKR-KDFRAKT         TO  MOD-KDFRAKT-NEXT                       
080600     MOVE OBKR-KDORDKL         TO  MOD-KDORDKL-NEXT                       
080700     MOVE OBKR-IDDC            TO  MOD-IDDC-NEXT                          
080800     MOVE OBKR-KDORDBEK        TO  MOD-KDORDBEK-NEXT                      
080900     .                                                                    
081000     EJECT                                                                
081100 CE-BACKA-BILDEN-KOD61 SECTION.                                           
081200     IF IX-SPAR > +1                                                      
081300       MOVE IX-SPAR TO IX-RAD                                             
081400       PERFORM UNTIL IX-RAD > MAX-RAD                                     
081500          PERFORM S04-BLANKA-RAD                                          
081600          ADD +1 TO IX-RAD                                                
081700       END-PERFORM                                                        
081800       MOVE SPAR-IDORDER-NEXT         TO MOD-IDORDER-NEXT                 
081900       MOVE SPAR-IDORDNR7-NEXT        TO MOD-IDORDNR7-NEXT                
082000       MOVE SPAR-IDARTNR-NEXT         TO MOD-IDARTNR-NEXT                 
082100       MOVE SPAR-IDLOPNR-NEXT         TO MOD-IDLOPNR-NEXT                 
082200       MOVE SPAR-IDSEKVNR-NEXT        TO MOD-IDSEKVNR-NEXT                
082300       MOVE SPAR-IDDC-NEXT            TO MOD-IDDC-NEXT                    
082400       MOVE SPAR-KDORDBEK-NEXT        TO MOD-KDORDBEK-NEXT                
082500       MOVE SPAR-KDORDKL-NEXT         TO MOD-KDORDKL-NEXT                 
082600       MOVE SPAR-KDFRAKT-NEXT         TO MOD-KDFRAKT-NEXT                 
082700       MOVE SPAR-TIORDREG-NEXT        TO MOD-TIORDREG-NEXT                
082800     END-IF                                                               
082900     .                                                                    
083000     EJECT                                                                
083100                                                                          
083200 E-KONTROLLERA-OM-TOM-SIDA  SECTION.                                      
083300                                                                          
083400     IF IX-RAD                 = 1                                        
083500         MOVE 'B10'            TO MOD-IDMFSFEL                            
083600     END-IF                                                               
083700     .                                                                    
083800     EJECT                                                                
083900                                                                          
084000 F-BERAKNA-MAX-MOD-LANGD    SECTION.                                      
084100                                                                          
084200                                                                          
084300     COMPUTE MAX-MOD-LANGD = LENGTH OF MOD-W9O34301 + 4                   
084400     MOVE 13                   TO  IX-RAD                                 
084500     PERFORM UNTIL IX-RAD      = 0                                        
084600         IF MOD-IDORDNR7-RAD(IX-RAD) = ZERO                               
084700             SUBTRACT +88      FROM MAX-MOD-LANGD                         
084800             SUBTRACT +1       FROM IX-RAD                                
084900         ELSE                                                             
085000             MOVE ZERO         TO IX-RAD                                  
085100         END-IF                                                           
085200     END-PERFORM                                                          
085300     .                                                                    
085400     EJECT                                                                
085500                                                                          
085600 S01-SKAPA-OKVAL-NYCKEL       SECTION.                                    
085700                                                                          
085800     MOVE LOW-VALUE            TO  W-WDQ1B1KY-MIN-X                       
085900     MOVE HIGH-VALUE           TO  W-WDQ1B1KY-MAX-X                       
086000                                                                          
086100     MOVE MID-IDDISTR          TO  W-Q1B-IDDISTR-MIN                      
086200                                   W-Q1B-IDDISTR-MAX                      
086300     MOVE MID-IDKUNDNR         TO  W-Q1B-IDKUNDNR-MIN                     
086400                                   W-Q1B-IDKUNDNR-MAX                     
086500                                                                          
086600     MOVE MID-TIORDREG    TO TMP1-YYMMDD                                  
086700     MOVE ZERO            TO TMP2-YYMMDD                                  
086800     PERFORM WY2000P1                                                     
086900     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
087000         MOVE MID-TIORDREG       TO WS-AAMMDD                             
087100         IF WS-AA < 50                                                    
087200           MOVE 20               TO WS-SEKEL-TAL                          
087300         ELSE                                                             
087400           MOVE 19               TO WS-SEKEL-TAL                          
087500         END-IF                                                           
087600         COMPUTE WS-TITIORDD-9KOMPL = 999999999 - WS-TITIORDD             
087700         MOVE WS-TITIORDD-9KOMPL TO W-Q1B-TITIOWDD9-MIN                   
087800                                    W-Q1B-TITIOWDD9-MAX                   
087900     END-IF                                                               
088000                                                                          
088100     IF MID-KDORDKL            > SPACE                                    
088200         MOVE MID-KDORDKL     TO  W-KDORDKL                               
088300     END-IF                                                               
088400                                                                          
088500     IF MID-IDORDNR7           >   ZERO                                   
088600         MOVE MID-IDORDNR7     TO  W-IDKUNDRF                             
088700     END-IF                                                               
088800                                                                          
088900     IF MID-IDARTNR            >   ZERO                                   
089000         MOVE MID-IDARTNR      TO  W-IDARTNR-Q1                           
089100     END-IF                                                               
089200     .                                                                    
089300     EJECT                                                                
089400                                                                          
089500 S02-LAES-WDQ1B-OKVAL SECTION.                                            
089600                                                                          
089700     IF MID-KDORDKL NOT = SPACE                                           
089800        IF MID-IDORDNR7 > ZERO                                            
089900           IF MID-IDARTNR > ZERO                                          
090000              PERFORM IMS-GN-Q1B-KL-ORDNR-ARTNR                           
090100           ELSE                                                           
090200              PERFORM IMS-GN-Q1B-KL-ORDNR                                 
090300           END-IF                                                         
090400        ELSE                                                              
090500           IF MID-IDARTNR > ZERO                                          
090600              PERFORM IMS-GN-Q1B-KL-ARTNR                                 
090700           ELSE                                                           
090800              PERFORM IMS-GN-Q1B-KL                                       
090900           END-IF                                                         
091000        END-IF                                                            
091100     ELSE                                                                 
091200        IF MID-IDORDNR7 > ZERO                                            
091300           IF MID-IDARTNR > ZERO                                          
091400              PERFORM IMS-GN-Q1B-ORDNR-ARTNR                              
091500           ELSE                                                           
091600              PERFORM IMS-GN-Q1B-ORDNR                                    
091700           END-IF                                                         
091800        ELSE                                                              
091900           IF MID-IDARTNR > ZERO                                          
092000              PERFORM IMS-GN-Q1B-ARTNR                                    
092100           ELSE                                                           
092200              PERFORM IMS-GN-WLORQO01-OKVAL                               
092300           END-IF                                                         
092400        END-IF                                                            
092500     END-IF                                                               
092600     .                                                                    
092700     EJECT                                                                
092800                                                                          
092900 S03-LAES-WDQ101-UNIK SECTION.                                            
093000                                                                          
093100     MOVE  SEQB-IDORDER        TO W-Q1-IDORDER                            
093200     MOVE  SEQB-IDARTNR        TO W-Q1-IDARTNR                            
093300     MOVE  SEQB-IDLOPNR        TO W-Q1-IDLOPNR                            
093400     MOVE  SEQB-IDSEKVNR       TO W-Q1-IDSEKVNR                           
093500     MOVE  SEQB-IDDC           TO W-Q1-IDDC                               
093600     MOVE  SEQB-KDORDBEK       TO W-Q1-KDORDBEK                           
093700                                                                          
093800     PERFORM IMS-GU-WLORQM01                                              
093900     .                                                                    
094000     EJECT                                                                
094100                                                                          
094200 S04-BLANKA-RAD   SECTION.                                                
094300                                                                          
094400     MOVE ZERO                 TO  MOD-IDORDNR7-RAD    (IX-RAD)           
094500                                   MOD-IDARTNR-RAD     (IX-RAD)           
094600                                   MOD-KVBEART         (IX-RAD)           
094700                                   MOD-KVAVBART        (IX-RAD)           
094800                                   MOD-KDORDBEK        (IX-RAD)           
094900                                   MOD-TIDISPIN        (IX-RAD)           
095000                                   MOD-IDORDNR7-RO     (IX-RAD)           
095100     MOVE SPACE                TO  MOD-BEERS           (IX-RAD)           
095200                                   MOD-IDDC            (IX-RAD)           
095300                                   MOD-IDBIL           (IX-RAD)           
095400                                   MOD-BERADREF        (IX-RAD)           
095500     .                                                                    
095600     EJECT                                                                
095700                                                                          
095800 S05-SKAPA-OKVAL-POSITION     SECTION.                                    
095900                                                                          
096000     MOVE LOW-VALUE            TO  W-WDQ1B1KY-MIN-X                       
096100     MOVE HIGH-VALUE           TO  W-WDQ1B1KY-MAX-X                       
096200                                                                          
096300     MOVE MID-IDDISTR          TO  W-Q1B-IDDISTR-MIN                      
096400                                   W-Q1B-IDDISTR-MAX                      
096500                                                                          
096600     MOVE MID-IDKUNDNR         TO  W-Q1B-IDKUNDNR-MIN                     
096700                                   W-Q1B-IDKUNDNR-MAX                     
096800                                                                          
096900     MOVE OBKR-TITIORDD-9KOMPL TO  W-Q1B-TITIOWDD9-MIN                    
097000                                   W-Q1B-TITIOWDD9-MAX                    
097100                                                                          
097200     IF MID-KDORDKL            > SPACE                                    
097300         MOVE MID-KDORDKL     TO  W-KDORDKL                               
097400     END-IF                                                               
097500                                                                          
097600     IF MID-IDORDNR7           >   ZERO                                   
097700         MOVE MID-IDORDNR7     TO  W-IDKUNDRF                             
097800     END-IF                                                               
097900                                                                          
098000     IF MID-IDARTNR            >   ZERO                                   
098100         MOVE MID-IDARTNR      TO  W-IDARTNR-Q1                           
098200     END-IF                                                               
098300     .                                                                    
098400     EJECT                                                                
098500                                                                          
098600* IMS SEKTIONER                                                           
098700     SKIP2                                                                
098800 IMS-GET-MSG SECTION.                                                     
098900     MOVE '  QC' TO GODK-STATUSKODER                                      
099000     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
099100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
099200     PERFORM IMS-STATUSKONTROLL                                           
099300     .                                                                    
099400     SKIP2                                                                
099500 IMS-INSERT-MSG SECTION.                                                  
099600     MOVE LOW-VALUE TO MSG-KDZ1 MSG-KDZ2                                  
099700     MOVE SPACE TO GODK-STATUSKODER                                       
099800     CALL CBLTDLI USING ISRT MSG-PCB MSG-IO-AREA WS-MODNAMN               
099900     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
100000     PERFORM IMS-STATUSKONTROLL                                           
100100     .                                                                    
100200     EJECT                                                                
100300 IMS-GU-WLORQM01              SECTION.                                    
100400     STRING 'WLORQM01(WDQ101KY =' W-WDQ101KY-X ')'                        
100500            DELIMITED BY SIZE INTO SSA1                                   
100600     MOVE '  '    TO GODK-STATUSKODER                                     
100700     CALL CBLTDLI USING GU ORQM-PCB DLI-IO-AREA SSA1                      
100800     MOVE ORQM-STATUS-CODE TO STATUS-WS                                   
100900     PERFORM IMS-STATUSKONTROLL                                           
101000     .                                                                    
101100     SKIP2                                                                
101200 IMS-GU-WLORQO01-KVAL         SECTION.                                    
101300     STRING 'WLORQO01(WDQ1B1KY =' W-WDQ1B1KY-MIN-X ')'                    
101400            DELIMITED BY SIZE INTO SSA1                                   
101500     MOVE '  GE'  TO GODK-STATUSKODER                                     
101600     CALL CBLTDLI USING GU ORQO-PCB DLI-IO-AREA SSA1                      
101700     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
101800     PERFORM IMS-STATUSKONTROLL                                           
101900     .                                                                    
102000     SKIP2                                                                
102100 IMS-GN-WLORQO01-OKVAL         SECTION.                                   
102200     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
102300                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X ')'                    
102400            DELIMITED BY SIZE INTO SSA1                                   
102500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
102600     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
102700     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
102800     PERFORM IMS-STATUSKONTROLL                                           
102900     .                                                                    
103000     EJECT                                                                
103100 IMS-GN-Q1B-KL-ORDNR-ARTNR SECTION.                                       
103200     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
103300                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
103400                    '&KDORDKL  =' W-KDORDKL-X                             
103500                    '&IDKUNDRF =' W-IDKUNDRF-X                            
103600                    '&IDARTNR  =' W-IDARTNR-Q1-X ')'                      
103700            DELIMITED BY SIZE INTO SSA1                                   
103800     MOVE '  GEGB' TO GODK-STATUSKODER                                    
103900     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
104000     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
104100     PERFORM IMS-STATUSKONTROLL                                           
104200     .                                                                    
104300     SKIP2                                                                
104400 IMS-GN-Q1B-KL-ORDNR SECTION.                                             
104500     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
104600                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
104700                    '&KDORDKL  =' W-KDORDKL-X                             
104800                    '&IDKUNDRF =' W-IDKUNDRF-X ')'                        
104900            DELIMITED BY SIZE INTO SSA1                                   
105000     MOVE '  GEGB' TO GODK-STATUSKODER                                    
105100     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
105200     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
105300     PERFORM IMS-STATUSKONTROLL                                           
105400     .                                                                    
105500     SKIP2                                                                
105600 IMS-GN-Q1B-KL-ARTNR SECTION.                                             
105700     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
105800                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
105900                    '&KDORDKL  =' W-KDORDKL-X                             
106000                    '&IDARTNR  =' W-IDARTNR-Q1-X ')'                      
106100            DELIMITED BY SIZE INTO SSA1                                   
106200     MOVE '  GEGB' TO GODK-STATUSKODER                                    
106300     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
106400     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
106500     PERFORM IMS-STATUSKONTROLL                                           
106600     .                                                                    
106700     SKIP2                                                                
106800 IMS-GN-Q1B-KL SECTION.                                                   
106900     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
107000                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
107100                    '&KDORDKL  =' W-KDORDKL-X ')'                         
107200            DELIMITED BY SIZE INTO SSA1                                   
107300     MOVE '  GEGB' TO GODK-STATUSKODER                                    
107400     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
107500     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
107600     PERFORM IMS-STATUSKONTROLL                                           
107700     .                                                                    
107800     SKIP2                                                                
107900 IMS-GN-Q1B-ORDNR-ARTNR SECTION.                                          
108000     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
108100                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
108200                    '&IDKUNDRF =' W-IDKUNDRF-X                            
108300                    '&IDARTNR  =' W-IDARTNR-Q1-X ')'                      
108400            DELIMITED BY SIZE INTO SSA1                                   
108500     MOVE '  GEGB' TO GODK-STATUSKODER                                    
108600     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
108700     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
108800     PERFORM IMS-STATUSKONTROLL                                           
108900     .                                                                    
109000     SKIP2                                                                
109100 IMS-GN-Q1B-ORDNR SECTION.                                                
109200     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
109300                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
109400                    '&IDKUNDRF =' W-IDKUNDRF-X ')'                        
109500            DELIMITED BY SIZE INTO SSA1                                   
109600     MOVE '  GEGB' TO GODK-STATUSKODER                                    
109700     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
109800     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
109900     PERFORM IMS-STATUSKONTROLL                                           
110000     .                                                                    
110100     SKIP2                                                                
110200 IMS-GN-Q1B-ARTNR SECTION.                                                
110300     STRING 'WLORQO01(WDQ1B1KY>=' W-WDQ1B1KY-MIN-X                        
110400                    '&WDQ1B1KY<=' W-WDQ1B1KY-MAX-X                        
110500                    '&IDARTNR  =' W-IDARTNR-Q1-X ')'                      
110600            DELIMITED BY SIZE INTO SSA1                                   
110700     MOVE '  GEGB' TO GODK-STATUSKODER                                    
110800     CALL CBLTDLI USING GN ORQO-PCB DLI-IO-AREA SSA1                      
110900     MOVE ORQO-STATUS-CODE TO STATUS-WS                                   
111000     PERFORM IMS-STATUSKONTROLL                                           
111100     .                                                                    
111200     EJECT                                                                
111300 IMS-GU-SATB01 SECTION.                                                   
111400     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
111500          DELIMITED BY SIZE INTO SSA1                                     
111600     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
111700          DELIMITED BY SIZE INTO SSA2                                     
111800     MOVE '  GE' TO GODK-STATUSKODER                                      
111900     CALL CBLTDLI USING GU SATB-PCB DLI-IO-AREA2 SSA1 SSA2                
112000     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
112100     PERFORM IMS-STATUSKONTROLL                                           
112200     .                                                                    
112300     SKIP2                                                                
112400 IMS-GN-SATB01 SECTION.                                                   
112500     STRING 'WLSATB11*D(WDJ1CSEQ =' W-WDJ1CSEQ-X ')'                      
112600          DELIMITED BY SIZE INTO SSA1                                     
112700     STRING 'WLSATB01(IDLEVNR  =' W-IDLEVNR-X ')'                         
112800          DELIMITED BY SIZE INTO SSA2                                     
112900     MOVE '  GE' TO GODK-STATUSKODER                                      
113000     CALL CBLTDLI USING GN SATB-PCB DLI-IO-AREA2 SSA1 SSA2                
113100     MOVE SATB-STATUS-CODE TO STATUS-WS                                   
113200     PERFORM IMS-STATUSKONTROLL                                           
113300     .                                                                    
113400     EJECT                                                                
113500 IMS-GU-WLORQI01-KVAL SECTION.                                            
113600     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
113700          DELIMITED BY SIZE INTO SSA1                                     
113800     MOVE '  GE' TO GODK-STATUSKODER                                      
113900     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-WDQ201 SSA1                    
114000     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
114100     PERFORM IMS-STATUSKONTROLL                                           
114200     .                                                                    
114300     EJECT                                                                
114400 IMS-GU-WLORQI01-RO   SECTION.                                            
114500     STRING 'WLORQI01(WDQ2CSEQ =' W-WDQ2CSEQ-X ')'                        
114600          DELIMITED BY SIZE INTO SSA1                                     
114700     MOVE '  GE' TO GODK-STATUSKODER                                      
114800     CALL CBLTDLI USING GU ORQI-PCB DLI-IO-WDQ201-RO SSA1                 
114900     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
115000     PERFORM IMS-STATUSKONTROLL                                           
115100     .                                                                    
115200     EJECT                                                                
115210 IMS-GNP-WLORQI12      SECTION.                                           
115270                                                                          
115271     MOVE 'WLORQI12' TO SSA1                                              
115273     MOVE '  GE' TO GODK-STATUSKODER                                      
115274     CALL CBLTDLI USING GNP ORQI-PCB DLI-IO-WDQ212 SSA1                   
115275     MOVE ORQI-STATUS-CODE TO STATUS-WS                                   
115276     PERFORM IMS-STATUSKONTROLL                                           
115277     .                                                                    
115290     EJECT                                                                
115300 IMS-GU-WDK611 SECTION.                                                   
115400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-K6-X ')'                      
115500          DELIMITED BY SIZE INTO SSA1                                     
115600     MOVE 'WDK611   '      TO SSA2                                        
115700     MOVE '  GE'           TO GODK-STATUSKODER                            
115800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
115900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
116000     PERFORM IMS-STATUSKONTROLL                                           
116100     .                                                                    
116200     EJECT                                                                
116300 IMS-STATUSKONTROLL SECTION.                                              
116400     SET STATUS-IX TO 1                                                   
116500     SEARCH GODK-STATUS                                                   
116600       AT END                                                             
116700         CALL FELLOG                                                      
116800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
116900         CONTINUE                                                         
117000     END-SEARCH                                                           
117100     .                                                                    
117200     EJECT                                                                
117300*    -COPY WY2000Q1                                                       
117400     EJECT                                                                
117500*    -COPY WY2000P1                                                       
