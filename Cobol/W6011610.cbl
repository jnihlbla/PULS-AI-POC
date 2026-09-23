000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6011610.                                                
000300 AUTHOR.         LARS THELL.                                              
000400 DATE-WRITTEN.   92/02/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SKAPAR R31 TRANSAKTIONER                                         
000900*        EN LEVERANTÖR EN AVI FLERA ARTIKLAR                              
001000*                                                                         
001100*W6011610 FOR BUSINESS LOGIC                                              
001200*                                                                         
001300*                                                                         
001400*    SUB PROGRAMMET W611REG UPPDATERAR W6INLA (W6D1)                      
001500*                           LÄSER      WLLEVA (WDF1)                      
001600*                           LÄSER      WLARTC (WDK6)                      
001700*                           LÄSER      WLBENA (WDD3)                      
001800*                           LÄSER      WLINLB (WDD9)                      
001900*    SUB PROGRAMMET W411SAP LÄSER      WLSAPC (WDH3)                      
002000*    INDATA.                                                              
002100*        TRANSAKTION: W6T116                                              
002200*        MID:         W6I11601                                            
002300*                                                                         
002400*    UTDATA.                                                              
002500*        MOD:         W6O11601                                            
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100 WORKING-STORAGE SECTION.                                                 
003200*    -- CHECKED BY WY2000                                                 
003300     SKIP3                                                                
003400 77  IDPGM                       PIC X(08)   VALUE 'W6011610'.            
003500                                                                          
003600*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003700 77  FILLER                      PIC X(8)  VALUE 'ERRORTEX'.              
003800 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003900                                                                          
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  YES                         PIC X       VALUE 'Y'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004300                                                                          
004400*    --- WORK FIELD FOR MOVING SPACE TO ANY KIND OF FIELD                 
004500 01  W-SPACE.                                                             
004600     03 FILLER                   PIC X(50)   VALUE SPACE.                 
004700                                                                          
004800*    --- WORK FIELD FOR MOVING ALL + TO ANY KIND OF FIELD                 
004900 01  W-PLUS.                                                              
005000     03 FILLER                   PIC X(50)   VALUE                        
005100        '++++++++++++++++++++++++++++++++++++++++++++++++++'.             
005200                                                                          
005300 77  MAX-MOD-LAENGD              PIC S9(4)  VALUE +901  COMP SYNC.        
005400 77  RAD-IX1                     PIC S9(3)  VALUE +0    COMP SYNC.        
005500 77  RAD-IX2                     PIC S9(3)  VALUE +0    COMP SYNC.        
005600 77  W-IDLEVNR                   PIC  X(5)  VALUE SPACE.                  
005700 77  INDX                        PIC S9(3)  VALUE +0    COMP SYNC.        
005800 77  INDX-DISPLAY                PIC  9(3)  VALUE  0.                     
005900 77  FIELD-NAME                  PIC X(16)   VALUE SPACE.                 
006000                                                                          
006100*    --- ARBETSFÄLT FÖR AKTUELLA VÄRDEN FRÅN SKÄRMEN                      
006200                                                                          
006300 77  WS-IDLEVNR                  PIC X(5)    VALUE SPACE.                 
006400 77  WS-KDRT                     PIC X(2)    VALUE SPACE.                 
006500 77  WS-IDFS                     PIC X(8)    VALUE SPACE.                 
006600 77  WS-TIAVIDAT                 PIC X(6)    VALUE SPACE.                 
006700 77  WS-ADINLOMR-PRT             PIC X(4)    VALUE SPACE.                 
006800 77  WS-IDFTG-CHK                PIC 9(2)    VALUE ZERO.                  
006900 77  WS-IDKONTO                  PIC 9(10)   VALUE ZERO.                  
007000 77  WS-IDANALYS                 PIC 9(12)   VALUE ZERO.                  
007100 77  WS-IDKST                    PIC X(10)   VALUE SPACE.                 
007200 77  WS-IDLBBET                  PIC X(12)   VALUE SPACE.                 
007300 77  WS-FLGODK                   PIC X(1)    VALUE SPACE.                 
007400 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
007500 77  FL-IDFS-OK                  PIC X       VALUE 'N'.                   
007600                                                                          
007700 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007800     88  INDATA-OK                           VALUE 'J'.                   
007900     88  INDATA-FEL                          VALUE 'N'.                   
008000                                                                          
008100 77  INDATA-IFYLLT-SW            PIC X       VALUE 'N'.                   
008200     88  INDATA-IFYLLT                       VALUE 'J'.                   
008300     88  INDATA-EJ-IFYLLT                    VALUE 'N'.                   
008400                                                                          
008500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
008600     88  NYCKLAR-OK                          VALUE 'J'.                   
008700     88  NYCKLAR-FEL                         VALUE 'N'.                   
008800                                                                          
008900 77  ALLT-SW                     PIC X       VALUE 'J'.                   
009000     88  ALLT-OK                             VALUE 'J'.                   
009100                                                                          
009200 77  W611REG-SW                  PIC X       VALUE 'N'.                   
009300     88  W611REG-OK                          VALUE 'J'.                   
009400                                                                          
009500 77  KONTO-LEV-SW                PIC X       VALUE 'J'.                   
009600     88  KONTO-LEV-FEL                       VALUE 'N'.                   
009700                                                                          
009800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
009900     88  EGEN-MID                            VALUE '6116'.                
010000     88  GODK-MID                            VALUE '6111' '6112'          
010100                                                   '6113' '6114'          
010200                                                   '6115' '6116'          
010300                                                   '6118' '6119'.         
010400     88  HELP-MID                            VALUE '0551'.                
010500       EJECT                                                              
010600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
010700 01  GENERELLA-SUBPROGRAM.                                                
010800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
010900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011100     03  W611REG                 PIC X(8)    VALUE 'W611REG'.             
011200     03  W411SAP                 PIC X(8)    VALUE 'W411SAP'.             
011300     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
011400     SKIP3                                                                
011500*    --- PARAMETERS TO ABEND                                              
011600                                                                          
011700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
011800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012000     SKIP3                                                                
012100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
012200*01 -COPY WMEDAREA                                                        
012300     SKIP3                                                                
012310*01  -COPY WWIDFTG                                                        
012400 01  MESSAGE-CODES.                                                       
012500     03  INF-UPDATE-DONE               PIC X(3) VALUE '001'.              
012600     03  ERR-CORR-HILITE-FLDS          PIC X(3) VALUE '020'.              
012700     03  ERR-UPDATE-NOT-DONE           PIC X(3) VALUE '004'.              
012800     03  ERR-PART-SUPERSEDED           PIC X(3) VALUE '223'.              
012900     03  INF-IDFS-ALREADY-EXISTS       PIC X(3) VALUE '030'.              
013000     03  INF-AVROP-SAKNAS              PIC X(3) VALUE '336'.              
013100     03  INVALID-KEY-FIELDS            PIC X(3) VALUE '022'.              
013200     03  R-ERR-IN-LINE-ONE             PIC X(3) VALUE '337'.              
013300     03  SAP-ACCOUNT-MISSING-IN-R3     PIC X(3) VALUE '338'.              
013400     03  SAP-ANAL-CC-MUST-BE-ENTERED   PIC X(3) VALUE '344'.              
013500     03  SAP-COSTCENTER-MISSING-IN-R3  PIC X(3) VALUE '341'.              
013600     03  SAP-ANALYSIS-NO-MISSING-IN-R3 PIC X(3) VALUE '343'.              
013700     03  SAP-ACCOUNT-MUST-BE-REG       PIC X(3) VALUE '339'.              
013800     03  SAP-ANALYSIS-NO-NOT-ALLOWED   PIC X(3) VALUE '342'.              
013900     03  SAP-COSTCENTER-NOT-ALLOWED    PIC X(3) VALUE '340'.              
014000     03  ERR-PRICE-IS-MISSING          PIC X(3) VALUE '260'.              
014010     03  ERR-WRNG-CURRENCY             PIC X(3) VALUE '426'.              
014100     03  ERR-PART-MISSING              PIC X(3) VALUE '041'.              
014200     03  ERR-PART-OTHER-COMPANY        PIC X(3) VALUE '088'.              
014300                                                                          
014400 01  W-REG-MSG-CODES.                                                     
014500     03  R-INF-PART-SUPERSEDED     PIC X(3)    VALUE '220'.               
014600     03  R-INF-IDFS-ALREADY-EXISTS PIC X(3)    VALUE '201'.               
014700     03  R-INF-AVROP-SAKNAS        PIC X(3)    VALUE '285'.               
014800     03  R-ERR-PRICE-IS-MISSING    PIC X(3)    VALUE '301'.               
014810     03  R-ERR-WRNG-CURRENCY       PIC X(3)    VALUE '151'.               
014900     03  R-ERR-PART-MISSING        PIC X(3)    VALUE '017'.               
015000     03  R-ERR-PART-OTHER-COMPANY  PIC X(3)    VALUE '088'.               
015100*                                                                         
015200*    --- PARAMETRAR TILL SUBPROGRAM W611REG                               
015300 01  FILLER                      PIC X(08)   VALUE 'W611REG'.             
015400*                                                                         
015500*01  -COPY W611REG0                                                       
015600*01  FILLER -COPY W611REG6    -RED LAENK-W611REG0.                        
015700*    EJECT                                                                
015800*    --- PARAMETRAR TILL SUBPROGRAM W411SAP                               
015900 01  FILLER                      PIC X(08)   VALUE 'W411SAP'.             
016000*                                                                         
016100*01  -COPY W411SAP                                                        
016200     SKIP3                                                                
016300*    --- IMS FUNKTIONSKODER                                               
016400*01  -COPY W0003                                                          
016500     EJECT                                                                
016600 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
016700     SKIP3                                                                
016800*01  -COPY WMFSAREA                                                       
016900     EJECT                                                                
017000                                                                          
017100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017200*                                                                         
017300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017400 01  KEYS-TO-DLI.                                                         
017500     03  W-IDDC-B6-X.                                                     
017600         05 W-IDDC-B6                  PIC X(2).                          
017700     03  W-IDDC-B6-LEV-X.                                                 
017800         05 W-IDDC-B6-LEV              PIC X(5).                          
017810     03  WS-IDFTG-B6                   PIC 9(2)  VALUE ZERO.              
017900                                                                          
018000 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
018100 01   DLI-IO-AREA-B601.                                                   
018200*     03  -COPY WDB601                                                    
018300                                                                          
018400 01  FILLER               PIC X(16)   VALUE 'WDB601 LEV'.                 
018500 01   DLI-IO-AREA-B601-LEV.                                               
018600*     03  -COPY WDB601   -PRE LEV-                                        
018700                                                                          
018800 01  SSA1                        PIC X(256).                              
018900                                                                          
019000 01  STATUS-WS                   PIC XX.                                  
019100     88 SEGMENT-FINNS                        VALUE '  '.                  
019200     88 SEGMENT-SAKNAS                       VALUE 'GE'.                  
019300     88 BASEN-SLUT                           VALUE 'GB'.                  
019400                                                                          
019500 01  GODK-STATUSKODER.                                                    
019600     03 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
019700                                                                          
019800     EJECT                                                                
019900 LINKAGE SECTION.                                                         
020000 01  REQU-AREA.                                                           
020100*    03 -COPY WZ01REQU                                                    
020200*    03 -COPY W60116I1                                                    
020300                                                                          
020400 01  RESP-AREA.                                                           
020500*    03 -COPY WZ01RESP                                                    
020600*    03 -COPY W60116O1                                                    
020700                                                                          
020800 01  MAX-KVRADER                PIC S9(4)  COMP.                          
020900                                                                          
021000 01 -COPY W0009     -PRE MSG-                                             
021100                                                                          
021200*01  -COPY W0008   -PRE WDB6-LEV-                                         
021300     05  FILLER                  PIC X.                                   
021400                                                                          
021500*01  -COPY W0008   -PRE WDB6-                                             
021600     05  FILLER                  PIC X.                                   
021700                                                                          
021800*   PCB'ER FÖR SUB PGM W611REG                                            
021900                                                                          
022000 01  REG-INLA1-PCB               PIC X.                                   
022100                                                                          
022200 01  REG-INLA2-PCB               PIC X.                                   
022300                                                                          
022400 01  REG-INLA3-PCB               PIC X.                                   
022500                                                                          
022600 01  REG-LEVA-PCB                PIC X.                                   
022700                                                                          
022800 01  REG-ARTC-PCB                PIC X.                                   
022900                                                                          
023000 01  REG-BENA-PCB                PIC X.                                   
023100                                                                          
023200 01  REG-WDD9-PCB                PIC X.                                   
023300                                                                          
023400 01  REG-WDK7-PCB                PIC X.                                   
023500                                                                          
023510 01  REG-WDB6-PCB                PIC X.                                   
023520                                                                          
023600*   PCB'ER FÖR SUB PGM W411SAP                                            
023700                                                                          
023800 01  SAP-SAPC-PCB              PIC X.                                     
023900                                                                          
023910*01    -COPY W0008     -PRE 9305-                                         
023920     05  FILLER                  PIC X(30).                               
023930                                                                          
024000     EJECT                                                                
024100 PROCEDURE DIVISION  USING REQU-AREA     RESP-AREA   MAX-KVRADER          
024200                           MSG-PCB                                        
024300                           WDB6-LEV-PCB   WDB6-PCB                        
024400                           REG-INLA1-PCB  REG-INLA2-PCB                   
024500                           REG-INLA3-PCB  REG-LEVA-PCB                    
024600                                          REG-ARTC-PCB                    
024700                           REG-BENA-PCB   REG-WDD9-PCB                    
024800                           REG-WDK7-PCB   REG-WDB6-PCB                    
024900                           SAP-SAPC-PCB                                   
024910                           9305-PCB.                                      
025000 MAIN SECTION.                                                            
025100                                                                          
025200*    DISPLAY 'W6011110 IN:  ' REQU-AREA                                   
025300     PERFORM A-INIT                                                       
025400                                                                          
025500     PERFORM B-INSPECT-KEYS                                               
025600     IF NYCKLAR-OK                                                        
025700       PERFORM G-INSPECT-INPUT-UPDATE                                     
025800     END-IF                                                               
025900                                                                          
026000*    DISPLAY 'W6011110 OUT: ' RESP-AREA                                   
026100     MOVE ZERO                  TO RETURN-CODE                            
026200     GOBACK                                                               
026300     .                                                                    
026400     EJECT                                                                
026500 A-INIT SECTION.                                                          
026600                                                                          
026700                                                                          
026800     MOVE SPACE                TO REG6-IDTRANS                            
026900                                  REG6-IDFS                               
027000                                  REG6-IDDC                               
027100                                  REG6-IDLBBET                            
027200                                  REG6-IDANALYS                           
027210                                  REG6-IDKST                              
027300                                  REG6-IDLEVNR                            
027400     MOVE JA                   TO REG6-IDLEVNR-OK                         
027500                                  REG6-KDRT-OK                            
027600                                  REG6-IDFS-OK                            
027700                                  REG6-TIAVIDAT-OK                        
027800                                  REG6-IDFTG-OK                           
027900                                  REG6-IDANALYS-OK                        
028000                                  REG6-IDKONTO-OK                         
028100                                  REG6-IDKST-OK                           
028200     MOVE NEJ                  TO REG6-FLGODK-IDFS                        
028300                                  REG6-FLGODK-IDARTNR                     
028400     MOVE ZERO                 TO REG6-KDRT                               
028500                                  REG6-TIAVIDAT                           
028600                                  REG6-IDFTG                              
028700                                  REG6-IDKONTO                            
028900                                  RESP-KVRADER                            
029000                                                                          
029100     MOVE +1                   TO RAD-IX1                                 
029200     PERFORM UNTIL RAD-IX1      > MAX-KVRADER                             
029300         MOVE JA               TO REG6-IDARTNR-OK (RAD-IX1)               
029400         MOVE SPACE            TO REG6-IDMFSFEL   (RAD-IX1)               
029500         MOVE ZERO             TO REG6-IDARTNR    (RAD-IX1)               
029600                                  REG6-KVAVIS     (RAD-IX1)               
029700         ADD +1                TO RAD-IX1                                 
029800     END-PERFORM                                                          
029900                                                                          
030000     MOVE JA                   TO W611REG-SW                              
030100     MOVE NEJ                  TO INDATA-IFYLLT-SW                        
030200                                                                          
030300     MOVE SPACE                TO RESP-IDMSG-ERROR                        
030400     MOVE SPACE                TO RESP-IDMSG-INFO                         
030500     MOVE SPACE                TO RESP-IDELMT-ERROR                       
030600     .                                                                    
030700     EJECT                                                                
030800                                                                          
030900 B-INSPECT-KEYS SECTION.                                                  
031000     MOVE JA TO NYCKLAR-SW                                                
031100     MOVE SPACE TO MED-IDMFSFEL                                           
031200                                                                          
031300     PERFORM BA-INSPECT-IDLEVNR                                           
031400     PERFORM BB-INSPECT-KDRT                                              
031500     PERFORM BC-INSPECT-IDFS                                              
031600     PERFORM BD-INSPECT-TIAVIDAT                                          
031700     PERFORM BL-INSPECT-FLYTTA-IDDC                                       
031800     PERFORM BE-FLYTTA-IDLBBET                                            
031900     PERFORM BF-FLYTTA-IDFTG                                              
032000     PERFORM BG-FLYTTA-IDKONTO                                            
032100     PERFORM BI-FLYTTA-IDANALYS                                           
032200     PERFORM BN-FLYTTA-IDKST                                              
032300     PERFORM BJ-FLYTTA-FLGODK                                             
032400     PERFORM BK-FLYTTA-ADINLOMR-PRT                                       
032500     PERFORM BO-INSPECT-SDC-NDC-LEV                                       
032600                                                                          
032700     IF NYCKLAR-FEL                                                       
032800       MOVE INVALID-KEY-FIELDS   TO RESP-IDMSG-ERROR                      
032900       PERFORM RESP-ROER-EJ-FAELT-IN                                      
033000       PERFORM MFS-LAES-IN-IGEN                                           
033100     END-IF                                                               
033200     .                                                                    
033300     EJECT                                                                
033400 BA-INSPECT-IDLEVNR SECTION.                                              
033500     IF REQU-IDLEVNR-KEY       = ALL '+' OR SPACE                         
033600         MOVE ZERO             TO WS-IDLEVNR                              
033700     ELSE                                                                 
033800         MOVE REQU-IDLEVNR-KEY TO WS-IDLEVNR                              
033900     END-IF                                                               
034000                                                                          
034100                                                                          
034200     IF WS-IDLEVNR = ZERO                                                 
034300       MOVE 'IDLEVNR'          TO RESP-IDELMT-ERROR                       
034400       MOVE NEJ                TO NYCKLAR-SW                              
034500     ELSE                                                                 
034600       MOVE WS-IDLEVNR         TO W-IDLEVNR                               
034700     END-IF                                                               
034800     .                                                                    
034900     EJECT                                                                
035000 BB-INSPECT-KDRT  SECTION.                                                
035100     IF REQU-KDRT-KEY          = ALL '+' OR SPACE                         
035200        MOVE ZERO              TO WS-KDRT                                 
035300     ELSE                                                                 
035400        MOVE REQU-KDRT-KEY     TO WS-KDRT                                 
035500        INSPECT WS-KDRT REPLACING LEADING SPACE BY ZERO                   
035600     END-IF                                                               
035700                                                                          
035800     IF WS-KDRT NUMERIC AND WS-KDRT NOT = 99                              
035900       CONTINUE                                                           
036000     ELSE                                                                 
036100       MOVE 'KDRT'             TO RESP-IDELMT-ERROR                       
036200       MOVE NEJ                TO NYCKLAR-SW                              
036300     END-IF                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 BC-INSPECT-IDFS  SECTION.                                                
036700     MOVE REQU-IDFS-KEY      TO WS-IDFS                                   
036800                                                                          
036900     MOVE 1   TO INDX                                                     
037000     MOVE NEJ TO FL-IDFS-OK                                               
037100     PERFORM UNTIL INDX > 8 OR FL-IDFS-OK = JA                            
037200       IF WS-IDFS(INDX:1) NUMERIC                                         
037300         IF WS-IDFS(INDX:1) > ZERO                                        
037400           MOVE JA TO FL-IDFS-OK                                          
037500         END-IF                                                           
037600       END-IF                                                             
037700       ADD 1 TO INDX                                                      
037800     END-PERFORM                                                          
037900                                                                          
038000     IF FL-IDFS-OK = NEJ                                                  
038100       MOVE 'IDFS'             TO RESP-IDELMT-ERROR                       
038200       MOVE NEJ TO NYCKLAR-SW                                             
038300     END-IF                                                               
038400     .                                                                    
038500     EJECT                                                                
038600 BD-INSPECT-TIAVIDAT SECTION.                                             
038700     MOVE REQU-TIAVIDAT-KEY    TO WS-TIAVIDAT                             
038800     INSPECT WS-TIAVIDAT REPLACING LEADING SPACE BY ZERO                  
038900                                                                          
039000     IF WS-TIAVIDAT NUMERIC AND WS-TIAVIDAT > ZERO                        
039100       CONTINUE                                                           
039200     ELSE                                                                 
039300       MOVE NEJ                TO NYCKLAR-SW                              
039400       MOVE 'TIAVIDAT'         TO RESP-IDELMT-ERROR                       
039500     END-IF                                                               
039600     .                                                                    
039700     EJECT                                                                
039800 BE-FLYTTA-IDLBBET        SECTION.                                        
039900     IF REQU-IDLBBET = ALL '+' OR SPACE                                   
040000       MOVE SPACE               TO WS-IDLBBET                             
040100     ELSE                                                                 
040200       MOVE REQU-IDLBBET        TO WS-IDLBBET                             
040300     END-IF                                                               
040400     .                                                                    
040500     EJECT                                                                
040600 BF-FLYTTA-IDFTG          SECTION.                                        
040700     IF REQU-IDFTG             = ALL '+' OR SPACE                         
040800        MOVE ZERO              TO WS-IDFTG-CHK                            
040900     ELSE                                                                 
041000        MOVE REQU-IDFTG        TO WS-IDFTG-CHK                            
041100        INSPECT WS-IDFTG-CHK REPLACING LEADING SPACE BY ZERO              
041200     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
041500 BG-FLYTTA-IDKONTO          SECTION.                                      
041600     IF REQU-IDKONTO           = ALL '+' OR SPACE                         
041700        MOVE ZERO              TO WS-IDKONTO                              
041800     ELSE                                                                 
041900        MOVE REQU-IDKONTO      TO WS-IDKONTO                              
042000        INSPECT WS-IDKONTO REPLACING LEADING SPACE BY ZERO                
042100     END-IF                                                               
042200     .                                                                    
042300     EJECT                                                                
042400 BI-FLYTTA-IDANALYS       SECTION.                                        
042500     IF REQU-IDANALYS          = ALL '+' OR SPACE                         
042600        MOVE ZERO              TO WS-IDANALYS                             
042700     ELSE                                                                 
042800        MOVE REQU-IDANALYS     TO WS-IDANALYS                             
042900        INSPECT WS-IDANALYS    REPLACING LEADING SPACE BY ZERO            
043000     END-IF                                                               
043100     .                                                                    
043200     EJECT                                                                
043300 BJ-FLYTTA-FLGODK  SECTION.                                               
043400     IF REQU-FLGODK            = ALL '+'                                  
043500       MOVE SPACE            TO WS-FLGODK                                 
043600     ELSE                                                                 
043700       MOVE REQU-FLGODK      TO WS-FLGODK                                 
043800     END-IF                                                               
043900                                                                          
044000     IF WS-FLGODK = SPACE OR JA OR NEJ  OR YES                            
044100       IF WS-FLGODK = YES                                                 
044200         MOVE JA TO WS-FLGODK                                             
044300       END-IF                                                             
044400     ELSE                                                                 
044500       MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                      
044600       MOVE MFS-ALFA-FAELT-FEL   TO RESP-FLGODK-IN-ATTR                   
044700       MOVE NEJ                  TO NYCKLAR-SW                            
044800     END-IF                                                               
044900     .                                                                    
045000     EJECT                                                                
045100 BK-FLYTTA-ADINLOMR-PRT  SECTION.                                         
045200     MOVE REQU-ADINLOMR-PRT-KEY  TO WS-ADINLOMR-PRT                       
045300     .                                                                    
045400     EJECT                                                                
045500 BL-INSPECT-FLYTTA-IDDC  SECTION.                                         
045600     MOVE REQU-IDDC-KEY        TO W-IDDC-B6                               
045700     MOVE REQU-IDDC-KEY        TO WS-IDDC                                 
045800                                                                          
045900     PERFORM IMS-GU-WDB601                                                
046000     IF DCS-CDC OR DCS-CDC-TR OR DCS-NDC-PF OR DCS-NDC-CN OR              
046010        DCS-NDC-OTHERS OR DCS-NDC-SA                                      
046100*      CONTINUE                                                           
046110       MOVE DCS-IDFTG          TO WS-IDFTG                                
046120                                  WS-IDFTG-B6                             
046200     ELSE                                                                 
046300       MOVE NEJ                TO NYCKLAR-SW                              
046400       MOVE SPACE              TO DCS-IDDC                                
046500       MOVE 'IDDC'             TO RESP-IDELMT-ERROR                       
046600     END-IF                                                               
046700     .                                                                    
046800     EJECT                                                                
046900 BN-FLYTTA-IDKST  SECTION.                                                
047000     IF REQU-IDKST             = ALL '+' OR SPACE                         
047100        MOVE SPACE             TO WS-IDKST                                
047200     ELSE                                                                 
047300        MOVE REQU-IDKST        TO WS-IDKST                                
047500     END-IF                                                               
047600     .                                                                    
047700     EJECT                                                                
047800 BO-INSPECT-SDC-NDC-LEV SECTION.                                          
047900     IF WS-IDLEVNR NOT = SPACE                                            
048000       MOVE WS-IDLEVNR         TO W-IDDC-B6-LEV                           
048100       PERFORM IMS-GU-WDB601-LEV                                          
048200       IF SEGMENT-FINNS                                                   
048300         MOVE NEJ              TO NYCKLAR-SW                              
048400         MOVE 'IDLEVNR'        TO RESP-IDELMT-ERROR                       
048500       END-IF                                                             
048600     END-IF                                                               
048700     .                                                                    
048800     EJECT                                                                
048900 G-INSPECT-INPUT-UPDATE SECTION.                                          
049000     MOVE JA                   TO INDATA-SW                               
049100     MOVE '001'                TO RESP-IDMSGVER                           
049200     PERFORM GA-INSPECT-ECONOMY-DATA                                      
049300     PERFORM GB-INSPECT-COLUMNS                                           
049400     IF INDATA-FEL                                                        
049500       IF INDATA-EJ-IFYLLT                                                
049600         MOVE ERR-UPDATE-NOT-DONE    TO RESP-IDMSG-ERROR                  
049700       ELSE                                                               
049800         MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                    
049900       END-IF                                                             
050000                                                                          
050100       IF SAP-BEFEL NOT = SPACE                                           
050200         MOVE SAP-BEFEL            TO RESP-IDMSG-ERROR                    
050300       END-IF                                                             
050400       PERFORM RESP-ROER-EJ-FAELT-IN                                      
050500                                                                          
050600       EVALUATE SAP-IDMFSFEL                                              
050700         WHEN '346'                                                       
050800           MOVE SAP-ACCOUNT-MISSING-IN-R3 TO RESP-IDMSG-ERROR             
050900           MOVE 'IDKONTO'                 TO RESP-IDELMT-ERROR            
051000           MOVE MFS-ALFA-FAELT-FEL        TO RESP-IDKONTO-IN-ATTR         
051100         WHEN '350'                                                       
051200           MOVE SAP-ACCOUNT-MUST-BE-REG   TO RESP-IDMSG-ERROR             
051300           MOVE 'IDKONTO'                 TO RESP-IDELMT-ERROR            
051400           MOVE MFS-ALFA-FAELT-FEL        TO RESP-IDKONTO-IN-ATTR         
051500         WHEN '347'                                                       
051600           MOVE SAP-ANALYSIS-NO-MISSING-IN-R3                             
051700                                          TO RESP-IDMSG-ERROR             
051800           MOVE 'IDANALYS'                TO RESP-IDELMT-ERROR            
051900           MOVE MFS-ALFA-FAELT-FEL        TO RESP-IDANALYS-IN-ATTR        
052000         WHEN '352'                                                       
052100           MOVE SAP-ANALYSIS-NO-NOT-ALLOWED                               
052200                                          TO RESP-IDMSG-ERROR             
052300           MOVE 'IDANALYS'                TO RESP-IDELMT-ERROR            
052400           MOVE MFS-ALFA-FAELT-FEL        TO RESP-IDANALYS-IN-ATTR        
052500         WHEN '348'                                                       
052600           MOVE SAP-COSTCENTER-MISSING-IN-R3                              
052700                                          TO RESP-IDMSG-ERROR             
052800           MOVE 'IDKST'                   TO RESP-IDELMT-ERROR            
052900           MOVE MFS-ALFA-FAELT-FEL        TO RESP-IDKST-IN-ATTR           
053000         WHEN '351'                                                       
053100           MOVE SAP-COSTCENTER-NOT-ALLOWED                                
053200                                          TO RESP-IDMSG-ERROR             
053300           MOVE 'IDKST'                   TO RESP-IDELMT-ERROR            
053400           MOVE MFS-ALFA-FAELT-FEL        TO RESP-IDKST-IN-ATTR           
053500         WHEN '352'                                                       
053600           MOVE SAP-ANAL-CC-MUST-BE-ENTERED TO RESP-IDMSG-ERROR           
053700           MOVE MFS-ALFA-FAELT-FEL       TO RESP-IDANALYS-IN-ATTR         
053800                                            RESP-IDKST-IN-ATTR            
053900       END-EVALUATE                                                       
054000                                                                          
054100**     IF SAP-IDKONTO-OK = NEJ                                            
054200**       MOVE SAP-ACCOUNT-MISSING-IN-R3 TO RESP-IDMSG-ERROR               
054300**       MOVE 'IDKONTO'               TO RESP-IDELMT-ERROR                
054400**       MOVE MFS-ALFA-FAELT-FEL        TO RESP-IDKONTO-IN-ATTR           
054500**     ELSE                                                               
054600**       IF SAP-IDANALYS-OK = NEJ AND SAP-IDKST-OK = NEJ                  
054700**         MOVE SAP-ANAL-CC-MUST-BE-ENTERED TO RESP-IDMSG-ERROR           
054800**         MOVE MFS-ALFA-FAELT-FEL     TO RESP-IDANALYS-IN-ATTR           
054900**                                        RESP-IDKST-IN-ATTR              
055000**       ELSE                                                             
055100**         IF SAP-IDANALYS-OK = NEJ                                       
055200**           MOVE SAP-ANALYSIS-NO-MISSING-IN-R3                           
055300**                                       TO RESP-IDMSG-ERROR              
055400**           MOVE 'IDANALYS'             TO RESP-IDELMT-ERROR             
055500**           MOVE MFS-ALFA-FAELT-FEL     TO RESP-IDANALYS-IN-ATTR         
055600**         ELSE                                                           
055700**           IF SAP-IDKST-OK = NEJ                                        
055800**             MOVE SAP-COSTCENTER-MISSING-IN-R3                          
055900**                                       TO RESP-IDMSG-ERROR              
056000**             MOVE 'IDKST'              TO RESP-IDELMT-ERROR             
056100**             MOVE MFS-ALFA-FAELT-FEL   TO RESP-IDKST-IN-ATTR            
056200**           END-IF                                                       
056300**         END-IF                                                         
056400**       END-IF                                                           
056500**     END-IF                                                             
056600     ELSE                                                                 
056700       IF WS-FLGODK = NEJ                                                 
056800         MOVE ERR-UPDATE-NOT-DONE TO RESP-IDMSG-ERROR                     
056900         MOVE 'FLGODK'            TO RESP-IDELMT-ERROR                    
057000         PERFORM MFS-FORM-ATTR                                            
057100         PERFORM RESP-RENSA-FAELT-IN                                      
057200         MOVE SPACE           TO RESP-IDFTG-IN                            
057300                                 RESP-IDKONTO-IN                          
057400                                 RESP-IDANALYS-IN                         
057500                                 RESP-IDKST-IN                            
057600       ELSE                                                               
057700         PERFORM GC-ANROPA-W611REG                                        
057800         IF WS-FLGODK          = JA OR NEJ                                
057900           CONTINUE                                                       
058000         ELSE                                                             
058100           PERFORM GD-INSPECT-FELFLAGGOR                                  
058200         END-IF                                                           
058300                                                                          
058400         IF W611REG-OK                                                    
058500           MOVE INF-UPDATE-DONE TO RESP-IDMSG-INFO                        
058600           PERFORM MFS-FORM-ATTR                                          
058700           PERFORM RESP-RENSA-FAELT-IN                                    
058800           MOVE SPACE           TO RESP-IDFTG-IN                          
058900                                   RESP-IDKONTO-IN                        
059000                                   RESP-IDANALYS-IN                       
059100                                   RESP-IDKST-IN                          
059200         ELSE                                                             
059300           IF RESP-IDMSG-INFO  = R-INF-IDFS-ALREADY-EXISTS                
059400             MOVE R-INF-IDFS-ALREADY-EXISTS TO RESP-IDMSG-INFO            
059500           ELSE                                                           
059600             IF RESP-IDMSG-ERROR       = SPACE                            
059700               MOVE ERR-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR              
059800             ELSE                                                         
059900               IF RESP-IDMSG-ERROR = R-INF-PART-SUPERSEDED                
060000               OR RESP-IDMSG-ERROR = R-INF-AVROP-SAKNAS                   
060100                                                                          
060200                 MOVE MFS-OEPPNA-ALFA-FAELT TO                            
060300                                 RESP-FLGODK-IN-ATTR                      
060400                 PERFORM MFS-STAENG-KOL-FAELT                             
060500               END-IF                                                     
060600             END-IF                                                       
060700           END-IF                                                         
060800           PERFORM RESP-ROER-EJ-FAELT-IN                                  
060900         END-IF                                                           
061000       END-IF                                                             
061100     END-IF                                                               
061200                                                                          
061300     IF (REG6-IDFTG-OK = JA AND W611REG-OK) OR                            
061400         REQU-IDFTG    = ALL '+'                                          
061500         MOVE SPACE              TO RESP-IDFTG-IN                         
061600     END-IF                                                               
061700*                                                                         
061800     IF (REG6-IDKONTO-OK = JA  AND W611REG-OK) OR                         
061900         REQU-IDKONTO     = ALL '+'                                       
062000         MOVE SPACE              TO RESP-IDKONTO-IN                       
062100     END-IF                                                               
062200*                                                                         
062300     IF (REG6-IDANALYS-OK = JA    AND W611REG-OK) OR                      
062400         REQU-IDANALYS       = ALL '+'                                    
062500         MOVE SPACE              TO RESP-IDANALYS-IN                      
062600     END-IF                                                               
062700*                                                                         
062800     IF (REG6-IDKST-OK = JA  AND W611REG-OK) OR                           
062900         REQU-IDKST     = ALL '+'                                         
063000         MOVE SPACE              TO RESP-IDKST-IN                         
063100     END-IF                                                               
063200                                                                          
063300**   COVERT THE ERROR CODE FROM SUBPROGRAM TO WEB ERR CODES               
063400     IF RESP-IDMSG-ERROR = R-INF-PART-SUPERSEDED                          
063500       MOVE ERR-PART-SUPERSEDED       TO RESP-IDMSG-ERROR                 
063600       MOVE 'IDARTNR'                 TO RESP-IDELMT-ERROR                
063700     ELSE                                                                 
063800       IF RESP-IDMSG-ERROR = R-INF-IDFS-ALREADY-EXISTS                    
063900         MOVE INF-IDFS-ALREADY-EXISTS TO RESP-IDMSG-INFO                  
064000         MOVE 'IDAVINR'               TO RESP-IDELMT-ERROR                
064100         MOVE SPACES                  TO RESP-IDMSG-ERROR                 
064200       ELSE                                                               
064300         IF RESP-IDMSG-ERROR = R-INF-AVROP-SAKNAS                         
064400           MOVE INF-AVROP-SAKNAS      TO RESP-IDMSG-ERROR                 
064500         ELSE                                                             
064600           IF RESP-IDMSG-ERROR = R-ERR-PRICE-IS-MISSING                   
064700             MOVE ERR-PRICE-IS-MISSING  TO RESP-IDMSG-ERROR               
064800           ELSE                                                           
064810             IF RESP-IDMSG-ERROR = R-ERR-WRNG-CURRENCY                    
064820               MOVE ERR-WRNG-CURRENCY     TO RESP-IDMSG-ERROR             
064830             ELSE                                                         
064900               IF RESP-IDMSG-ERROR = R-ERR-PART-MISSING                   
065000                 MOVE ERR-PART-MISSING      TO RESP-IDMSG-ERROR           
065100                 MOVE 'IDARTNR'             TO RESP-IDELMT-ERROR          
065200               ELSE                                                       
065300                 IF RESP-IDMSG-ERROR = R-ERR-PART-OTHER-COMPANY           
065400                   MOVE ERR-PART-OTHER-COMPANY TO RESP-IDMSG-ERROR        
065500                 END-IF                                                   
065600               END-IF                                                     
065610             END-IF                                                       
065700           END-IF                                                         
065800         END-IF                                                           
065900       END-IF                                                             
066000     END-IF                                                               
066100                                                                          
066200*                                                                         
066300     IF RESP-IDMSG-INFO = R-INF-IDFS-ALREADY-EXISTS                       
066400       MOVE INF-IDFS-ALREADY-EXISTS TO RESP-IDMSG-INFO                    
066500       MOVE 'IDAVINR'               TO RESP-IDELMT-ERROR                  
066600     END-IF                                                               
066700     .                                                                    
066800     EJECT                                                                
066900 GA-INSPECT-ECONOMY-DATA SECTION.                                         
067000     PERFORM GAA-INSPECT-NUMERIC                                          
067100     IF INDATA-OK                                                         
067200       PERFORM GAC-INSPECT-SAP-DATA                                       
067300     END-IF                                                               
067400     .                                                                    
067500     EJECT                                                                
067600 GAA-INSPECT-NUMERIC       SECTION.                                       
067700     IF WS-IDFTG-CHK NUMERIC                                              
067800       MOVE MFS-NUM-FAELT-RAETT TO RESP-IDFTG-IN-ATTR                     
067900     ELSE                                                                 
068000       MOVE MFS-NUM-FAELT-FEL   TO RESP-IDFTG-IN-ATTR                     
068100       MOVE NEJ                 TO INDATA-SW                              
068200                                   REG6-IDFTG-OK                          
068300     END-IF                                                               
068400                                                                          
068500     IF WS-IDKONTO NUMERIC                                                
068600       MOVE MFS-NUM-FAELT-RAETT TO RESP-IDKONTO-IN-ATTR                   
068700     ELSE                                                                 
068800       MOVE MFS-NUM-FAELT-FEL   TO RESP-IDFTG-IN-ATTR                     
068900       MOVE NEJ                 TO INDATA-SW                              
069000                                   REG6-IDKONTO-OK                        
069100     END-IF                                                               
069200                                                                          
069300     IF WS-IDANALYS NUMERIC                                               
069400       MOVE MFS-NUM-FAELT-RAETT TO RESP-IDANALYS-IN-ATTR                  
069500     ELSE                                                                 
069600       MOVE MFS-NUM-FAELT-FEL   TO RESP-IDANALYS-IN-ATTR                  
069700       MOVE NEJ                 TO INDATA-SW                              
069800                                   REG6-IDANALYS-OK                       
069900     END-IF                                                               
070000                                                                          
070200     MOVE MFS-NUM-FAELT-RAETT TO RESP-IDKST-IN-ATTR                       
070800     .                                                                    
070900     EJECT                                                                
071000 GAC-INSPECT-SAP-DATA      SECTION.                                       
071100*    IF DCS-CDC OR DCS-CDC-TR OR DCS-NDC-PF OR DCS-NDC-OTHERS             
071200*      MOVE 'SEPV'             TO SAP-KDTRADP                             
071300*    ELSE                                                                 
071400*      IF DCS-NDC-CN                                                      
071500*        MOVE 'CN05'           TO SAP-KDTRADP                             
071600*      ELSE                                                               
071700*        MOVE 'SEPV'           TO SAP-KDTRADP                             
071800*      END-IF                                                             
071900*    END-IF                                                               
071910     IF IDFTG-NON-VCC                                                     
071920       PERFORM IMS-GU-WDB601-FTG                                          
071930       IF SEGMENT-FINNS                                                   
071940         MOVE DCS-KDTRADP    TO SAP-KDTRADP                               
071950       END-IF                                                             
071960     ELSE                                                                 
071970       MOVE 'SEPV'           TO SAP-KDTRADP                               
071980     END-IF                                                               
072000*                                                                         
072100     MOVE ZERO                   TO SAP-IDDISTR                           
072200     MOVE SPACE                  TO SAP-KDFAKTYP                          
072300     MOVE ZERO                   TO SAP-IDFTG                             
072400     IF WS-IDKONTO NOT NUMERIC                                            
072500       MOVE ZERO                 TO SAP-IDKONTO                           
072600     ELSE                                                                 
072700       MOVE WS-IDKONTO           TO SAP-IDKONTO                           
072800     END-IF                                                               
072900     IF WS-IDANALYS = ALL '+' OR                                          
073000        WS-IDANALYS = ALL '0'                                             
073100       MOVE SPACE                TO SAP-IDANALYS                          
073200     ELSE                                                                 
073300       MOVE WS-IDANALYS          TO SAP-IDANALYS                          
073400     END-IF                                                               
073800     MOVE WS-IDKST               TO SAP-IDKST                             
074000     MOVE SPACE                  TO SAP-IDPROFIT                          
074100     MOVE +2                     TO SAP-KDCALL                            
074200                                                                          
074300     CALL W411SAP USING SAP-W411SAP SAP-SAPC-PCB                          
074400                                                                          
074500     IF SAP-BEFEL NOT = SPACE                                             
074600       MOVE NEJ                  TO INDATA-SW                             
074700       IF SAP-IDKONTO-OK = NEJ                                            
074800         MOVE MFS-ALFA-FAELT-FEL TO RESP-IDKONTO-IN-ATTR                  
074900       ELSE                                                               
075000         IF SAP-IDANALYS-OK = NEJ                                         
075100           MOVE MFS-ALFA-FAELT-FEL TO RESP-IDANALYS-IN-ATTR               
075200         ELSE                                                             
075300           IF SAP-IDKST-OK = NEJ                                          
075400             MOVE MFS-ALFA-FAELT-FEL TO RESP-IDKST-IN-ATTR                
075500           END-IF                                                         
075600         END-IF                                                           
075700       END-IF                                                             
075800     END-IF                                                               
075900     .                                                                    
076000     EJECT                                                                
076100 GB-INSPECT-COLUMNS     SECTION.                                          
076200     MOVE +1 TO RAD-IX1                                                   
076300     PERFORM UNTIL RAD-IX1     > MAX-KVRADER                              
076400       EVALUATE TRUE                                                      
076500         WHEN REQU-IDARTNR-LINE(RAD-IX1) = ALL '+' AND                    
076600              REQU-KVAVIS-LINE (RAD-IX1) = ALL '+'                        
076700           CONTINUE                                                       
076800                                                                          
076900         WHEN (REQU-IDARTNR-LINE(RAD-IX1) NUMERIC AND                     
077000               REQU-KVAVIS-LINE (RAD-IX1) NUMERIC)                        
077100             MOVE JA                      TO INDATA-IFYLLT-SW             
077200             MOVE MFS-NUM-FAELT-RAETT TO                                  
077300                               RESP-IDARTNR-LINE-ATTR (RAD-IX1)           
077400                               RESP-KVAVIS-LINE-ATTR   (RAD-IX1)          
077500             MOVE REQU-IDARTNR-LINE(RAD-IX1) TO                           
077600                               RESP-IDARTNR-LINE(RAD-IX1)                 
077700             MOVE REQU-KVAVIS-LINE (RAD-IX1) TO                           
077800                               RESP-KVAVIS-LINE (RAD-IX1)                 
077900         WHEN OTHER                                                       
078000           MOVE MFS-NUM-FAELT-FEL TO                                      
078100                               RESP-IDARTNR-LINE-ATTR (RAD-IX1)           
078200                               RESP-KVAVIS-LINE-ATTR (RAD-IX1)            
078300           MOVE NEJ              TO INDATA-SW                             
078400       END-EVALUATE                                                       
078500       ADD +1                  TO RAD-IX1                                 
078600     END-PERFORM                                                          
078700                                                                          
078800     IF INDATA-EJ-IFYLLT                                                  
078900       MOVE NEJ                   TO INDATA-SW                            
079000     END-IF                                                               
079100     .                                                                    
079200     EJECT                                                                
079300 GC-ANROPA-W611REG  SECTION.                                              
079400                                                                          
079500     PERFORM GCA-SKAPA-LAENKAREA                                          
079600     CALL W611REG USING LAENK-W611REG0                                    
079700                        REG-INLA1-PCB REG-INLA2-PCB                       
079800                        REG-INLA3-PCB REG-LEVA-PCB                        
079900                                      REG-ARTC-PCB                        
080000                        REG-BENA-PCB  REG-WDD9-PCB                        
080100                        REG-WDK7-PCB  REG-WDB6-PCB                        
080110                        9305-PCB                                          
080200     .                                                                    
080300     EJECT                                                                
080400 GCA-SKAPA-LAENKAREA  SECTION.                                            
080500                                                                          
080600     MOVE '6116'               TO REG6-IDTRANS                            
080700     MOVE DCS-IDDC             TO REG6-IDDC                               
080800     MOVE WS-IDLEVNR           TO REG6-IDLEVNR                            
080900     MOVE WS-KDRT              TO REG6-KDRT                               
081000     MOVE WS-IDFS              TO REG6-IDFS                               
081100     MOVE WS-TIAVIDAT          TO REG6-TIAVIDAT                           
081200     MOVE WS-IDFTG-CHK         TO REG6-IDFTG                              
081300     MOVE WS-IDKONTO           TO REG6-IDKONTO                            
081400     MOVE WS-IDANALYS          TO REG6-IDANALYS                           
081500     MOVE WS-IDKST             TO REG6-IDKST                              
081600     MOVE WS-IDLBBET           TO REG6-IDLBBET                            
081700     MOVE WS-FLGODK            TO REG6-FLGODK                             
081800     MOVE MAX-KVRADER          TO REG6-KVRADER-MAX                        
081900                                                                          
082000     MOVE +1    TO RAD-IX1                                                
082100     PERFORM UNTIL RAD-IX1  >  REG6-KVRADER-MAX                           
082200         IF REQU-IDARTNR-LINE(RAD-IX1) = ALL '+'                          
082300           MOVE ZERO                  TO REG6-IDARTNR (RAD-IX1)           
082400                                         REG6-KVAVIS  (RAD-IX1)           
082500         ELSE                                                             
082600           MOVE REQU-IDARTNR-LINE(RAD-IX1)                                
082700                                      TO REG6-IDARTNR (RAD-IX1)           
082800           MOVE REQU-KVAVIS-LINE(RAD-IX1)                                 
082900                                      TO REG6-KVAVIS  (RAD-IX1)           
083000         END-IF                                                           
083100         ADD +1                       TO RAD-IX1                          
083200     END-PERFORM                                                          
083300     .                                                                    
083400     EJECT                                                                
083500 GD-INSPECT-FELFLAGGOR SECTION.                                           
083600     MOVE +1                   TO RAD-IX2                                 
083700     PERFORM GDA-INSPECT-FELFLAGGOR-KOLUMN                                
083800                                                                          
083900     IF REG6-IDLEVNR-OK                 =  JA  AND                        
084000        REG6-KDRT-OK                    =  JA  AND                        
084100        REG6-TIAVIDAT-OK                =  JA  AND                        
084200        REG6-IDFS-OK                    =  JA                             
084300       IF REG6-FLGODK-IDARTNR           =  JA                             
084400         MOVE MFS-OEPPNA-ALFA-FAELT    TO RESP-FLGODK-IN-ATTR             
084500         MOVE INF-IDFS-ALREADY-EXISTS  TO RESP-IDMSG-INFO                 
084600         MOVE 'IDAVINR'                TO RESP-IDELMT-ERROR               
084700         PERFORM MFS-STAENG-IN-FAELT                                      
084800         MOVE NEJ                      TO W611REG-SW                      
084900       END-IF                                                             
085000     ELSE                                                                 
085100       MOVE NEJ                     TO W611REG-SW                         
085200                                                                          
085300       IF REG6-IDLEVNR-OK               = JA    AND                       
085400          REG6-KDRT-OK                  = JA    AND                       
085500          REG6-TIAVIDAT-OK              = JA    AND                       
085600          REG6-FLGODK-IDFS              = JA                              
085700         MOVE MFS-OEPPNA-ALFA-FAELT   TO RESP-FLGODK-IN-ATTR              
085800         MOVE INF-IDFS-ALREADY-EXISTS TO RESP-IDMSG-INFO                  
085900         MOVE 'IDAVINR'               TO RESP-IDELMT-ERROR                
086000         PERFORM MFS-STAENG-IN-FAELT                                      
086100       ELSE                                                               
086200         IF RESP-IDMSG-ERROR = SPACE                                      
086300           MOVE R-ERR-IN-LINE-ONE      TO RESP-IDMSG-ERROR                
086400         END-IF                                                           
086500       END-IF                                                             
086600     END-IF                                                               
086700     .                                                                    
086800     EJECT                                                                
086900 GDA-INSPECT-FELFLAGGOR-KOLUMN SECTION.                                   
087000     MOVE +1                   TO RAD-IX1                                 
087100     PERFORM UNTIL RAD-IX1     >  REG6-KVRADER-MAX                        
087200       IF REG6-IDARTNR-OK (RAD-IX2) = JA                                  
087300         MOVE MFS-NUM-FAELT-RAETT TO                                      
087400                                RESP-IDARTNR-LINE-ATTR (RAD-IX1)          
087500         MOVE REQU-IDARTNR-LINE(RAD-IX1) TO                               
087600                                RESP-IDARTNR-LINE(RAD-IX1)                
087700       ELSE                                                               
087800         IF RESP-IDMSG-ERROR = SPACE OR R-INF-IDFS-ALREADY-EXISTS         
088000           MOVE REG6-IDMFSFEL (RAD-IX2) TO RESP-IDMSG-ERROR               
088100         END-IF                                                           
088200         MOVE MFS-NUM-FAELT-FEL TO                                        
088300                                RESP-IDARTNR-LINE-ATTR (RAD-IX1)          
088400         MOVE NEJ               TO W611REG-SW                             
088500       END-IF                                                             
088600                                                                          
088700       ADD +1                             TO RAD-IX1                      
088800                                             RAD-IX2                      
088900     END-PERFORM                                                          
089000                                                                          
089100     IF RESP-IDMSG-ERROR = SPACE OR R-INF-IDFS-ALREADY-EXISTS             
089200       CONTINUE                                                           
089300     ELSE                                                                 
089400       MOVE +1                            TO RAD-IX1                      
089500       PERFORM UNTIL RAD-IX1 > REG6-KVRADER-MAX                           
089600         IF REG6-IDMFSFEL (RAD-IX1) = R-INF-IDFS-ALREADY-EXISTS           
089700             MOVE MFS-NUM-FAELT-RAETT     TO                              
089800                         RESP-IDARTNR-LINE-ATTR (RAD-IX1)                 
089900         END-IF                                                           
090000         ADD +1                           TO RAD-IX1                      
090100       END-PERFORM                                                        
090200     END-IF                                                               
090300     .                                                                    
090400     EJECT                                                                
090500                                                                          
090600                                                                          
090700* --- RESP SEKTIONER ---                                                  
090800* --- RESP SEKTIONER ---                                                  
090900* --- RESP SEKTIONER ---                                                  
091000                                                                          
091100 RESP-RENSA-FAELT-UT-RADER SECTION.                                       
091200                                                                          
091300*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
091400                                                                          
091500     MOVE +1                     TO RAD-IX1                               
091600     PERFORM UNTIL RAD-IX1       >  MAX-KVRADER                           
091700       PERFORM RESP-RENSA-RAD-FAELT-UT                                    
091800       ADD +1                TO RAD-IX1                                   
091900     END-PERFORM                                                          
092000     .                                                                    
092100     SKIP2                                                                
092200                                                                          
092300 RESP-RENSA-RAD-FAELT-UT SECTION.                                         
092400                                                                          
092500*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
092600     MOVE W-SPACE         TO RESP-IDARTNR-LINE (RAD-IX1)                  
092700                             RESP-KVAVIS-LINE (RAD-IX1)                   
092800     .                                                                    
092900     SKIP2                                                                
093000                                                                          
093100 RESP-RENSA-FAELT-IN SECTION.                                             
093200*    --- ALLA INDATA-FÄLT                                                 
093300     MOVE W-SPACE              TO RESP-FLGODK-IN                          
093400                                                                          
093500     MOVE +1                   TO RAD-IX1                                 
093600     PERFORM UNTIL RAD-IX1     >  MAX-KVRADER                             
093700         MOVE W-SPACE          TO RESP-IDARTNR-LINE (RAD-IX1)             
093800                                  RESP-KVAVIS-LINE (RAD-IX1)              
093900         ADD +1                TO RAD-IX1                                 
094000     END-PERFORM                                                          
094100     .                                                                    
094200     SKIP2                                                                
094300                                                                          
094400 RESP-ROER-EJ-FAELT-IN SECTION.                                           
094500*    --- ALLA INDATA-FÄLT                                                 
094600     MOVE W-PLUS               TO RESP-IDLBBET                            
094700                                  RESP-IDFTG-IN                           
094800                                  RESP-IDKONTO-IN                         
094900                                  RESP-IDANALYS-IN                        
095000                                  RESP-IDKST-IN                           
095100                                  RESP-FLGODK-IN                          
095200                                                                          
095300     MOVE +1                     TO RAD-IX1                               
095400     PERFORM UNTIL RAD-IX1       >  MAX-KVRADER                           
095500       MOVE W-PLUS               TO RESP-IDARTNR-LINE(RAD-IX1)            
095600                                    RESP-KVAVIS-LINE(RAD-IX1)             
095700       ADD +1                    TO RAD-IX1                               
095800     END-PERFORM                                                          
095900     .                                                                    
096000     SKIP2                                                                
096100                                                                          
096200 MFS-LAES-IN-IGEN SECTION.                                                
096300                                                                          
096400     MOVE MFS-ADD-LAES-IN-FAELT TO RESP-IDFTG-IN-ATTR                     
096500                                   RESP-IDKONTO-IN-ATTR                   
096600                                   RESP-IDANALYS-IN-ATTR                  
096700                                   RESP-IDKST-IN-ATTR                     
096800                                                                          
096900     PERFORM MFS-LAES-IN-IGEN-KOL-FAELT                                   
097000     .                                                                    
097100     EJECT                                                                
097200 MFS-LAES-IN-IGEN-KOL-FAELT SECTION.                                      
097300                                                                          
097400     MOVE +1                     TO RAD-IX1                               
097500     PERFORM UNTIL RAD-IX1       >  MAX-KVRADER                           
097600         MOVE MFS-ADD-LAES-IN-FAELT TO                                    
097700                                RESP-IDARTNR-LINE-ATTR (RAD-IX1)          
097800                                RESP-KVAVIS-LINE-ATTR (RAD-IX1)           
097900         ADD +1                 TO RAD-IX1                                
098000     END-PERFORM                                                          
098100     .                                                                    
098200     SKIP2                                                                
098300 MFS-FORM-ATTR SECTION.                                                   
098400                                                                          
098500     MOVE MFS-FORMATETS-ATTR   TO RESP-IDLBBET                            
098600                                  RESP-FLGODK-IN                          
098700                                                                          
098800     PERFORM MFS-FORM-ATTR-KOL-FAELT-IN                                   
098900     EJECT                                                                
099000     .                                                                    
099100 MFS-FORM-ATTR-KOL-FAELT-IN SECTION.                                      
099200                                                                          
099300     MOVE +1                     TO RAD-IX1                               
099400     PERFORM UNTIL RAD-IX1       >  MAX-KVRADER                           
099500       MOVE MFS-FORMATETS-ATTR TO RESP-IDARTNR-LINE-ATTR (RAD-IX1)        
099600                                  RESP-KVAVIS-LINE-ATTR (RAD-IX1)         
099700       ADD +1                    TO RAD-IX1                               
099800     END-PERFORM                                                          
099900     .                                                                    
100000     SKIP2                                                                
100100                                                                          
100200* --- IMS SEKTIONER ---                                                   
100300* --- IMS SEKTIONER ---                                                   
100400* --- IMS SEKTIONER ---                                                   
100500     SKIP3                                                                
100600 MFS-STAENG-IN-FAELT  SECTION.                                            
100700     MOVE MFS-STAENG-FAELT            TO RESP-IDFTG-IN-ATTR               
100800                                         RESP-IDKONTO-IN-ATTR             
100900                                         RESP-IDANALYS-IN-ATTR            
101000                                         RESP-IDKST-IN-ATTR               
101100                                                                          
101200     MOVE +1                          TO RAD-IX2                          
101300     PERFORM MFS-STAENG-KOL-FAELT                                         
101400     .                                                                    
101500     SKIP3                                                                
101600 MFS-STAENG-KOL-FAELT SECTION.                                            
101700     MOVE +1                          TO RAD-IX1                          
101800     PERFORM UNTIL RAD-IX1            =  MAX-KVRADER                      
101900       IF REG6-IDARTNR-OK (RAD-IX2) =  JA                                 
102000         MOVE MFS-STAENG-FAELT TO                                         
102100                             RESP-IDARTNR-LINE-ATTR (RAD-IX1)             
102200       ELSE                                                               
102300          MOVE MFS-STAENG-FAELT-HI TO                                     
102400                             RESP-IDARTNR-LINE-ATTR (RAD-IX1)             
102500       END-IF                                                             
102600       MOVE MFS-STAENG-FAELT TO                                           
102700                             RESP-KVAVIS-LINE-ATTR (RAD-IX1)              
102800       ADD +1                TO RAD-IX1                                   
102900                                RAD-IX2                                   
103000     END-PERFORM                                                          
103100     .                                                                    
103200     EJECT                                                                
103300 IMS-GU-WDB601-LEV SECTION.                                               
103400     STRING 'WDB601  (IDLEVNDC =' W-IDDC-B6-LEV-X ')'                     
103500          DELIMITED BY SIZE INTO SSA1                                     
103600     MOVE '  GE' TO GODK-STATUSKODER                                      
103700     CALL CBLTDLI USING GU WDB6-LEV-PCB DLI-IO-AREA-B601-LEV SSA1         
103800     MOVE WDB6-LEV-STATUS-CODE    TO STATUS-WS                            
103900     PERFORM IMS-STATUSKONTROLL                                           
104000     .                                                                    
104100     EJECT                                                                
104200 IMS-GU-WDB601    SECTION.                                                
104300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
104400          DELIMITED BY SIZE INTO SSA1                                     
104500     MOVE '  GE' TO GODK-STATUSKODER                                      
104600     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
104700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
104800     PERFORM IMS-STATUSKONTROLL                                           
104900     IF SEGMENT-SAKNAS                                                    
105000         MOVE SPACE TO DCS-KDDC                                           
105100     END-IF                                                               
105200     .                                                                    
105300     EJECT                                                                
105310 IMS-GU-WDB601-FTG SECTION.                                               
105320     STRING 'WDB601  (IDFTG    =' WS-IDFTG-B6 ')'                         
105330            DELIMITED BY SIZE INTO SSA1                                   
105340     MOVE '  GE'                 TO GODK-STATUSKODER                      
105350     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
105360     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
105370     PERFORM IMS-STATUSKONTROLL                                           
105380     .                                                                    
105390     SKIP3                                                                
105400 IMS-STATUSKONTROLL SECTION.                                              
105500                                                                          
105600     SET STATUS-IX TO 1                                                   
105700     SEARCH GODK-STATUS                                                   
105800       AT END                                                             
105900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
106000         DELIMITED BY SIZE INTO FELTEXT                                   
106100         CALL FELLOG                                                      
106200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
106300         CONTINUE                                                         
106400     END-SEARCH                                                           
106500     .                                                                    
