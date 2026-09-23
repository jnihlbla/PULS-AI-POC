000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL010700.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   03/11/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAMN: CARPARTS.LDC.WL0107                                            
000800*    WEB-LDC: WL010700 PROGRAM IS A REPLICA OF W6030700 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        GODSMOTTAGNING HISTORIK LDC                                      
001300*                                                                         
001400*        PROGRAMMET LÄSER WLINLC (WDL6)                                   
001500*                         WLBENA (WDD3)                                   
001510*                         WDB623                                          
001600*                                                                         
001700*    INDATA.                                                              
001800*        TRANSAKTION: WL0107U                                             
001900*        REQUEST:     WZ01REQU                                            
002000*                     WL0107I1                                            
002100*                                                                         
002200*    UTDATA.                                                              
002300*        RESPONSE:    WZ01RESP                                            
002400*                     WL0107O1                                            
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800*    -- CHECKED BY WY2000                                                 
003900 77  IDPGM                       PIC X(08) VALUE 'WL010700'.              
004000 77  WS-ADRESS                   PIC X(50) VALUE                          
004100          'CARPARTS.LDC.GOODSRECEIVINGHISTORY'.                           
004200 77  WS-RESP-AREA                PIC S9(5) VALUE ZERO COMP-3.             
004210 77  FILLER                      PIC X(08) VALUE 'ERRTEXT:'.              
004300 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004400 77  KDRC-DISPLAY                PIC Z(5).                                
004500 77  JA                          PIC X     VALUE 'J'.                     
004600 77  NEJ                         PIC X     VALUE 'N'.                     
004700 77  INDX                        PIC S9(4) VALUE +0    COMP SYNC.         
004800 77  MAX-INDX                    PIC S9(4) VALUE +500  COMP SYNC.         
004900 77  WS-KVANT                    PIC S9(5) VALUE ZERO COMP-3.             
005000 77  WS-TIREGDAT                 PIC 9(6)  VALUE ZERO.                    
005100 77  WS-IDINLEV-REGDAT           PIC 9(6)  VALUE ZERO.                    
005200 77  WS-DAINLEV                  PIC 9(16) VALUE ZERO.                    
005300 77  WS-FLDC                     PIC X     VALUE SPACE.                   
005400 77  WS-IDSKYLT-CN               PIC X(3)  VALUE 'RCN'.                   
005500 77  WS-IDSKYLT-GB               PIC X(3)  VALUE 'GB '.                   
005600 77  WS-CP-UTF8                  PIC X(4)  VALUE 'UTF8'.                  
005700 77  WS-CP-278                   PIC X(3)  VALUE '278'.                   
005710 77  FOERSTA-GAANG               PIC X(1)    VALUE 'N'.                   
005720 77  W-NDEL-KVRAPP               PIC S9(7)   VALUE ZERO  COMP-3.          
005800                                                                          
005810 01  ALL-SPACE.                                                           
005820     03 FILLER                   PIC X(50)   VALUE SPACE.                 
005830                                                                          
005900*    --- VALID DC CODES                                                   
006000*01  -COPY WWDC99                                                         
006100     EJECT                                                                
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4) COMP VALUE +0.                 
006400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4) COMP VALUE +16.                
006500 77  RKOD-ABEND-MED-DUMP         PIC S9(4) COMP VALUE +1000.              
006600                                                                          
006700 77  NYCKLAR-SW                  PIC X     VALUE 'J'.                     
006800     88  NYCKLAR-OK                        VALUE 'J'.                     
006900     88  NYCKLAR-FEL                       VALUE 'N'.                     
007000                                                                          
007010 77  WS-IDTRACK                  PIC X     VALUE 'N'.                     
007020     88  IDTRACK-YES                       VALUE 'J'.                     
007030     88  IDTRACK-NO                        VALUE 'N'.                     
007040                                                                          
007100*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007200 01  GENERELLA-SUBPROGRAM.                                                
007300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007600     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007700     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
007800     EJECT                                                                
007900 01  MESSAGE-CODES.                                                       
008000     03  INVALID-KEY-FIELDS      PIC X(3)   VALUE '022'.                  
008100     03  IS-INVALID              PIC X(3)   VALUE '023'.                  
008200     03  TOO-MANY-LINES          PIC X(3)   VALUE '028'.                  
008300     03  SYSTEM-ERROR            PIC X(3)   VALUE '099'.                  
008400     03  KEYS-ARE-MISSING        PIC X(3)   VALUE '041'.                  
008500     03  LINES-NOT-FOUND         PIC X(3)   VALUE '027'.                  
008600     EJECT                                                                
008700     SKIP3                                                                
008800     EJECT                                                                
008900 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
009000*01  -COPY WTRAUTF8                                                       
009100     EJECT                                                                
009200                                                                          
009300 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009400     SKIP3                                                                
009500*01  -COPY WZ01SUB                                                        
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
009800 01  REQU-AREA.                                                           
009900*    03  -COPY WZ01REQU                                                   
010000*    03  -COPY WL0107I1                                                   
010100     EJECT                                                                
010200 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
010300 01  RESP-AREA.                                                           
010400*    03  -COPY WZ01RESP                                                   
010500*    03  -COPY WL0107O1                                                   
010600     EJECT                                                                
010700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
010900     SKIP3                                                                
011000 01  NYCKLAR-TILL-DLI.                                                    
011100     03  W-DAINLEV-MIN-X.                                                 
011200         05  W-DAINLEV-MIN       PIC 9(16)  VALUE ZERO.                   
011210     03  W-DAINLEV-X.                                                     
011220         05  W-DAINLEV           PIC 9(16)  VALUE ZERO.                   
011230                                                                          
011300     03  W-IDARTNR-X.                                                     
011400         05  W-IDARTNR           PIC S9(9)  VALUE ZERO COMP-3.            
011500     03  W-IDSKYLT-X.                                                     
011600         05  W-IDSKYLT           PIC X(3)   VALUE SPACE.                  
011610     03  W-IDDC-X.                                                        
011620         05  W-IDDC              PIC X(2)   VALUE SPACE.                  
011700     SKIP2                                                                
011800*    --- STATUS-KOD FRÅN IMS                                              
011900 01  STATUS-WS                   PIC XX.                                  
012000     88  SEGMENT-FINNS                       VALUE '  '.                  
012100     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012300     SKIP2                                                                
012400 01  GODK-STATUSKODER.                                                    
012500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012600     SKIP3                                                                
012700 01  SSA1                        PIC X(128).                              
012800 01  SSA2                        PIC X(128).                              
012810 01  SSA3                        PIC X(128).                              
012900     EJECT                                                                
013000*    --- IMS FUNKTIONSKODER                                               
013100*01  -COPY W0003                                                          
013200     EJECT                                                                
013300*    ---  DLI INPUT-OUTPUT AREA                                           
013400 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL601'.                      
013500 01  DLI-IO-WDL601.                                                       
013600*    03  -COPY WDL601  -PRE INLC-                                         
013700     EJECT                                                                
013800 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL611'.                      
013900 01  DLI-IO-WDL611.                                                       
014000*    03  -COPY WDL611  -PRE INLC-                                         
014100     EJECT                                                                
014110 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL621'.                      
014120 01  DLI-IO-WDL621.                                                       
014130*    03  -COPY WDL621                                                     
014140     EJECT                                                                
014150 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDL623'.                      
014160 01  DLI-IO-WDL623.                                                       
014170*    03  -COPY WDL623                                                     
014180     EJECT                                                                
014190 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB601'.                      
014191 01  DLI-IO-WDB601.                                                       
014192*    03  -COPY WDB601                                                     
014193     EJECT                                                                
014200 01  DLI-IO-AREA2.                                                        
014300     03  WLBENA11.                                                        
014400*        05  -COPY WDD311  -PRE BENA-                                     
014500     EJECT                                                                
014600 LINKAGE SECTION.                                                         
014700*01  -COPY W0009   -PRE MSG-                                              
014800     EJECT                                                                
014900*01  -COPY W0008   -PRE WDL6-                                             
015000     05  FILLER                  PIC X.                                   
015100     EJECT                                                                
015110*01  -COPY W0008   -PRE WDB6-                                             
015120     05  FILLER                  PIC X.                                   
015130     EJECT                                                                
015200*01  -COPY W0008   -PRE BENA-                                             
015300     05  FILLER                  PIC X.                                   
015400     EJECT                                                                
015500 PROCEDURE DIVISION  USING MSG-PCB WDL6-PCB WDB6-PCB                      
015600                           BENA-PCB.                                      
015700 MAIN SECTION.                                                            
015800     ENTRY 'DLITCBL' USING MSG-PCB WDL6-PCB WDB6-PCB                      
015900                           BENA-PCB.                                      
016000                                                                          
016100     PERFORM S01-HAEMTA-ANROPSDATA                                        
016200     IF SUB-KDRC = 0                                                      
016300        PERFORM A-INIT                                                    
016400        PERFORM B-KOLLA-NYCKLAR                                           
016500        IF NYCKLAR-OK                                                     
016600           PERFORM F-LAES-VISA-INFO                                       
016700        END-IF                                                            
016800        PERFORM S02-RETURNERA-SVAR                                        
016900     END-IF                                                               
017000                                                                          
017100     MOVE ZERO TO RETURN-CODE                                             
017200     GOBACK                                                               
017300     .                                                                    
017400     EJECT                                                                
017500 A-INIT SECTION.                                                          
017600                                                                          
017700     MOVE ALL '+' TO RESP-AREA                                            
017800     MOVE ZERO    TO RESP-KVRADER                                         
017900                     WS-KVANT                                             
018000     MOVE SPACE   TO RESP-IDMSG-ERROR                                     
018100                     RESP-IDMSG-INFO                                      
018200                     RESP-IDELMT-ERROR                                    
018300                                                                          
018400     MOVE '001'   TO RESP-IDMSGVER                                        
018500                                                                          
018600     MOVE ALL X'20' TO  RESP-BEART                                        
018700     .                                                                    
018800     EJECT                                                                
018900 B-KOLLA-NYCKLAR SECTION.                                                 
019000                                                                          
019100     MOVE JA TO NYCKLAR-SW                                                
019200                                                                          
019300***  KONTROLL AV REQU-KDPGMACT                                            
019400     IF REQU-KDPGMACT = 'S' OR 'U'                                        
019500        CONTINUE                                                          
019600     ELSE                                                                 
019700        MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                             
019800        MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                            
019900        MOVE NEJ TO NYCKLAR-SW                                            
020000     END-IF                                                               
020100                                                                          
020200***  KONTROLL AV REQU-IDARTNR                                             
020300     IF REQU-IDARTNR-KEY  = ALL '+'                                       
020400        MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                               
020500        MOVE NEJ TO NYCKLAR-SW                                            
020600     ELSE                                                                 
020700        INSPECT REQU-IDARTNR-KEY                                          
020800             REPLACING LEADING SPACE BY ZERO                              
020900        IF REQU-IDARTNR-KEY NUMERIC                                       
021000           MOVE REQU-IDARTNR-KEY TO W-IDARTNR                             
021100        ELSE                                                              
021200           MOVE 'IDARTNR' TO RESP-IDELMT-ERROR                            
021300           MOVE NEJ TO NYCKLAR-SW                                         
021400        END-IF                                                            
021500     END-IF                                                               
021600                                                                          
021700***  KONTROLL IDDC-KEY                                                    
021800     MOVE REQU-IDDC-KEY TO RESP-IDDC-KEY                                  
021900                           WS-IDDC                                        
021910                           W-IDDC                                         
022000                                                                          
022001     PERFORM IMS-GU-WDB601                                                
022002     MOVE DCS-FLTRACK TO RESP-FLTRACK                                     
022003     IF DCS-FLTRACK = 'J'                                                 
022004       MOVE 'J' TO WS-IDTRACK                                             
022005     END-IF                                                               
022006                                                                          
022100     MOVE REQU-IDARTNR-KEY TO RESP-IDARTNR-KEY                            
022200     INSPECT RESP-IDARTNR-KEY REPLACING LEADING ZERO BY SPACE             
022300                                                                          
022400     IF NYCKLAR-FEL                                                       
022500        IF RESP-IDELMT-ERROR = 'KDPGMACT'                                 
022600           MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                          
022700        ELSE                                                              
022800           MOVE INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                    
022900        END-IF                                                            
023000     END-IF                                                               
023100     .                                                                    
023200     EJECT                                                                
023300 F-LAES-VISA-INFO SECTION.                                                
023400                                                                          
023500     PERFORM FA-LAES-GRUNDDATA                                            
023600                                                                          
023700     IF SEGMENT-SAKNAS                                                    
023800        MOVE LINES-NOT-FOUND TO RESP-IDMSG-ERROR                          
023900     ELSE                                                                 
              MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                           
              IF DCS-UNICODE-IDSKYLT                                            
                 MOVE 'UTF8'             TO TRAUTF8-KDCP                        
              ELSE                                                              
                 MOVE '278 '             TO TRAUTF8-KDCP                        
              END-IF                                                            
024700        PERFORM IMS-GU-WLBENA11                                           
024800        IF SEGMENT-FINNS                                                  
024900           MOVE BENA-TEXT-BEART TO TRAUTF8-TECONV-FROM                    
025000        ELSE                                                              
025100           MOVE SPACE           TO TRAUTF8-TECONV-FROM                    
025200           MOVE WS-CP-278       TO TRAUTF8-KDCP                           
025300        END-IF                                                            
              IF TRAUTF8-TECONV-FROM = SPACES                                   
               MOVE 'GB'  TO W-IDSKYLT                                          
               MOVE '278' TO TRAUTF8-KDCP                                       
               PERFORM IMS-GU-WLBENA11                                          
               MOVE BENA-TEXT-BEART    TO TRAUTF8-TECONV-FROM                   
              END-IF                                                            
025400*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
025500        CALL WTRAUTF8 USING TRAUTF8-AREA                                  
025600                                                                          
025700*    -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                        
025800        MOVE TRAUTF8-TECONV-TO  TO RESP-BEART                             
025900                                                                          
026000        PERFORM FB-LAES-RADDATA                                           
026100                                                                          
026200        IF SEGMENT-FINNS                                                  
026210          MOVE INLC-INL-DAINLEV   TO W-DAINLEV                            
026300          IF REQU-IDDC-KEY = INLC-INL-IDDC                                
026400             MOVE 'J' TO WS-FLDC                                          
026500          ELSE                                                            
026600             MOVE 'N' TO WS-FLDC                                          
026700          END-IF                                                          
026800          IF WS-FLDC = 'J'                                                
026900             MOVE +1 TO INDX                                              
027000          ELSE                                                            
027100             MOVE +0 TO INDX                                              
027200          END-IF                                                          
027300        END-IF                                                            
027400                                                                          
027500        PERFORM UNTIL SEGMENT-SAKNAS                                      
027600          IF INDX <= MAX-INDX                                             
027700            IF WS-FLDC = 'J'                                              
027800               ADD +1 TO WS-KVANT                                         
027900               MOVE INLC-INL-IDPTYP  TO RESP-IDPTYP(INDX)                 
028000               MOVE INLC-INL-IDDC    TO RESP-IDDC(INDX)                   
028100               MOVE INLC-INL-IDLEVNR TO RESP-IDLEVNR(INDX)                
028200               MOVE INLC-INL-KDRT    TO RESP-KDRT(INDX)                   
028300                                                                          
028400               IF INLC-INL-KDRT = 0                                       
028500                 MOVE INLC-INL-IDKUNDNR TO RESP-IDKUNDNR(INDX)            
028600                 MOVE INLC-INL-IDKOLLI  TO RESP-IDKOLLI (INDX)            
028700               ELSE                                                       
028800                 MOVE INLC-INL-IDDISTR  TO RESP-IDLEVNR (INDX)            
028900                 MOVE INLC-INL-IDKUNDNR TO RESP-IDKUNDNR(INDX)            
029000                 MOVE ZERO              TO RESP-IDKOLLI (INDX)            
029100               END-IF                                                     
029200                                                                          
029300               MOVE ZERO             TO RESP-IDLOPNRM(INDX)               
029400               INSPECT RESP-IDLOPNRM(INDX)                                
029500                        REPLACING LEADING ZERO BY SPACE                   
029600               IF INLC-INL-IDPTYP = 'R30' OR '310'                        
029700                  MOVE INLC-INL-IDFAKT   TO RESP-IDLOPNRM(INDX)           
029800               END-IF                                                     
029900               IF INLC-INL-IDPTYP = 'R31'                                 
030000                  MOVE INLC-INL-IDLOPNRM TO RESP-IDLOPNRM(INDX)           
030100               END-IF                                                     
030200               IF INLC-INL-IDPTYP = 'R32' OR 'R33' OR 'R40'               
030300                  IF INLC-INL-IDLOPNRM = 0                                
030400                    MOVE INLC-INL-IDFAKT TO RESP-IDLOPNRM(INDX)           
030500                  ELSE                                                    
030600                    MOVE INLC-INL-IDLOPNRM TO RESP-IDLOPNRM(INDX)         
030700                  END-IF                                                  
030800               END-IF                                                     
030900                                                                          
031000               IF INLC-INL-IDPTYP = 'R34'                                 
031100                  IF INLC-INL-IDLOPNRM NUMERIC                            
031200                    MOVE INLC-INL-IDLOPNRM TO RESP-IDLOPNRM(INDX)         
031300                  END-IF                                                  
031400               END-IF                                                     
031500                                                                          
031600               MOVE INLC-INL-IDKUNDRF    TO RESP-IDKUNDRF(INDX)           
031610                                                                          
031700               IF INLC-INL-TIAVIDAT > 0                                   
031720                 MOVE INLC-INL-TIAVIDAT  TO RESP-TIREGDAT(INDX)           
031730               ELSE                                                       
031800                 MOVE INLC-INL-DAINLEV   TO WS-DAINLEV                    
031900                 MOVE WS-DAINLEV (3:6)   TO WS-IDINLEV-REGDAT             
032000                 COMPUTE WS-TIREGDAT = 999999 - WS-IDINLEV-REGDAT         
032100                 MOVE WS-TIREGDAT        TO RESP-TIREGDAT(INDX)           
032110               END-IF                                                     
032200                                                                          
032210               MOVE INLC-INL-KVRETUR     TO RESP-KVRETUR(INDX)            
032220               MOVE INLC-INL-KDAVVANT    TO RESP-KDAVVANT(INDX)           
032300               MOVE INLC-INL-TIINLINL    TO RESP-TIINLINL(INDX)           
032400               MOVE INLC-INL-KVAVIS      TO RESP-KVAVIS(INDX)             
032500               MOVE INLC-INL-KVANTMOT    TO RESP-KVANTMOT(INDX)           
032600               MOVE INLC-INL-KVART-SKROT TO RESP-KVART-SKROT(INDX)        
032620               MOVE INLC-INL-KVTULRET    TO RESP-KVTULRET(INDX)           
032700                                                                          
032800               IF INLC-INL-FLTULLST = 'J'                                 
032900                 MOVE 'Y'                TO RESP-FLTULLST(INDX)           
033000               ELSE                                                       
033100                 MOVE INLC-INL-FLTULLST  TO RESP-FLTULLST(INDX)           
033200               END-IF                                                     
033300                                                                          
033301               IF INLC-INL-FLMAKUL = 'J'                                  
033302                 MOVE 'Y'                TO RESP-FLMAKUL(INDX)            
033303               ELSE                                                       
033304                 MOVE INLC-INL-FLMAKUL   TO RESP-FLMAKUL(INDX)            
033305               END-IF                                                     
033306                                                                          
033310               MOVE INLC-INL-IDUSER-003  TO RESP-IDUSER-003(INDX)         
033320*                                                                         
033330***       READ WDL623 SEGMENTS FOR IDTRACK                                
033340               IF IDTRACK-YES                                             
033350                 PERFORM IMS-GU-WDL623                                    
033360                 IF SEGMENT-FINNS                                         
033370                    MOVE TINL-IDTRACK TO RESP-IDTRACK(INDX)               
033380                 ELSE                                                     
033390                    MOVE SPACES       TO RESP-IDTRACK(INDX)               
033391                 END-IF                                                   
033392               ELSE                                                       
033393                 MOVE SPACES       TO RESP-IDTRACK(INDX)                  
033394               END-IF                                                     
033395*                                                                         
033400             END-IF                                                       
033500          END-IF                                                          
033520***       READ WDL621 SEGMENTS FOR P32                                    
033521          IF WS-FLDC = 'J'                                                
033530            PERFORM FC-GET-P32-SEGMENTS                                   
033531          END-IF                                                          
033540                                                                          
033550***       CONTINUE WITH WDL611 READING                                    
033560          IF INDX > MAX-INDX                                              
033570            CONTINUE                                                      
033580          ELSE                                                            
033600            PERFORM FB-LAES-RADDATA                                       
033700            IF SEGMENT-FINNS                                              
033710               MOVE INLC-INL-DAINLEV     TO W-DAINLEV                     
033800               IF REQU-IDDC-KEY = INLC-INL-IDDC                           
033900                  MOVE 'J' TO WS-FLDC                                     
034000               ELSE                                                       
034100                  MOVE 'N' TO WS-FLDC                                     
034200               END-IF                                                     
034300            END-IF                                                        
034400            IF WS-FLDC = 'J' OR SEGMENT-SAKNAS                            
034500               ADD 1 TO INDX                                              
034600            END-IF                                                        
034700            IF SEGMENT-FINNS AND INDX > MAX-INDX                          
034800               IF WS-FLDC = 'J'                                           
034900                  ADD +1 TO WS-KVANT                                      
035000               END-IF                                                     
035100            END-IF                                                        
035110          END-IF                                                          
035200        END-PERFORM                                                       
035300                                                                          
035400        MOVE WS-KVANT TO RESP-KVRADER                                     
035500        IF WS-KVANT > MAX-INDX                                            
                 MOVE 500 TO RESP-KVRADER                                       
035600           MOVE TOO-MANY-LINES TO RESP-IDMSG-ERROR                        
035700        END-IF                                                            
035800                                                                          
035900     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200 FA-LAES-GRUNDDATA SECTION.                                               
036300                                                                          
036400     PERFORM IMS-GU-WDL601                                                
036500     .                                                                    
036600     EJECT                                                                
036700 FB-LAES-RADDATA SECTION.                                                 
036800                                                                          
036900     PERFORM IMS-GNP-WDL611                                               
037000     .                                                                    
037100     EJECT                                                                
037110 FC-GET-P32-SEGMENTS  SECTION.                                            
037120     MOVE JA   TO FOERSTA-GAANG                                           
037130     PERFORM IMS-GNP-WDL621                                               
037140     IF SEGMENT-FINNS                                                     
037150       ADD +1 TO INDX                                                     
037150       ADD +1 TO WS-KVANT                                                 
037160     END-IF                                                               
037170*                                                                         
037180     PERFORM UNTIL NOT (SEGMENT-FINNS AND INDX <= MAX-INDX)               
037190       IF FOERSTA-GAANG = JA                                              
037191         MOVE ZERO           TO W-NDEL-KVRAPP                             
037192         PERFORM FCA-FLYTTA-SPAR                                          
037194         MOVE NEJ            TO FOERSTA-GAANG                             
037195       END-IF                                                             
037196       COMPUTE W-NDEL-KVRAPP = W-NDEL-KVRAPP + NDEL-KVRAPP                
037197       MOVE W-NDEL-KVRAPP    TO RESP-KVANTMOT(INDX)                       
037198       PERFORM IMS-GNP-WDL621                                             
037199     END-PERFORM                                                          
037200*                                                                         
037201**   IF SEGMENT-FINNS                                                     
037202**     MOVE W-DAINLEV (2:15) TO SPAR-DAINLEV-NEXT                         
037203**   END-IF                                                               
037204     .                                                                    
037205     EJECT                                                                
037206 FCA-FLYTTA-SPAR SECTION.                                                 
037207     SKIP2                                                                
037208     MOVE 'P32'             TO RESP-IDPTYP    (INDX)                      
037209     MOVE NDEL-TIREGDAT     TO RESP-TIINLINL  (INDX)                      
037210     MOVE ALL-SPACE         TO RESP-IDLOPNRM  (INDX)                      
037211                               RESP-IDDC      (INDX)                      
037212                               RESP-IDLEVNR   (INDX)                      
037213                               RESP-KDRT      (INDX)                      
037214                               RESP-IDKUNDRF  (INDX)                      
037215                               RESP-TIREGDAT  (INDX)                      
037216                               RESP-KVANTMOT  (INDX)                      
037217                               RESP-KVART-SKROT(INDX)                     
037218                               RESP-FLMAKUL   (INDX)                      
037219                               RESP-KVAVIS    (INDX)                      
037220                               RESP-IDUSER-003(INDX)                      
037221                                                                          
037222     .                                                                    
037223     EJECT                                                                
037230 S01-HAEMTA-ANROPSDATA SECTION.                                           
037300                                                                          
037400     MOVE 'GETARG'               TO SUB-KDFUNC                            
037500     MOVE WS-ADRESS              TO SUB-ADDISPABS                         
037600     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
037700                                                                          
037800     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
037900                                                                          
038000     IF SUB-KDRC > 0                                                      
038100       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
038200       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
038300       DELIMITED BY SIZE INTO FELTEXT                                     
038400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
038500     END-IF                                                               
038600     .                                                                    
038700     SKIP3                                                                
038800 S02-RETURNERA-SVAR SECTION.                                              
038900                                                                          
039000     COMPUTE WS-RESP-AREA = LENGTH OF RESP-AREA                           
039100       - LENGTH OF RESP-TABELLRAD * (500 - WS-KVANT)                      
039200                                                                          
039300     MOVE 'RETURN'                   TO SUB-KDFUNC                        
039400     MOVE WS-RESP-AREA               TO SUB-KVDLEN                        
039500                                                                          
039600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
039700                                                                          
039800     IF SUB-KDRC > 0                                                      
039900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
040000       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
040100       DELIMITED BY SIZE INTO FELTEXT                                     
040200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
040300     END-IF                                                               
040400     .                                                                    
040500     EJECT                                                                
040600* --- IMS SEKTIONER ---                                                   
040700 IMS-GU-WDL601   SECTION.                                                 
040800     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
040900          DELIMITED BY SIZE INTO SSA1                                     
041000     MOVE '  GE'           TO GODK-STATUSKODER                            
041100     CALL CBLTDLI          USING GU WDL6-PCB DLI-IO-WDL601 SSA1           
041200     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
041300     PERFORM IMS-STATUSKONTROLL                                           
041400     .                                                                    
041500     SKIP3                                                                
041600 IMS-GNP-WDL611   SECTION.                                                
041700     STRING 'WDL611  (DAINLEV =>' W-DAINLEV-MIN-X ')'                     
041800          DELIMITED BY SIZE INTO SSA1                                     
041900     MOVE '  GE'           TO GODK-STATUSKODER                            
042000     CALL CBLTDLI          USING GNP WDL6-PCB DLI-IO-WDL611   SSA1        
042100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
042200     PERFORM IMS-STATUSKONTROLL                                           
042300     .                                                                    
042400     EJECT                                                                
042410 IMS-GNP-WDL621   SECTION.                                                
042420     STRING 'WDL611  (DAINLEV = ' W-DAINLEV-X ')'                         
042430          DELIMITED BY SIZE INTO SSA1                                     
042440     MOVE 'WDL621  '       TO SSA2                                        
042450     MOVE '  GE'           TO GODK-STATUSKODER                            
042460     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL621 SSA1 SSA2              
042470     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
042480     PERFORM IMS-STATUSKONTROLL                                           
042490     .                                                                    
042491     EJECT                                                                
042492 IMS-GU-WDL623 SECTION.                                                   
042493                                                                          
042494     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
042495          DELIMITED BY SIZE INTO SSA1                                     
042496     STRING 'WDL611  (DAINLEV = ' W-DAINLEV-X ')'                         
042497          DELIMITED BY SIZE INTO SSA2                                     
042500     MOVE 'WDL623  '       TO SSA3                                        
042501     MOVE '  GE'            TO GODK-STATUSKODER                           
042502     CALL CBLTDLI     USING GNP WDL6-PCB DLI-IO-WDL623                    
042503                               SSA1 SSA2 SSA3                             
042504     MOVE WDL6-STATUS-CODE  TO STATUS-WS                                  
042505     PERFORM IMS-STATUSKONTROLL                                           
042506     .                                                                    
042507     EJECT                                                                
042508 IMS-GU-WDB601 SECTION.                                                   
042509     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
042510          DELIMITED BY SIZE INTO SSA1                                     
042511     MOVE '  ' TO GODK-STATUSKODER                                        
042512     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601  SSA1                   
042513     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
042514     PERFORM IMS-STATUSKONTROLL                                           
042518     .                                                                    
042519     EJECT                                                                
042520                                                                          
042530 IMS-GU-WLBENA11 SECTION.                                                 
042600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
042700          DELIMITED BY SIZE INTO SSA1                                     
042800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
042900          DELIMITED BY SIZE INTO SSA2                                     
043000     MOVE '  GE' TO GODK-STATUSKODER                                      
043100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA2 SSA1 SSA2                
043200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
043300     PERFORM IMS-STATUSKONTROLL                                           
043400     .                                                                    
043500     SKIP3                                                                
043600 IMS-STATUSKONTROLL SECTION.                                              
043700     SET STATUS-IX TO 1                                                   
043800     SEARCH GODK-STATUS                                                   
043900       AT END                                                             
044000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
044100         DELIMITED BY SIZE INTO FELTEXT                                   
044200         CALL FELLOG                                                      
044300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
044400         CONTINUE                                                         
044500     END-SEARCH                                                           
044600     .                                                                    
