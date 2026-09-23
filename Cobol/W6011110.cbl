000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W6011110.                                                
000400*AUTHOR.         LARS THELL.                                              
000500*DATE-WRITTEN.   92/02/17.                                                
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*W6011110 FOR BUSINESS LOGIC                                              
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        SKAPAR R31 TRANSAKTIONER                                         
001300*        EN LEVERANTÖR EN AVI FLERA ARTIKLAR                              
001400*                                                                         
001500*    SUB PROGRAMMET W611REG UPPDATERAR W6INLA (W6D1)                      
001600*                           LÄSER      WLLEVA (WDF1)                      
001700*                                                                         
001800*                           LÄSER      WLBENA (WDD3)                      
001900*                           LÄSER      WLINLB (WDD9)                      
002000*                           LÄSER      WDB6                               
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: W6W111T                                             
002400*        MID:         W60111I1                                            
002500*                     WZ01REQU                                            
002600*                                                                         
002700*    UTDATA.                                                              
002800*        MOD:         W60111O1                                            
002900*                     WZ01RESP                                            
003000*                                                                         
003100     SKIP3                                                                
003200 ENVIRONMENT DIVISION.                                                    
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500 WORKING-STORAGE SECTION.                                                 
003600                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W6011110'.            
003900                                                                          
004000*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
004100*                                                                         
004200 77  FILLER                      PIC X(08)   VALUE 'CURRENT:'.            
004300 77  WS-CURRENT-SECTION          PIC X(32)   VALUE SPACE.                 
004400 77  FILLER                      PIC X(08)   VALUE 'IMS-SEC:'.            
004500 77  WS-CURRENT-IMS-SECTION      PIC X(24)   VALUE SPACE.                 
004600*                                                                         
004700 77  FILLER                      PIC X(08) VALUE 'ERRORTEX'.              
004800 77  ERROR-TEXT                  PIC X(40) VALUE SPACE.                   
004900 77  KDRC-DISPLAY                PIC Z(5).                                
005000                                                                          
005100 77  JA                          PIC X       VALUE 'J'.                   
005200 77  YES                         PIC X       VALUE 'Y'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400                                                                          
005500*    --- WORK FIELD FOR MOVING SPACE TO ANY KIND OF FIELD                 
005600 01  W-SPACE.                                                             
005700     03 FILLER                   PIC X(50)   VALUE SPACE.                 
005800                                                                          
005900*    --- WORK FIELD FOR MOVING ALL + TO ANY KIND OF FIELD                 
006000 01  W-PLUS.                                                              
006100     03 FILLER                   PIC X(50)   VALUE                        
006200        '++++++++++++++++++++++++++++++++++++++++++++++++++'.             
006300                                                                          
006400 01  FILLER                      PIC X(16)   VALUE 'INDEX FÄLT  '.        
006500                                                                          
006600 77  RAD-IX                      PIC S9(3)  VALUE +0    COMP SYNC.        
006700 77  RAD-IX2                     PIC S9(9)  VALUE +0    COMP SYNC.        
006800 77  INDX                        PIC S9(3)  VALUE +0    COMP SYNC.        
006900                                                                          
007000 77  INDX-DISPLAY                PIC  9(3)  VALUE  0.                     
007100                                                                          
007200*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
007300 01  FILLER                      PIC X(16)   VALUE 'ARBETSFÄLT  '.        
007400                                                                          
007500 77  WS-REQU-IDLEVNR             PIC X(5)    VALUE SPACE.                 
007600 77  WS-REQU-KDRT                PIC 9(2)    VALUE ZERO.                  
007700 77  WS-REQU-IDFS                PIC X(8)    VALUE SPACE.                 
007800 77  WS-REQU-TIAVIDAT            PIC 9(6)    VALUE ZERO.                  
007900 77  WS-REQU-ADINLOMR-PRT        PIC X(4)    VALUE SPACE.                 
008000 77  WS-REQU-IDLBBET             PIC X(12)   VALUE SPACE.                 
008100 77  WS-REQU-FLGODK              PIC X(1)    VALUE SPACE.                 
008200 77  FL-IDFS-OK                  PIC X       VALUE 'N'.                   
008300                                                                          
008400 77  WS-REG1-IDARTNR             PIC 9(8)    VALUE ZERO.                  
008500 77  WS-REG1-KVAVIS              PIC 9(6)    VALUE ZERO.                  
008600                                                                          
008700 01  FILLER                      PIC X(16)   VALUE 'SWITCHAR    '.        
008800                                                                          
008900 77  INDATA-SW                   PIC X       VALUE 'J'.                   
009000     88  INDATA-OK                           VALUE 'J'.                   
009100     88  INDATA-FEL                          VALUE 'N'.                   
009200                                                                          
009300 77  INDATA-IFYLLT-SW            PIC X       VALUE 'N'.                   
009400     88  INDATA-IFYLLT                       VALUE 'J'.                   
009500     88  INDATA-EJ-IFYLLT                    VALUE 'N'.                   
009600                                                                          
009700 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
009800     88  NYCKLAR-OK                          VALUE 'J'.                   
009900     88  NYCKLAR-FEL                         VALUE 'N'.                   
010000                                                                          
010100 77  ALLT-SW                     PIC X       VALUE 'J'.                   
010200     88  ALLT-OK                             VALUE 'J'.                   
010300                                                                          
010400 77  W611REG-SW                  PIC X       VALUE 'J'.                   
010500     88  W611REG-OK                          VALUE 'J'.                   
010600 01  FILLER                      PIC X(16)   VALUE 'GODK TRANSAR'.        
010700                                                                          
010800 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
010900     88  EGEN-MID                            VALUE '6111'.                
011000     88  GODK-MID                            VALUE '6111' '6112'          
011100                                                   '6113' '6114'          
011200                                                   '6115' '6116'          
011300                                                   '6118' '6119'.         
011400     88  HELP-MID                            VALUE '0551'.                
011500     EJECT                                                                
011600 01  FILLER                      PIC X(16)   VALUE 'PARM SUBPGM '.        
011700*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
011800 01  GENERELLA-SUBPROGRAM.                                                
011900*    03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
012000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012200     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012300     03  W611REG                 PIC X(8)    VALUE 'W611REG'.             
012400     EJECT                                                                
012500*    --- PARAMETERS TO ABEND                                              
012600                                                                          
012700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013000     SKIP3                                                                
013100*    --- PARAMETRAR TILL SUBPROGRAM WMEDKONV                              
013200*01 -KOPY WMEDAREA                                                        
013300     SKIP3                                                                
013400 01  RESPONSE-CODES.                                                      
013500     03  R-INF-UPDATE-DONE        PIC X(3)    VALUE '001'.                
013600     03  R-ERR-UPDATE-NOT-ALLOWED PIC X(3)    VALUE '007'.                
013700     03  R-CORR-HILITE-FLDS       PIC X(3)    VALUE '020'.                
013800     03  R-ERR-WRONG-KEY          PIC X(3)    VALUE '022'.                
013900     03  R-MUST-BE-NUMERIC        PIC X(3)    VALUE '024'.                
014000     03  R-ERR-NOT-FOUND          PIC X(3)    VALUE '025'.                
014100     03  PF11-AND-NO-DATA         PIC X(3)    VALUE '014'.                
014200     03  R-ERR-WRONG-PRINTER      PIC X(3)    VALUE '347'.                
014300     03  R-PRICE-INFO-MISSING     PIC X(3)    VALUE '260'.                
014310     03  R-PRICE-WRNG-CURRENCY    PIC X(3)    VALUE '426'.                
014400     03  R-ERR-KEYS-MISSING       PIC X(3)    VALUE '348'.                
014500     03  R-INF-ARTIKEL-ERSATT     PIC X(3)    VALUE '223'.                
014600     03  R-INF-AVROP-SAKNAS       PIC X(3)    VALUE '336'.                
014700     03  R-ERR-IN-LINE-ONE        PIC X(3)    VALUE '337'.                
014800     03  R-INF-UPDATE-NOT-DONE    PIC X(3)    VALUE '004'.                
014900     03  R-INF-AVISERING-FINNS    PIC X(3)    VALUE '030'.                
015000     03  R-ERR-SUPPL-MISSING      PIC X(3)    VALUE '025'.                
015200     03  R-INF-PART-OTHER-COMPANY PIC X(3)    VALUE '360'.                
015300     03  R-ERR-DIRECT-SUPPLIER    PIC X(3)    VALUE '349'.                
015400                                                                          
015500 01  REG1-MSG-CODES.                                                      
015600     03  REG1-INF-PART-MISSING    PIC X(3)    VALUE '017'.                
015700     03  REG1-INF-PART-OTHER-COMPANY PIC X(3) VALUE '088'.                
015800     03  REG1-INF-AVISERING-FINNS PIC X(3)    VALUE '201'.                
015900     03  REG1-INF-PART-REPLACED   PIC X(3)    VALUE '220'.                
016000     03  REG1-INF-ADVICE-MISSING  PIC X(3)    VALUE '285'.                
016100     03  REG1-INF-PRICE-MISSING   PIC X(3)    VALUE '301'.                
016110     03  REG1-INF-WRNG-CURRENCY   PIC X(3)    VALUE '151'.                
016200     03  REG1-INF-DIRECT-SUPPLIER PIC X(3)    VALUE '306'.                
016300     EJECT                                                                
016400                                                                          
016500 01  FILLER                  PIC X(17)   VALUE 'LÄNK-W611REGO'.           
016600                                                                          
016700*01  -COPY W611REG0                                                       
016800*01  FILLER -COPY W611REG1    -RED LAENK-W611REG0                         
016900     EJECT                                                                
017000*                                                                         
017100 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
017200     SKIP3                                                                
017300*01  -COPY WMFSAREA                                                       
017400     EJECT                                                                
017500*                                                                         
017600*    --- AREOR TILL GEMENSAMMA SUBPROGRAM                                 
017700*                                                                         
017800 01  FILLER                      PIC X(16)  VALUE 'LÄNKAREOR'.            
017900*                                                                         
018000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018100*                                                                         
018200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018300*    --- STATUS-KOD FRÅN IMS                                              
018400 01  KEYS-TO-DLI.                                                         
018500     03  W-IDDC-B6-X.                                                     
018600         05 W-IDDC-B6                  PIC X(2).                          
018700     03  W-IDDC-B6-LEV-X.                                                 
018800         05 W-IDDC-B6-LEV              PIC X(5).                          
018900                                                                          
019000                                                                          
019100 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
019200 01   DLI-IO-AREA-B601.                                                   
019300*     03  -COPY WDB601                                                    
019400                                                                          
019500 01  FILLER               PIC X(16)   VALUE 'WDB601 LEV'.                 
019600 01   DLI-IO-AREA-B601-LEV.                                               
019700*     03  -COPY WDB601   -PRE LEV-                                        
019800                                                                          
019900 01  SSA1                        PIC X(256).                              
020000                                                                          
020100 01  STATUS-WS                   PIC XX.                                  
020200     88 SEGMENT-FINNS                        VALUE '  '.                  
020300     88 SEGMENT-SAKNAS                       VALUE 'GE'.                  
020400     88 BASEN-SLUT                           VALUE 'GB'.                  
020500                                                                          
020600 01  GODK-STATUSKODER.                                                    
020700     03 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                 
020800                                                                          
020900     EJECT                                                                
021000*    --- IMS FUNKTIONSKODER                                               
021100*01  -COPY W0003                                                          
021200     EJECT                                                                
021300 LINKAGE SECTION.                                                         
021400 01  FILLER                      PIC X(16)  VALUE 'REQU-AREA'.            
021500*                                                                         
021600 01  MAX-KVRADER                 PIC S9(4)  COMP.                         
021700*                                                                         
021800 01  REQU-AREA.                                                           
021900*    03 -COPY WZ01REQU                                                    
022000*    03 -COPY W60111I1                                                    
022100*                                                                         
022200 01  FILLER                      PIC X(16)  VALUE 'RESP-AREA'.            
022300*                                                                         
022400 01  RESP-AREA.                                                           
022500*    03 -COPY WZ01RESP                                                    
022600*    03 -COPY W60111O1                                                    
022700                                                                          
022800 01 -COPY W0009     -PRE MSG-                                             
022900                                                                          
023000*01  -COPY W0008   -PRE WDB6-LEV-                                         
023100     05  FILLER                  PIC X.                                   
023200                                                                          
023300*01  -COPY W0008   -PRE WDB6-                                             
023400     05  FILLER                  PIC X.                                   
023500                                                                          
023600*   PCB'ER FÖR SUB PGM W611REG                                            
023700                                                                          
023800 01  REG-INLA1-PCB               PIC X.                                   
023900                                                                          
024000 01  REG-INLA2-PCB               PIC X.                                   
024100                                                                          
024200 01  REG-INLA3-PCB               PIC X.                                   
024300                                                                          
024400 01  REG-LEVA-PCB                PIC X.                                   
024500                                                                          
024600 01  REG-ARTC-PCB                PIC X.                                   
024700                                                                          
024800 01  REG-BENA-PCB                PIC X.                                   
024900                                                                          
025000 01  REG-INLB-PCB                PIC X.                                   
025100                                                                          
025200 01  REG-WDK7-PCB                PIC X.                                   
025210                                                                          
025220 01  REG-WDB6-PCB                PIC X.                                   
025230                                                                          
025240*01    -COPY W0008     -PRE 9305-                                         
025250     05  FILLER                  PIC X(30).                               
025260                                                                          
025270     EJECT                                                                
025400 PROCEDURE DIVISION  USING REQU-AREA     RESP-AREA  MAX-KVRADER           
025500                           MSG-PCB                                        
025600                           WDB6-LEV-PCB  WDB6-PCB                         
025700                           REG-INLA1-PCB REG-INLA2-PCB                    
025800                           REG-INLA3-PCB REG-LEVA-PCB                     
025900                           REG-ARTC-PCB  REG-BENA-PCB                     
026000                           REG-INLB-PCB  REG-WDK7-PCB                     
026010                           REG-WDB6-PCB  9305-PCB.                        
026100 MAIN SECTION.                                                            
026200                                                                          
026300     PERFORM A-INIT                                                       
026400     PERFORM B-INSPECT-NYCKLAR                                            
026500     IF NYCKLAR-OK                                                        
026600                                                                          
026700       PERFORM G-INSPECT-INPUT-UPDATE                                     
026800     END-IF                                                               
026900                                                                          
027000     MOVE ZERO                  TO RETURN-CODE                            
027100     GOBACK                                                               
027200     .                                                                    
027300     EJECT                                                                
027400                                                                          
027500 A-INIT          SECTION.                                                 
027600                                                                          
027700     MOVE SPACE                TO REG1-IDTRANS                            
027800                                  REG1-IDFS                               
027900                                  REG1-IDLBBET                            
028000     MOVE JA                   TO REG1-IDLEVNR-OK                         
028100                                  REG1-KDRT-OK                            
028200                                  REG1-IDFS-OK                            
028300                                  REG1-TIAVIDAT-OK                        
028400     MOVE NEJ                  TO REG1-FLGODK-IDFS                        
028500                                  REG1-FLGODK-IDARTNR                     
028600     MOVE SPACE                TO REG1-IDLEVNR                            
028700     MOVE ZERO                 TO REG1-KDRT                               
028800                                  REG1-TIAVIDAT                           
028900                                                                          
029000     MOVE +1                   TO RAD-IX                                  
029100     PERFORM UNTIL RAD-IX       > MAX-KVRADER                             
029200         MOVE JA               TO REG1-IDARTNR-OK (RAD-IX)                
029300         MOVE ZERO             TO REG1-IDARTNR    (RAD-IX)                
029400                                  REG1-KVAVIS     (RAD-IX)                
029500         MOVE SPACE            TO REG1-IDMFSFEL   (RAD-IX)                
029600         ADD +1                TO RAD-IX                                  
029700     END-PERFORM                                                          
029800                                                                          
029900     MOVE '001'                TO RESP-IDMSGVER                           
030000                                                                          
030100     MOVE JA                   TO W611REG-SW                              
030200     MOVE NEJ                  TO INDATA-IFYLLT-SW                        
030300                                                                          
030400     MOVE SPACE                TO RESP-IDMSG-ERROR                        
030500     MOVE SPACE                TO RESP-IDMSG-INFO                         
030600     .                                                                    
030700     EJECT                                                                
030800                                                                          
030900 B-INSPECT-NYCKLAR SECTION.                                               
031000     MOVE 'B-INSPECT-NYCKLAR        ' TO WS-CURRENT-SECTION               
031100                                                                          
031200     MOVE JA TO NYCKLAR-SW                                                
031300                                                                          
031400     PERFORM BA-INSPECT-IDLEVNR                                           
031500     PERFORM BB-INSPECT-KDRT                                              
031600     PERFORM BC-INSPECT-IDFS                                              
031700     PERFORM BD-INSPECT-TIAVIDAT                                          
031800     PERFORM BE-INSPECT-IDLBBET                                           
031900     PERFORM BF-INSPECT-FLGODK                                            
032000     PERFORM BG-INSPECT-IDDC                                              
032100     PERFORM BJ-INSPECT-SDC-NDC-LEV                                       
032200                                                                          
032300     IF REQU-ADINLOMR-PRT-KEY NOT = ALL '+' OR SPACE                      
032400         MOVE REQU-ADINLOMR-PRT-KEY TO WS-REQU-ADINLOMR-PRT               
032500     END-IF                                                               
032600                                                                          
032700     IF NYCKLAR-FEL                                                       
032800       MOVE R-ERR-WRONG-KEY      TO RESP-IDMSG-ERROR                      
032900       PERFORM RESP-ROER-EJ-FAELT-IN                                      
033000     END-IF                                                               
033100                                                                          
033200     .                                                                    
033300     EJECT                                                                
033400 BA-INSPECT-IDLEVNR SECTION.                                              
033500     MOVE 'BA-INSPECT-IDLEVNR       ' TO WS-CURRENT-SECTION               
033600                                                                          
033700     IF REQU-IDLEVNR-KEY       = ALL '+' OR SPACE                         
033800         MOVE ZERO             TO WS-REQU-IDLEVNR                         
033900     ELSE                                                                 
034000         MOVE REQU-IDLEVNR-KEY TO WS-REQU-IDLEVNR                         
034100     END-IF                                                               
034200                                                                          
034300     IF WS-REQU-IDLEVNR = SPACE                                           
034400       MOVE 'IDLEVNR'          TO RESP-IDELMT-ERROR                       
034500       MOVE NEJ                TO NYCKLAR-SW                              
034600     END-IF                                                               
034700     .                                                                    
034800     EJECT                                                                
034900 BB-INSPECT-KDRT  SECTION.                                                
035000                                                                          
035100     IF REQU-KDRT-KEY          = ALL '+' OR SPACE                         
035200        MOVE ZERO              TO WS-REQU-KDRT                            
035300     ELSE                                                                 
035400        MOVE REQU-KDRT-KEY     TO WS-REQU-KDRT                            
035500        INSPECT WS-REQU-KDRT REPLACING LEADING SPACE BY ZERO              
035600     END-IF                                                               
035700                                                                          
035800     IF WS-REQU-KDRT NUMERIC AND WS-REQU-KDRT NOT = 99                    
035900         CONTINUE                                                         
036000     ELSE                                                                 
036100       MOVE 'KDRT'            TO RESP-IDELMT-ERROR                        
036200       MOVE NEJ                TO NYCKLAR-SW                              
036300     END-IF                                                               
036400     .                                                                    
036500     EJECT                                                                
036600 BC-INSPECT-IDFS  SECTION.                                                
036700                                                                          
036800                                                                          
036900     IF REQU-IDFS-KEY     NOT  = ALL '+' OR SPACE                         
037000       MOVE REQU-IDFS-KEY    TO WS-REQU-IDFS                              
037100     END-IF                                                               
037200                                                                          
037300     MOVE 1   TO INDX                                                     
037400     MOVE NEJ TO FL-IDFS-OK                                               
037500     PERFORM UNTIL INDX > 8 OR FL-IDFS-OK = JA                            
037600       IF WS-REQU-IDFS(INDX:1) NUMERIC                                    
037700         IF WS-REQU-IDFS(INDX:1) > ZERO                                   
037800           MOVE JA TO FL-IDFS-OK                                          
037900         END-IF                                                           
038000       END-IF                                                             
038100       ADD 1 TO INDX                                                      
038200     END-PERFORM                                                          
038300                                                                          
038400     IF FL-IDFS-OK = NEJ                                                  
038500       MOVE 'IDFS'                TO RESP-IDELMT-ERROR                    
038600       MOVE NEJ                   TO NYCKLAR-SW                           
038700     END-IF                                                               
038800     .                                                                    
038900     EJECT                                                                
039000 BD-INSPECT-TIAVIDAT SECTION.                                             
039100                                                                          
039200                                                                          
039300     IF REQU-TIAVIDAT-KEY NOT   = ALL '+' OR SPACE                        
039400       MOVE REQU-TIAVIDAT-KEY   TO WS-REQU-TIAVIDAT                       
039500       INSPECT WS-REQU-TIAVIDAT REPLACING LEADING SPACE BY ZERO           
039600     END-IF                                                               
039700                                                                          
039800     IF WS-REQU-TIAVIDAT NUMERIC AND WS-REQU-TIAVIDAT > ZERO              
039900       CONTINUE                                                           
040000     ELSE                                                                 
040100       MOVE 'TIAVIDAT'          TO RESP-IDELMT-ERROR                      
040200       MOVE NEJ                 TO NYCKLAR-SW                             
040300     END-IF                                                               
040400     .                                                                    
040500     EJECT                                                                
040600 BE-INSPECT-IDLBBET SECTION.                                              
040700                                                                          
040800     IF REQU-IDLBBET = ALL '+' OR SPACE                                   
040900       MOVE SPACE               TO WS-REQU-IDLBBET                        
041000     ELSE                                                                 
041100       MOVE REQU-IDLBBET        TO WS-REQU-IDLBBET                        
041200     END-IF                                                               
041300     .                                                                    
041400     EJECT                                                                
041500 BF-INSPECT-FLGODK SECTION.                                               
041600                                                                          
041700     IF REQU-FLGODK         = SPACE OR ZERO                               
041800       MOVE SPACE               TO WS-REQU-FLGODK                         
041900     ELSE                                                                 
042000       MOVE REQU-FLGODK         TO WS-REQU-FLGODK                         
042100     END-IF                                                               
042200                                                                          
042300     IF WS-REQU-FLGODK  = SPACE  OR  JA  OR  NEJ  OR  YES                 
042400       MOVE WS-REQU-FLGODK      TO REG1-FLGODK                            
042500       IF REG1-FLGODK = YES                                               
042600          MOVE JA               TO REG1-FLGODK                            
042700       END-IF                                                             
042800     ELSE                                                                 
042900       MOVE 'FLGODK'            TO RESP-IDELMT-ERROR                      
043000       MOVE NEJ                 TO NYCKLAR-SW                             
043100     END-IF                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 BG-INSPECT-IDDC  SECTION.                                                
043500                                                                          
043600     MOVE REQU-IDDC-KEY        TO W-IDDC-B6                               
043700                                                                          
043800     PERFORM IMS-GU-WDB601                                                
043900                                                                          
044000     IF DCS-CDC OR DCS-CDC-TR OR DCS-NDC-NA OR DCS-NDC-PF                 
044100     OR DCS-NDC-CN OR DCS-NDC-OTHERS OR DCS-NDC-SA                        
044200       MOVE  DCS-IDDC  TO REG1-IDDC                                       
044300     ELSE                                                                 
044400       MOVE 'IDDC'              TO RESP-IDELMT-ERROR                      
044500       MOVE NEJ                 TO NYCKLAR-SW                             
044600     END-IF                                                               
044700                                                                          
044800     IF DCS-NDC-NA OR DCS-NDC-PF OR DCS-NDC-CN OR DCS-NDC-OTHERS          
044810        OR DCS-NDC-SA                                                     
044900       IF WS-REQU-KDRT NOT = 00                                           
045000         MOVE 'KDRT'            TO RESP-IDELMT-ERROR                      
045100         MOVE NEJ TO NYCKLAR-SW                                           
045200       END-IF                                                             
045300     END-IF                                                               
045400     .                                                                    
045500     EJECT                                                                
045600                                                                          
045700 BJ-INSPECT-SDC-NDC-LEV SECTION.                                          
045800     MOVE 'BJ-INSPECT-SDC-NDC-LEV   ' TO WS-CURRENT-SECTION               
045900                                                                          
046000     IF WS-REQU-IDLEVNR NOT = SPACE                                       
046100                                                                          
046200       MOVE WS-REQU-IDLEVNR    TO W-IDDC-B6-LEV                           
046300       PERFORM IMS-GU-WDB601-LEV                                          
046400                                                                          
046500       IF SEGMENT-FINNS                                                   
046600         MOVE 'IDLEVNR'        TO RESP-IDELMT-ERROR                       
046700         MOVE NEJ              TO NYCKLAR-SW                              
046800       END-IF                                                             
046900     END-IF                                                               
047000     .                                                                    
047100     EJECT                                                                
047200                                                                          
047300 G-INSPECT-INPUT-UPDATE        SECTION.                                   
047400     MOVE 'G-INSPECT-INPUT-UPDATE' TO WS-CURRENT-SECTION                  
047500                                                                          
047600     MOVE JA                   TO INDATA-SW                               
047700     MOVE '001'                TO RESP-IDMSGVER                           
047800                                                                          
047900     PERFORM GA-INSPECT-COLUMNS                                           
048000     IF INDATA-FEL                                                        
048100                                                                          
048200       IF INDATA-EJ-IFYLLT                                                
048300          MOVE R-INF-UPDATE-NOT-DONE  TO RESP-IDMSG-INFO                  
048400*                                                                         
048500        ELSE                                                              
048600          MOVE R-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR                     
048700                                                                          
048800       END-IF                                                             
048900       PERFORM RESP-ROER-EJ-FAELT-IN                                      
049000     ELSE                                                                 
049100                                                                          
049200       PERFORM GB-ANROPA-W611REG                                          
049300                                                                          
049400       IF WS-REQU-FLGODK = JA OR NEJ OR YES                               
049500         CONTINUE                                                         
049600       ELSE                                                               
049700         PERFORM GC-INSPECT-FELFLAGGOR                                    
049800       END-IF                                                             
049900                                                                          
050000       IF W611REG-OK                                                      
050100         MOVE R-INF-UPDATE-DONE       TO RESP-IDMSG-INFO                  
050200         PERFORM MFS-FORM-ATTR                                            
050300         PERFORM RESP-RENSA-FAELT-IN                                      
050400       ELSE                                                               
050500         IF RESP-IDMSG-INFO  = REG1-INF-AVISERING-FINNS                   
050600                            OR R-INF-AVISERING-FINNS                      
050700           CONTINUE                                                       
050800         ELSE                                                             
050900            IF RESP-IDMSG-ERROR =  R-ERR-SUPPL-MISSING                    
051000               CONTINUE                                                   
051100            ELSE                                                          
051200               IF RESP-IDMSG-ERROR = SPACE                                
051300                   MOVE R-CORR-HILITE-FLDS TO RESP-IDMSG-ERROR            
051400               ELSE                                                       
051500                  IF RESP-IDMSG-ERROR  = R-INF-ARTIKEL-ERSATT             
051600                  OR RESP-IDMSG-ERROR  = R-INF-AVROP-SAKNAS               
051700                     MOVE MFS-OEPPNA-ALFA-FAELT TO                        
051800                                      RESP-FLGODK-IN-ATTR                 
051900                     PERFORM RESP-STAENG-KOL-FAELT                        
052000                  END-IF                                                  
052100               END-IF                                                     
052200            END-IF                                                        
052300         END-IF                                                           
052400         PERFORM RESP-ROER-EJ-FAELT-IN                                    
052500       END-IF                                                             
052600     END-IF                                                               
052700                                                                          
052800*          -- TRANSLATE MEDKONV MSG TO WEB MSG                            
052900     IF RESP-IDMSG-ERROR = REG1-INF-PART-MISSING                          
053000       MOVE R-ERR-NOT-FOUND                TO RESP-IDMSG-ERROR            
053100       MOVE 'IDARTNR'                      TO RESP-IDELMT-ERROR           
053200     ELSE                                                                 
053300       IF RESP-IDMSG-ERROR = REG1-INF-PART-OTHER-COMPANY                  
053400          MOVE R-INF-PART-OTHER-COMPANY TO RESP-IDMSG-ERROR               
053500       ELSE                                                               
053600         IF RESP-IDMSG-INFO  = REG1-INF-AVISERING-FINNS                   
053700         OR RESP-IDMSG-ERROR = REG1-INF-AVISERING-FINNS                   
053800            MOVE R-INF-AVISERING-FINNS TO RESP-IDMSG-INFO                 
053900            MOVE SPACE                     TO RESP-IDMSG-ERROR            
054000            MOVE 'IDAVINR'                 TO RESP-IDELMT-ERROR           
054100         ELSE                                                             
054200           IF RESP-IDMSG-ERROR = REG1-INF-PART-REPLACED                   
054300              MOVE R-INF-ARTIKEL-ERSATT    TO RESP-IDMSG-INFO             
054310              MOVE SPACE                   TO RESP-IDMSG-ERROR            
054400           ELSE                                                           
054500             IF RESP-IDMSG-ERROR = REG1-INF-ADVICE-MISSING                
054600                MOVE R-INF-AVROP-SAKNAS    TO RESP-IDMSG-INFO             
054700                MOVE SPACE                 TO RESP-IDMSG-ERROR            
054800                MOVE 'IDAVINR'             TO RESP-IDELMT-ERROR           
054900             ELSE                                                         
055000               IF RESP-IDMSG-ERROR = REG1-INF-PRICE-MISSING               
055100                  MOVE R-PRICE-INFO-MISSING  TO RESP-IDMSG-ERROR          
055200               ELSE                                                       
055210                 IF RESP-IDMSG-ERROR = REG1-INF-WRNG-CURRENCY             
055220                    MOVE R-PRICE-WRNG-CURRENCY TO RESP-IDMSG-ERROR        
055230                 ELSE                                                     
055300                   IF RESP-IDMSG-ERROR = REG1-INF-DIRECT-SUPPLIER         
055400                      MOVE R-ERR-DIRECT-SUPPLIER                          
055500                        TO RESP-IDMSG-ERROR                               
055600                   END-IF                                                 
055610                 END-IF                                                   
055700               END-IF                                                     
055800             END-IF                                                       
055900           END-IF                                                         
056000         END-IF                                                           
056100       END-IF                                                             
056200     END-IF                                                               
056300                                                                          
056400     .                                                                    
056500     EJECT                                                                
056600 GA-INSPECT-COLUMNS SECTION.                                              
056700                                                                          
056800     PERFORM GAA-INSPECT-COLUMN                                           
056900                                                                          
057000     IF INDATA-IFYLLT                                                     
057100       CONTINUE                                                           
057200     ELSE                                                                 
057300       MOVE NEJ                TO INDATA-SW                               
057400     END-IF                                                               
057500     .                                                                    
057600     EJECT                                                                
057700 GAA-INSPECT-COLUMN  SECTION.                                             
057800     MOVE 'GAA-INSPECT-KOLUMN     ' TO WS-CURRENT-SECTION                 
057900                                                                          
058000     MOVE +1             TO RAD-IX                                        
058100     PERFORM UNTIL RAD-IX > MAX-KVRADER                                   
058200                                                                          
058300       EVALUATE TRUE                                                      
058400         WHEN (REQU-IDARTNR-LINE(RAD-IX) = ALL '+' )  AND                 
058500              (REQU-KVAVIS-LINE (RAD-IX) = ALL '+' )                      
058600           CONTINUE                                                       
058700*                                                                         
058800         WHEN (REQU-IDARTNR-LINE(RAD-IX) NUMERIC AND                      
058900               REQU-KVAVIS-LINE (RAD-IX) NUMERIC)                         
059000                                                                          
059100           MOVE JA               TO INDATA-IFYLLT-SW                      
059200           MOVE MFS-NUM-FAELT-RAETT TO                                    
059300                     RESP-IDARTNR-LINE-ATTR (RAD-IX)                      
059400                     RESP-KVAVIS-LINE-ATTR (RAD-IX)                       
059500                                                                          
059600         WHEN OTHER                                                       
059700                                                                          
059800           MOVE R-MUST-BE-NUMERIC  TO RESP-IDMSG-ERROR                    
059900           MOVE SPACE TO RESP-IDELMT-ERROR                                
060000           MOVE RAD-IX TO INDX-DISPLAY                                    
060100           IF REQU-IDARTNR-LINE(RAD-IX) NUMERIC                           
060200             STRING 'KVAVIS*' INDX-DISPLAY                                
060300               DELIMITED BY SIZE   INTO RESP-IDELMT-ERROR                 
060400           ELSE                                                           
060500             STRING 'IDARTNR*' INDX-DISPLAY                               
060600               DELIMITED BY SIZE   INTO RESP-IDELMT-ERROR                 
060700           END-IF                                                         
060800                                                                          
060900           MOVE MFS-NUM-FAELT-FEL TO                                      
061000                      RESP-IDARTNR-LINE-ATTR (RAD-IX)                     
061100                      RESP-KVAVIS-LINE-ATTR (RAD-IX)                      
061200           MOVE NEJ              TO INDATA-SW                             
061300           MOVE JA               TO INDATA-IFYLLT-SW                      
061400                                                                          
061500       END-EVALUATE                                                       
061600       ADD +1                    TO RAD-IX                                
061700     END-PERFORM                                                          
061800                                                                          
061900     .                                                                    
062000     EJECT                                                                
062100 GB-ANROPA-W611REG  SECTION.                                              
062200     MOVE 'GB-ANROPA-W611REG    ' TO WS-CURRENT-SECTION                   
062300                                                                          
062400     PERFORM GBA-SKAPA-LAENKAREA                                          
062500                                                                          
062600     CALL W611REG USING LAENK-W611REG0                                    
062700                        REG-INLA1-PCB REG-INLA2-PCB                       
062800                        REG-INLA3-PCB REG-LEVA-PCB                        
062900                                      REG-ARTC-PCB                        
063000                        REG-BENA-PCB  REG-INLB-PCB                        
063100                        REG-WDK7-PCB  REG-WDB6-PCB                        
063110                                      9305-PCB                            
063200     .                                                                    
063300     EJECT                                                                
063400 GBA-SKAPA-LAENKAREA  SECTION.                                            
063500     MOVE 'GBA-SKAPA-LAENKAREA  ' TO WS-CURRENT-SECTION                   
063600                                                                          
063700     MOVE '6111'               TO REG1-IDTRANS                            
063800     MOVE WS-REQU-IDLEVNR      TO REG1-IDLEVNR                            
063900     MOVE WS-REQU-KDRT         TO REG1-KDRT                               
064000     MOVE WS-REQU-IDFS         TO REG1-IDFS                               
064100     MOVE WS-REQU-TIAVIDAT     TO REG1-TIAVIDAT                           
064200     MOVE WS-REQU-IDLBBET      TO REG1-IDLBBET                            
064300     MOVE MAX-KVRADER          TO REG1-KVRADER-MAX                        
064400                                                                          
064500     MOVE +1    TO RAD-IX                                                 
064600                   RAD-IX2                                                
064700     PERFORM UNTIL RAD-IX  >  REG1-KVRADER-MAX                            
064800                                                                          
064900       IF REQU-IDARTNR-LINE(RAD-IX) = ALL '+'                             
065000       OR REQU-IDARTNR-LINE(RAD-IX) = SPACE                               
065100         MOVE ZERO                      TO REG1-IDARTNR(RAD-IX2)          
065200                                           REG1-KVAVIS (RAD-IX2)          
065300       ELSE                                                               
065400         MOVE REQU-IDARTNR-LINE(RAD-IX) TO REG1-IDARTNR(RAD-IX2)          
065500         MOVE REQU-KVAVIS-LINE(RAD-IX)  TO REG1-KVAVIS (RAD-IX2)          
065600       END-IF                                                             
065700       ADD +1 TO RAD-IX                                                   
065800                 RAD-IX2                                                  
065900     END-PERFORM                                                          
066000     .                                                                    
066100     EJECT                                                                
066200 GC-INSPECT-FELFLAGGOR SECTION.                                           
066300     MOVE 'GC-INSPECT-FELFLAGGOR  ' TO WS-CURRENT-SECTION                 
066400                                                                          
066500     IF REG1-IDLEVNR-OK                 =  JA  AND                        
066600        REG1-KDRT-OK                    =  JA  AND                        
066700        REG1-TIAVIDAT-OK                =  JA  AND                        
066800        REG1-IDFS-OK                    =  JA                             
066900       IF REG1-FLGODK-IDARTNR           =  JA                             
067000         MOVE MFS-OEPPNA-ALFA-FAELT      TO RESP-FLGODK-IN-ATTR           
067100         MOVE R-INF-AVISERING-FINNS      TO RESP-IDMSG-INFO               
067200         MOVE 'IDAVINR'                  TO RESP-IDELMT-ERROR             
067300         PERFORM RESP-STAENG-KOL-FAELT                                    
067400       END-IF                                                             
067500     ELSE                                                                 
067600       MOVE NEJ                   TO W611REG-SW                           
067700                                                                          
067800       IF REG1-IDLEVNR-OK               = JA    AND                       
067900          REG1-KDRT-OK                  = JA    AND                       
068000          REG1-TIAVIDAT-OK              = JA    AND                       
068100         (REG1-FLGODK-IDFS              = JA    OR                        
068200          REG1-FLGODK-IDARTNR           = JA)                             
068300                                                                          
068400         MOVE MFS-OEPPNA-ALFA-FAELT      TO RESP-FLGODK-IN-ATTR           
068500         MOVE R-INF-AVISERING-FINNS      TO RESP-IDMSG-INFO               
068600         MOVE 'IDAVINR'                  TO RESP-IDELMT-ERROR             
068700         PERFORM RESP-STAENG-KOL-FAELT                                    
068800       ELSE                                                               
068900         IF REG1-IDLEVNR-OK     = NEJ                                     
069000           MOVE R-ERR-SUPPL-MISSING      TO RESP-IDMSG-ERROR              
069100           MOVE 'IDLEVNR'                TO RESP-IDELMT-ERROR             
069200         ELSE                                                             
069300                                                                          
069400           IF RESP-IDMSG-ERROR = SPACE                                    
069500             MOVE R-ERR-IN-LINE-ONE      TO RESP-IDMSG-ERROR              
069600           END-IF                                                         
069700         END-IF                                                           
069800       END-IF                                                             
069900     END-IF                                                               
070000                                                                          
070100     IF W611REG-OK                                                        
070200       MOVE +1                   TO RAD-IX2                               
070300                                                                          
070400       PERFORM GCA-INSPECT-FELFLAGGOR-KOLUMN                              
070500     END-IF                                                               
070600     .                                                                    
070700     EJECT                                                                
070800 GCA-INSPECT-FELFLAGGOR-KOLUMN SECTION.                                   
070900     MOVE 'GCA-INSPECT-FELFLAGGOR-KOLUMN' TO WS-CURRENT-SECTION           
071000                                                                          
071100     MOVE +1                   TO RAD-IX                                  
071200     PERFORM UNTIL RAD-IX  >  REG1-KVRADER-MAX                            
071300       IF REG1-IDARTNR-OK (RAD-IX2) = JA                                  
071400         MOVE MFS-NUM-FAELT-RAETT TO                                      
071500                    RESP-IDARTNR-LINE-ATTR (RAD-IX)                       
071600       ELSE                                                               
071700         IF RESP-IDMSG-ERROR = SPACE OR REG1-INF-AVISERING-FINNS          
071800                                                                          
071900           MOVE REG1-IDMFSFEL (RAD-IX2) TO RESP-IDMSG-ERROR               
072000           MOVE SPACE            TO RESP-IDELMT-ERROR                     
072100           MOVE RAD-IX           TO INDX-DISPLAY                          
072200           STRING 'IDARTNR*' INDX-DISPLAY                                 
072300             DELIMITED BY SIZE   INTO RESP-IDELMT-ERROR                   
072400         END-IF                                                           
072500         MOVE MFS-NUM-FAELT-FEL        TO                                 
072600                RESP-IDARTNR-LINE-ATTR (RAD-IX)                           
072700                                                                          
072800         MOVE NEJ                      TO W611REG-SW                      
072900       END-IF                                                             
073000       ADD +1                          TO RAD-IX                          
073100                                          RAD-IX2                         
073200     END-PERFORM                                                          
073300                                                                          
073400     IF RESP-IDMSG-ERROR = SPACE OR REG1-INF-AVISERING-FINNS              
073500       CONTINUE                                                           
073600     ELSE                                                                 
073700       MOVE +1                            TO RAD-IX                       
073800       MOVE +1                            TO RAD-IX2                      
073900       PERFORM UNTIL RAD-IX  > MAX-KVRADER                                
074000         IF REG1-IDMFSFEL (RAD-IX2) = REG1-INF-AVISERING-FINNS            
074100            MOVE MFS-ALFA-FAELT-RAETT  TO                                 
074200                    RESP-IDARTNR-LINE-ATTR (RAD-IX)                       
074300         END-IF                                                           
074400         ADD +1                           TO RAD-IX                       
074500         ADD +1                           TO RAD-IX2                      
074600       END-PERFORM                                                        
074700     END-IF                                                               
074800     .                                                                    
074900     SKIP2                                                                
075000                                                                          
075100* --- RESP SEKTIONER ---                                                  
075200* --- RESP SEKTIONER ---                                                  
075300* --- RESP SEKTIONER ---                                                  
075400                                                                          
075500 RESP-STAENG-KOL-FAELT  SECTION.                                          
075600                                                                          
075700     MOVE +1                          TO RAD-IX2                          
075800                                         RAD-IX                           
075900     PERFORM UNTIL RAD-IX             >  MAX-KVRADER                      
076000         IF REG1-IDARTNR-OK (RAD-IX2) =  JA                               
076100             MOVE MFS-STAENG-FAELT TO                                     
076200                                 RESP-IDARTNR-LINE-ATTR(RAD-IX)           
076300          ELSE                                                            
076400             MOVE MFS-STAENG-FAELT-HI TO                                  
076500                                 RESP-IDARTNR-LINE-ATTR(RAD-IX)           
076600         END-IF                                                           
076700         MOVE MFS-STAENG-FAELT TO RESP-KVAVIS-LINE-ATTR(RAD-IX)           
076800         ADD +1                TO RAD-IX                                  
076900                                  RAD-IX2                                 
077000     END-PERFORM                                                          
077100     .                                                                    
077200     EJECT                                                                
077300                                                                          
077400 MFS-FORM-ATTR    SECTION.                                                
077500                                                                          
077600*    --- ALLA INDATA-FÄLT                                                 
077700     MOVE MFS-FORMATETS-ATTR   TO RESP-FLGODK-IN                          
077800                                                                          
077900     MOVE +1                     TO RAD-IX                                
078000     PERFORM UNTIL RAD-IX        >  MAX-KVRADER                           
078100       MOVE MFS-FORMATETS-ATTR TO RESP-IDARTNR-LINE-ATTR(RAD-IX)          
078200                                  RESP-KVAVIS-LINE-ATTR(RAD-IX)           
078300       ADD +1                    TO RAD-IX                                
078400     END-PERFORM                                                          
078500     .                                                                    
078600     SKIP2                                                                
078700                                                                          
078800 RESP-RENSA-FAELT-UT-RADER SECTION.                                       
078900                                                                          
079000*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
079100                                                                          
079200     MOVE +1                     TO RAD-IX                                
079300     PERFORM UNTIL RAD-IX        >  MAX-KVRADER                           
079400       PERFORM RESP-RENSA-RAD-FAELT-UT                                    
079500       ADD +1                TO RAD-IX                                    
079600     END-PERFORM                                                          
079700     .                                                                    
079800     SKIP2                                                                
079900 RESP-RENSA-RAD-FAELT-UT SECTION.                                         
080000                                                                          
080100*    --- UTDATA-FÄLT PÅ BLÄDDRINGSRADER                                   
080200     MOVE W-SPACE         TO RESP-IDARTNR-LINE(RAD-IX)                    
080300                             RESP-KVAVIS-LINE (RAD-IX)                    
080400     .                                                                    
080500     SKIP2                                                                
080600 RESP-RENSA-FAELT-IN SECTION.                                             
080700                                                                          
080800*    --- ALLA INDATA-FÄLT                                                 
080900     MOVE W-SPACE              TO RESP-FLGODK-IN                          
081000                                                                          
081100     MOVE +1                   TO RAD-IX                                  
081200     PERFORM UNTIL RAD-IX      >  MAX-KVRADER                             
081300         MOVE W-SPACE          TO RESP-IDARTNR-LINE (RAD-IX)              
081400                                  RESP-KVAVIS-LINE (RAD-IX)               
081500         ADD +1                TO RAD-IX                                  
081600     END-PERFORM                                                          
081700     .                                                                    
081800     SKIP2                                                                
081900 RESP-ROER-EJ-FAELT-IN SECTION.                                           
082000                                                                          
082100*    --- ALLA INDATA-FÄLT                                                 
082200     MOVE +001                   TO RAD-IX                                
082300     PERFORM UNTIL RAD-IX        >  MAX-KVRADER                           
082400       MOVE ALL '+'              TO RESP-IDARTNR-LINE(RAD-IX)             
082500                                    RESP-KVAVIS-LINE(RAD-IX)              
082600       ADD +001                  TO RAD-IX                                
082700     END-PERFORM                                                          
082800     .                                                                    
082900     SKIP2                                                                
083000                                                                          
083100* --- IMS SEKTIONER ---                                                   
083200* --- IMS SEKTIONER ---                                                   
083300* --- IMS SEKTIONER ---                                                   
083400                                                                          
083500     SKIP3                                                                
083600 IMS-GU-WDB601-LEV SECTION.                                               
083700     MOVE 'IMS-GU-WDB601-LEV      '    TO WS-CURRENT-IMS-SECTION          
083800                                                                          
083900     STRING 'WDB601  (IDLEVNDC =' W-IDDC-B6-LEV-X ')'                     
084000          DELIMITED BY SIZE INTO SSA1                                     
084100     MOVE '  GE' TO GODK-STATUSKODER                                      
084200     CALL CBLTDLI USING GU WDB6-LEV-PCB DLI-IO-AREA-B601-LEV SSA1         
084300     MOVE WDB6-LEV-STATUS-CODE    TO STATUS-WS                            
084400     PERFORM IMS-STATUSKONTROLL                                           
084500     .                                                                    
084600     EJECT                                                                
084700 IMS-GU-WDB601    SECTION.                                                
084800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
084900          DELIMITED BY SIZE INTO SSA1                                     
085000     MOVE '  GE' TO GODK-STATUSKODER                                      
085100     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
085200     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
085300     PERFORM IMS-STATUSKONTROLL                                           
085400     IF SEGMENT-SAKNAS                                                    
085500         MOVE SPACE TO DCS-KDDC                                           
085600     END-IF                                                               
085700     .                                                                    
085800     EJECT                                                                
085900 IMS-STATUSKONTROLL SECTION.                                              
086000     MOVE 'IMS-STATUSKONTROLL     '    TO WS-CURRENT-IMS-SECTION          
086100                                                                          
086200     SET STATUS-IX TO 1                                                   
086300     SEARCH GODK-STATUS                                                   
086400       AT END                                                             
086500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
086600         DELIMITED BY SIZE INTO ERROR-TEXT                                
086700         CALL FELLOG                                                      
086800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
086900         CONTINUE                                                         
087000     END-SEARCH                                                           
087100     .                                                                    
