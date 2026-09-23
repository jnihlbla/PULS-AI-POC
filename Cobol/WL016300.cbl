000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL016300.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   JUNI 2005.                                               
000600                                                                          
000700                                                                          
000800     REMARKS.                                                             
000900* WL016300 PROGRAM IS A REPLICA OF W4071500 PROGRAM                       
001000* AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                                
001100*                                                                         
001110*                                                                         
001200*    NAMN:       CARPARTS.LDC.RETURNPERMISSIONQUERY                       
001300*                                                                         
001400                                                                          
001500*    FUNCTION:                                                            
001600*        QUESTION ON RETURN PERMIT.                                       
001700*        KEYS THAT HAVE TO BE FILLED IN: DISTR/CUST/REPORT.NO             
001800*        READS DATABASE WLKREE AND SHOWS INFO.                            
001900*                                                                         
002000*        THE PROGRAM READS     WLKREE (WDA2)                              
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSACTION: WL0163                                              
002400*        REQU:        WL0163I1                                            
002500*                                                                         
002600*    OUTDATA.                                                             
002700*        RESP:        WL0163O1                                            
002800*                                                                         
002900*    E'TRACKER: 5179820  DATE 2007-06                                     
002900*    E'TRACKER: 10296404 DATE 2017-01                                     
002900*    E'TRACKER: 10302968 DATE 2017-07                                     
003000*                                                                         
003100                                                                          
003200                                                                          
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600 WORKING-STORAGE SECTION.                                                 
003700*    -- CHECKED BY WY2000                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'W4071500'.            
003900 77  FILLER                      PIC X(8)    VALUE 'AAAAAAAA'.            
004000 77  PGM-POS                     PIC X(32)   VALUE SPACE.                 
004100 77  KDRC-DISPLAY                PIC Z(5)    VALUE ZERO.                  
004200                                                                          
004300*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
004400 77  FILLER                      PIC X(8)    VALUE 'BBBBBBBB'.            
004500 77  ERROR-TEXT                  PIC X(64)   VALUE SPACE.                 
004600                                                                          
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  YES                         PIC X       VALUE 'Y'.                   
004900 77  NOO                         PIC X       VALUE 'N'.                   
005000                                                                          
005100*    --- INDEX FOR SCROLL LINES                                           
005200 77  FILLER                      PIC X(8)    VALUE 'CCCCCCCC'.            
005300 77  INDX                        PIC S9(4)  VALUE +0    COMP-3.           
005400 77  MAX-MOD-LENGTH              PIC S9(4)  VALUE +1200 COMP-3.           
005500 77  MAX-INDX                    PIC S9(4)  VALUE +500  COMP-3.           
005600 77  WS-IDARTNR                  PIC 9(9)   VALUE ZERO.                   
005700 77  IDARTNR-WS                  PIC 9(8)   VALUE ZERO.                   
005800 77  IDRADNR-WS                  PIC 9(4)   VALUE ZERO.                   
005900 77  IDDISTR-WS                  PIC 9(4)   VALUE ZERO.                   
006000 77  IDKUNDNR-WS                 PIC 9(6)   VALUE ZERO.                   
006100 77  IDRAPPNR-WS                 PIC 9(7)   VALUE ZERO.                   
006200 77  WS-TIRETANK                 PIC S9(6)  VALUE ZERO  COMP-3.           
006300*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
006400                                                                          
006500 77  FILLER                      PIC X(8)    VALUE 'DDDDDDDD'.            
006600 77  KEYS-SW                     PIC X       VALUE 'J'.                   
006700     88  KEYS-OK                             VALUE 'Y'.                   
006800     88  KEYS-WRONG                          VALUE 'N'.                   
006900                                                                          
007000 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007100     88  OWN-MID                             VALUE '4715'.                
007200     88  GOOD-MID                            VALUE '4711' '4712'          
007300                                                   '4713' '4714'          
007400                                                   '4715' '4716'          
007500                                                   '472D'.                
007600     88  HELP-MID                            VALUE '0551'.                
007700     88  4737-MID                            VALUE '4737'.                
007800     EJECT                                                                
007900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
008000 01  GENERAL-SUBPROGRAM.                                                  
008100     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008400     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
008500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008600     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008700     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008800     EJECT                                                                
008900*                                                                         
009000*    --- PARAMETERS TO ABEND                                              
009100                                                                          
009200 77  FILLER                      PIC X(08)   VALUE 'ABENDARE'.            
009300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +33.              
009400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009600     SKIP2                                                                
009640                                                                          
009700 77  FILLER                      PIC X(08)   VALUE 'MESSAGES'.            
009800 01  MESSAGE-CODES.                                                       
009900     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
010000     03  ERR-CORR-FIELDS         PIC X(3)    VALUE '023'.                 
010100     03  ERR-ARTIKEL-SAKNAS      PIC X(3)    VALUE '017'.                 
010200     03  INF-NO-MORE-LINES       PIC X(3)    VALUE '316'.                 
010300     03  INF-FIRST-PAGE          PIC X(3)    VALUE '006'.                 
010400     03  INF-MORE-INFO-EXISTS    PIC X(3)    VALUE '105'.                 
010500     03  ERR-HIGHL-FIELDS        PIC X(3)    VALUE '409'.                 
010600     03  ERR-NO-UPDATE           PIC X(3)    VALUE '777'.                 
010700     03  ERR-LINE-MISSING        PIC X(3)    VALUE '027'.                 
010710     03  ERR-NOT-AUTHORIZED      PIC X(3)    VALUE '00A'.                 
010800     EJECT                                                                
010900*    ---  LÄNKAREA TILL W418OKOD                                          
011000 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
011100                                                                          
011200*01 -COPY W418OKOD           -PRE OKOD-.                                  
011300     EJECT                                                                
011400*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
011500*                                                                         
011600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011700     SKIP3                                                                
011800*01  -COPY WZ01SUB                                                        
011900     SKIP3                                                                
012000 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
012100     SKIP3                                                                
012200 01  REQU-AREA.                                                           
012300*    03  -COPY WZ01REQU                                                   
012400*    03  -COPY WL0163I1                                                   
012500     SKIP3                                                                
012600 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012700     SKIP3                                                                
012800 01  RESP-AREA.                                                           
012900*    03  -COPY WZ01RESP                                                   
013000*    03  -COPY WL0163O1                                                   
013100     SKIP3                                                                
013200*    --- WORK-AREAS FOR IMS-SECTIONS                                      
013300*                                                                         
013400     SKIP3                                                                
013500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013600                                                                          
013700 01  KEYS-TO-DLI.                                                         
013800     03  W-IDLEVANM-X.                                                    
013900         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
014000         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
014100         05  W-IDRAPPNR          PIC 9(7).                                
014200     03  W-WDA2KEY-X.                                                     
014300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014400         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
014500                                                                          
014600*    --- STATUS-KOD FRÅN IMS                                              
014700 01  STATUS-WS                   PIC XX.                                  
014800     88  SEGMENT-FOUND                       VALUE '  '.                  
014900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
015000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
015100                                                                          
015200 01  GOOD-STATUSCODES.                                                    
015300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015400                                                                          
015500 01  SSA1                        PIC X(64).                               
015600 01  SSA2                        PIC X(64).                               
015700     SKIP3                                                                
015800*    --- IMS FUNCTION CODES                                               
015900*01  -COPY W0003                                                          
016000     SKIP3                                                                
016100*    ---  DLI INPUT-OUTPUT AREA                                           
016200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016300                                                                          
016400 01  DLI-IO-AREA.                                                         
016500     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
016600                                                                          
016700     03  WLKREE01 REDEFINES IO-AREA.                                      
016800*        05  -COPY WDA201                                                 
016900                                                                          
017000     03  WLKREE11 REDEFINES IO-AREA.                                      
017100*        05  -COPY WDA211                                                 
017200     SKIP3                                                                
017300 LINKAGE SECTION.                                                         
017400                                                                          
017500*01  -COPY W0009   -PRE MSG-                                              
017600     EJECT                                                                
017700*01  -COPY W0008  -PRE KREE-                                              
017800     05  FILLER                  PIC X.                                   
017900     EJECT                                                                
018000 PROCEDURE DIVISION  USING MSG-PCB                                        
018100                           KREE-PCB.                                      
018200     ENTRY 'DLITCBL' USING MSG-PCB                                        
018300                           KREE-PCB.                                      
018400 MAIN SECTION.                                                            
018500                                                                          
018600     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
018700     IF SUB-KDRC = 0                                                      
018800                                                                          
018900       PERFORM A-INIT                                                     
019000       PERFORM B-CHECK-KEYS                                               
019100       IF KEYS-OK                                                         
019200*        IF MFS-FIRST                                                     
019300*          PERFORM C-FIRST-PAGE                                           
019400*        ELSE                                                             
019500*          IF MFS-NEXT                                                    
019600*            PERFORM D-NEXT-PAGE                                          
019700*          ELSE                                                           
019800*            PERFORM E-SAME-PAGE                                          
019900*          END-IF                                                         
020000*        END-IF                                                           
020100         PERFORM F-READ-SHOW-INFO                                         
020200       END-IF                                                             
020300       PERFORM S02-RETURN-RESPONSE                                        
020400     END-IF                                                               
020500                                                                          
020600     MOVE ZERO TO RETURN-CODE                                             
020700     GOBACK                                                               
020800     .                                                                    
020900     EJECT                                                                
021000 A-INIT SECTION.                                                          
021100     MOVE 'STA A-INIT        ' TO PGM-POS                                 
021200                                                                          
021300     MOVE ALL '+'              TO RESP-WL0163O1                           
021400     MOVE 001                  TO RESP-IDMSGVER                           
021500     MOVE SPACE                TO RESP-IDMSG-ERROR                        
021600                                  RESP-IDMSG-INFO                         
021700                                  RESP-IDELMT-ERROR                       
021800     MOVE ZERO                 TO RESP-KVRADER                            
021900     .                                                                    
022000     EJECT                                                                
022100 B-CHECK-KEYS SECTION.                                                    
022200     MOVE 'STA B-CHECK-KEYS  ' TO PGM-POS                                 
022300                                                                          
022400*    MOVE 'GB'                  TO MED-IDSKYLT                            
022500                                                                          
022600     MOVE YES TO KEYS-SW                                                  
022700                                                                          
022800*    MOVE MSGI-IDDISTR  TO W-IDDISTR                                      
022900*    MOVE MSGI-IDKUNDNR TO W-IDKUNDNR                                     
023000*    MOVE MSGI-IDRAPPNR TO W-IDRAPPNR                                     
023100*    MOVE MSGI-IDRADNR                    TO IDRADNR-WS                   
023200*    MOVE MSGI-IDARTNR (2:8)              TO IDARTNR-WS                   
023300*                                                                         
023400     MOVE REQU-IDDC-KEY2           TO RESP-IDDC-KEY2                      
023500*    MOVE REQU-IDUSER              TO RESP-IDUSER                         
023600                                                                          
023700     IF REQU-IDDISTR-KEY2 NOT = ALL '+'                                   
023800       MOVE REQU-IDDISTR-KEY2      TO IDDISTR-WS                          
023900     ELSE                                                                 
024000       MOVE ZERO                   TO IDDISTR-WS                          
024100       MOVE 'IDDISTR'              TO RESP-IDELMT-ERROR                   
024200     END-IF                                                               
024300                                                                          
024400     IF REQU-IDKUNDNR-KEY2 NOT = ALL '+'                                  
024500       MOVE REQU-IDKUNDNR-KEY2     TO IDKUNDNR-WS                         
024600     ELSE                                                                 
024700       MOVE ZERO                   TO IDKUNDNR-WS                         
024800       IF RESP-IDELMT-ERROR = SPACE                                       
024900         MOVE 'IDKUNDNR'           TO RESP-IDELMT-ERROR                   
025000       END-IF                                                             
025100     END-IF                                                               
025200                                                                          
025300     IF REQU-IDRAPPNR-KEY2 NOT = ALL '+'                                  
025400       MOVE REQU-IDRAPPNR-KEY2    TO IDRAPPNR-WS                          
025500     ELSE                                                                 
025600       MOVE ZERO                  TO IDRAPPNR-WS                          
025700       IF RESP-IDELMT-ERROR = SPACE                                       
025800         MOVE 'IDRAPPNR'          TO RESP-IDELMT-ERROR                    
025900       END-IF                                                             
026000     END-IF                                                               
026100                                                                          
026200     IF REQU-IDARTNR-KEY2 NOT = ALL '+'                                   
026300       IF REQU-IDARTNR-KEY2 NUMERIC                                       
026400         MOVE REQU-IDARTNR-KEY2 TO IDARTNR-WS                             
026500       ELSE                                                               
026600         MOVE 'IDARTNR'            TO RESP-IDELMT-ERROR                   
026700         MOVE NOO                  TO KEYS-SW                             
026800       END-IF                                                             
026900     ELSE                                                                 
027000       MOVE ZERO                   TO IDARTNR-WS                          
027100     END-IF                                                               
027200                                                                          
027300     IF REQU-IDRADNR-KEY2 NOT = ALL '+'                                   
027400       IF REQU-IDRADNR-KEY2 NUMERIC                                       
027410         MOVE REQU-IDRADNR-KEY2    TO IDRADNR-WS                          
027420       ELSE                                                               
027500         IF RESP-IDELMT-ERROR = SPACE                                     
027600           MOVE 'IDRADNR'          TO RESP-IDELMT-ERROR                   
027700         END-IF                                                           
027710         MOVE NOO                  TO KEYS-SW                             
027800       END-IF                                                             
027900     ELSE                                                                 
028000       MOVE ZERO                   TO IDRADNR-WS                          
028100     END-IF                                                               
028200                                                                          
028300     INSPECT IDARTNR-WS REPLACING LEADING SPACE BY ZERO                   
028400     INSPECT IDRADNR-WS REPLACING LEADING SPACE BY ZERO                   
028500                                                                          
028600     IF REQU-IDDISTR-KEY2 NOT NUMERIC                                     
028700       IF RESP-IDELMT-ERROR = SPACE                                       
028800         MOVE 'IDDISTR'            TO RESP-IDELMT-ERROR                   
028900       END-IF                                                             
029000       MOVE NOO                    TO KEYS-SW                             
029100     ELSE                                                                 
029200       MOVE IDDISTR-WS             TO W-IDDISTR                           
029400     END-IF                                                               
029500                                                                          
029600     IF REQU-IDKUNDNR-KEY2 NOT NUMERIC                                    
029700       IF RESP-IDELMT-ERROR = SPACE                                       
029800         MOVE 'IDKUNDNR'           TO RESP-IDELMT-ERROR                   
029900       END-IF                                                             
030000       MOVE NOO                    TO KEYS-SW                             
030100     ELSE                                                                 
030200       MOVE IDKUNDNR-WS            TO W-IDKUNDNR                          
030400     END-IF                                                               
030500                                                                          
030600     IF REQU-IDRAPPNR-KEY2 NOT NUMERIC                                    
030700       IF RESP-IDELMT-ERROR = SPACE                                       
030800         MOVE 'IDRAPPNR'           TO RESP-IDELMT-ERROR                   
030900       END-IF                                                             
031000       MOVE NOO                    TO KEYS-SW                             
031100     ELSE                                                                 
031200       MOVE IDRAPPNR-WS            TO W-IDRAPPNR                          
031400     END-IF                                                               
031500                                                                          
031600     IF IDARTNR-WS NOT NUMERIC                                            
031800       IF RESP-IDELMT-ERROR = SPACE                                       
031900         MOVE 'IDARTNR'            TO RESP-IDELMT-ERROR                   
032000       END-IF                                                             
032010       MOVE NOO                    TO KEYS-SW                             
032100     ELSE                                                                 
032200       MOVE IDARTNR-WS             TO W-IDARTNR                           
032400     END-IF                                                               
032500                                                                          
032600     IF IDRADNR-WS NOT NUMERIC                                            
032700       IF RESP-IDELMT-ERROR = SPACE                                       
032800         MOVE 'IDRADNR'            TO RESP-IDELMT-ERROR                   
032900       END-IF                                                             
033000       MOVE NOO                    TO KEYS-SW                             
033010     ELSE                                                                 
033030       MOVE IDRADNR-WS             TO W-IDRADNR                           
033100     END-IF                                                               
033200                                                                          
916500*    FIX TO MAKE IT POSSIBLE FOR A SPECIFIC USER TO HANDLE                
916500*    RETURNS FROM CA (FTG=54) TO US (DC=44, FTG=53)                       
917800*    IF REQU-IDUSER = 'PHCA4G1'                                           
917800*       AND REQU-IDDC-KEY2 = '44'                                         
917800*      MOVE '54'           TO REQU-IDFTG-KEY2                             
070900*    END-IF                                                               
      *    END FIX                                                              
                                                                                
033201     IF REQU-IDFTG-KEY2 NOT NUMERIC                                       
033202       MOVE NOO                    TO KEYS-SW                             
033203       MOVE 'IDFTG'                TO RESP-IDELMT-ERROR                   
033204       MOVE '023'                  TO RESP-IDMSG-ERROR                    
033205     END-IF                                                               
033206                                                                          
033210     IF KEYS-OK                                                           
033300       MOVE IDDISTR-WS           TO RESP-IDDISTR-KEY2                     
033310       INSPECT RESP-IDDISTR-KEY2  REPLACING LEADING ZERO BY SPACE         
033400       MOVE IDKUNDNR-WS          TO RESP-IDKUNDNR-KEY2                    
033410       INSPECT RESP-IDKUNDNR-KEY2 REPLACING LEADING ZERO BY SPACE         
033500       MOVE IDRAPPNR-WS          TO RESP-IDRAPPNR-KEY2                    
033510       INSPECT RESP-IDRAPPNR-KEY2 REPLACING LEADING ZERO BY SPACE         
033600       MOVE IDARTNR-WS           TO RESP-IDARTNR-KEY2                     
033610       INSPECT RESP-IDARTNR-KEY2  REPLACING LEADING ZERO BY SPACE         
033700       MOVE IDRADNR-WS           TO RESP-IDRADNR-KEY2                     
033701       INSPECT RESP-IDRADNR-KEY2  REPLACING LEADING ZERO BY SPACE         
033710     END-IF                                                               
033800                                                                          
034400*    IF RESP-IDKUNDNR-KEY2       = SPACE                                  
034500*       MOVE '     0'           TO RESP-IDKUNDNR-KEY2                     
034600*    END-IF                                                               
034700                                                                          
034800     IF KEYS-WRONG                                                        
034900       MOVE ERR-WRONG-KEY          TO RESP-IDMSG-ERROR                    
035000     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300 F-READ-SHOW-INFO SECTION.                                                
035400                                                                          
035500     PERFORM IMS-GET-KREE01                                               
035600                                                                          
035700     IF SEGMENT-MISSING                                                   
035800        MOVE ERR-LINE-MISSING      TO RESP-IDMSG-ERROR                    
035900        MOVE ZERO                  TO RESP-KVRADER                        
036000     ELSE                                                                 
036010      IF ANM-IDFTG NOT = REQU-IDFTG-KEY2                                  
036020         MOVE ERR-NOT-AUTHORIZED   TO RESP-IDMSG-ERROR                    
036030         MOVE ZERO                 TO RESP-KVRADER                        
036040      ELSE                                                                
036050                                                                          
036100       IF ANM-DARETILL > 0                                                
036200          MOVE ANM-DARETILL(3:6) TO RESP-TIRETILL                         
036300*      ELSE                                                               
036400*         MOVE MFS-RENSA-FAELT   TO RESP-TIRETILL                         
036500       END-IF                                                             
036600       MOVE ANM-DARETANK(3:6)    TO WS-TIRETANK                           
036700       MOVE +1 TO INDX                                                    
036800                                                                          
036900       PERFORM IMS-GNP-KREE11                                             
037000                                                                          
037100       PERFORM UNTIL INDX > MAX-INDX                                      
037200                  OR SEGMENT-MISSING                                      
037300         IF SEGMENT-FOUND                                                 
037400           MOVE LEV-IDARTNR      TO RESP-IDARTNR   (INDX)                 
037500           MOVE LEV-IDRADNR      TO RESP-IDRADNR   (INDX)                 
037600           MOVE LEV-KDANMORS     TO RESP-KDANMORS  (INDX)                 
037700           MOVE LEV-KVLEVANM-BEKR TO RESP-KVLEVANM-BEKR (INDX)            
037800           MOVE LEV-KVLEVANM     TO RESP-KVLEVANM  (INDX)                 
037900           IF LEV-IDLOPNRM > +0                                           
038000             MOVE WS-TIRETANK    TO RESP-TIRETANK  (INDX)                 
038100           ELSE                                                           
038200             MOVE ZERO           TO RESP-TIRETANK  (INDX)                 
038300           END-IF                                                         
038400           MOVE LEV-TIINLINL     TO RESP-TIINLINL  (INDX)                 
038500           MOVE LEV-KVRETINL     TO RESP-KVRETINL  (INDX)                 
038600           MOVE LEV-KVRETINL-SKR TO RESP-KVRETINL-SKR (INDX)              
038700           MOVE LEV-KVAVV-KVANT  TO RESP-KVAVV-KVANT (INDX)               
038800           MOVE LEV-KVAVV-KVAL   TO RESP-KVAVV-KVAL (INDX)                
038900           IF LEV-FLTEXT         = JA                                     
039000*            IF MSGI-IDLAND-SPR = 'GB'                                    
039100               MOVE YES         TO RESP-FLTEXT  (INDX)                    
039200*            ELSE                                                         
039300*              MOVE LEV-FLTEXT  TO RESP-FLTEXT  (INDX)                    
039400*            END-IF                                                       
039500           ELSE                                                           
039600             MOVE LEV-FLTEXT    TO RESP-FLTEXT  (INDX)                    
039700           END-IF                                                         
039800           IF LEV-IDANSTNR-RET > ZERO                                     
039900              MOVE LEV-IDANSTNR-RET TO RESP-IDANSTNR-RET                  
040000           END-IF                                                         
040100                                                                          
040200*--- ANROPA KONTROLL AV ORSAKSKODER                                       
040300           MOVE LEV-KDANMORS    TO OKOD-KDANMORS                          
040400           CALL W418OKOD USING OKOD-W418OKOD                              
040500           IF OKOD-FL-RETILL = 'J' OR                                     
040600              OKOD-FL-INTERNUPPACKNING = 'J'                              
040700             IF LEV-KDKREBEH = 'ANN'                                      
040800               MOVE 'XA'         TO RESP-FLRETILL  (INDX)                 
040900             ELSE                                                         
041000               MOVE 'X '         TO RESP-FLRETILL  (INDX)                 
041100             END-IF                                                       
041200           ELSE                                                           
041300             IF LEV-KDKREBEH = 'ANN'                                      
041400               MOVE ' A'         TO RESP-FLRETILL  (INDX)                 
041500             ELSE                                                         
041600               MOVE '  '         TO RESP-FLRETILL  (INDX)                 
041700             END-IF                                                       
041800           END-IF                                                         
041900                                                                          
042000           PERFORM IMS-GNP-KREE11                                         
042100*        ELSE                                                             
042200*          MOVE MFS-ERASE-FIELD  TO MOD-IDARTNR    (INDX)                 
042300*                                   MOD-KDANMORS   (INDX)                 
042400*                                   MOD-KDANMORS   (INDX)                 
042500*                                   MOD-KVLEVANM-BEKR (INDX)              
042600*                                   MOD-KVLEVANM   (INDX)                 
042700*                                   MOD-TIRETANK   (INDX)                 
042800         END-IF                                                           
042900         ADD 1 TO INDX                                                    
043000       END-PERFORM                                                        
043100                                                                          
043200       COMPUTE RESP-KVRADER = INDX - 1                                    
043300       END-COMPUTE                                                        
043400                                                                          
043500      END-IF                                                              
043510     END-IF                                                               
043600     .                                                                    
043700     EJECT                                                                
043800 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
043900     MOVE 'STA S01-FETCH-REQUEST          ' TO PGM-POS                    
044000                                                                          
044100     MOVE 'GETARG'               TO SUB-KDFUNC                            
044200     MOVE 'CARPARTS.LDC.RETURNPERMISSIONQUERY' TO SUB-ADDISPABS           
044300     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
044400                                                                          
044500     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
044600                                                                          
044700     IF SUB-KDRC > 0                                                      
044800       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
044900       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
045000       DELIMITED BY SIZE INTO ERROR-TEXT                                  
045100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
045200     END-IF                                                               
045300     MOVE 'END S01-FETCH-REQUEST          ' TO PGM-POS                    
045400     .                                                                    
045500     SKIP3                                                                
045600 S02-RETURN-RESPONSE SECTION.                                             
045700     MOVE 'STA S02-RETURN-RESPONSE        ' TO PGM-POS                    
045800                                                                          
045900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
046000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
046100                                                                          
046200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
046300                                                                          
046400     IF SUB-KDRC > 0                                                      
046500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
046600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
046700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
046800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
046900     END-IF                                                               
047000     MOVE 'END S02-RETURN-RESPONSE        ' TO PGM-POS                    
047100     .                                                                    
047200     EJECT                                                                
047300* --- IMS SECTIONS ---                                                    
047400                                                                          
047500 IMS-GET-KREE01 SECTION.                                                  
047600                                                                          
047700     STRING 'WLKREE01(IDLEVANM =' W-IDLEVANM-X ')'                        
047800          DELIMITED BY SIZE INTO SSA1                                     
047900     MOVE '  GE' TO GOOD-STATUSCODES                                      
048000     CALL CBLTDLI USING GU KREE-PCB DLI-IO-AREA SSA1                      
048100     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
048200     PERFORM IMS-STATUSCHECK                                              
048300     .                                                                    
048400     EJECT                                                                
048500 IMS-GNP-KREE11 SECTION.                                                  
048600                                                                          
048700     STRING 'WLKREE11(WDA211KY>=' W-WDA2KEY-X ')'                         
048800          DELIMITED BY SIZE INTO SSA1                                     
048900     MOVE '  GE' TO GOOD-STATUSCODES                                      
049000     CALL CBLTDLI USING GNP KREE-PCB DLI-IO-AREA SSA1                     
049100     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
049200     PERFORM IMS-STATUSCHECK                                              
049300     .                                                                    
049400     EJECT                                                                
049500 IMS-STATUSCHECK SECTION.                                                 
049600                                                                          
049700     SET STATUS-IX TO 1                                                   
049800     SEARCH GOOD-STATUS                                                   
049900       AT END                                                             
050000         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
050100         DELIMITED BY SIZE INTO ERROR-TEXT                                
050200         CALL FELLOG                                                      
050300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
050400         CONTINUE                                                         
050500     END-SEARCH                                                           
050600     .                                                                    
