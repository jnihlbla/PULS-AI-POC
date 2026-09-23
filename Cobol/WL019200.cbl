000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL019200.                                                
000400 AUTHOR.         BERT ANDERSSON.                                          
000500 DATE-WRITTEN.   06/12/29.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME:       'CARPARTS.LDC.SHIPPINGCREATEBOLLATOTAL'                  
000900*                                                                         
001000*    FUNKTION:                                                            
001100*        LÄSER HÄNDELSEBAS OCH KONTROLLERAR INMATNING. OM INDATA          
001200*        OK SÅ STARTAS ETT BAKGRUNDS-MPP SOM SKRIVER LISTOR FÖR           
001300*        ITALIEN.                                                         
001400*                                                                         
001410******************************************************************        
001500*    OBS!WL019200 PROGRAM IS A REPLICA OF W4066700 PROGRAM                
001600*        AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS                          
001610******************************************************************        
001700*                                                                         
001800*        PROGRAMMET LÄSER      WL4491 (WDR4)                              
001900*                              WL4494 (WDR4)                              
002000*                                                                         
002100*    INDATA.                                                              
002200*        TRANSACTION: WL0192T                                             
002300*        REQUEST:     WL0192I1                                            
002400*                                                                         
002500*    OUTDATA.                                                             
002600*        RESPONSE:    WL0192O1                                            
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200 WORKING-STORAGE SECTION.                                                 
003300                                                                          
003400*    -- CHECKED BY WY2000                                                 
003500 77  IDPGM                       PIC X(08)   VALUE 'WL019200'.            
003600                                                                          
003700*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003800 77  FILLER                      PIC X(08)   VALUE 'FELTEXT:'.            
003900 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
004000 77  FILLER                      PIC X(08)   VALUE 'PGM-POS:'.            
004100 77  WS-PGM-POS                  PIC X(40)   VALUE SPACE.                 
004200 77  FILLER                      PIC X(08)   VALUE 'IMS-SEC:'.            
004300 77  WS-IMS-SEC                  PIC X(40)   VALUE SPACE.                 
004400                                                                          
004500 77  WS-ABSTRACT-ADRESS          PIC X(50)                                
004600     VALUE 'CARPARTS.LDC.SHIPPINGCREATEBOLLATOTAL'.                       
004700 77  KDRC-DISPLAY                PIC Z(5).                                
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000 77  IX                          PIC S9(4)   VALUE +0 COMP SYNC.          
005100 77  INDX                        PIC S9(4)   VALUE +0 COMP SYNC.          
005200 77  MAX-INDX                    PIC S9(4)   VALUE +30 COMP SYNC.         
005300 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005400 77  WS-DATUM                    PIC X(6)    VALUE ZERO.                  
005500 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
005600 77  WS-IDTRPTNR                 PIC Z(2)9.                               
005700 77  WS-IDMSG-INFO               PIC X(3).                                
005800 77  WS-IDELMT-ERROR             PIC X(16).                               
005900 77  WS-IDMSG-ERROR              PIC X(3).                                
006000 77  WS-RESP-KVRADER             PIC 9(4).                                
006100     SKIP2                                                                
006200 77    FILLER                    PIC X(8)    VALUE 'AAAAAAAA'.            
006300*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
006400                                                                          
006500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006600     88  NYCKLAR-OK                          VALUE 'J'.                   
006700     88  NYCKLAR-FEL                         VALUE 'N'.                   
006800                                                                          
006900 77  INPUT-SW                    PIC X       VALUE 'J'.                   
007000     88  INMATNING-OK                        VALUE 'J'.                   
007100     88  INMATNING-EJ-OK                     VALUE 'N'.                   
007200                                                                          
007300 77  INDATA-SW                   PIC X       VALUE 'J'.                   
007400     88  INDATA-OK                           VALUE 'J'.                   
007500     88  INDATA-EJ-OK                        VALUE 'N'.                   
007600                                                                          
007700 77  STARTA-PRINT-SW             PIC X       VALUE 'J'.                   
007800     88  PRINT-STARTAD                       VALUE 'J'.                   
007900                                                                          
008000 77  SHOW-SW                     PIC X       VALUE 'N'.                   
008100     88  SHOWED                              VALUE 'J'.                   
008200                                                                          
008300 77  ALLT-SW                     PIC X       VALUE 'J'.                   
008400     88  ALLT-OK                             VALUE 'J'.                   
008500                                                                          
008600 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
008700     88  EGEN-MID                            VALUE 'L192'.                
008800     88  GODK-MID                            VALUE 'L192' '4662'          
008900                                                   '4669'.                
009000     88  HELP-MID                            VALUE '0551'.                
009100     SKIP2                                                                
009200 77    FILLER                    PIC X(8)    VALUE 'SUBPGM: '.            
009300*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
009400 01  GENERELLA-SUBPROGRAM.                                                
009500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010000     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
010100     EJECT                                                                
010200*    --- PARAMETERS TO ABEND                                              
010300                                                                          
010400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010700                                                                          
010800     SKIP3                                                                
010900 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011000*01  -COPY WZ01SUB                                                        
011100     EJECT                                                                
011200                                                                          
011300 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
011400 01  REQU-AREA.                                                           
011500*    03 -COPY WZ01REQU                                                    
011600*    03 -COPY WL0192I1                                                    
011700     EJECT                                                                
011800                                                                          
011900 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
012000 01  RESP-AREA.                                                           
012100*    03 -COPY WZ01RESP                                                    
012200*    03 -COPY WL0192O1                                                    
012300     EJECT                                                                
012400                                                                          
012500 01  MESSAGE-CODES.                                                       
012600     03  ERR-WRONG-KEY           PIC X(3)    VALUE '022'.                 
012700     03  INF-PRESS-PF11          PIC X(3)    VALUE '003'.                 
012800     03  ERR-PF11-AND-NO-DATA    PIC X(3)    VALUE '011'.                 
012900     03  INF-DATA-MISSING        PIC X(3)    VALUE '041'.                 
013000     03  INF-PRINT-REQUESTED     PIC X(3)    VALUE '325'.                 
013100     03  INF-PRESS-SEARCH-OR-EXE PIC X(3)    VALUE '326'.                 
013200     03  SYSTEM-ERROR            PIC X(03)   VALUE '099'.                 
013300     EJECT                                                                
013400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
013500 01  FILLER                      PIC X(08)   VALUE 'WDATAREA'.            
013600*01 -COPY WDATAREA                                                        
013700     EJECT                                                                
013800 01    FILLER                    PIC X(8)    VALUE 'PGM-SW:'.             
013900 01  W-PROG-TO-PROG-SW-AREA.                                              
014000     03  M-SW-LL-1               PIC S9(4)   VALUE +100 COMP SYNC.        
014100     03  M-SW-Z1-Z2-1            PIC X(2)    VALUE LOW-VALUE.             
014200     03  M-SW-KDTRANS-1          PIC X(8)    VALUE 'WL0193X '.            
014300     03  M-SW-IDTRANS-1          PIC X(4)    VALUE 'L192'.                
014400     03  M-SW-KDMFSTYP-1         PIC X(1)    VALUE '2'.                   
014500     03  MID -COPY WL0193I1 -PRE L193-                                    
014600     EJECT                                                                
014700 01    FILLER                    PIC X(16)   VALUE 'TRANS-AREA '.         
014800*01    L193-TRANSAREA.                                                    
014900*  03    L193-REQU-TIDATUM       PIC 9(6)    VALUE ZERO.                  
015000*  03    L193-REQU-KDSVAR        PIC X       VALUE SPACE.                 
015100*  03    L193-REQU-IDDC          PIC X(2)    VALUE SPACE.                 
015200*  03    L193-REQU-IDTRPTNR      PIC 9(3)    VALUE ZERO.                  
015300*  03    L193-REQU-IDLBBET       PIC X(12)   VALUE SPACE.                 
015400     EJECT                                                                
015500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015600*                                                                         
015700     EJECT                                                                
015800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015900     SKIP3                                                                
016000 01  NYCKLAR-TILL-DLI.                                                    
016100     03  W-WDGXKEY-X.                                                     
016200         05  W-IDHTYP            PIC X(4)    VALUE '4491'.                
016300         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
016400         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
016500     SKIP2                                                                
016600     03  W-KY4494-MIN-X.                                                  
016700         05  W-DALASTN-MIN       PIC  9(8)   VALUE ZERO.                  
016800         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
016900                                                                          
017000     03  W-KY4494-MAX-X.                                                  
017100         05  W-DALASTN-MAX       PIC  9(8)   VALUE ZERO.                  
017200         05  FILLER              PIC X(26)   VALUE HIGH-VALUE.            
017300     EJECT                                                                
017400*    --- STATUS-KOD FRÅN IMS                                              
017500 01  STATUS-WS                   PIC XX.                                  
017600     88  SEGMENT-FINNS                       VALUE '  '.                  
017700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017900     SKIP2                                                                
018000 01  GODK-STATUSKODER.                                                    
018100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018200     SKIP3                                                                
018300 01  SSA1                        PIC X(128).                              
018400 01  SSA2                        PIC X(64).                               
018500     EJECT                                                                
018600*    --- IMS FUNKTIONSKODER                                               
018700*01  -COPY W0003                                                          
018800     EJECT                                                                
018900*    ---  DLI INPUT-OUTPUT AREA                                           
019000 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
019100     SKIP3                                                                
019200 01  DLI-IO-AREA.                                                         
019300     03  IO-AREA                 PIC X(300)  VALUE SPACE.                 
019400     SKIP3                                                                
019500     03  WL449112 REDEFINES IO-AREA.                                      
019600*        05  -COPY WDGX4494  -PRE BOLLA-                                  
019700     EJECT                                                                
019800 LINKAGE SECTION.                                                         
019900                                                                          
020000*01  -COPY W0009   -PRE MSG-                                              
020100     EJECT                                                                
020200*01  -COPY W0009   -PRE ALT-                                              
020300     EJECT                                                                
020400*01  -COPY W0008   -PRE USEA-                                             
020500     05  FILLER                  PIC X.                                   
020600     EJECT                                                                
020700*01  -COPY W0008  -PRE 4494-                                              
020800     05  FILLER                  PIC X.                                   
020900     EJECT                                                                
021000 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB 4494-PCB.                      
021100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB 4494-PCB.                      
021200                                                                          
021300     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
021400     IF SUB-KDRC = 0                                                      
021500       IF REQU-KDPGMACT = 'E' OR 'S'                                      
021600         PERFORM A-INIT                                                   
021700         PERFORM B-KOLLA-NYCKLAR                                          
021800         IF NYCKLAR-OK                                                    
021900           IF REQU-KDPGMACT = 'E'                                         
022000             PERFORM G-KOLLA-INPUT                                        
022100             IF INDATA-OK                                                 
022200               PERFORM H-STARTA-PRINTPROGRAM                              
022300             END-IF                                                       
022400           END-IF                                                         
022500           PERFORM F-LAES-VISA-INFO                                       
022600         END-IF                                                           
022700                                                                          
022800       ELSE                                                               
022900         MOVE INF-PRESS-SEARCH-OR-EXE  TO RESP-IDMSG-ERROR                
023000       END-IF                                                             
023100       MOVE RESP-IDMSG-INFO     TO WS-IDMSG-INFO                          
023200       MOVE RESP-IDMSG-ERROR    TO WS-IDMSG-ERROR                         
023300       MOVE RESP-IDELMT-ERROR   TO WS-IDELMT-ERROR                        
023400       IF WS-IDMSG-ERROR NOT = SPACE                                      
023500          MOVE WS-IDMSG-ERROR    TO RESP-IDMSG-ERROR                      
023600          MOVE WS-IDELMT-ERROR   TO RESP-IDELMT-ERROR                     
023700          IF WS-IDMSG-INFO NOT = SPACE                                    
023800            MOVE WS-IDMSG-INFO   TO RESP-IDMSG-INFO                       
023900          END-IF                                                          
024000          MOVE 001               TO RESP-IDMSGVER                         
024100       END-IF                                                             
024101                                                                          
024110       IF WS-IDMSG-ERROR NOT = SPACE                                      
024120       OR REQU-KDPGMACT = 'S'                                             
024200         PERFORM S02-RETURN-RESPONSE                                      
024300       END-IF                                                             
024310     END-IF                                                               
024400                                                                          
024500     MOVE ZERO TO RETURN-CODE                                             
024600     GOBACK                                                               
024700     .                                                                    
024800     EJECT                                                                
024900 A-INIT SECTION.                                                          
025000                                                                          
025100     MOVE JA TO NYCKLAR-SW                                                
025200                                                                          
025300     MOVE ALL '+'                         TO RESP-AREA                    
025400     MOVE 001                             TO RESP-IDMSGVER                
025500     MOVE SPACE                           TO RESP-IDMSG-ERROR             
025600                                             RESP-IDMSG-INFO              
025700                                             RESP-IDELMT-ERROR            
025800     MOVE ZERO                            TO RESP-KVRADER                 
025900                                                                          
026000     ACCEPT DAGENS-DATUM FROM DATE                                        
026100     .                                                                    
026200     EJECT                                                                
026300 B-KOLLA-NYCKLAR SECTION.                                                 
026400     MOVE 'STA B-KOLLA-NYCKLAR '       TO WS-PGM-POS                      
026500                                                                          
026600     IF REQU-TIDATUM-KEY = ALL '+'                                        
026700       MOVE DAGENS-DATUM TO WS-DATUM                                      
026800     ELSE                                                                 
026900       MOVE REQU-TIDATUM-KEY TO WS-DATUM                                  
027000       INSPECT WS-DATUM REPLACING LEADING SPACE BY ZERO                   
027100     END-IF                                                               
027200                                                                          
027300     MOVE REQU-IDDC-KEY              TO RESP-IDDC-KEY                     
027400                                        WS-IDDC                           
027500                                                                          
027600     IF WS-DATUM NUMERIC                                                  
027700       IF WS-DATUM = ZERO                                                 
027800         MOVE DAGENS-DATUM TO WS-DATUM                                    
027900       END-IF                                                             
028000                                                                          
028100       MOVE 'AAMMDD'  TO DAT-KDDATFORM                                    
028200       MOVE WS-DATUM  TO DAT-I-TIDATUM                                    
028300       CALL WDATKONV USING DAT-KDDATFORM                                  
028400                           DAT-I-TIDATUM                                  
028500                           DAT-O-TIDATUM                                  
028600                           DAT-KDSVAR                                     
028700       IF DAT-KDSVAR-OK                                                   
028800         MOVE DAT-TIAAMMDD TO W-DALASTN-MIN                               
028900                              W-DALASTN-MAX                               
029000         MOVE DAT-TISEKEL  TO W-DALASTN-MIN (1:2)                         
029100                              W-DALASTN-MAX (1:2)                         
029200         MOVE WS-DATUM     TO   RESP-TIDATUM-KEY                          
029300*        INSPECT RESP-TIDATUM-KEY REPLACING LEADING ZERO BY SPACE         
029400       ELSE                                                               
029500         MOVE NEJ TO NYCKLAR-SW                                           
029600       END-IF                                                             
029700     ELSE                                                                 
029800       MOVE NEJ TO NYCKLAR-SW                                             
029900     END-IF                                                               
030000                                                                          
030100     IF NYCKLAR-FEL                                                       
030200       MOVE 'TIAAMMDD'                  TO RESP-IDELMT-ERROR              
030300       MOVE ERR-WRONG-KEY               TO RESP-IDMSG-ERROR               
030400     END-IF                                                               
030500     MOVE 'END B-KOLLA-NYCKLAR '       TO WS-PGM-POS                      
030600     .                                                                    
030700     EJECT                                                                
030800 F-LAES-VISA-INFO SECTION.                                                
030900     MOVE 'STA F-LAES-VISA-INFO'       TO WS-PGM-POS                      
031000                                                                          
031100     MOVE WS-IDDC    TO W-IDDC                                            
031200     PERFORM IMS-GU-WL4491                                                
031300                                                                          
031400     IF SEGMENT-SAKNAS                                                    
031500* FLYTTA LÄMPLIGT FELMEDDELANDE                                           
031600       MOVE 'DATA'                     TO RESP-IDELMT-ERROR               
031700       MOVE INF-DATA-MISSING           TO RESP-IDMSG-ERROR                
031800     ELSE                                                                 
031900                                                                          
032000       MOVE +1 TO INDX                                                    
032100       PERFORM IMS-GNP-WL4494                                             
032200       PERFORM UNTIL INDX > MAX-INDX                                      
032300         IF SEGMENT-FINNS                                                 
032400           MOVE NEJ TO SHOW-SW                                            
032410                                                                          
032500           MOVE +1  TO IX                                                 
032600           PERFORM UNTIL IX = INDX OR (SHOWED)                            
032700             MOVE BOLLA-4494-IDTRPTNR  TO WS-IDTRPTNR                     
032800             IF WS-IDTRPTNR        = RESP-IDTRPTNR (IX) AND               
032900                BOLLA-4494-IDLBBET = RESP-IDLBBET (IX)                    
033000               MOVE JA               TO SHOW-SW                           
033100             END-IF                                                       
033200             ADD +1 TO IX                                                 
033300           END-PERFORM                                                    
033310                                                                          
033400           IF NOT SHOWED                                                  
033500             MOVE BOLLA-4494-IDTRPTNR  TO RESP-IDTRPTNR (INDX)            
033600             MOVE BOLLA-4494-IDLBBET   TO RESP-IDLBBET (INDX)             
033700             ADD +1 TO INDX                                               
033800             ADD  1                    TO WS-RESP-KVRADER                 
033900           END-IF                                                         
034000                                                                          
034100           PERFORM IMS-GNP-WL4494                                         
034200         ELSE                                                             
034300           ADD +1 TO INDX                                                 
034400         END-IF                                                           
034500       END-PERFORM                                                        
034600     END-IF                                                               
034700                                                                          
034800     IF WS-RESP-KVRADER > ZERO                                            
034900       MOVE WS-RESP-KVRADER            TO RESP-KVRADER                    
035000     END-IF                                                               
035100     MOVE 'END F-LAES-VISA-INFO'       TO WS-PGM-POS                      
035200     .                                                                    
035300     EJECT                                                                
035400 G-KOLLA-INPUT SECTION.                                                   
035500     MOVE 'STA G-KOLLA-INPUT '         TO WS-PGM-POS                      
035600                                                                          
035700     MOVE NEJ                          TO STARTA-PRINT-SW                 
035800     MOVE +1 TO INDX                                                      
035810                                                                          
035900     PERFORM UNTIL INDX > MAX-INDX                                        
035910                OR PRINT-STARTAD                                          
036000       IF REQU-KDSVAR (INDX) = '+' OR SPACE                               
036100          MOVE NEJ                     TO INDATA-SW                       
036200       ELSE                                                               
036210                                                                          
036300         IF REQU-KDSVAR (INDX) = 'P' OR 'F' OR 'A'                        
036400           PERFORM H-STARTA-PRINTPROGRAM                                  
036500           MOVE JA                     TO STARTA-PRINT-SW                 
036600         ELSE                                                             
036700           IF NOT PRINT-STARTAD                                           
037000             MOVE NEJ                  TO INDATA-SW                       
037100             MOVE 'KDSVAR '            TO RESP-IDELMT-ERROR               
037200             MOVE ERR-WRONG-KEY        TO RESP-IDMSG-ERROR                
037300           END-IF                                                         
037400         END-IF                                                           
037500       END-IF                                                             
037600       ADD +1 TO INDX                                                     
037700     END-PERFORM                                                          
037800     MOVE 'END G-KOLLA-INPUT '         TO WS-PGM-POS                      
037900     .                                                                    
038000     EJECT                                                                
038100 H-STARTA-PRINTPROGRAM SECTION.                                           
038200     MOVE 'STA H-STARTA-PRINTPROGRAM'  TO WS-PGM-POS                      
038300                                                                          
038400     MOVE REQU-KDSVAR (INDX)   TO L193-REQU-KDSVAR                        
038500     MOVE WS-DATUM             TO L193-REQU-TIDATUM                       
038600     MOVE REQU-IDDC-KEY        TO L193-REQU-IDDC                          
038700     INSPECT REQU-IDTRPTNR (INDX) REPLACING LEADING SPACE BY ZERO         
038800     MOVE REQU-IDTRPTNR (INDX) TO L193-REQU-IDTRPTNR                      
038900                                                                          
039500     MOVE REQU-IDLBBET(INDX)   TO L193-REQU-IDLBBET                       
039600     MOVE REQU-IDUSER          TO L193-REQU-IDUSER                        
039700     MOVE REQU-IDMSGVER        TO L193-REQU-IDMSGVER                      
039800     MOVE REQU-KDPGMACT        TO L193-REQU-KDPGMACT                      
039900                                                                          
040000     PERFORM IMS-INSERT-ALT-MSG                                           
040100                                                                          
040200     MOVE 'START PRINT'                 TO RESP-IDELMT-ERROR              
040300     MOVE INF-PRINT-REQUESTED           TO RESP-IDMSG-INFO                
040400                                                                          
040500     MOVE 'END H-STARTA-PRINTPROGRAM'  TO WS-PGM-POS                      
040600     .                                                                    
040700     EJECT                                                                
040800*    --- DISPATCHER SECTIONS                                              
040900 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
041000     MOVE 'STA S01-FETCH-REQUEST'    TO WS-PGM-POS                        
041100                                                                          
041200     MOVE 'GETARG'               TO SUB-KDFUNC                            
041300     MOVE WS-ABSTRACT-ADRESS     TO SUB-ADDISPABS                         
041400     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
041500                                                                          
041600     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
041700                                                                          
041800     IF SUB-KDRC > 0                                                      
041900       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
042000       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
042100       DELIMITED BY SIZE INTO FELTEXT                                     
042200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
042300     END-IF                                                               
042400     .                                                                    
042500     SKIP3                                                                
042600 S02-RETURN-RESPONSE SECTION.                                             
042700     MOVE 'STA S02-RETURN-RESPONSE'    TO  WS-PGM-POS                     
042800                                                                          
042900     MOVE 'RETURN'                   TO SUB-KDFUNC                        
043000     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
043100                                                                          
043200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
043300                                                                          
043400     IF SUB-KDRC > 0                                                      
043500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
043600       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
043700       DELIMITED BY SIZE INTO FELTEXT                                     
043800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
043900     END-IF                                                               
044000     .                                                                    
044100     EJECT                                                                
044200* --- IMS SEKTIONER ---                                                   
044300*                                                                         
044400*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
044500*                 III     III MM MMMMM MM SSSS   SSSS                     
044600*                 IIIII IIIII MM  MMM  MM SSS SSS SSS                     
044700*                 IIIII IIIII MM M M M MM SSS  SSSSSS                     
044800*                 IIIII IIIII MM MM MM MM SSSSSS  SSS                     
044900*                 IIIII IIIII MM MMMMM MM SSS SSS SSS                     
045000*                 III     III MM MMMMM MM SSSS   SSSS                     
045100*                 IIIIIIIIIII MMMMMMMMMMM SSSSSSSSSSS                     
045200*                                                                         
045300     SKIP3                                                                
045400 IMS-INSERT-ALT-MSG SECTION.                                              
045500     MOVE 'STA IMS-INSERT-ALT-MSG'   TO WS-IMS-SEC                        
045600                                                                          
045700     MOVE SPACE TO GODK-STATUSKODER                                       
045800     CALL CBLTDLI USING ISRT ALT-PCB W-PROG-TO-PROG-SW-AREA               
045900     MOVE ALT-STATUS-CODE TO STATUS-WS                                    
046000     PERFORM IMS-STATUSKONTROLL                                           
046100     .                                                                    
046200     EJECT                                                                
046300 IMS-GU-WL4491  SECTION.                                                  
046400     MOVE 'STA IMS-GU-WL4491     '   TO WS-IMS-SEC                        
046500                                                                          
046600     STRING 'WL449101(WDGXKEY  =' W-WDGXKEY-X ')'                         
046700          DELIMITED BY SIZE INTO SSA1                                     
046800     MOVE '  GE' TO GODK-STATUSKODER                                      
046900     CALL CBLTDLI USING GU 4494-PCB DLI-IO-AREA SSA1                      
047000     MOVE 4494-STATUS-CODE TO STATUS-WS                                   
047100     PERFORM IMS-STATUSKONTROLL                                           
047200     .                                                                    
047300     SKIP2                                                                
047400 IMS-GNP-WL4494 SECTION.                                                  
047500     MOVE 'STA IMS-GNP-WL4494    '   TO WS-IMS-SEC                        
047600                                                                          
047700     STRING 'WL449112(KY4494  >=' W-KY4494-MIN-X                          
047800                    '&KY4494  <=' W-KY4494-MAX-X ')'                      
047900          DELIMITED BY SIZE INTO SSA1                                     
048000     MOVE '  GE' TO GODK-STATUSKODER                                      
048100     CALL CBLTDLI USING GNP 4494-PCB DLI-IO-AREA SSA1                     
048200     MOVE 4494-STATUS-CODE TO STATUS-WS                                   
048300     PERFORM IMS-STATUSKONTROLL                                           
048400     .                                                                    
048500     EJECT                                                                
048600 IMS-STATUSKONTROLL SECTION.                                              
048700                                                                          
048800     SET STATUS-IX TO 1                                                   
048900     SEARCH GODK-STATUS                                                   
049000       AT END                                                             
049100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
049200         DELIMITED BY SIZE INTO FELTEXT                                   
049300         CALL FELLOG                                                      
049400       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
049500         CONTINUE                                                         
049600     END-SEARCH                                                           
049700     .                                                                    
