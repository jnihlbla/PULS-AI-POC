000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WQ0XVL2S.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   2010-10-26.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        SUB PROGRAM FOR CONVERTING A "VALUE LIST" TO CONDITIONS          
001000*        SUITABLE FOR AN SQL WHERE CLAUSE.                                
001100*                                                                         
001200*        A SINGLE VALUE "V1" BECOMES "=V1"                                
001300*        A LIST OF VALUES "V1,V2,V3..." BECOMES "IN(V1,V2,V3...)"         
001400*        INTERVALS "V1-V2" BECOMES "BETWEEN V1 AND V2"                    
001500*        COMBINATIONS OF THESE VARIANTS ARE JOINED TOGETHER INTO          
001600*        ONE LONG CONDITION STRING.                                       
001700*                                                                         
001800                                                                          
001900     EJECT                                                                
002000 DATA DIVISION.                                                           
002100 WORKING-STORAGE SECTION.                                                 
002200                                                                          
002300 77  IDPGM                       PIC X(8)    VALUE 'WQ0XVL2S'.            
002400 77  YES                         PIC X       VALUE 'J'.                   
002500 77  NOO                         PIC X       VALUE 'N'.                   
002600 77  FEL                         PIC X       VALUE 'F'.                   
002700 77  KOMMA                       PIC X       VALUE ','.                   
002800 77  MINUS                       PIC X       VALUE '-'.                   
002900 77  ENOUGH-SPACES               PIC X(6)    VALUE '      '.              
003000                                                                          
003100 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
003200 01  FILLER REDEFINES TODAYS-DATE.                                        
003300     03  TODAYS-DATE-YEAR        PIC 9(2).                                
003400     03  TODAYS-DATE-MONTH       PIC 9(2).                                
003500     03  TODAYS-DATE-DAY         PIC 9(2).                                
003600                                                                          
003700                                                                          
003800 01  GENERAL-SUBPROGRAMS.                                                 
003900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
004000                                                                          
004100*    --- PARAMETERS TO ABEND                                              
004200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
004300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
004400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
004500                                                                          
004600 01  ERROR-TEXT.                                                          
004700     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
004800     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
004900                                                                          
005000     EJECT                                                                
005100 01  IN-LIST                     PIC X(980).                              
005200 01  IN-POS                      PIC S9(4) BINARY VALUE ZERO.             
005300 01  SINGLE-COUNT                PIC S9(4) BINARY VALUE ZERO.             
005400 01  MINUS-COUNT                 PIC S9(4) BINARY VALUE ZERO.             
005500                                                                          
005600 01  W-OR                        PIC X(3).                                
005700                                                                          
005800 01  W-CORREL                    PIC X(2).                                
005900                                                                          
006000 01  W-VALUE                     PIC X(50).                               
006100 01  W-VALUE-1                   PIC X(50).                               
006200 01  W-VALUE-2                   PIC X(50).                               
006300 01  WW-VALUE                    PIC X(50).                               
006400                                                                          
006500 01  WS-RUNDATUM-1               PIC X(6)  VALUE SPACE.                   
006600 01  WS-RUNDATUM-2               PIC X(6)  VALUE SPACE.                   
006700 01  WS-RUNDATUM-START           PIC X(6)  VALUE SPACE.                   
006800 01  WS-RUNDATUM-END             PIC X(6)  VALUE SPACE.                   
006900 01  WS-TIMEPERIOD               PIC X(1)  VALUE SPACE.                   
007000 01  WS-BACK-OR-FORTH            PIC X(1)  VALUE SPACE.                   
007100                                                                          
007200 01  WS-COMPUTE-DATE             PIC S9(6) BINARY VALUE ZERO.             
007300 01  WS-COMPUTE-M                PIC S9(4) BINARY VALUE ZERO.             
007400 01  WS-COMPUTE-P                PIC S9(4) BINARY VALUE ZERO.             
007500 01  WS-COMPUTE-T                PIC X(4)  VALUE SPACE.                   
007600                                                                          
007700 01  W-TODAYS-DATE               PIC S9(6) BINARY VALUE ZERO.             
007800 01  W-TODAYS-WEEK               PIC S9(4) BINARY VALUE ZERO.             
007900 01  W-TODAYS-MONTH              PIC S9(4) BINARY VALUE ZERO.             
008000 01  W-TODAYS-PERIOD             PIC S9(4) BINARY VALUE ZERO.             
008100 01  W-TODAYS-YEAR               PIC S9(2) BINARY VALUE ZERO.             
008200 01  W-NEW-YEAR                  PIC S9(2) BINARY VALUE ZERO.             
008300                                                                          
008400 01  IPOS                        PIC S9(4) BINARY VALUE ZERO.             
008500 01  ILEN                        PIC S9(4) BINARY VALUE ZERO.             
008600 01  IPOS2                       PIC S9(4) BINARY VALUE ZERO.             
008700 01  ILEN2                       PIC S9(4) BINARY VALUE ZERO.             
008800 01  OPOS                        PIC S9(4) BINARY VALUE ZERO.             
008900                                                                          
009000 01  GENERAL-SUBPROGRAMS.                                                 
009100     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
009200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009300     SKIP3                                                                
009400                                                                          
009500*    -COPY WZ20DAYS                                                       
009600     EJECT                                                                
009700                                                                          
009800*01  -COPY WDATAREA                                                       
009900     EJECT                                                                
010000                                                                          
010100     EJECT                                                                
010200 LINKAGE SECTION.                                                         
010300                                                                          
010400*    -COPY WQ0XVL2S                                                       
010500     EJECT                                                                
010600 PROCEDURE DIVISION USING VL2S-WQ0XVL2S.                                  
010700 MAIN SECTION.                                                            
010800                                                                          
010900     PERFORM A-INIT                                                       
011000                                                                          
011100     PERFORM UNTIL IPOS > LENGTH OF VL2S-TEELMTVAL                        
011200                                                                          
011300       MOVE ZERO TO ILEN                                                  
011400       INSPECT VL2S-TEELMTVAL(IPOS:)                                      
011500         TALLYING ILEN                                                    
011600         FOR CHARACTERS BEFORE KOMMA                                      
011700                                                                          
011800       IF ILEN > 0 AND VL2S-TEELMTVAL(IPOS:ILEN) NOT = SPACE              
011900         PERFORM B-PROCESS-ONE-CSV-VALUE                                  
012000       ELSE                                                               
012100*        -- NOTHING BETWEEN COMMA CHARACTERS                              
012200         MOVE FEL TO VL2S-KDSVAR                                          
012300       END-IF                                                             
012400                                                                          
012500       COMPUTE IPOS = IPOS + ILEN + 1                                     
012600     END-PERFORM                                                          
012700                                                                          
012800                                                                          
012900     IF VL2S-KDSVAR NOT = SPACE                                           
013000       MOVE SPACE TO VL2S-WHERE-COND                                      
013100     ELSE                                                                 
013200       PERFORM C-FINAL-PROCESSING                                         
013300     END-IF                                                               
013400                                                                          
013500     MOVE ZERO TO RETURN-CODE                                             
013600     GOBACK                                                               
013700     .                                                                    
013800                                                                          
013900     EJECT                                                                
014000 A-INIT SECTION.                                                          
014100                                                                          
014200     MOVE 1      TO IPOS  OPOS  IN-POS                                    
014300     MOVE ZERO   TO SINGLE-COUNT                                          
014400     MOVE SPACE  TO IN-LIST                                               
014500     MOVE SPACE  TO VL2S-KDSVAR VL2S-WHERE-COND                           
014600     MOVE SPACE  TO W-OR                                                  
014700                                                                          
014800     IF VL2S-TECORREL NOT = SPACE AND LOW-VALUE                           
014900       STRING VL2S-TECORREL '.'                                           
015000       DELIMITED BY SIZE                                                  
015100       INTO W-CORREL                                                      
015200     ELSE                                                                 
015300       MOVE SPACE TO W-CORREL                                             
015400     END-IF                                                               
015500                                                                          
015600*    -- START WITH AN OPENING BANANA                                      
015700     MOVE '(' TO VL2S-WHERE-COND                                          
015800     MOVE 2 TO OPOS                                                       
015900                                                                          
016000                                                                          
016100                                                                          
016200     MOVE FUNCTION CURRENT-DATE (3:6) TO W-TODAYS-DATE                    
016300                                                                          
016400     MOVE 'IDAG' TO DAT-KDDATFORM                                         
016500     CALL WDATKONV USING DAT-KDDATFORM                                    
016600                         DAT-I-TIDATUM                                    
016700                         DAT-O-TIDATUM                                    
016800                         DAT-KDSVAR                                       
016900                                                                          
017000     MOVE DAT-TIAAVVD(1:4)      TO W-TODAYS-WEEK                          
017100     MOVE DAT-TIAARP            TO W-TODAYS-PERIOD                        
017200     MOVE DAT-TIAAMMDD(1:4)     TO W-TODAYS-MONTH                         
017300     MOVE DAT-TIAAMMDD(1:2)     TO W-TODAYS-YEAR                          
017400     .                                                                    
017500                                                                          
017600     EJECT                                                                
017700 B-PROCESS-ONE-CSV-VALUE SECTION.                                         
017800                                                                          
017900*    ONE VALUE IN A COMMA-SEPARATED LIST HAS BEEN FOUND.                  
018000*    CHECK IF IT IS A SINGLE VALUE OR INTERVAL                            
018100*    SINGLE VALUES ARE SAVED FOR A "IN" CLAUSE                            
018200*    INTERVALS ARE OUTPUT AS A BETWEEN CLAUSE                             
018300                                                                          
018400     MOVE '999999'   TO WS-RUNDATUM-START                                 
018500     MOVE '000000'   TO WS-RUNDATUM-END                                   
018600     MOVE SPACE      TO W-VALUE                                           
018700                        W-VALUE-1                                         
018800                        W-VALUE-2                                         
018900                                                                          
019000     MOVE ZERO TO MINUS-COUNT                                             
019100     INSPECT VL2S-TEELMTVAL(IPOS:ILEN)                                    
019200       TALLYING MINUS-COUNT FOR ALL MINUS                                 
019300                                                                          
019400     EVALUATE MINUS-COUNT                                                 
019500       WHEN 0                                                             
019600*         -- SINGLE VALUE                                                 
019700          MOVE VL2S-TEELMTVAL(IPOS:ILEN) TO W-VALUE                       
019800          PERFORM S05-CHECK-AND-CONVERT                                   
019900       WHEN 1                                                             
020000*         -- INTERVAL                                                     
020100          UNSTRING VL2S-TEELMTVAL(IPOS:ILEN)                              
020200            DELIMITED BY MINUS INTO W-VALUE-1 W-VALUE-2                   
020300                                                                          
020400          MOVE W-VALUE-1 TO W-VALUE                                       
020500          PERFORM S05-CHECK-AND-CONVERT                                   
020600                                                                          
020700          IF VL2S-KDSVAR = SPACE                                          
020800            MOVE W-VALUE-2 TO W-VALUE                                     
020900            PERFORM S05-CHECK-AND-CONVERT                                 
021000          END-IF                                                          
021100                                                                          
021200       WHEN OTHER                                                         
021300*         -- MORE THAN ONE MINUS IS INVALID                               
021400          MOVE FEL TO VL2S-KDSVAR                                         
021500     END-EVALUATE                                                         
021600                                                                          
021700*     -- CHECK IF DATE VALUES HAVE BEEN COMPUTED                          
021800      IF WS-RUNDATUM-START NOT = '999999'                                 
021900         MOVE WS-RUNDATUM-START TO W-VALUE-1                              
022000      END-IF                                                              
022100      IF WS-RUNDATUM-END   NOT = '000000'                                 
022200         MOVE WS-RUNDATUM-END   TO W-VALUE-2                              
022300      END-IF                                                              
022400                                                                          
022500                                                                          
022600*    -- NOTE: MINUS-COUNT MAY HAVE BEEN CHANGED IF RELATIVE               
022700*    -- DATES WERE USED. A SINGLE VALUE MAY NOW BE AN INTERVAL            
022800*    -- AND NEW START AND END-VALUES MAY HAVE BEEN COMPUTED               
022900     EVALUATE MINUS-COUNT                                                 
023000       WHEN 0                                                             
023100*        -- SINGLE VALUE                                                  
023200         PERFORM S01-MINUS-COUNT-ZERO                                     
023300                                                                          
023400       WHEN 1                                                             
023500*        -- INTERVAL                                                      
023600         PERFORM S02-MINUS-COUNT-ONE                                      
023700     END-EVALUATE                                                         
023800     .                                                                    
023900                                                                          
024000     EJECT                                                                
024100 C-FINAL-PROCESSING SECTION.                                              
024200                                                                          
024300     EVALUATE SINGLE-COUNT                                                
024400       WHEN 0                                                             
024500*        -- NO SINGLE VALUES                                              
024600         CONTINUE                                                         
024700                                                                          
024800       WHEN 1                                                             
024900*        -- ONE SINGLE VALUE, MAKE A SIMPLE = TEST                        
025000         STRING W-OR   DELIMITED BY '  '                                  
025100           W-CORREL    DELIMITED BY SPACE                                 
025200           VL2S-IDELMT DELIMITED BY SPACE                                 
025300           '='     DELIMITED BY SIZE                                      
025400           IN-LIST DELIMITED BY ENOUGH-SPACES                             
025500           INTO VL2S-WHERE-COND WITH POINTER OPOS                         
025600                                                                          
025700       WHEN OTHER                                                         
025800*        -- MULTIPLE VALUES, MAKE AN IN(...) CLAUSE                       
025900         STRING W-OR   DELIMITED BY '  '                                  
026000           W-CORREL    DELIMITED BY SPACE                                 
026100           VL2S-IDELMT DELIMITED BY SPACE                                 
026200           ' IN('  DELIMITED BY SIZE                                      
026300           IN-LIST DELIMITED BY ENOUGH-SPACES                             
026400           ')'     DELIMITED BY SIZE                                      
026500           INTO VL2S-WHERE-COND WITH POINTER OPOS                         
026600     END-EVALUATE                                                         
026700                                                                          
026800*    -- END WITH A CLOSING BANANA                                         
026900     STRING ')' DELIMITED  BY SIZE                                        
027000       INTO VL2S-WHERE-COND WITH POINTER OPOS                             
027100     .                                                                    
027200     EJECT                                                                
027300                                                                          
027400 S01-MINUS-COUNT-ZERO SECTION.                                            
027500*    MAKE A COMMA-SEPARATED LIST OF ALL SINGLE VALUES.                    
027600*    IF ONLY ONE VALUE, IT WILL LATER BE USED IN A SIMPLE = TEST          
027700*    IF MANY VALUES, THE LIST WILL LATER BE USED IN A IN(...)             
027800*    CLAUSE.                                                              
027900     ADD 1 TO SINGLE-COUNT                                                
028000     IF SINGLE-COUNT = 1                                                  
028100       STRING W-VALUE DELIMITED BY ENOUGH-SPACES                          
028200              INTO IN-LIST WITH POINTER IN-POS                            
028300     ELSE                                                                 
028400*      SKIP VALUES IF THE LIST IS TOO LONG.                               
028500       MOVE ZERO TO ILEN2                                                 
028600       INSPECT W-VALUE  TALLYING ILEN2                                    
028700               FOR CHARACTERS BEFORE ENOUGH-SPACES                        
028800       IF IN-POS + ILEN2 < LENGTH OF IN-LIST                              
028900          STRING KOMMA  DELIMITED BY SIZE                                 
029000                 W-VALUE DELIMITED BY ENOUGH-SPACES                       
029100              INTO IN-LIST WITH POINTER IN-POS                            
029200       END-IF                                                             
029300     END-IF                                                               
029400     .                                                                    
029500     EJECT                                                                
029600                                                                          
029700 S02-MINUS-COUNT-ONE  SECTION.                                            
029800     STRING W-OR   DELIMITED BY '  '                                      
029900            W-CORREL    DELIMITED BY SPACE                                
030000            VL2S-IDELMT DELIMITED BY SPACE                                
030100            ' BETWEEN ' DELIMITED BY SIZE                                 
030200            W-VALUE-1   DELIMITED BY ENOUGH-SPACES                        
030300            ' AND '     DELIMITED BY SIZE                                 
030400            W-VALUE-2   DELIMITED BY ENOUGH-SPACES                        
030500            ' '         DELIMITED BY SIZE                                 
030600            INTO VL2S-WHERE-COND WITH POINTER OPOS                        
030700            MOVE 'OR '   TO W-OR                                          
030800     .                                                                    
030900     EJECT                                                                
031000                                                                          
031100 S05-CHECK-AND-CONVERT  SECTION.                                          
031200                                                                          
031300     MOVE 1 TO IPOS2                                                      
031400     INSPECT W-VALUE  TALLYING IPOS2                                      
031500             FOR LEADING SPACE                                            
031600     MOVE ZERO TO ILEN2                                                   
031700     INSPECT W-VALUE(IPOS2:)  TALLYING ILEN2                              
031800             FOR CHARACTERS BEFORE SPACE                                  
031900                                                                          
032000     EVALUATE VL2S-KDFORMAT                                               
032100       WHEN 'AN'                                                          
032200           CONTINUE                                                       
032300                                                                          
032400       WHEN 'NU'                                                          
032500          IF W-VALUE(IPOS2:ILEN2) NOT NUMERIC                             
032600            MOVE FEL TO VL2S-KDSVAR                                       
032700          END-IF                                                          
032800                                                                          
032900       WHEN 'D6'                                                          
033000         MOVE '999999'   TO WS-RUNDATUM-1                                 
033100         MOVE '000000'   TO WS-RUNDATUM-2                                 
033200         IF W-VALUE(IPOS2:ILEN2) NUMERIC                                  
033300           IF ILEN2 NOT = 6                                               
033400             MOVE FEL TO VL2S-KDSVAR                                      
033500           ELSE                                                           
033600*            -- TREAT SINGLE VALUE AS AN INTERVAL                         
033700             MOVE W-VALUE(IPOS2:ILEN2) TO WS-RUNDATUM-1                   
033800                                          WS-RUNDATUM-2                   
033900           END-IF                                                         
034000         ELSE                                                             
034100           PERFORM S10-CHANGE-TO-REAL-DATE                                
034200*          -- SINGLE VALUE IS NOW A COMPUTED INTERVAL                     
034300           MOVE 1 TO MINUS-COUNT                                          
034400         END-IF                                                           
034500                                                                          
034600         IF WS-RUNDATUM-1 < WS-RUNDATUM-START                             
034700            MOVE WS-RUNDATUM-1 TO WS-RUNDATUM-START                       
034800         END-IF                                                           
034900         IF WS-RUNDATUM-2 > WS-RUNDATUM-END                               
035000            MOVE WS-RUNDATUM-2 TO WS-RUNDATUM-END                         
035100         END-IF                                                           
035200                                                                          
035300     END-EVALUATE                                                         
035400     .                                                                    
035500     EJECT                                                                
035600                                                                          
035700 S10-CHANGE-TO-REAL-DATE SECTION.                                         
035800* -- A NON-NUMERIC RELATIVE DATE FORMAT HAS BEEN ENTERED                  
035900* -- VALID RELATIVE FORMATS ARE [B/F]9..{Y/M/P/W}                         
036000*    PREFIX B OR F (BACK OR FORWARD) MAY BE OMITTED, DEFAULT IS B         
036100*    NUMERIC EQUALS NUMBER OF TIME PERIODS                                
036200*    TIME PERIOD: D=DAYS, W=WEEKS, M=MONTH, P=PERIOD, Y=YEARS             
036300*    THIS SINGLE RELATIVE DATE WILL BE INTERPRETED AS                     
036400*    AN INTERVAL.                                                         
036500                                                                          
036600     PERFORM S10A-EXTRACT-DATE-PARTS                                      
036700                                                                          
036800     IF (WS-BACK-OR-FORTH = 'B' OR 'F')                                   
036900     AND (WS-TIMEPERIOD = 'D' OR 'W' OR 'M' OR 'P' OR 'Y'                 
037000                              OR 'V'               OR 'Å' )               
037100       EVALUATE WS-TIMEPERIOD                                             
037200        WHEN 'D'                                                          
037300          PERFORM S10D-COMPUTE-DAY                                        
037400        WHEN 'W'                                                          
037500        WHEN 'V'                                                          
037600          PERFORM S10W-COMPUTE-WEEK                                       
037700        WHEN 'M'                                                          
037800          PERFORM S10M-COMPUTE-MONTH                                      
037900        WHEN 'P'                                                          
038000          PERFORM S10P-COMPUTE-PERIOD                                     
038100        WHEN 'Y'                                                          
038200        WHEN 'Å'                                                          
038300          PERFORM S10Y-COMPUTE-YEAR                                       
038400        WHEN OTHER                                                        
038500          MOVE FEL TO VL2S-KDSVAR                                         
038600       END-EVALUATE                                                       
038700                                                                          
038800     ELSE                                                                 
038900       MOVE FEL TO VL2S-KDSVAR                                            
039000     END-IF                                                               
039100     .                                                                    
039200     EJECT                                                                
039300                                                                          
039400 S10A-EXTRACT-DATE-PARTS SECTION.                                         
039500*    -- REMOVE LEADING SPACE                                              
039600     MOVE W-VALUE(IPOS2:ILEN2) TO WW-VALUE                                
039700                                                                          
039800     IF WW-VALUE(1:1) NUMERIC                                             
039900       MOVE 'B' TO WS-BACK-OR-FORTH                                       
040000       IF WW-VALUE(1:2) NUMERIC                                           
040100         IF WW-VALUE(1:3) NUMERIC                                         
040200           MOVE WW-VALUE(1:3) TO WS-COMPUTE-DATE                          
040300           MOVE WW-VALUE(4:1) TO WS-TIMEPERIOD                            
040400         ELSE                                                             
040500           MOVE WW-VALUE(1:2) TO WS-COMPUTE-DATE                          
040600           MOVE WW-VALUE(3:1) TO WS-TIMEPERIOD                            
040700         END-IF                                                           
040800       ELSE                                                               
040900         MOVE WW-VALUE(1:1)      TO WS-COMPUTE-DATE                       
041000         MOVE WW-VALUE(2:1)      TO WS-TIMEPERIOD                         
041100       END-IF                                                             
041200     ELSE                                                                 
041300       MOVE WW-VALUE(1:1) TO WS-BACK-OR-FORTH                             
041400       IF WW-VALUE(2:1) NUMERIC                                           
041500         IF WW-VALUE(2:2) NUMERIC                                         
041600           IF WW-VALUE(2:3) NUMERIC                                       
041700             MOVE WW-VALUE(2:3) TO WS-COMPUTE-DATE                        
041800             MOVE WW-VALUE(5:1) TO WS-TIMEPERIOD                          
041900           ELSE                                                           
042000             MOVE WW-VALUE(2:2) TO WS-COMPUTE-DATE                        
042100             MOVE WW-VALUE(4:1) TO WS-TIMEPERIOD                          
042200           END-IF                                                         
042300         ELSE                                                             
042400           MOVE WW-VALUE(2:1) TO WS-COMPUTE-DATE                          
042500           MOVE WW-VALUE(3:1) TO WS-TIMEPERIOD                            
042600         END-IF                                                           
042700       END-IF                                                             
042800     END-IF                                                               
042900     .                                                                    
043000     EJECT                                                                
043100                                                                          
043200 S10D-COMPUTE-DAY SECTION.                                                
043300     IF WS-BACK-OR-FORTH = 'B'                                            
043400       IF WS-COMPUTE-DATE = ZERO                                          
043500         CONTINUE                                                         
043600       ELSE                                                               
043700         COMPUTE WS-COMPUTE-DATE = WS-COMPUTE-DATE * -1                   
043800       END-IF                                                             
043900     END-IF                                                               
044000     MOVE W-TODAYS-DATE       TO DAYS-TIDATE1                             
044100     MOVE 'YYMMDD'            TO DAYS-KDDATFMT1                           
044200     MOVE WS-COMPUTE-DATE     TO DAYS-KVDAYS                              
044300     MOVE ' '                 TO DAYS-IDCALEND                            
044400     MOVE SPACE               TO DAYS-TIDATE2                             
044500     MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                           
044600     CALL WZ20DAYS USING                                                  
044700          DAYS-WZ20DAYS                                                   
044800     IF DAYS-KDRC = ZERO                                                  
044900       MOVE DAYS-TIDATE2      TO WS-RUNDATUM-1                            
045000       MOVE DAYS-TIDATE2      TO WS-RUNDATUM-2                            
045100     END-IF                                                               
045200     .                                                                    
045300     EJECT                                                                
045400                                                                          
045500 S10W-COMPUTE-WEEK SECTION.                                               
045600     IF WS-BACK-OR-FORTH = 'B'                                            
045700       IF WS-COMPUTE-DATE = ZERO                                          
045800         CONTINUE                                                         
045900       ELSE                                                               
046000         COMPUTE WS-COMPUTE-DATE = WS-COMPUTE-DATE * -1                   
046100       END-IF                                                             
046200     END-IF                                                               
046300     IF WS-COMPUTE-DATE = ZERO                                            
046400       MOVE ZERO TO WS-COMPUTE-DATE                                       
046500     ELSE                                                                 
046600       COMPUTE WS-COMPUTE-DATE = WS-COMPUTE-DATE * 7                      
046700     END-IF                                                               
046800     MOVE W-TODAYS-WEEK       TO DAYS-TIDATE1                             
046900     MOVE 'YYWW'              TO DAYS-KDDATFMT1                           
047000     MOVE WS-COMPUTE-DATE     TO DAYS-KVDAYS                              
047100     MOVE ' '                 TO DAYS-IDCALEND                            
047200     MOVE SPACE               TO DAYS-TIDATE2                             
047300     MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                           
047400     CALL WZ20DAYS USING                                                  
047500          DAYS-WZ20DAYS                                                   
047600     IF DAYS-KDRC = ZERO                                                  
047700       MOVE DAYS-TIDATE2      TO WS-RUNDATUM-1                            
047800     END-IF                                                               
047900                                                                          
048000     MOVE WS-RUNDATUM-1       TO DAYS-TIDATE1                             
048100     MOVE 'YYMMDD'            TO DAYS-KDDATFMT1                           
048200     MOVE 6                   TO DAYS-KVDAYS                              
048300     MOVE ' '                 TO DAYS-IDCALEND                            
048400     MOVE SPACE               TO DAYS-TIDATE2                             
048500     MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                           
048600     CALL WZ20DAYS USING                                                  
048700          DAYS-WZ20DAYS                                                   
048800     IF DAYS-KDRC = ZERO                                                  
048900       MOVE DAYS-TIDATE2      TO WS-RUNDATUM-2                            
049000     END-IF                                                               
049100     .                                                                    
049200     EJECT                                                                
049300                                                                          
049400 S10M-COMPUTE-MONTH SECTION.                                              
049500     IF WS-BACK-OR-FORTH = 'B'                                            
049600       MOVE W-TODAYS-MONTH TO WS-COMPUTE-M                                
049700       IF WS-COMPUTE-DATE = ZERO                                          
049800         MOVE WS-COMPUTE-M TO WS-COMPUTE-T                                
049900       ELSE                                                               
050000         PERFORM UNTIL WS-COMPUTE-DATE = 0                                
050100           COMPUTE WS-COMPUTE-M = WS-COMPUTE-M - 1                        
050200           MOVE WS-COMPUTE-M TO WS-COMPUTE-T                              
050300           IF WS-COMPUTE-T(3:2) = '00'                                    
050400             COMPUTE WS-COMPUTE-M = WS-COMPUTE-M - 88                     
050500           END-IF                                                         
050600           COMPUTE WS-COMPUTE-DATE = WS-COMPUTE-DATE - 1                  
050700           MOVE WS-COMPUTE-M TO WS-COMPUTE-T                              
050800         END-PERFORM                                                      
050900       END-IF                                                             
051000     END-IF                                                               
051100     IF WS-BACK-OR-FORTH = 'F'                                            
051200       MOVE W-TODAYS-MONTH TO WS-COMPUTE-M                                
051300       IF WS-COMPUTE-DATE = ZERO                                          
051400         MOVE WS-COMPUTE-M TO WS-COMPUTE-T                                
051500       ELSE                                                               
051600         PERFORM UNTIL WS-COMPUTE-DATE = 0                                
051700           COMPUTE WS-COMPUTE-M = WS-COMPUTE-M + 1                        
051800           MOVE WS-COMPUTE-M TO WS-COMPUTE-T                              
051900           IF WS-COMPUTE-T(3:2) = '13'                                    
052000             COMPUTE WS-COMPUTE-M = WS-COMPUTE-M + 88                     
052100           END-IF                                                         
052200           COMPUTE WS-COMPUTE-DATE = WS-COMPUTE-DATE - 1                  
052300           MOVE WS-COMPUTE-M TO WS-COMPUTE-T                              
052400         END-PERFORM                                                      
052500       END-IF                                                             
052600     END-IF                                                               
052700     MOVE WS-COMPUTE-T      TO WS-RUNDATUM-1(1:4)                         
052800     MOVE '01'              TO WS-RUNDATUM-1(5:2)                         
052900                                                                          
053000     MOVE WS-RUNDATUM-1(1:4)       TO WS-RUNDATUM-2                       
053100     MOVE '31'                     TO WS-RUNDATUM-2(5:2)                  
053200     .                                                                    
053300     EJECT                                                                
053400                                                                          
053500 S10P-COMPUTE-PERIOD SECTION.                                             
053600     IF WS-BACK-OR-FORTH = 'B'                                            
053700       MOVE W-TODAYS-PERIOD TO WS-COMPUTE-P                               
053800       IF WS-COMPUTE-DATE = ZERO                                          
053900         MOVE WS-COMPUTE-P TO WS-COMPUTE-T                                
054000       ELSE                                                               
054100         PERFORM UNTIL WS-COMPUTE-DATE = 0                                
054200           COMPUTE WS-COMPUTE-P = WS-COMPUTE-P - 1                        
054300           MOVE WS-COMPUTE-P TO WS-COMPUTE-T                              
054400           IF WS-COMPUTE-T(3:2) = '00'                                    
054500             COMPUTE WS-COMPUTE-P = WS-COMPUTE-P - 88                     
054600           END-IF                                                         
054700           COMPUTE WS-COMPUTE-DATE = WS-COMPUTE-DATE - 1                  
054800           MOVE WS-COMPUTE-P TO WS-COMPUTE-T                              
054900         END-PERFORM                                                      
055000       END-IF                                                             
055100     END-IF                                                               
055200     IF WS-BACK-OR-FORTH = 'F'                                            
055300       MOVE W-TODAYS-PERIOD TO WS-COMPUTE-P                               
055400       IF WS-COMPUTE-DATE = ZERO                                          
055500         MOVE WS-COMPUTE-P TO WS-COMPUTE-T                                
055600       ELSE                                                               
055700         PERFORM UNTIL WS-COMPUTE-DATE = 0                                
055800           COMPUTE WS-COMPUTE-P = WS-COMPUTE-P + 1                        
055900           MOVE WS-COMPUTE-P TO WS-COMPUTE-T                              
056000           IF WS-COMPUTE-T(3:2) = '13'                                    
056100             COMPUTE WS-COMPUTE-P = WS-COMPUTE-P + 88                     
056200           END-IF                                                         
056300           COMPUTE WS-COMPUTE-DATE = WS-COMPUTE-DATE - 1                  
056400           MOVE WS-COMPUTE-P TO WS-COMPUTE-T                              
056500         END-PERFORM                                                      
056600       END-IF                                                             
056700     END-IF                                                               
056800                                                                          
056900     MOVE 'AARP'       TO DAT-KDDATFORM                                   
057000     MOVE WS-COMPUTE-T TO DAT-I-TIDATUM                                   
057100     CALL WDATKONV USING DAT-KDDATFORM                                    
057200                         DAT-I-TIDATUM                                    
057300                         DAT-O-TIDATUM                                    
057400                         DAT-KDSVAR                                       
057500                                                                          
057600     MOVE DAT-TIAAMMDD(1:6)     TO WS-RUNDATUM-1                          
057700                                                                          
057800     COMPUTE WS-COMPUTE-P = WS-COMPUTE-P + 1                              
057900     MOVE WS-COMPUTE-P TO WS-COMPUTE-T                                    
058000     IF WS-COMPUTE-T(3:2) = '13'                                          
058100       COMPUTE WS-COMPUTE-P = WS-COMPUTE-P + 88                           
058200     END-IF                                                               
058300     MOVE WS-COMPUTE-P TO WS-COMPUTE-T                                    
058400                                                                          
058500     MOVE 'AARP'       TO DAT-KDDATFORM                                   
058600     MOVE WS-COMPUTE-T TO DAT-I-TIDATUM                                   
058700     CALL WDATKONV USING DAT-KDDATFORM                                    
058800                         DAT-I-TIDATUM                                    
058900                         DAT-O-TIDATUM                                    
059000                         DAT-KDSVAR                                       
059100                                                                          
059200     MOVE DAT-TIAAMMDD(1:6)   TO WS-RUNDATUM-2                            
059300                                                                          
059400     MOVE WS-RUNDATUM-2(1:6)  TO DAYS-TIDATE2                             
059500     MOVE 'YYMMDD'            TO DAYS-KDDATFMT2                           
059600     MOVE 1                   TO DAYS-KVDAYS                              
059700     MOVE ' '                 TO DAYS-IDCALEND                            
059800     MOVE SPACE               TO DAYS-TIDATE1                             
059900     MOVE 'YYMMDD'            TO DAYS-KDDATFMT1                           
060000     CALL WZ20DAYS USING                                                  
060100          DAYS-WZ20DAYS                                                   
060200     IF DAYS-KDRC = ZERO                                                  
060300       MOVE DAYS-TIDATE1      TO WS-RUNDATUM-2(1:6)                       
060400     END-IF                                                               
060500     .                                                                    
060600     EJECT                                                                
060700                                                                          
060800 S10Y-COMPUTE-YEAR SECTION.                                               
060900     IF WS-BACK-OR-FORTH = 'B'                                            
061000       IF WS-COMPUTE-DATE = ZERO                                          
061100         CONTINUE                                                         
061200       ELSE                                                               
061300         COMPUTE WS-COMPUTE-DATE = WS-COMPUTE-DATE * -1                   
061400       END-IF                                                             
061500     END-IF                                                               
061600     COMPUTE W-NEW-YEAR = W-TODAYS-YEAR +                                 
061700                          WS-COMPUTE-DATE                                 
061800     MOVE W-NEW-YEAR          TO WS-RUNDATUM-1(1:2)                       
061900     MOVE '0101'              TO WS-RUNDATUM-1(3:4)                       
062000                                                                          
062100     MOVE W-NEW-YEAR          TO WS-RUNDATUM-2(1:2)                       
062200     MOVE '1231'              TO WS-RUNDATUM-2(3:4)                       
062300     .                                                                    
062400                                                                          
