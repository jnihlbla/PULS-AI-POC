000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W271BUYR.                                                
000400 AUTHOR.         CHESTER COUCH.                                           
000500 DATE-WRITTEN.   22/10/05.                                                
000600 DATE-COMPILED.                                                           
000710                                                                          
000800*    FUNKTION:                                                            
000900*        SUBPROGRAM TO DETERMINE AND RETURN                               
001000*           IDPERSON-BUY (A.K.A. BUYER, REFILL SEGMENTATION ID)           
001100*          AND                                                            
001200*           IDREFTAB                                                      
001300*                                                                         
001400*                                                                         
001500*        PROGRAM READS     WDB6                                           
001600*                          WDL7                                           
001700*                          WDL8                                           
001800*                                                                         
001900*        PROGRAM UPDATES   NONE                                           
002000*                                                                         
002100*                                                                         
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 DATA DIVISION.                                                           
002600                                                                          
002700 WORKING-STORAGE SECTION.                                                 
002800     SKIP3                                                                
002900 77  IDPGM                       PIC X(8)    VALUE 'W271BUYR'.            
003000 77  JA                          PIC X       VALUE 'J'.                   
003100 77  NEJ                         PIC X       VALUE 'N'.                   
003200     EJECT                                                                
003300                                                                          
003400 01  WORKING-FIELDS.                                                      
003500     03 WS-KVOT                  PIC S9(7)   VALUE ZERO COMP-3.           
003600     03 INDX                     PIC  9(2)   VALUE ZERO.                  
003700                                                                          
003800 01  SWITCHES.                                                            
003900     03 SW-EXTENDED-REFILL            PIC X(1)  VALUE SPACES.             
004000     03 SW-SET-VOL-AND-LIFECYCLE      PIC X(1)  VALUE SPACES.             
004100     03 SW-SOP-2-YEARS-AGO-AT-MOST    PIC X(1)  VALUE SPACES.             
004200     03 SW-EOP-MORE-THAN-5-YEARS-AGO  PIC X(1)  VALUE SPACES.             
004300                                                                          
004400*    -COPY WWPRODSL                                                       
004500*                                                                         
004600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004700 01  FILLER REDEFINES DAGENS-DATUM.                                       
004800     03 DAGENS-AA                PIC 9(2).                                
004900     03 DAGENS-MAANAD            PIC 9(2).                                
005000     03 DAGENS-DAG               PIC 9(2).                                
005100*                                                                         
005200 01  DAGENS-DATUM-CENTURY        PIC 9(8)    VALUE ZERO.                  
005201                                                                          
005210 01  DAGENS-VECKA                PIC 9(2)    VALUE ZERO.                  
005300*                                                                         
005400                                                                          
005500**** GENERAL SUBROUTINE                                                   
005600 01  DYNAMISKA-SUBPROGRAM.                                                
005700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005900     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
005910     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006000                                                                          
006100*    ---PARAMETRAR TILL WZ20DAYS                                          
006200 01  FILLER                      PIC X(16)   VALUE 'WZ20DAYS'.            
006300*01  -COPY WZ20DAYS                                                       
006400     EJECT                                                                
006401                                                                          
006410*    --- PARAMETRAR TILL DATKONV                                          
006420*                                                                         
006430*01  -COPY WDATAREA                                                       
006440     EJECT                                                                
006500                                                                          
006510*    -COPY WY2000W2                                                       
006520     EJECT                                                                
006530                                                                          
006540*    -COPY WY2000W3                                                       
006550     EJECT                                                                
006600 01  FELTEXT.                                                             
006700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT='.            
006800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006900     EJECT                                                                
007000*                                                                         
007100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
007200*                                                                         
007300 01  NYCKLAR-TILL-DLI.                                                    
007400     03  W-IDARTNR-X.                                                     
007500         05  W-IDARTNR           PIC S9(9)   COMP-3.                      
007600     03  W-IDDC-X.                                                        
007700         05  W-IDDC              PIC X(2)    VALUE ZERO.                  
007800     03  W-IDPERSON-BUY-X.                                                
007900         05  W-IDPERSON-BUY      PIC S9(3)   COMP-3.                      
007910     03  W-TIAAAA-X.                                                      
007920         05  W-TIAAAA            PIC 9(4)   VALUE ZERO.                   
007930                                                                          
008000                                                                          
008100 01  IMS-WS.                                                              
008200     03  FILLER                  PIC X(8)    VALUE 'IMS-WS  '.            
008300                                                                          
008400*                            *** STATUSCODES FROM IMS                     
008500     03 STATUS-WS                PIC XX.                                  
008600         88  SEGMENT-FOUND                   VALUE '  '.                  
008700         88  SEGMENT-MISSING                 VALUE 'GE'.                  
008900                                                                          
009000     03  GOOD-STATUSCODES.                                                
009100         05  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.            
009200                                                                          
009300     03 ERROR-TEXT.                                                       
009400         05 FILLER               PIC X(10)   VALUE 'ERROR-TEXT'.          
009500         05 ERROR-TEXT-STR       PIC X(72)   VALUE SPACE.                 
009600                                                                          
009700     03  SSA1                    PIC X(128).                              
009800     03  SSA2                    PIC X(128).                              
009900     03  SSA3                    PIC X(128).                              
010000     EJECT                                                                
010100                                                                          
010200*    --- IMS FUNKTIONSKODER                                               
010300*01  -COPY W0003                                                          
010400     EJECT                                                                
010500                                                                          
010600*    --- DLI INPUT-OUTPUT AREA                                            
010700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB601'.             
010800 01  DLI-IO-WDB601.                                                       
010900*    03  -COPY WDB601                                                     
011000     EJECT                                                                
011100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB619'.             
011200 01  DLI-IO-WDB619.                                                       
011300*    03  -COPY WDB619                                                     
011400     EJECT                                                                
011500 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL711'.             
011600 01  DLI-IO-WDL711.                                                       
011700*    03  -COPY WDL711                                                     
011800     EJECT                                                                
011900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDL811'.             
012000 01  DLI-IO-WDL811.                                                       
012100*    03  -COPY WDL811                                                     
012200     EJECT                                                                
012300 LINKAGE SECTION.                                                         
012400                                                                          
012500*01 -COPY W271BUYR                                                        
012600     EJECT                                                                
012700                                                                          
012800*01  -COPY W0008  -PRE WDB6-                                              
012900     05  FILLER                  PIC X.                                   
013000     EJECT                                                                
013100                                                                          
013200*01  -COPY W0008  -PRE WDL7-                                              
013300     05  FILLER                  PIC X.                                   
013400     EJECT                                                                
013500                                                                          
013600*01  -COPY W0008  -PRE WDL8-                                              
013700     05  FILLER                  PIC X.                                   
013800     EJECT                                                                
013900                                                                          
014000 PROCEDURE DIVISION USING BUYR-W271BUYR                                   
014100                          WDB6-PCB WDL7-PCB WDL8-PCB.                     
014200                                                                          
014300     PERFORM A-INIT                                                       
014400                                                                          
014500     IF  BUYR-IDARTNR IS NUMERIC                                          
014600     AND BUYR-IDARTNR  > ZERO                                             
014601                                                                          
014670                                                                          
014700        EVALUATE BUYR-KDCALL                                              
014800           WHEN 001                                                       
014900           WHEN 002                                                       
015000             PERFORM B-SET-IDPERSON-BUY-MAJOR                             
015100             IF SW-SET-VOL-AND-LIFECYCLE = JA                             
015200                PERFORM C-SET-VOL-AND-LIFECYCLE                           
015300             END-IF                                                       
015400             PERFORM D-SET-FLBUYER                                        
015500             PERFORM E-SET-IDREFTAB                                       
015600           WHEN OTHER                                                     
015700             SET BUYR-KDSVAR-FEL   TO TRUE                                
015800             MOVE 'ERROR KDCALL'   TO BUYR-TEXT                           
015900             DISPLAY 'W271BUYR ERROR KDCALL=' BUYR-KDCALL                 
016000        END-EVALUATE                                                      
016100     ELSE                                                                 
016200         SET BUYR-KDSVAR-FEL  TO TRUE                                     
016300         MOVE 'INVALID PART NO.'   TO BUYR-TEXT                           
016400         DISPLAY 'W271BUYR INVALID PART NO.=' BUYR-IDARTNR                
016500     END-IF                                                               
016600                                                                          
016700     MOVE ZERO TO RETURN-CODE                                             
016800     GOBACK                                                               
016900     .                                                                    
017000     EJECT                                                                
017100                                                                          
017200 A-INIT SECTION.                                                          
017300                                                                          
017400     MOVE SPACES                  TO BUYR-KDSVAR                          
017500*                                                                         
017600     MOVE FUNCTION CURRENT-DATE(1:8)                                      
017700                                  TO DAGENS-DATUM-CENTURY                 
017710     MOVE FUNCTION CURRENT-DATE(1:4)                                      
017720                                  TO W-TIAAAA                             
017800                                                                          
017900     ACCEPT DAGENS-DATUM        FROM DATE                                 
017901                                                                          
017910     MOVE 'IDAG'         TO DAT-KDDATFORM                                 
017920     CALL WDATKONV USING DAT-KDDATFORM                                    
017930                         DAT-I-TIDATUM                                    
017940                         DAT-O-TIDATUM                                    
017950                         DAT-KDSVAR                                       
017960                                                                          
017970     IF DAT-KDSVAR-OK                                                     
017980        MOVE DAT-TIVV    TO DAGENS-VECKA                                  
017996     ELSE                                                                 
017997        MOVE 'FEL FRÅN WDATKONV 1  I A-INIT SECTION I W27110' TO          
017998                                    FELTEXT-STR                           
017999        DISPLAY FELTEXT                                                   
018000        CALL FELLOG                                                       
018001     END-IF                                                               
018002                                                                          
018003     IF  BUYR-TISOP-2-YEARS-AGO   NUMERIC                                 
018004     AND BUYR-TISOP-2-YEARS-AGO > ZERO                                    
018005        CONTINUE                                                          
018006     ELSE                                                                 
018007        MOVE DAGENS-DATUM            TO DAYS-TIDATE1                      
018008        MOVE 'YYMMDD'                TO DAYS-KDDATFMT1                    
018010        MOVE -730                    TO DAYS-KVDAYS                       
018011*----------- -730 = 2 YEARS X 365 DAYS AGO                                
018012        MOVE 'YYWWD'                 TO DAYS-KDDATFMT2                    
018013        MOVE SPACE                   TO DAYS-TIDATE2                      
018015                                        DAYS-IDCALEND                     
018016                                                                          
018017        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
018018                                                                          
018019        IF DAYS-KDRC = 8                                                  
018020           STRING 'FEL SOP-ANROP TILL WZ20DAYS - SEC A-'                  
018021           DELIMITED BY SIZE INTO FELTEXT-STR                             
018022           DISPLAY FELTEXT                                                
018023           DISPLAY '     DAGENS-DATUM=' DAGENS-DATUM                      
018025           CALL FELLOG                                                    
018030        ELSE                                                              
018032           MOVE DAYS-TIDATE2(1:5)    TO BUYR-TISOP-2-YEARS-AGO            
018033           DISPLAY 'BUYR BUYR-TISOP-2-YEARS-AGO   ='                      
018034-                        BUYR-TISOP-2-YEARS-AGO                           
018035        END-IF                                                            
018036     END-IF                                                               
018037                                                                          
018038     IF  BUYR-TISOP-TODAY         NUMERIC                                 
018039     AND BUYR-TISOP-TODAY       > ZERO                                    
018040        CONTINUE                                                          
018041     ELSE                                                                 
018042        MOVE DAGENS-DATUM            TO DAYS-TIDATE1                      
018043        MOVE 'YYMMDD'                TO DAYS-KDDATFMT1                    
018044        MOVE 0                       TO DAYS-KVDAYS                       
018045*----------- 0  = TODAY                                                   
018046        MOVE 'YYWWD'                 TO DAYS-KDDATFMT2                    
018047        MOVE SPACE                   TO DAYS-TIDATE2                      
018048                                        DAYS-IDCALEND                     
018049                                                                          
018050        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
018051                                                                          
018052        IF DAYS-KDRC = 8                                                  
018053           STRING 'FEL SOP-ANROP TILL WZ20DAYS - SEC A-'                  
018054           DELIMITED BY SIZE INTO FELTEXT-STR                             
018055           DISPLAY FELTEXT                                                
018056           DISPLAY '     DAGENS-DATUM=' DAGENS-DATUM                      
018057           CALL FELLOG                                                    
018058        ELSE                                                              
018059           MOVE DAYS-TIDATE2(1:5)    TO BUYR-TISOP-TODAY                  
018060           DISPLAY 'BUYR BUYR-TISOP-TODAY         ='                      
018061-                        BUYR-TISOP-TODAY                                 
018062        END-IF                                                            
018063     END-IF                                                               
018064                                                                          
018065     IF  BUYR-TIURPROD-5-YEARS-AGO   NUMERIC                              
018066     AND BUYR-TIURPROD-5-YEARS-AGO > ZERO                                 
018067        CONTINUE                                                          
018068     ELSE                                                                 
018069        MOVE DAGENS-DATUM            TO DAYS-TIDATE1                      
018070        MOVE 'YYMMDD'                TO DAYS-KDDATFMT1                    
018071        MOVE -1825                   TO DAYS-KVDAYS                       
018072*----------- -1825 = 5 YEARS X 365 DAYS AGO                               
018073        MOVE 'YYWW'                  TO DAYS-KDDATFMT2                    
018074*----------- DAY=1 IS ASSUMED IN WZ30DAYS                                 
018075        MOVE SPACE                   TO DAYS-TIDATE2                      
018076                                        DAYS-IDCALEND                     
018077                                                                          
018078        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
018079                                                                          
018080        IF DAYS-KDRC = 8                                                  
018081           STRING 'FEL EOP-ANROP TILL WZ20DAYS - SEC A-'                  
018082           DELIMITED BY SIZE INTO FELTEXT-STR                             
018083           DISPLAY FELTEXT                                                
018084           DISPLAY '     DAGENS-DATUM=' DAGENS-DATUM                      
018086           CALL FELLOG                                                    
018087        ELSE                                                              
018088           MOVE DAYS-TIDATE2(1:4)    TO BUYR-TIURPROD-5-YEARS-AGO         
018089           DISPLAY 'BUYR BUYR-TIURPROD-5-YEARS-AGO='                      
018090-                        BUYR-TIURPROD-5-YEARS-AGO                        
018092        END-IF                                                            
018093     END-IF                                                               
018094     .                                                                    
018100     EJECT                                                                
018200                                                                          
018300 B-SET-IDPERSON-BUY-MAJOR SECTION.                                        
018400                                                                          
018500     PERFORM BA-PREPARE-CHECKS                                            
018600                                                                          
018700     MOVE JA                          TO SW-SET-VOL-AND-LIFECYCLE         
018800                                                                          
018900     EVALUATE TRUE                                                        
019000     WHEN BUYR-FLBUYUPD    = JA                                           
019100*------- BUYER LOCKED                                                     
019200     WHEN BUYR-IDPERSON-BUY-IN >= 41                                      
019210      AND BUYR-IDPERSON-BUY-IN <= 99                                      
019300*------- MANUAL BUYER(S)                                                  
019400          MOVE BUYR-IDPERSON-BUY-IN   TO BUYR-IDPERSON-BUY                
019500          MOVE NEJ                    TO SW-SET-VOL-AND-LIFECYCLE         
019600     WHEN KDPRODSL-LOCAL                                                  
019700*------- LOCAL SUPPLIER                                                   
019800          MOVE 040                    TO BUYR-IDPERSON-BUY                
019900          MOVE NEJ                    TO SW-SET-VOL-AND-LIFECYCLE         
020000     WHEN BUYR-KDUART = 'S'                                               
020100      AND (BUYR-IDFKNGRP = 8341 OR 8342 OR 8343)                          
020200*------- KEYS                                                             
020300          MOVE 020                    TO BUYR-IDPERSON-BUY                
020400          MOVE NEJ                    TO SW-SET-VOL-AND-LIFECYCLE         
020500     WHEN BUYR-KDUART = 'S'                                               
020600*------- SPECIAL ORDER (E.G. CABLE HARNESS, DECALS)                       
020700          MOVE 030                    TO BUYR-IDPERSON-BUY                
020800          MOVE NEJ                    TO SW-SET-VOL-AND-LIFECYCLE         
020810     WHEN KDPRODSL-EMB                                                    
020820*------- EMBALLAGE                                                        
020830          MOVE 010                    TO BUYR-IDPERSON-BUY                
020840          MOVE NEJ                    TO SW-SET-VOL-AND-LIFECYCLE         
020900     WHEN SW-EXTENDED-REFILL = JA                                         
021000*------- EXTENDED REFILL                                                  
021100          MOVE 800                    TO BUYR-IDPERSON-BUY                
021200     WHEN BUYR-FLFLYG = JA                                                
021300*------- ALWAYS AIR                                                       
021400          MOVE 700                    TO BUYR-IDPERSON-BUY                
021500     WHEN BUYR-KDFARLIG = 4 OR 6 OR 7                                     
021600*------- DANGEROUS GOODS                                                  
021700          MOVE 500                    TO BUYR-IDPERSON-BUY                
022400     WHEN BUYR-IDFKNGRP =  8611 OR 8614 OR 8616 OR                        
022410                           8621 OR 8624 OR 8626                           
022500*------- BUMPERS                                                          
022600          MOVE 600                    TO BUYR-IDPERSON-BUY                
022601     WHEN KDPRODSL-TOOLS                                                  
022602*------- TOOLS                                                            
022603          MOVE 900                    TO BUYR-IDPERSON-BUY                
022604     WHEN BUYR-FLBSNES  = 'Y'                                             
022605*------- KEY BUSINESS                                                     
022606          MOVE 100                    TO BUYR-IDPERSON-BUY                
022610     WHEN BUYR-KVEOP    = 15                                              
022620*------- FUNCTION CRITICAL                                                
022630          MOVE 200                    TO BUYR-IDPERSON-BUY                
022700     WHEN KDPRODSL-ACC                                                    
022800       OR KDPRODSL-WHEELS                                                 
022900*------- ACCESSORIES (OR WHEELS)                                          
023000          MOVE 400                    TO BUYR-IDPERSON-BUY                
023800     WHEN OTHER                                                           
023900*------- NONE OF THE ABOVE, JUST A NORMAL PART                            
024000          MOVE 300                    TO BUYR-IDPERSON-BUY                
024100     END-EVALUATE                                                         
024200     .                                                                    
024300     EJECT                                                                
024400                                                                          
024500 BA-PREPARE-CHECKS  SECTION.                                              
024600                                                                          
024601     MOVE NEJ           TO SW-SOP-2-YEARS-AGO-AT-MOST                     
024610                           SW-EOP-MORE-THAN-5-YEARS-AGO                   
024611                           SW-EXTENDED-REFILL                             
024612     MOVE ZERO          TO WS-KVOT                                        
024620                                                                          
024700     MOVE BUYR-KDPRODSL               TO TEST-KDPRODSL                    
024800                                                                          
024810     IF BUYR-IDDC-REF  > SPACE                                            
024811        MOVE BUYR-IDDC-REF  TO W-IDDC                                     
024820        PERFORM IMS-GU-WDB601                                             
024870        IF SEGMENT-FOUND                                                  
024880        AND (DCS-USA OR DCS-CANADA)                                       
024883           MOVE BUYR-IDDC      TO W-IDDC                                  
024884           PERFORM IMS-GU-WDB601                                          
024890           IF SEGMENT-FOUND                                               
024891           AND (DCS-USA OR DCS-CANADA)                                    
025100               MOVE JA                TO SW-EXTENDED-REFILL               
025400           END-IF                                                         
025410        END-IF                                                            
025420     END-IF                                                               
025500     .                                                                    
025600     EJECT                                                                
025700                                                                          
025800 C-SET-VOL-AND-LIFECYCLE SECTION.                                         
025900                                                                          
026000*-- VOLUME, I.E. DETERMINE IF BULKY (OR NOT)                              
026100     EVALUATE TRUE                                                        
026200     WHEN BUYR-VLARTNTO                   >=  200000                      
026300     WHEN BUYR-VLARTNTO * BUYR-KVPB-TOT   >= 5000000                      
026400*--------BULKY                                                            
026500          ADD 10        TO BUYR-IDPERSON-BUY                              
026600     END-EVALUATE                                                         
026700                                                                          
026800     PERFORM CA-PREPARE-CHECKS                                            
027500                                                                          
027600*-- LIFE CYCLE, I.E. DETERMINE IF PHASE IN, PHASE OUT OR NORMAL           
027700     EVALUATE TRUE                                                        
027800     WHEN WS-KVOT                       < 30                              
027900      AND SW-SOP-2-YEARS-AGO-AT-MOST    = JA                              
028000*------- PHASE IN                                                         
028100          CONTINUE                                                        
028200     WHEN WS-KVOT                       < 30                              
028300      AND SW-EOP-MORE-THAN-5-YEARS-AGO  = JA                              
028400*------- PHASE OUT                                                        
028500          ADD  2        TO BUYR-IDPERSON-BUY                              
028600     WHEN OTHER                                                           
028700*------- NORMAL                                                           
028800          ADD  1        TO BUYR-IDPERSON-BUY                              
028900     END-EVALUATE                                                         
029000     .                                                                    
029100     EJECT                                                                
029200                                                                          
029300 CA-PREPARE-CHECKS  SECTION.                                              
029400                                                                          
029500*-- DETERMINE IF 2 YEARS SINCE SOP                                        
029700     IF BUYR-TISOP  > ZERO                                                
031200        MOVE BUYR-TISOP                 TO TMP1-YYWWD                     
031210        MOVE BUYR-TISOP-2-YEARS-AGO     TO TMP2-YYWWD                     
031211        PERFORM WY2000P2                                                  
031220        IF  TMP1-YYWWD  >= TMP2-YYWWD                                     
031500           MOVE JA   TO SW-SOP-2-YEARS-AGO-AT-MOST                        
031700        END-IF                                                            
031800     END-IF                                                               
031900                                                                          
032200     IF  BUYR-TIURPROD  =  ZERO                                           
032201     AND BUYR-TISOP     > ZERO                                            
032202     AND BUYR-TISOP NOT = 99999                                           
032203*---- DEFAILT EOP TO 1 YEAR (365 DAYS) AFTER SOP                          
032207        MOVE BUYR-TISOP                TO DAYS-TIDATE1                    
032208        MOVE 'YYWWD'                   TO DAYS-KDDATFMT1                  
032209        MOVE 'YYWWD'                   TO DAYS-KDDATFMT2                  
032210        MOVE 365                       TO DAYS-KVDAYS                     
032211        MOVE SPACE                     TO DAYS-TIDATE2                    
032212        MOVE SPACE                     TO DAYS-IDCALEND                   
032213                                                                          
032214        CALL WZ20DAYS USING DAYS-WZ20DAYS                                 
032215                                                                          
032216        IF DAYS-KDRC = 8                                                  
032217           STRING 'FEL EOP-DEFAULT LL WZ20DAYS - SEC CA-'                 
032218           DELIMITED BY SIZE INTO FELTEXT-STR                             
032219           DISPLAY FELTEXT                                                
032220           DISPLAY '       BUYR-TISOP=' BUYR-TISOP                        
032221           DISPLAY '      DAYS-KVDAYS=' DAYS-KVDAYS                       
032222           CALL FELLOG                                                    
032223        ELSE                                                              
032224           MOVE DAYS-TIDATE2(1:4)    TO BUYR-TIURPROD                     
032225***         DISPLAY 'W271BUYR: PART=' BUYR-IDARTNR                        
032226*-                    ': BUYR-TIURPROD DEFAULTED=' BUYR-TIURPROD          
032227*-                     ' (6 YRS AFTER BUYR-TISOP=' BUYR-TISOP ')'         
032230        END-IF                                                            
032231     END-IF                                                               
032232                                                                          
032233*-- DETERMINE IF 5 YEARS SINCE EOP                                        
032240     IF BUYR-TIURPROD                   >  ZERO                           
032241        MOVE BUYR-TIURPROD              TO TMP1-YYWW                      
032242        MOVE BUYR-TIURPROD-5-YEARS-AGO  TO TMP2-YYWW                      
032245        PERFORM WY2000P3                                                  
032250        IF  TMP1-YYWW   <  TMP2-YYWW                                      
034000           MOVE JA   TO SW-EOP-MORE-THAN-5-YEARS-AGO                      
034100        END-IF                                                            
034300     END-IF                                                               
034400                                                                          
034500*-- GET NUMBER OF PICKS (I.E. KVOT)                                       
034700     IF SW-SOP-2-YEARS-AGO-AT-MOST   = JA                                 
034800     OR SW-EOP-MORE-THAN-5-YEARS-AGO = JA                                 
034920        PERFORM S20-GET-SUM-KVOT-ORDER-HITS                               
036500     END-IF                                                               
036600     .                                                                    
036700     EJECT                                                                
036800                                                                          
036900 D-SET-FLBUYER      SECTION.                                              
037000                                                                          
037100     IF BUYR-IDPERSON-BUY-IN = BUYR-IDPERSON-BUY                          
037200          MOVE NEJ          TO BUYR-FLBUYER-CHANGED                       
037300     ELSE                                                                 
037400          MOVE JA           TO BUYR-FLBUYER-CHANGED                       
037500     END-IF                                                               
037600     .                                                                    
037700     EJECT                                                                
037800                                                                          
037900 E-SET-IDREFTAB     SECTION.                                              
038000                                                                          
038100     IF BUYR-FLTABUPD    = JA                                             
038200*----- TABLE LOCKED                                                       
038300        MOVE BUYR-IDREFTAB-IN  TO BUYR-IDREFTAB                           
038400        MOVE NEJ               TO BUYR-FLTABLE-CHANGED                    
038500     ELSE                                                                 
038600        MOVE ZERO              TO BUYR-IDREFTAB                           
038700                                                                          
038800        MOVE BUYR-IDDC         TO W-IDDC                                  
038900        PERFORM IMS-GU-WDB601                                             
039000        IF SEGMENT-FOUND                                                  
039100           MOVE BUYR-IDPERSON-BUY  TO W-IDPERSON-BUY                      
039200           PERFORM IMS-GNP-WDB619                                         
039300           IF SEGMENT-FOUND                                               
039310              IF  SW-SOP-2-YEARS-AGO-AT-MOST   = NEJ                      
039320              AND SW-EOP-MORE-THAN-5-YEARS-AGO = NEJ                      
039321*-------------- PICKS WERE NOT GOTTEN BEFORE SO MUST DO IT NOW            
039322                 PERFORM S20-GET-SUM-KVOT-ORDER-HITS                      
039330              END-IF                                                      
039331                                                                          
039332              IF BUYR-KVPB-TOT < DCS-KVPB-LIM-HF                          
039333*-------------------- FORECAST < MIN FORECAST ALLOWED FOR HF              
039334                 MOVE BUYT-IDREFTAB-LF  TO BUYR-IDREFTAB                  
039335              ELSE                                                        
039336                 IF BUYR-KVPB-TOT > DCS-KVPB-LIM-LF                       
039337*----------------------- FORECAST > MAX FORECAST ALLOWED FOR LF           
039338                    MOVE BUYT-IDREFTAB-HF  TO BUYR-IDREFTAB               
039339                 ELSE                                                     
039340                    IF WS-KVOT       > DCS-KVOT                           
039341*------------------- NUMBER OF PICKS > MAX NUMBER ALLOWED FOR LF          
039342                       MOVE BUYT-IDREFTAB-HF  TO BUYR-IDREFTAB            
039370                    ELSE                                                  
039380                       MOVE BUYT-IDREFTAB-LF  TO BUYR-IDREFTAB            
039512                    END-IF                                                
039513                 END-IF                                                   
039514              END-IF                                                      
039520           END-IF                                                         
039600        END-IF                                                            
039700        IF BUYR-IDREFTAB-IN     = BUYR-IDREFTAB                           
039800             MOVE NEJ          TO BUYR-FLTABLE-CHANGED                    
039900        ELSE                                                              
040000             MOVE JA           TO BUYR-FLTABLE-CHANGED                    
040100        END-IF                                                            
040200     END-IF                                                               
040300     .                                                                    
040400     EJECT                                                                
040401                                                                          
040403 S20-GET-SUM-KVOT-ORDER-HITS SECTION.                                     
040410                                                                          
040420     MOVE BUYR-IDARTNR                TO W-IDARTNR                        
040430                                                                          
040440     EVALUATE TRUE                                                        
040450     WHEN BUYR-KDCALL = 001                                               
040460        MOVE BUYR-IDDC                   TO W-IDDC                        
040470        PERFORM IMS-GU-WDL711                                             
040480        IF SEGMENT-FOUND                                                  
040490           MOVE +1                       TO INDX                          
040491           PERFORM UNTIL INDX            >  53                            
040492              ADD DC-KVOT-RULL(INDX)     TO WS-KVOT                       
040493              ADD DC-KVOT-REF-RULL(INDX) TO WS-KVOT                       
040494              ADD +1                     TO INDX                          
040495           END-PERFORM                                                    
040496        END-IF                                                            
040497     WHEN BUYR-KDCALL = 002                                               
040498        PERFORM IMS-GU-WDL811                                             
040500        IF SEGMENT-FOUND                                                  
040501           MOVE +1                       TO INDX                          
040502           PERFORM UNTIL INDX            >  DAGENS-VECKA - 1              
040503              ADD AAR-KVOT-PROG(INDX)    TO WS-KVOT                       
040504              ADD AAR-KVOT-REFILL(INDX)  TO WS-KVOT                       
040505              ADD +1                     TO INDX                          
040506           END-PERFORM                                                    
040507        END-IF                                                            
040520                                                                          
040521        COMPUTE W-TIAAAA  = W-TIAAAA - 1                                  
040522        PERFORM IMS-GU-WDL811                                             
040523        IF SEGMENT-FOUND                                                  
040524           MOVE DAGENS-VECKA             TO INDX                          
040525           PERFORM UNTIL INDX            >  53                            
040526              ADD AAR-KVOT-PROG(INDX)    TO WS-KVOT                       
040527              ADD AAR-KVOT-REFILL(INDX)  TO WS-KVOT                       
040528              ADD +1                     TO INDX                          
040529           END-PERFORM                                                    
040530        END-IF                                                            
040531     END-EVALUATE                                                         
040537     .                                                                    
040538     EJECT                                                                
040540                                                                          
040600 IMS-GU-WDB601    SECTION.                                                
040800                                                                          
040900     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
041000          DELIMITED BY SIZE INTO SSA1                                     
041100     MOVE '  '                TO GOOD-STATUSCODES                         
041200     CALL CBLTDLI USING GU                                                
041300                        WDB6-PCB                                          
041400                        DLI-IO-WDB601                                     
041500                        SSA1                                              
041600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
041700     PERFORM IMS-STATUSCHECK                                              
041800     .                                                                    
041900     SKIP3                                                                
042000                                                                          
042100 IMS-GNP-WDB619 SECTION.                                                  
042300                                                                          
042400     STRING 'WDB619  (IDPERSBU =' W-IDPERSON-BUY-X ')'                    
042500          DELIMITED BY SIZE INTO SSA1                                     
042600     MOVE '  GE'              TO GOOD-STATUSCODES                         
042700     CALL CBLTDLI USING GNP                                               
042800                        WDB6-PCB                                          
042900                        DLI-IO-WDB619                                     
043000                        SSA1                                              
043100     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
043200     PERFORM IMS-STATUSCHECK                                              
043300     .                                                                    
043400     SKIP3                                                                
043500                                                                          
043600 IMS-GU-WDL711     SECTION.                                               
043700     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
043800          DELIMITED BY SIZE INTO SSA1                                     
043900     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
044000          DELIMITED BY SIZE INTO SSA2                                     
044100     MOVE '  GE' TO GOOD-STATUSCODES                                      
044200     CALL CBLTDLI USING GU                                                
044300                        WDL7-PCB                                          
044400                        DLI-IO-WDL711                                     
044500                        SSA1                                              
044600                        SSA2                                              
044700     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
044800     PERFORM IMS-STATUSCHECK                                              
044900     .                                                                    
045000     SKIP3                                                                
045100                                                                          
045110 IMS-GU-WDL811      SECTION.                                              
045120                                                                          
045130     STRING 'WDL801  (IDARTNR  =' W-IDARTNR-X ')'                         
045140          DELIMITED BY SIZE INTO SSA1                                     
045150     STRING 'WDL811  (TIAAAA   =' W-TIAAAA-X ')'                          
045160          DELIMITED BY SIZE INTO SSA2                                     
045161     MOVE '  GE' TO GOOD-STATUSCODES                                      
045180     CALL CBLTDLI USING GU                                                
045181                        WDL8-PCB                                          
045182                        DLI-IO-WDL811                                     
045183                        SSA1                                              
045184                        SSA2                                              
045190     MOVE WDL8-STATUS-CODE TO STATUS-WS                                   
045191     PERFORM IMS-STATUSCHECK                                              
045193     .                                                                    
045194     SKIP3                                                                
045195                                                                          
045200 IMS-STATUSCHECK SECTION.                                                 
045300     SKIP2                                                                
045400     SET STATUS-IX               TO 1                                     
045500     SEARCH GOOD-STATUS                                                   
045600       AT END                                                             
045700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
045800             DELIMITED BY SIZE INTO ERROR-TEXT-STR                        
045900         DISPLAY ERROR-TEXT                                               
046000         CALL FELLOG                                                      
046100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
046200         CONTINUE                                                         
046300     END-SEARCH                                                           
046400     .                                                                    
046401     EJECT                                                                
046402*    -COPY WY2000P2                                                       
046403     EJECT                                                                
046410*    -COPY WY2000P3                                                       
046420     EJECT                                                                
