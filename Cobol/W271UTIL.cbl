000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300                                                                          
000400 PROGRAM-ID.     W271UTIL.                                                
000500 AUTHOR.         ARUP DATTA.                                              
000600 DATE-WRITTEN.   19/05/30.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNKTION:                                                            
001000*        SUBPROGRAM TO CHECK THE FOLLOWING :                              
001100*        -THE SOURCE OF A LOCAL PART                                      
001200*            A REFILL PART CAN BE SOURCED FROM LOCAL SUPPLIERS IN         
001300*            DIFFERENT COUNTRIES.                                         
001400*        -CALCULATE FORECAST OF CDC AND XDC                               
001500*        -CALCULATE WEEKLY FORECAST WHEN WE HAVE FUTURE FORECAST          
001600*        -CALCULATE WEEKLY FORECAST WHEN WE HAVE SEASON FOR XDC           
001700*                                                                         
001800*        PROGRAMMET READS     WDK6                                        
001900*                             WDK7                                        
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500*                                                                         
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003400     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100     SKIP2                                                                
004200*    -- CHECKED BY WY2000                                                 
004300     SKIP3                                                                
004400 77  IDPGM                       PIC X(8)    VALUE 'W271UTIL'.            
004500 77  JA                          PIC X       VALUE 'J'.                   
004600 77  NEJ                         PIC X       VALUE 'N'.                   
004700 77  WS-CURRENT-SECTION          PIC X(80)   VALUE SPACES.                
004800     EJECT                                                                
004900                                                                          
005000 01  WORKING-FIELDS.                                                      
005100     03 DAGENS-DATUM.                                                     
005200         05 DAGENS-AAR           PIC 9(4).                                
005300         05 DAGENS-MAANAD        PIC 9(2).                                
005400         05 DAGENS-DAG           PIC 9(2).                                
005500     03 FILLER REDEFINES DAGENS-DATUM.                                    
005600         05 FILLER               PIC 9(2).                                
005700         05 DAGENS-AAMMDD        PIC 9(6).                                
005800     03 WS-TIAAVVD-SPAR          PIC 9(5)    VALUE ZERO.                  
005900     03 FILLER REDEFINES WS-TIAAVVD-SPAR.                                 
006000        05 WS-TIAAVV-SPAR        PIC 9(4).                                
006100        05 WS-DAY-SPAR           PIC 9(1).                                
006200     03 WS-TIDATE2.                                                       
006300        05 WS-TIDATE2-AAVVD      PIC X(5)    VALUE SPACES.                
006400        05 FILLER                PIC X(15)   VALUE SPACES.                
006500     03 WS-TIAAVVD-CURR          PIC 9(5).                                
006600     03 WS-TIAAVVD-START         PIC 9(5).                                
006700     03 FILLER REDEFINES WS-TIAAVVD-START.                                
006800        05 WS-TIAAVV-START       PIC 9(4).                                
006900        05 WS-TID-START          PIC 9(1).                                
007000     03 WS-TIAAVVD-GROUP.                                                 
007100        05 WS-TIAAVVD-T                      OCCURS 156.                  
007200           07 WS-TIAAVV-T        PIC 9(4)    VALUE ZERO.                  
007300           07 WS-DAY-T           PIC 9(1)    VALUE ZERO.                  
007400     03 WS-DATUM-SEAS            PIC 9(5)    VALUE ZERO.                  
007500     03 FILLER REDEFINES WS-DATUM-SEAS.                                   
007600        05 WS-DATUM-AA           PIC 9(2).                                
007700        05 WS-DATUM-VV           PIC 9(2).                                
007800        05 WS-DATUM-D            PIC 9(1).                                
007900     03 WS-DAT-IN-AAVV.                                                   
008000        05 WS-DAT-IN-AA          PIC 9(2)    VALUE ZERO.                  
008100        05 WS-DAT-IN-VV          PIC 9(2)    VALUE ZERO.                  
008200        05 WS-DAT-UT-VV          PIC 9(2)    VALUE ZERO.                  
008300     03 WS-IN-AAVV               PIC 9(4)    VALUE ZERO.                  
008400     03 WS-KVDAYS                PIC S9(3)   VALUE ZERO COMP-3.           
008500     03 WS-LOCAL-COUNT           PIC S9(1)   VALUE ZERO.                  
008600     03 WS-IDDC-SPAR             PIC X(2)    VALUE SPACES.                
008700     03 WS-CDC-SE                PIC X(2)    VALUE '11'.                  
008800     03 WS-IDLEVNR               PIC X(5)    VALUE SPACES.                
008900     03 WS-KVPB-TOT              PIC S9(6)V9(1)                           
009000                                             VALUE ZERO  COMP-3.          
009100     03 WS-KVPB-TOT-CDC          PIC S9(6)V9(1)                           
009200                                             VALUE ZERO  COMP-3.          
009300     03 WS-KVPB-TOT-XDC          PIC S9(6)V9(1)                           
009400                                             VALUE ZERO  COMP-3.          
009500     03 WS-KVPB-TOT-XDC-NOSEAS   PIC S9(6)V9(1)                           
009600                                             VALUE ZERO  COMP-3.          
009700*                                                                         
009800     03  W-FAKTOR              PIC S9(1)V9(3) VALUE ZERO COMP-3.          
009900     03  W-KVPB-SEP            PIC S9(6)V9(3) VALUE ZERO COMP-3.          
010000     03  W-KVPB-SATS           PIC S9(6)V9(3) VALUE ZERO COMP-3.          
010100     03  W-KVPB-REF            PIC S9(6)V9(3) VALUE ZERO COMP-3.          
010200     03  W-KVPBREOI            PIC S9(6)V9(3) VALUE ZERO COMP-3.          
010300     03  W-KVPB-JUST1          PIC S9(6)V9(3) VALUE ZERO COMP-3.          
010400     03  W-KVPB-JUST2          PIC S9(6)V9(3) VALUE ZERO COMP-3.          
010500     03  W-XDC-KVPB-JUST1      PIC S9(6)V9(3) VALUE ZERO COMP-3.          
010600     03  W-XDC-KVPB-JUST2      PIC S9(6)V9(3) VALUE ZERO COMP-3.          
010700*                                                                         
010800     03  W-CDC-JUST-PB-FINNS     PIC X       VALUE 'N'.                   
010900     03  W-XDC-JUST-PB-FINNS     PIC X       VALUE 'N'.                   
011000     03  W-CDC-TIJUST1           PIC S9(5)   VALUE ZERO COMP-3.           
011100     03  W-CDC-TIJUST2           PIC S9(5)   VALUE ZERO COMP-3.           
011200     03  W-XDC-TIJUST1           PIC S9(5)   VALUE ZERO COMP-3.           
011300     03  W-XDC-TIJUST2           PIC S9(5)   VALUE ZERO COMP-3.           
011400*                                                                         
011500     03  W-NDC-JUST-PB-FINNS     PIC X       VALUE 'N'.                   
011600     03  W-NDC-KVPB-JUST         PIC S9(6)V9(1) OCCURS 2                  
011700                                             VALUE ZERO COMP-3.           
011800     03  W-NDC-TIPBJUST          PIC S9(5)      OCCURS 2                  
011900                                             VALUE ZERO COMP-3.           
012000     03  WS-NDC-TIPBJUST         PIC 9(4)    VALUE ZERO.                  
012100*                                                                         
012200     03  IX                      PIC S9(9) VALUE ZERO COMP SYNC.          
012300     03  IX-S                    PIC S9(9) VALUE ZERO COMP SYNC.          
012400     03  IX-V                    PIC S9(9) VALUE ZERO COMP SYNC.          
012500     03  IX-DT                   PIC S9(9) VALUE ZERO COMP SYNC.          
012600     03  IX-PER                  PIC S9(9) VALUE ZERO COMP SYNC.          
012700     03  IX-FRAN                 PIC S9(9) VALUE ZERO COMP SYNC.          
012800     03  IX-TILL                 PIC S9(9) VALUE ZERO COMP SYNC.          
012900     03  IX-PER-MAX              PIC S9(9) VALUE +36  COMP SYNC.          
013000     03  IX-PER-MAX-PLUSONE      PIC S9(9) VALUE +37  COMP SYNC.          
013100     03  IX-DT-MAX               PIC S9(9) VALUE +156 COMP SYNC.          
013200     03  IX-VECKA-MAX            PIC S9(9) VALUE +156 COMP SYNC.          
013300     03  INDEX-ONE               PIC S9(9) VALUE +1   COMP SYNC.          
013400     03  W-CDC-SEAS-JUST-FINNS   PIC X       VALUE 'N'.                   
013500     03  W-CDC-SEAS-FINNS        PIC X       VALUE 'N'.                   
013600     03  W-XDC-SEAS-FINNS        PIC X       VALUE 'N'.                   
013700     03  W-XDC-SEAS-FINNS-DC     PIC X       VALUE 'N'.                   
013800     03  WS-FLFLYG               PIC X       VALUE 'N'.                   
013900*                                                                         
014000*-- PER-TABELL ÄR EN RULLANDE TABELL DÄR                                  
014100*-- IX = 1  ÄR JANUARI                                                    
014200*-- IX = 12 ÄR DECEMBER                                                   
014300*                                                                         
014400     03 PER-TABELL OCCURS 37.                                             
014500        05 PER-PERIOD            PIC  9(2)   VALUE ZERO.                  
014600        05 PER-AA                PIC  9(2)   VALUE ZERO.                  
014700        05 PER-START-VV          PIC  9(2)   VALUE ZERO.                  
014800        05 PER-SLUT-VV           PIC  9(2)   VALUE ZERO.                  
014900                                                                          
015000     03 PERIOD-ANTAL             PIC S9(9)   VALUE +37 COMP-3.            
015100     03  WS-TIAAPER.                                                      
015200         05 TIAA                 PIC 9(2)    VALUE ZERO.                  
015300         05 PER                  PIC 9(2)    VALUE ZERO.                  
015400     03 TIAAPER REDEFINES WS-TIAAPER PIC 9(4).                            
015500                                                                          
015600*                                                                         
015700 01  WS-SOURCE-MKT-CN            PIC X(1)    VALUE 'N'.                   
015800     88 SOURCE-MKT-CN-NEJ                    VALUE 'N'.                   
015900     88 SOURCE-MKT-CN-JA                     VALUE 'J'.                   
016000*                                                                         
016100 01  WS-SOURCE-MKT-US            PIC X(1)    VALUE 'N'.                   
016200     88 SOURCE-MKT-US-NEJ                    VALUE 'N'.                   
016300     88 SOURCE-MKT-US-JA                     VALUE 'J'.                   
016400*                                                                         
016500 01  WS-DUMMY-SUPPLIER           PIC X(1)    VALUE 'N'.                   
016600     88 DUMMY-SUPPLIER-JA                    VALUE 'J'.                   
016700                                                                          
016800 01  SW-VALID-DC-FL              PIC X(1)    VALUE 'N'.                   
016900     88 VALID-DC-JA                          VALUE 'J'.                   
017000                                                                          
017100 01  SW-FIRST-EXECUTE            PIC X(1)    VALUE 'J'.                   
017200     88 FIRST-RUN-JA                         VALUE 'J'.                   
017300     88 FIRST-RUN-NEJ                        VALUE 'N'.                   
017400                                                                          
017500 01  SW-DEMAND-DC11-REF          PIC X(1)    VALUE 'N'.                   
017600     88 DC11-REF-DEMAND                      VALUE 'J'.                   
017700                                                                          
017800*      --- VALID IDDC CODES                                               
017900*                                                                         
018000*01    -COPY WWDC99                                                       
018100       EJECT                                                              
018200*                                                                         
018300     EJECT                                                                
018400 01  DYNAMISKA-SUBPROGRAM.                                                
018500*                                                                         
018600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
018700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
018800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
018900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
019000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
019100     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
019200     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
019300     EJECT                                                                
019400*    --- PARAMETRAR TILL POSTSUM                                          
019500*                                                                         
019600*01  -COPY W0005   -PRE  POSTSUM-                                         
019700     EJECT                                                                
019800 01  FILLER                  PIC X(16) VALUE 'WDATAREA        '.          
019900*    ---PARAMETRAR TILL DATKONV                                           
020000*01  -COPY WDATAREA                                                       
020100     EJECT                                                                
020200*    --- PARAMETRAR TILL WZ20DAYS                                         
020300*01 -COPY WZ20DAYS                                                        
020400     EJECT                                                                
020500*    ---VARIABLES TO SUBPROGRAM W009VADD                                  
020600 01  DATUM-AAVV                  PIC S9(5)   COMP-3.                      
020700 01  ANTAL-VECKOR                PIC S9(3)   COMP-3.                      
020800     EJECT                                                                
020900*    --- PARAMETRAR TILL ABEND                                            
021000                                                                          
021100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
021200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
021300     SKIP2                                                                
021400 01  FELTEXT.                                                             
021500     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
021600     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
021700     EJECT                                                                
021800*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
021900*                                                                         
022000     EJECT                                                                
022100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
022200     SKIP3                                                                
022300 01  NYCKLAR-TILL-DLI.                                                    
022400     03  W-IDARTNR-X.                                                     
022500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
022600     03  W-KDSEGKEY-X.                                                    
022700         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
022800     03  W-IDDC-K7-MIN-X.                                                 
022900         05  W-IDDC-K7-MIN       PIC X(2)    VALUE LOW-VALUES.            
023000     03  W-IDDC-K7-MAX-X.                                                 
023100         05  W-IDDC-K7-MAX       PIC X(2)    VALUE HIGH-VALUES.           
023200     03  W-IDDCREF-X.                                                     
023300         05  W-IDDCREF           PIC X(2)    VALUE SPACE.                 
023400     03  W-IDDC-X.                                                        
023500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
023600     03  W-IDDC-B6-X.                                                     
023700         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
023800     03  W-IDDC-B616-X.                                                   
023900         05  W-IDDC-B616         PIC X(2)    VALUE SPACE.                 
024000                                                                          
024100     SKIP2                                                                
024200*    --- STATUS-KOD FRÅN IMS                                              
024300 01  STATUS-WS                   PIC XX.                                  
024400     88  SEGMENT-FINNS                       VALUE '  '.                  
024500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
024600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
024700     SKIP2                                                                
024800 01  GODK-STATUSKODER.                                                    
024900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
025000     SKIP3                                                                
025100 01  SSA1                        PIC X(64).                               
025200 01  SSA2                        PIC X(64).                               
025300 01  SSA3                        PIC X(64).                               
025400     EJECT                                                                
025500*    --- IMS FUNKTIONSKODER                                               
025600*01  -COPY W0003                                                          
025700     EJECT                                                                
025800*    ---  DLI INPUT-OUTPUT AREA                                           
025900 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK601'.             
026000 01  DLI-IO-AREA-WDK601.                                                  
026100*        05  -COPY WDK601                                                 
026200     EJECT                                                                
026300 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK611'.             
026400 01  DLI-IO-AREA-WDK611.                                                  
026500*        05  -COPY WDK611                                                 
026600     EJECT                                                                
026700 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK626'.             
026800 01  DLI-IO-AREA-WDK626.                                                  
026900*        05  -COPY WDK626                                                 
027000     EJECT                                                                
027100 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK61129'.           
027200 01  DLI-IO-AREA-WDK61129.                                                
027300*    03  -COPY WDK611 -PRE K6-                                            
027400*    03  -COPY WDK629 -PRE K6-                                            
027500     EJECT                                                                
027600 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK701'.             
027700 01  DLI-IO-AREA-WDK701.                                                  
027800*        05  -COPY WDK701                                                 
027900     EJECT                                                                
028000 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK711'.             
028100 01  DLI-IO-AREA-WDK711.                                                  
028200*        05  -COPY WDK711                                                 
028300     EJECT                                                                
028400 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDK727'.             
028500 01  DLI-IO-AREA-WDK727.                                                  
028600*        05  -COPY WDK727                                                 
028700     EJECT                                                                
028800 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB601'.             
028900 01  DLI-IO-AREA-B601.                                                    
029000*        05  -COPY WDB601 -PRE B6-                                        
029100     EJECT                                                                
029200 01  FILLER                  PIC X(16) VALUE 'DLI-IO-WDB616'.             
029300 01  DLI-IO-AREA-B616.                                                    
029400*        05  -COPY WDB616 -PRE B6-                                        
029500     EJECT                                                                
029600 LINKAGE SECTION.                                                         
029700                                                                          
029800*    -COPY W271UTIL                                                       
029900                                                                          
030000     EJECT                                                                
030100*01  -COPY W0008      -PRE WDK6-                                          
030200     05  FILLER                  PIC X.                                   
030300     EJECT                                                                
030400*01  -COPY W0008      -PRE WDK7-                                          
030500     05  FILLER                  PIC X.                                   
030600     EJECT                                                                
030700*01  -COPY W0008      -PRE WDB6-                                          
030800     05  FILLER                  PIC X.                                   
030900     EJECT                                                                
031000 PROCEDURE DIVISION  USING UTIL-W271UTIL WDK6-PCB WDK7-PCB                
031100                           WDB6-PCB.                                      
031200                                                                          
031300     PERFORM A-INIT                                                       
031400                                                                          
031500     EVALUATE UTIL-KDCALL                                                 
031600       WHEN 001                                                           
031700         PERFORM B-CHECK-ARTICLE                                          
031800       WHEN 002                                                           
031900         PERFORM C-PROGNOS-REFILL                                         
032000       WHEN 003                                                           
032100         PERFORM D-FUT-FORECAST-VECKA                                     
032200       WHEN 004                                                           
032300         PERFORM E-FUT-FORECAST-WEEK-GXDC                                 
032400       WHEN 005                                                           
032500         PERFORM E-FUT-FORECAST-WEEK-GXDC                                 
032600       WHEN OTHER                                                         
032700         SET UTIL-KDSVAR-FEL      TO TRUE                                 
032800         MOVE 'ERROR KDCALL'      TO UTIL-TEXT                            
032900         DISPLAY 'W271UTIL ERROR KDCALL'                                  
033000     END-EVALUATE                                                         
033100                                                                          
033200     SET FIRST-RUN-NEJ         TO TRUE                                    
033300                                                                          
033400     MOVE ZERO TO RETURN-CODE                                             
033500     GOBACK                                                               
033600     .                                                                    
033700     EJECT                                                                
033800                                                                          
033900 A-INIT SECTION.                                                          
034000                                                                          
034100     MOVE SPACES                  TO UTIL-KDSVAR                          
034200     MOVE NEJ                     TO WS-DUMMY-SUPPLIER                    
034300                                     WS-FLFLYG                            
034400                                     SW-VALID-DC-FL                       
034500                                     SW-DEMAND-DC11-REF                   
034600     MOVE LOW-VALUES              TO W-IDDC-K7-MIN                        
034700     MOVE HIGH-VALUES             TO W-IDDC-K7-MAX                        
034800     MOVE ZERO                    TO WS-IN-AAVV                           
034900                                     WS-TIAAVVD-SPAR                      
035000*                                                                         
035100***  BELOW SHOULD BE RUN ONLY TIME FOR A BATCH JOB                        
035200***  WORKING TABLES FOR DATES ARE LOADED IN FIRST RUN                     
035300*                                                                         
035400     IF FIRST-RUN-JA                                                      
035500        MOVE FUNCTION CURRENT-DATE (1:8)                                  
035600                                  TO DAGENS-DATUM                         
035700        MOVE 'AAMMDD'             TO DAT-KDDATFORM                        
035800        MOVE DAGENS-AAMMDD        TO DAT-I-TIDATUM                        
035900                                                                          
036000        CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                   
036100                            DAT-O-TIDATUM DAT-KDSVAR                      
036200                                                                          
036300        IF DAT-KDSVAR-OK                                                  
036400           MOVE DAT-TIAAVVD       TO WS-TIAAVVD-CURR                      
036500                                     WS-TIAAVVD-START                     
036600        ELSE                                                              
036700           STRING ' FEL FRÅN WDATKONV I W271UTIL - CALL1'                 
036800           DELIMITED BY SIZE INTO FELTEXT-STR                             
036900           DISPLAY FELTEXT                                                
037000           PERFORM S99-ABEND                                              
037100        END-IF                                                            
037200        PERFORM S11-CALC-LT-ADJ-DATE                                      
037300     END-IF                                                               
037400*                                                                         
037500***  WHEN DATE IS PASSED FOR KDCALL 3, USE INPUT DATE                     
037600***  WITHOUT ADDITIONAL PROCESSING                                        
037700*                                                                         
037800     IF UTIL-KDCALL                = 003  AND                             
037900        UTIL-TIAAVVD               > ZERO                                 
038000        MOVE UTIL-TIAAVVD         TO WS-TIAAVVD-SPAR                      
038100        MOVE WS-TIAAVV-SPAR       TO WS-IN-AAVV                           
038200     ELSE                                                                 
038300        PERFORM AA-CALC-DATE                                              
038400     END-IF                                                               
038500     .                                                                    
038600     EJECT                                                                
038700                                                                          
038800 AA-CALC-DATE SECTION.                                                    
038900*                                                                         
039000***  CALCULATE THE LEADTIME ADJUSTED DATE                                 
039100                                                                          
039200     IF  UTIL-IDARTNR   IS NUMERIC                                        
039300     AND UTIL-IDARTNR    > ZERO                                           
039400       MOVE UTIL-IDARTNR               TO W-IDARTNR                       
039500       MOVE UTIL-IDDC                  TO WS-IDDC                         
039600                                          W-IDDC                          
039700       IF UTIL-IDDC  NOT > SPACES  AND                                    
039800          UTIL-KDCALL    = 003                                            
039900          MOVE WS-CDC-SE               TO WS-IDDC                         
040000                                          W-IDDC                          
040100       END-IF                                                             
040200                                                                          
040300       IF UTIL-KDCALL = 003                                               
040400          IF CDC-SE                                                       
040500             PERFORM IMS-GU-WDK61129                                      
040600             IF SEGMENT-FINNS                                             
040700                MOVE K6-CLAG-IDDC-REF  TO W-IDDC-B616                     
040800                MOVE WS-CDC-SE         TO W-IDDC-B6                       
040900                IF K6-CREF-FLFLYG  = JA                                   
041000                   MOVE JA             TO WS-FLFLYG                       
041100                END-IF                                                    
041200             ELSE                                                         
041300                SET UTIL-KDSVAR-FEL    TO TRUE                            
041400                MOVE 'NOT REFILL PART'                                    
041500                                       TO UTIL-TEXT                       
041600                DISPLAY 'PART NUMBER   :' UTIL-IDARTNR                    
041700             END-IF                                                       
041800          ELSE                                                            
041900             PERFORM IMS-GU-WDK711                                        
042000             IF SEGMENT-FINNS                                             
042100                MOVE SLAG-IDDC-REF     TO W-IDDC-B616                     
042200                MOVE SLAG-IDDC         TO W-IDDC-B6                       
042300                IF SLAG-FLFLYG  = JA                                      
042400                   MOVE JA             TO WS-FLFLYG                       
042500                END-IF                                                    
042600             ELSE                                                         
042700                SET UTIL-KDSVAR-FEL    TO TRUE                            
042800                MOVE 'NOT REFILL PART'                                    
042900                                       TO UTIL-TEXT                       
043000                DISPLAY 'PART NUMBER   :' UTIL-IDARTNR                    
043100             END-IF                                                       
043200          END-IF                                                          
043300*                                                                         
043400***    IF LOCAL PART, LEADTIME SHOULD BE PASSED FROM CALLING PGM          
043500*                                                                         
043600          PERFORM IMS-GU-WDB616                                           
043700          IF SEGMENT-FINNS                                                
043800             IF WS-FLFLYG            = JA                                 
043900                MOVE B6-REF-KVDLTID-AIRETA                                
044000                                       TO WS-KVDAYS                       
044100             ELSE                                                         
044200                MOVE B6-REF-KVDLTID-TOT                                   
044300                                       TO WS-KVDAYS                       
044400             END-IF                                                       
044500          ELSE                                                            
044600             SET UTIL-KDSVAR-FEL       TO TRUE                            
044700             MOVE 'REFILL RULE MISSING'                                   
044800                                       TO UTIL-TEXT                       
044900          END-IF                                                          
045000       END-IF                                                             
045100     ELSE                                                                 
045200       SET UTIL-KDSVAR-FEL             TO TRUE                            
045300       MOVE 'INVALID PART'             TO UTIL-TEXT                       
045400       DISPLAY 'PART NUMBER   :' UTIL-IDARTNR                             
045500     END-IF                                                               
045600                                                                          
045700     IF UTIL-KDSVAR-OK                                                    
045800        IF UTIL-KDCALL = 003 OR 004                                       
045900           PERFORM S11-CALC-LT-ADJ-DATE                                   
046000        END-IF                                                            
046100     END-IF                                                               
046200     .                                                                    
046300     EJECT                                                                
046400                                                                          
046500 B-CHECK-ARTICLE SECTION.                                                 
046600                                                                          
046700     IF  UTIL-IDARTNR   IS NUMERIC                                        
046800     AND UTIL-IDARTNR    > ZERO                                           
046900                                                                          
047000       MOVE UTIL-IDARTNR          TO W-IDARTNR                            
047100       PERFORM IMS-GU-WDK601                                              
047200       IF SEGMENT-FINNS                                                   
047300          MOVE ART-IDLEVNR        TO WS-IDLEVNR                           
047400          IF ART-IDLEVNR = '9998' OR ' '                                  
047500             SET DUMMY-SUPPLIER-JA                                        
047600                                  TO TRUE                                 
047700          ELSE                                                            
047800            PERFORM IMS-GNP-WDK611                                        
047900            IF SEGMENT-FINNS                                              
048000               IF CLAG-IDDC-REF    > SPACES                               
048100                  CONTINUE                                                
048200               ELSE                                                       
048300                  ADD 1           TO WS-LOCAL-COUNT                       
048400                  MOVE WS-CDC-SE  TO WS-IDDC-SPAR                         
048500               END-IF                                                     
048600            END-IF                                                        
048700          END-IF                                                          
048800       END-IF                                                             
048900                                                                          
049000       PERFORM IMS-GU-WDK701                                              
049100       IF SEGMENT-FINNS                                                   
049200          PERFORM IMS-GNP-WDK711                                          
049300          PERFORM UNTIL SEGMENT-SAKNAS                                    
049400            IF SLAG-IDDC-REF       > SPACES                               
049500               CONTINUE                                                   
049600            ELSE                                                          
049700***            WE SHOULD COUNT ONE MARKET ONLY ONCE EVEN IF               
049800***            MULTIPLE DCS IN THE MARKET SOURCES PART LOCALLY            
049900               MOVE SLAG-IDDC     TO WS-IDDC                              
050000               IF  NDC-NA                                                 
050100               AND SOURCE-MKT-US-NEJ                                      
050200                   ADD 1          TO WS-LOCAL-COUNT                       
050300                   MOVE SLAG-IDDC TO WS-IDDC-SPAR                         
050400                   SET SOURCE-MKT-US-JA                                   
050500                                  TO TRUE                                 
050600               END-IF                                                     
050700               IF  NDC-CN                                                 
050800               AND SOURCE-MKT-CN-NEJ                                      
050900                   ADD 1          TO WS-LOCAL-COUNT                       
051000                   MOVE SLAG-IDDC TO WS-IDDC-SPAR                         
051100                   SET SOURCE-MKT-CN-JA                                   
051200                                  TO TRUE                                 
051300               END-IF                                                     
051400            END-IF                                                        
051500            PERFORM IMS-GNP-WDK711                                        
051600          END-PERFORM                                                     
051700       END-IF                                                             
051800                                                                          
051900       IF WS-LOCAL-COUNT           > 1                                    
052000           MOVE 'VARIOUS'         TO UTIL-TEXT                            
052100       ELSE                                                               
052200          IF WS-LOCAL-COUNT        > ZERO                                 
052300***          CHECK WS-IDDC WITH EVALUATE                                  
052400             MOVE WS-IDDC-SPAR    TO WS-IDDC                              
052500             EVALUATE TRUE                                                
052600               WHEN CDC-SE                                                
052700                 MOVE 'CDC'       TO UTIL-TEXT                            
052800               WHEN NDC-NA                                                
052900                 MOVE 'NDC-NA'    TO UTIL-TEXT                            
053000               WHEN NDC-CN                                                
053100                 MOVE 'NDC-CN'    TO UTIL-TEXT                            
053200               WHEN OTHER                                                 
053300                 SET UTIL-KDSVAR-FEL                                      
053400                                  TO TRUE                                 
053500             END-EVALUATE                                                 
053600          ELSE                                                            
053700             IF DUMMY-SUPPLIER-JA                                         
053800                MOVE 'NONE'       TO UTIL-TEXT                            
053900             ELSE                                                         
054000                SET UTIL-KDSVAR-FEL                                       
054100                                  TO TRUE                                 
054200             END-IF                                                       
054300          END-IF                                                          
054400       END-IF                                                             
054500     END-IF                                                               
054600     .                                                                    
054700     EJECT                                                                
054800                                                                          
054900 C-PROGNOS-REFILL SECTION.                                                
055000                                                                          
055100*    CALCUATE TOTAL FORECAST REFILLED BY A CERTAIN DC                     
055200*    IDDCREF CONTAINS THE REFILLING DC                                    
055300*                                                                         
055400     MOVE ZERO                   TO UTIL-KVPB-TOT                         
055500                                    WS-KVPB-TOT                           
055600                                                                          
055700     MOVE UTIL-IDARTNR           TO W-IDARTNR                             
055800     PERFORM IMS-GU-WDK701                                                
055900     IF SEGMENT-FINNS                                                     
056000        MOVE UTIL-IDDC-REF       TO W-IDDCREF                             
056100        PERFORM IMS-GNP-WDK711-IDDCREF                                    
056200        PERFORM UNTIL SEGMENT-SAKNAS                                      
056300           COMPUTE WS-KVPB-TOT    = WS-KVPB-TOT                           
056400                                  + SLAG-KVPB-REF                         
056500                                  + SLAG-KVPBREOI                         
056600           PERFORM IMS-GNP-WDK711-IDDCREF                                 
056700        END-PERFORM                                                       
056800        MOVE WS-KVPB-TOT         TO UTIL-KVPB-TOT                         
056900     END-IF                                                               
057000     .                                                                    
057100     EJECT                                                                
057200                                                                          
057300 D-FUT-FORECAST-VECKA SECTION.                                            
057400                                                                          
057500*    CALCUATE PB-TOT FOR THE WEEK.                                        
057600*    INCLUDE FUTURE FORECAST IN CALCULATION                               
057700*                                                                         
057800     INITIALIZE                          UTIL-UTDATA                      
057900     MOVE ZERO                        TO UTIL-KVPB-TOT                    
058000                                         WS-KVPB-TOT                      
058100                                         WS-KVPB-TOT-CDC                  
058200                                         WS-KVPB-TOT-XDC                  
058300                                         WS-KVPB-TOT-XDC-NOSEAS           
058400     MOVE +1                          TO W-FAKTOR                         
058500     MOVE NEJ                         TO UTIL-FLPB-JUST                   
058600                                         UTIL-FLFFC                       
058700                                         UTIL-FLSEAS                      
058800                                         W-XDC-JUST-PB-FINNS              
058900                                         W-CDC-SEAS-FINNS                 
059000                                         W-XDC-SEAS-FINNS                 
059100                                         W-XDC-SEAS-FINNS-DC              
059200     MOVE LOW-VALUES                  TO W-IDDC-K7-MIN                    
059300     MOVE HIGH-VALUES                 TO W-IDDC-K7-MAX                    
059400                                                                          
059500     IF  UTIL-IDARTNR   IS NUMERIC                                        
059600     AND UTIL-IDARTNR    > ZERO                                           
059700                                                                          
059800       MOVE UTIL-IDARTNR              TO W-IDARTNR                        
059900       PERFORM IMS-GU-WDK601                                              
060000       IF SEGMENT-FINNS                                                   
060100          PERFORM IMS-GNP-WDK611                                          
060200          IF SEGMENT-FINNS                                                
060300             IF CLAG-IDDC-REF          > SPACES                           
060400                DIVIDE CLAG-KVPB-SEP  BY 1                                
060500                                  GIVING W-KVPB-SEP                       
060600                SUBTRACT CLAG-REDIRLEV                                    
060700                                    FROM W-FAKTOR                         
060800                MULTIPLY W-FAKTOR     BY W-KVPB-SEP                       
060900*                                                                         
061000                DIVIDE CLAG-KVPB-SATS BY 1                                
061100                                  GIVING W-KVPB-SATS                      
061200*                                                                         
061300                PERFORM S01-FINNS-JUST-PB-CDC                             
061400                IF W-CDC-JUST-PB-FINNS = JA                               
061500                   PERFORM S05A-PB-JUSTERINGAR-CDC                        
061600                END-IF                                                    
061700                PERFORM S03A-FINNS-SEAS-CDC                               
061800             END-IF                                                       
061900          END-IF                                                          
062000          COMPUTE WS-KVPB-TOT-CDC      = WS-KVPB-TOT-CDC                  
062100                                       + W-KVPB-SEP                       
062200                                       + W-KVPB-SATS                      
062300       END-IF                                                             
062400                                                                          
062500***    FIND WEEKLY FORECAST FOR XDCS REFILLED FROM CDC                    
062600       MOVE WS-CDC-SE                 TO W-IDDCREF                        
062700       PERFORM S06-WEEKLY-PB-CDC                                          
062800     END-IF                                                               
062900     COMPUTE WS-KVPB-TOT = WS-KVPB-TOT-CDC + WS-KVPB-TOT-XDC              
063000                                                                          
063100     MOVE WS-KVPB-TOT                 TO UTIL-KVPB-TOT                    
063200                                         UTIL-KVPB-TOT-NOSEAS             
063300*                                                                         
063400***  IF FUTURE FORECAST EXISTS OR SEASON EXISTS SET FLAG                  
063500     IF W-CDC-JUST-PB-FINNS = JA                                          
063600     OR W-XDC-JUST-PB-FINNS = JA                                          
063700     OR W-CDC-SEAS-FINNS    = JA                                          
063800     OR W-XDC-SEAS-FINNS    = JA                                          
063900        MOVE JA                       TO UTIL-FLPB-JUST                   
064000        IF W-CDC-JUST-PB-FINNS = JA                                       
064100        OR W-XDC-JUST-PB-FINNS = JA                                       
064200           MOVE JA                    TO UTIL-FLFFC                       
064300        END-IF                                                            
064400        IF W-CDC-SEAS-FINNS    = JA                                       
064500        OR W-XDC-JUST-PB-FINNS = JA                                       
064600           MOVE JA                    TO UTIL-FLSEAS                      
064700        END-IF                                                            
064800     END-IF                                                               
064900     .                                                                    
065000     EJECT                                                                
065100*                                                                         
065200 E-FUT-FORECAST-WEEK-GXDC SECTION.                                        
065300                                                                          
065400*    CALCUATE PB-TOT FOR THE WEEK                                         
065500*    INCLUDE FUTURE FORECAST IN CALCULATION                               
065600*                                                                         
065700     MOVE ZERO                        TO UTIL-KVPB-TOT                    
065800                                         WS-KVPB-TOT-XDC                  
065900                                         WS-KVPB-TOT-XDC-NOSEAS           
066000     MOVE NEJ                         TO UTIL-FLPB-JUST                   
066100                                         UTIL-FLFFC                       
066200                                         UTIL-FLSEAS                      
066300                                         W-XDC-JUST-PB-FINNS              
066400                                         W-XDC-SEAS-FINNS                 
066500                                         W-XDC-SEAS-FINNS-DC              
066600                                                                          
066700     IF UTIL-IDARTNR     IS NUMERIC  AND                                  
066800        UTIL-IDARTNR      > ZERO                                          
066900                                                                          
067000*                                                                         
067100***  REFILLING DC CAN BE SPACES FOR LOCALLY PURCHASED PARTS               
067200*                                                                         
067300        IF UTIL-IDDC      > SPACES                                        
067400           MOVE UTIL-IDARTNR          TO W-IDARTNR                        
067500           MOVE UTIL-IDDC             TO W-IDDC-K7-MIN                    
067600                                         W-IDDC-K7-MAX                    
067700           MOVE UTIL-IDDC-REF         TO W-IDDCREF                        
067800           PERFORM S07-WEEKLY-PB-XDC                                      
067900                                                                          
068000***  IF FUTURE FORECAST EXISTS OR SEASON EXISTS SET FLAG                  
068100           IF W-XDC-JUST-PB-FINNS      = JA                               
068200           OR W-XDC-SEAS-FINNS         = JA                               
068300              MOVE JA                 TO UTIL-FLPB-JUST                   
068400              IF W-XDC-JUST-PB-FINNS                                      
068500                                       = JA                               
068600                 MOVE JA              TO UTIL-FLFFC                       
068700              END-IF                                                      
068800              IF W-XDC-SEAS-FINNS      = JA                               
068900                 MOVE JA              TO UTIL-FLSEAS                      
069000              END-IF                                                      
069100           END-IF                                                         
069200        ELSE                                                              
069300           MOVE 'INVALID DC '                                             
069400                                      TO UTIL-TEXT                        
069500           SET UTIL-KDSVAR-FEL        TO TRUE                             
069600                                                                          
069700           DISPLAY 'W271UTIL-IDDC     :' UTIL-IDDC                        
069800           DISPLAY 'W271UTIL-IDDC-REF :' UTIL-IDDC-REF                    
069900        END-IF                                                            
070000     ELSE                                                                 
070100       MOVE 'INVALID PART NO '        TO UTIL-TEXT                        
070200       SET UTIL-KDSVAR-FEL            TO TRUE                             
070300       DISPLAY 'UTIL-IDARTNR          :' UTIL-IDARTNR                     
070400     END-IF                                                               
070500                                                                          
070600     .                                                                    
070700     EJECT                                                                
070800*                                                                         
070900 S01-FINNS-JUST-PB-CDC  SECTION.                                          
071000                                                                          
071100***  CHECK IF FUTURE FORECAST EXISTS IN WDK6                              
071200*                                                                         
071300     MOVE NEJ                   TO W-CDC-JUST-PB-FINNS                    
071400                                   W-CDC-SEAS-JUST-FINNS                  
071500     MOVE ZERO                  TO W-KVPB-JUST1                           
071600                                   W-KVPB-JUST2                           
071700                                   W-CDC-TIJUST1                          
071800                                   W-CDC-TIJUST2                          
071900     PERFORM IMS-GNP-WDK626                                               
072000     IF SEGMENT-FINNS                                                     
072100                                                                          
072200        IF JUST-KVPB-JUST (1)    > ZERO                                   
072300        OR JUST-KVPB-JUST (2)    > ZERO                                   
072400           MOVE JA              TO W-CDC-JUST-PB-FINNS                    
072500        END-IF                                                            
072600                                                                          
072700        IF JUST-KVPB-JUST (1)    > ZERO                                   
072800           DIVIDE JUST-KVPB-JUST (1) BY 1                                 
072900                            GIVING W-KVPB-JUST1                           
073000           MOVE JUST-TIPBJUST (1)                                         
073100                                TO W-CDC-TIJUST1                          
073200        END-IF                                                            
073300        IF JUST-KVPB-JUST (2)    > ZERO                                   
073400           DIVIDE JUST-KVPB-JUST (2) BY 1                                 
073500                            GIVING W-KVPB-JUST2                           
073600           MOVE JUST-TIPBJUST (2)                                         
073700                                TO W-CDC-TIJUST2                          
073800        END-IF                                                            
073900*                                                                         
074000***     CHECK IF SEASON EXISTS                                            
074100*                                                                         
074200        MOVE +1                   TO IX                                   
074300        PERFORM UNTIL          IX  > +12                                  
074400          IF JUST-RESEASON    (IX) >  0                                   
074500             IF JUST-RESEASON (IX)                                        
074600                               NOT = 1.00                                 
074700                MOVE JA           TO W-CDC-SEAS-JUST-FINNS                
074800             END-IF                                                       
074900          END-IF                                                          
075000          ADD +1                  TO IX                                   
075100        END-PERFORM                                                       
075200     END-IF                                                               
075300     .                                                                    
075400     EJECT                                                                
075500 S02-FINNS-JUST-PB-XDC  SECTION.                                          
075600                                                                          
075700***  CHECK IF FUTURE FORECAST EXISTS IN WDK7                              
075800*                                                                         
075900     MOVE ZERO                  TO W-XDC-KVPB-JUST1                       
076000                                   W-XDC-KVPB-JUST2                       
076100                                   W-XDC-TIJUST1                          
076200                                   W-XDC-TIJUST2                          
076300                                                                          
076400     MOVE SLAG-IDDC             TO W-IDDC                                 
076500     PERFORM IMS-GNP-WDK727                                               
076600     IF SEGMENT-FINNS                                                     
076700                                                                          
076800        IF PROG-KVPB-JUST(1)     > ZERO                                   
076900        OR PROG-KVPB-JUST(2)     > ZERO                                   
077000           MOVE JA              TO W-XDC-JUST-PB-FINNS                    
077100        END-IF                                                            
077200                                                                          
077300        IF PROG-KVPB-JUST(1)     > ZERO                                   
077400           DIVIDE PROG-KVPB-JUST(1)  BY 1                                 
077500                            GIVING W-XDC-KVPB-JUST1                       
077600           MOVE PROG-TIPBJUST(1)                                          
077700                                TO W-XDC-TIJUST1                          
077800        END-IF                                                            
077900        IF PROG-KVPB-JUST(2)     > ZERO                                   
078000           DIVIDE PROG-KVPB-JUST(2)  BY 1                                 
078100                            GIVING W-XDC-KVPB-JUST2                       
078200           MOVE PROG-TIPBJUST(2)                                          
078300                                TO W-XDC-TIJUST2                          
078400        END-IF                                                            
078500     END-IF                                                               
078600     .                                                                    
078700     EJECT                                                                
078800 S03A-FINNS-SEAS-CDC SECTION.                                             
078900                                                                          
079000*    IF SEASON EXISTS ON CDC, UPDATE SEASON FLAG                          
079100*                                                                         
079200     MOVE +1                TO IX                                         
079300     IF CLAG-DASEASON       >= DAGENS-DATUM                               
079400        PERFORM UNTIL    IX >  +12                                        
079500          IF CLAG-RESEASON-PLAN (IX) NOT = 1.00                           
079600             MOVE JA        TO W-CDC-SEAS-FINNS                           
079700          END-IF                                                          
079800          ADD +1            TO IX                                         
079900        END-PERFORM                                                       
080000     ELSE                                                                 
080100        IF W-CDC-SEAS-JUST-FINNS = JA                                     
080200           MOVE JA          TO W-CDC-SEAS-FINNS                           
080300        END-IF                                                            
080400     END-IF                                                               
080500     .                                                                    
080600     EJECT                                                                
080700 S03B-FINNS-SEAS-XDC SECTION.                                             
080800                                                                          
080900*    IF SEASON EXISTS ON XDC, UPDATE SEASON FLAG                          
081000*                                                                         
081100     MOVE +1                TO IX                                         
081200     PERFORM UNTIL      IX   > +12                                        
081300       IF SLAG-RESEASON (IX) NOT = 1.00                                   
081400          MOVE JA           TO W-XDC-SEAS-FINNS                           
081500                               W-XDC-SEAS-FINNS-DC                        
081600       END-IF                                                             
081700       ADD +1               TO IX                                         
081800     END-PERFORM                                                          
081900     .                                                                    
082000     EJECT                                                                
082100 S05A-PB-JUSTERINGAR-CDC  SECTION.                                        
082200                                                                          
082300***  CHECK IF LEADTIME ADJUSTED DATE IS GT FFC DATE                       
082400***  SO IF FUTURE FORECAST EXISTS WITHIN LEADTIME ADJ DATE,               
082500***  THEN THE FFC SHOULD BE CONSIDERED                                    
082600*                                                                         
082700     IF (W-KVPB-JUST1      > ZERO)  OR                                    
082800        (W-KVPB-JUST2      > ZERO)                                        
082900                                                                          
083000       IF  W-CDC-TIJUST1  <= WS-IN-AAVV                                   
083100       AND W-CDC-TIJUST1   > ZERO                                         
083200          MOVE W-KVPB-JUST1         TO W-KVPB-SEP                         
083300       END-IF                                                             
083400                                                                          
083500       IF  W-CDC-TIJUST2  <= WS-IN-AAVV                                   
083600       AND W-CDC-TIJUST2   > ZERO                                         
083700          MOVE W-KVPB-JUST2         TO W-KVPB-SEP                         
083800       END-IF                                                             
083900                                                                          
084000       IF  W-CDC-TIJUST1  <= WS-IN-AAVV                                   
084100       AND W-CDC-TIJUST2  <= WS-IN-AAVV                                   
084200       AND W-CDC-TIJUST1   > ZERO                                         
084300       AND W-CDC-TIJUST2   > ZERO                                         
084400                                                                          
084500          IF W-CDC-TIJUST1 < W-CDC-TIJUST2                                
084600            MOVE W-KVPB-JUST2       TO W-KVPB-SEP                         
084700          ELSE                                                            
084800            MOVE W-KVPB-JUST1       TO W-KVPB-SEP                         
084900          END-IF                                                          
085000       END-IF                                                             
085100     ELSE                                                                 
085200       CONTINUE                                                           
085300     END-IF                                                               
085400     .                                                                    
085500     EJECT                                                                
085600 S05B-PB-JUSTERINGAR-XDC  SECTION.                                        
085700                                                                          
085800***  CHECK IF LEADTIME ADJUSTED DATE IS GT FFC DATE                       
085900***  SO IF FUTURE FORECAST EXISTS WITHIN LEADTIME ADJ DATE,               
086000***  THEN THE FFC SHOULD BE CONSIDERED                                    
086100*                                                                         
086200     IF (W-XDC-KVPB-JUST1  > ZERO)  OR                                    
086300        (W-XDC-KVPB-JUST2  > ZERO)                                        
086400                                                                          
086500       IF  W-XDC-TIJUST1  <= WS-IN-AAVV                                   
086600       AND W-XDC-TIJUST1   > ZERO                                         
086700           MOVE W-XDC-KVPB-JUST1    TO W-KVPB-REF                         
086800       END-IF                                                             
086900                                                                          
087000       IF  W-XDC-TIJUST2  <= WS-IN-AAVV                                   
087100       AND W-XDC-TIJUST2   > ZERO                                         
087200           MOVE W-XDC-KVPB-JUST2    TO W-KVPB-REF                         
087300       END-IF                                                             
087400                                                                          
087500       IF  W-XDC-TIJUST1  <= WS-IN-AAVV                                   
087600       AND W-XDC-TIJUST2  <= WS-IN-AAVV                                   
087700       AND W-XDC-TIJUST1   > ZERO                                         
087800       AND W-XDC-TIJUST2   > ZERO                                         
087900                                                                          
088000          IF W-XDC-TIJUST1 < W-XDC-TIJUST2                                
088100            MOVE W-XDC-KVPB-JUST2   TO W-KVPB-REF                         
088200          ELSE                                                            
088300            MOVE W-XDC-KVPB-JUST1   TO W-KVPB-REF                         
088400          END-IF                                                          
088500       END-IF                                                             
088600     ELSE                                                                 
088700       CONTINUE                                                           
088800     END-IF                                                               
088900     .                                                                    
089000     EJECT                                                                
089100 S06-WEEKLY-PB-CDC SECTION.                                               
089200                                                                          
089300***  GET WEEKLY FORECAST FOR XDCS.                                        
089400***  ADJUSTED WITH FUTURE FORECAST                                        
089500***                                                                       
089600     PERFORM IMS-GU-WDK701                                                
089700     IF SEGMENT-FINNS                                                     
089800        PERFORM IMS-GNP-WDK711-IDDCREF                                    
089900        PERFORM UNTIL SEGMENT-SAKNAS                                      
090000          PERFORM S03B-FINNS-SEAS-XDC                                     
090100          DIVIDE SLAG-KVPB-REF     BY 1                                   
090200                               GIVING W-KVPB-REF                          
090300          DIVIDE SLAG-KVPBREOI     BY 1                                   
090400                               GIVING W-KVPBREOI                          
090500*                                                                         
090600***       CHECK FOR FUTURE FORECAST WITHIN LEADTIME                       
090700***       IF FFC EXISTS, THEN USE FFC TO DETERMINE DEMAND                 
090800*                                                                         
090900          PERFORM S02-FINNS-JUST-PB-XDC                                   
091000          IF W-XDC-JUST-PB-FINNS    = JA                                  
091100             PERFORM S05B-PB-JUSTERINGAR-XDC                              
091200          END-IF                                                          
091300*                                                                         
091400          COMPUTE WS-KVPB-TOT-XDC                                         
091500                                    = WS-KVPB-TOT-XDC                     
091600                                    + W-KVPB-REF                          
091700                                    + W-KVPBREOI                          
091800                                                                          
091900          PERFORM IMS-GNP-WDK711-IDDCREF                                  
092000        END-PERFORM                                                       
092100     END-IF                                                               
092200     .                                                                    
092300     EJECT                                                                
092400                                                                          
092500 S07-WEEKLY-PB-XDC SECTION.                                               
092600                                                                          
092700***  GET WEEKLY FORECAST FOR XDCS.                                        
092800***  ADJUSTED WITH FUTURE FORECAST                                        
092900***                                                                       
093000     MOVE NEJ                         TO W-XDC-SEAS-FINNS-DC              
093100                                                                          
093200     PERFORM IMS-GU-WDK701                                                
093300     IF SEGMENT-FINNS                                                     
093400        PERFORM IMS-GNP-WDK711-IDDCREF                                    
093500        IF SEGMENT-FINNS                                                  
093600           PERFORM S03B-FINNS-SEAS-XDC                                    
093700           DIVIDE SLAG-KVPB-REF       BY 1                                
093800                                  GIVING W-KVPB-REF                       
093900           DIVIDE SLAG-KVPBREOI       BY 1                                
094000                                  GIVING W-KVPBREOI                       
094100***                                                                       
094200***  THE FLAG IS SET TO YES ONLY WHEN WE NEED TO SIMULATE                 
094300***  ON SCREENS 2352, 2372 AND 2382                                       
094400***                                                                       
094500           IF UTIL-FLSIM = JA                                             
094600              IF UTIL-KVPB-REF  NOT    = SLAG-KVPB-REF                    
094700                 MOVE UTIL-KVPB-REF   TO W-KVPB-REF                       
094800              END-IF                                                      
094900                                                                          
095000              IF UTIL-KVPBREOI  NOT    = SLAG-KVPBREOI                    
095100                 MOVE UTIL-KVPBREOI   TO W-KVPBREOI                       
095200              END-IF                                                      
095300           END-IF                                                         
095400*                                                                         
095500***  CHECK FOR FUTURE FORECAST WITHIN LEADTIME                            
095600***  IF FFC EXISTS, THEN USE FFC TO DETERMINE DEMAND                      
095700*                                                                         
095800           PERFORM S02-FINNS-JUST-PB-XDC                                  
095900*                                                                         
096000***  CALCULATE 52 WEEK DEMAND USING WORKING TABLES                        
096100***  AND STORE IN COMMUNICATION AREA                                      
096200*                                                                         
096300           PERFORM S07A-CALC-156-WEEK-DEMAND                              
096400        END-IF                                                            
096500     END-IF                                                               
096600     .                                                                    
096700     EJECT                                                                
096800 S07A-CALC-156-WEEK-DEMAND SECTION.                                       
096900                                                                          
097000*                                                                         
097100***  CALCULATE DEMAND FOR 156 WEEKS                                       
097200***  IX-V IS USED TO INDICATE 156 OCCURANCES FOR 156 WEEKS                
097300***  IX-DT IS FOR TABLE THAT WE BUILD IN FURST RUN                        
097400*                                                                         
097500     MOVE +1                        TO IX-V                               
097600                                       IX-DT                              
097700     PERFORM UNTIL IX-V  > IX-VECKA-MAX                                   
097800        MOVE ZERO                   TO WS-KVPB-TOT-XDC                    
097900                                       WS-KVPB-TOT-XDC-NOSEAS             
098000                                       WS-DATUM-SEAS                      
098100*                                                                         
098200***     FOR KDCALL 003, WS-IN-AAVV ALREADY POPULATED IN                   
098300***     SECTIONS A-INIT/S12-DATE-ADJMT                                    
098400*                                                                         
098500        IF UTIL-KDCALL    = 005                                           
098600           MOVE WS-TIAAVV-T  (IX-DT)                                      
098700                                    TO WS-IN-AAVV                         
098800           MOVE WS-TIAAVVD-T (IX-DT)                                      
098900                                    TO WS-DATUM-SEAS                      
099000        ELSE                                                              
099100           IF UTIL-KDCALL = 004                                           
099200              MOVE WS-TIAAVV-SPAR   TO WS-IN-AAVV                         
099300              MOVE WS-TIAAVVD-SPAR  TO WS-DATUM-SEAS                      
099400           END-IF                                                         
099500        END-IF                                                            
099600                                                                          
099700        IF W-XDC-JUST-PB-FINNS       = JA                                 
099800           PERFORM S05B-PB-JUSTERINGAR-XDC                                
099900        END-IF                                                            
100000        COMPUTE WS-KVPB-TOT-XDC                                           
100100                                     = WS-KVPB-TOT-XDC                    
100200                                     + W-KVPB-REF                         
100300                                     + W-KVPBREOI                         
100400                                                                          
100500        MOVE WS-KVPB-TOT-XDC        TO WS-KVPB-TOT-XDC-NOSEAS             
100600*                                                                         
100700***  IF SEASON EXISTS, ADJUST DEMAND WITH SEASON FACTOR                   
100800*                                                                         
100900        IF W-XDC-SEAS-FINNS-DC       = JA                                 
101000           IF (UTIL-KDCALL           = 004  OR                            
101100               UTIL-KDCALL           = 005) AND                           
101200              WS-DATUM-SEAS          > ZERO                               
101300              MOVE +1               TO IX                                 
101400              PERFORM UNTIL IX       > PERIOD-ANTAL                       
101500                IF  WS-DATUM-AA      = PER-AA       (IX)                  
101600                AND (WS-DATUM-VV NOT < PER-START-VV (IX)                  
101700                AND  WS-DATUM-VV NOT > PER-SLUT-VV  (IX))                 
101800                    MOVE PER-PERIOD (IX)                                  
101900                                    TO IX-S                               
102000                    COMPUTE WS-KVPB-TOT-XDC                               
102100                     = (SLAG-RESEASON (IX-S)                              
102200                                     * WS-KVPB-TOT-XDC)                   
102300                END-IF                                                    
102400                ADD +1              TO IX                                 
102500              END-PERFORM                                                 
102600           END-IF                                                         
102700        END-IF                                                            
102800*                                                                         
102900***     ONLY FOR FIRST WEEK WE POPULATE PB-TOT                            
103000*                                                                         
103100        IF IX-V = INDEX-ONE                                               
103200           MOVE WS-KVPB-TOT-XDC     TO UTIL-KVPB-TOT                      
103300                                       UTIL-PBTOT-V (IX-V)                
103400           MOVE WS-KVPB-TOT-XDC-NOSEAS                                    
103500                                    TO UTIL-KVPB-TOT-NOSEAS               
103600        ELSE                                                              
103700           MOVE WS-KVPB-TOT-XDC     TO UTIL-PBTOT-V (IX-V)                
103800        END-IF                                                            
103900*                                                                         
104000***     IF KDCALL IS NOT 5, WE ONLY NEED SINGLE WEEK DEMAND               
104100*                                                                         
104200        IF UTIL-KDCALL   NOT = 005                                        
104300           MOVE IX-VECKA-MAX        TO IX-V                               
104400        END-IF                                                            
104500        ADD +1                      TO IX-V                               
104600                                       IX-DT                              
104700     END-PERFORM                                                          
104800     .                                                                    
104900     EJECT                                                                
105000 S11-CALC-LT-ADJ-DATE SECTION.                                            
105100                                                                          
105200     MOVE +1                        TO IX-DT                              
105300     MOVE WS-TIAAVVD-CURR           TO WS-TIAAVVD-START                   
105400                                                                          
105500     IF FIRST-RUN-JA                                                      
105600        PERFORM S13-CALC-PERIOD                                           
105700        MOVE WS-TIAAVVD-START       TO WS-TIAAVVD-T (IX-DT)               
105800     END-IF                                                               
105900*                                                                         
106000***  POPULATE DATE TABLE FOR THE CURRENT WEEK                             
106100*                                                                         
106200     PERFORM S12-DATE-ADJMT                                               
106300     ADD +1                         TO IX-DT                              
106400                                                                          
106500     IF FIRST-RUN-JA                                                      
106600        PERFORM UNTIL IX-DT > IX-DT-MAX                                   
106700*                                                                         
106800***     INCREMENT WEEK BY 1, POPULATE DATE TAB FOR 156 WEEKS              
106900***     WHEN CALLED FROM BATCH, THIS SHOULD HAPPEN ONLY FOR               
107000***     FIRST PART AND DC. BASICALLY FIRST TIME THE PGM IS CALLED         
107100*                                                                         
107200          MOVE WS-TIAAVV-START      TO DATUM-AAVV                         
107300          MOVE 1                    TO ANTAL-VECKOR                       
107400                                                                          
107500          CALL W009VADD          USING DATUM-AAVV                         
107600                                       ANTAL-VECKOR                       
107700                                                                          
107800          MOVE DATUM-AAVV           TO WS-TIAAVV-START                    
107900          MOVE 1                    TO WS-TID-START                       
108000*                                                                         
108100***     POPULATE DATE TABLE FOR SUBSEQUENT WEEKS                          
108200***     ONLY FOR KDCALL 005, MULTIPLE OCCURANCE OF DATE NEEDED.           
108300*                                                                         
108400          MOVE WS-TIAAVVD-START     TO WS-TIAAVVD-T (IX-DT)               
108500          ADD +1                    TO IX-DT                              
108600        END-PERFORM                                                       
108700     END-IF                                                               
108800     .                                                                    
108900     EJECT                                                                
109000 S12-DATE-ADJMT SECTION.                                                  
109100                                                                          
109200*                                                                         
109300***  ADJUST DATE WITH LEADTIME WHEN NEEDED                                
109400*                                                                         
109500     IF UTIL-KDCALL   = 004   AND                                         
109600        UTIL-TIAAVVD  > ZERO                                              
109700        MOVE UTIL-TIAAVVD           TO WS-TIAAVVD-SPAR                    
109800     ELSE                                                                 
109900       IF UTIL-KDCALL = 005                                               
110000***       DATE TABLE POPULATED IN S11- SECTION AS                         
110100***       NO ADJUSTMENT NECESSARY                                         
110200          CONTINUE                                                        
110300       ELSE                                                               
110400          MOVE WS-TIAAVVD-START     TO DAYS-TIDATE1                       
110500          MOVE WS-KVDAYS            TO DAYS-KVDAYS                        
110600                                                                          
110700          MOVE 'YYWWD'              TO DAYS-KDDATFMT1                     
110800          MOVE 'YYWWD'              TO DAYS-KDDATFMT2                     
110900          MOVE SPACE                TO DAYS-TIDATE2                       
111000                                       DAYS-IDCALEND                      
111100                                       WS-TIDATE2                         
111200                                                                          
111300          CALL WZ20DAYS  USING DAYS-WZ20DAYS                              
111400                                                                          
111500          IF DAYS-KDRC = 8                                                
111600             STRING 'FEL VID WZ20DAYS ANROP - SEC S12-'                   
111700             DELIMITED BY SIZE INTO FELTEXT                               
111800             CALL FELLOG                                                  
111900          ELSE                                                            
112000             MOVE DAYS-TIDATE2      TO WS-TIDATE2                         
112100             MOVE WS-TIDATE2-AAVVD  TO WS-TIAAVVD-SPAR                    
112200             MOVE WS-TIAAVV-SPAR    TO WS-IN-AAVV                         
112300          END-IF                                                          
112400       END-IF                                                             
112500     END-IF                                                               
112600     .                                                                    
112700     EJECT                                                                
112800 S13-CALC-PERIOD SECTION.                                                 
112900                                                                          
113000*                                                                         
113100***  POPULATE THE PERIODS FOR THE NEXT 3 YEARS - 36 PERIODS               
113200*                                                                         
113300     MOVE 'AAVVD'             TO DAT-KDDATFORM                            
113400     IF UTIL-TIAAVVD > ZERO                                               
113500        MOVE UTIL-TIAAVVD     TO DAT-I-TIDATUM                            
113600     ELSE                                                                 
113700        MOVE WS-TIAAVVD-START TO DAT-I-TIDATUM                            
113800     END-IF                                                               
113900                                                                          
114000     CALL WDATKONV         USING DAT-KDDATFORM DAT-I-TIDATUM              
114100                                 DAT-O-TIDATUM DAT-KDSVAR                 
114200                                                                          
114300     IF DAT-KDSVAR-OK                                                     
114400        MOVE DAT-TIAARP       TO WS-TIAAPER                               
114500     ELSE                                                                 
114600        STRING ' FEL FRÅN WDATKONV I W271UTIL IN SEC S13-'                
114700        DELIMITED BY SIZE INTO FELTEXT-STR                                
114800        DISPLAY FELTEXT                                                   
114900        PERFORM S99-ABEND                                                 
115000     END-IF                                                               
115100***  CHECK IF YEAR HAS 52 OR 53 WEEKS                                     
115200***                                                                       
115300     MOVE TIAA                TO WS-DAT-IN-AA                             
115400     MOVE 53                  TO WS-DAT-IN-VV                             
115500     MOVE 'AAVV  '            TO DAT-KDDATFORM                            
115600     MOVE WS-DAT-IN-AAVV      TO DAT-I-TIDATUM                            
115700     CALL WDATKONV   USING DAT-KDDATFORM DAT-I-TIDATUM                    
115800                           DAT-O-TIDATUM DAT-KDSVAR                       
115900     IF DAT-KDSVAR-OK                                                     
116000       MOVE 53                TO WS-DAT-UT-VV                             
116100     ELSE                                                                 
116200       MOVE 52                TO WS-DAT-UT-VV                             
116300     END-IF                                                               
116400*                                                                         
116500***  PERIOD TABLE POPULATED WITH 13 OCCURANCES                            
116600***  BUT ONLY 12 OCCURANCES SHOULD BE USED                                
116700***  13TH OCCURANCE, END WEEK FOR THE PERIOD IS NOT POPULATED             
116800*                                                                         
116900     MOVE +1                  TO IX                                       
117000                                                                          
117100     PERFORM UNTIL IX          > IX-PER-MAX-PLUSONE                       
117200       MOVE 'AARP  '          TO DAT-KDDATFORM                            
117300       MOVE TIAAPER           TO DAT-I-TIDATUM                            
117400       CALL WDATKONV       USING DAT-KDDATFORM DAT-I-TIDATUM              
117500                                 DAT-O-TIDATUM DAT-KDSVAR                 
117600       IF DAT-KDSVAR-OK                                                   
117700          IF PER      =  1                                                
117800            MOVE 01           TO PER-START-VV (IX)                        
117900          ELSE                                                            
118000            MOVE DAT-TIVV     TO PER-START-VV (IX)                        
118100          END-IF                                                          
118200          MOVE PER            TO PER-PERIOD   (IX)                        
118300          MOVE TIAA           TO PER-AA       (IX)                        
118400       ELSE                                                               
118500           STRING ' FEL DATUM - DATKONV I W271UTIL S13- CALL2'            
118600           DELIMITED BY SIZE INTO FELTEXT-STR                             
118700           DISPLAY FELTEXT                                                
118800           PERFORM S99-ABEND                                              
118900       END-IF                                                             
119000       ADD +1                 TO IX                                       
119100                                 PER                                      
119200       IF PER                  >  12                                      
119300         ADD +1               TO TIAA                                     
119400         MOVE 01              TO PER                                      
119500       END-IF                                                             
119600     END-PERFORM                                                          
119700                                                                          
119800     MOVE +1                  TO IX-TILL                                  
119900     MOVE +2                  TO IX-FRAN                                  
120000     PERFORM UNTIL IX-TILL     > IX-PER-MAX                               
120100       IF PER-START-VV (IX-FRAN) = 01                                     
120200          MOVE WS-DAT-UT-VV   TO PER-SLUT-VV  (IX-TILL)                   
120300       ELSE                                                               
120400          COMPUTE PER-SLUT-VV (IX-TILL)                                   
120500                               = PER-START-VV (IX-FRAN) - 1               
120600       END-IF                                                             
120700       ADD 1                  TO IX-TILL                                  
120800                                 IX-FRAN                                  
120900     END-PERFORM                                                          
121000     .                                                                    
121100     EJECT                                                                
121200                                                                          
121300 S99-ABEND SECTION.                                                       
121400                                                                          
121500     SKIP2                                                                
121600     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
121700     .                                                                    
121800     EJECT                                                                
121900                                                                          
122000* --- IMS SEKTIONER ---                                                   
122100     SKIP3                                                                
122200                                                                          
122300 IMS-GU-WDK601 SECTION.                                                   
122400                                                                          
122500     MOVE 'IMS-GU-WDK601          ' TO WS-CURRENT-SECTION                 
122600                                                                          
122700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
122800          DELIMITED BY SIZE INTO SSA1                                     
122900     MOVE '  GE'              TO GODK-STATUSKODER                         
123000     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK601 SSA1               
123100     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
123200     PERFORM IMS-STATUSKONTROLL                                           
123300     .                                                                    
123400     EJECT                                                                
123500 IMS-GNP-WDK611 SECTION.                                                  
123600                                                                          
123700     MOVE 'IMS-GNP-WDK611         ' TO WS-CURRENT-SECTION                 
123800                                                                          
123900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X')'                         
124000          DELIMITED BY SIZE INTO SSA1                                     
124100     MOVE '  GE'              TO GODK-STATUSKODER                         
124200     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK611 SSA1              
124300     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
124400     PERFORM IMS-STATUSKONTROLL                                           
124500     .                                                                    
124600     EJECT                                                                
124700 IMS-GNP-WDK626 SECTION.                                                  
124800                                                                          
124900     MOVE 'IMS-GNP-WDK626         ' TO WS-CURRENT-SECTION                 
125000                                                                          
125100     MOVE 'WDK626    '        TO SSA1                                     
125200     MOVE '  GE'              TO GODK-STATUSKODER                         
125300     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-AREA-WDK626 SSA1              
125400     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
125500     PERFORM IMS-STATUSKONTROLL                                           
125600     .                                                                    
125700     SKIP3                                                                
125800 IMS-GU-WDK61129 SECTION.                                                 
125900                                                                          
126000     MOVE 'IMS-GU-WDK61129        ' TO WS-CURRENT-SECTION                 
126100                                                                          
126200     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
126300          DELIMITED BY SIZE INTO SSA1                                     
126400     STRING 'WDK611  *D(KDSEGKEY =' W-KDSEGKEY-X ')'                      
126500          DELIMITED BY SIZE INTO SSA2                                     
126600     MOVE 'WDK629  '          TO SSA3                                     
126610     MOVE '  GE'              TO GODK-STATUSKODER                         
126800     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-AREA-WDK61129                  
126900                           SSA1 SSA2 SSA3                                 
127000     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
127100     PERFORM IMS-STATUSKONTROLL                                           
127200     .                                                                    
127300     SKIP3                                                                
127400 IMS-GU-WDK701 SECTION.                                                   
127500                                                                          
127600     MOVE 'IMS-GU-WDK701          ' TO WS-CURRENT-SECTION                 
127700                                                                          
127800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
127900          DELIMITED BY SIZE INTO SSA1                                     
128000     MOVE '  GE'              TO GODK-STATUSKODER                         
128100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK701 SSA1               
128200     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
128300     PERFORM IMS-STATUSKONTROLL                                           
128400     .                                                                    
128500     EJECT                                                                
128600 IMS-GU-WDK711 SECTION.                                                   
128700                                                                          
128800     MOVE 'IMS-GU-WDK701          ' TO WS-CURRENT-SECTION                 
128900                                                                          
129000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
129100          DELIMITED BY SIZE INTO SSA1                                     
129200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
129300          DELIMITED BY SIZE INTO SSA2                                     
129400     MOVE '  GE'            TO GODK-STATUSKODER                           
129500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-WDK711 SSA1 SSA2          
129600     MOVE WDK7-STATUS-CODE  TO STATUS-WS                                  
129700     PERFORM IMS-STATUSKONTROLL                                           
129800     .                                                                    
129900     EJECT                                                                
130000 IMS-GNP-WDK711 SECTION.                                                  
130100                                                                          
130200     MOVE 'IMS-GNP-WDK711         ' TO WS-CURRENT-SECTION                 
130300                                                                          
130400     MOVE 'WDK711   '       TO SSA1                                       
130500     MOVE '  GEGP' TO GODK-STATUSKODER                                    
130600     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK711 SSA1              
130700     MOVE WDK7-STATUS-CODE  TO STATUS-WS                                  
130800     PERFORM IMS-STATUSKONTROLL                                           
130900     .                                                                    
131000     EJECT                                                                
131100 IMS-GNP-WDK711-IDDCREF SECTION.                                          
131200                                                                          
131300     MOVE 'IMS-GNP-WDK711-IDDCREF ' TO WS-CURRENT-SECTION                 
131400                                                                          
131500     STRING 'WDK711  (IDDC    >=' W-IDDC-K7-MIN-X                         
131600                    '&IDDC    <=' W-IDDC-K7-MAX-X                         
131700                    '&IDDCREF  =' W-IDDCREF-X ')'                         
131800          DELIMITED BY SIZE INTO SSA1                                     
131900     MOVE '  GE'              TO GODK-STATUSKODER                         
132000     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK711 SSA1              
132100     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
132200     PERFORM IMS-STATUSKONTROLL                                           
132300     .                                                                    
132400     EJECT                                                                
132500 IMS-GNP-WDK727 SECTION.                                                  
132600                                                                          
132700     MOVE 'IMS-GNP-WDK727         ' TO WS-CURRENT-SECTION                 
132800                                                                          
132900     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
133000          DELIMITED BY SIZE INTO SSA1                                     
133100     MOVE 'WDK727  '          TO SSA2                                     
133200     MOVE '  GE'              TO GODK-STATUSKODER                         
133300     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-WDK727 SSA1 SSA2         
133400     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
133500     PERFORM IMS-STATUSKONTROLL                                           
133600     .                                                                    
133700     EJECT                                                                
133800 IMS-GU-WDB616      SECTION.                                              
133900                                                                          
134000     MOVE 'IMS-GU-WDB616          ' TO WS-CURRENT-SECTION                 
134100                                                                          
134200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
134300          DELIMITED BY SIZE INTO SSA1                                     
134400     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
134500          DELIMITED BY SIZE INTO SSA2                                     
134600     MOVE '  GE'              TO GODK-STATUSKODER                         
134700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B616 SSA1 SSA2            
134800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
134900     PERFORM IMS-STATUSKONTROLL                                           
135000     .                                                                    
135100     SKIP3                                                                
135200 IMS-STATUSKONTROLL SECTION.                                              
135300                                                                          
135400     SET STATUS-IX TO 1                                                   
135500     SEARCH GODK-STATUS                                                   
135600       AT END                                                             
135700         STRING 'FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                 
135800         DELIMITED BY SIZE INTO FELTEXT                                   
135900         CALL FELLOG                                                      
136000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
136100         CONTINUE                                                         
136200     END-SEARCH                                                           
136300     .                                                                    
136400     EJECT                                                                
