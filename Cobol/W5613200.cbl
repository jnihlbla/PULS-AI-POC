000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5613200.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   18/04/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        ADD PART, SUPPLIER AND PRICE INFO TO INBOUND LIST                
000900*                                                                         
001000*        THE PROGRAM READS     WDK6 WDK7 WDF1 WDD3                        
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- INBOUND LIST FOR US/CN                                     
002500     SELECT W56131                     ASSIGN TO W56132D1.                
002600     SKIP2                                                                
002700*          --- LOCAL INBOUND LIST FOR CN                                  
002800     SELECT W56132                     ASSIGN TO W56132D2.                
002900     SKIP2                                                                
003000*          --- LOCAL INBOUND LIST FOR US                                  
003100     SELECT W56133                     ASSIGN TO W56132D3.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP2                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W56131                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY W56131      -L.                                                
004200     SKIP3                                                                
004300 FD  W56132                                                               
004400     RECORDING       V                                                    
004500     BLOCK CONTAINS  0.                                                   
004600                                                                          
004700 01  UT-RECORD-CN                PIC X(235).                              
004800     SKIP3                                                                
004900 FD  W56133                                                               
005000     RECORDING       V                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300 01  UT-RECORD-US                PIC X(235).                              
005400     EJECT                                                                
005500 WORKING-STORAGE SECTION.                                                 
005600                                                                          
005700 77  IDPGM                       PIC X(8)    VALUE 'W5613200'.            
005800 77  YES                         PIC X       VALUE 'J'.                   
005910 77  NOO                         PIC X       VALUE 'N'.                   
005920 77  IX-MARKUP                   PIC S9(4)   BINARY.                      
005930 77  IX                          PIC S9(4)   BINARY.                      
006000                                                                          
006100 77  W56131-EOF-SW               PIC X       VALUE 'N'.                   
006200     88  END-OF-W56131                       VALUE 'J'.                   
006300                                                                          
006400 77  WS-PRARTBEL-PR              PIC S9(8)V9(5) COMP-3.                   
006500 77  WS-LANDING-COST             PIC S9(10)V9(5) COMP-3.                  
006520 77  WS-MARKUP                   PIC 9V9(2)  VALUE ZERO.                  
006521                                                                          
006530 77  WS-PROD-SW                  PIC X       VALUE 'N'.                   
006540     88 PRODKOD-MISSING                      VALUE 'N'.                   
006550     88 PRODKOD-FOUND                        VALUE 'J'.                   
006600                                                                          
006700 01  WS-CITY-TAB-VALUES.                                                  
006800     03  FILLER                  PIC X(20)   VALUE                        
006900                                             '41RUTHERFORD'.              
007000     03  FILLER                  PIC X(20)   VALUE                        
007100                                             '42SUWANEE'.                 
007200     03  FILLER                  PIC X(20)   VALUE                        
007300                                             '43ONTARIO'.                 
007400     03  FILLER                  PIC X(20)   VALUE                        
007500                                             '44AUBURN'.                  
007600     03  FILLER                  PIC X(20)   VALUE                        
007700                                             '45BOLINGBROO'.              
007800     03  FILLER                  PIC X(20)   VALUE                        
007900                                             '46JACKSONVIL'.              
007910     03  FILLER                  PIC X(20)   VALUE                        
007920                                             '47NORTHLAKE'.               
008000     03  FILLER                  PIC X(20)   VALUE                        
008100                                             '71SHANGHAI'.                
008200     03  FILLER                  PIC X(20)   VALUE                        
008300                                             '72BEIJING'.                 
008400     03  FILLER                  PIC X(20)   VALUE                        
008500                                             '73GUANGZHOU'.               
008510     03  FILLER                  PIC X(20)   VALUE                        
008520                                             '74CHENGDU'.                 
008530     03  FILLER                  PIC X(20)   VALUE                        
008540                                             '92BATTERI'.                 
008600 01  WS-CITY-TAB REDEFINES WS-CITY-TAB-VALUES.                            
008700     03  WS-IDDC-ADCITY OCCURS 12 TIMES                                   
008800                        ASCENDING KEY IS WS-IDDC                          
008900                        INDEXED BY ADCITY-IX.                             
009000         05  WS-IDDC             PIC X(02).                               
009100         05  WS-ADCITY           PIC X(18).                               
009200     EJECT                                                                
009300 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
009400 01  FILLER REDEFINES TODAYS-DATE.                                        
009500     03  TODAYS-DATE-YEAR        PIC 9(2).                                
009600     03  TODAYS-DATE-MONTH       PIC 9(2).                                
009700     03  TODAYS-DATE-DAY         PIC 9(2).                                
009800     EJECT                                                                
009810*01    -COPY WWMARKUP                                                     
009820*01    -COPY WWDCKONS                                                     
009900 01  GENERAL-SUBPROGRAMS.                                                 
010000*                                                                         
010100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010500     SKIP2                                                                
010600*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
010700                                                                          
010800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
011100     SKIP2                                                                
011200 01  ERROR-TEXT.                                                          
011300     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
011400     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
011500     EJECT                                                                
011600*    --- PARAMETRAR TILL POSTSUM                                          
011700*                                                                         
011800*01  -COPY W0005   -PRE  POSTSUM-                                         
011900     EJECT                                                                
012000 01  IN-AREA-START               PIC X(24)   VALUE                        
012100                                 'IN-AREA-START  '.                       
012200     SKIP2                                                                
012300                                                                          
012400*01  AREA -COPY W56131     -PRE IN-                                       
012500     EJECT                                                                
012600 01  UT-AREA-START               PIC X(24)   VALUE                        
012700                                 'UT-AREA-START  '.                       
012800     SKIP2                                                                
012900                                                                          
013000 01  UT-HEADER.                                                           
013100     03  UT-H-IDDC               PIC X(02)   VALUE 'DC'.                  
013200     03  FILLER                  PIC X(01)   VALUE ';'.                   
013300     03  UT-H-ADCITY             PIC X(04)   VALUE 'CITY'.                
013400     03  FILLER                  PIC X(01)   VALUE ';'.                   
013500     03  UT-H-IDLOPNRM           PIC X(07)   VALUE 'INVOICE'.             
013600     03  FILLER                  PIC X(01)   VALUE ';'.                   
013700     03  UT-H-IDKUNDRF           PIC X(05)   VALUE 'ORDER'.               
013800     03  FILLER                  PIC X(01)   VALUE ';'.                   
013900     03  UT-H-IDLEVNR            PIC X(06)   VALUE 'VENDOR'.              
014000     03  FILLER                  PIC X(01)   VALUE ';'.                   
014100     03  UT-H-BELEV              PIC X(11)   VALUE 'VENDOR NAME'.         
014200     03  FILLER                  PIC X(01)   VALUE ';'.                   
014300     03  UT-H-IDARTNR            PIC X(07)   VALUE 'PART NO'.             
014400     03  FILLER                  PIC X(01)   VALUE ';'.                   
014500     03  UT-H-BEART              PIC X(09)   VALUE 'PART DESC'.           
014600     03  FILLER                  PIC X(01)   VALUE ';'.                   
014700     03  UT-H-KDPRODSL           PIC X(08)   VALUE 'PROD GRP'.            
014800     03  FILLER                  PIC X(01)   VALUE ';'.                   
014900     03  UT-H-KDPSLLOC           PIC X(14)   VALUE                        
015000                                             'LOCAL PROD GRP'.            
015100     03  FILLER                  PIC X(01)   VALUE ';'.                   
015200     03  UT-H-PRARTBEL-PR        PIC X(10)   VALUE 'UNIT PRICE'.          
015300     03  FILLER                  PIC X(01)   VALUE ';'.                   
015310     03  UT-H-PRARTNTO           PIC X(19)                                
015311                                 VALUE 'TRANSFER PRICE(SEK)'.             
015320     03  FILLER                  PIC X(01)   VALUE ';'.                   
015400     03  UT-H-KVAVIS             PIC X(03)   VALUE 'QTY'.                 
015500     03  FILLER                  PIC X(01)   VALUE ';'.                   
015600     03  UT-H-KVANTMOT           PIC X(07)   VALUE 'BIN QTY'.             
015700     03  FILLER                  PIC X(01)   VALUE ';'.                   
015800     03  UT-H-LANDING-COST       PIC X(12)   VALUE                        
015900                                             'LANDING COST'.              
016000     03  FILLER                  PIC X(01)   VALUE ';'.                   
016100     03  UT-H-KDVALISO           PIC X(08)   VALUE 'CURRENCY'.            
016200     03  FILLER                  PIC X(01)   VALUE ';'.                   
016210     03  UT-H-PRKURS             PIC X(13)                                
016211                                 VALUE      'CURRENCY RATE'.              
016220     03  FILLER                  PIC X(01)   VALUE ';'.                   
016300     03  UT-H-EXT-LANDING-COST   PIC X(21)   VALUE                        
016400                                    'EXTENDED LANDING COST'.              
016500     03  FILLER                  PIC X(01)   VALUE ';'.                   
016510     03  UT-H-TEXT               PIC X(04)   VALUE                        
016520                                    'TEXT'.                               
016530     03  FILLER                  PIC X(01)   VALUE ';'.                   
016600 01  UT-AREA.                                                             
016700     03  UT-IDDC                 PIC X(02)   VALUE SPACE.                 
016800     03  FILLER                  PIC X(01)   VALUE ';'.                   
016900     03  UT-ADCITY               PIC X(20)   VALUE SPACE.                 
017000     03  FILLER                  PIC X(01)   VALUE ';'.                   
017100     03  UT-IDLOPNRM             PIC Z(8)9   VALUE ZERO.                  
017200     03  FILLER                  PIC X(01)   VALUE ';'.                   
017300     03  UT-IDKUNDRF             PIC X(10)   VALUE SPACE.                 
017400     03  FILLER                  PIC X(01)   VALUE ';'.                   
017500     03  UT-IDLEVNR              PIC X(05)   VALUE SPACE.                 
017600     03  FILLER                  PIC X(01)   VALUE ';'.                   
017700     03  UT-BELEV                PIC X(35)   VALUE SPACE.                 
017800     03  FILLER                  PIC X(01)   VALUE ';'.                   
017900     03  UT-IDARTNR              PIC Z(8)9   VALUE ZERO.                  
018000     03  FILLER                  PIC X(01)   VALUE ';'.                   
018100     03  UT-BEART                PIC X(25)   VALUE SPACE.                 
018200     03  FILLER                  PIC X(01)   VALUE ';'.                   
018300     03  UT-KDPRODSL             PIC Z(2)9   VALUE ZERO.                  
018400     03  FILLER                  PIC X(01)   VALUE ';'.                   
018500     03  UT-KDPSLLOC             PIC Z9      VALUE ZERO.                  
018600     03  FILLER                  PIC X(01)   VALUE ';'.                   
018700     03  UT-PRARTBEL-PR          PIC Z(7)9.9(5) VALUE ZERO.               
018800     03  FILLER                  PIC X(01)   VALUE ';'.                   
018810     03  UT-PRARTNTO             PIC Z(6)9.9(2)- VALUE ZERO.              
018820     03  FILLER                  PIC X(01)   VALUE ';'.                   
018900     03  UT-KVAVIS               PIC Z(6)9   VALUE ZERO.                  
019000     03  FILLER                  PIC X(01)   VALUE ';'.                   
019100     03  UT-KVANTMOT             PIC Z(6)9-  VALUE ZERO.                  
019200     03  FILLER                  PIC X(01)   VALUE ';'.                   
019300     03  UT-LANDING-COST         PIC Z(9)9.9(5) VALUE ZERO.               
019400     03  FILLER                  PIC X(01)   VALUE ';'.                   
019500     03  UT-KDVALISO             PIC X(3)    VALUE SPACE.                 
019600     03  FILLER                  PIC X(01)   VALUE ';'.                   
019610     03  UT-PRKURS               PIC Z(5)9.9(5) VALUE ZERO.               
019620     03  FILLER                  PIC X(01)   VALUE ';'.                   
019700     03  UT-EXT-LANDING-COST     PIC Z(9)9.9(5)- VALUE ZERO.              
019800     03  FILLER                  PIC X(01)   VALUE ';'.                   
019810     03  UT-TEXT                 PIC X(4)    VALUE SPACE.                 
019820     03  FILLER                  PIC X(01)   VALUE ';'.                   
019900     EJECT                                                                
020000*    --- AREAS FOR IMS-SECTIONS                                           
020100*                                                                         
020200     EJECT                                                                
020300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
020400     SKIP3                                                                
020500 01  KEYS-FOR-DLI.                                                        
020600     03  W-IDDC-X.                                                        
020700         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
020800     03  W-IDLEVNR-X.                                                     
020900         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
021000     03  W-IDARTNR-X.                                                     
021100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
021200     03  W-IDSKYLT-X.                                                     
021300         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
021310     03  W-DAPRLIST-X.                                                    
021320         05  W-DAPRLIST          PIC 9(8)    VALUE ZERO.                  
021400     SKIP2                                                                
021500*    --- STATUS-KOD FRÅN IMS                                              
021600 01  STATUS-WS                   PIC XX.                                  
021700     88  SEGMENT-FOUND                       VALUE '  '.                  
021800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
021900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
022000     SKIP2                                                                
022100 01  GOOD-STATUSCODES.                                                    
022200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
022300     SKIP3                                                                
022400 01  SSA1                        PIC X(64).                               
022500 01  SSA2                        PIC X(64).                               
022600     EJECT                                                                
022700*    --- IMS FUNCTION CODES                                               
022800*01  -COPY W0003                                                          
022900     EJECT                                                                
023000*    ---  DLI INPUT-OUTPUT AREA                                           
023100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF106'.                      
023200 01  DLI-IO-WDF106.                                                       
023300*    03  -COPY WDF106                                                     
023400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
023500 01  DLI-IO-WDD311.                                                       
023600*    03  -COPY WDD311                                                     
023700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
023800 01  DLI-IO-WDK601.                                                       
023900*    03  -COPY WDK601                                                     
024000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
024100 01  DLI-IO-WDK611.                                                       
024200*    03  -COPY WDK611                                                     
024300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
024400 01  DLI-IO-WDK711.                                                       
024500*    03  -COPY WDK711                                                     
024600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK724'.                      
024700 01  DLI-IO-WDK724.                                                       
024800*    03  -COPY WDK724                                                     
024810 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB601'.                      
024820 01  DLI-IO-WDB601.                                                       
024830*    03  -COPY WDB601                                                     
024900     EJECT                                                                
025000 LINKAGE SECTION.                                                         
025100                                                                          
025200*01  -COPY W0008  -PRE WDF1-                                              
025300     05  FILLER                  PIC X.                                   
025400     EJECT                                                                
025500*01  -COPY W0008  -PRE WDD3-                                              
025600     05  FILLER                  PIC X.                                   
025700     EJECT                                                                
025800*01  -COPY W0008  -PRE WDK6-                                              
025900     05  FILLER                  PIC X.                                   
025901     EJECT                                                                
025902*01  -COPY W0008  -PRE WDK7-                                              
025903     05  FILLER                  PIC X.                                   
025904     EJECT                                                                
025910*01  -COPY W0008   -PRE WDB6-                                             
025920     05  FILLER                  PIC X.                                   
026000     EJECT                                                                
026400 PROCEDURE DIVISION  USING WDF1-PCB WDD3-PCB                              
026500                           WDK6-PCB WDK7-PCB WDB6-PCB.                    
026600 MAIN SECTION.                                                            
026700     ENTRY 'DLITCBL' USING WDF1-PCB WDD3-PCB                              
026800                           WDK6-PCB WDK7-PCB WDB6-PCB.                    
026900                                                                          
027000                                                                          
027100     PERFORM A-INIT                                                       
027200                                                                          
027300     PERFORM S01-READ-W56131                                              
027400     PERFORM UNTIL END-OF-W56131                                          
027500       PERFORM B-PROCESS                                                  
027600       PERFORM S01-READ-W56131                                            
027700     END-PERFORM                                                          
027800                                                                          
027900                                                                          
028000     PERFORM Z-FINIT                                                      
028100                                                                          
028200     MOVE ZERO TO RETURN-CODE                                             
028300     GOBACK                                                               
028400     .                                                                    
028500     EJECT                                                                
028600 A-INIT SECTION.                                                          
028700                                                                          
028800     OPEN INPUT  W56131                                                   
028900                                                                          
029000     OPEN OUTPUT W56132                                                   
029100                 W56133                                                   
029200                                                                          
029300     ACCEPT TODAYS-DATE        FROM DATE                                  
029400     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
029500                                                                          
029600     PERFORM S11-WRITE-HEADERS                                            
029700                                                                          
029800     .                                                                    
029900     EJECT                                                                
030000 B-PROCESS SECTION.                                                       
030100                                                                          
030200     MOVE IN-IDARTNR             TO W-IDARTNR                             
030300     MOVE IN-IDDC                TO W-IDDC                                
030400     MOVE IN-IDLEVNR             TO W-IDLEVNR                             
030500                                                                          
030600     PERFORM IMS-GU-WDK711                                                
030700     IF SEGMENT-FOUND                                                     
030800       IF SLAG-IDDC-REF = SPACES                                          
031000         PERFORM BA-FETCH-WDK6                                            
031010         PERFORM BB-FETCH-WDK7                                            
031100         PERFORM BC-FETCH-WDD3                                            
031200         PERFORM BD-FETCH-WDF1                                            
031300         PERFORM BE-FETCH-CITY                                            
031400                                                                          
031500         MOVE IN-IDARTNR         TO UT-IDARTNR                            
031600         MOVE IN-IDDC            TO UT-IDDC                               
031700         MOVE IN-IDLOPNRM        TO UT-IDLOPNRM                           
031800         MOVE IN-IDKUNDRF        TO UT-IDKUNDRF                           
031900         MOVE IN-IDLEVNR         TO UT-IDLEVNR                            
032000         MOVE IN-KVAVIS          TO UT-KVAVIS                             
032100         MOVE IN-KVANTMOT        TO UT-KVANTMOT                           
032110         IF IN-KVANTMOT = ZERO                                            
032111           MOVE 'CAN' TO UT-TEXT                                          
032120         ELSE                                                             
032121           MOVE SPACE  TO UT-TEXT                                         
032130         END-IF                                                           
032200                                                                          
032300         PERFORM S11-WRITE-W56132-33                                      
032400       END-IF                                                             
032500     END-IF                                                               
032600     .                                                                    
032700     EJECT                                                                
032800 BB-FETCH-WDK7 SECTION.                                                   
032900                                                                          
033110     MOVE IN-IDLEVNR             TO W-IDLEVNR                             
033410     COMPUTE W-DAPRLIST    =   99999999 - IN-TIAVIDAT                     
033520     PERFORM IMS-GNP-WDK724                                               
033530                                                                          
033540     IF SEGMENT-MISSING                                                   
033550       MOVE SPACES               TO UT-KDVALISO                           
033580     ELSE                                                                 
033590       MOVE SPRL-KDVALISO        TO UT-KDVALISO                           
033600     END-IF                                                               
033700                                                                          
033711     IF W-IDLEVNR = '1441'                                                
033717       COMPUTE WS-LANDING-COST ROUNDED = IN-KVAVIS * WS-MARKUP            
033718                               * IN-PRARTNTO / IN-PRKURS                  
033719       ON SIZE ERROR                                                      
033720         MOVE 0                  TO WS-LANDING-COST                       
033721       END-COMPUTE                                                        
033722       MOVE WS-LANDING-COST      TO UT-LANDING-COST                       
033723                                                                          
033724                                                                          
033725       COMPUTE WS-LANDING-COST ROUNDED = IN-KVANTMOT * WS-MARKUP          
033726                               * IN-PRARTNTO / IN-PRKURS                  
033727       ON SIZE ERROR                                                      
033728         MOVE 0                  TO WS-LANDING-COST                       
033729       END-COMPUTE                                                        
033730       MOVE WS-LANDING-COST      TO UT-EXT-LANDING-COST                   
033731       MOVE IN-PRARTNTO          TO UT-PRARTNTO                           
033732       MOVE IN-PRKURS            TO UT-PRKURS                             
033733     ELSE                                                                 
033734       IF SEGMENT-MISSING                                                 
033735         MOVE ZEROES             TO UT-PRARTBEL-PR                        
033736                                    WS-PRARTBEL-PR                        
033737       ELSE                                                               
033738         MOVE SPRL-PRARTBEL-PR   TO UT-PRARTBEL-PR                        
033739                                    WS-PRARTBEL-PR                        
033740       END-IF                                                             
033741       COMPUTE WS-LANDING-COST ROUNDED = IN-KVAVIS *                      
033750                                         WS-PRARTBEL-PR                   
033760       MOVE WS-LANDING-COST      TO UT-LANDING-COST                       
033770                                                                          
033780       COMPUTE WS-LANDING-COST ROUNDED = IN-KVANTMOT *                    
033790                                         WS-PRARTBEL-PR                   
033791       MOVE WS-LANDING-COST        TO UT-EXT-LANDING-COST                 
033792     END-IF                                                               
033793                                                                          
034700                                                                          
035400                                                                          
035500     .                                                                    
035600     EJECT                                                                
035700 BA-FETCH-WDK6 SECTION.                                                   
035800                                                                          
035900     PERFORM IMS-GU-WDK601                                                
036000                                                                          
036100     MOVE ART-KDPRODSL           TO UT-KDPRODSL                           
036200                                                                          
036300     PERFORM IMS-GNP-WDK611                                               
036400     IF SEGMENT-FOUND                                                     
036500       MOVE CLAG-KDPSLLOC        TO UT-KDPSLLOC                           
036600     END-IF                                                               
036610                                                                          
036611     MOVE 1                      TO WS-MARKUP                             
036620     MOVE NOO                    TO WS-PROD-SW                            
036630                                                                          
036640     PERFORM IMS-GU-WDB601                                                
036650                                                                          
036660     PERFORM                                                              
036670     VARYING IX FROM 1 BY 1                                               
036680       UNTIL IX > MARKUP-TAB-MAX OR                                       
036690             PRODKOD-FOUND                                                
036691       IF MARKUP-LPC (IX) = CLAG-KDPSLLOC                                 
036692         SET PRODKOD-FOUND       TO TRUE                                  
036693         MOVE IX                 TO IX-MARKUP                             
036694       END-IF                                                             
036695     END-PERFORM                                                          
036696                                                                          
036697     IF PRODKOD-FOUND                                                     
036698       IF DCS-NDC-NA AND DCS-USA                                          
036699         MOVE MARKUP-FAKTOR-USA (IX-MARKUP)                               
036700                                 TO WS-MARKUP                             
036701       END-IF                                                             
036702     END-IF                                                               
036704     .                                                                    
036706     EJECT                                                                
036900 BC-FETCH-WDD3 SECTION.                                                   
037000                                                                          
037100     PERFORM IMS-GU-WDD311                                                
037200                                                                          
037300     IF SEGMENT-FOUND                                                     
037400       MOVE TEXT-BEART           TO UT-BEART                              
037500     END-IF                                                               
037600                                                                          
037700     .                                                                    
037800     EJECT                                                                
037900 BD-FETCH-WDF1 SECTION.                                                   
038000                                                                          
038100     PERFORM IMS-GU-WDF106                                                
038200                                                                          
038300     IF SEGMENT-FOUND                                                     
038400       MOVE ADR-BELEV            TO UT-BELEV                              
038500     END-IF                                                               
038600                                                                          
038700     .                                                                    
038800     EJECT                                                                
038900 BE-FETCH-CITY SECTION.                                                   
039000                                                                          
039100     SEARCH ALL WS-IDDC-ADCITY                                            
039200       AT END                                                             
039300         MOVE 'NO MATCHING CITY FOUND'                                    
039400                                 TO ERROR-TEXT-STR                        
039500         CALL FELLOG                                                      
039600       WHEN WS-IDDC (ADCITY-IX) = IN-IDDC                                 
039700         MOVE WS-ADCITY (ADCITY-IX)                                       
039800                                 TO UT-ADCITY                             
039900     END-SEARCH                                                           
040000                                                                          
040100     .                                                                    
040200     EJECT                                                                
040300 Z-FINIT SECTION.                                                         
040400     CLOSE W56131                                                         
040500           W56132                                                         
040600           W56133                                                         
040700     SKIP2                                                                
040800     MOVE 'S'                    TO POSTSUM-OPKOD                         
040900     CALL POSTSUM             USING POSTSUM-PARM                          
041000     .                                                                    
041100     EJECT                                                                
041200 S01-READ-W56131  SECTION.                                                
041300     READ W56131               INTO IN-AREA                               
041400       AT END                                                             
041500         MOVE HIGH-VALUE         TO IN-AREA                               
041600         SET END-OF-W56131       TO TRUE                                  
041700                                                                          
041800       NOT AT END                                                         
041900         MOVE 'W56131'           TO POSTSUM-FDNAMN                        
042000         MOVE 'W56132D1'         TO POSTSUM-DDNAMN2                       
042100         MOVE SPACES             TO POSTSUM-TRANSTYP                      
042200         CALL POSTSUM         USING POSTSUM-PARM                          
042300     END-READ                                                             
042400     .                                                                    
042500     EJECT                                                                
042600 S11-WRITE-HEADERS SECTION.                                               
042700                                                                          
042800     WRITE UT-RECORD-CN        FROM UT-HEADER                             
042900                                                                          
043000     WRITE UT-RECORD-US        FROM UT-HEADER                             
043100                                                                          
043200     .                                                                    
043300     EJECT                                                                
043400 S11-WRITE-W56132-33 SECTION.                                             
043500                                                                          
043600     EVALUATE IN-IDLANDX2                                                 
043700       WHEN 'CN'                                                          
043800         WRITE UT-RECORD-CN    FROM UT-AREA                               
043900         MOVE 'W56132'           TO POSTSUM-FDNAMN                        
044000         MOVE 'W56132D2'         TO POSTSUM-DDNAMN2                       
044100       WHEN 'US'                                                          
044200         WRITE UT-RECORD-US    FROM UT-AREA                               
044300         MOVE 'W56133'           TO POSTSUM-FDNAMN                        
044400         MOVE 'W56132D3'         TO POSTSUM-DDNAMN2                       
044500     END-EVALUATE                                                         
044600                                                                          
044700     INITIALIZE UT-AREA                                                   
044800                                                                          
044900     MOVE SPACES                 TO POSTSUM-TRANSTYP                      
045000     CALL POSTSUM             USING POSTSUM-PARM                          
045100     .                                                                    
045200     EJECT                                                                
045300 S99-ABEND SECTION.                                                       
045400                                                                          
045500     SKIP2                                                                
045600     MOVE 'S'                    TO POSTSUM-OPKOD                         
045700     CALL POSTSUM             USING POSTSUM-PARM                          
045800     CALL ABEND               USING RKOD-ABEND                            
045900     .                                                                    
046000     EJECT                                                                
046100* --- IMS SECTIONS  ---                                                   
046200                                                                          
046300     EJECT                                                                
046400 IMS-GU-WDF106 SECTION.                                                   
046500                                                                          
046600     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
046700             DELIMITED BY SIZE INTO SSA1                                  
046800     MOVE 'WDF106             '  TO SSA2                                  
046900     MOVE '  GE'                 TO GOOD-STATUSCODES                      
047000     CALL CBLTDLI             USING GU                                    
047100                                    WDF1-PCB                              
047200                                    DLI-IO-WDF106                         
047300                                    SSA1 SSA2                             
047400     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
047500     PERFORM IMS-STATUSCHECK                                              
047600     .                                                                    
047700     EJECT                                                                
047800 IMS-GU-WDD311 SECTION.                                                   
047900                                                                          
048000     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
048100             DELIMITED BY SIZE INTO SSA1                                  
048200     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
048300             DELIMITED BY SIZE INTO SSA2                                  
048400     MOVE '  GE'                 TO GOOD-STATUSCODES                      
048500     CALL CBLTDLI             USING GU                                    
048600                                    WDD3-PCB                              
048700                                    DLI-IO-WDD311                         
048800                                    SSA1 SSA2                             
048900     MOVE WDD3-STATUS-CODE       TO STATUS-WS                             
049000     PERFORM IMS-STATUSCHECK                                              
049100     .                                                                    
049200     EJECT                                                                
049300 IMS-GU-WDK601 SECTION.                                                   
049400                                                                          
049500     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
049600             DELIMITED BY SIZE INTO SSA1                                  
049700     MOVE '  '                   TO GOOD-STATUSCODES                      
049800     CALL CBLTDLI             USING GU                                    
049900                                    WDK6-PCB                              
050000                                    DLI-IO-WDK601                         
050100                                    SSA1                                  
050200     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
050300     PERFORM IMS-STATUSCHECK                                              
050400     .                                                                    
050500     EJECT                                                                
050600 IMS-GNP-WDK611 SECTION.                                                  
050700                                                                          
050800     MOVE 'WDK611             '  TO SSA1                                  
050900     MOVE '  GE'                 TO GOOD-STATUSCODES                      
051000     CALL CBLTDLI             USING GNP                                   
051100                                    WDK6-PCB                              
051200                                    DLI-IO-WDK611                         
051300                                    SSA1                                  
051400     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
051500     PERFORM IMS-STATUSCHECK                                              
051600     .                                                                    
051700     EJECT                                                                
051800 IMS-GU-WDK711 SECTION.                                                   
051900                                                                          
052000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
052100             DELIMITED BY SIZE INTO SSA1                                  
052200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
052300             DELIMITED BY SIZE INTO SSA2                                  
052400     MOVE '  GE'                 TO GOOD-STATUSCODES                      
052500     CALL CBLTDLI             USING GU                                    
052600                                    WDK7-PCB                              
052700                                    DLI-IO-WDK711                         
052800                                    SSA1 SSA2                             
052900     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
053000     PERFORM IMS-STATUSCHECK                                              
053100     .                                                                    
053200     EJECT                                                                
053300 IMS-GNP-WDK724 SECTION.                                                  
053400                                                                          
053410     STRING 'WDK724  (DAPRLIST>=' W-DAPRLIST                              
053420                    '&IDLEVNRP =' W-IDLEVNR-X ')'                         
053430          DELIMITED BY SIZE INTO SSA1                                     
053500     MOVE 'WDK724             '  TO SSA1                                  
053600     MOVE '  GE'                 TO GOOD-STATUSCODES                      
053700     CALL CBLTDLI             USING GNP                                   
053800                                    WDK7-PCB                              
053900                                    DLI-IO-WDK724                         
054000                                    SSA1                                  
054100     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
054200     PERFORM IMS-STATUSCHECK                                              
054300     .                                                                    
054400     EJECT                                                                
054401                                                                          
054410 IMS-GU-WDB601    SECTION.                                                
054420     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
054430             DELIMITED BY SIZE INTO SSA1                                  
054440     MOVE '  GE'                 TO GOOD-STATUSCODES                      
054450     CALL CBLTDLI             USING GU                                    
054460                                    WDB6-PCB                              
054470                                    DLI-IO-WDB601                         
054480                                    SSA1                                  
054490     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
054491     PERFORM IMS-STATUSCHECK                                              
054492     .                                                                    
054493     EJECT                                                                
054500 IMS-STATUSCHECK SECTION.                                                 
054600                                                                          
054700     SET STATUS-IX TO 1                                                   
054800     SEARCH GOOD-STATUS                                                   
054900       AT END                                                             
055000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
055100           DELIMITED BY SIZE INTO ERROR-TEXT                              
055200         DISPLAY ERROR-TEXT                                               
055300         CALL FELLOG                                                      
055400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
055500         CONTINUE                                                         
055600     END-SEARCH                                                           
055700     .                                                                    
