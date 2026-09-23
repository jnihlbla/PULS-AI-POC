000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5127000.                                                
000300 AUTHOR.         BHAT ARCHANA.                                            
000400 DATE-WRITTEN.   19/02/04.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM CHECKS FOR R32 DELIVERIES FROM EXTERNAL             
000900*        SUPPLIERS TO CDC ON LAST WORKING DAY OF THE MONTH                
001000*                                                                         
001100*        THE PROGRAM READS     WDB6                                       
001200*        THE PROGRAM READS     WDL2                                       
001210*        THE PROGRAM READS     WDK6                                       
001300*                                                                         
001400*    ABENDCODES:                                                          
001500*        U0016 -  . . . .                                                 
001600*        U1000 -  . . . .                                                 
001700*                                                                         
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100     SKIP2                                                                
002200 INPUT-OUTPUT SECTION.                                                    
002300                                                                          
002400 FILE-CONTROL.                                                            
002500     SKIP2                                                                
002600*          --- R32 DELIVERIES                                             
002700     SELECT W51270                     ASSIGN TO W51270D1.                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP2                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W51270                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  RECORD -COPY W51270 -PRE  UT-  -L.                                   
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W5127000'.            
004200 77  YES                         PIC X       VALUE 'J'.                   
004300 77  NOO                         PIC X       VALUE 'N'.                   
004301 77  WS-DATE                     PIC 9(7)    VALUE 0.                     
004310 77  WS-IDDC-IX                  PIC 9(3)    VALUE ZERO.                  
004320 77  MAX-IDDC-IX                 PIC 9(3)    VALUE ZERO.                  
004400 77  WS-EXTER-SUPPLIER           PIC X(1)    VALUE 'N'.                   
004600 77  WS-PREV-DAY-3               PIC 9(6)    VALUE 0.                     
004610 77  WS-PREV-DAY-2               PIC 9(6)    VALUE 0.                     
004620 77  WS-PREV-DAY-1               PIC 9(6)    VALUE 0.                     
004700 77  WS-VALUE                    PIC S9(9)V9(2) COMP-3 VALUE 0.           
004800 77  WS-MAX-VALUE                PIC S9(9)V9(2) COMP-3                    
004900                                 VALUE 500000.                            
005000 77  WS-CURRENT-DATE             PIC 9(8)    VALUE ZERO.                  
005010 77  WS-PRARTBES-PR              PIC S9(7)V9(2) COMP-3 VALUE 0.           
005020 77  WS-PRARTSTD                 PIC S9(7)V9(2) COMP-3 VALUE 0.           
005100     EJECT                                                                
005110 01 WS-TIUPPDAT.                                                          
005120    03 WS-TIAA                   PIC 9(2) VALUE 0.                        
005130    03 WS-TIMM                   PIC 9(2) VALUE 0.                        
005140    03 WS-TIDD                   PIC 9(2) VALUE 0.                        
005200 01  GENERAL-SUBPROGRAMS.                                                 
005300*                                                                         
005400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
005900     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
005910     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006000     SKIP2                                                                
006100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006600     SKIP2                                                                
006610 01  WS-IDDC-TABLE.                                                       
006620    03 WS-VALID-IDDC  OCCURS 300 ASCENDING KEY IS TAB-IDDC                
006630                      INDEXED BY INDX.                                    
006640       05 TAB-IDDC               PIC X(2).                                
006650       05 TAB-IDLEVNR-DC         PIC X(5).                                
006660                                                                          
006700 01  ERROR-TEXT.                                                          
006800     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
006900     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007000     EJECT                                                                
007100*    --- PARAMETRAR TILL POSTSUM                                          
007200*                                                                         
007300*01  -COPY W0005   -PRE  POSTSUM-                                         
007400     EJECT                                                                
007500*01  -COPY WORKAREA                                                       
007600     EJECT                                                                
007700*01  -COPY WZ20DAYS                                                       
007800     EJECT                                                                
007900*01  -COPY WWDC99                                                         
008000     EJECT                                                                
008100*01  -COPY WWDCKONS                                                       
008200     EJECT                                                                
008300 01  UT-AREA-START               PIC X(24)   VALUE                        
008400                                 'UT-AREA-START  '.                       
008500     SKIP2                                                                
008600                                                                          
008700*01  AREA -COPY W51270     -PRE UT-                                       
008800     EJECT                                                                
008900*    --- AREAS FOR IMS-SECTIONS                                           
009000*                                                                         
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009300     SKIP3                                                                
009400 01  KEYS-FOR-DLI.                                                        
009500     03  W-IDDC-X.                                                        
009600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
009610     03  W-IDARTNR-X.                                                     
009620         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
009630     03  W-IDLEVNR-X.                                                     
009640         05 W-IDLEVNR            PIC X(5)    VALUE SPACE.                 
009700     SKIP2                                                                
009800*    --- STATUS-KOD FRÅN IMS                                              
009900 01  STATUS-WS                   PIC XX.                                  
010000     88  SEGMENT-FOUND                       VALUE '  '.                  
010100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
010200     88  SEGMENT-END-OF-DB                   VALUE 'GB'.                  
010300     SKIP2                                                                
010400 01  GOOD-STATUSCODES.                                                    
010500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010600     SKIP3                                                                
010700 01  SSA1                        PIC X(64).                               
010800 01  SSA2                        PIC X(64).                               
010810 01  SSA3                        PIC X(64).                               
010900     EJECT                                                                
011000*    --- IMS FUNCTION CODES                                               
011100*01  -COPY W0003                                                          
011200     EJECT                                                                
011300*    ---  DLI INPUT-OUTPUT AREA                                           
011400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB6'.                        
011500 01  DLI-IO-AREA-B601.                                                    
011600*    03  -COPY WDB601                                                     
011700 01  DLI-IO-AREA.                                                         
011800     03  IO-AREA                 PIC X(1600) VALUE SPACE.                 
011900     SKIP3                                                                
012000     03  WDL201   REDEFINES IO-AREA.                                      
012100*        05  -COPY WDL201                                                 
012200     SKIP3                                                                
012300     03  WDL221   REDEFINES IO-AREA.                                      
012400*        05  -COPY WDL221                                                 
012410 01  FILLER                      PIC X(16)  VALUE 'WDK611'.               
012420 01  DLI-IO-WDK611.                                                       
012430*  03   -COPY WDK611                                                      
012431 01  FILLER                      PIC X(16)  VALUE 'WDK621'.               
012432 01  DLI-IO-WDK621.                                                       
012440*  03   -COPY WDK621                                                      
012500     EJECT                                                                
012600 LINKAGE SECTION.                                                         
012700                                                                          
012800                                                                          
012900*01  -COPY W0008  -PRE WDB6-                                              
013000     05  FILLER                  PIC X.                                   
013100                                                                          
013200*01  -COPY W0008  -PRE WDL2-                                              
013300     05  FILLER                  PIC X.                                   
013301                                                                          
013310*01  -COPY W0008  -PRE WDK6-                                              
013320     05  FILLER                  PIC X.                                   
013400                                                                          
013500     EJECT                                                                
013600 PROCEDURE DIVISION  USING WDB6-PCB WDL2-PCB WDK6-PCB.                    
013700                                                                          
013800 MAIN SECTION.                                                            
013900     ENTRY 'DLITCBL' USING WDB6-PCB WDL2-PCB WDK6-PCB.                    
014000                                                                          
014100     PERFORM A-INIT                                                       
014200                                                                          
014300     PERFORM IMS-GET-WDL2                                                 
014400     PERFORM UNTIL SEGMENT-END-OF-DB                                      
014500       EVALUATE WDL2-SEG-NAME-FB                                          
014600         WHEN 'WDL201'                                                    
014700           MOVE ART-IDARTNR      TO UT-IDARTNR                            
014800                                    W-IDARTNR                             
015110         WHEN 'WDL221'                                                    
015121          IF MOT-IDPTYP = 'R32'                                           
015130            PERFORM B-PROCESS                                             
015140          END-IF                                                          
015200       END-EVALUATE                                                       
015300       PERFORM IMS-GET-WDL2                                               
015400     END-PERFORM                                                          
015500     PERFORM Z-FINIT                                                      
015600                                                                          
015700     MOVE ZERO TO RETURN-CODE                                             
015800     GOBACK                                                               
015900     .                                                                    
016000     EJECT                                                                
016100 A-INIT SECTION.                                                          
016200                                                                          
016300     OPEN OUTPUT W51270                                                   
016400                                                                          
016500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016600     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-CURRENT-DATE                  
016700                                                                          
016800* GET THE LAST DAY OF PREVIOUS MONTH                                      
016900     MOVE WS-CURRENT-DATE (1:8)       TO DAYS-TIDATE2                     
017000     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT2                   
017100     MOVE 01                          TO DAYS-KVDAYS                      
017200     MOVE SPACE                       TO DAYS-IDCALEND                    
017300     MOVE SPACE                       TO DAYS-TIDATE1                     
017400     MOVE 'YYYYMMDD'                  TO DAYS-KDDATFMT1                   
017500     CALL WZ20DAYS                 USING DAYS-WZ20DAYS                    
017600                                                                          
017700     IF DAYS-KDRC = ZERO                                                  
017800       MOVE DAYS-TIDATE1(3:6)         TO WS-PREV-DAY-3                    
017900     ELSE                                                                 
018000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
018100     END-IF                                                               
018200                                                                          
018300     COMPUTE WS-PREV-DAY-2 = WS-PREV-DAY-3 - 1                            
018400     COMPUTE WS-PREV-DAY-1 = WS-PREV-DAY-2 - 1                            
019620                                                                          
019630     PERFORM AA-LOAD-DC-TABLE                                             
019700     .                                                                    
019800     EJECT                                                                
019810 AA-LOAD-DC-TABLE SECTION.                                                
019820                                                                          
019830     INITIALIZE WS-IDDC-TABLE                                             
019840     MOVE +1 TO WS-IDDC-IX                                                
019850                MAX-IDDC-IX                                               
019860     PERFORM IMS-GN-WDB601                                                
019870     PERFORM UNTIL SEGMENT-END-OF-DB                                      
019890       MOVE DCS-IDDC            TO TAB-IDDC(WS-IDDC-IX)                   
019924       MOVE DCS-IDLEVNR-DC      TO TAB-IDLEVNR-DC(WS-IDDC-IX)             
019932                                                                          
019933       ADD +1                   TO WS-IDDC-IX                             
019934                                   MAX-IDDC-IX                            
019935       IF WS-IDDC-IX > 300                                                
019936          MOVE 'DC-TABLE FULL'  TO ERROR-TEXT                             
019937          CALL FELLOG                                                     
019938       END-IF                                                             
019940       PERFORM IMS-GN-WDB601                                              
019941     END-PERFORM                                                          
019942     .                                                                    
019943     EJECT                                                                
019960 B-PROCESS SECTION.                                                       
020000                                                                          
020100     MOVE NOO                  TO WS-EXTER-SUPPLIER                       
020101                                                                          
020110     MOVE MOT-TIUPPDAT         TO WS-DATE                                 
020111     MOVE WS-DATE(2:2)         TO WS-TIAA                                 
020112     MOVE WS-DATE(4:2)         TO WS-TIMM                                 
020113     MOVE WS-DATE(6:2)         TO WS-TIDD                                 
020114                                                                          
020120     MOVE MOT-IDLEVNR          TO W-IDLEVNR                               
020130                                                                          
020200     IF WS-TIUPPDAT = WS-PREV-DAY-3                                       
020201     OR WS-TIUPPDAT = WS-PREV-DAY-2                                       
020202     OR WS-TIUPPDAT = WS-PREV-DAY-1                                       
020300       MOVE MOT-IDDC           TO WS-IDDC                                 
020400                                  W-IDDC                                  
020500* CHECK IF EXTERNAL SUPPLIER                                              
020600       IF CDC-SE                                                          
020610         IF MOT-KDRT = 0                                                  
020700           PERFORM BA-CHECK-SUPPLIER                                      
021500         END-IF                                                           
021510       END-IF                                                             
021600                                                                          
021700       IF WS-EXTER-SUPPLIER = YES                                         
021800          PERFORM BB-CALC-R32-VALUE                                       
021900       END-IF                                                             
022000     END-IF                                                               
022100     .                                                                    
022200     EJECT                                                                
022300 BA-CHECK-SUPPLIER SECTION.                                               
022301                                                                          
022302     SET INDX                  TO +1                                      
022303     SEARCH WS-VALID-IDDC                                                 
022304       AT END                                                             
022305          MOVE YES             TO  WS-EXTER-SUPPLIER                      
022306       WHEN TAB-IDLEVNR-DC(INDX) = MOT-IDLEVNR                            
022307          MOVE NOO             TO  WS-EXTER-SUPPLIER                      
022308                                                                          
022309     END-SEARCH                                                           
022310     .                                                                    
022311     EJECT                                                                
022320 BB-CALC-R32-VALUE SECTION.                                               
022400                                                                          
022410     PERFORM IMS-GET-WDK611                                               
022420     IF SEGMENT-FOUND                                                     
022421       MOVE CLAG-PRARTSTD      TO WS-PRARTSTD                             
022422       PERFORM IMS-GET-WDK621                                             
022423       IF SEGMENT-FOUND                                                   
022430         MOVE PRL-PRARTBES-PR  TO WS-PRARTBES-PR                          
022431       ELSE                                                               
022432         MOVE ZERO             TO WS-PRARTBES-PR                          
022440       END-IF                                                             
022450     ELSE                                                                 
022460       MOVE ZERO               TO WS-PRARTSTD                             
022461                                  WS-PRARTBES-PR                          
022470     END-IF                                                               
022480                                                                          
022490     IF WS-PRARTBES-PR NOT = 0                                            
022500       COMPUTE WS-VALUE ROUNDED = MOT-KVAVIS * WS-PRARTBES-PR             
022501     ELSE                                                                 
022502       COMPUTE WS-VALUE ROUNDED = MOT-KVAVIS * WS-PRARTSTD                
022503     END-IF                                                               
022510                                                                          
022600     IF WS-VALUE >= WS-MAX-VALUE                                          
022700       MOVE WS-TIUPPDAT        TO UT-R32-DATE                             
022800       MOVE MOT-KVAVIS         TO UT-KVAVIS                               
022900       MOVE WS-PRARTBES-PR     TO UT-PRARTBES-PR                          
022910       MOVE WS-PRARTSTD        TO UT-PRARTSTD                             
023000       MOVE WS-VALUE           TO UT-R32-VALUE                            
023100       MOVE MOT-IDLEVNR        TO UT-IDLEVNR                              
023200       PERFORM S11-WRITE-W51270                                           
023300     END-IF                                                               
023400     .                                                                    
023500     EJECT                                                                
023600 Z-FINIT SECTION.                                                         
023700     CLOSE W51270                                                         
023800     SKIP2                                                                
023900     MOVE 'S' TO POSTSUM-OPKOD                                            
024000     CALL POSTSUM USING POSTSUM-PARM                                      
024100     .                                                                    
024200     EJECT                                                                
024300 S11-WRITE-W51270 SECTION.                                                
024400                                                                          
024500     WRITE UT-RECORD FROM UT-AREA                                         
024600                                                                          
024700     MOVE 'W51270' TO POSTSUM-FDNAMN                                      
024800     MOVE 'W51270D1' TO POSTSUM-DDNAMN2                                   
024900     CALL POSTSUM USING POSTSUM-PARM                                      
025000     .                                                                    
025100     EJECT                                                                
026000* --- IMS SECTIONS  ---                                                   
026100                                                                          
026200                                                                          
026210 IMS-GN-WDB601    SECTION.                                                
026220     MOVE 'WDB601  '       TO SSA1                                        
026230     MOVE '  GB'           TO GOOD-STATUSCODES                            
026240     CALL CBLTDLI       USING GN WDB6-PCB DLI-IO-AREA-B601 SSA1           
026250     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
026260     PERFORM IMS-STATUSCHECK                                              
026270     .                                                                    
026280     EJECT                                                                
027300 IMS-GET-WDL2   SECTION.                                                  
027400                                                                          
027500     CALL CBLTDLI       USING GN WDL2-PCB DLI-IO-AREA                     
027600     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
027700     MOVE '  GAGKGB'       TO GOOD-STATUSCODES                            
027800     PERFORM IMS-STATUSCHECK                                              
027900     .                                                                    
027901     EJECT                                                                
027902 IMS-GET-WDK611 SECTION.                                                  
027903                                                                          
027904     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
027905          DELIMITED BY SIZE INTO SSA1                                     
027906     STRING 'WDK611  (KDSEGKEY =1)'                                       
027907          DELIMITED BY SIZE INTO SSA2                                     
027910     MOVE '  GE'           TO GOOD-STATUSCODES                            
027911     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
027912     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
027913     PERFORM IMS-STATUSCHECK                                              
027914     .                                                                    
027915     EJECT                                                                
027916 IMS-GET-WDK621 SECTION.                                                  
027920                                                                          
027970     STRING 'WDK621  (IDLEVNR  =' W-IDLEVNR-X ')'                         
027980          DELIMITED BY SIZE INTO SSA1                                     
027990     MOVE '  GE'           TO GOOD-STATUSCODES                            
027991     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK621 SSA1                   
027992     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
027993     PERFORM IMS-STATUSCHECK                                              
027994     .                                                                    
027995     EJECT                                                                
028000 IMS-STATUSCHECK SECTION.                                                 
028100                                                                          
028200     SET STATUS-IX TO 1                                                   
028300     SEARCH GOOD-STATUS                                                   
028400       AT END                                                             
028500         STRING ' INVALID STATUS CODE FROM IMS:' STATUS-WS                
028600           DELIMITED BY SIZE INTO ERROR-TEXT                              
028700         DISPLAY ERROR-TEXT                                               
028800         CALL FELLOG                                                      
028900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
029000         CONTINUE                                                         
029100     END-SEARCH                                                           
029200     .                                                                    
