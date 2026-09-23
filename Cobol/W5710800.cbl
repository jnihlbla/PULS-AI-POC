001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W5710800.                                                
001300 AUTHOR.         ARCHANA BHAT.                                            
001400 DATE-WRITTEN.   11/11/17.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        CREATES INVENTORY AUDIT LIST AND A LIST OF EXCLUDED PARTS        
001900*        FROM WDK7                                                        
002000*                                                                         
002101*        THE PROGRAM READS     WDB6                                       
002110*        THE PROGRAM READS     WDK7                                       
002120*        THE PROGRAM READS     WDD3                                       
002200*                                                                         
002300*    ABENDCODES:                                                          
002400*        U0016 -  . . . .                                                 
002500*        U1000 -  . . . .                                                 
002600*                                                                         
002700                                                                          
002800     SKIP3                                                                
002900 ENVIRONMENT DIVISION.                                                    
003000     SKIP2                                                                
003100 INPUT-OUTPUT SECTION.                                                    
003200                                                                          
003300 FILE-CONTROL.                                                            
003401     SKIP2                                                                
003402*          --- INVENTORY INFO                                             
003403     SELECT W571D1                     ASSIGN TO W57108D1.                
003404     SKIP2                                                                
003405*          --- INVENTORY AUDIT LIST A                                     
003406     SELECT W571D2                     ASSIGN TO W57108D2.                
003408*          --- INVENTORY AUDIT LIST B                                     
003409     SELECT W571D3                     ASSIGN TO W57108D3.                
003410*          --- INVENTORY AUDIT LIST C                                     
003411     SELECT W571D4                     ASSIGN TO W57108D4.                
003412*          --- INVENTORY AUDIT LIST D                                     
003413     SELECT W571D5                     ASSIGN TO W57108D5.                
003414*          --- INVENTORY AUDIT LIST E                                     
003415     SELECT W571D6                     ASSIGN TO W57108D6.                
003416*          --- WDK7 MISSING PARTS A                                       
003420     SELECT W571D7                     ASSIGN TO W57108D7.                
003430*          --- WDK7 MISSING PARTS B                                       
003440     SELECT W571D8                     ASSIGN TO W57108D8.                
003450*          --- WDK7 MISSING PARTS C                                       
003460     SELECT W571D9                     ASSIGN TO W57108D9.                
003470*          --- WDK7 MISSING PARTS D                                       
003480     SELECT W571D10                    ASSIGN TO W57108DA.                
003490*          --- WDK7 MISSING PARTS E                                       
003500     SELECT W571D11                    ASSIGN TO W57108DB.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004001     SKIP3                                                                
004002 FD  W571D1                                                               
004003     RECORDING       F                                                    
004004     BLOCK CONTAINS  0.                                                   
004005                                                                          
004006*01  -COPY W57105      -L.                                                
004007                                                                          
004008 FD  W571D2                                                               
004009     RECORDING       V                                                    
004010     BLOCK CONTAINS  0.                                                   
004011 01  W571D2-REC              PIC X(73).                                   
004012                                                                          
004013 FD  W571D3                                                               
004014     RECORDING       V                                                    
004015     BLOCK CONTAINS  0.                                                   
004016 01  W571D3-REC              PIC X(73).                                   
004017                                                                          
004018 FD  W571D4                                                               
004019     RECORDING       V                                                    
004020     BLOCK CONTAINS  0.                                                   
004021 01  W571D4-REC              PIC X(73).                                   
004022                                                                          
004023 FD  W571D5                                                               
004024     RECORDING       V                                                    
004025     BLOCK CONTAINS  0.                                                   
004026 01  W571D5-REC              PIC X(73).                                   
004027                                                                          
004028 FD  W571D6                                                               
004029     RECORDING       V                                                    
004030     BLOCK CONTAINS  0.                                                   
004031 01  W571D6-REC              PIC X(73).                                   
004032                                                                          
004033 FD  W571D7                                                               
004034     RECORDING       V                                                    
004035     BLOCK CONTAINS  0.                                                   
004040 01  W571D7-REC              PIC X(85).                                   
004100                                                                          
004110 FD  W571D8                                                               
004120     RECORDING       V                                                    
004130     BLOCK CONTAINS  0.                                                   
004140 01  W571D8-REC              PIC X(85).                                   
004141                                                                          
004150 FD  W571D9                                                               
004160     RECORDING       V                                                    
004170     BLOCK CONTAINS  0.                                                   
004180 01  W571D9-REC              PIC X(85).                                   
004181                                                                          
004190 FD  W571D10                                                              
004191     RECORDING       V                                                    
004192     BLOCK CONTAINS  0.                                                   
004193 01  W571D10-REC             PIC X(85).                                   
004194                                                                          
004195 FD  W571D11                                                              
004196     RECORDING       V                                                    
004197     BLOCK CONTAINS  0.                                                   
004198 01  W571D11-REC             PIC X(85).                                   
004199                                                                          
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'W5710800'.            
004500 77  YES                         PIC X       VALUE 'J'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004610 77  WS-KVLS-DEV                 PIC S9(7)   VALUE ZERO.                  
004620 77  WS-EXCL-PARTS               PIC 9(7)    VALUE ZERO.                  
004630 77  WS-AUDIT-LINES              PIC 9(7)    VALUE ZERO.                  
004640 77  WS-MAX-ROWS-1               PIC 9(5)    VALUE 10000.                 
004650 77  WS-MAX-ROWS-2               PIC 9(5)    VALUE 20000.                 
004660 77  WS-MAX-ROWS-3               PIC 9(5)    VALUE 30000.                 
004670 77  WS-MAX-ROWS-4               PIC 9(5)    VALUE 40000.                 
004680 77  WS-MAX-ROWS-5               PIC 9(5)    VALUE 50000.                 
004700 77  WS-CURRENT-DATE             PIC X(8)    VALUE SPACES.                
004800 77  WS-CURRENT-TIME             PIC 9(6)    VALUE ZERO.                  
004801                                                                          
004802 77  WS-IDSKYLT-CHINESE          PIC X(3)    VALUE 'RCN'.                 
004803 77  WS-IDSKYLT-ENGLISH          PIC X(3)    VALUE 'GB '.                 
004804                                                                          
004805 77  WS-DEL-PARTS                PIC X(15)   VALUE                        
004806               'PART DELETED'.                                            
004807 77  WS-NO-DEL-PARTS             PIC X(18)   VALUE                        
004808               'NO EXCLUDED PARTS'.                                       
004809 77  WS-NO-PARTS                 PIC X(18)   VALUE                        
004810               'NO PARTS'.                                                
004811                                                                          
004812 77  W571D1-EOF-SW               PIC X       VALUE 'N'.                   
004813     88  END-OF-W571D1                       VALUE 'J'.                   
004820 77  WS-WDK7-HDR-SW              PIC X       VALUE 'N'.                   
004830     88  WS-WRITE-WDK7-HDR                   VALUE 'Y'.                   
004840 77  WS-UPLOAD-ACS-SW            PIC X       VALUE 'N'.                   
004850     88  WS-UPLOAD-ACS                       VALUE 'Y'.                   
004860 77  WS-WRITE-D2-HDR-SW          PIC X       VALUE 'N'.                   
004870     88  WS-WRITE-D2-HDR                     VALUE 'J'.                   
004880 77  WS-WRITE-D3-HDR-SW          PIC X       VALUE 'N'.                   
004890     88  WS-WRITE-D3-HDR                     VALUE 'J'.                   
004900 77  WS-WRITE-D4-HDR-SW          PIC X       VALUE 'N'.                   
005000     88  WS-WRITE-D4-HDR                     VALUE 'J'.                   
005100 77  WS-WRITE-D5-HDR-SW          PIC X       VALUE 'N'.                   
005200     88  WS-WRITE-D5-HDR                     VALUE 'J'.                   
005300 77  WS-WRITE-D6-HDR-SW          PIC X       VALUE 'N'.                   
005400     88  WS-WRITE-D6-HDR                     VALUE 'J'.                   
005410 77  WS-WRITE-D7-HDR-SW          PIC X       VALUE 'N'.                   
005420     88  WS-WRITE-D7-HDR                     VALUE 'J'.                   
005430 77  WS-WRITE-D8-HDR-SW          PIC X       VALUE 'N'.                   
005440     88  WS-WRITE-D8-HDR                     VALUE 'J'.                   
005450 77  WS-WRITE-D9-HDR-SW          PIC X       VALUE 'N'.                   
005460     88  WS-WRITE-D9-HDR                     VALUE 'J'.                   
005470 77  WS-WRITE-D10-HDR-SW         PIC X       VALUE 'N'.                   
005480     88  WS-WRITE-D10-HDR                    VALUE 'J'.                   
005490 77  WS-WRITE-D11-HDR-SW         PIC X       VALUE 'N'.                   
005491     88  WS-WRITE-D11-HDR                    VALUE 'J'.                   
005500     EJECT                                                                
005600 01  GENERAL-SUBPROGRAMS.                                                 
005700*                                                                         
005800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006110     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006120     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
006130     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
006200                                                                          
006900 01  ERROR-TEXT.                                                          
007000     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007201     EJECT                                                                
007202*    --- PARAMETRAR TILL POSTSUM                                          
007203*                                                                         
007210*01  -COPY W0005   -PRE  POSTSUM-                                         
007211                                                                          
007212 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
007213*01  -COPY WTRAUTF8                                                       
007214*                                                                         
007215*                                                                         
007220*01  -COPY WL01TIDZ                                                       
007401     EJECT                                                                
007402 01  W57105-AREA-START           PIC X(24)   VALUE                        
007403                                 'W57105-AREA-START  '.                   
007404     SKIP2                                                                
007405                                                                          
007406*01  AREA -COPY W57105     -PRE W57105-                                   
007407     EJECT                                                                
007440 01  W571D2-AREA-START           PIC X(24)   VALUE                        
007450                                 'W571D2-AREA-START  '.                   
007460 01  W571D2-CONTROL-REC1.                                                 
007470*                                                                         
007480     03  FILLER                  PIC X(66)  VALUE                         
007490                                 ' ¤DAPW57108-001'.                       
007491     EJECT                                                                
007492 01  W571D2-CONTROL-REC2.                                                 
007493*                                                                         
007494     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
007495                                                                          
007496     03  W571D2-CTL-IDDC         PIC X(2)    VALUE SPACE.                 
007497                                                                          
007498     03  FILLER                  PIC X(66)  VALUE SPACE.                  
007499     EJECT                                                                
007500 01  W571D2-HEADER.                                                       
007501*    03 -COPY W5710801                                                    
007502*                                                                         
007503 01  W571D2-DETAIL.                                                       
007504*    03 -COPY W5710802                                                    
007510     EJECT                                                                
007520 01  W571D3-AREA-START           PIC X(24)   VALUE                        
007530                                 'W571D3-AREA-START  '.                   
007540 01  W571D3-CONTROL-REC1.                                                 
007550*                                                                         
007560     03  FILLER                  PIC X(85)  VALUE                         
007570                                 ' ¤DAPW57108-002'.                       
007580     EJECT                                                                
007590 01  W571D3-CONTROL-REC2.                                                 
007591*                                                                         
007592     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
007593                                                                          
007594     03  W571D3-CTL-IDDC         PIC X(2)    VALUE SPACE.                 
007595                                                                          
007596     03  FILLER                  PIC X(78)  VALUE SPACE.                  
007597     EJECT                                                                
007598 01  W571D3-HEADER.                                                       
007599*    03 -COPY W5710803                                                    
007600*                                                                         
007601 01  W571D3-DETAIL.                                                       
007602*    03 -COPY W5710804                                                    
007610*    --- AREAS FOR IMS-SECTIONS                                           
007700*                                                                         
007800     EJECT                                                                
007900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008000     SKIP3                                                                
008100 01  KEYS-FOR-DLI.                                                        
008220     03  W-IDDC-B6-X.                                                     
008230         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
008240     03  W-WDGXKEY-5104-X.                                                
008250          05 W-IDHTYP-5103       PIC X(4)    VALUE '5103'.                
008260          05 W-LOWVALUE          PIC X(26)   VALUE LOW-VALUE.             
008270     03  W-IDDC-5104-X.                                                   
008280         05  W-IDDC-5104         PIC X(2)    VALUE SPACE.                 
008290     03  W-IDARTNR-X.                                                     
008291         05  W-IDARTNR           PIC S9(9)   VALUE ZERO  COMP-3.          
008293     03  W-IDDC-X.                                                        
008294         05  W-IDDC              PIC X(2).                                
008297     03  W-IDSKYLT-X.                                                     
008298         05  W-IDSKYLT            PIC X(3)    VALUE SPACE.                
008300     SKIP2                                                                
008400*    --- STATUS-KOD FRÅN IMS                                              
008500 01  STATUS-WS                   PIC XX.                                  
008600     88  SEGMENT-FOUND                       VALUE '  '.                  
008700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008900     SKIP2                                                                
009000 01  GOOD-STATUSCODES.                                                    
009100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009500     EJECT                                                                
009600*    --- IMS FUNCTION CODES                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010101 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
010102 01  DLI-IO-WDB601.                                                       
010103*    03  -COPY WDB601                                                     
010104 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
010105 01  DLI-IO-WDK701.                                                       
010106*    03  -COPY WDK701                                                     
010107     EJECT                                                                
010108 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
010109 01  DLI-IO-WDK711.                                                       
010110*    03  -COPY WDK711                                                     
010120 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5104'.                    
010130 01  DLI-IO-WDGX5104.                                                     
010140*    03  -COPY WDGX5104                                                   
010150 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
010160 01  DLI-IO-WDD311.                                                       
010170*    03  -COPY WDD311                                                     
010400     EJECT                                                                
010500 LINKAGE SECTION.                                                         
010600                                                                          
010701                                                                          
010702*01  -COPY W0008  -PRE WDB6-                                              
010703     05  FILLER                  PIC X.                                   
010704                                                                          
010705*01  -COPY W0008  -PRE WDK7-                                              
010710     05  FILLER                  PIC X.                                   
010720*01  -COPY W0008  -PRE 5104-                                              
010730     05  FILLER                  PIC X.                                   
010740*01  -COPY W0008  -PRE WDD3-                                              
010750     05  FILLER                  PIC X.                                   
010800     EJECT                                                                
010901 PROCEDURE DIVISION  USING WDK7-PCB 5104-PCB WDB6-PCB WDD3-PCB.           
010902 MAIN SECTION.                                                            
010910     ENTRY 'DLITCBL' USING WDK7-PCB 5104-PCB WDB6-PCB WDD3-PCB.           
011000                                                                          
011200                                                                          
011300     PERFORM A-INIT                                                       
011400                                                                          
011510     PERFORM S01-READ-W571D1                                              
011520     PERFORM B-GET-DATE-TIME-FROM-WDB6                                    
011530     PERFORM C-READ-WDR2                                                  
012700     PERFORM Z-FINIT                                                      
012800                                                                          
012900     MOVE ZERO TO RETURN-CODE                                             
013000     GOBACK                                                               
013100     .                                                                    
013200     EJECT                                                                
013300 A-INIT SECTION.                                                          
013401                                                                          
013410     OPEN INPUT  W571D1                                                   
013501                                                                          
013502     OPEN OUTPUT W571D2                                                   
013510                 W571D3                                                   
013520                 W571D4                                                   
013530                 W571D5                                                   
013540                 W571D6                                                   
013550                 W571D7                                                   
013560                 W571D8                                                   
013570                 W571D9                                                   
013580                 W571D10                                                  
013590                 W571D11                                                  
013600                                                                          
013700     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-CURRENT-DATE                 
013800     MOVE FUNCTION CURRENT-DATE (9:6)  TO WS-CURRENT-TIME                 
013810     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014000     .                                                                    
014100     EJECT                                                                
014110*-----------------------------------------------------------------        
014120*  GET THE LOCAL DATE AND TIME USING THE TIMEZONE VALUE PRESENT IN        
014130*  WDB6 DATABASE.                                                         
014140*-----------------------------------------------------------------        
014150*                                                                         
014160 B-GET-DATE-TIME-FROM-WDB6 SECTION.                                       
014170                                                                          
014180     MOVE W57105-IDDC              TO W-IDDC-B6                           
014190     PERFORM IMS-GU-WDB601                                                
014191     IF SEGMENT-FOUND                                                     
014192        MOVE '011'                 TO MSGI-KDCALL                         
014193        MOVE DCS-IDTIDZON          TO MSGI-IDTIDZON                       
014193        MOVE DCS-IDDC              TO MSGI-IDDC                           
014194        MOVE WS-CURRENT-DATE(3:6)  TO MSGI-TILOKDAT                       
014195        MOVE WS-CURRENT-TIME(1:4)  TO MSGI-TILOKTID                       
014196        CALL WL01TIDZ   USING      MSGI-WL01TIDZ                          
014197                                                                          
014198        STRING WS-CURRENT-DATE(1:2)                                       
014199               MSGI-TILOKDAT DELIMITED BY SIZE                            
014200                                    INTO HEAD-TIDATETIME(1:8)             
014201        MOVE HEAD-TIDATETIME(1:8)     TO HDR-TIDATETIME(1:8)              
014210                                                                          
014220        STRING MSGI-TILOKTID WS-CURRENT-TIME(5:2)                         
014230               DELIMITED BY SIZE    INTO HEAD-TIDATETIME(9:6)             
014231        MOVE HEAD-TIDATETIME(9:6)     TO HDR-TIDATETIME(9:6)              
014240     END-IF                                                               
014250     .                                                                    
014251 C-READ-WDR2 SECTION.                                                     
014252                                                                          
014253     MOVE YES     TO WS-WRITE-D2-HDR-SW                                   
014254                     WS-WRITE-D3-HDR-SW                                   
014255                     WS-WRITE-D4-HDR-SW                                   
014256                     WS-WRITE-D5-HDR-SW                                   
014257                     WS-WRITE-D6-HDR-SW                                   
014258                     WS-WRITE-D7-HDR-SW                                   
014259                     WS-WRITE-D8-HDR-SW                                   
014260                     WS-WRITE-D9-HDR-SW                                   
014261                     WS-WRITE-D10-HDR-SW                                  
014262                     WS-WRITE-D11-HDR-SW                                  
014263     MOVE W57105-IDDC           TO W-IDDC-5104                            
014264     PERFORM IMS-GU-WDGX5104                                              
014265     IF SEGMENT-FOUND                                                     
014266        IF 5104-KDACS = 'U'                                               
014267           SET WS-UPLOAD-ACS    TO TRUE                                   
014268           MOVE W57105-IDDC     TO W571D2-CTL-IDDC                        
014269                                   HEAD-IDDC                              
014270           MOVE '1         '    TO HEAD-IDAFPRCD                          
014271           PERFORM S02-WRITE-W571D2-HEADER                                
014272        END-IF                                                            
014273     END-IF                                                               
014274                                                                          
014275     IF WS-UPLOAD-ACS                                                     
014276        SET WS-WRITE-WDK7-HDR TO TRUE                                     
014277        PERFORM UNTIL END-OF-W571D1                                       
014278          PERFORM CA-CHECK-PARTS-IN-WDK7                                  
014280          PERFORM S01-READ-W571D1                                         
014290        END-PERFORM                                                       
014292        IF WS-EXCL-PARTS = 0                                              
014293           MOVE '2         '     TO LINE2-IDAFPRCD                        
014294           MOVE 0                TO LINE2-IDARTNR                         
014295           MOVE SPACES           TO LINE2-BEART                           
014296                                                                          
014297*    WE NEED SOME ADJUSTMENT TO UTF8                                      
014298*    BEFORE DISPLAY OF SPACES                                             
014299                                                                          
014300           MOVE LINE2-BEART         TO TRAUTF8-TECONV-FROM                
014301           MOVE '278 '              TO TRAUTF8-KDCP                       
014302           MOVE 25                  TO TRAUTF8-KVMAXTL                    
014303           CALL WTRAUTF8 USING TRAUTF8-AREA                               
014304           MOVE TRAUTF8-TECONV-TO   TO LINE2-BEART                        
014305                                                                          
014306           MOVE WS-NO-DEL-PARTS     TO LINE2-MESSAGE                      
014307           PERFORM S07-WRITE-W571D7-DETAIL                                
014308        END-IF                                                            
014309        IF WS-AUDIT-LINES = 0                                             
014310           MOVE '2         '     TO LINE-IDAFPRCD                         
014311           MOVE 0                TO LINE-IDARTNR                          
014312                                    LINE-ADLAGOMR                         
014313                                    LINE-ADGANG                           
014314                                    LINE-ADPLATS                          
014315                                    LINE-KVLS-DEV                         
014316           MOVE WS-NO-PARTS      TO LINE-BEART                            
014317                                                                          
014318*    WE NEED SOME ADJUSTMENT TO UTF8                                      
014319*    BEFORE DISPLAY OF SPACES                                             
014320                                                                          
014321           MOVE LINE-BEART          TO TRAUTF8-TECONV-FROM                
014322           MOVE '278 '              TO TRAUTF8-KDCP                       
014323           MOVE 25                  TO TRAUTF8-KVMAXTL                    
014324           CALL WTRAUTF8 USING TRAUTF8-AREA                               
014325           MOVE TRAUTF8-TECONV-TO   TO LINE-BEART                         
014326                                                                          
014327           PERFORM S02-WRITE-W571D2-DETAIL                                
014328        END-IF                                                            
014329     END-IF                                                               
014330     .                                                                    
014331                                                                          
014332 CA-CHECK-PARTS-IN-WDK7 SECTION.                                          
014333     IF WS-WRITE-WDK7-HDR                                                 
014334        MOVE W57105-IDDC   TO W571D3-CTL-IDDC                             
014335                              HDR-IDDC                                    
014336        MOVE '1         '  TO HDR-IDAFPRCD                                
014337        PERFORM S07-WRITE-W571D7-HEADER                                   
014338        MOVE NOO           TO WS-WDK7-HDR-SW                              
014339     END-IF                                                               
014340                                                                          
014341     IF W57105-ADLAGOMR NOT = LOW-VALUES                                  
014342       MOVE W57105-IDARTNR   TO W-IDARTNR                                 
014343       MOVE W57105-IDDC      TO W-IDDC                                    
014344       PERFORM IMS-GU-WDK711                                              
014345       IF SEGMENT-FOUND                                                   
014346          PERFORM CAA-CREATE-AUDIT-LIST                                   
014347       ELSE                                                               
014348          PERFORM CAB-CREATE-EXCL-PARTS-LIST                              
014349       END-IF                                                             
014350     END-IF                                                               
014351     .                                                                    
014352 CAA-CREATE-AUDIT-LIST SECTION.                                           
014353     IF W57105-ADLAGOMR NOT = LOW-VALUES                                  
014354       IF W57105-IDCOUNTER-T-REG NOT = SPACE                              
014355          IF W57105-KVTCOUNT NOT = W57105-KVLS                            
014356             COMPUTE WS-KVLS-DEV = W57105-KVLS - W57105-KVTCOUNT          
014357             PERFORM CAAA-CREATE-AUDIT-LIST-LINE                          
014358          END-IF                                                          
014359       ELSE                                                               
014360          IF W57105-IDCOUNTER-R-REG NOT = SPACE                           
014361             IF W57105-KVRCOUNT NOT = W57105-KVLS                         
014362                COMPUTE WS-KVLS-DEV =                                     
014363                        W57105-KVLS - W57105-KVRCOUNT                     
014364                PERFORM CAAA-CREATE-AUDIT-LIST-LINE                       
014365             END-IF                                                       
014366          ELSE                                                            
014367             IF W57105-KVPCOUNT NOT = W57105-KVLS                         
014368                COMPUTE WS-KVLS-DEV =                                     
014369                        W57105-KVLS - W57105-KVPCOUNT                     
014370                PERFORM CAAA-CREATE-AUDIT-LIST-LINE                       
014371             END-IF                                                       
014372          END-IF                                                          
014373       END-IF                                                             
014374     END-IF                                                               
014375     .                                                                    
014376*-----------------------------------------------------------------        
014377* CREATE LINES  OF THE INVENTORY AUDIT LIST                               
014378*-----------------------------------------------------------------        
014379 CAAA-CREATE-AUDIT-LIST-LINE SECTION.                                     
014380     MOVE '2         '     TO LINE-IDAFPRCD                               
014381     MOVE W57105-IDARTNR   TO LINE-IDARTNR                                
014382     MOVE W57105-BEART     TO LINE-BEART                                  
014383                                                                          
014384*    WE NEED SOME ADJUSTMENT FOR CHINESE NAME                             
014385*    BEFORE DISPLAY OF LINE-BEART                                         
014386                                                                          
014387     MOVE W57105-IDARTNR        TO W-IDARTNR                              
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
014395     PERFORM IMS-GU-WDD311                                                
014396     IF SEGMENT-FOUND                                                     
014397        MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                    
014398     ELSE                                                                 
014399        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
014400        MOVE '278 '             TO TRAUTF8-KDCP                           
014401     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
014402     MOVE 25                    TO TRAUTF8-KVMAXTL                        
014403     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
014404     MOVE TRAUTF8-TECONV-TO     TO LINE-BEART                             
014405                                                                          
014406     MOVE W57105-ADLAGOMR  TO LINE-ADLAGOMR                               
014407     MOVE W57105-ADGANG    TO LINE-ADGANG                                 
014408     MOVE W57105-ADPLATS   TO LINE-ADPLATS                                
014409     COMPUTE WS-KVLS-DEV = WS-KVLS-DEV * -1                               
014410     COMPUTE LINE-SUAVCOST-DEV = WS-KVLS-DEV * W57105-PRAVCOST            
014411     MOVE WS-KVLS-DEV      TO LINE-KVLS-DEV                               
014412     ADD +1                TO WS-AUDIT-LINES                              
014413                                                                          
014414     PERFORM CAAAA-WRITE-AUDIT-REPORT                                     
014415     .                                                                    
014416 CAB-CREATE-EXCL-PARTS-LIST SECTION.                                      
014417                                                                          
014418     MOVE '2         '     TO LINE2-IDAFPRCD                              
014419     MOVE W57105-IDARTNR   TO LINE2-IDARTNR                               
014420     MOVE W57105-BEART     TO LINE2-BEART                                 
014421                                                                          
014422*    WE NEED SOME ADJUSTMENT FOR CHINESE NAME                             
014423*    BEFORE DISPLAY OF LINE2-BEART                                        
014424                                                                          
014425     MOVE W57105-IDARTNR        TO W-IDARTNR                              
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
014433     PERFORM IMS-GU-WDD311                                                
014434     IF SEGMENT-FOUND                                                     
014435        MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                    
014436     ELSE                                                                 
014437        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
014438        MOVE '278 '             TO TRAUTF8-KDCP                           
014439     END-IF                                                               
014440     MOVE 25                    TO TRAUTF8-KVMAXTL                        
014441     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
014442     MOVE TRAUTF8-TECONV-TO     TO LINE2-BEART                            
014443                                                                          
014444     MOVE WS-DEL-PARTS     TO LINE2-MESSAGE                               
014445     ADD +1                TO WS-EXCL-PARTS                               
014446                                                                          
014447     PERFORM CABA-WRITE-EXCL-PART-REPORT                                  
014448     .                                                                    
014449                                                                          
014450 CAAAA-WRITE-AUDIT-REPORT SECTION.                                        
014451     EVALUATE TRUE                                                        
014452     WHEN WS-AUDIT-LINES <= WS-MAX-ROWS-2                                 
014453     AND  WS-AUDIT-LINES > WS-MAX-ROWS-1                                  
014454        IF WS-WRITE-D2-HDR                                                
014455           PERFORM S02-WRITE-W571D2-HEADER                                
014456           MOVE NOO  TO WS-WRITE-D2-HDR-SW                                
014457        END-IF                                                            
014458     WHEN WS-AUDIT-LINES <= WS-MAX-ROWS-3                                 
014459     AND  WS-AUDIT-LINES > WS-MAX-ROWS-2                                  
014460        IF WS-WRITE-D3-HDR                                                
014461           PERFORM S03-WRITE-W571D3-HEADER                                
014462           MOVE NOO  TO WS-WRITE-D3-HDR-SW                                
014463        END-IF                                                            
014464     WHEN WS-AUDIT-LINES <= WS-MAX-ROWS-4                                 
014465     AND  WS-AUDIT-LINES > WS-MAX-ROWS-3                                  
014466        IF WS-WRITE-D4-HDR                                                
014467           PERFORM S04-WRITE-W571D4-HEADER                                
014468           MOVE NOO  TO WS-WRITE-D4-HDR-SW                                
014469        END-IF                                                            
014470     WHEN WS-AUDIT-LINES <= WS-MAX-ROWS-5                                 
014471     AND  WS-AUDIT-LINES > WS-MAX-ROWS-4                                  
014472        IF WS-WRITE-D5-HDR                                                
014473           PERFORM S05-WRITE-W571D5-HEADER                                
014474           MOVE NOO  TO WS-WRITE-D5-HDR-SW                                
014475        END-IF                                                            
014476     END-EVALUATE                                                         
014477                                                                          
014478     EVALUATE TRUE                                                        
014479     WHEN WS-AUDIT-LINES <= WS-MAX-ROWS-1                                 
014480        PERFORM S02-WRITE-W571D2-DETAIL                                   
014481     WHEN WS-AUDIT-LINES <= WS-MAX-ROWS-2                                 
014482        PERFORM S03-WRITE-W571D3-DETAIL                                   
014483     WHEN WS-AUDIT-LINES <= WS-MAX-ROWS-3                                 
014484        PERFORM S04-WRITE-W571D4-DETAIL                                   
014485     WHEN WS-AUDIT-LINES <= WS-MAX-ROWS-4                                 
014486        PERFORM S05-WRITE-W571D5-DETAIL                                   
014487     WHEN WS-AUDIT-LINES <= WS-MAX-ROWS-5                                 
014488        PERFORM S06-WRITE-W571D6-DETAIL                                   
014489     END-EVALUATE                                                         
014490     .                                                                    
014491 CABA-WRITE-EXCL-PART-REPORT SECTION.                                     
014492     EVALUATE TRUE                                                        
014493     WHEN WS-EXCL-PARTS <= WS-MAX-ROWS-2                                  
014494     AND WS-EXCL-PARTS > WS-MAX-ROWS-1                                    
014495        IF WS-WRITE-D8-HDR                                                
014496           PERFORM S08-WRITE-W571D8-HEADER                                
014497           MOVE NOO  TO WS-WRITE-D8-HDR-SW                                
014498        END-IF                                                            
014499     WHEN WS-EXCL-PARTS <= WS-MAX-ROWS-3                                  
014500     AND WS-EXCL-PARTS > WS-MAX-ROWS-2                                    
014501        IF WS-WRITE-D9-HDR                                                
014502           PERFORM S09-WRITE-W571D9-HEADER                                
014503           MOVE NOO  TO WS-WRITE-D9-HDR-SW                                
014504        END-IF                                                            
014505     WHEN WS-EXCL-PARTS <= WS-MAX-ROWS-4                                  
014506     AND WS-EXCL-PARTS > WS-MAX-ROWS-3                                    
014507        IF WS-WRITE-D10-HDR                                               
014508           PERFORM S10-WRITE-W571D10-HEADER                               
014509           MOVE NOO  TO WS-WRITE-D10-HDR-SW                               
014510        END-IF                                                            
014511     WHEN WS-EXCL-PARTS <= WS-MAX-ROWS-5                                  
014512     AND WS-EXCL-PARTS > WS-MAX-ROWS-4                                    
014513        IF WS-WRITE-D11-HDR                                               
014514           PERFORM S11-WRITE-W571D11-HEADER                               
014515           MOVE NOO  TO WS-WRITE-D11-HDR-SW                               
014516        END-IF                                                            
014517     END-EVALUATE                                                         
014518                                                                          
014519     EVALUATE TRUE                                                        
014520     WHEN WS-EXCL-PARTS <= WS-MAX-ROWS-1                                  
014521        PERFORM S07-WRITE-W571D7-DETAIL                                   
014522     WHEN WS-EXCL-PARTS <= WS-MAX-ROWS-2                                  
014523        PERFORM S08-WRITE-W571D8-DETAIL                                   
014524     WHEN WS-EXCL-PARTS <= WS-MAX-ROWS-3                                  
014525        PERFORM S09-WRITE-W571D9-DETAIL                                   
014526     WHEN WS-EXCL-PARTS <= WS-MAX-ROWS-4                                  
014527        PERFORM S10-WRITE-W571D10-DETAIL                                  
014528     WHEN WS-EXCL-PARTS <= WS-MAX-ROWS-5                                  
014529        PERFORM S11-WRITE-W571D11-DETAIL                                  
014530     END-EVALUATE                                                         
014531     .                                                                    
014532 Z-FINIT SECTION.                                                         
014533     CLOSE W571D1                                                         
014534           W571D2                                                         
014535           W571D3                                                         
014536           W571D4                                                         
014537           W571D5                                                         
014538           W571D6                                                         
014539           W571D7                                                         
014540           W571D8                                                         
014541           W571D9                                                         
014542           W571D10                                                        
014543           W571D11                                                        
014544     SKIP2                                                                
014545     MOVE 'S' TO POSTSUM-OPKOD                                            
014550     CALL POSTSUM USING POSTSUM-PARM                                      
014600     .                                                                    
014601     EJECT                                                                
014602 S01-READ-W571D1  SECTION.                                                
014603     READ W571D1 INTO W57105-AREA                                         
014604     AT END                                                               
014606        SET END-OF-W571D1 TO TRUE                                         
014607                                                                          
014608     NOT AT END                                                           
014609        MOVE 'W571D1' TO POSTSUM-FDNAMN                                   
014610        MOVE 'W57108D1' TO POSTSUM-DDNAMN2                                
014612        MOVE SPACES        TO POSTSUM-TRANSTYP                            
014613        CALL POSTSUM USING POSTSUM-PARM                                   
014614     END-READ                                                             
014620     .                                                                    
014701     EJECT                                                                
014702 S02-WRITE-W571D2-HEADER SECTION.                                         
014703     WRITE W571D2-REC FROM W571D2-CONTROL-REC1                            
014704     WRITE W571D2-REC FROM W571D2-CONTROL-REC2                            
014705     WRITE W571D2-REC FROM W571D2-HEADER                                  
014706     .                                                                    
014707                                                                          
014708 S02-WRITE-W571D2-DETAIL SECTION.                                         
014709     WRITE W571D2-REC FROM W571D2-DETAIL                                  
014710     .                                                                    
014711                                                                          
014712 S03-WRITE-W571D3-HEADER SECTION.                                         
014713     WRITE W571D3-REC FROM W571D2-CONTROL-REC1                            
014714     WRITE W571D3-REC FROM W571D2-CONTROL-REC2                            
014715     WRITE W571D3-REC FROM W571D2-HEADER                                  
014716     .                                                                    
014717                                                                          
014718 S03-WRITE-W571D3-DETAIL SECTION.                                         
014719     WRITE W571D3-REC FROM W571D2-DETAIL                                  
014720     .                                                                    
014721                                                                          
014722 S04-WRITE-W571D4-HEADER SECTION.                                         
014723     WRITE W571D4-REC FROM W571D2-CONTROL-REC1                            
014724     WRITE W571D4-REC FROM W571D2-CONTROL-REC2                            
014725     WRITE W571D4-REC FROM W571D2-HEADER                                  
014726     .                                                                    
014727                                                                          
014728 S04-WRITE-W571D4-DETAIL SECTION.                                         
014729     WRITE W571D4-REC FROM W571D2-DETAIL                                  
014730     .                                                                    
014731                                                                          
014732 S05-WRITE-W571D5-HEADER SECTION.                                         
014733     WRITE W571D5-REC FROM W571D2-CONTROL-REC1                            
014734     WRITE W571D5-REC FROM W571D2-CONTROL-REC2                            
014735     WRITE W571D5-REC FROM W571D2-HEADER                                  
014736     .                                                                    
014737                                                                          
014738 S05-WRITE-W571D5-DETAIL SECTION.                                         
014739     WRITE W571D5-REC FROM W571D2-DETAIL                                  
014740     .                                                                    
014741                                                                          
014742 S06-WRITE-W571D6-HEADER SECTION.                                         
014743     WRITE W571D6-REC FROM W571D2-CONTROL-REC1                            
014744     WRITE W571D6-REC FROM W571D2-CONTROL-REC2                            
014745     WRITE W571D6-REC FROM W571D2-HEADER                                  
014746     .                                                                    
014747                                                                          
014748 S06-WRITE-W571D6-DETAIL SECTION.                                         
014749     WRITE W571D6-REC FROM W571D2-DETAIL                                  
014750     .                                                                    
014751                                                                          
014752 S07-WRITE-W571D7-HEADER SECTION.                                         
014753     WRITE W571D7-REC FROM W571D3-CONTROL-REC1                            
014754     WRITE W571D7-REC FROM W571D3-CONTROL-REC2                            
014755     WRITE W571D7-REC FROM W571D3-HEADER                                  
014760     .                                                                    
014770                                                                          
014780 S07-WRITE-W571D7-DETAIL SECTION.                                         
014790     WRITE W571D7-REC FROM W571D3-DETAIL                                  
014800     .                                                                    
014810 S08-WRITE-W571D8-HEADER SECTION.                                         
014820     WRITE W571D8-REC FROM W571D3-CONTROL-REC1                            
014830     WRITE W571D8-REC FROM W571D3-CONTROL-REC2                            
014840     WRITE W571D8-REC FROM W571D3-HEADER                                  
014850     .                                                                    
014860                                                                          
014870 S08-WRITE-W571D8-DETAIL SECTION.                                         
014880     WRITE W571D8-REC FROM W571D3-DETAIL                                  
014890     .                                                                    
014891 S09-WRITE-W571D9-HEADER SECTION.                                         
014892     WRITE W571D9-REC FROM W571D3-CONTROL-REC1                            
014893     WRITE W571D9-REC FROM W571D3-CONTROL-REC2                            
014894     WRITE W571D9-REC FROM W571D3-HEADER                                  
014895     .                                                                    
014896                                                                          
014897 S09-WRITE-W571D9-DETAIL SECTION.                                         
014898     WRITE W571D9-REC FROM W571D3-DETAIL                                  
014899     .                                                                    
014900 S10-WRITE-W571D10-HEADER SECTION.                                        
014901     WRITE W571D10-REC FROM W571D3-CONTROL-REC1                           
014902     WRITE W571D10-REC FROM W571D3-CONTROL-REC2                           
014903     WRITE W571D10-REC FROM W571D3-HEADER                                 
014904     .                                                                    
014905                                                                          
014906 S10-WRITE-W571D10-DETAIL SECTION.                                        
014907     WRITE W571D10-REC FROM W571D3-DETAIL                                 
014908     .                                                                    
014910 S11-WRITE-W571D11-HEADER SECTION.                                        
015000     WRITE W571D11-REC FROM W571D3-CONTROL-REC1                           
015100     WRITE W571D11-REC FROM W571D3-CONTROL-REC2                           
015200     WRITE W571D11-REC FROM W571D3-HEADER                                 
015300     .                                                                    
015400                                                                          
015500 S11-WRITE-W571D11-DETAIL SECTION.                                        
015600     WRITE W571D11-REC FROM W571D3-DETAIL                                 
015700     .                                                                    
015901 IMS-GU-WDK711 SECTION.                                                   
015902     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
015903            DELIMITED BY SIZE INTO SSA1                                   
015904     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
015905            DELIMITED BY SIZE INTO SSA2                                   
015906     MOVE '  GE' TO GOOD-STATUSCODES                                      
015907     CALL  CBLTDLI  USING GU WDK7-PCB DLI-IO-WDK701 SSA1 SSA2             
015908     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
015909     PERFORM IMS-STATUSCHECK                                              
015910     .                                                                    
015911 IMS-GU-WDB601 SECTION.                                                   
015920                                                                          
015930     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
015940          DELIMITED BY SIZE INTO SSA1                                     
015950     MOVE '  '                TO GOOD-STATUSCODES                         
015960     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
015970     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
015980     PERFORM IMS-STATUSCHECK                                              
015990     .                                                                    
015991 IMS-GU-WDGX5104 SECTION.                                                 
015992                                                                          
015993     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-5104-X ')'                    
015994          DELIMITED BY SIZE INTO SSA1                                     
015995     STRING 'WDGX5104(IDDC     =' W-IDDC-5104-X ')'                       
015996          DELIMITED BY SIZE INTO SSA2                                     
015997     MOVE '  GE' TO GOOD-STATUSCODES                                      
015998     CALL CBLTDLI USING GHU 5104-PCB DLI-IO-WDGX5104 SSA1 SSA2            
015999     MOVE 5104-STATUS-CODE    TO STATUS-WS                                
016000     PERFORM IMS-STATUSCHECK                                              
016001     .                                                                    
016002 IMS-GU-WDD311 SECTION.                                                   
016003                                                                          
016004     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
016005             DELIMITED BY SIZE INTO SSA1                                  
016006     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
016007             DELIMITED BY SIZE INTO SSA2                                  
016008     MOVE '  GE'                 TO GOOD-STATUSCODES                      
016009     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
016010     MOVE WDD3-STATUS-CODE       TO STATUS-WS                             
016011     PERFORM IMS-STATUSCHECK                                              
016012     .                                                                    
016020 IMS-STATUSCHECK SECTION.                                                 
016100                                                                          
016200     SET STATUS-IX TO 1                                                   
016300     SEARCH GOOD-STATUS                                                   
016400       AT END                                                             
016500         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
016600           DELIMITED BY SIZE INTO ERROR-TEXT                              
016700         DISPLAY ERROR-TEXT                                               
016800         CALL FELLOG                                                      
016900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
017000         CONTINUE                                                         
017100     END-SEARCH                                                           
017200     .                                                                    
