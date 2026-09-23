001100 ID DIVISION.                                                             
001200 PROGRAM-ID.     W5710700.                                                
001300 AUTHOR.         ARCHANA BHAT.                                            
001400 DATE-WRITTEN.   11/11/17.                                                
001500 DATE-COMPILED.                                                           
001600                                                                          
001700*    FUNCTION:                                                            
001800*        INVENTORY DOWNLOAD SELECTION - EXCLUDED PARTS                    
001900*                                                                         
002001*        THE PROGRAM READS     WDR2                                       
002010*        THE PROGRAM READS     WDD8                                       
002020*        THE PROGRAM READS     WDB6                                       
002030*        THE PROGRAM READS     WDD3                                       
002100*                                                                         
002200*    ABENDCODES:                                                          
002300*        U0016 -  . . . .                                                 
002400*        U1000 -  . . . .                                                 
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003301     SKIP2                                                                
003302*          --- DC VALUE                                                   
003303     SELECT W571D1                     ASSIGN TO W57107D1.                
003304*          --- LIST OF DC'S THAT HAVE OPTED FOR ACS                       
003305     SELECT W571D2                     ASSIGN TO W57107D2.                
003306*          --- D&P REPORT1                                                
003307     SELECT W571D3                     ASSIGN TO W57107D3.                
003308*          --- D&P REPORT2                                                
003309     SELECT W571D4                     ASSIGN TO W57107D4.                
003310*          --- D&P REPORT3                                                
003311     SELECT W571D5                     ASSIGN TO W57107D5.                
003312*          --- D&P REPORT4                                                
003313     SELECT W571D6                     ASSIGN TO W57107D6.                
003314*          --- D&P REPORT5                                                
003315     SELECT W571D7                     ASSIGN TO W57107D7.                
003320                                                                          
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003908 FD  W571D1                                                               
003909     RECORDING       F                                                    
003910     BLOCK CONTAINS  0.                                                   
003920 01  INPOST                  PIC X(80).                                   
003921*                                                                         
003930 FD  W571D2                                                               
003940     RECORDING       F                                                    
003950     BLOCK CONTAINS  0.                                                   
003960                                                                          
003970*01  -COPY W57101      -L.                                                
003971                                                                          
003980 FD  W571D3                                                               
003990     RECORDING       V                                                    
003991     BLOCK CONTAINS  0.                                                   
003992 01  W571D3-REC              PIC X(111).                                  
004000     EJECT                                                                
004010 FD  W571D4                                                               
004020     RECORDING       V                                                    
004030     BLOCK CONTAINS  0.                                                   
004040 01  W571D4-REC              PIC X(111).                                  
004050     EJECT                                                                
004060 FD  W571D5                                                               
004070     RECORDING       V                                                    
004080     BLOCK CONTAINS  0.                                                   
004090 01  W571D5-REC              PIC X(111).                                  
004091     EJECT                                                                
004092 FD  W571D6                                                               
004093     RECORDING       V                                                    
004094     BLOCK CONTAINS  0.                                                   
004095 01  W571D6-REC              PIC X(111).                                  
004096     EJECT                                                                
004097 FD  W571D7                                                               
004098     RECORDING       V                                                    
004099     BLOCK CONTAINS  0.                                                   
004100 01  W571D7-REC              PIC X(111).                                  
004101     EJECT                                                                
004110 WORKING-STORAGE SECTION.                                                 
004200                                                                          
004300 77  IDPGM                       PIC X(8)    VALUE 'W5710700'.            
004320 77  WS-SUVALINV-INC             PIC 9(11)V9(2) VALUE ZERO.               
004330 77  WS-SUVALINV-EXCL            PIC 9(11)V9(2) VALUE ZERO.               
004340 77  WS-KVINVART-INC             PIC 9(7) VALUE ZERO.                     
004350 77  WS-KVINVART-EXCL            PIC 9(7) VALUE ZERO.                     
004351 77  WS-TIINVDAT                 PIC S9(7) VALUE ZERO.                    
004360 77  WS-REC-CNT                  PIC 9(8) VALUE ZERO.                     
004361 77  WS-MAX-ROWS-1               PIC 9(5) VALUE 10000.                    
004362 77  WS-MAX-ROWS-2               PIC 9(5) VALUE 20000.                    
004363 77  WS-MAX-ROWS-3               PIC 9(5) VALUE 30000.                    
004364 77  WS-MAX-ROWS-4               PIC 9(5) VALUE 40000.                    
004365 77  WS-MAX-ROWS-5               PIC 9(5) VALUE 50000.                    
004366 77  WS-KVINVART                 PIC S9(7) VALUE ZERO.                    
004370 77  WS-SUVALINV                 PIC 9(11)V9(2) VALUE ZERO.               
004380 77  WS-REQTYDEV-INC             PIC 9(3)V9(2) VALUE ZERO.                
004390 77  WS-REQTYDEV-EXCL            PIC 9(3)V9(2) VALUE ZERO.                
004391 77  WS-REVALDEV-INC             PIC 9(3)V9(2) VALUE ZERO.                
004392 77  WS-REVALDEV-EXCL            PIC 9(3)V9(2) VALUE ZERO.                
004393 77  WS-NO-EXCL-PART             PIC X(41) VALUE                          
004394               'NO EXCLUDED PARTS'.                                       
004396 77  WS-CURRENT-DATE             PIC X(8)    VALUE SPACES.                
004397 77  WS-CURRENT-TIME             PIC 9(6)    VALUE ZERO.                  
004400 77  YES                         PIC X       VALUE 'J'.                   
004500 77  NOO                         PIC X       VALUE 'N'.                   
004600                                                                          
004700 77  WS-IDSKYLT-CHINESE          PIC X(3)    VALUE 'RCN'.                 
004701 77  WS-IDSKYLT-ENGLISH          PIC X(3)    VALUE 'GB '.                 
004702                                                                          
004703 77  W571D1-EOF-SW               PIC X       VALUE 'N'.                   
004704     88  END-OF-W571D1                       VALUE 'J'.                   
004705 77  W571D2-EOF-SW               PIC X       VALUE 'N'.                   
004710     88  END-OF-W571D2                       VALUE 'J'.                   
004720 77  WS-PASS-SW                  PIC X       VALUE 'N'.                   
004730     88  WS-PASS                             VALUE 'Y'.                   
004740 77  WS-EXCL-PART-SW             PIC X       VALUE 'N'.                   
004750     88  WS-EXCL-PART                        VALUE 'Y'.                   
004760 77  WS-DONE-SW                  PIC X       VALUE 'N'.                   
004770     88  WS-DONE                             VALUE 'Y'.                   
004780 77  WS-WRITE-D4-HDR-SW          PIC X       VALUE 'N'.                   
004790     88  WS-WRITE-D4-HDR                     VALUE 'J'.                   
004791 77  WS-WRITE-D5-HDR-SW          PIC X       VALUE 'N'.                   
004792     88  WS-WRITE-D5-HDR                     VALUE 'J'.                   
004793 77  WS-WRITE-D6-HDR-SW          PIC X       VALUE 'N'.                   
004794     88  WS-WRITE-D6-HDR                     VALUE 'J'.                   
004795 77  WS-WRITE-D7-HDR-SW          PIC X       VALUE 'N'.                   
004796     88  WS-WRITE-D7-HDR                     VALUE 'J'.                   
004800     EJECT                                                                
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600*                                                                         
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
005800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006020     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
006030     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
006040     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
006100     SKIP2                                                                
006800 01  ERROR-TEXT.                                                          
006900     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
007000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007301     EJECT                                                                
007302*01  -COPY WL01TIDZ                                                       
007303                                                                          
007304 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
007305*01  -COPY WTRAUTF8                                                       
007307                                                                          
007313 01  W571D1-AREA-START           PIC X(24)   VALUE                        
007314                                 'W571D1-AREA-START  '.                   
007315 01  W571D1-AREA.                                                         
007316     03 WS-IDDC                  PIC X(2)    VALUE SPACE.                 
007317     03 FILLER                   PIC X(78)   VALUE SPACE.                 
007318*                                                                         
007319 01  W571D2-AREA-START           PIC X(24)   VALUE                        
007320                                 'W571D2-AREA-START  '.                   
007322*01  AREA -COPY W57101     -PRE W57101-                                   
007323     EJECT                                                                
007324                                                                          
007325 01  W571D3-AREA-START           PIC X(24)   VALUE                        
007326                                 'W571D3-AREA-START  '.                   
007328 01  W571D3-CONTROL-REC1.                                                 
007329*                                                                         
007330     03  FILLER                  PIC X(111)  VALUE                        
007331                                 ' ¤DAPW57107-001'.                       
007332     EJECT                                                                
007333 01  W571D3-CONTROL-REC2.                                                 
007334*                                                                         
007335     03  FILLER                  PIC X(5)    VALUE ' ¤DAP'.               
007336                                                                          
007337     03  W571D3-CTL-IDDC         PIC X(2)    VALUE SPACE.                 
007338                                                                          
007339     03  FILLER                  PIC X(104)  VALUE SPACE.                 
007340     EJECT                                                                
007341 01  W571D3-HEADER.                                                       
007342*    03 -COPY W5710701                                                    
007343*                                                                         
007345 01  W571D3-DETAIL.                                                       
007346*    03 -COPY W5710702                                                    
007347                                                                          
007348 01  FILLER                      PIC X(16)  VALUE 'WDATAREA'.             
007349*01  -COPY WDATAREA                                                       
007350     EJECT                                                                
007360                                                                          
007500*    --- AREAS FOR IMS-SECTIONS                                           
007600*                                                                         
007700     EJECT                                                                
007800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008120 01  KEYS-TILL-DLI.                                                       
008130     03  W-IDDC-B6-X.                                                     
008140         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
008150     03  W-WDGXKEY-X.                                                     
008160          05 W-IDHTYP            PIC X(4)    VALUE '5103'.                
008170          05 W-LOWVALUE          PIC X(26)   VALUE LOW-VALUE.             
008180     03  W-IDDC-5104-X.                                                   
008190         05  W-IDDC-5104         PIC X(2)    VALUE SPACE.                 
008194     03  W-WDD801-IDARTNR-X.                                              
008195         05  W-WDD801-IDARTNR    PIC S9(9)   VALUE ZERO   COMP-3.         
008196     03  W-WDD811-IDDC-X.                                                 
008197         05  W-WDD811-IDDC       PIC X(2)    VALUE SPACE.                 
008199     03 W-IDARTNR-X.                                                      
008200        05  W-IDARTNR            PIC S9(9)   VALUE ZERO   COMP-3.         
008202     03 W-IDSKYLT-X.                                                      
008203        05  W-IDSKYLT            PIC X(3)    VALUE SPACE.                 
008210     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FOUND                       VALUE '  '.                  
008600     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008700     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008710     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
008800     SKIP2                                                                
008900 01  GOOD-STATUSCODES.                                                    
009000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009100     SKIP3                                                                
009200 01  SSA1                        PIC X(64).                               
009300 01  SSA2                        PIC X(64).                               
009400     EJECT                                                                
009500*    --- IMS FUNCTION CODES                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009900*    ---  DLI INPUT-OUTPUT AREA                                           
010001 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5104'.                    
010002 01  DLI-IO-WDGX5104.                                                     
010003*    03  -COPY WDGX5104                                                   
010008 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD811'.                      
010009 01  DLI-IO-WDD811.                                                       
010010*    03  -COPY WDD811   -PRE WDD8-                                        
010020 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
010030 01  DLI-IO-WDB601.                                                       
010040*    03  -COPY WDB601                                                     
010050 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD311'.                      
010060 01  DLI-IO-WDD311.                                                       
010070*    03  -COPY WDD311                                                     
010300     EJECT                                                                
010400 LINKAGE SECTION.                                                         
010500                                                                          
010640*01  -COPY W0008  -PRE WDB6-                                              
010650     05  FILLER                  PIC X.                                   
010660                                                                          
010670*01  -COPY W0008  -PRE 5104-                                              
010680     05  FILLER                  PIC X.                                   
010690                                                                          
010693*01  -COPY W0008  -PRE WDD8-                                              
010694     05  FILLER                  PIC X.                                   
010695                                                                          
010696*01  -COPY W0008  -PRE WDD3-                                              
010697     05  FILLER                  PIC X.                                   
010700     EJECT                                                                
010801 PROCEDURE DIVISION  USING 5104-PCB WDD8-PCB WDB6-PCB WDD3-PCB.           
010802                                                                          
010803 MAIN SECTION.                                                            
010810     ENTRY 'DLITCBL' USING 5104-PCB WDD8-PCB WDB6-PCB WDD3-PCB.           
010900                                                                          
011100                                                                          
011200     PERFORM A-INIT                                                       
011300                                                                          
011401     PERFORM S01-READ-W571D1                                              
011402     PERFORM B-GET-DATE-TIME-FROM-WDB6                                    
011410     PERFORM C-GET-DATA-FROM-WDR2                                         
012500                                                                          
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013301                                                                          
013302     OPEN INPUT  W571D1                                                   
013310                 W571D2                                                   
013320     OPEN OUTPUT W571D3                                                   
013321                 W571D4                                                   
013322                 W571D5                                                   
013323                 W571D6                                                   
013324                 W571D7                                                   
013325                                                                          
013330     INITIALIZE W571D3-DETAIL                                             
013500                                                                          
013710     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013720     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-CURRENT-DATE                 
013730     MOVE FUNCTION CURRENT-DATE (9:6)  TO WS-CURRENT-TIME                 
013900     .                                                                    
014000     EJECT                                                                
014010*-----------------------------------------------------------------        
014020*  GET THE LOCAL DATE AND TIME USING THE TIMEZONE VALUE PRESENT IN        
014030*  WDB6 DATABASE.                                                         
014040*-----------------------------------------------------------------        
014041*                                                                         
014050 B-GET-DATE-TIME-FROM-WDB6 SECTION.                                       
014060                                                                          
014061     MOVE WS-IDDC  TO W-IDDC-B6                                           
014070     PERFORM IMS-GU-WDB601                                                
014080     IF SEGMENT-FOUND                                                     
014090        MOVE '011'                 TO MSGI-KDCALL                         
014091        MOVE DCS-IDTIDZON          TO MSGI-IDTIDZON                       
014091        MOVE DCS-IDDC              TO MSGI-IDDC                           
014092        MOVE WS-CURRENT-DATE(3:6)  TO MSGI-TILOKDAT                       
014093        MOVE WS-CURRENT-TIME(1:4)  TO MSGI-TILOKTID                       
014094        CALL WL01TIDZ   USING      MSGI-WL01TIDZ                          
014095                                                                          
014096        STRING WS-CURRENT-DATE(1:2)                                       
014097               MSGI-TILOKDAT DELIMITED BY SIZE                            
014098                                    INTO HEAD-TIDATETIME(1:8)             
014100                                                                          
014110        STRING MSGI-TILOKTID WS-CURRENT-TIME(5:2)                         
014120               DELIMITED BY SIZE    INTO HEAD-TIDATETIME(9:6)             
014140     END-IF                                                               
014150     .                                                                    
014160*-----------------------------------------------------------------        
014170*  READ THE WDR2 DATABASE TO DETERMINE THE TYPE OF INVENTORY PROCE        
014180*  A - SIMULATION ; D- DOWNLOAD                                           
014190*-----------------------------------------------------------------        
014191 C-GET-DATA-FROM-WDR2 SECTION.                                            
014192     MOVE WS-IDDC  TO W-IDDC-5104                                         
014193     PERFORM IMS-GU-WDGX5104                                              
014194     IF SEGMENT-FOUND                                                     
014195        IF 5104-KDACS = 'D'                                               
014198           PERFORM CA-DOWNLOAD                                            
014205        END-IF                                                            
014206     END-IF                                                               
014207     .                                                                    
014208*-----------------------------------------------------------------        
014209* START THE DOWNLOAD PROCESS                                              
014210*-----------------------------------------------------------------        
014211 CA-DOWNLOAD SECTION.                                                     
014212     MOVE YES     TO WS-WRITE-D4-HDR-SW                                   
014213                     WS-WRITE-D5-HDR-SW                                   
014214                     WS-WRITE-D6-HDR-SW                                   
014215                     WS-WRITE-D7-HDR-SW                                   
014228**  FOR EVERY PART FROM W57101 FILE, DO A SELECTION. CREATE A LIST        
014229**  FOR EVERY EXCLUDED PART.                                              
014230     PERFORM S02-READ-W571D2                                              
014231     MOVE WS-IDDC TO W571D3-CTL-IDDC                                      
014232                     HEAD-IDDC                                            
014233     MOVE '1         '     TO HEAD-IDAFPRCD                               
014234     PERFORM S03-WRITE-W571D3-HEADER                                      
014235     PERFORM UNTIL END-OF-W571D2                                          
014236       IF WS-DONE                                                         
014237          CONTINUE                                                        
014238       ELSE                                                               
014239          PERFORM CAA-SELECT-57101-DATA                                   
014240*** FOR EACH EXCLUDED PART, CREATE A LIST OF EXCLUDED PARTS               
014241          IF WS-EXCL-PART                                                 
014242             PERFORM CAB-CREATE-EXCL-PART-LIST                            
014243          END-IF                                                          
014244       END-IF                                                             
014245       PERFORM S02-READ-W571D2                                            
014246     END-PERFORM                                                          
014247                                                                          
014249     IF WS-KVINVART-EXCL = 0                                              
014250        MOVE '2         '    TO LINE-IDAFPRCD                             
014251        MOVE SPACES          TO LINE-BEART                                
014252                                                                          
014253        MOVE ZEROS           TO LINE-IDARTNR                              
014254                                LINE-ADLAGOMR                             
014255                                LINE-ADGANG                               
014256                                LINE-ADPLATS                              
014257                                LINE-KVLS                                 
014258                                LINE-ADBUFFOMR                            
014259                                LINE-ADBUFFGANG                           
014260                                LINE-ADBUFFPL                             
014261        MOVE WS-NO-EXCL-PART TO LINE-MESSAGE                              
014262        PERFORM S03-WRITE-W571D3-DETAIL                                   
014263     END-IF                                                               
014264     .                                                                    
014284*-----------------------------------------------------------------        
014285* SELECT THE PARTS FROM THE W57101 FILE THAT ARE TO BE INCLUDED OR        
014286* EXCLUDED FROM THE INVENTORY DOWNLOAD SELECTION LIST.                    
014287*-----------------------------------------------------------------        
014288 CAA-SELECT-57101-DATA SECTION.                                           
014289     MOVE NOO          TO WS-PASS-SW                                      
014290                          WS-EXCL-PART-SW                                 
014291     IF W57101-ACS-KVLS = 0                                               
014292*** SHOULDN'T INCLUDE PARTS WITH NO STOCKBALANCE                          
014293       ADD +1             TO WS-KVINVART-EXCL                             
014295       SET WS-EXCL-PART   TO TRUE                                         
014296     ELSE                                                                 
014297       IF W57101-ACS-KVLS < 0                                             
014298       AND W57101-ACS-PRAVCOST NOT = 0                                    
014299*** CAN'T CALCULATE THE VALUE WHEN NEGATIVE BALANCE                       
014300*** BUT THEY SHOULD BE INVENTORED                                         
014301         ADD +1      TO WS-KVINVART-INC                                   
014302         SET WS-PASS TO TRUE                                              
014303       ELSE                                                               
014304         IF 5104-FLNOHAND = NOO                                           
014305** WHEN THE INVENTORY OF INACTIVE PARTS IS TO BE SKIPPED:                 
014306** IF THE LAST ACTIVITY DATE OF THE PARTS IS GREATER THAN THE             
014307** DATE OF THE LAST INVENTORY, INCLUDE THE PARTS IN THE LIST              
014308           MOVE 'AAVVD '      TO DAT-KDDATFORM                            
014309           MOVE W57101-ACS-TIINVDAT TO DAT-I-TIDATUM                      
014310           IF W57101-ACS-TIINVDAT > ZERO                                  
014311             CALL  WDATKONV  USING DAT-KDDATFORM                          
014312                                   DAT-I-TIDATUM                          
014313                                   DAT-O-TIDATUM                          
014314                                   DAT-KDSVAR                             
014315             IF DAT-KDSVAR-OK                                             
014316                 CONTINUE                                                 
014317             ELSE                                                         
014318                 CALL  FELLOG                                             
014319             END-IF                                                       
014320             MOVE DAT-TIAAMMDD TO WS-TIINVDAT                             
014321           ELSE                                                           
014322             MOVE ZERO         TO WS-TIINVDAT                             
014323           END-IF                                                         
014324           IF W57101-ACS-TIORDREG  > WS-TIINVDAT                          
014325           OR W57101-ACS-TIAVCOST  > WS-TIINVDAT                          
014326           OR W57101-ACS-TIRETUR-BEORD > WS-TIINVDAT                      
014327           OR W57101-ACS-TISKROT-BEORD > WS-TIINVDAT                      
014328** CALCULATE THE NUMBER OF PARTS THAT ARE TO BE INCLUDED                  
014329             IF W57101-ACS-PRAVCOST NOT = 0                               
014330             AND 5104-PRAVCOST < W57101-ACS-PRAVCOST                      
014331               COMPUTE WS-SUVALINV-INC = WS-SUVALINV-INC +                
014332                       (W57101-ACS-KVLS * W57101-ACS-PRAVCOST)            
014333               ADD +1  TO WS-KVINVART-INC                                 
014334               SET WS-PASS TO TRUE                                        
014335             ELSE                                                         
014336** CALCULATE THE VALUE OF THE PART                                        
014337** (THE STOCK BALANCE * THE AVERAGE COST OF THE PART)                     
014338               IF 5104-SUARTAVG <                                         
014339                  W57101-ACS-KVLS * W57101-ACS-PRAVCOST                   
014340                 COMPUTE WS-SUVALINV-INC = WS-SUVALINV-INC +              
014341                         (W57101-ACS-KVLS * W57101-ACS-PRAVCOST)          
014342                 ADD +1  TO WS-KVINVART-INC                               
014343                 SET WS-PASS TO TRUE                                      
014344               ELSE                                                       
014345                 COMPUTE WS-SUVALINV-EXCL = WS-SUVALINV-EXCL +            
014346                         (W57101-ACS-KVLS * W57101-ACS-PRAVCOST)          
014347                 ADD +1  TO WS-KVINVART-EXCL                              
014348                 SET WS-EXCL-PART TO TRUE                                 
014349               END-IF                                                     
014350             END-IF                                                       
014351           ELSE                                                           
014352             COMPUTE WS-SUVALINV-EXCL = WS-SUVALINV-EXCL +                
014353                     (W57101-ACS-KVLS * W57101-ACS-PRAVCOST)              
014354             ADD +1  TO WS-KVINVART-EXCL                                  
014355             SET WS-EXCL-PART TO TRUE                                     
014356           END-IF                                                         
014357         END-IF                                                           
014358                                                                          
014359** INVENTORY THE INACTIVE PARTS:                                          
014360         IF 5104-FLNOHAND = YES                                           
014361** CALCULATE THE NUMBER OF PARTS THAT ARE TO BE INCLUDED                  
014362           IF 5104-PRAVCOST < W57101-ACS-PRAVCOST                         
014363             COMPUTE WS-SUVALINV-INC = WS-SUVALINV-INC +                  
014364                     (W57101-ACS-KVLS * W57101-ACS-PRAVCOST)              
014365             ADD +1    TO WS-KVINVART-INC                                 
014366             SET WS-PASS TO TRUE                                          
014367           ELSE                                                           
014368**     CALCULATE THE VALUE OF THE PART                                    
014369             IF 5104-SUARTAVG <                                           
014370                W57101-ACS-KVLS * W57101-ACS-PRAVCOST                     
014371               COMPUTE WS-SUVALINV-INC = WS-SUVALINV-INC +                
014372                       (W57101-ACS-KVLS * W57101-ACS-PRAVCOST)            
014373               ADD +1    TO WS-KVINVART-INC                               
014374               SET WS-PASS TO TRUE                                        
014375             ELSE                                                         
014376               COMPUTE WS-SUVALINV-EXCL = WS-SUVALINV-EXCL +              
014377                       (W57101-ACS-KVLS * W57101-ACS-PRAVCOST)            
014378               ADD +1    TO WS-KVINVART-EXCL                              
014379               SET WS-EXCL-PART TO TRUE                                   
014380             END-IF                                                       
014381           END-IF                                                         
014382         END-IF                                                           
014383       END-IF                                                             
014384     END-IF                                                               
014385     .                                                                    
014386 CAB-CREATE-EXCL-PART-LIST SECTION.                                       
014387     MOVE W57101-ACS-IDARTNR TO W-WDD801-IDARTNR                          
014388     MOVE W57101-ACS-IDDC    TO W-WDD811-IDDC                             
014389     PERFORM IMS-GU-WDD811                                                
014390     IF SEGMENT-FOUND OR SEGMENT-MISSING                                  
014391        PERFORM CABA-CREATE-LINE-EXCL-PART                                
014392     END-IF                                                               
014393     .                                                                    
014394*-----------------------------------------------------------------        
014395* CREATE THE REPORT LINES OF THE EXCLUDED PART LIST.                      
014396* - FOR THE PARTS FOR WHICH A BUFFER LOCATION IS NOT FOUND,               
014397*   BLANK OUT THE BUFFER DETAILS AND CREATE A LIST.                       
014398* - FOR THE PARTS FOR WHICH A BUFFER LOCATION IS PRESENT, READ THE        
014399*   WDD8 DATABASE UNTIL ALL THE BUFFER LOCATION FOR THAT PART ARE         
014400*   LOCATED.CREATE A LIST.                                                
014401*-----------------------------------------------------------------        
014402 CABA-CREATE-LINE-EXCL-PART SECTION.                                      
014403     MOVE W57101-ACS-IDARTNR          TO LINE-IDARTNR                     
014404     MOVE W57101-ACS-BEART            TO LINE-BEART                       
014405     MOVE W57101-ACS-ADLAGOMR         TO LINE-ADLAGOMR                    
014406     MOVE W57101-ACS-ADGANG           TO LINE-ADGANG                      
014407     MOVE W57101-ACS-ADPLATS          TO LINE-ADPLATS                     
014408     MOVE W57101-ACS-KVLS             TO LINE-KVLS                        
014409     MOVE '2         '                TO LINE-IDAFPRCD                    
014410     MOVE SPACES                      TO LINE-MESSAGE                     
014411     ADD +1                           TO WS-REC-CNT                       
014412     IF SEGMENT-MISSING                                                   
014413        MOVE ZEROS                    TO LINE-ADBUFFOMR                   
014414                                         LINE-ADBUFFGANG                  
014415                                         LINE-ADBUFFPL                    
014416        PERFORM CABB-WRITE-DAP-REPORT                                     
014417     ELSE                                                                 
014418        MOVE WDD8-SALDO-ADBUFFOMR     TO LINE-ADBUFFOMR                   
014419        MOVE WDD8-SALDO-ADBUFFGANG    TO LINE-ADBUFFGANG                  
014420        MOVE WDD8-SALDO-ADBUFFPL      TO LINE-ADBUFFPL                    
014421        PERFORM CABB-WRITE-DAP-REPORT                                     
014422                                                                          
014423        MOVE SPACES                   TO STATUS-WS                        
014424        PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-NOMORE                   
014425          PERFORM IMS-GN-WDD811                                           
014426          IF SEGMENT-FOUND                                                
014427             ADD +1                     TO WS-REC-CNT                     
014428             MOVE WDD8-SALDO-ADBUFFOMR  TO LINE-ADBUFFOMR                 
014429             MOVE WDD8-SALDO-ADBUFFGANG TO LINE-ADBUFFGANG                
014430             MOVE WDD8-SALDO-ADBUFFPL   TO LINE-ADBUFFPL                  
014431             MOVE ZEROS                 TO LINE-IDARTNR                   
014432                                           LINE-ADLAGOMR                  
014433                                           LINE-ADGANG                    
014434                                           LINE-ADPLATS                   
014435                                           LINE-KVLS                      
014436             MOVE SPACES                TO LINE-BEART                     
014437                                           LINE-MESSAGE                   
014438             PERFORM CABB-WRITE-DAP-REPORT                                
014439          END-IF                                                          
014440        END-PERFORM                                                       
014441     END-IF                                                               
014442     .                                                                    
014443     EJECT                                                                
014444 CABB-WRITE-DAP-REPORT SECTION.                                           
014445                                                                          
014446*    WE NEED SOME ADJUSTMENT FOR CHINESE NAME                             
014447*    BEFORE SENDING TO D&P                                                
014448                                                                          
014449     MOVE LINE-IDARTNR          TO W-IDARTNR                              
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
014457     PERFORM IMS-GU-WDD311                                                
014458     IF SEGMENT-FOUND                                                     
014459        MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                    
014460     ELSE                                                                 
014461        MOVE SPACE              TO TRAUTF8-TECONV-FROM                    
014462        MOVE '278 '             TO TRAUTF8-KDCP                           
014463     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GU-WDD311                                               
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
014464     MOVE 25                    TO TRAUTF8-KVMAXTL                        
014465     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
014466     MOVE TRAUTF8-TECONV-TO     TO LINE-BEART                             
014468                                                                          
014469     EVALUATE TRUE                                                        
014470     WHEN WS-REC-CNT <= WS-MAX-ROWS-2                                     
014471     AND  WS-REC-CNT > WS-MAX-ROWS-1                                      
014472        IF WS-WRITE-D4-HDR                                                
014473           PERFORM S04-WRITE-W571D4-HEADER                                
014474           MOVE NOO  TO WS-WRITE-D4-HDR-SW                                
014475        END-IF                                                            
014476     WHEN WS-REC-CNT <= WS-MAX-ROWS-3                                     
014477     AND  WS-REC-CNT > WS-MAX-ROWS-2                                      
014478        IF WS-WRITE-D5-HDR                                                
014479           PERFORM S05-WRITE-W571D5-HEADER                                
014480           MOVE NOO  TO WS-WRITE-D5-HDR-SW                                
014481        END-IF                                                            
014482     WHEN WS-REC-CNT <= WS-MAX-ROWS-4                                     
014483     AND  WS-REC-CNT > WS-MAX-ROWS-3                                      
014484        IF WS-WRITE-D6-HDR                                                
014485           PERFORM S06-WRITE-W571D6-HEADER                                
014486           MOVE NOO  TO WS-WRITE-D6-HDR-SW                                
014487        END-IF                                                            
014488     WHEN WS-REC-CNT <= WS-MAX-ROWS-5                                     
014489     AND  WS-REC-CNT > WS-MAX-ROWS-4                                      
014490        IF WS-WRITE-D7-HDR                                                
014491           PERFORM S07-WRITE-W571D7-HEADER                                
014492           MOVE NOO  TO WS-WRITE-D7-HDR-SW                                
014493        END-IF                                                            
014494     END-EVALUATE                                                         
014495                                                                          
014496     EVALUATE TRUE                                                        
014497     WHEN WS-REC-CNT <= WS-MAX-ROWS-1                                     
014498        PERFORM S03-WRITE-W571D3-DETAIL                                   
014499     WHEN WS-REC-CNT <= WS-MAX-ROWS-2                                     
014500        PERFORM S04-WRITE-W571D4-DETAIL                                   
014501     WHEN WS-REC-CNT <= WS-MAX-ROWS-3                                     
014502        PERFORM S05-WRITE-W571D5-DETAIL                                   
014503     WHEN WS-REC-CNT <= WS-MAX-ROWS-4                                     
014504        PERFORM S06-WRITE-W571D6-DETAIL                                   
014505     WHEN WS-REC-CNT <= WS-MAX-ROWS-5                                     
014506        PERFORM S07-WRITE-W571D7-DETAIL                                   
014507     END-EVALUATE                                                         
014508     .                                                                    
014509 Z-FINIT SECTION.                                                         
014510     CLOSE W571D1                                                         
014511           W571D2                                                         
014512           W571D3                                                         
014513           W571D4                                                         
014514           W571D5                                                         
014515           W571D6                                                         
014516           W571D7                                                         
014517     SKIP2                                                                
014518     MOVE 'S' TO POSTSUM-OPKOD                                            
014519     CALL POSTSUM USING POSTSUM-PARM                                      
014520     .                                                                    
014530     EJECT                                                                
014624 S01-READ-W571D1  SECTION.                                                
014625     READ W571D1 INTO W571D1-AREA                                         
014626     AT END                                                               
014627        SET END-OF-W571D1 TO TRUE                                         
014628                                                                          
014629     NOT AT END                                                           
014630        MOVE 'W571D1' TO POSTSUM-FDNAMN                                   
014631        MOVE 'W57107D1' TO POSTSUM-DDNAMN2                                
014632        CALL POSTSUM USING POSTSUM-PARM                                   
014633     END-READ                                                             
014634     .                                                                    
014635 S02-READ-W571D2  SECTION.                                                
014636     SKIP2                                                                
014637     READ W571D2 INTO W57101-AREA                                         
014638     AT END                                                               
014639        SET END-OF-W571D2 TO TRUE                                         
014640                                                                          
014641     NOT AT END                                                           
014642        IF W57101-ACS-IDDC NOT = WS-IDDC                                  
014643           SET WS-DONE TO TRUE                                            
014644        ELSE                                                              
014645           MOVE NOO TO WS-DONE-SW                                         
014646        END-IF                                                            
014647        MOVE 'W571D2'     TO POSTSUM-FDNAMN                               
014648        MOVE 'W57107D2'   TO POSTSUM-DDNAMN2                              
014649        MOVE 'IN  '       TO POSTSUM-TRANSTYP                             
014650        CALL POSTSUM USING POSTSUM-PARM                                   
014651     END-READ                                                             
014652     .                                                                    
014653 S03-WRITE-W571D3-HEADER SECTION.                                         
014654     WRITE W571D3-REC FROM W571D3-CONTROL-REC1                            
014655     WRITE W571D3-REC FROM W571D3-CONTROL-REC2                            
014656     WRITE W571D3-REC FROM W571D3-HEADER                                  
014657     .                                                                    
014659 S03-WRITE-W571D3-DETAIL SECTION.                                         
014660     WRITE W571D3-REC FROM W571D3-DETAIL                                  
014661     .                                                                    
014662 S04-WRITE-W571D4-HEADER SECTION.                                         
014663     WRITE W571D4-REC FROM W571D3-CONTROL-REC1                            
014664     WRITE W571D4-REC FROM W571D3-CONTROL-REC2                            
014665     WRITE W571D4-REC FROM W571D3-HEADER                                  
014666     .                                                                    
014667 S04-WRITE-W571D4-DETAIL SECTION.                                         
014668     WRITE W571D4-REC FROM W571D3-DETAIL                                  
014669     .                                                                    
014670 S05-WRITE-W571D5-HEADER SECTION.                                         
014671     WRITE W571D5-REC FROM W571D3-CONTROL-REC1                            
014672     WRITE W571D5-REC FROM W571D3-CONTROL-REC2                            
014673     WRITE W571D5-REC FROM W571D3-HEADER                                  
014674     .                                                                    
014675 S05-WRITE-W571D5-DETAIL SECTION.                                         
014676     WRITE W571D5-REC FROM W571D3-DETAIL                                  
014677     .                                                                    
014678 S06-WRITE-W571D6-HEADER SECTION.                                         
014679     WRITE W571D6-REC FROM W571D3-CONTROL-REC1                            
014680     WRITE W571D6-REC FROM W571D3-CONTROL-REC2                            
014681     WRITE W571D6-REC FROM W571D3-HEADER                                  
014682     .                                                                    
014686 S06-WRITE-W571D6-DETAIL SECTION.                                         
014687     WRITE W571D6-REC FROM W571D3-DETAIL                                  
014688     .                                                                    
014689 S07-WRITE-W571D7-HEADER SECTION.                                         
014690     WRITE W571D7-REC FROM W571D3-CONTROL-REC1                            
014691     WRITE W571D7-REC FROM W571D3-CONTROL-REC2                            
014692     WRITE W571D7-REC FROM W571D3-HEADER                                  
014693     .                                                                    
014694 S07-WRITE-W571D7-DETAIL SECTION.                                         
014695     WRITE W571D7-REC FROM W571D3-DETAIL                                  
014696     .                                                                    
014700     EJECT                                                                
015500* --- IMS SECTIONS  ---                                                   
015600                                                                          
015700 IMS-GU-WDB601 SECTION.                                                   
015800                                                                          
015810     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
015820          DELIMITED BY SIZE INTO SSA1                                     
015830     MOVE '  GE' TO GOOD-STATUSCODES                                      
015840     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
015850     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
015860     PERFORM IMS-STATUSCHECK                                              
015870     .                                                                    
015880     EJECT                                                                
015890 IMS-GU-WDGX5104 SECTION.                                                 
015891                                                                          
015892     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
015893          DELIMITED BY SIZE INTO SSA1                                     
015894     STRING 'WDGX5104(IDDC     =' W-IDDC-5104-X ')'                       
015895          DELIMITED BY SIZE INTO SSA2                                     
015896     MOVE '  GE' TO GOOD-STATUSCODES                                      
015897     CALL CBLTDLI USING GU 5104-PCB DLI-IO-WDGX5104 SSA1 SSA2             
015898     MOVE 5104-STATUS-CODE TO STATUS-WS                                   
015899     PERFORM IMS-STATUSCHECK                                              
015900     .                                                                    
015901 IMS-GU-WDD811 SECTION.                                                   
015902     STRING 'WDD801  (IDARTNR  =' W-WDD801-IDARTNR-X ')'                  
015903          DELIMITED BY SIZE INTO SSA1                                     
015904     STRING 'WDD811  (IDDC     =' W-WDD811-IDDC-X ')'                     
015905          DELIMITED BY SIZE INTO SSA2                                     
015906     MOVE '  GE'           TO GOOD-STATUSCODES                            
015907     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD811 SSA1 SSA2               
015908     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
015909     PERFORM IMS-STATUSCHECK                                              
015910     .                                                                    
015911     EJECT                                                                
015912 IMS-GN-WDD811 SECTION.                                                   
015913     STRING 'WDD801  (IDARTNR  =' W-WDD801-IDARTNR-X ')'                  
015914          DELIMITED BY SIZE INTO SSA1                                     
015915     STRING 'WDD811  (IDDC     =' W-WDD811-IDDC-X ')'                     
015916          DELIMITED BY SIZE INTO SSA2                                     
015917     MOVE '  GEGB'         TO GOOD-STATUSCODES                            
015918     CALL CBLTDLI USING GN WDD8-PCB DLI-IO-WDD811 SSA1 SSA2               
015919     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
015920     PERFORM IMS-STATUSCHECK                                              
015921     .                                                                    
015922     EJECT                                                                
015923 IMS-GU-WDD311 SECTION.                                                   
015925                                                                          
015926     STRING 'WDD301  (WDD3BSEQ =' W-IDARTNR-X ')'                         
015927             DELIMITED BY SIZE INTO SSA1                                  
015928     STRING 'WDD311  (IDSKYLT  =' W-IDSKYLT-X ')'                         
015929             DELIMITED BY SIZE INTO SSA2                                  
015930     MOVE '  GE'                 TO GOOD-STATUSCODES                      
015931     CALL CBLTDLI USING GU WDD3-PCB DLI-IO-WDD311 SSA1 SSA2               
015932     MOVE WDD3-STATUS-CODE       TO STATUS-WS                             
015933     PERFORM IMS-STATUSCHECK                                              
015934     .                                                                    
015935     EJECT                                                                
015940 IMS-STATUSCHECK SECTION.                                                 
016000                                                                          
016100     SET STATUS-IX TO 1                                                   
016200     SEARCH GOOD-STATUS                                                   
016300       AT END                                                             
016400         STRING ' INVALID STATUS CODE FROM IMS:' STATUS-WS                
016500           DELIMITED BY SIZE INTO ERROR-TEXT                              
016600         DISPLAY ERROR-TEXT                                               
016700         CALL FELLOG                                                      
016800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
016900         CONTINUE                                                         
017000     END-SEARCH                                                           
017100     .                                                                    
