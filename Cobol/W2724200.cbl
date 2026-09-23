000101 ID DIVISION.                                                             
000201     SKIP2                                                                
000301 PROGRAM-ID.     W2724200.                                                
000401*AUTHOR.         JOHAN NIHLBLAD.                                          
000501*DATE-WRITTEN.   DEC 2016.                                                
000601                                                                          
000701*    REMARKS                                                              
000801*                                                                         
000901*    FUNKTION:                                                            
001001*        BEVAKA PASSIVERING AV ARTIKEL                                    
001101*                                                                         
001201*        PROGRAMMET UPPDATERAR WDK6                                       
001401*                                                                         
001501*        PROGRAM READS         WDK6                                       
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003203*                                                                         
003303     SELECT W27242                     ASSIGN TO W27242D1.                
003403*          --- UT-FIL MED ARTIKLAR FÖR PASSIVERING                        
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP3                                                                
003800 FILE SECTION.                                                            
003903*                                                                         
004003 FD  W27242                                                               
004103     RECORDING       F                                                    
004203     BLOCK CONTAINS  0.                                                   
004303                                                                          
004403*01  POST -COPY W27242  -PRE UT-    -L.                                   
004503                                                                          
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800     SKIP2                                                                
004900*    -COPY WY2000W1                                                       
005000     SKIP3                                                                
005101*    -COPY WWDC99                                                         
005201     SKIP3                                                                
005301 77  IDPGM                       PIC X(8)    VALUE 'W2724200'.            
005400 77  YES                         PIC X       VALUE 'Y'.                   
005500 77  NOO                         PIC X       VALUE 'N'.                   
005601 77  JA                          PIC X       VALUE 'J'.                   
005701 77  NEJ                         PIC X       VALUE 'N'.                   
005801     88  WDK712-FINNS                        VALUE 'J'.                   
005900     SKIP2                                                                
006000 01  FELTEXT.                                                             
006100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006300     EJECT                                                                
006401 01  ARBETSFAELT.                                                         
006501     03  FILLER                  PIC X(10)   VALUE 'ARBETSFÄLT'.          
006601*                                                                         
006701     03  IX1                     PIC 9(2)    VALUE ZERO.                  
006801     03  IX2                     PIC 9(2)    VALUE ZERO.                  
006901     03  MAX-IX1                 PIC 9(2)    VALUE 13.                    
007001     03  MAX-IX2                 PIC 9(2)    VALUE 10.                    
007101     03  WS-DC-FLAG              PIC X(1)    VALUE 'N'.                   
007201     03  WS-PASS-FLAG            PIC X(1)    VALUE 'N'.                   
007301     03  WS-ADLAGOMR-FLAG        PIC X(1)    VALUE 'N'.                   
007401     03  WS-ALL-ZERO             PIC X(1)    VALUE 'N'.                   
007501     03  WS-TIINLINL-VALID       PIC X(1)    VALUE 'N'.                   
007601     03  WS-WEEKS                PIC 9(5)    VALUE ZERO.                  
007701     03  TODAYS-DATE             PIC 9(6)    VALUE ZERO.                  
007801     03  WS-DATE                 PIC 9(6)    VALUE ZERO.                  
007901     03  WS-DATE-YYWWD           PIC 9(5)    VALUE ZERO.                  
008001     03  WS-KDPRODSL-601         PIC 9(2)    VALUE ZERO.                  
008101     03  WS-TIINLINL             PIC 9(6)    VALUE ZERO.                  
008201     03  WS-PRIS                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
008300*                                                                         
009001 01  WS-TABLE-ARRAY.                                                      
012001     03    WS-PASS-WDB613 OCCURS 13.                                      
012101         10 WS-KVVECKOR-LSALES PIC S9(3) COMP-3.                          
012201         10 WS-KVVECKOR-PUBV     PIC S9(3) COMP-3.                        
012301         10 WS-PRARTSTD          PIC 9(7).                                
012401         10 WS-VLARTNTO          PIC 9(8).                                
012501         10 WS-KDPRODSL          PIC S9(3) COMP-3.                        
012601         10 WS-ADLAGOMR OCCURS 10 TIMES PIC S9(3) COMP-3.                 
012701                                                                          
012801 01  WS-VLARTNTO-JFR             PIC 9(8)V9(1) VALUE ZERO.                
012901 01  FILLER REDEFINES WS-VLARTNTO-JFR.                                    
013001     03  WS-VLARTNTO-HELTAL      PIC 9(8).                                
013101     03  WS-VLARTNTO-DECIMAL     PIC 9(1).                                
013201                                                                          
013301 01  DYNAMISKA-SUBPROGRAM.                                                
013400*                                                                         
013500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
013600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
013700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
013800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
013900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
014000     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
014101     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
014200     EJECT                                                                
014300*    --- PARAMETRAR TILL ABEND                                            
014400                                                                          
014500 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
014600 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
014700     SKIP2                                                                
014803*                                                                         
014903 01  UT-AREA-START              PIC X(24)   VALUE                         
015003                                 'UT-AREA-START  '.                       
015103     SKIP2                                                                
015203                                                                          
015303*01  AREA -COPY W27242     -PRE UT-                                       
015400     EJECT                                                                
015503*                                                                         
015600*    --- PARAMETRAR TILL POSTSUM                                          
015700*                                                                         
015800*01  -COPY W0005   -PRE  POSTSUM-                                         
015900     EJECT                                                                
016000*    --- PARAMETRAR TILL WDATKONV                                         
016100*                                                                         
016200*01  -COPY WDATAREA                                                       
016300*                                                                         
016401*    --- PARAMETERS FOR WZ20DAYS SUBPROGRAM                               
016501*01  -COPY WZ20DAYS                                                       
016601*                                                                         
016700*    --- PARAMETRAR TILL DATKORT                                          
016801 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27242'.              
016900     SKIP2                                                                
017000 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
017100     SKIP2                                                                
017200*01  -COPY WDATKORT                                                       
017300     EJECT                                                                
017400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017500     SKIP3                                                                
017600 01  NYCKLAR-TILL-DLI.                                                    
017700     03  W-IDARTNR-X.                                                     
017800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017900     03  W-KDSEGKEY-X.                                                    
018000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
018100     03  W-IDDC-X.                                                        
018201         05  W-IDDC              PIC X(2)    VALUE '11'.                  
018301     03  W-IDLAND-X.                                                      
018401         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
018501     03  W-IDDC-MIN-X.                                                    
018601         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
018701     03  W-DAINLEV-MIN-X.                                                 
018801         05  W-DAINLEV-MIN       PIC 9(16)   VALUE ZERO.                  
018901     03  W-DAINLEV-MAX-X.                                                 
019001         05  W-DAINLEV-MAX       PIC 9(16)   VALUE                        
019101                                             9999999999999999.            
019200*                                                                         
019300*    --- STATUS-KOD FRÅN IMS                                              
019400 01  STATUS-WS                   PIC XX.                                  
019500     88  SEGMENT-FINNS                       VALUE '  '.                  
019600     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019700     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
019900     88  IMS-EJ-OK                           VALUE 'XD'.                  
020000     SKIP2                                                                
020100 01  GODK-STATUSKODER.                                                    
020200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020300     SKIP3                                                                
020400 01  SSA1                        PIC X(128).                              
020500 01  SSA2                        PIC X(64).                               
020600     EJECT                                                                
020700*    --- IMS FUNKTIONSKODER                                               
020800*01  -COPY W0003                                                          
020900     EJECT                                                                
021000*    ---  DLI INPUT-OUTPUT AREA                                           
021100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
021200 01  DLI-IO-WDK601.                                                       
021300*    03  -COPY WDK601    -PRE WDK6-                                       
021400*                                                                         
021500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
021600 01  DLI-IO-WDK611.                                                       
021700*    03  -COPY WDK611                                                     
021800*                                                                         
021900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK629'.                      
022000 01  DLI-IO-WDK629.                                                       
022100*    03  -COPY WDK629                                                     
022200*                                                                         
022401 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL601'.                      
022501 01  DLI-IO-WDL601.                                                       
022601*    03  -COPY WDL601                                                     
022701*                                                                         
022801 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL611'.                      
022901 01  DLI-IO-WDL611.                                                       
023001*    03  -COPY WDL611                                                     
023101*                                                                         
023601 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
023700 01  DLI-IO-WDB601.                                                       
023800*    03  -COPY WDB601                                                     
023900*                                                                         
024000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB613'.                      
024100 01  DLI-IO-WDB613.                                                       
024200*    03  -COPY WDB613                                                     
024300*                                                                         
024400 LINKAGE SECTION.                                                         
024500                                                                          
024600*01  -COPY W0009  -PRE MSG-                                               
024700     EJECT                                                                
024800*01  -COPY W0008  -PRE WDK6-                                              
024900     05  FILLER                  PIC X.                                   
025000     EJECT                                                                
025700*01  -COPY W0008  -PRE WDL6-                                              
025800     05  FILLER                  PIC X.                                   
025900     EJECT                                                                
026300*01  -COPY W0008  -PRE WDB6-                                              
026400     05  FILLER                  PIC X.                                   
026500     EJECT                                                                
026601 PROCEDURE DIVISION  USING MSG-PCB  WDK6-PCB                              
026701                           WDL6-PCB WDB6-PCB.                             
026801     ENTRY 'DLITCBL' USING MSG-PCB  WDK6-PCB                              
026901                           WDL6-PCB WDB6-PCB.                             
027000                                                                          
027100     PERFORM A-INIT                                                       
027200                                                                          
027501     PERFORM IMS-GN-WDK629                                                
027601                                                                          
027701     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
029401       IF CREF-KDREFSTA = 'A'                                             
029601         PERFORM IMS-GNP-WDK611                                           
029701         PERFORM IMS-GNP-WDK601                                           
029702         MOVE WDK6-ART-IDARTNR TO W-IDARTNR                               
029703                                                                          
030201         IF SEGMENT-FINNS                                                 
030301           IF CLAG-KDERS > 20                                             
030401****FÖR ATT PASSIVERA KDERS > 20 OBEROENDE AV ATT DET GÅTT ETT            
030501****HALVÅR SEDAN AKTIVERING                                               
030601             PERFORM E-SKRIV-UPDFIL                                       
030801           END-IF                                                         
030901         END-IF                                                           
031201                                                                          
031301         MOVE 'YYMMDD'           TO DAYS-KDDATFMT1                        
031401         MOVE CREF-TIREFSTA      TO WS-DATE                               
031501         MOVE WS-DATE            TO DAYS-TIDATE1                          
031601         PERFORM S01-GET-WEEKS-WZ20DAYS                                   
031701         IF DAYS-KVDAYS > 180                                             
031901           PERFORM C-CHECK-PASSIVATION-RULES                              
032101         END-IF                                                           
032201       END-IF                                                             
032600*                                                                         
032801       PERFORM IMS-GN-WDK629                                              
032900     END-PERFORM                                                          
033000                                                                          
034003     PERFORM Z-FINIT                                                      
034103                                                                          
034200     MOVE ZERO TO RETURN-CODE                                             
034300     GOBACK                                                               
034400     .                                                                    
034500 A-INIT SECTION.                                                          
034603                                                                          
034703     OPEN OUTPUT W27242                                                   
034803                                                                          
034903     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
035003                                                                          
035101     INITIALIZE WS-TABLE-ARRAY                                            
035201                                                                          
035301     PERFORM AA-LOAD-WDB6-TABLE                                           
035401                                                                          
035501     ACCEPT TODAYS-DATE FROM DATE                                         
035600     .                                                                    
035701 AA-LOAD-WDB6-TABLE SECTION.                                              
036001     PERFORM IMS-GU-WDB601-KY                                             
038001*                                                                         
039001     PERFORM IMS-GNP-WDB613                                               
040001     MOVE +1 TO IX1                                                       
041001     PERFORM UNTIL SEGMENT-SAKNAS                                         
042001       MOVE PASS-KVVECKOR-LSALES TO WS-KVVECKOR-LSALES (IX1)              
043001       MOVE PASS-KVVECKOR-PUBV     TO WS-KVVECKOR-PUBV   (IX1)            
044001       MOVE PASS-PRARTSTD          TO WS-PRARTSTD        (IX1)            
045001       MOVE PASS-VLARTNTO          TO WS-VLARTNTO        (IX1)            
046001       MOVE PASS-KDPRODSL          TO WS-KDPRODSL        (IX1)            
047001       MOVE +1 TO IX2                                                     
048001       PERFORM UNTIL IX2 > MAX-IX2                                        
048101         MOVE PASS-ADLAGOMR (IX2) TO WS-ADLAGOMR (IX1 IX2)                
048201         ADD +1 TO IX2                                                    
048301       END-PERFORM                                                        
048401       PERFORM IMS-GNP-WDB613                                             
048501       ADD +1 TO IX1                                                      
048601     END-PERFORM                                                          
048701*                                                                         
049101     .                                                                    
050301 C-CHECK-PASSIVATION-RULES SECTION.                                       
050401     MOVE +1  TO IX1                                                      
050501     MOVE NOO TO WS-PASS-FLAG                                             
050601*                                                                         
050701     PERFORM UNTIL IX1 > MAX-IX1 OR WS-PASS-FLAG = 'Y'                    
050801                OR WS-KVVECKOR-LSALES (IX1) = 0                           
050901       MOVE NOO TO WS-PASS-FLAG                                           
051001       PERFORM CA-CHECK-LSALES-RULES                                      
051101                                                                          
051201       IF WS-PASS-FLAG = 'Y'                                              
051301         PERFORM CB-CHECK-PUBV-RULES                                      
051401       END-IF                                                             
051501                                                                          
051601       IF WS-PASS-FLAG = 'Y'                                              
051701         PERFORM CC-CHECK-PRICE-RULES                                     
051801       END-IF                                                             
051901                                                                          
052001       IF WS-PASS-FLAG = 'Y'                                              
052101         PERFORM CD-CHECK-VOLUME-RULES                                    
052201       END-IF                                                             
052301                                                                          
052401       IF WS-PASS-FLAG = 'Y'                                              
052501         PERFORM CE-CHECK-PRODSL-RULES                                    
052601       END-IF                                                             
052701                                                                          
052801       IF WS-PASS-FLAG = 'Y'                                              
052901         PERFORM CF-CHECK-ADLAGOMR-RULES                                  
053001       END-IF                                                             
053101       ADD +1 TO IX1                                                      
053201     END-PERFORM                                                          
053301                                                                          
053401     IF WS-PASS-FLAG = 'Y'                                                
053501       PERFORM E-SKRIV-UPDFIL                                             
053601     END-IF                                                               
053701     .                                                                    
053801 CA-CHECK-LSALES-RULES SECTION.                                           
054001     IF CREF-TIREFEFT > 0                                                 
054101       MOVE 'YYMMDD'       TO DAYS-KDDATFMT1                              
054201       MOVE CREF-TIREFEFT  TO WS-DATE                                     
054301       MOVE WS-DATE        TO DAYS-TIDATE1                                
054401       PERFORM S01-GET-WEEKS-WZ20DAYS                                     
054501       IF WS-WEEKS >= WS-KVVECKOR-LSALES (IX1)                            
054601         MOVE YES TO WS-PASS-FLAG                                         
054701       ELSE                                                               
054801         MOVE NOO TO WS-PASS-FLAG                                         
054901       END-IF                                                             
055001     ELSE                                                                 
055101       PERFORM IMS-GU-WDL601                                              
055201       IF SEGMENT-FINNS                                                   
055301         PERFORM IMS-GNP-WDL611                                           
055401         MOVE ZERO TO WS-TIINLINL                                         
055500         PERFORM UNTIL SEGMENT-SAKNAS                                     
055601           IF INL-IDDC = W-IDDC                                           
055701             MOVE INL-TIINLINL TO TMP1-YYMMDD                             
055801             MOVE WS-TIINLINL  TO TMP2-YYMMDD                             
055901             PERFORM WY2000P1                                             
056001             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
056101               MOVE INL-TIINLINL TO WS-TIINLINL                           
056201             END-IF                                                       
056301           END-IF                                                         
056401           PERFORM IMS-GNP-WDL611                                         
056501         END-PERFORM                                                      
056601         IF WS-TIINLINL  > 0                                              
056701           MOVE 'YYMMDD'       TO DAYS-KDDATFMT1                          
056801           MOVE WS-TIINLINL    TO WS-DATE                                 
056901           MOVE WS-DATE        TO DAYS-TIDATE1                            
057001           PERFORM S01-GET-WEEKS-WZ20DAYS                                 
057101           IF WS-WEEKS >= WS-KVVECKOR-LSALES (IX1)                        
057201             MOVE YES TO WS-PASS-FLAG                                     
057301           ELSE                                                           
057401             MOVE NOO TO WS-PASS-FLAG                                     
057501           END-IF                                                         
057601         END-IF                                                           
057701       END-IF                                                             
057801     END-IF                                                               
057901     .                                                                    
058001 CB-CHECK-PUBV-RULES SECTION.                                             
059002                                                                          
059702     MOVE 'YYWWD'            TO DAYS-KDDATFMT1                            
059802     MOVE WDK6-ART-TIFINLV   TO WS-DATE-YYWWD                             
059902     MOVE WS-DATE-YYWWD      TO DAYS-TIDATE1                              
060002     PERFORM S01-GET-WEEKS-WZ20DAYS                                       
060102     IF (WS-WEEKS >= WS-KVVECKOR-PUBV (IX1) OR                            
060202       WS-KVVECKOR-PUBV (IX1) = 0 )                                       
060302       MOVE YES TO WS-PASS-FLAG                                           
060402     ELSE                                                                 
060502       MOVE NOO TO WS-PASS-FLAG                                           
060602     END-IF                                                               
060901     .                                                                    
061001     EJECT                                                                
061102                                                                          
061201 CC-CHECK-PRICE-RULES SECTION.                                            
061302                                                                          
061802     MOVE CLAG-PRARTSTD TO WS-PRIS                                        
062302     IF (WS-PRIS >= WS-PRARTSTD(IX1) OR                                   
062402         WS-PRARTSTD(IX1) = 0)                                            
062502       MOVE YES TO WS-PASS-FLAG                                           
062602     ELSE                                                                 
062702       MOVE NOO TO WS-PASS-FLAG                                           
062802     END-IF                                                               
063001     .                                                                    
063101 CD-CHECK-VOLUME-RULES SECTION.                                           
063202                                                                          
063902     MOVE CLAG-VLARTNTO       TO WS-VLARTNTO-JFR                          
064101                                                                          
064201     IF (WS-VLARTNTO-HELTAL >= WS-VLARTNTO(IX1) OR                        
064301         WS-VLARTNTO(IX1) = 0)                                            
064401       MOVE YES TO WS-PASS-FLAG                                           
064501     ELSE                                                                 
064601       MOVE NOO TO WS-PASS-FLAG                                           
064701     END-IF                                                               
064801     .                                                                    
064902                                                                          
065001 CE-CHECK-PRODSL-RULES SECTION.                                           
065102                                                                          
065202     MOVE WDK6-ART-KDPRODSL TO WS-KDPRODSL-601                            
065302                                                                          
065401     IF (WS-KDPRODSL-601 = WS-KDPRODSL(IX1) OR                            
065501         WS-KDPRODSL(IX1) = 0)                                            
065601       MOVE YES TO WS-PASS-FLAG                                           
065701     ELSE                                                                 
065801       MOVE NOO TO WS-PASS-FLAG                                           
065901     END-IF                                                               
066000     .                                                                    
066101 CF-CHECK-ADLAGOMR-RULES SECTION.                                         
066202                                                                          
066301     MOVE NOO TO WS-ADLAGOMR-FLAG                                         
066401     MOVE +1 TO IX2                                                       
066501     PERFORM UNTIL IX2 > MAX-IX2 OR WS-ADLAGOMR-FLAG = 'Y'                
066602       IF CLAG-ADLAGOMR = WS-ADLAGOMR(IX1 IX2)                            
066701         AND WS-ADLAGOMR(IX1 IX2) > 0                                     
066801         MOVE YES TO WS-PASS-FLAG                                         
066901         MOVE YES TO WS-ADLAGOMR-FLAG                                     
067001       ELSE                                                               
067101         MOVE NOO TO WS-PASS-FLAG                                         
067201       END-IF                                                             
067301       ADD +1 TO IX2                                                      
067401     END-PERFORM                                                          
067501*                                                                         
067601     IF WS-ADLAGOMR-FLAG = 'N'                                            
067701       MOVE +1 TO IX2                                                     
067801       MOVE YES TO WS-ALL-ZERO                                            
067901       PERFORM UNTIL IX2 > MAX-IX2 OR WS-ALL-ZERO = 'N'                   
068001         IF WS-ADLAGOMR(IX1 IX2) = 0                                      
068101           MOVE YES TO WS-PASS-FLAG                                       
068201         ELSE                                                             
068301           MOVE NOO TO WS-PASS-FLAG                                       
068401           MOVE NOO TO WS-ALL-ZERO                                        
068501         END-IF                                                           
068601         ADD +1 TO IX2                                                    
068701       END-PERFORM                                                        
068801     END-IF                                                               
068900     .                                                                    
069002                                                                          
069101 E-SKRIV-UPDFIL  SECTION.                                                 
069200*    PASSIVERA EN ARTIKEL PÅ ARTS-BASEN (WDK7)                            
069303                                                                          
069403     MOVE WDK6-ART-IDARTNR  TO UT-IDARTNR                                 
069503     MOVE TODAYS-DATE       TO UT-TIREFSTA                                
069603                                                                          
069703     PERFORM S02-SKRIV-W27242                                             
069803*    MOVE 'P'          TO UT-KDREFSTA                                     
069903*    MOVE NOO          TO UT-FLREFBEO                                     
070003*                         UT-FLREFNYO                                     
070103*    MOVE +0           TO UT-KVPB-SEP                                     
070203*                         UT-KVREFOVL                                     
070303*                         UT-KVREFPKT                                     
070403*                         UT-TIPBDAT                                      
070503*                         UT-TIREFPAF                                     
070603*                         UT-TIREFPKT                                     
070703*                         UT-TIREFSTO                                     
070801                                                                          
070900     .                                                                    
071003     EJECT                                                                
071103                                                                          
071203 Z-FINIT SECTION.                                                         
071303                                                                          
071403     CLOSE W27242                                                         
071803                                                                          
071903     MOVE 'S' TO POSTSUM-OPKOD                                            
072003     CALL POSTSUM USING POSTSUM-PARM                                      
072103     .                                                                    
072203     EJECT                                                                
072303                                                                          
072400 S01-GET-WEEKS-WZ20DAYS SECTION.                                          
072500     MOVE ZERO           TO DAYS-KVDAYS                                   
072600     MOVE 'YYMMDD'       TO DAYS-KDDATFMT2                                
072700     MOVE TODAYS-DATE    TO DAYS-TIDATE2                                  
072800                                                                          
072900     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
073000*                                                                         
073100     IF DAYS-KDRC = +0                                                    
073200       COMPUTE WS-WEEKS = DAYS-KVDAYS / 7                                 
073300     ELSE                                                                 
073400       MOVE 'ERROR FROM WZ20DAYS MODULE' TO FELTEXT-STR                   
073500       DISPLAY FELTEXT                                                    
073600       DISPLAY DAYS-KDRC                                                  
073700       CALL FELLOG                                                        
073800     END-IF                                                               
073900     .                                                                    
074003                                                                          
074103 S02-SKRIV-W27242 SECTION.                                                
074203                                                                          
074303     WRITE UT-POST FROM UT-AREA                                           
074403                                                                          
074503     MOVE 'W27242'   TO POSTSUM-FDNAMN                                    
074603     MOVE 'W27242D1' TO POSTSUM-DDNAMN2                                   
074703     CALL POSTSUM USING POSTSUM-PARM                                      
074803     .                                                                    
074903     SKIP3                                                                
075003                                                                          
075100 S99-ABEND SECTION.                                                       
075200     MOVE 'S' TO POSTSUM-OPKOD                                            
075300     CALL POSTSUM USING POSTSUM-PARM                                      
075400     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
075500     .                                                                    
075600* --- IMS SEKTIONER ---                                                   
075701 IMS-GN-WDK629 SECTION.                                                   
075801     MOVE 'WDK629 '        TO SSA1                                        
075901     MOVE '  GEGB'           TO GODK-STATUSKODER                          
076001     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-WDK629 SSA1                    
076101     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
076201     PERFORM IMS-STATUSKONTROLL                                           
076301     SKIP3                                                                
076401     .                                                                    
076501     EJECT                                                                
076601 IMS-GNP-WDK611 SECTION.                                                  
076701     MOVE 'WDK611 '        TO SSA1                                        
076801     MOVE '  '             TO GODK-STATUSKODER                            
076901     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
077001     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
077101     PERFORM IMS-STATUSKONTROLL                                           
077201     SKIP3                                                                
077301     .                                                                    
077401     EJECT                                                                
077501 IMS-GNP-WDK601 SECTION.                                                  
077601     MOVE 'WDK601 '        TO SSA1                                        
077701     MOVE '  '             TO GODK-STATUSKODER                            
077801     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK601 SSA1                   
077901     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
078001     PERFORM IMS-STATUSKONTROLL                                           
078101     SKIP3                                                                
078201     .                                                                    
078301     EJECT                                                                
079001******************************************************************        
082201 IMS-GU-WDL601 SECTION.                                                   
082301     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
082401          DELIMITED BY SIZE INTO SSA1                                     
082501     MOVE '  GE' TO GODK-STATUSKODER                                      
082601     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
082701     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
082801     PERFORM IMS-STATUSKONTROLL                                           
082901     .                                                                    
083001 IMS-GNP-WDL611 SECTION.                                                  
083101     STRING 'WDL611  (DAINLEV >=' W-DAINLEV-MIN-X                         
083201                    '&DAINLEV <=' W-DAINLEV-MAX-X                         
083301                    '&IDDC     =' W-IDDC-X  ')'                           
083401          DELIMITED BY SIZE INTO SSA1                                     
083501     MOVE '  GE' TO GODK-STATUSKODER                                      
083601     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
083701     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
083801     PERFORM IMS-STATUSKONTROLL                                           
083901     .                                                                    
087300 IMS-GU-WDB601-KY SECTION.                                                
087401     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
087501          DELIMITED BY SIZE INTO SSA1                                     
087601     MOVE '  ' TO GODK-STATUSKODER                                        
087701     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
087801     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
087901     PERFORM IMS-STATUSKONTROLL                                           
088001     .                                                                    
088800 IMS-GNP-WDB613 SECTION.                                                  
088901     MOVE 'WDB613  '       TO SSA1                                        
089001     MOVE '  GE' TO GODK-STATUSKODER                                      
089101     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB613 SSA1                   
089201     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
089301     PERFORM IMS-STATUSKONTROLL                                           
089401     .                                                                    
089501     EJECT                                                                
092100 IMS-STATUSKONTROLL SECTION.                                              
092200     SET STATUS-IX TO 1                                                   
092300     SEARCH GODK-STATUS                                                   
092400       AT END                                                             
092500         MOVE 'FELAKTIG IMS-RETURKOD' TO FELTEXT-STR                      
092600         DISPLAY FELTEXT                                                  
092700         CALL FELLOG                                                      
092800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
092900         CONTINUE                                                         
093000     END-SEARCH                                                           
093100     .                                                                    
094000*    -COPY WY2000P1                                                       
