000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2331400.                                                
000300 AUTHOR.         KIHLBERG STEFAN.                                         
000400 DATE-WRITTEN.   13/02/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        PROGRAM TO SELECT AND SUM INCOMMING DELIVERIES TO NDC'S          
000900*        IN CHINA ANDE CREATE VOLUME FILE TO SI+                          
001000*                                                                         
001200*        THE PROGRAM READS     WDK6                                       
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
002600*          --- INCOMMING DELIVERIES TO CHINA NDC                          
002700     SELECT W61216                     ASSIGN TO W23314D1.                
002800     SKIP2                                                                
002900*          --- VOLUME FILE TO SIPLUS DC71 CHN07                           
003000     SELECT W23314                     ASSIGN TO W23314D2.                
003100     SKIP2                                                                
003200*          --- VOLUME FILE TO SIPLUS DC72 CHN04                           
003300     SELECT W23315                     ASSIGN TO W23314D3.                
003400     SKIP2                                                                
003500*          --- VOLUME FILE TO SIPLUS  DC73 AEFZT                          
003600     SELECT W23316                     ASSIGN TO W23314D4.                
003610     SKIP2                                                                
003620*          --- VOLUME FILE TO SIPLUS  UC41 CUGZQ                          
003630     SELECT W23317                     ASSIGN TO W23314D5.                
003640     SKIP2                                                                
003650*          --- VOLUME FILE TO SIPLUS  UC43 CUGZR                          
003660     SELECT W23318                     ASSIGN TO W23314D6.                
003670     SKIP2                                                                
003680*          --- VOLUME FILE TO SIPLUS  UC44 AEHRD                          
003690     SELECT W23322                     ASSIGN TO W23314D7.                
003691     SKIP2                                                                
003692*          --- VOLUME FILE TO SIPLUS  UC45 AEKS1                          
003693     SELECT W23323                     ASSIGN TO W23314D8.                
003694     SKIP2                                                                
003695*          --- VOLUME FILE TO SIPLUS  DC46 AELGT                          
003696     SELECT W23325                     ASSIGN TO W23314D9.                
003697                                                                          
003698*          --- VOLUME FILE TO SIPLUS  DC47 AE9X5                          
003699     SELECT W23326                     ASSIGN TO W23314DA.                
003700     EJECT                                                                
003800 DATA DIVISION.                                                           
003900     SKIP2                                                                
004000 FILE SECTION.                                                            
004100     SKIP3                                                                
004200 FD  W61216                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  -COPY W61216      -L.                                                
004700     SKIP3                                                                
004800 FD  W23314                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005200*01  RECORD -COPY A310G073 -PRE  CHN07-  -L.                              
005300     SKIP3                                                                
005400 FD  W23315                                                               
005500     RECORDING       F                                                    
005600     BLOCK CONTAINS  0.                                                   
005700                                                                          
005800*01  RECORD -COPY A310G073 -PRE  CHN04-  -L.                              
005900     SKIP3                                                                
006000 FD  W23316                                                               
006100     RECORDING       F                                                    
006200     BLOCK CONTAINS  0.                                                   
006300                                                                          
006400*01  RECORD -COPY A310G073 -PRE  AEFZT-  -L.                              
006500                                                                          
006510 FD  W23317                                                               
006520     RECORDING       F                                                    
006530     BLOCK CONTAINS  0.                                                   
006540                                                                          
006550*01  RECORD -COPY A310G073 -PRE  NDC41-  -L.                              
006560     SKIP3                                                                
006570 FD  W23318                                                               
006580     RECORDING       F                                                    
006590     BLOCK CONTAINS  0.                                                   
006591                                                                          
006592*01  RECORD -COPY A310G073 -PRE  NDC43-  -L.                              
006593     SKIP3                                                                
006594 FD  W23322                                                               
006595     RECORDING       F                                                    
006596     BLOCK CONTAINS  0.                                                   
006597                                                                          
006598*01  RECORD -COPY A310G073 -PRE  NDC44-  -L.                              
006599     SKIP3                                                                
006600 FD  W23323                                                               
006601     RECORDING       F                                                    
006602     BLOCK CONTAINS  0.                                                   
006603                                                                          
006604*01  RECORD -COPY A310G073 -PRE  NDC45-  -L.                              
006605     SKIP3                                                                
006606 FD  W23325                                                               
006607     RECORDING       F                                                    
006608     BLOCK CONTAINS  0.                                                   
006609                                                                          
006610*01  RECORD -COPY A310G073 -PRE  NDC46-  -L.                              
006611                                                                          
006612 FD  W23326                                                               
006613     RECORDING       F                                                    
006614     BLOCK CONTAINS  0.                                                   
006615                                                                          
006616*01  RECORD -COPY A310G073 -PRE  NDC47-  -L.                              
006617     EJECT                                                                
006620 WORKING-STORAGE SECTION.                                                 
006700                                                                          
006800 77  IDPGM                       PIC X(8)    VALUE 'W2331400'.            
006900 77  YES                         PIC X       VALUE 'J'.                   
007000 77  NOO                         PIC X       VALUE 'N'.                   
007100 77  CURRENT-SECTION             PIC X(20)   VALUE SPACE.                 
007200                                                                          
007300 77  W61216-EOF-SW               PIC X       VALUE 'N'.                   
007400     88  END-OF-W61216                       VALUE 'J'.                   
007500     EJECT                                                                
007600                                                                          
007700 01 WORK-FIELDS.                                                          
007800     03 SPAR-IDDC                PIC X(2)    VALUE SPACE.                 
007900     03 SPAR-IDLEVNR             PIC X(5)    VALUE SPACE.                 
008000     03 SPAR-IDARTNR             PIC 9(9)    VALUE ZERO.                  
008100                                                                          
008200     03 WS-ANT-INLEV             PIC S9(9)   VALUE ZERO COMP-3.           
008300     03 WS-PT                    PIC S9(3)   VALUE +73  COMP-3.           
008400     03 WS-IDLEVNR-DC            PIC  X(5)   VALUE SPACE.                 
008500     03 WS-KDPRODSL              PIC S9(3)   VALUE ZERO COMP-3.           
008510     03 WS-TIAAPP                PIC S9(5)   VALUE ZERO COMP-3.           
008600                                                                          
008700 01  TODAYS-DATE                 PIC  9(6)   VALUE ZERO.                  
008800 01  FILLER REDEFINES TODAYS-DATE.                                        
008900     03  TODAYS-DATE-YEAR        PIC 9(2).                                
009000     03  TODAYS-DATE-MONTH       PIC 9(2).                                
009100     03  TODAYS-DATE-DAY         PIC 9(2).                                
009200     EJECT                                                                
009300 01  GENERAL-SUBPROGRAMS.                                                 
009400*                                                                         
009500     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010000     SKIP2                                                                
010100*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
010200                                                                          
010300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010600     SKIP2                                                                
010700 01  ERROR-TEXT.                                                          
010800     03  FILLER                  PIC X(10)    VALUE 'ERROR-TEXT'.         
010900     03  ERROR-TEXT-STR          PIC X(70)   VALUE SPACE.                 
011000     EJECT                                                                
011200*                                                                         
011300 01  PROGRAM-NAME                PIC X(6)    VALUE 'W23314'.              
011400     SKIP2                                                                
011500 01  DATECARD-ID                 PIC X(6)    VALUE 'WDATUM'.              
011600     SKIP2                                                                
011900*    --- PARAMETRAR TILL POSTSUM                                          
012000*                                                                         
012100*01  -COPY W0005   -PRE  POSTSUM-                                         
012200     EJECT                                                                
012300 01  IN-AREA-START               PIC X(24)   VALUE                        
012400                                 'IN-AREA-START  '.                       
012500     SKIP2                                                                
012600                                                                          
012700*01  AREA -COPY W61216     -PRE IN-                                       
012800     EJECT                                                                
012900 01  CHN07-AREA-START            PIC X(24)   VALUE                        
013000                                 'CHN07-AREA-START  '.                    
013100     SKIP2                                                                
013200                                                                          
013300*01  AREA -COPY A310G073     -PRE SIPLUS-                                 
013400     EJECT                                                                
013500 01  SIPLUS-AREA-START            PIC X(24)   VALUE                       
013600                                 'SIPLUS-AREA-START  '.                   
013700     SKIP2                                                                
013800                                                                          
013900*    --- AREAS FOR IMS-SECTIONS                                           
014000*                                                                         
014100     EJECT                                                                
014200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014300     SKIP3                                                                
014400 01  KEYS-FOR-DLI.                                                        
014500     03  W-IDDC-X.                                                        
014600         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
014700     03  W-IDARTNR-X.                                                     
014800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014900     SKIP2                                                                
015000*    --- STATUS-KOD FRÅN IMS                                              
015100 01  STATUS-WS                   PIC XX.                                  
015200     88  SEGMENT-FOUND                       VALUE '  '.                  
015300     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
015400     88  SEGMENT-MISSING                     VALUE 'GE'.                  
015500     SKIP2                                                                
015600 01  GOOD-STATUSCODES.                                                    
015700     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015800     SKIP3                                                                
015900 01  SSA1                        PIC X(64).                               
016000 01  SSA2                        PIC X(64).                               
016100     EJECT                                                                
016200*    --- IMS FUNCTION CODES                                               
016300*01  -COPY W0003                                                          
016400     EJECT                                                                
016500*    ---  DLI INPUT-OUTPUT AREA                                           
016900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
017000 01  DLI-IO-WDK601.                                                       
017100*    03  -COPY WDK601                                                     
017200     EJECT                                                                
017300 LINKAGE SECTION.                                                         
017800                                                                          
017900*01  -COPY W0008  -PRE WDK6-                                              
018000     05  FILLER                  PIC X.                                   
018100     EJECT                                                                
018200 PROCEDURE DIVISION  USING WDK6-PCB.                                      
018300 MAIN SECTION.                                                            
018400     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
018500                                                                          
018600                                                                          
018700     PERFORM A-INIT                                                       
018800                                                                          
018900     PERFORM S01-READ-W61216                                              
019000     PERFORM UNTIL END-OF-W61216                                          
019100                                                                          
019200       MOVE IN-IDDC      TO SPAR-IDDC                                     
019300       MOVE IN-IDLEVNR   TO SPAR-IDLEVNR                                  
019400       MOVE IN-IDARTNR   TO SPAR-IDARTNR                                  
019410       MOVE IN-TIAAPP    TO WS-TIAAPP                                     
019420       MOVE IN-IDLEVNR-DC TO WS-IDLEVNR-DC                                
019430       MOVE ZERO         TO WS-ANT-INLEV                                  
019500                                                                          
019600       PERFORM UNTIL END-OF-W61216 OR                                     
019700       IN-IDDC    NOT = SPAR-IDDC  OR                                     
019800       IN-IDLEVNR NOT = SPAR-IDLEVNR  OR                                  
019900       IN-IDARTNR NOT = SPAR-IDARTNR                                      
020000                                                                          
020100         PERFORM B-SUMMERA                                                
020200                                                                          
020300         PERFORM S01-READ-W61216                                          
020400                                                                          
020500                                                                          
020600       END-PERFORM                                                        
020700       PERFORM C-KOLLA-KDPRODSL                                           
020800       PERFORM D-SKAPA-SIPLUS                                             
020900       EVALUATE SPAR-IDDC                                                 
021000         WHEN '71'                                                        
021100           PERFORM S11-WRITE-W23314                                       
021200         WHEN '72'                                                        
021300           PERFORM S12-WRITE-W23315                                       
021400         WHEN '73'                                                        
021500           PERFORM S13-WRITE-W23316                                       
021510         WHEN '41'                                                        
021520           PERFORM S14-WRITE-W23317                                       
021530         WHEN '43'                                                        
021540           PERFORM S15-WRITE-W23318                                       
021550         WHEN '44'                                                        
021560           PERFORM S16-WRITE-W23322                                       
021570         WHEN '45'                                                        
021580           PERFORM S17-WRITE-W23323                                       
021590         WHEN '46'                                                        
021591           PERFORM S18-WRITE-W23325                                       
021592         WHEN '47'                                                        
021593           PERFORM S19-WRITE-W23326                                       
021600       END-EVALUATE                                                       
021700     END-PERFORM                                                          
021800                                                                          
021900     PERFORM Z-FINIT                                                      
022000                                                                          
022100     MOVE ZERO TO RETURN-CODE                                             
022200     GOBACK                                                               
022300     .                                                                    
022400     EJECT                                                                
022500 A-INIT SECTION.                                                          
022600                                                                          
022700     OPEN INPUT  W61216                                                   
022800                                                                          
022900     OPEN OUTPUT W23314                                                   
023000                 W23315                                                   
023100                 W23316                                                   
023110                 W23317                                                   
023120                 W23318                                                   
023130                 W23322                                                   
023140                 W23323                                                   
023150                 W23325                                                   
023160                 W23326                                                   
023200                                                                          
023900     .                                                                    
024000     EJECT                                                                
024100 B-SUMMERA SECTION.                                                       
024200     MOVE 'B-SUMMERA' TO CURRENT-SECTION.                                 
024300                                                                          
024400     IF IN-IDPTYP = 'R31' AND IN-TIINLMOT >= IN-TIAAMMDD AND              
024500        IN-TIINLINL = 0                                                   
024600        COMPUTE WS-ANT-INLEV = WS-ANT-INLEV + IN-KVAVIS                   
024700     END-IF                                                               
024800                                                                          
024900     IF IN-IDPTYP = 'R32' AND IN-TIINLMOT >= IN-TIAAMMDD AND              
025000        IN-TIINLINL >= IN-TIAAMMDD                                        
025100        COMPUTE WS-ANT-INLEV = WS-ANT-INLEV + IN-KVANTMOT                 
025200     END-IF                                                               
025300                                                                          
025400     IF IN-IDPTYP = 'R32' AND IN-TIINLMOT < IN-TIAAMMDD AND               
025500        IN-TIINLINL >= IN-TIAAMMDD                                        
025600        COMPUTE WS-ANT-INLEV = WS-ANT-INLEV + IN-KVANTMOT                 
025700        COMPUTE WS-ANT-INLEV = WS-ANT-INLEV - IN-KVAVIS                   
025800     END-IF                                                               
025900     .                                                                    
026000     EJECT                                                                
026100                                                                          
026200 C-KOLLA-KDPRODSL SECTION.                                                
026300                                                                          
026400     MOVE 'C-KOLLA-KDPRODSL' TO CURRENT-SECTION.                          
026500                                                                          
026600     MOVE SPAR-IDARTNR TO W-IDARTNR                                       
026700     PERFORM IMS-GET-WDK601                                               
026800     IF SEGMENT-FOUND                                                     
026900       MOVE ART-KDPRODSL TO WS-KDPRODSL                                   
027000     END-IF                                                               
027100     .                                                                    
027200     EJECT                                                                
027300                                                                          
027400 D-SKAPA-SIPLUS SECTION.                                                  
027500                                                                          
027600     MOVE 'D-SKAPA-SIPLUSS' TO CURRENT-SECTION.                           
027700                                                                          
027800     MOVE WS-PT             TO SIPLUS-PT                                  
027900     MOVE SPAR-IDARTNR      TO SIPLUS-ARTNR                               
028000     MOVE WS-IDLEVNR-DC     TO SIPLUS-GSDB-FORB                           
028100     MOVE SPAR-IDLEVNR      TO SIPLUS-GSDB-LEV                            
028200                                                                          
028300     EVALUATE WS-KDPRODSL                                                 
028400       WHEN  16                                                           
028500                  MOVE '5' TO SIPLUS-KDSEKTOR                             
028600       WHEN  17                                                           
028610                  MOVE '5' TO SIPLUS-KDSEKTOR                             
028700       WHEN  15                                                           
028800                  MOVE '6' TO SIPLUS-KDSEKTOR                             
028900       WHEN OTHER                                                         
029000                  MOVE '3' TO SIPLUS-KDSEKTOR                             
029100     END-EVALUATE                                                         
029200                                                                          
029300     MOVE WS-ANT-INLEV    TO SIPLUS-ANT-INLEV                             
029310     MOVE WS-TIAAPP       TO SIPLUS-PER                                   
029400     MOVE ZERO            TO SIPLUS-INDEX-LEV                             
029500                             SIPLUS-INDEX-KVAL                            
029600                             SIPLUS-FORBNR                                
029700                             SIPLUS-LEVNUM                                
029710     .                                                                    
029800     EJECT                                                                
029900                                                                          
030000 Z-FINIT SECTION.                                                         
030100     CLOSE W61216                                                         
030200           W23314                                                         
030300           W23315                                                         
030400           W23316                                                         
030410           W23317                                                         
030420           W23318                                                         
030430           W23322                                                         
030440           W23323                                                         
030450           W23325                                                         
030460           W23326                                                         
030500     .                                                                    
030600     EJECT                                                                
030700 S01-READ-W61216  SECTION.                                                
030800     READ W61216 INTO IN-AREA                                             
030900     AT END                                                               
031000        MOVE HIGH-VALUE TO IN-AREA                                        
031100        SET END-OF-W61216 TO TRUE                                         
031200                                                                          
031300     NOT AT END                                                           
031400        MOVE 'W61216' TO POSTSUM-FDNAMN                                   
031500        MOVE 'W23314D1' TO POSTSUM-DDNAMN2                                
031700        MOVE IN-IDPTYP TO POSTSUM-TRANSTYP                                
031800        CALL POSTSUM USING POSTSUM-PARM                                   
031900     END-READ                                                             
032000     .                                                                    
032100     EJECT                                                                
032200 S11-WRITE-W23314 SECTION.                                                
032300                                                                          
032400     WRITE CHN07-RECORD FROM SIPLUS-AREA                                  
032500                                                                          
032600     MOVE 'CHN07' TO POSTSUM-TRANSTYP                                     
032700     MOVE 'W23314' TO POSTSUM-FDNAMN                                      
032800     MOVE 'W23314D2' TO POSTSUM-DDNAMN2                                   
032900     CALL POSTSUM USING POSTSUM-PARM                                      
033000     .                                                                    
033100     EJECT                                                                
033200 S12-WRITE-W23315 SECTION.                                                
033300                                                                          
033400     WRITE CHN04-RECORD FROM SIPLUS-AREA                                  
033500                                                                          
033600     MOVE 'CHN04' TO POSTSUM-TRANSTYP                                     
033700     MOVE 'W23315' TO POSTSUM-FDNAMN                                      
033800     MOVE 'W23314D3' TO POSTSUM-DDNAMN2                                   
033900     CALL POSTSUM USING POSTSUM-PARM                                      
034000     .                                                                    
034100     EJECT                                                                
034200 S13-WRITE-W23316 SECTION.                                                
034300                                                                          
034400     WRITE AEFZT-RECORD FROM SIPLUS-AREA                                  
034500                                                                          
034600     MOVE 'AEFZT' TO POSTSUM-TRANSTYP                                     
034700     MOVE 'W23316' TO POSTSUM-FDNAMN                                      
034800     MOVE 'W23314D4' TO POSTSUM-DDNAMN2                                   
034900     CALL POSTSUM USING POSTSUM-PARM                                      
035000     .                                                                    
035100     EJECT                                                                
035110 S14-WRITE-W23317 SECTION.                                                
035120                                                                          
035130     WRITE NDC41-RECORD FROM SIPLUS-AREA                                  
035140                                                                          
035150     MOVE 'NDC41'    TO POSTSUM-TRANSTYP                                  
035160     MOVE 'W23317'   TO POSTSUM-FDNAMN                                    
035170     MOVE 'W23314D5' TO POSTSUM-DDNAMN2                                   
035180     CALL POSTSUM USING POSTSUM-PARM                                      
035190     .                                                                    
035191     EJECT                                                                
035192 S15-WRITE-W23318 SECTION.                                                
035194                                                                          
035195     WRITE NDC43-RECORD FROM SIPLUS-AREA                                  
035196                                                                          
035197     MOVE 'NDC43'    TO POSTSUM-TRANSTYP                                  
035198     MOVE 'W23318'   TO POSTSUM-FDNAMN                                    
035199     MOVE 'W23314D6' TO POSTSUM-DDNAMN2                                   
035200     CALL POSTSUM USING POSTSUM-PARM                                      
035201     .                                                                    
035202     EJECT                                                                
035203 S16-WRITE-W23322 SECTION.                                                
035205                                                                          
035206     WRITE NDC44-RECORD FROM SIPLUS-AREA                                  
035207                                                                          
035208     MOVE 'NDC44'    TO POSTSUM-TRANSTYP                                  
035209     MOVE 'W23322'   TO POSTSUM-FDNAMN                                    
035210     MOVE 'W23314D7' TO POSTSUM-DDNAMN2                                   
035211     CALL POSTSUM USING POSTSUM-PARM                                      
035212     .                                                                    
035213     EJECT                                                                
035214 S17-WRITE-W23323 SECTION.                                                
035216                                                                          
035217     WRITE NDC45-RECORD FROM SIPLUS-AREA                                  
035218                                                                          
035219     MOVE 'NDC45'    TO POSTSUM-TRANSTYP                                  
035220     MOVE 'W23323'   TO POSTSUM-FDNAMN                                    
035221     MOVE 'W23314D8' TO POSTSUM-DDNAMN2                                   
035222     CALL POSTSUM USING POSTSUM-PARM                                      
035223     .                                                                    
035224     EJECT                                                                
035225 S18-WRITE-W23325 SECTION.                                                
035227                                                                          
035228     WRITE NDC46-RECORD FROM SIPLUS-AREA                                  
035229                                                                          
035230     MOVE 'NDC46'    TO POSTSUM-TRANSTYP                                  
035231     MOVE 'W23325'   TO POSTSUM-FDNAMN                                    
035232     MOVE 'W23314D9' TO POSTSUM-DDNAMN2                                   
035233     CALL POSTSUM USING POSTSUM-PARM                                      
035234     .                                                                    
035235     EJECT                                                                
035236 S19-WRITE-W23326 SECTION.                                                
035237                                                                          
035238     WRITE NDC47-RECORD FROM SIPLUS-AREA                                  
035239                                                                          
035240     MOVE 'NDC47'    TO POSTSUM-TRANSTYP                                  
035250     MOVE 'W23326'   TO POSTSUM-FDNAMN                                    
035260     MOVE 'W23314DA' TO POSTSUM-DDNAMN2                                   
035270     CALL POSTSUM USING POSTSUM-PARM                                      
035280     .                                                                    
035290     EJECT                                                                
040102 S99-ABEND SECTION.                                                       
040103                                                                          
040104     SKIP2                                                                
040105     MOVE 'S' TO POSTSUM-OPKOD                                            
040106     CALL POSTSUM USING POSTSUM-PARM                                      
040107     CALL ABEND USING RKOD-ABEND                                          
040108     .                                                                    
040109     EJECT                                                                
040110* --- IMS SECTIONS  ---                                                   
040111                                                                          
040112     EJECT                                                                
040113 IMS-GET-WDK601 SECTION.                                                  
040114                                                                          
040115     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
040116          DELIMITED BY SIZE INTO SSA1                                     
040117     MOVE '  GE' TO GOOD-STATUSCODES                                      
040118     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
040119     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
040120     PERFORM IMS-STATUSCHECK                                              
040121     .                                                                    
040122     EJECT                                                                
040123 IMS-STATUSCHECK SECTION.                                                 
040124                                                                          
040125     SET STATUS-IX TO 1                                                   
040126     SEARCH GOOD-STATUS                                                   
040127       AT END                                                             
040128         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
040129           DELIMITED BY SIZE INTO ERROR-TEXT                              
040130         DISPLAY ERROR-TEXT                                               
040131         CALL FELLOG                                                      
040132       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
040133         CONTINUE                                                         
040140     END-SEARCH                                                           
040200     .                                                                    
