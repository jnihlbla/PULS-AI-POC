000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5614100.                                                
000300 AUTHOR.         DADHICH PRERNA.                                          
000400 DATE-WRITTEN.   18/06/11.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        CREATES A .CSV FILE FOR US/CN FOR WEEKLY DATA                    
000900*                                                                         
001000*        THE PROGRAM READS     WDL6 WDK6 WDK7                             
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . .                                                   
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
002400*          --- OUTPUT  FILE FOR CN/US                                     
002500     SELECT W56141                     ASSIGN TO W56141D1.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP2                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  W56141                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003301*01  RECORD -COPY W56141 -PRE  UT-  -L.                                   
003302     EJECT                                                                
003600                                                                          
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'W5614100'.            
004000 77  YES                         PIC X       VALUE 'J'.                   
004100 77  NOO                         PIC X       VALUE 'N'.                   
004110 77  WS-TIINLINL                 PIC 9(6)    VALUE ZERO.                  
004120 77  WS-PREV-IDARTNR             PIC S9(9)   VALUE ZERO COMP-3.           
004200     EJECT                                                                
004300 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
004400 01  FILLER REDEFINES TODAYS-DATE.                                        
004500     03  TODAYS-DATE-YEAR        PIC 9(2).                                
004600     03  TODAYS-DATE-MONTH       PIC 9(2).                                
004700     03  TODAYS-DATE-DAY         PIC 9(2).                                
004710     EJECT                                                                
004800 01  SDAY-DATE                   PIC 9(8)    VALUE ZERO.                  
004900 01  FILLER REDEFINES SDAY-DATE.                                          
005000     03  START-DATE-CC           PIC 9(2).                                
005100     03  START-DATE-YYMMDD       PIC 9(6).                                
005220 01  CDAY-DATE                   PIC 9(8)    VALUE ZERO.                  
005230 01  FILLER REDEFINES CDAY-DATE.                                          
005240     03  CURRENT-DATE-CC         PIC 9(2).                                
005250     03  CURRENT-DATE-YYMMDD     PIC 9(6).                                
005800                                                                          
005900 01  WS-TIAAVVD.                                                          
006000     03  WS-TIAAVV               PIC S9(4).                               
006100     03  FILLER                  PIC S9(1).                               
006200     EJECT                                                                
006310*      --- VALID IDDC CODES                                               
006320*                                                                         
006330*01    -COPY WWDC99                                                       
006340*                                                                         
006350*01    -COPY WWDCLAND                                                     
006400 01  GENERAL-SUBPROGRAMS.                                                 
006500*                                                                         
006600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006800     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007010*01  -COPY WDATAREA                                                       
007100     SKIP2                                                                
007200*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007300                                                                          
007400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007700     SKIP2                                                                
007800 01  ERROR-TEXT.                                                          
007900     03  FILLER                  PIC X(10)    VALUE 'ERROR-TEXT'.         
008000     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008100     EJECT                                                                
008200*    --- PARAMETRAR TILL POSTSUM                                          
008300*                                                                         
008400*01  -COPY W0005   -PRE  POSTSUM-                                         
008500     EJECT                                                                
008600 01  UT-AREA-START               PIC X(24)   VALUE                        
008700                                 'UT-AREA-START  '.                       
011320     SKIP2                                                                
011321                                                                          
011330*01  AREA -COPY W56141     -PRE UT-                                       
011500*    --- AREAS FOR IMS-SECTIONS                                           
011600*                                                                         
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011900     SKIP3                                                                
012000 01  KEYS-FOR-DLI.                                                        
012100     03  W-IDDC-X.                                                        
012200         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
012300     03  W-IDARTNR-X.                                                     
012400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012500     SKIP2                                                                
012600*    --- STATUS-KOD FRÅN IMS                                              
012700 01  STATUS-WS                   PIC XX.                                  
012800     88  SEGMENT-FOUND                       VALUE '  '.                  
012900     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
013000     88  SEGMENT-MISSING                     VALUE 'GE'.                  
013010     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013100     SKIP2                                                                
013200 01  GOOD-STATUSCODES.                                                    
013300     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013400     SKIP3                                                                
013500 01  SSA1                        PIC X(64).                               
013600 01  SSA2                        PIC X(64).                               
013700     EJECT                                                                
013800*    --- IMS FUNCTION CODES                                               
013900*01  -COPY W0003                                                          
014000     EJECT                                                                
014100*    ---  DLI INPUT-OUTPUT AREA                                           
014200 01  DLI-IO-AREA.                                                         
014300     03  IO-AREA                 PIC X(1600) VALUE SPACE.                 
014400     SKIP3                                                                
014500 03  WDL601   REDEFINES IO-AREA.                                          
014600*    05  -COPY WDL601                                                     
014700     SKIP3                                                                
014800 03  WDL611   REDEFINES IO-AREA.                                          
014900*    05  -COPY WDL611                                                     
014910     SKIP3                                                                
014920 01  FILLER         PIC X(16)    VALUE 'DLI-IO-WDK711'.                   
015000 01  DLI-IO-WDK711.                                                       
015100*    03  -COPY WDK711                                                     
015110     SKIP3                                                                
015200 01  FILLER         PIC X(16)    VALUE 'DLI-IO-WDK724'.                   
015300 01  DLI-IO-WDK724.                                                       
015400*    03  -COPY WDK724                                                     
015500 01  FILLER         PIC X(16)    VALUE 'DLI-IO-WDK601'.                   
015600 01  DLI-IO-WDK601.                                                       
015700*    03  -COPY WDK601  -PRE  L-                                           
015800 01  FILLER         PIC X(16)    VALUE 'DLI-IO-WDK611'.                   
015900 01  DLI-IO-WDK611.                                                       
016000*    03  -COPY WDK611                                                     
016100     EJECT                                                                
016200 LINKAGE SECTION.                                                         
016300                                                                          
016400                                                                          
016500*01  -COPY W0008  -PRE WDL6-                                              
016600     05  FILLER                  PIC X.                                   
016700     EJECT                                                                
016800*01  -COPY W0008  -PRE WDK7-                                              
016900     05  FILLER                  PIC X.                                   
017000     EJECT                                                                
017100*01  -COPY W0008  -PRE WDK6-                                              
017200     05  FILLER                  PIC X.                                   
017300     EJECT                                                                
017400                                                                          
017500 PROCEDURE DIVISION  USING WDL6-PCB WDK7-PCB  WDK6-PCB.                   
017600 MAIN SECTION.                                                            
017700     ENTRY 'DLITCBL' USING WDL6-PCB WDK7-PCB  WDK6-PCB.                   
017800                                                                          
017900                                                                          
018000     PERFORM A-INIT                                                       
018100     PERFORM IMS-GET-WDL6                                                 
018200     PERFORM UNTIL SEGMENT-SLUT                                           
018400       EVALUATE WDL6-SEG-NAME-FB                                          
018410         WHEN 'WDL601'                                                    
018420           MOVE ART-IDARTNR      TO W-IDARTNR                             
018500         WHEN 'WDL611'                                                    
018520           PERFORM B-PROCESS                                              
018700       END-EVALUATE                                                       
018800       PERFORM IMS-GET-WDL6                                               
019000     END-PERFORM                                                          
019300     PERFORM Z-FINIT                                                      
019400                                                                          
019500     MOVE ZERO TO RETURN-CODE                                             
019600     GOBACK                                                               
019700     .                                                                    
019800     EJECT                                                                
019900 A-INIT SECTION.                                                          
020000                                                                          
020100     OPEN OUTPUT W56141                                                   
020200                                                                          
020300     ACCEPT TODAYS-DATE          FROM DATE                                
020400     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
020600                                                                          
020610* CALCULATE CURRENT DATE *                                                
020700                                                                          
020710     MOVE TODAYS-DATE            TO DAT-I-TIDATUM                         
020800     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
020900     CALL WDATKONV USING DAT-KDDATFORM                                    
021000                         DAT-I-TIDATUM                                    
021100                         DAT-O-TIDATUM                                    
021200                         DAT-KDSVAR                                       
021300     IF DAT-KDSVAR-OK                                                     
021400         MOVE DAT-TIAAVVD(1:4)   TO WS-TIAAVV                             
021500         MOVE DAT-TIAAMMDD       TO CURRENT-DATE-YYMMDD                   
021600     END-IF                                                               
021700                                                                          
021710* CALCULATE START DATE OF THE WEEK *                                      
021800                                                                          
021810     MOVE WS-TIAAVV              TO DAT-I-TIDATUM                         
021900     MOVE 'AAVV'                 TO DAT-KDDATFORM                         
022000     CALL WDATKONV USING DAT-KDDATFORM                                    
022100                         DAT-I-TIDATUM                                    
022200                         DAT-O-TIDATUM                                    
022300                         DAT-KDSVAR                                       
022400     IF DAT-KDSVAR-OK                                                     
022500       MOVE DAT-TIAAMMDD         TO START-DATE-YYMMDD                     
022600     ELSE                                                                 
022700       MOVE ' INVALID RET CODE FROM WDATKONV'                             
022800                                 TO ERROR-TEXT-STR                        
022900       DISPLAY ERROR-TEXT                                                 
023000       CALL FELLOG                                                        
023100     END-IF                                                               
023200     .                                                                    
023300     EJECT                                                                
023400                                                                          
023500 B-PROCESS SECTION.                                                       
023900                                                                          
023901     MOVE INL-IDDC               TO WS-IDDC                               
023904     MOVE INL-TIINLINL           TO WS-TIINLINL                           
023905       IF NDC-CN OR NDC-US                                                
023911         IF WS-TIINLINL >= START-DATE-YYMMDD  AND                         
023920            WS-TIINLINL <= CURRENT-DATE-YYMMDD                            
023930                                                                          
023940            MOVE INL-IDDC        TO W-IDDC                                
024000            PERFORM IMS-GU-WDK711                                         
024100              IF SEGMENT-FOUND                                            
024204                IF SLAG-IDDC-REF = SPACES                                 
024209                 MOVE W-IDARTNR                                           
024210                                 TO UT-IDARTNR                            
024211                  MOVE INL-IDDC                                           
024212                                 TO UT-IDDC                               
024213                  MOVE INL-KVANTMOT                                       
024214                                 TO UT-KVANTMOT                           
024220                                                                          
024900                  IF INL-IDLOPNRM = ZERO                                  
025000                    MOVE INL-IDFAKT                                       
025010                                 TO UT-IDLOPNRM                           
025100                  ELSE                                                    
025200                    MOVE INL-IDLOPNRM                                     
025210                                 TO UT-IDLOPNRM                           
025300                  END-IF                                                  
025400                                                                          
025410                  PERFORM BB-FETCH-WDK6                                   
025500                  PERFORM S01-SEARCH-IDLAND                               
025510                  PERFORM BC-FETCH-WDK7                                   
025600                  PERFORM S11-WRITE-W56141                                
025700                END-IF                                                    
025710              END-IF                                                      
025800         END-IF                                                           
025810       END-IF                                                             
025900     .                                                                    
026000     EJECT                                                                
026100                                                                          
026110 S01-SEARCH-IDLAND SECTION.                                               
026120                                                                          
026130     SEARCH ALL DC-LAND                                                   
026140       AT END                                                             
026150         MOVE ' NO MATCH FOUND IN WWDCLAND'                               
026160                                 TO ERROR-TEXT-STR                        
026170         CALL FELLOG                                                      
026180       WHEN DCLAND-IDDC (DCLAND-IX) = WS-IDDC                             
026190         MOVE DCLAND-IDLANDX2(DCLAND-IX)                                  
026191                                 TO UT-IDLANDX2                           
026192     END-SEARCH                                                           
026193     .                                                                    
026194     EJECT                                                                
026195                                                                          
026200 BB-FETCH-WDK6 SECTION.                                                   
026300                                                                          
026400     IF(W-IDARTNR = WS-PREV-IDARTNR)                                      
026500         CONTINUE                                                         
026510     ELSE                                                                 
026520        PERFORM IMS-GU-WDK601                                             
026600        MOVE L-ART-IDARTNR       TO WS-PREV-IDARTNR                       
026700        MOVE L-ART-KDPRODSL      TO UT-KDPRODSL                           
026800        PERFORM IMS-GNP-WDK611                                            
026900          IF SEGMENT-FOUND                                                
027000            MOVE CLAG-KDPSLLOC   TO UT-KDPSLLOC                           
027100          END-IF                                                          
027110     END-IF                                                               
027200     .                                                                    
027300     EJECT                                                                
027301                                                                          
027310 BC-FETCH-WDK7 SECTION.                                                   
027320                                                                          
027330     PERFORM IMS-GNP-WDK724                                               
027340                                                                          
027350     PERFORM                                                              
027360       UNTIL SEGMENT-MISSING     OR                                       
027370             INL-IDLEVNR = SPRL-IDLEVNR-PR                                
027380       PERFORM IMS-GNP-WDK724                                             
027390     END-PERFORM                                                          
027391                                                                          
027392     IF SEGMENT-MISSING                                                   
027394       MOVE ZEROES               TO      UT-PRARTBEL-PR                   
027397     ELSE                                                                 
027401       MOVE SPRL-PRARTBEL-PR     TO      UT-PRARTBEL-PR                   
027403     END-IF                                                               
027408     .                                                                    
027409     EJECT                                                                
027410                                                                          
027420                                                                          
027500 Z-FINIT SECTION.                                                         
027600     CLOSE W56141                                                         
027700     SKIP2                                                                
027800     MOVE 'S'                    TO      POSTSUM-OPKOD                    
027900     CALL POSTSUM             USING      POSTSUM-PARM                     
028000     .                                                                    
028100     EJECT                                                                
028200                                                                          
030101                                                                          
030110 S11-WRITE-W56141 SECTION.                                                
030120                                                                          
030130     WRITE UT-RECORD           FROM      UT-AREA                          
030140     MOVE 'W56141'               TO      POSTSUM-FDNAMN                   
030150     MOVE 'W56141D1'             TO      POSTSUM-DDNAMN2                  
030160                                                                          
030180     MOVE SPACES                 TO      POSTSUM-TRANSTYP                 
030190     CALL POSTSUM             USING      POSTSUM-PARM                     
030191                                                                          
030192     .                                                                    
030193     EJECT                                                                
030194                                                                          
030210 S99-ABEND SECTION.                                                       
030300                                                                          
030400     SKIP2                                                                
030500     MOVE 'S'                    TO POSTSUM-OPKOD                         
030600     CALL POSTSUM             USING POSTSUM-PARM                          
030700     CALL ABEND               USING RKOD-ABEND                            
030800     .                                                                    
030900     EJECT                                                                
031000* --- IMS SECTIONS  ---                                                   
031100                                                                          
031200 IMS-GET-WDL6   SECTION.                                                  
031300                                                                          
031400     CALL CBLTDLI             USING GN WDL6-PCB DLI-IO-AREA               
031500     MOVE WDL6-STATUS-CODE       TO STATUS-WS                             
031600     MOVE '  GAGKGB'             TO GOOD-STATUSCODES                      
031700     PERFORM IMS-STATUSCHECK                                              
031800     .                                                                    
031900     EJECT                                                                
032000                                                                          
032100 IMS-GU-WDK601 SECTION.                                                   
032200                                                                          
032300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
032400             DELIMITED BY SIZE INTO SSA1                                  
032500     MOVE '  '                   TO GOOD-STATUSCODES                      
032600     CALL CBLTDLI             USING GU                                    
032700                                    WDK6-PCB                              
032800                                    DLI-IO-WDK601                         
032900                                    SSA1                                  
033000     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
033100     PERFORM IMS-STATUSCHECK                                              
033200     .                                                                    
033300     EJECT                                                                
033400                                                                          
033500 IMS-GNP-WDK611 SECTION.                                                  
033600                                                                          
033700     MOVE 'WDK611             '  TO SSA1                                  
033800     MOVE '  GE'                 TO GOOD-STATUSCODES                      
033900     CALL CBLTDLI             USING GNP                                   
034000                                    WDK6-PCB                              
034100                                    DLI-IO-WDK611                         
034200                                    SSA1                                  
034300     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
034400     PERFORM IMS-STATUSCHECK                                              
034500     .                                                                    
034600     EJECT                                                                
034700                                                                          
034800 IMS-GU-WDK711 SECTION.                                                   
034900                                                                          
035000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
035100             DELIMITED BY SIZE INTO SSA1                                  
035200     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
035300             DELIMITED BY SIZE INTO SSA2                                  
035400     MOVE '  GE'                 TO GOOD-STATUSCODES                      
035500     CALL CBLTDLI             USING GU                                    
035600                                    WDK7-PCB                              
035700                                    DLI-IO-WDK711                         
035800                                    SSA1 SSA2                             
035900     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
036000     PERFORM IMS-STATUSCHECK                                              
036100     .                                                                    
036200     EJECT                                                                
036300                                                                          
036400 IMS-GNP-WDK724 SECTION.                                                  
036500                                                                          
036600     MOVE 'WDK724             '  TO SSA1                                  
036700     MOVE '  GE'                 TO GOOD-STATUSCODES                      
036800     CALL CBLTDLI             USING GNP                                   
036900                                    WDK7-PCB                              
037000                                    DLI-IO-WDK724                         
037100                                    SSA1                                  
037200     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
037300     PERFORM IMS-STATUSCHECK                                              
037400     .                                                                    
037500     EJECT                                                                
037600                                                                          
037700 IMS-STATUSCHECK SECTION.                                                 
037800                                                                          
037900     SET STATUS-IX TO 1                                                   
038000     SEARCH GOOD-STATUS                                                   
038100       AT END                                                             
038200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
038300           DELIMITED BY SIZE INTO ERROR-TEXT                              
038400         DISPLAY ERROR-TEXT                                               
038500         CALL FELLOG                                                      
038600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
038700         CONTINUE                                                         
038800     END-SEARCH                                                           
038900     .                                                                    
