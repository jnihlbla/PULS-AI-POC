000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2334600.                                                
000300 AUTHOR.         KIHLBERG STEFAN.                                         
000400 DATE-WRITTEN.   13/03/06.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        PROGRAM THAT SUM UP CALL OF'S FOR THE COMMING 12 PERIODS         
000900*        SORTED PER DC / SUPPLIER / PARTNUMBER                            
001000*                                                                         
001100*        THE PROGRAM READS     WDK6                                       
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001610*    COPYBOOKS:                                                           
001620*        A310G072 - HAVE PACKED DECIMAL FIELDS                            
001630*        A310GN72 - HAVE NORMAL DECIMAL FIELDS                            
001640*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- CALL OFF'S FOR NDC-CN                                      
002600     SELECT W23345                     ASSIGN TO W23346D1.                
002700     SKIP2                                                                
002800*          --- SUMMED CALL OFFS DC71                                      
002900     SELECT W23346                     ASSIGN TO W23346D2.                
003000     SKIP2                                                                
003100*          --- SUMMED CALL OFFS DC72                                      
003200     SELECT W23347                     ASSIGN TO W23346D3.                
003300     SKIP2                                                                
003400*          --- SUMMED CALL OFFS DC 73                                     
003500     SELECT W23348                     ASSIGN TO W23346D4.                
003600     SKIP2                                                                
003700*          --- SUMMED CALL OFFS DC 11                                     
003800     SELECT W23349                     ASSIGN TO W23346D5.                
003900     SKIP2                                                                
004000*          --- SUMMED CALL OFFS DC41                                      
004100     SELECT W23350                     ASSIGN TO W23346D6.                
004200     SKIP2                                                                
004300*          --- SUMMED CALL OFFS DC43                                      
004400     SELECT W23351                     ASSIGN TO W23346D7.                
004500     SKIP2                                                                
004600*          --- SUMMED CALL OFFS DC44                                      
004700     SELECT W23352                     ASSIGN TO W23346D8.                
004800     SKIP2                                                                
004900*          --- SUMMED CALL OFFS DC45                                      
005000     SELECT W23353                     ASSIGN TO W23346D9.                
005100     SKIP2                                                                
005200*          --- SUMMED CALL OFFS DC46                                      
005300     SELECT W23354                     ASSIGN TO W23346DA.                
005301     SKIP2                                                                
005310*          --- SUMMED CALL OFFS DC47                                      
005320     SELECT W23355                     ASSIGN TO W23346DB.                
005321     SKIP2                                                                
005330*          --- SUMMED CALL OFFS FOR ALL PLANTS TO S4HANA                  
005340     SELECT W23346S4                   ASSIGN TO W23346DC.                
005350     SKIP2                                                                
005420     EJECT                                                                
005500 DATA DIVISION.                                                           
005600     SKIP2                                                                
005700 FILE SECTION.                                                            
005800     SKIP3                                                                
005900 FD  W23345                                                               
006000     RECORDING       F                                                    
006100     BLOCK CONTAINS  0.                                                   
006200                                                                          
006300*01  -COPY W23344      -L.                                                
006400     SKIP3                                                                
006500 FD  W23346                                                               
006600     RECORDING       F                                                    
006700     BLOCK CONTAINS  0.                                                   
006800                                                                          
006900*01  RECORD -COPY A310G072 -PRE  CHN07-  -L.                              
007000     SKIP3                                                                
007100 FD  W23347                                                               
007200     RECORDING       F                                                    
007300     BLOCK CONTAINS  0.                                                   
007400                                                                          
007500*01  RECORD -COPY A310G072 -PRE  CHN04-  -L.                              
007600     SKIP3                                                                
007700 FD  W23348                                                               
007800     RECORDING       F                                                    
007900     BLOCK CONTAINS  0.                                                   
008000                                                                          
008100*01  RECORD -COPY A310G072 -PRE  AEFZT-  -L.                              
008200     EJECT                                                                
008300 FD  W23349                                                               
008400     RECORDING       F                                                    
008500     BLOCK CONTAINS  0.                                                   
008600                                                                          
008700*01  RECORD -COPY A310G072 -PRE  CDCSE-  -L.                              
008800     EJECT                                                                
008900                                                                          
009000 FD  W23350                                                               
009100     RECORDING       F                                                    
009200     BLOCK CONTAINS  0.                                                   
009300                                                                          
009400*01  RECORD -COPY A310G072 -PRE  USA41-  -L.                              
009500     EJECT                                                                
009600                                                                          
009700 FD  W23351                                                               
009800     RECORDING       F                                                    
009900     BLOCK CONTAINS  0.                                                   
010000                                                                          
010100*01  RECORD -COPY A310G072 -PRE  USA43-  -L.                              
010200     EJECT                                                                
010300                                                                          
010400 FD  W23352                                                               
010500     RECORDING       F                                                    
010600     BLOCK CONTAINS  0.                                                   
010700                                                                          
010800*01  RECORD -COPY A310G072 -PRE  USA44-  -L.                              
010900     EJECT                                                                
011000                                                                          
011100 FD  W23353                                                               
011200     RECORDING       F                                                    
011300     BLOCK CONTAINS  0.                                                   
011400                                                                          
011500*01  RECORD -COPY A310G072 -PRE  USA45-  -L.                              
011600     EJECT                                                                
011700                                                                          
011800 FD  W23354                                                               
011900     RECORDING       F                                                    
012000     BLOCK CONTAINS  0.                                                   
012100                                                                          
012200*01  RECORD -COPY A310G072 -PRE  USA46-  -L.                              
012300     EJECT                                                                
012301                                                                          
012310 FD  W23355                                                               
012320     RECORDING       F                                                    
012330     BLOCK CONTAINS  0.                                                   
012340                                                                          
012350*01  RECORD -COPY A310G072 -PRE  USA47-  -L.                              
012360     EJECT                                                                
012370     SKIP3                                                                
012380 FD  W23346S4                                                             
012390     RECORDING       F                                                    
012391     BLOCK CONTAINS  0.                                                   
012392                                                                          
012393*01  RECORD -COPY A310GN72 -PRE  S4-     -L.                              
012394     SKIP3                                                                
012454     EJECT                                                                
012460 WORKING-STORAGE SECTION.                                                 
012500                                                                          
012600 77  IDPGM                       PIC X(8)    VALUE 'W2334600'.            
012700 77  YES                         PIC X       VALUE 'J'.                   
012800 77  NOO                         PIC X       VALUE 'N'.                   
012900 77  CURRENT-SECTION             PIC X(20)   VALUE SPACE.                 
013000                                                                          
013100 77  W23345-EOF-SW               PIC X       VALUE 'N'.                   
013200     88  END-OF-W23345                       VALUE 'J'.                   
013300     EJECT                                                                
013400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
013500 01  FILLER REDEFINES TODAYS-DATE.                                        
013600     03  TODAYS-DATE-YEAR        PIC 9(2).                                
013700     03  TODAYS-DATE-MONTH       PIC 9(2).                                
013800     03  TODAYS-DATE-DAY         PIC 9(2).                                
013900                                                                          
014000 01 WS-TIAAPP                    PIC 9(4)       VALUE ZERO.               
014100 01 WS-TIAAPP-DELAR              REDEFINES WS-TIAAPP.                     
014200     03 WS-TIAA                  PIC 9(2).                                
014300     03 WS-TIPP                  PIC 9(2).                                
014400                                                                          
014500 77  TEST-KDPRODSL-SW            PIC 9(3)    VALUE ZERO.                  
014600     88  KDPRODSL-LYNC                       VALUE 31 THRU 39.            
014710     EJECT                                                                
014800                                                                          
014900 01  WORK-FIELDS.                                                         
015000     03 SPAR-IDDC                PIC X(2)    VALUE SPACE.                 
015100     03 SPAR-IDLEVNR             PIC X(5)    VALUE SPACE.                 
015200     03 SPAR-IDARTNR             PIC 9(9)    VALUE ZERO.                  
015300                                                                          
015400     03 WS-ANT-INLEV             PIC S9(9)   VALUE ZERO COMP-3.           
015500     03 WS-IDLEVNR-DC            PIC  X(5)   VALUE SPACE.                 
015600     03 WS-KDPRODSL              PIC S9(3)   VALUE ZERO COMP-3.           
015700     03 WS-TIAAPP-NEXT           PIC  9(4)   VALUE ZERO.                  
015800     03 IX                       PIC  9(2)   VALUE ZERO.                  
015900     03 MAX-IX                   PIC  9(2)   VALUE 12.                    
016000                                                                          
016100     03 WS-KVAVROP-SUM           PIC 9(9)    VALUE ZERO.                  
016200     03 WS-PT                    PIC S9(3)   VALUE +72  COMP-3.           
016300                                                                          
016400     03 TAB-SUMTABELL            OCCURS 12.                               
016500      05 TAB-TIAAPP-INL          PIC S9(5)  VALUE ZERO COMP-3.            
016600      05 TAB-KVAVROP             PIC 9(9)   VALUE ZERO.                   
016610     03  WS-MQ-LINE1.                                                     
016620      05  FILLER                  PIC X(19)   VALUE                       
016630                                     '¤MQMPROP PlantCode='.               
016640      05  WS-MQ-PLANTCODE         PIC X(5)    VALUE SPACE.                
016650      05  FILLER                  PIC X(120)  VALUE SPACE.                
016660                                                                          
016800 01  GENERAL-SUBPROGRAMS.                                                 
016900*                                                                         
017000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
017100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
017200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
017300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
017400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
017500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
017600     SKIP2                                                                
017700*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
017800                                                                          
017900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
018000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
018100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
018200     SKIP2                                                                
018300 01  ERROR-TEXT.                                                          
018400     03  FILLER                  PIC X(10)    VALUE 'ERROR-TEXT'.         
018500     03  ERROR-TEXT-STR          PIC X(80)   VALUE SPACE.                 
018600     EJECT                                                                
018700*    --- PARAMETERS FOR SUBPROGRAM DATKORT                                
018800*                                                                         
018900 01  PROGRAM-NAME                PIC X(6)    VALUE 'W23346'.              
019000     SKIP2                                                                
019100 01  DATECARD-ID                 PIC X(6)    VALUE 'WDATUM'.              
019200     SKIP2                                                                
019300*01  -COPY WDATKORT                                                       
019400     EJECT                                                                
019500*    --- PARAMETRAR TILL POSTSUM                                          
019600*                                                                         
019700*01  -COPY W0005   -PRE  POSTSUM-                                         
019800     EJECT                                                                
019900*01  -COPY WDATAREA                                                       
020000     EJECT                                                                
020100 01  IN-AREA-START               PIC X(24)   VALUE                        
020200                                 'IN-AREA-START  '.                       
020300     SKIP2                                                                
020400                                                                          
020500*01  AREA -COPY W23344     -PRE IN-                                       
020600     EJECT                                                                
020700 01  SIPLUS-AREA-START            PIC X(24)   VALUE                       
020800                                 'SIPLUS-AREA-START  '.                   
020900     SKIP2                                                                
021000                                                                          
021100*01  AREA -COPY A310G072     -PRE SIPLUS-                                 
021200     EJECT                                                                
021210 01  S4HANA-AREA-START            PIC X(24)   VALUE                       
021220                                 'S4HANA-AREA-START  '.                   
021230     SKIP2                                                                
021240                                                                          
021250*01  AREA -COPY A310GN72     -PRE S4HANA-                                 
021260     EJECT                                                                
021300*    --- AREAS FOR IMS-SECTIONS                                           
021400*                                                                         
021500     EJECT                                                                
021600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
021700     SKIP3                                                                
021800 01  KEYS-FOR-DLI.                                                        
021900     03  W-IDARTNR-X.                                                     
022000         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
022100     SKIP2                                                                
022200*    --- STATUS-KOD FRÅN IMS                                              
022300 01  STATUS-WS                   PIC XX.                                  
022400     88  SEGMENT-FOUND                       VALUE '  '.                  
022500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
022600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
022700     SKIP2                                                                
022800 01  GOOD-STATUSCODES.                                                    
022900     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
023000     SKIP3                                                                
023100 01  SSA1                        PIC X(64).                               
023200 01  SSA2                        PIC X(64).                               
023300     EJECT                                                                
023400*    --- IMS FUNCTION CODES                                               
023500*01  -COPY W0003                                                          
023600     EJECT                                                                
023700*    ---  DLI INPUT-OUTPUT AREA                                           
023800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
023900 01  DLI-IO-WDK601.                                                       
024000*    03  -COPY WDK601                                                     
024100     EJECT                                                                
024200                                                                          
024300                                                                          
024400 LINKAGE SECTION.                                                         
024500*01  -COPY W0008  -PRE WDK6-                                              
024600     05  FILLER                  PIC X.                                   
024700     EJECT                                                                
024800                                                                          
024900 PROCEDURE DIVISION  USING WDK6-PCB.                                      
025000 MAIN SECTION.                                                            
025100     ENTRY 'DLITCBL' USING WDK6-PCB.                                      
025200                                                                          
025300                                                                          
025400     PERFORM A-INIT                                                       
025500                                                                          
025600     PERFORM S01-READ-W23345                                              
025700     PERFORM AA1-SKAPA-TABELL                                             
025800     PERFORM UNTIL END-OF-W23345                                          
025900       MOVE IN-IDDC        TO SPAR-IDDC                                   
026000       MOVE IN-IDLEVNR     TO SPAR-IDLEVNR                                
026100       MOVE IN-IDARTNR     TO SPAR-IDARTNR                                
026200       MOVE IN-IDLEVNR-DC  TO WS-IDLEVNR-DC                               
026300       MOVE IN-TIAAPP-NEXT TO WS-TIAAPP-NEXT                              
026400       PERFORM AA-NOLLSTALL                                               
026500                                                                          
026600       PERFORM C-KOLLA-KDPRODSL                                           
026700                                                                          
026800       PERFORM UNTIL END-OF-W23345   OR                                   
026900       IN-IDDC    NOT = SPAR-IDDC    OR                                   
027000       IN-IDLEVNR NOT = SPAR-IDLEVNR OR                                   
027100       IN-IDARTNR NOT = SPAR-IDARTNR                                      
027200                                                                          
027300         PERFORM B-SUMMERA                                                
027800                                                                          
027900         PERFORM S01-READ-W23345                                          
028000       END-PERFORM                                                        
028100                                                                          
028200       IF NOT KDPRODSL-LYNC                                               
028300          PERFORM D-SKAPA-SIPLUS-S4HANA-FORCAST                           
028400          EVALUATE SPAR-IDDC                                              
028500            WHEN '71'                                                     
028600              PERFORM S11-WRITE-W23346                                    
028700            WHEN '72'                                                     
028800              PERFORM S12-WRITE-W23347                                    
028900            WHEN '73'                                                     
029000              PERFORM S13-WRITE-W23348                                    
029100            WHEN '11'                                                     
029200              PERFORM S14-WRITE-W23349                                    
029300            WHEN '41'                                                     
029400              PERFORM S15-WRITE-W23350                                    
029500            WHEN '43'                                                     
029600              PERFORM S16-WRITE-W23351                                    
029700            WHEN '44'                                                     
029800              PERFORM S17-WRITE-W23352                                    
029900            WHEN '45'                                                     
030000              PERFORM S18-WRITE-W23353                                    
030100            WHEN '46'                                                     
030200              PERFORM S19-WRITE-W23354                                    
030210            WHEN '47'                                                     
030220              PERFORM S20-WRITE-W23355                                    
030300          END-EVALUATE                                                    
030310          PERFORM S21-WRITE-W23346S4                                      
030320       ELSE                                                               
030330**********ONLY LYNK IN EUROPE*********                                    
030340          PERFORM D-SKAPA-SIPLUS-S4HANA-FORCAST                           
030350          IF SPAR-IDDC = '11'                                             
030360              PERFORM S14-WRITE-W23349                                    
030370          END-IF                                                          
030400       END-IF                                                             
030500     END-PERFORM                                                          
030600                                                                          
030700                                                                          
030800     PERFORM Z-FINIT                                                      
030900                                                                          
031000     MOVE ZERO TO RETURN-CODE                                             
031100     GOBACK                                                               
031200     .                                                                    
031300     EJECT                                                                
031400 A-INIT SECTION.                                                          
031500                                                                          
031600     MOVE 'A-INIT'      TO CURRENT-SECTION.                               
031700                                                                          
031800     OPEN INPUT  W23345                                                   
031900                                                                          
032000     OPEN OUTPUT W23346                                                   
032100                 W23347                                                   
032200                 W23348                                                   
032300                 W23349                                                   
032400                 W23350                                                   
032500                 W23351                                                   
032600                 W23352                                                   
032700                 W23353                                                   
032800                 W23354                                                   
032810                 W23355                                                   
032811                 W23346S4                                                 
032821                                                                          
032822     INITIALIZE WS-MQ-PLANTCODE                                           
032823                                                                          
033000     CALL DATKORT USING PROGRAM-NAME DATECARD-ID DATUMKORT                
033100     MOVE D-AAR       TO TODAYS-DATE-YEAR                                 
033200     MOVE D-MAANAD    TO TODAYS-DATE-MONTH                                
033300     MOVE D-DAG       TO TODAYS-DATE-DAY                                  
033400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
033500                                                                          
033600     .                                                                    
033700     EJECT                                                                
033800                                                                          
033900 AA1-SKAPA-TABELL SECTION.                                                
034000     MOVE 'AA1-SKAPA-TABELL' TO CURRENT-SECTION.                          
034100                                                                          
034200*    CREATE TAB                                                           
034300     MOVE 1 TO IX                                                         
034400     PERFORM UNTIL IX > MAX-IX                                            
034500       IF IX = 1                                                          
034600         MOVE IN-TIAAPP-NEXT TO TAB-TIAAPP-INL(IX)                        
034700       ELSE                                                               
034800         MOVE TAB-TIAAPP-INL(IX - 1)  TO WS-TIAAPP                        
034900         IF WS-TIPP = 12                                                  
035000             MOVE 01 TO WS-TIPP                                           
035100             ADD 1   TO WS-TIAA                                           
035200         ELSE                                                             
035300             ADD 1 TO WS-TIPP                                             
035400         END-IF                                                           
035500         MOVE WS-TIAAPP      TO TAB-TIAAPP-INL(IX)                        
035600         MOVE ZERO           TO TAB-KVAVROP(IX)                           
035700       END-IF                                                             
035800       ADD 1        TO  IX                                                
035900     END-PERFORM                                                          
036000     .                                                                    
036100     EJECT                                                                
036200                                                                          
036300 AA-NOLLSTALL SECTION.                                                    
036400     MOVE 'AA-NOLLSTALL' TO CURRENT-SECTION.                              
036500                                                                          
036600     MOVE IN-IDLEVNR-DC  TO WS-IDLEVNR-DC                                 
036700     MOVE IN-TIAAPP-NEXT TO WS-TIAAPP-NEXT                                
036800                                                                          
036900     MOVE 1 TO IX                                                         
037000     PERFORM UNTIL IX > MAX-IX                                            
037100       MOVE ZERO           TO TAB-KVAVROP(IX)                             
037200       ADD 1               TO IX                                          
037300     END-PERFORM                                                          
037400     .                                                                    
037500     EJECT                                                                
037600                                                                          
037700 B-SUMMERA SECTION.                                                       
037800     MOVE 'B-SUMMERA'   TO CURRENT-SECTION.                               
037900                                                                          
038000     MOVE 1 TO IX                                                         
038100     IF IN-TIAAPP-INL < IN-TIAAPP-NEXT                                    
038200       COMPUTE TAB-KVAVROP(1) = TAB-KVAVROP(1) + IN-KVAVROP               
038300     ELSE                                                                 
038400       MOVE 1 TO IX                                                       
038500       PERFORM UNTIL IX > MAX-IX                                          
038600       OR IN-TIAAPP-INL = TAB-TIAAPP-INL(IX)                              
038700                                                                          
038800         ADD 1         TO IX                                              
038900       END-PERFORM                                                        
039000       IF IX > MAX-IX                                                     
039100          CONTINUE                                                        
039200       ELSE                                                               
039300         COMPUTE TAB-KVAVROP(IX) = TAB-KVAVROP(IX) + IN-KVAVROP           
039400       END-IF                                                             
039500     END-IF                                                               
039600     .                                                                    
039700     EJECT                                                                
039800                                                                          
039900 C-KOLLA-KDPRODSL SECTION.                                                
040000                                                                          
040100     MOVE 'C-KOLLA-KDPRODSL' TO CURRENT-SECTION.                          
040200                                                                          
040300     MOVE ZERO           TO TEST-KDPRODSL-SW                              
040400     MOVE IN-IDARTNR     TO W-IDARTNR                                     
040500     PERFORM IMS-GET-WDK601                                               
040600     IF SEGMENT-FOUND                                                     
040700       MOVE ART-KDPRODSL TO WS-KDPRODSL                                   
040800                            TEST-KDPRODSL-SW                              
040900     END-IF                                                               
041000     .                                                                    
041100     EJECT                                                                
041200                                                                          
041300 D-SKAPA-SIPLUS-S4HANA-FORCAST SECTION.                                   
041400                                                                          
041500     MOVE 'D-SKAPA-SIPLUS-S4HANA-' TO CURRENT-SECTION.                    
041600                                                                          
041700     MOVE WS-PT             TO SIPLUS-PT                                  
041710                               S4HANA-PT                                  
041800     MOVE SPAR-IDARTNR      TO SIPLUS-ARTNR                               
041810                               S4HANA-ARTNR                               
041900     IF SPAR-IDDC = '11'                                                  
042000        MOVE 'BP2TW'        TO SIPLUS-GSDB-FORB                           
042010                               S4HANA-GSDB-FORB                           
042100     ELSE                                                                 
042200        MOVE WS-IDLEVNR-DC  TO SIPLUS-GSDB-FORB                           
042210                               S4HANA-GSDB-FORB                           
042300     END-IF                                                               
042400     MOVE SPAR-IDLEVNR      TO SIPLUS-GSDB-LEV                            
042410                               S4HANA-GSDB-LEV                            
042500                                                                          
042600     EVALUATE WS-KDPRODSL                                                 
042700       WHEN  16                                                           
042800                  MOVE '5' TO SIPLUS-KDSEKTOR                             
042810                              S4HANA-KDSEKTOR                             
042900       WHEN  17                                                           
043000                  MOVE '5' TO SIPLUS-KDSEKTOR                             
043010                              S4HANA-KDSEKTOR                             
043100       WHEN  15                                                           
043200                  MOVE '6' TO SIPLUS-KDSEKTOR                             
043210                              S4HANA-KDSEKTOR                             
043300       WHEN OTHER                                                         
043400                  MOVE '3' TO SIPLUS-KDSEKTOR                             
043410                              S4HANA-KDSEKTOR                             
043500     END-EVALUATE                                                         
043600                                                                          
043700     MOVE 1 TO IX                                                         
043800     PERFORM UNTIL IX > MAX-IX                                            
043900       MOVE TAB-KVAVROP(IX)     TO SIPLUS-ARSANT-PER(IX)                  
043910                                   S4HANA-ARSANT-PER(IX)                  
044000       ADD 1         TO IX                                                
044100     END-PERFORM                                                          
044200     MOVE WS-TIAAPP-NEXT   TO SIPLUS-PER                                  
044210                              S4HANA-PER                                  
044300     MOVE ZERO             TO  SIPLUS-FORBNR                              
044400                               SIPLUS-LEVNUM                              
044410                               S4HANA-FORBNR                              
044420                               S4HANA-LEVNUM                              
044500*    CALL ABEND USING RKOD-ABEND-WITH-DUMP                                
044600     .                                                                    
044700     EJECT                                                                
044800                                                                          
044900 Z-FINIT SECTION.                                                         
045000     CLOSE W23345                                                         
045100           W23346                                                         
045200           W23347                                                         
045300           W23348                                                         
045400           W23349                                                         
045500           W23350                                                         
045600           W23351                                                         
045700           W23352                                                         
045800           W23353                                                         
045900           W23354                                                         
045910           W23355                                                         
045920           W23346S4                                                       
046000     SKIP2                                                                
046100     MOVE 'S' TO POSTSUM-OPKOD                                            
046200     CALL POSTSUM USING POSTSUM-PARM                                      
046300     .                                                                    
046400     EJECT                                                                
046500 S01-READ-W23345  SECTION.                                                
046600     READ W23345 INTO IN-AREA                                             
046700     AT END                                                               
046800        MOVE HIGH-VALUE TO IN-AREA                                        
046900        SET END-OF-W23345 TO TRUE                                         
047000                                                                          
047100     NOT AT END                                                           
047200        MOVE 'W23345' TO POSTSUM-FDNAMN                                   
047300        MOVE 'W23346D1' TO POSTSUM-DDNAMN2                                
047400*       -- CHANGE TO MOVE SPACE IF THE FILE LACKS RECORD TYPES            
047500        MOVE SPACE     TO POSTSUM-TRANSTYP                                
047600        CALL POSTSUM USING POSTSUM-PARM                                   
047700     END-READ                                                             
047800     .                                                                    
047900     EJECT                                                                
048000 S11-WRITE-W23346 SECTION.                                                
048100                                                                          
048200     WRITE CHN07-RECORD FROM SIPLUS-AREA                                  
048300                                                                          
048400     MOVE 'CHN07' TO POSTSUM-TRANSTYP                                     
048500     MOVE 'W23346' TO POSTSUM-FDNAMN                                      
048600     MOVE 'W23346D2' TO POSTSUM-DDNAMN2                                   
048700     CALL POSTSUM USING POSTSUM-PARM                                      
048800     .                                                                    
048900     EJECT                                                                
048910 S21-WRITE-W23346S4 SECTION.                                              
048920                                                                          
048921     IF S4HANA-GSDB-FORB = WS-MQ-PLANTCODE                                
048922       CONTINUE                                                           
048923     ELSE                                                                 
048924       MOVE S4HANA-GSDB-FORB     TO WS-MQ-PLANTCODE                       
048925       WRITE S4-RECORD           FROM WS-MQ-LINE1                         
048926       MOVE SPACE                TO S4-RECORD                             
048927     END-IF                                                               
048928                                                                          
048930     WRITE S4-RECORD FROM S4HANA-AREA                                     
048940                                                                          
048950     MOVE 'PLANT'   TO POSTSUM-TRANSTYP                                   
048960     MOVE 'S4HANA'   TO POSTSUM-FDNAMN                                    
048970     MOVE 'W23346DC' TO POSTSUM-DDNAMN2                                   
048980     CALL POSTSUM USING POSTSUM-PARM                                      
048990     .                                                                    
048991     EJECT                                                                
049010 S12-WRITE-W23347 SECTION.                                                
049100                                                                          
049200     WRITE CHN04-RECORD FROM SIPLUS-AREA                                  
049300                                                                          
049400     MOVE 'CHN04' TO POSTSUM-TRANSTYP                                     
049500     MOVE 'W23347' TO POSTSUM-FDNAMN                                      
049600     MOVE 'W23346D3' TO POSTSUM-DDNAMN2                                   
049700     CALL POSTSUM USING POSTSUM-PARM                                      
049800     .                                                                    
049900     EJECT                                                                
050000 S13-WRITE-W23348 SECTION.                                                
050100                                                                          
050200     WRITE AEFZT-RECORD FROM SIPLUS-AREA                                  
050300                                                                          
050400     MOVE 'AEFZT'  TO POSTSUM-TRANSTYP                                    
050500     MOVE 'W23348' TO POSTSUM-FDNAMN                                      
050600     MOVE 'W23346D4' TO POSTSUM-DDNAMN2                                   
050700     CALL POSTSUM USING POSTSUM-PARM                                      
050800     .                                                                    
050900     EJECT                                                                
051000 S14-WRITE-W23349 SECTION.                                                
051100                                                                          
051200     WRITE CDCSE-RECORD FROM SIPLUS-AREA                                  
051300                                                                          
051400     MOVE 'CDCSE'  TO POSTSUM-TRANSTYP                                    
051500     MOVE 'W23349' TO POSTSUM-FDNAMN                                      
051600     MOVE 'W23346D5' TO POSTSUM-DDNAMN2                                   
051700     CALL POSTSUM USING POSTSUM-PARM                                      
051800     .                                                                    
051900     EJECT                                                                
052000 S15-WRITE-W23350 SECTION.                                                
052100                                                                          
052200     WRITE USA41-RECORD FROM SIPLUS-AREA                                  
052300                                                                          
052400     MOVE 'USA41'    TO POSTSUM-TRANSTYP                                  
052500     MOVE 'W23350'   TO POSTSUM-FDNAMN                                    
052600     MOVE 'W23346D6' TO POSTSUM-DDNAMN2                                   
052700     CALL POSTSUM USING POSTSUM-PARM                                      
052800     .                                                                    
052900     EJECT                                                                
053000 S16-WRITE-W23351 SECTION.                                                
053100                                                                          
053200     WRITE USA43-RECORD FROM SIPLUS-AREA                                  
053300                                                                          
053400     MOVE 'USA43'    TO POSTSUM-TRANSTYP                                  
053500     MOVE 'W23351'   TO POSTSUM-FDNAMN                                    
053600     MOVE 'W23346D7' TO POSTSUM-DDNAMN2                                   
053700     CALL POSTSUM USING POSTSUM-PARM                                      
053800     .                                                                    
053900     EJECT                                                                
054000 S17-WRITE-W23352 SECTION.                                                
054100                                                                          
054200     WRITE USA44-RECORD FROM SIPLUS-AREA                                  
054300                                                                          
054400     MOVE 'USA44'    TO POSTSUM-TRANSTYP                                  
054500     MOVE 'W23352'   TO POSTSUM-FDNAMN                                    
054600     MOVE 'W23346D8' TO POSTSUM-DDNAMN2                                   
054700     CALL POSTSUM USING POSTSUM-PARM                                      
054800     .                                                                    
054900     EJECT                                                                
055000 S18-WRITE-W23353 SECTION.                                                
055100                                                                          
055200     WRITE USA45-RECORD FROM SIPLUS-AREA                                  
055300                                                                          
055400     MOVE 'USA45'    TO POSTSUM-TRANSTYP                                  
055500     MOVE 'W23353'   TO POSTSUM-FDNAMN                                    
055600     MOVE 'W23346D9' TO POSTSUM-DDNAMN2                                   
055700     CALL POSTSUM USING POSTSUM-PARM                                      
055800     .                                                                    
055900     EJECT                                                                
056000 S19-WRITE-W23354 SECTION.                                                
056100                                                                          
056200     WRITE USA46-RECORD FROM SIPLUS-AREA                                  
056300                                                                          
056400     MOVE 'USA46'    TO POSTSUM-TRANSTYP                                  
056500     MOVE 'W23354'   TO POSTSUM-FDNAMN                                    
056600     MOVE 'W23346DA' TO POSTSUM-DDNAMN2                                   
056700     CALL POSTSUM USING POSTSUM-PARM                                      
056800     .                                                                    
056900     EJECT                                                                
056917 S20-WRITE-W23355 SECTION.                                                
056920                                                                          
056930     WRITE USA47-RECORD FROM SIPLUS-AREA                                  
056940                                                                          
056950     MOVE 'USA47'    TO POSTSUM-TRANSTYP                                  
056960     MOVE 'W23355'   TO POSTSUM-FDNAMN                                    
056970     MOVE 'W23346DB' TO POSTSUM-DDNAMN2                                   
056980     CALL POSTSUM USING POSTSUM-PARM                                      
056990     .                                                                    
056991     EJECT                                                                
057010 S99-ABEND SECTION.                                                       
057100                                                                          
057200     SKIP2                                                                
057300     MOVE 'S' TO POSTSUM-OPKOD                                            
057400     CALL POSTSUM USING POSTSUM-PARM                                      
057500     CALL ABEND USING RKOD-ABEND                                          
057600     .                                                                    
057700     EJECT                                                                
057800* --- IMS SECTIONS  ---                                                   
057900                                                                          
058000     EJECT                                                                
058100 IMS-GET-WDK601 SECTION.                                                  
058200                                                                          
058300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
058400          DELIMITED BY SIZE INTO SSA1                                     
058500     MOVE '  GE' TO GOOD-STATUSCODES                                      
058600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
058700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
058800     PERFORM IMS-STATUSCHECK                                              
058900     .                                                                    
059000     EJECT                                                                
059100 IMS-STATUSCHECK SECTION.                                                 
059200                                                                          
059300     SET STATUS-IX TO 1                                                   
059400     SEARCH GOOD-STATUS                                                   
059500       AT END                                                             
059600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
059700           DELIMITED BY SIZE INTO ERROR-TEXT                              
059800         DISPLAY ERROR-TEXT                                               
059900         CALL FELLOG                                                      
060000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
060100         CONTINUE                                                         
060200     END-SEARCH                                                           
060300     .                                                                    
