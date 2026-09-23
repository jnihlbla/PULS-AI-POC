000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3355000.                                                
000400*AUTHOR.         THOMAS LARSSON.                                          
000500*DATE-WRITTEN.   94/02/17.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER FIL MED ARTIKLAR SOM HAR FÖRÄNDRAD ERSÄTTNINGSKOD.         
001100*        LÄSER ERSÄTTNINGSREGISTRET ( WDD7 ) FÖR ATT HÄMTA                
001200*        INFORMATION TILL UTFIL.                                          
001300*                                                                         
001400*        PROGRAMMET LÄSER      WDD7                                       
001500*                              WDK6                                       
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100*    REMARKS:                                                             
002200*        ETRACKER NR 989171    /041026                                    
002300*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- ARTIKLAR MED FÖRÄNDRAD ERSÄTTNINGSKOD                      
003200     SELECT W33549                     ASSIGN TO W33550D1.                
003300     SKIP2                                                                
003400*          --- ERSÄTTNINGSINFORMATION                                     
003500     SELECT W33551                     ASSIGN TO W33550D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W33549                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400     SKIP2                                                                
004500*01  -COPY W33549      -L.                                                
004600     SKIP3                                                                
004700 FD  W33551                                                               
004800     RECORDING       V                                                    
004900     BLOCK CONTAINS  0.                                                   
005000     SKIP2                                                                
005100*01  POST -COPY W33550 -PRE  UT-  -L.                                     
005200     SKIP2                                                                
005300*01  POST -COPY W33551 -PRE  UT1-  -L.                                    
005400     SKIP2                                                                
005500*01  POST -COPY W33552 -PRE  UT2-  -L.                                    
005600     SKIP2                                                                
005700*01  POST -COPY W33553 -PRE  UT3-  -L.                                    
005800     SKIP2                                                                
005900*01  POST -COPY W33554 -PRE  UT4-  -L.                                    
006000     SKIP2                                                                
006100*01  POST -COPY W33555 -PRE  UT5-  -L.                                    
006200     SKIP2                                                                
006300*01  POST -COPY W33556 -PRE  UT6-  -L.                                    
006400     EJECT                                                                
006500 WORKING-STORAGE SECTION.                                                 
006600*    -- CHECKED BY WY2000 ALMOST!!                                        
006700     SKIP2                                                                
006800 77  IDPGM                       PIC X(8)    VALUE 'W3355000'.            
006900 77  JA                          PIC X       VALUE 'J'.                   
007000 77  NEJ                         PIC X       VALUE 'N'.                   
007100                                                                          
007200 01  WS-TIERSDAT-KONV            PIC 9(8) VALUE ZERO.                     
007300 01  FILLER REDEFINES WS-TIERSDAT-KONV.                                   
007400     03  WS-TIERSDAT-1AA         PIC 9(2).                                
007500     03  WS-TIERSDAT-2AA         PIC 9(2).                                
007600     03  WS-TIERSDAT-MMDD        PIC 9(4).                                
007700                                                                          
007800 77  WS-TIERSDAT-AAVVD           PIC 9(5) VALUE ZERO.                     
007900                                                                          
008000 77  W33549-EOF-SW               PIC X       VALUE 'N'.                   
008100     88  END-OF-W33549                       VALUE 'J'.                   
008200                                                                          
008300 77  POST-SW                     PIC X       VALUE 'N'.                   
008400     88  POSTER-FINNS                        VALUE 'J'.                   
008500     88  POSTER-SAKNAS                       VALUE 'N'.                   
008600                                                                          
008700 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
008800 01  FILLER REDEFINES DAGENS-DATUM.                                       
008900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009200     SKIP2                                                                
009300 01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
009400 01  FILLER REDEFINES DAGENS-TID.                                         
009500     03  DAGENS-HHMMSS           PIC 9(6).                                
009600     03  FILLER                  PIC 9(2).                                
009700     SKIP2                                                                
009800 77  ERSAETTNING                 PIC 9(2)    VALUE ZERO.                  
009900     88  ENTYDIG-ERSAETTNING                 VALUE 01 11 21               
010000                                                   02 22                  
010100                                                   03 23                  
010200                                                   07 27                  
010300                                                   09 19 29               
010400                                                   52.                    
010500     88  EJ-ENTYDIG-ERSAETTNING              VALUE 04 14 24               
010600                                                   05 25                  
010700                                                   06 26                  
010800                                                   08 28.                 
010900     EJECT                                                                
011000                                                                          
011100 01  DYNAMISKA-SUBPROGRAM.                                                
011200*                                                                         
011300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011700     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011800     SKIP2                                                                
011900*    --- PARAMETRAR TILL ABEND                                            
012000                                                                          
012100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012300     SKIP2                                                                
012400 77  WS-DIERS-ERS                PIC 9(3)V9(3) VALUE ZERO.                
012500 77  WS-DIERS-KVOT               PIC 9(3)V9(3) VALUE ZERO.                
012600 77  SPAR-KDERS-TILLK            PIC S9(3) VALUE ZERO COMP-3.             
012700     SKIP2                                                                
012800 77  ANTAL                       PIC S9(7) VALUE +0 COMP SYNC.            
012900                                                                          
013000 01  FILLER                  PIC X(16)   VALUE 'WS-SEKTION'.              
013100 01  WS-SEKTION                  PIC X(30)   VALUE SPACE.                 
013200 01  FILLER                  PIC X(16)   VALUE 'WS-FIL-SEKTION'.          
013300 01  WS-FIL-SEKTION              PIC X(30)   VALUE SPACE.                 
013400 01  FILLER                  PIC X(16)   VALUE 'WS-IMS-SEKTION'.          
013500 01  WS-IMS-SEKTION              PIC X(30)   VALUE SPACE.                 
013600                                                                          
013700                                                                          
013800 01  FELTEXT.                                                             
013900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
014000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
014100     EJECT                                                                
014200*    --- PARAMETRAR TILL POSTSUM                                          
014300*                                                                         
014400*01  -COPY W0005   -PRE  POSTSUM-                                         
014500     EJECT                                                                
014600*01  -COPY WWPRODSL                                                       
014700     EJECT                                                                
014800*    --- PARAMETRAR TILL WDATKONV                                         
014900*                                                                         
015000*01  -COPY WDATAREA                                                       
015100     EJECT                                                                
015200 01  IN-AREA-START               PIC X(24)   VALUE                        
015300                                 'IN-AREA-START  '.                       
015400                                                                          
015500*01  AREA -COPY W33549     -PRE IN-                                       
015600                                                                          
015700     EJECT                                                                
015800 01  UT-AREA-START               PIC X(24)   VALUE                        
015900                                 'UT-AREA-START  '.                       
016000     SKIP2                                                                
016100 01  UT-AREA.                                                             
016200     03  FILLER                  PIC X(400).                              
016300*01  FILLER -COPY W33550      -PRE UT-   -RED  UT-AREA                    
016400     EJECT                                                                
016500*01  FILLER -COPY W33551      -PRE UT1-  -RED  UT-AREA                    
016600     EJECT                                                                
016700*01  FILLER -COPY W33552      -PRE UT2-  -RED  UT-AREA                    
016800     EJECT                                                                
016900*01  FILLER -COPY W33553      -PRE UT3-  -RED  UT-AREA                    
017000     EJECT                                                                
017100*01  FILLER -COPY W33554      -PRE UT4-  -RED  UT-AREA                    
017200     EJECT                                                                
017300*01  FILLER -COPY W33555      -PRE UT5-  -RED  UT-AREA                    
017400     EJECT                                                                
017500*01  FILLER -COPY W33556      -PRE UT6-  -RED  UT-AREA                    
017600     EJECT                                                                
017700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017800*                                                                         
017900     SKIP2                                                                
018000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018100     SKIP2                                                                
018200 01  NYCKLAR-TILL-DLI.                                                    
018300     03  W-IDARTNR-X.                                                     
018400         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018500* TILL WDK6.                                                              
018600     03  W-WDK6-IDARTNR-X.                                                
018700         05  W-K6-IDARTNR        PIC S9(9)   VALUE ZERO COMP-3.           
018800     03  W-KDSEGKEY-X.                                                    
018900         05  W-KDSEGKEY           PIC X(1)     VALUE '1'.                 
019000     SKIP2                                                                
019100*    --- STATUS-KOD FRÅN IMS                                              
019200 01  STATUS-WS                   PIC XX.                                  
019300     88  SEGMENT-FINNS                       VALUE '  '.                  
019400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019600     SKIP2                                                                
019700 01  GODK-STATUSKODER.                                                    
019800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019900     SKIP2                                                                
020000 01  SSA1                        PIC X(64).                               
020100 01  SSA2                        PIC X(64).                               
020200     EJECT                                                                
020300*    --- IMS FUNKTIONSKODER                                               
020400*01  -COPY W0003                                                          
020500     EJECT                                                                
020600*    ---  DLI INPUT-OUTPUT AREA                                           
020700 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
020800 01  DLI-IO-WDK601.                                                       
020900*    03  -COPY WDK601                                                     
021000     EJECT                                                                
021100 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
021200 01  DLI-IO-WDK611.                                                       
021300*    03  -COPY WDK611                                                     
021400     EJECT                                                                
021500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
021600     SKIP3                                                                
021700 01  DLI-IO-AREA.                                                         
021800     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
021900     SKIP3                                                                
022000     03  WLERSA01 REDEFINES IO-AREA.                                      
022100*        05  -COPY WDD701  -PRE ERSA-                                     
022200     EJECT                                                                
022300     03  WLERSA11 REDEFINES IO-AREA.                                      
022400*        05  -COPY WDD702  -PRE ERSA-                                     
022500     EJECT                                                                
022600     03  WLERSA13 REDEFINES IO-AREA.                                      
022700*        05  -COPY WDD704  -PRE ERSA-                                     
022800     EJECT                                                                
022900 LINKAGE SECTION.                                                         
023000                                                                          
023100     SKIP2                                                                
023200*01  -COPY W0008  -PRE ERSA-                                              
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500*01  -COPY W0008 -PRE  WDK6-.                                             
023600     05  FILLER        PIC X.                                             
023700     EJECT                                                                
023800 PROCEDURE DIVISION  USING ERSA-PCB WDK6-PCB.                             
023900     ENTRY 'DLITCBL' USING ERSA-PCB WDK6-PCB.                             
024000                                                                          
024100     SKIP2                                                                
024200     PERFORM A-INIT                                                       
024300     PERFORM S01-LAES-W33549                                              
024400     IF NOT END-OF-W33549                                                 
024500       MOVE JA            TO POST-SW                                      
024600       MOVE '550'         TO UT-IDPTYP                                    
024700       MOVE 'A'           TO UT-IDVTYP                                    
024800       MOVE DAGENS-DATUM  TO UT-TIFILDAT                                  
024900       MOVE DAGENS-HHMMSS TO UT-TIHHMMSS                                  
025000       PERFORM S11-SKRIV-W33551                                           
025100       MOVE '551'         TO UT1-IDPTYP                                   
025200       MOVE 'A'           TO UT1-IDVTYP                                   
025300       PERFORM S11-SKRIV-W33551                                           
025400     END-IF                                                               
025500     PERFORM UNTIL END-OF-W33549                                          
025600       MOVE ZERO TO WS-TIERSDAT-KONV                                      
025700       PERFORM S03-HAEMTA-KDERS-UTGDAT                                    
025800       PERFORM B-HAEMTA-ERS-INFO                                          
025900       PERFORM S01-LAES-W33549                                            
026000     END-PERFORM                                                          
026100                                                                          
026200     IF POSTER-FINNS                                                      
026300       MOVE '556' TO UT6-IDPTYP                                           
026400       MOVE 'A' TO UT6-IDVTYP                                             
026500       MOVE ANTAL TO UT6-KVTRANS                                          
026600       PERFORM S11-SKRIV-W33551                                           
026700     END-IF                                                               
026800     PERFORM Z-FINIT                                                      
026900                                                                          
027000     MOVE ZERO TO RETURN-CODE                                             
027100     GOBACK                                                               
027200     .                                                                    
027300     EJECT                                                                
027400 A-INIT SECTION.                                                          
027500     MOVE 'A-INIT'    TO WS-SEKTION                                       
027600                                                                          
027700     OPEN INPUT  W33549                                                   
027800                                                                          
027900     OPEN OUTPUT W33551                                                   
028000     SKIP2                                                                
028100     ACCEPT DAGENS-DATUM  FROM DATE                                       
028200     ACCEPT DAGENS-TID    FROM TIME                                       
028300     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
028400     .                                                                    
028500     EJECT                                                                
028600 B-HAEMTA-ERS-INFO SECTION.                                               
028700     MOVE 'B-HAEMTA-ERS-INFO'   TO WS-SEKTION                             
028800     SKIP2                                                                
028900     IF IN-KDERS-NEW > IN-KDERS-OLD                                       
029000     OR IN-KDERS-NEW = IN-KDERS-OLD                                       
029100       MOVE IN-KDERS-NEW TO ERSAETTNING                                   
029200       IF ENTYDIG-ERSAETTNING                                             
029300         PERFORM BA-SKAPA-552-POST                                        
029400       ELSE                                                               
029500         PERFORM BB-SKAPA-553-554-POST                                    
029600       END-IF                                                             
029700     ELSE                                                                 
029800       PERFORM BC-SKAPA-555-POST                                          
029900     END-IF                                                               
030000     .                                                                    
030100     EJECT                                                                
030200 BA-SKAPA-552-POST SECTION.                                               
030300     MOVE 'BA-SKAPA-552-POST'   TO WS-SEKTION                             
030400     SKIP2                                                                
030500     MOVE IN-IDARTNR TO W-IDARTNR                                         
030600                        W-K6-IDARTNR                                      
030700     PERFORM IMS-GU-WDK601                                                
030800     IF SEGMENT-FINNS                                                     
030900       MOVE ART-KDPRODSL  TO UT2-KDPRODSL                                 
031000                             TEST-KDPRODSL                                
031100       IF WS-TIERSDAT-KONV = ZERO                                         
031200         MOVE ART-TIERSDAT  TO WS-TIERSDAT-AAVVD                          
031300*        CONVERT TO YYYYMMDD WS-TIERSDAT-KONV                             
031400         PERFORM S04-WDATKONV                                             
031500       END-IF                                                             
031600     ELSE                                                                 
031700       MOVE ZERO          TO UT2-KDPRODSL                                 
031800       MOVE 11            TO TEST-KDPRODSL                                
031900*      MOVE ZERO          TO WS-TIERSDAT-KONV                             
032000     END-IF                                                               
032100*    IF KDPRODSL-LYNK                                                     
032200*      CONTINUE                                                           
032300*    ELSE                                                                 
032400       PERFORM IMS-GU-WDD701                                              
032500       IF SEGMENT-FINNS                                                   
032600         MOVE ERSA-DIERS-ERS TO WS-DIERS-ERS                              
032700         IF WS-TIERSDAT-KONV = ZERO                                       
032800         AND IN-KDERS-NEW < 20                                            
032900*--- OM DET INTE ÄR NOLL SÅ FINNS DET ETT DATUM PÅ WDK6(W33549)           
033000*--- SOM SKALL ANVÄNDAS.                                                  
033100           PERFORM S02-HAEMTA-DATUM                                       
033200         END-IF                                                           
033300         PERFORM IMS-GNP-WDD702-FIRST                                     
033400         IF SEGMENT-FINNS                                                 
033500           PERFORM UNTIL SEGMENT-SAKNAS                                   
033600            IF ERSA-FLTEXT = NEJ                                          
033700             MOVE ERSA-IDARTNR-TILLK TO UT2-IDARTNR-TILLK                 
033800                                        W-K6-IDARTNR                      
033900             PERFORM IMS-GU-WDK601                                        
034000             IF SEGMENT-FINNS                                             
034100               MOVE ART-KDPRODSL     TO UT2-KDPRODSL-TILLK                
034200               MOVE ART-KDERS-UTG    TO SPAR-KDERS-TILLK                  
034300               PERFORM IMS-GNP-WDK611                                     
034400               IF SEGMENT-FINNS                                           
034500                 MOVE CLAG-KDERS       TO UT2-KDERS-TILLK                 
034600               ELSE                                                       
034700                 MOVE SPAR-KDERS-TILLK TO UT2-KDERS-TILLK                 
034800               END-IF                                                     
034900             ELSE                                                         
035000               MOVE ZERO             TO UT2-KDPRODSL-TILLK                
035100               MOVE ZERO             TO UT2-KDERS-TILLK                   
035200             END-IF                                                       
035300             COMPUTE WS-DIERS-KVOT ROUNDED =                              
035400                     ERSA-DIERS-TILLK / WS-DIERS-ERS                      
035500             IF WS-DIERS-KVOT < +1                                        
035600               MOVE +1             TO UT2-DIERS-KVOT                      
035700             ELSE                                                         
035800               MOVE WS-DIERS-KVOT  TO UT2-DIERS-KVOT                      
035900             END-IF                                                       
036000             MOVE '552'            TO UT2-IDPTYP                          
036100             MOVE 'A'              TO UT2-IDVTYP                          
036200             MOVE IN-IDARTNR       TO UT2-IDARTNR                         
036300             MOVE IN-KDERS-NEW     TO UT2-KDERS                           
036400             MOVE WS-TIERSDAT-KONV TO UT2-DAREGDAT                        
036500            ELSE                                                          
036600             MOVE ZERO              TO UT2-IDARTNR-TILLK                  
036700             MOVE ZERO              TO UT2-DIERS-KVOT                     
036800             MOVE ZERO              TO UT2-KDPRODSL-TILLK                 
036900             MOVE ZERO              TO UT2-KDERS-TILLK                    
037000             MOVE '552'             TO UT2-IDPTYP                         
037100             MOVE 'A'               TO UT2-IDVTYP                         
037200             MOVE IN-IDARTNR        TO UT2-IDARTNR                        
037300             MOVE IN-KDERS-NEW      TO UT2-KDERS                          
037400             MOVE WS-TIERSDAT-KONV  TO UT2-DAREGDAT                       
037500            END-IF                                                        
037600            ADD +1                TO ANTAL                                
037700            PERFORM S11-SKRIV-W33551                                      
037800            PERFORM IMS-GNP-WDD702                                        
037900           END-PERFORM                                                    
038000         ELSE                                                             
038100           MOVE ZERO              TO UT2-IDARTNR-TILLK                    
038200           MOVE ZERO              TO UT2-DIERS-KVOT                       
038300           MOVE ZERO              TO UT2-KDPRODSL-TILLK                   
038400           MOVE ZERO              TO UT2-KDERS-TILLK                      
038500           MOVE '552'             TO UT2-IDPTYP                           
038600           MOVE 'A'               TO UT2-IDVTYP                           
038700           MOVE IN-IDARTNR        TO UT2-IDARTNR                          
038800           MOVE IN-KDERS-NEW      TO UT2-KDERS                            
038900           MOVE WS-TIERSDAT-KONV  TO UT2-DAREGDAT                         
039000           ADD +1                 TO ANTAL                                
039100           PERFORM S11-SKRIV-W33551                                       
039200         END-IF                                                           
039300       ELSE                                                               
039400         MOVE ZERO                TO UT2-IDARTNR-TILLK                    
039500         MOVE ZERO                TO UT2-DIERS-KVOT                       
039600         MOVE ZERO                TO UT2-KDPRODSL-TILLK                   
039700         MOVE ZERO                TO UT2-KDERS-TILLK                      
039800         MOVE '552'               TO UT2-IDPTYP                           
039900         MOVE 'A'                 TO UT2-IDVTYP                           
040000         MOVE IN-IDARTNR          TO UT2-IDARTNR                          
040100         MOVE IN-KDERS-NEW        TO UT2-KDERS                            
040200         MOVE WS-TIERSDAT-KONV    TO UT2-DAREGDAT                         
040300         ADD +1                   TO ANTAL                                
040400         PERFORM S11-SKRIV-W33551                                         
040500       END-IF                                                             
040600*    END-IF                                                               
040700     .                                                                    
040800     EJECT                                                                
040900 BB-SKAPA-553-554-POST SECTION.                                           
041000     MOVE 'BB-SKAPA-553-554-POST'   TO WS-SEKTION                         
041100     SKIP2                                                                
041200     MOVE IN-IDARTNR TO W-IDARTNR                                         
041300                        W-K6-IDARTNR                                      
041400     PERFORM IMS-GU-WDK601                                                
041500     IF SEGMENT-FINNS                                                     
041600       MOVE ART-KDPRODSL   TO UT3-KDPRODSL                                
041700                              UT4-KDPRODSL                                
041800       IF WS-TIERSDAT-KONV = ZERO                                         
041900         MOVE ART-TIERSDAT  TO WS-TIERSDAT-AAVVD                          
042000*        CONVERT TO YYYYMMDD WS-TIERSDAT-KONV                             
042100         PERFORM S04-WDATKONV                                             
042200       END-IF                                                             
042300     ELSE                                                                 
042400       MOVE ZERO           TO UT3-KDPRODSL                                
042500                              UT4-KDPRODSL                                
042600     END-IF                                                               
042700     MOVE UT3-KDPRODSL  TO TEST-KDPRODSL                                  
042800     IF KDPRODSL-LYNK                                                     
042900       CONTINUE                                                           
043000     ELSE                                                                 
043100       PERFORM IMS-GU-WDD701                                              
043200       IF SEGMENT-FINNS                                                   
043300         MOVE ERSA-DIERS-ERS TO WS-DIERS-ERS                              
043400         IF WS-TIERSDAT-KONV = ZERO                                       
043500         AND IN-KDERS-NEW < 20                                            
043600*--- OM DET INTE ÄR NOLL SÅ FINNS DET ETT DATUM PÅ WDK6(W33549)           
043700*--- SOM SKALL ANVÄNDAS.                                                  
043800           PERFORM S02-HAEMTA-DATUM                                       
043900         END-IF                                                           
044000         PERFORM IMS-GNP-WDD702-FIRST                                     
044100         IF SEGMENT-FINNS                                                 
044200           PERFORM UNTIL SEGMENT-SAKNAS                                   
044300            IF ERSA-FLTEXT = NEJ                                          
044400             MOVE ERSA-IDARTNR-TILLK TO UT3-IDARTNR-TILLK                 
044500                                        W-K6-IDARTNR                      
044600             PERFORM IMS-GU-WDK601                                        
044700             IF SEGMENT-FINNS                                             
044800               MOVE ART-KDPRODSL     TO UT2-KDPRODSL-TILLK                
044900               MOVE ART-KDERS-UTG    TO SPAR-KDERS-TILLK                  
045000               PERFORM IMS-GNP-WDK611                                     
045100               IF SEGMENT-FINNS                                           
045200                 MOVE CLAG-KDERS       TO UT2-KDERS-TILLK                 
045300               ELSE                                                       
045400                 MOVE SPAR-KDERS-TILLK TO UT2-KDERS-TILLK                 
045500               END-IF                                                     
045600             ELSE                                                         
045700               MOVE ZERO             TO UT2-KDPRODSL-TILLK                
045800               MOVE ZERO             TO UT2-KDERS-TILLK                   
045900             END-IF                                                       
046000             COMPUTE WS-DIERS-KVOT =                                      
046100                     ERSA-DIERS-TILLK / WS-DIERS-ERS                      
046200             IF WS-DIERS-ERS < +1                                         
046300               MOVE +1             TO UT3-DIERS-KVOT                      
046400             ELSE                                                         
046500               MOVE WS-DIERS-KVOT  TO UT3-DIERS-KVOT                      
046600             END-IF                                                       
046700             MOVE '553'            TO UT3-IDPTYP                          
046800             MOVE 'A'              TO UT3-IDVTYP                          
046900             MOVE IN-IDARTNR       TO UT3-IDARTNR                         
047000             MOVE IN-KDERS-NEW     TO UT3-KDERS                           
047100             MOVE WS-TIERSDAT-KONV TO UT3-DAREGDAT                        
047200            ELSE                                                          
047300             MOVE '554'            TO UT4-IDPTYP                          
047400             MOVE 'A'              TO UT4-IDVTYP                          
047500             MOVE IN-IDARTNR       TO UT4-IDARTNR                         
047600             MOVE IN-KDERS-NEW     TO UT4-KDERS                           
047700             MOVE ERSA-BEERS       TO UT4-BEERS                           
047800            END-IF                                                        
047900            ADD +1                  TO ANTAL                              
048000            PERFORM S11-SKRIV-W33551                                      
048100            PERFORM IMS-GNP-WDD702                                        
048200           END-PERFORM                                                    
048300         END-IF                                                           
048400       END-IF                                                             
048500     END-IF                                                               
048600     .                                                                    
048700     EJECT                                                                
048800 BC-SKAPA-555-POST SECTION.                                               
048900     MOVE 'BC-SKAPA-555-POST'   TO WS-SEKTION                             
049000     SKIP2                                                                
049100     MOVE IN-IDARTNR TO W-IDARTNR                                         
049200                        W-K6-IDARTNR                                      
049300     PERFORM IMS-GU-WDK601                                                
049400     IF SEGMENT-FINNS                                                     
049500       MOVE ART-KDPRODSL    TO UT5-KDPRODSL                               
049600       IF WS-TIERSDAT-KONV = ZERO                                         
049700         MOVE ART-TIERSDAT  TO WS-TIERSDAT-AAVVD                          
049800*        CONVERT TO YYYYMMDD WS-TIERSDAT-KONV                             
049900         PERFORM S04-WDATKONV                                             
050000       END-IF                                                             
050100     ELSE                                                                 
050200       MOVE ZERO            TO UT5-KDPRODSL                               
050300     END-IF                                                               
050400     PERFORM IMS-GU-WDD701                                                
050500     IF SEGMENT-FINNS                                                     
050600       IF WS-TIERSDAT-KONV = ZERO                                         
050700       AND IN-KDERS-NEW < 20                                              
050800*--- OM DET INTE ÄR NOLL SÅ FINNS DET ETT DATUM PÅ WDK6(W33549)           
050900*--- SOM SKALL TAS ISTÄLLET.                                              
051000         PERFORM S02-HAEMTA-DATUM                                         
051100       END-IF                                                             
051200     ELSE                                                                 
051300       MOVE ZERO TO WS-TIERSDAT-KONV                                      
051400     END-IF                                                               
051500     MOVE '555'          TO UT5-IDPTYP                                    
051600     MOVE 'A'            TO UT5-IDVTYP                                    
051700     MOVE IN-IDARTNR     TO UT5-IDARTNR                                   
051800     MOVE IN-KDERS-OLD   TO UT5-KDERS-OLD                                 
051900     MOVE IN-KDERS-NEW   TO UT5-KDERS-NEW                                 
052000     MOVE WS-TIERSDAT-KONV TO UT5-DAREGDAT                                
052100     ADD +1              TO ANTAL                                         
052200     PERFORM S11-SKRIV-W33551                                             
052300     .                                                                    
052400     EJECT                                                                
052500 S02-HAEMTA-DATUM SECTION.                                                
052600                                                                          
052700     PERFORM IMS-GNP-WDD704                                               
052800     IF SEGMENT-FINNS                                                     
052900       IF IN-KDERS-NEW > 10 AND < 20                                      
053000          MOVE ERSA-TIERSDAT-PREL-C1 TO WS-TIERSDAT-AAVVD                 
053100          PERFORM S04-WDATKONV                                            
053200       ELSE                                                               
053300         IF IN-KDERS-NEW > 0 AND < 10                                     
053400           MOVE ERSA-TIERSDAT-REG TO WS-TIERSDAT-AAVVD                    
053500           PERFORM S04-WDATKONV                                           
053600         END-IF                                                           
053700       END-IF                                                             
053800     ELSE                                                                 
053900       MOVE ZERO TO WS-TIERSDAT-KONV                                      
054000     END-IF                                                               
054100     .                                                                    
054200     EJECT                                                                
054300                                                                          
054400 S03-HAEMTA-KDERS-UTGDAT SECTION.                                         
054500     IF IN-TIERSDAT > 0                                                   
054600*        KONVERTERA TILL ÅÅÅÅMMDD WS-TIERSDAT-KONV                        
054700       MOVE IN-TIERSDAT        TO DAT-I-TIDATUM                           
054800       MOVE 'AAVVD' TO DAT-KDDATFORM                                      
054900       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
055000                             DAT-O-TIDATUM DAT-KDSVAR                     
055100       IF DAT-KDSVAR-OK                                                   
055200         MOVE DAT-TIAAMMDD TO WS-TIERSDAT-KONV                            
055300*          LÄGG IN ÅRTUSENDET                                             
055400         IF  WS-TIERSDAT-2AA  > 50                                        
055500           MOVE 19 TO   WS-TIERSDAT-1AA                                   
055600         ELSE                                                             
055700           MOVE 20 TO   WS-TIERSDAT-1AA                                   
055800         END-IF                                                           
055900       ELSE                                                               
056000         MOVE ZERO TO WS-TIERSDAT-KONV                                    
056100       END-IF                                                             
056200     ELSE                                                                 
056300       MOVE ZERO TO WS-TIERSDAT-KONV                                      
056400     END-IF                                                               
056500                                                                          
056600     .                                                                    
056700     EJECT                                                                
056800                                                                          
056900 S04-WDATKONV SECTION.                                                    
057000     MOVE WS-TIERSDAT-AAVVD  TO DAT-I-TIDATUM                             
057100     MOVE 'AAVVD' TO DAT-KDDATFORM                                        
057200     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
057300                           DAT-O-TIDATUM DAT-KDSVAR                       
057400     IF DAT-KDSVAR-OK                                                     
057500       MOVE DAT-TIAAMMDD TO WS-TIERSDAT-KONV                              
057600*        INSERT THE MILLENNIUM                                            
057700       IF  WS-TIERSDAT-2AA  > 50                                          
057800         MOVE 19 TO   WS-TIERSDAT-1AA                                     
057900       ELSE                                                               
058000         MOVE 20 TO   WS-TIERSDAT-1AA                                     
058100       END-IF                                                             
058200     ELSE                                                                 
058300       MOVE ZERO TO WS-TIERSDAT-KONV                                      
058400     END-IF                                                               
058500     .                                                                    
058600     EJECT                                                                
058700                                                                          
058800 Z-FINIT SECTION.                                                         
058900     MOVE 'Z-FINIT'   TO WS-SEKTION                                       
059000     CLOSE W33549                                                         
059100           W33551                                                         
059200     SKIP2                                                                
059300     MOVE 'S' TO POSTSUM-OPKOD                                            
059400     CALL POSTSUM USING POSTSUM-PARM                                      
059500     .                                                                    
059600     EJECT                                                                
059700 S01-LAES-W33549  SECTION.                                                
059800     MOVE 'S01-LAES-W33549'   TO WS-FIL-SEKTION                           
059900     SKIP2                                                                
060000     READ W33549 INTO IN-AREA                                             
060100     AT END                                                               
060200        MOVE HIGH-VALUE TO IN-AREA                                        
060300        SET END-OF-W33549 TO TRUE                                         
060400                                                                          
060500     NOT AT END                                                           
060600        MOVE 'W33549' TO POSTSUM-FDNAMN                                   
060700        MOVE 'W33550D1' TO POSTSUM-DDNAMN2                                
060800        MOVE 'ERSA'    TO POSTSUM-TRANSTYP                                
060900        CALL POSTSUM USING POSTSUM-PARM                                   
061000     END-READ                                                             
061100     .                                                                    
061200     EJECT                                                                
061300 S11-SKRIV-W33551 SECTION.                                                
061400     MOVE 'S11-SKRIV-W33551'   TO WS-FIL-SEKTION                          
061500     SKIP2                                                                
061600     IF UT-IDPTYP = '550'                                                 
061700       WRITE UT-POST FROM UT-AREA                                         
061800     ELSE                                                                 
061900       IF UT-IDPTYP = '551'                                               
062000         WRITE UT1-POST FROM UT-AREA                                      
062100       ELSE                                                               
062200         IF UT-IDPTYP = '552'                                             
062300           WRITE UT2-POST FROM UT-AREA                                    
062400         ELSE                                                             
062500           IF UT-IDPTYP = '553'                                           
062600             WRITE UT3-POST FROM UT-AREA                                  
062700           ELSE                                                           
062800             IF UT-IDPTYP = '554'                                         
062900               WRITE UT4-POST FROM UT-AREA                                
063000             ELSE                                                         
063100               IF UT-IDPTYP = '555'                                       
063200                 WRITE UT5-POST FROM UT-AREA                              
063300               ELSE                                                       
063400                 IF UT-IDPTYP = '556'                                     
063500                   WRITE UT6-POST FROM UT-AREA                            
063600                 END-IF                                                   
063700               END-IF                                                     
063800             END-IF                                                       
063900           END-IF                                                         
064000         END-IF                                                           
064100       END-IF                                                             
064200     END-IF                                                               
064300                                                                          
064400     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
064500     MOVE 'W33551' TO POSTSUM-FDNAMN                                      
064600     MOVE 'W33550D2' TO POSTSUM-DDNAMN2                                   
064700     CALL POSTSUM USING POSTSUM-PARM                                      
064800     .                                                                    
064900     EJECT                                                                
065000* --- IMS SEKTIONER ---                                                   
065100     SKIP3                                                                
065200                                                                          
065300 IMS-GU-WDD701 SECTION.                                                   
065400     MOVE 'IMS-GU-WDD701'      TO WS-IMS-SEKTION                          
065500     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
065600          DELIMITED BY SIZE INTO SSA1                                     
065700     MOVE '  GE' TO GODK-STATUSKODER                                      
065800     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA SSA1                      
065900     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
066000     PERFORM IMS-STATUSKONTROLL                                           
066100     .                                                                    
066200     SKIP2                                                                
066300 IMS-GNP-WDD702-FIRST SECTION.                                            
066400     MOVE 'IMS-GNP-WDD702-FIRST' TO WS-IMS-SEKTION                        
066500     MOVE 'WLERSA11*F' TO SSA1                                            
066600     MOVE '  GE' TO GODK-STATUSKODER                                      
066700     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
066800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
066900     PERFORM IMS-STATUSKONTROLL                                           
067000     .                                                                    
067100     EJECT                                                                
067200 IMS-GNP-WDD702 SECTION.                                                  
067300     MOVE 'IMS-GNP-WDD702'      TO WS-IMS-SEKTION                         
067400     MOVE 'WLERSA11 ' TO SSA1                                             
067500     MOVE '  GE' TO GODK-STATUSKODER                                      
067600     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
067700     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
067800     PERFORM IMS-STATUSKONTROLL                                           
067900     .                                                                    
068000     EJECT                                                                
068100 IMS-GNP-WDD704 SECTION.                                                  
068200     MOVE 'IMS-GNP-WDD704'      TO WS-IMS-SEKTION                         
068300     MOVE 'WLERSA13 ' TO SSA1                                             
068400     MOVE '  GE' TO GODK-STATUSKODER                                      
068500     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
068600     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
068700     PERFORM IMS-STATUSKONTROLL                                           
068800     .                                                                    
068900     EJECT                                                                
069000 IMS-GU-WDK601 SECTION.                                                   
069100     MOVE 'IMS-GU-WDK601'       TO WS-IMS-SEKTION                         
069200     STRING 'WDK601  (IDARTNR  =' W-WDK6-IDARTNR-X ')'                    
069300             DELIMITED BY SIZE INTO SSA1                                  
069400     MOVE '  GE' TO GODK-STATUSKODER                                      
069500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
069600     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
069700     PERFORM IMS-STATUSKONTROLL                                           
069800     .                                                                    
069900     EJECT                                                                
070000 IMS-GNP-WDK611 SECTION.                                                  
070100     MOVE 'IMS-GNP-WDK611'       TO WS-IMS-SEKTION                        
070200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
070300          DELIMITED BY SIZE INTO SSA1                                     
070400     MOVE '  GE'             TO GODK-STATUSKODER                          
070500     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
070600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
070700     PERFORM IMS-STATUSKONTROLL                                           
070800     .                                                                    
070900     EJECT                                                                
071000                                                                          
071100 IMS-STATUSKONTROLL SECTION.                                              
071200     SKIP2                                                                
071300     SET STATUS-IX TO 1                                                   
071400     SEARCH GODK-STATUS                                                   
071500       AT END                                                             
071600         STRING 'FELAKTIG STATUSKOD FRÅN IMS : ' STATUS-WS                
071700         DELIMITED BY SIZE INTO FELTEXT                                   
071800         CALL FELLOG                                                      
071900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
072000         CONTINUE                                                         
072100     END-SEARCH                                                           
072200     .                                                                    
