000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3313800.                                                
000400*AUTHOR.         ELEONOR ÖSTRÖM.                                          
000500*DATE-WRITTEN.   07/11/07.                                                
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
002200*        ETRACKER NR 5174378   /071107                                    
002300*                                                                         
002400     SKIP3                                                                
002500 ENVIRONMENT DIVISION.                                                    
002600     SKIP2                                                                
002700 INPUT-OUTPUT SECTION.                                                    
002800                                                                          
002900 FILE-CONTROL.                                                            
003000     SKIP2                                                                
003100*          --- ARTIKLAR MED FÖRÄNDRAD ERSÄTTNINGSKOD                      
003200     SELECT W33138                     ASSIGN TO W33138D1.                
003300     SKIP2                                                                
003400*          --- ERSÄTTNINGSINFORMATION                                     
003500     SELECT W33139                     ASSIGN TO W33138D2.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W33138                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400     SKIP2                                                                
004500*01  -COPY W33138      -L.                                                
004600     SKIP3                                                                
004700 FD  W33139                                                               
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
006800 77  IDPGM                       PIC X(8)    VALUE 'W3313800'.            
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
008000 77  W33138-EOF-SW               PIC X       VALUE 'N'.                   
008100     88  END-OF-W33138                       VALUE 'J'.                   
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
014600*    --- PARAMETRAR TILL WDATKONV                                         
014700*                                                                         
014800*01  -COPY WDATAREA                                                       
014900     EJECT                                                                
015000 01  IN-AREA-START               PIC X(24)   VALUE                        
015100                                 'IN-AREA-START  '.                       
015200                                                                          
015300*01  AREA -COPY W33138     -PRE IN-                                       
015400                                                                          
015500     EJECT                                                                
015600 01  UT-AREA-START               PIC X(24)   VALUE                        
015700                                 'UT-AREA-START  '.                       
015800     SKIP2                                                                
015900 01  UT-AREA.                                                             
016000     03  FILLER                  PIC X(400).                              
016100*01  FILLER -COPY W33550      -PRE UT-   -RED  UT-AREA                    
016200     EJECT                                                                
016300*01  FILLER -COPY W33551      -PRE UT1-  -RED  UT-AREA                    
016400     EJECT                                                                
016500*01  FILLER -COPY W33552      -PRE UT2-  -RED  UT-AREA                    
016600     EJECT                                                                
016700*01  FILLER -COPY W33553      -PRE UT3-  -RED  UT-AREA                    
016800     EJECT                                                                
016900*01  FILLER -COPY W33554      -PRE UT4-  -RED  UT-AREA                    
017000     EJECT                                                                
017100*01  FILLER -COPY W33555      -PRE UT5-  -RED  UT-AREA                    
017200     EJECT                                                                
017300*01  FILLER -COPY W33556      -PRE UT6-  -RED  UT-AREA                    
017400     EJECT                                                                
017500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
017600*                                                                         
017700     SKIP2                                                                
017800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017900     SKIP2                                                                
018000 01  NYCKLAR-TILL-DLI.                                                    
018100     03  W-IDARTNR-X.                                                     
018200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
018300* TILL WDK6.                                                              
018400     03  W-WDK6-IDARTNR-X.                                                
018500         05  W-K6-IDARTNR        PIC S9(9)   VALUE ZERO COMP-3.           
018600     03  W-KDSEGKEY-X.                                                    
018700         05  W-KDSEGKEY           PIC X(1)     VALUE '1'.                 
018800     SKIP2                                                                
018900*    --- STATUS-KOD FRÅN IMS                                              
019000 01  STATUS-WS                   PIC XX.                                  
019100     88  SEGMENT-FINNS                       VALUE '  '.                  
019200     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
019300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
019400     SKIP2                                                                
019500 01  GODK-STATUSKODER.                                                    
019600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019700     SKIP2                                                                
019800 01  SSA1                        PIC X(64).                               
019900 01  SSA2                        PIC X(64).                               
020000     EJECT                                                                
020100*    --- IMS FUNKTIONSKODER                                               
020200*01  -COPY W0003                                                          
020300     EJECT                                                                
020400*    ---  DLI INPUT-OUTPUT AREA                                           
020500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK601'.         
020600 01  DLI-IO-WDK601.                                                       
020700*    03  -COPY WDK601                                                     
020800     EJECT                                                                
020900 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK611'.         
021000 01  DLI-IO-WDK611.                                                       
021100*    03  -COPY WDK611                                                     
021200     EJECT                                                                
021300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
021400     SKIP3                                                                
021500 01  DLI-IO-AREA.                                                         
021600     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
021700     SKIP3                                                                
021800     03  WLERSA01 REDEFINES IO-AREA.                                      
021900*        05  -COPY WDD701  -PRE ERSA-                                     
022000     EJECT                                                                
022100     03  WLERSA11 REDEFINES IO-AREA.                                      
022200*        05  -COPY WDD702  -PRE ERSA-                                     
022300     EJECT                                                                
022400     03  WLERSA13 REDEFINES IO-AREA.                                      
022500*        05  -COPY WDD704  -PRE ERSA-                                     
022600     EJECT                                                                
022700 LINKAGE SECTION.                                                         
022800                                                                          
022900     SKIP2                                                                
023000*01  -COPY W0008  -PRE ERSA-                                              
023100     05  FILLER                  PIC X.                                   
023200     EJECT                                                                
023300*01  -COPY W0008 -PRE  WDK6-.                                             
023400     05  FILLER        PIC X.                                             
023500     EJECT                                                                
023600 PROCEDURE DIVISION  USING ERSA-PCB WDK6-PCB.                             
023700     ENTRY 'DLITCBL' USING ERSA-PCB WDK6-PCB.                             
023800                                                                          
023900     SKIP2                                                                
024000     PERFORM A-INIT                                                       
024100     PERFORM S01-LAES-W33138                                              
024200     IF NOT END-OF-W33138                                                 
024300       MOVE JA            TO POST-SW                                      
024400       MOVE '550'         TO UT-IDPTYP                                    
024500       MOVE 'A'           TO UT-IDVTYP                                    
024600       MOVE DAGENS-DATUM  TO UT-TIFILDAT                                  
024700       MOVE DAGENS-HHMMSS TO UT-TIHHMMSS                                  
024800       PERFORM S11-SKRIV-W33139                                           
024900       MOVE '551'         TO UT1-IDPTYP                                   
025000       MOVE 'A'           TO UT1-IDVTYP                                   
025100       PERFORM S11-SKRIV-W33139                                           
025200     END-IF                                                               
025300     PERFORM UNTIL END-OF-W33138                                          
025400       MOVE ZERO TO WS-TIERSDAT-KONV                                      
025500       PERFORM S03-HAEMTA-KDERS-UTGDAT                                    
025600       PERFORM B-HAEMTA-ERS-INFO                                          
025700       PERFORM S01-LAES-W33138                                            
025800     END-PERFORM                                                          
025900                                                                          
026000     IF POSTER-FINNS                                                      
026100       MOVE '556' TO UT6-IDPTYP                                           
026200       MOVE 'A' TO UT6-IDVTYP                                             
026300       MOVE ANTAL TO UT6-KVTRANS                                          
026400       PERFORM S11-SKRIV-W33139                                           
026500     END-IF                                                               
026600     PERFORM Z-FINIT                                                      
026700                                                                          
026800     MOVE ZERO TO RETURN-CODE                                             
026900     GOBACK                                                               
027000     .                                                                    
027100     EJECT                                                                
027200 A-INIT SECTION.                                                          
027300     MOVE 'A-INIT'    TO WS-SEKTION                                       
027400                                                                          
027500     OPEN INPUT  W33138                                                   
027600                                                                          
027700     OPEN OUTPUT W33139                                                   
027800     SKIP2                                                                
027900     ACCEPT DAGENS-DATUM  FROM DATE                                       
028000     ACCEPT DAGENS-TID    FROM TIME                                       
028100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
028200     .                                                                    
028300     EJECT                                                                
028400 B-HAEMTA-ERS-INFO SECTION.                                               
028500     MOVE 'B-HAEMTA-ERS-INFO'   TO WS-SEKTION                             
028600     SKIP2                                                                
028700     IF IN-KDERS-NEW > IN-KDERS-OLD                                       
028800       MOVE IN-KDERS-NEW TO ERSAETTNING                                   
028900       IF ENTYDIG-ERSAETTNING                                             
029000         PERFORM BA-SKAPA-552-POST                                        
029100       ELSE                                                               
029200         PERFORM BB-SKAPA-553-554-POST                                    
029300       END-IF                                                             
029400     ELSE                                                                 
029500       PERFORM BC-SKAPA-555-POST                                          
029600     END-IF                                                               
029700     .                                                                    
029800     EJECT                                                                
029900 BA-SKAPA-552-POST SECTION.                                               
030000     MOVE 'BA-SKAPA-552-POST'   TO WS-SEKTION                             
030100     SKIP2                                                                
030200     MOVE IN-IDARTNR TO W-IDARTNR                                         
030300                        W-K6-IDARTNR                                      
030400     PERFORM IMS-GU-WDK601                                                
030500     IF SEGMENT-FINNS                                                     
030600       MOVE ART-KDPRODSL  TO UT2-KDPRODSL                                 
030700     ELSE                                                                 
030800       MOVE ZERO          TO UT2-KDPRODSL                                 
030900     END-IF                                                               
031000     PERFORM IMS-GU-WDD701                                                
031100     IF SEGMENT-FINNS                                                     
031200       MOVE ERSA-DIERS-ERS TO WS-DIERS-ERS                                
031300       IF WS-TIERSDAT-KONV = ZERO                                         
031400*--- OM DET INTE ÄR NOLL SÅ FINNS DET ETT DATUM PÅ WDK6(W33137)           
031500*--- SOM SKALL ANVÄNDAS.                                                  
031600         PERFORM S02-HAEMTA-DATUM                                         
031700       END-IF                                                             
031800       PERFORM IMS-GNP-WDD702-FIRST                                       
031900       IF SEGMENT-FINNS                                                   
032000         PERFORM UNTIL SEGMENT-SAKNAS                                     
032100           MOVE ERSA-IDARTNR-TILLK TO UT2-IDARTNR-TILLK                   
032200                                      W-K6-IDARTNR                        
032300           PERFORM IMS-GU-WDK601                                          
032400           IF SEGMENT-FINNS                                               
032500             MOVE ART-KDPRODSL     TO UT2-KDPRODSL-TILLK                  
032600             MOVE ART-KDERS-UTG    TO SPAR-KDERS-TILLK                    
032700             PERFORM IMS-GNP-WDK611                                       
032800             IF SEGMENT-FINNS                                             
032900               MOVE CLAG-KDERS       TO UT2-KDERS-TILLK                   
033000             ELSE                                                         
033100               MOVE SPAR-KDERS-TILLK TO UT2-KDERS-TILLK                   
033200             END-IF                                                       
033300           ELSE                                                           
033400             MOVE ZERO             TO UT2-KDPRODSL-TILLK                  
033500             MOVE ZERO             TO UT2-KDERS-TILLK                     
033600           END-IF                                                         
033700           COMPUTE WS-DIERS-KVOT ROUNDED =                                
033800                   ERSA-DIERS-TILLK / WS-DIERS-ERS                        
033900           IF WS-DIERS-KVOT < +1                                          
034000             MOVE +1             TO UT2-DIERS-KVOT                        
034100           ELSE                                                           
034200             MOVE WS-DIERS-KVOT  TO UT2-DIERS-KVOT                        
034300           END-IF                                                         
034400           MOVE '552'            TO UT2-IDPTYP                            
034500           MOVE 'A'              TO UT2-IDVTYP                            
034600           MOVE IN-IDARTNR       TO UT2-IDARTNR                           
034700           MOVE IN-KDERS-NEW     TO UT2-KDERS                             
034800           MOVE WS-TIERSDAT-KONV TO UT2-DAREGDAT                          
034900           ADD +1                TO ANTAL                                 
035000           PERFORM S11-SKRIV-W33139                                       
035100           PERFORM IMS-GNP-WDD702                                         
035200         END-PERFORM                                                      
035300       ELSE                                                               
035400         MOVE ZERO              TO UT2-IDARTNR-TILLK                      
035500         MOVE ZERO              TO UT2-DIERS-KVOT                         
035600         MOVE ZERO              TO UT2-KDPRODSL-TILLK                     
035700         MOVE ZERO              TO UT2-KDERS-TILLK                        
035800         MOVE '552'             TO UT2-IDPTYP                             
035900         MOVE 'A'               TO UT2-IDVTYP                             
036000         MOVE IN-IDARTNR        TO UT2-IDARTNR                            
036100         MOVE IN-KDERS-NEW      TO UT2-KDERS                              
036200         MOVE WS-TIERSDAT-KONV  TO UT2-DAREGDAT                           
036300         ADD +1                 TO ANTAL                                  
036400         PERFORM S11-SKRIV-W33139                                         
036500       END-IF                                                             
036600     ELSE                                                                 
036700       MOVE ZERO                TO UT2-IDARTNR-TILLK                      
036800       MOVE ZERO                TO UT2-DIERS-KVOT                         
036900       MOVE ZERO                TO UT2-KDPRODSL-TILLK                     
037000       MOVE ZERO                TO UT2-KDERS-TILLK                        
037100       MOVE '552'               TO UT2-IDPTYP                             
037200       MOVE 'A'                 TO UT2-IDVTYP                             
037300       MOVE IN-IDARTNR          TO UT2-IDARTNR                            
037400       MOVE IN-KDERS-NEW        TO UT2-KDERS                              
037500       MOVE WS-TIERSDAT-KONV    TO UT2-DAREGDAT                           
037600       ADD +1                   TO ANTAL                                  
037700       PERFORM S11-SKRIV-W33139                                           
037800     END-IF                                                               
037900     .                                                                    
038000     EJECT                                                                
038100 BB-SKAPA-553-554-POST SECTION.                                           
038200     MOVE 'BB-SKAPA-553-554-POST'   TO WS-SEKTION                         
038300     SKIP2                                                                
038400     MOVE IN-IDARTNR TO W-IDARTNR                                         
038500                        W-K6-IDARTNR                                      
038600     PERFORM IMS-GU-WDK601                                                
038700     IF SEGMENT-FINNS                                                     
038800       MOVE ART-KDPRODSL   TO UT3-KDPRODSL                                
038900                              UT4-KDPRODSL                                
039000     ELSE                                                                 
039100       MOVE ZERO           TO UT3-KDPRODSL                                
039200                              UT4-KDPRODSL                                
039300     END-IF                                                               
039400     PERFORM IMS-GU-WDD701                                                
039500     IF SEGMENT-FINNS                                                     
039600       MOVE ERSA-DIERS-ERS TO WS-DIERS-ERS                                
039700       IF WS-TIERSDAT-KONV = ZERO                                         
039800*--- OM DET INTE ÄR NOLL SÅ FINNS DET ETT DATUM PÅ WDK6(W33137)           
039900*--- SOM SKALL ANVÄNDAS.                                                  
040000         PERFORM S02-HAEMTA-DATUM                                         
040100       END-IF                                                             
040200       PERFORM IMS-GNP-WDD702-FIRST                                       
040300       IF SEGMENT-FINNS                                                   
040400         PERFORM UNTIL SEGMENT-SAKNAS                                     
040500           IF ERSA-FLTEXT = NEJ                                           
040600             MOVE ERSA-IDARTNR-TILLK TO UT3-IDARTNR-TILLK                 
040700                                        W-K6-IDARTNR                      
040800             PERFORM IMS-GU-WDK601                                        
040900             IF SEGMENT-FINNS                                             
041000               MOVE ART-KDPRODSL     TO UT2-KDPRODSL-TILLK                
041100               MOVE ART-KDERS-UTG    TO SPAR-KDERS-TILLK                  
041200               PERFORM IMS-GNP-WDK611                                     
041300               IF SEGMENT-FINNS                                           
041400                 MOVE CLAG-KDERS       TO UT2-KDERS-TILLK                 
041500               ELSE                                                       
041600                 MOVE SPAR-KDERS-TILLK TO UT2-KDERS-TILLK                 
041700               END-IF                                                     
041800             ELSE                                                         
041900               MOVE ZERO             TO UT2-KDPRODSL-TILLK                
042000               MOVE ZERO             TO UT2-KDERS-TILLK                   
042100             END-IF                                                       
042200             COMPUTE WS-DIERS-KVOT =                                      
042300                     ERSA-DIERS-TILLK / WS-DIERS-ERS                      
042400             IF WS-DIERS-ERS < +1                                         
042500               MOVE +1             TO UT3-DIERS-KVOT                      
042600             ELSE                                                         
042700               MOVE WS-DIERS-KVOT  TO UT3-DIERS-KVOT                      
042800             END-IF                                                       
042900             MOVE '553'            TO UT3-IDPTYP                          
043000             MOVE 'A'              TO UT3-IDVTYP                          
043100             MOVE IN-IDARTNR       TO UT3-IDARTNR                         
043200             MOVE IN-KDERS-NEW     TO UT3-KDERS                           
043300             MOVE WS-TIERSDAT-KONV TO UT3-DAREGDAT                        
043400           ELSE                                                           
043500             MOVE '554'            TO UT4-IDPTYP                          
043600             MOVE 'A'              TO UT4-IDVTYP                          
043700             MOVE IN-IDARTNR       TO UT4-IDARTNR                         
043800             MOVE IN-KDERS-NEW     TO UT4-KDERS                           
043900             MOVE ERSA-BEERS       TO UT4-BEERS                           
044000           END-IF                                                         
044100           ADD +1                  TO ANTAL                               
044200           PERFORM S11-SKRIV-W33139                                       
044300           PERFORM IMS-GNP-WDD702                                         
044400         END-PERFORM                                                      
044500       END-IF                                                             
044600     END-IF                                                               
044700     .                                                                    
044800     EJECT                                                                
044900 BC-SKAPA-555-POST SECTION.                                               
045000     MOVE 'BC-SKAPA-555-POST'   TO WS-SEKTION                             
045100     SKIP2                                                                
045200     MOVE IN-IDARTNR TO W-IDARTNR                                         
045300                        W-K6-IDARTNR                                      
045400     PERFORM IMS-GU-WDK601                                                
045500     IF SEGMENT-FINNS                                                     
045600       MOVE ART-KDPRODSL    TO UT5-KDPRODSL                               
045700     ELSE                                                                 
045800       MOVE ZERO            TO UT5-KDPRODSL                               
045900     END-IF                                                               
046000     PERFORM IMS-GU-WDD701                                                
046100     IF SEGMENT-FINNS                                                     
046200       IF WS-TIERSDAT-KONV = ZERO                                         
046300*--- OM DET INTE ÄR NOLL SÅ FINNS DET ETT DATUM PÅ WDK6(W33137)           
046400*--- SOM SKALL TAS ISTÄLLET.                                              
046500         PERFORM S02-HAEMTA-DATUM                                         
046600       END-IF                                                             
046700     ELSE                                                                 
046800       MOVE ZERO TO WS-TIERSDAT-KONV                                      
046900     END-IF                                                               
047000     MOVE '555'          TO UT5-IDPTYP                                    
047100     MOVE 'A'            TO UT5-IDVTYP                                    
047200     MOVE IN-IDARTNR     TO UT5-IDARTNR                                   
047300     MOVE IN-KDERS-OLD   TO UT5-KDERS-OLD                                 
047400     MOVE IN-KDERS-NEW   TO UT5-KDERS-NEW                                 
047500     MOVE WS-TIERSDAT-KONV TO UT5-DAREGDAT                                
047600     ADD +1              TO ANTAL                                         
047700     PERFORM S11-SKRIV-W33139                                             
047800     .                                                                    
047900     EJECT                                                                
048000 S02-HAEMTA-DATUM SECTION.                                                
048100                                                                          
048200     PERFORM IMS-GNP-WDD704                                               
048300     IF SEGMENT-FINNS                                                     
048400       MOVE ERSA-TIERSDAT-REG TO WS-TIERSDAT-AAVVD                        
048500*        KONVERTERA TILL ÅÅÅÅMMDD WS-TIERSDAT-KONV                        
048600       MOVE WS-TIERSDAT-AAVVD  TO DAT-I-TIDATUM                           
048700       MOVE 'AAVVD' TO DAT-KDDATFORM                                      
048800       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
048900                             DAT-O-TIDATUM DAT-KDSVAR                     
049000       IF DAT-KDSVAR-OK                                                   
049100         MOVE DAT-TIAAMMDD TO WS-TIERSDAT-KONV                            
049200*          LÄGG IN ÅRTUSENDET                                             
049300         IF  WS-TIERSDAT-2AA  > 50                                        
049400           MOVE 19 TO   WS-TIERSDAT-1AA                                   
049500         ELSE                                                             
049600           MOVE 20 TO   WS-TIERSDAT-1AA                                   
049700         END-IF                                                           
049800       ELSE                                                               
049900         MOVE ZERO TO WS-TIERSDAT-KONV                                    
050000       END-IF                                                             
050100     ELSE                                                                 
050200       MOVE ZERO TO WS-TIERSDAT-KONV                                      
050300     END-IF                                                               
050400     .                                                                    
050500     EJECT                                                                
050600                                                                          
050700 S03-HAEMTA-KDERS-UTGDAT SECTION.                                         
050800     IF IN-TIERSDAT > 0                                                   
050900*        KONVERTERA TILL ÅÅÅÅMMDD WS-TIERSDAT-KONV                        
051000       MOVE IN-TIERSDAT        TO DAT-I-TIDATUM                           
051100       MOVE 'AAVVD' TO DAT-KDDATFORM                                      
051200       CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                    
051300                             DAT-O-TIDATUM DAT-KDSVAR                     
051400       IF DAT-KDSVAR-OK                                                   
051500         MOVE DAT-TIAAMMDD TO WS-TIERSDAT-KONV                            
051600*          LÄGG IN ÅRTUSENDET                                             
051700         IF  WS-TIERSDAT-2AA  > 50                                        
051800           MOVE 19 TO   WS-TIERSDAT-1AA                                   
051900         ELSE                                                             
052000           MOVE 20 TO   WS-TIERSDAT-1AA                                   
052100         END-IF                                                           
052200       ELSE                                                               
052300         MOVE ZERO TO WS-TIERSDAT-KONV                                    
052400       END-IF                                                             
052500     ELSE                                                                 
052600       MOVE ZERO TO WS-TIERSDAT-KONV                                      
052700     END-IF                                                               
052800                                                                          
052900     .                                                                    
053000     EJECT                                                                
053100                                                                          
053200 Z-FINIT SECTION.                                                         
053300     MOVE 'Z-FINIT'   TO WS-SEKTION                                       
053400     CLOSE W33138                                                         
053500           W33139                                                         
053600     SKIP2                                                                
053700     MOVE 'S' TO POSTSUM-OPKOD                                            
053800     CALL POSTSUM USING POSTSUM-PARM                                      
053900     .                                                                    
054000     EJECT                                                                
054100 S01-LAES-W33138  SECTION.                                                
054200     MOVE 'S01-LAES-W33138'   TO WS-FIL-SEKTION                           
054300     SKIP2                                                                
054400     READ W33138 INTO IN-AREA                                             
054500     AT END                                                               
054600        MOVE HIGH-VALUE TO IN-AREA                                        
054700        SET END-OF-W33138 TO TRUE                                         
054800                                                                          
054900     NOT AT END                                                           
055000        MOVE 'W33138' TO POSTSUM-FDNAMN                                   
055100        MOVE 'W33138D1' TO POSTSUM-DDNAMN2                                
055200        MOVE 'ERSA'    TO POSTSUM-TRANSTYP                                
055300        CALL POSTSUM USING POSTSUM-PARM                                   
055400     END-READ                                                             
055500     .                                                                    
055600     EJECT                                                                
055700 S11-SKRIV-W33139 SECTION.                                                
055800     MOVE 'S11-SKRIV-W33139'   TO WS-FIL-SEKTION                          
055900     SKIP2                                                                
056000     IF UT-IDPTYP = '550'                                                 
056100       WRITE UT-POST FROM UT-AREA                                         
056200     ELSE                                                                 
056300       IF UT-IDPTYP = '551'                                               
056400         WRITE UT1-POST FROM UT-AREA                                      
056500       ELSE                                                               
056600         IF UT-IDPTYP = '552'                                             
056700           WRITE UT2-POST FROM UT-AREA                                    
056800         ELSE                                                             
056900           IF UT-IDPTYP = '553'                                           
057000             WRITE UT3-POST FROM UT-AREA                                  
057100           ELSE                                                           
057200             IF UT-IDPTYP = '554'                                         
057300               WRITE UT4-POST FROM UT-AREA                                
057400             ELSE                                                         
057500               IF UT-IDPTYP = '555'                                       
057600                 WRITE UT5-POST FROM UT-AREA                              
057700               ELSE                                                       
057800                 IF UT-IDPTYP = '556'                                     
057900                   WRITE UT6-POST FROM UT-AREA                            
058000                 END-IF                                                   
058100               END-IF                                                     
058200             END-IF                                                       
058300           END-IF                                                         
058400         END-IF                                                           
058500       END-IF                                                             
058600     END-IF                                                               
058700                                                                          
058800     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
058900     MOVE 'W33138' TO POSTSUM-FDNAMN                                      
059000     MOVE 'W33138D2' TO POSTSUM-DDNAMN2                                   
059100     CALL POSTSUM USING POSTSUM-PARM                                      
059200     .                                                                    
059300     EJECT                                                                
059400* --- IMS SEKTIONER ---                                                   
059500     SKIP3                                                                
059600                                                                          
059700 IMS-GU-WDD701 SECTION.                                                   
059800     MOVE 'IMS-GU-WDD701'      TO WS-IMS-SEKTION                          
059900     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
060000          DELIMITED BY SIZE INTO SSA1                                     
060100     MOVE '  GE' TO GODK-STATUSKODER                                      
060200     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA SSA1                      
060300     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
060400     PERFORM IMS-STATUSKONTROLL                                           
060500     .                                                                    
060600     SKIP2                                                                
060700 IMS-GNP-WDD702-FIRST SECTION.                                            
060800     MOVE 'IMS-GNP-WDD702-FIRST' TO WS-IMS-SEKTION                        
060900     MOVE 'WLERSA11*F' TO SSA1                                            
061000     MOVE '  GE' TO GODK-STATUSKODER                                      
061100     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
061200     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
061300     PERFORM IMS-STATUSKONTROLL                                           
061400     .                                                                    
061500     EJECT                                                                
061600 IMS-GNP-WDD702 SECTION.                                                  
061700     MOVE 'IMS-GNP-WDD702'      TO WS-IMS-SEKTION                         
061800     MOVE 'WLERSA11 ' TO SSA1                                             
061900     MOVE '  GE' TO GODK-STATUSKODER                                      
062000     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
062100     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
062200     PERFORM IMS-STATUSKONTROLL                                           
062300     .                                                                    
062400     EJECT                                                                
062500 IMS-GNP-WDD704 SECTION.                                                  
062600     MOVE 'IMS-GNP-WDD704'      TO WS-IMS-SEKTION                         
062700     MOVE 'WLERSA13 ' TO SSA1                                             
062800     MOVE '  GE' TO GODK-STATUSKODER                                      
062900     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
063000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
063100     PERFORM IMS-STATUSKONTROLL                                           
063200     .                                                                    
063300     EJECT                                                                
063400 IMS-GU-WDK601 SECTION.                                                   
063500     MOVE 'IMS-GU-WDK601'       TO WS-IMS-SEKTION                         
063600     STRING 'WDK601  (IDARTNR  =' W-WDK6-IDARTNR-X ')'                    
063700             DELIMITED BY SIZE INTO SSA1                                  
063800     MOVE '  GE' TO GODK-STATUSKODER                                      
063900     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
064000     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
064100     PERFORM IMS-STATUSKONTROLL                                           
064200     .                                                                    
064300     EJECT                                                                
064400 IMS-GNP-WDK611 SECTION.                                                  
064500     MOVE 'IMS-GNP-WDK611'       TO WS-IMS-SEKTION                        
064600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
064700          DELIMITED BY SIZE INTO SSA1                                     
064800     MOVE '  GE'             TO GODK-STATUSKODER                          
064900     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
065000     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
065100     PERFORM IMS-STATUSKONTROLL                                           
065200     .                                                                    
065300     EJECT                                                                
065400                                                                          
065500 IMS-STATUSKONTROLL SECTION.                                              
065600     SKIP2                                                                
065700     SET STATUS-IX TO 1                                                   
065800     SEARCH GODK-STATUS                                                   
065900       AT END                                                             
066000         STRING 'FELAKTIG STATUSKOD FRÅN IMS : ' STATUS-WS                
066100         DELIMITED BY SIZE INTO FELTEXT                                   
066200         CALL FELLOG                                                      
066300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
066400         CONTINUE                                                         
066500     END-SEARCH                                                           
066600     .                                                                    
