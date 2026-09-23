000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3355200.                                                
000400*AUTHOR.         THOMAS LARSSON.                                          
000500*DATE-WRITTEN.   94/03/04.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER WDK6 MED SB.FÖR DE ARTIKLAR SOM HAR ERSÄTTNINGS-           
001100*        KOD SOM ÄR STÖRRE ÄN NOLL HÄMTAR MAN INFORMATION FRÅN            
001200*        ERSÄTTNINGSREGISTRET OCH SKAPAR EN FIL TILL MARKNADS-            
001300*        BOLAGEN. LÄSER ÄVEN WDK6 MED GU FÖR ATT FÅ FRAM PRODUKT-         
001400*        SLAG FÖR DEN ERSÄTTANDE ARTIKELN.                                
001500*                                                                         
001600*        PROGRAMMET LÄSER      WLARTC (WDK6)                              
001700*                              WLERSA (WDD7)                              
001800*                                                                         
001900*    ABENDKODER:                                                          
002000*        U0016 -  . . . .                                                 
002100*        U1000 -  . . . .                                                 
002200*                                                                         
002300*    REMARKS:                                                             
002400*        ETRACKER NR 989171 /041026                                       
002500*                                                                         
002600                                                                          
002700     SKIP3                                                                
002800 ENVIRONMENT DIVISION.                                                    
002900     SKIP2                                                                
003000 INPUT-OUTPUT SECTION.                                                    
003100                                                                          
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*          --- FIL MED ERSÄTTNINGSINFORMATION                             
003500     SELECT W33553                     ASSIGN TO W33552D1.                
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP3                                                                
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W33553                                                               
004200     RECORDING       V                                                    
004300     BLOCK CONTAINS  0.                                                   
004400     SKIP2                                                                
004500*01  POST -COPY W33550 -PRE  UT-  -L.                                     
004600     SKIP2                                                                
004700*01  POST -COPY W33551 -PRE  UT1-  -L.                                    
004800     SKIP2                                                                
004900*01  POST -COPY W33552 -PRE  UT2-  -L.                                    
005000     SKIP2                                                                
005100*01  POST -COPY W33553 -PRE  UT3-  -L.                                    
005200     SKIP2                                                                
005300*01  POST -COPY W33554 -PRE  UT4-  -L.                                    
005400     SKIP2                                                                
005500*01  POST -COPY W33555 -PRE  UT5-  -L.                                    
005600     SKIP2                                                                
005700*01  POST -COPY W33556 -PRE  UT6-  -L.                                    
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
006000*    -- CHECKED BY WY2000 ALMOST!!                                        
006100     SKIP2                                                                
006200 77  IDPGM                       PIC X(8)    VALUE 'W3355200'.            
006300 77  JA                          PIC X       VALUE 'J'.                   
006400 77  NEJ                         PIC X       VALUE 'N'.                   
006500     SKIP2                                                                
006600 01  WS-TIERSDAT-KONV            PIC 9(8)    VALUE ZERO.                  
006700 01  FILLER REDEFINES WS-TIERSDAT-KONV.                                   
006800     03  WS-TIERSDAT-1AA         PIC 9(2).                                
006900     03  WS-TIERSDAT-2AA         PIC 9(2).                                
007000     03  WS-TIERSDAT-MMDD        PIC 9(4).                                
007100                                                                          
007200 77  WS-TIERSDAT-AAVVD           PIC 9(5)    VALUE ZERO.                  
007300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007400 01  FILLER REDEFINES DAGENS-DATUM.                                       
007500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007800                                                                          
007900 01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
008000 01  FILLER REDEFINES DAGENS-TID.                                         
008100     03  DAGENS-HHMMSS           PIC 9(6).                                
008200     03  FILLER                  PIC 9(2).                                
008300                                                                          
008400     EJECT                                                                
008500 77  ERSAETTNING                 PIC 9(2)    VALUE ZERO.                  
008600     88  ENTYDIG-ERSAETTNING                 VALUE 01 11 21               
008700                                                   02 22                  
008800                                                   03 23                  
008900                                                   07 27                  
009000                                                   09 19 29               
009100                                                   52.                    
009200     88 EJ-ENTYDIG-ERSAETTNING               VALUE 04 14 24               
009300                                                   05 25                  
009400                                                   06 26                  
009500                                                   08 28.                 
009600                                                                          
009700 77  FIRST-TIME-SW               PIC X       VALUE 'N'.                   
009800     88  FIRST-TIME                          VALUE 'J'.                   
009900                                                                          
010000 77  WS-DIERS-ERS                PIC 9(3)V9(3) VALUE ZERO.                
010100 77  WS-DIERS-KVOT               PIC 9(3)V9(3) VALUE ZERO.                
010200                                                                          
010300 77  ANTAL                       PIC S9(7) VALUE +0 COMP SYNC.            
010400                                                                          
010500 77  SPAR-IDARTNR                PIC S9(9) VALUE ZERO COMP-3.             
010600 77  SPAR-KDERS                  PIC S9(3) VALUE ZERO COMP-3.             
010700 77  SPAR-KDPRODSL               PIC S9(3) VALUE ZERO COMP-3.             
010800 77  SPAR-KDERS-TILLK            PIC S9(3) VALUE ZERO COMP-3.             
010900     EJECT                                                                
011000 01  DYNAMISKA-SUBPROGRAM.                                                
011100*                                                                         
011200     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
011300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011700     SKIP2                                                                
011800*    --- PARAMETRAR TILL ABEND                                            
011900                                                                          
012000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
012100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
012200     SKIP2                                                                
012300 01  FELTEXT.                                                             
012400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
012500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
012600     EJECT                                                                
012700*    --- PARAMETRAR TILL POSTSUM                                          
012800*                                                                         
012900*01  -COPY W0005   -PRE  POSTSUM-                                         
013000     EJECT                                                                
013100*    --- PARAMETRAR TILL WDATKONV                                         
013200*                                                                         
013300*01  -COPY WDATAREA                                                       
013400     EJECT                                                                
013500*01  -COPY WWPRODSL                                                       
013600     EJECT                                                                
013700 01  UT-AREA-START               PIC X(24)   VALUE                        
013800                                 'UT-AREA-START  '.                       
013900     SKIP2                                                                
014000 01  UT-AREA.                                                             
014100     03  FILLER                  PIC X(400).                              
014200*01  FILLER -COPY W33550      -PRE UT-   -RED  UT-AREA                    
014300     EJECT                                                                
014400*01  FILLER -COPY W33551      -PRE UT1-  -RED  UT-AREA                    
014500     EJECT                                                                
014600*01  FILLER -COPY W33552      -PRE UT2-  -RED  UT-AREA                    
014700     EJECT                                                                
014800*01  FILLER -COPY W33553      -PRE UT3-  -RED  UT-AREA                    
014900     EJECT                                                                
015000*01  FILLER -COPY W33554      -PRE UT4-  -RED  UT-AREA                    
015100     EJECT                                                                
015200*01  FILLER -COPY W33555      -PRE UT5-  -RED  UT-AREA                    
015300     EJECT                                                                
015400*01  FILLER -COPY W33556      -PRE UT6-  -RED  UT-AREA                    
015500     EJECT                                                                
015600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015700*                                                                         
015800     SKIP2                                                                
015900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016000     SKIP3                                                                
016100 01  NYCKLAR-TILL-DLI.                                                    
016200     03  W-IDARTNR-X.                                                     
016300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016400     SKIP2                                                                
016500*    TILL WDK6 UTAN SB.                                                   
016600     03  W-WDK6-IDARTNR-X.                                                
016700         05  W-K6-IDARTNR        PIC S9(9)   VALUE ZERO COMP-3.           
016800     03  W-KDSEGKEY-X.                                                    
016900         05  W-KDSEGKEY          PIC X(1)     VALUE '1'.                  
017000     SKIP2                                                                
017100*    --- STATUS-KOD FRÅN IMS                                              
017200 01  STATUS-WS                   PIC XX.                                  
017300     88  SEGMENT-FINNS                       VALUE '  '.                  
017400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017500     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017600     SKIP2                                                                
017700 01  GODK-STATUSKODER.                                                    
017800     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017900     SKIP3                                                                
018000 01  SSA1                        PIC X(64).                               
018100 01  SSA2                        PIC X(64).                               
018200     EJECT                                                                
018300*    --- IMS FUNKTIONSKODER                                               
018400*01  -COPY W0003                                                          
018500     EJECT                                                                
018600*    ---  DLI INPUT-OUTPUT AREA                                           
018700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
018800     SKIP3                                                                
018900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK601'.        
019000 01  DLI-IO-WDK601.                                                       
019100*    03  -COPY WDK601                                                     
019200 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-WDK611'.        
019300 01  DLI-IO-WDK611.                                                       
019400*    03  -COPY WDK611                                                     
019500     EJECT                                                                
019600     EJECT                                                                
019700 01  DLI-IO-AREA.                                                         
019800     03  IO-AREA                 PIC X(800)  VALUE SPACE.                 
019900     SKIP3                                                                
020000     03  WLARTC01 REDEFINES IO-AREA.                                      
020100*        05  -COPY WDK601  -PRE ARTC-                                     
020200     SKIP3                                                                
020300     03  WLARTC11 REDEFINES IO-AREA.                                      
020400*        05  -COPY WDK611  -PRE ARTC-                                     
020500     EJECT                                                                
020600 01  DLI-IO-AREA2.                                                        
020700     03  IO-AREA2                PIC X(150)  VALUE SPACE.                 
020800     SKIP3                                                                
020900     03  WLERSA01 REDEFINES IO-AREA2.                                     
021000*        05  -COPY WDD701  -PRE ERSA-                                     
021100     SKIP3                                                                
021200     03  WLERSA11 REDEFINES IO-AREA2.                                     
021300*        05  -COPY WDD702  -PRE ERSA-                                     
021400     EJECT                                                                
021500     03  WLERSA13 REDEFINES IO-AREA2.                                     
021600*        05  -COPY WDD704  -PRE ERSA-                                     
021700     EJECT                                                                
021800 LINKAGE SECTION.                                                         
021900                                                                          
022000     SKIP2                                                                
022100*01  -COPY W0008  -PRE ARTC-                                              
022200     05  FILLER                  PIC X.                                   
022300     EJECT                                                                
022400*01  -COPY W0008  -PRE ERSA-                                              
022500     05  FILLER                  PIC X.                                   
022600     EJECT                                                                
022700*01  -COPY W0008  -PRE WDK6-                                              
022800     05  FILLER                  PIC X.                                   
022900     EJECT                                                                
023000 PROCEDURE DIVISION  USING ARTC-PCB ERSA-PCB WDK6-PCB.                    
023100     ENTRY 'DLITCBL' USING ARTC-PCB ERSA-PCB WDK6-PCB.                    
023200                                                                          
023300     SKIP2                                                                
023400     PERFORM A-INIT                                                       
023500     MOVE JA TO FIRST-TIME-SW                                             
023600     PERFORM IMS-GN-WLARTC                                                
023700     PERFORM UNTIL SEGMENT-SLUT                                           
023800       EVALUATE ARTC-SEG-NAME-FB                                          
023900                                                                          
024000         WHEN 'WDK601  '                                                  
024100              MOVE ARTC-ART-IDARTNR   TO SPAR-IDARTNR                     
024200              MOVE ARTC-ART-KDPRODSL  TO SPAR-KDPRODSL                    
024300                                         TEST-KDPRODSL                    
024400              MOVE ARTC-ART-TIERSDAT  TO WS-TIERSDAT-AAVVD                
024500*                                                                         
024600*  FÖR ATT FÅ MED BORTRENSADE ERSÄTTNINGAR                                
024700*                                                                         
024800              IF ARTC-ART-KDERS-UTG > ZERO                                
024900*               IF KDPRODSL-LYNK                                          
025000*                 CONTINUE                                                
025100*               ELSE                                                      
025200                  MOVE ARTC-ART-KDERS-UTG TO SPAR-KDERS                   
025300                  PERFORM S03-HAEMTA-KDERS-UTGDAT                         
025400                  PERFORM B-HAEMTA-ERSAETTNINGS-INFO                      
025500*               END-IF                                                    
025600              END-IF                                                      
025700*                                                                         
025800         WHEN 'WDK611  '                                                  
025900              IF ARTC-CLAG-KDERS > ZERO                                   
026000              AND ARTC-CLAG-KDERS < +53                                   
026100*               IF KDPRODSL-LYNK                                          
026200*                 CONTINUE                                                
026300*               ELSE                                                      
026400                  MOVE ARTC-CLAG-KDERS TO SPAR-KDERS                      
026500                  IF  WS-TIERSDAT-AAVVD > 0                               
026600                    PERFORM S03-HAEMTA-KDERS-UTGDAT                       
026700                  ELSE                                                    
026800                    MOVE ZERO TO WS-TIERSDAT-KONV                         
026900                  END-IF                                                  
027000                  PERFORM B-HAEMTA-ERSAETTNINGS-INFO                      
027100*               END-IF                                                    
027200              END-IF                                                      
027300       END-EVALUATE                                                       
027400       PERFORM IMS-GN-WLARTC                                              
027500     END-PERFORM                                                          
027600                                                                          
027700     MOVE '556'     TO UT6-IDPTYP                                         
027800     MOVE 'A'       TO UT6-IDVTYP                                         
027900     MOVE ANTAL     TO UT6-KVTRANS                                        
028000     PERFORM S11-SKRIV-W33553                                             
028100                                                                          
028200     PERFORM Z-FINIT                                                      
028300                                                                          
028400     MOVE ZERO TO RETURN-CODE                                             
028500     GOBACK                                                               
028600     .                                                                    
028700     EJECT                                                                
028800 A-INIT SECTION.                                                          
028900                                                                          
029000     OPEN OUTPUT W33553                                                   
029100     SKIP2                                                                
029200     ACCEPT DAGENS-DATUM  FROM DATE                                       
029300     ACCEPT DAGENS-TID    FROM TIME                                       
029400                                                                          
029500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
029600     .                                                                    
029700     EJECT                                                                
029800 B-HAEMTA-ERSAETTNINGS-INFO SECTION.                                      
029900     SKIP2                                                                
030000     IF FIRST-TIME                                                        
030100       MOVE '550'         TO UT-IDPTYP                                    
030200       MOVE 'A'           TO UT-IDVTYP                                    
030300       MOVE DAGENS-DATUM  TO UT-TIFILDAT                                  
030400       MOVE DAGENS-HHMMSS TO UT-TIHHMMSS                                  
030500       PERFORM S11-SKRIV-W33553                                           
030600       MOVE '551'         TO UT1-IDPTYP                                   
030700       MOVE 'A'           TO UT1-IDVTYP                                   
030800       PERFORM S11-SKRIV-W33553                                           
030900       MOVE NEJ TO FIRST-TIME-SW                                          
031000     END-IF                                                               
031100     MOVE SPAR-KDERS TO ERSAETTNING                                       
031200     IF ENTYDIG-ERSAETTNING                                               
031300       PERFORM BA-SKAPA-552-POST                                          
031400     ELSE                                                                 
031500       PERFORM BB-SKAPA-553-554-POST                                      
031600     END-IF                                                               
031700     .                                                                    
031800     EJECT                                                                
031900 BA-SKAPA-552-POST SECTION.                                               
032000     SKIP2                                                                
032100     MOVE SPAR-IDARTNR TO W-IDARTNR                                       
032200     PERFORM IMS-GU-WDD701                                                
032300     IF SEGMENT-FINNS                                                     
032400       MOVE ERSA-DIERS-ERS TO WS-DIERS-ERS                                
032500       IF WS-TIERSDAT-KONV = ZERO                                         
032600*--- OM DET INTE ÄR NOLL SÅ FINNS DET ETT DATUM PÅ WDK6(W33549)           
032700*--- SOM SKALL ANVÄNDAS.                                                  
032800         PERFORM S02-HAEMTA-DATUM                                         
032900       END-IF                                                             
033000       PERFORM IMS-GNP-WDD702-FIRST                                       
033100       IF SEGMENT-FINNS                                                   
033200         PERFORM UNTIL SEGMENT-SAKNAS                                     
033300             MOVE ERSA-IDARTNR-TILLK TO UT2-IDARTNR-TILLK                 
033400                                        W-K6-IDARTNR                      
033500             PERFORM IMS-GU-WDK601                                        
033600             IF SEGMENT-FINNS                                             
033700               MOVE ART-KDPRODSL     TO UT2-KDPRODSL-TILLK                
033800               MOVE ART-KDERS-UTG    TO SPAR-KDERS-TILLK                  
033900               PERFORM IMS-GNP-WDK611                                     
034000               IF SEGMENT-FINNS                                           
034100                 MOVE CLAG-KDERS       TO UT2-KDERS-TILLK                 
034200               ELSE                                                       
034300                 MOVE SPAR-KDERS-TILLK TO UT2-KDERS-TILLK                 
034400               END-IF                                                     
034500                                                                          
034600             ELSE                                                         
034700               MOVE ZERO             TO UT2-KDPRODSL-TILLK                
034800               MOVE ZERO             TO UT2-KDERS-TILLK                   
034900             END-IF                                                       
035000             COMPUTE WS-DIERS-KVOT ROUNDED =                              
035100                     ERSA-DIERS-TILLK / WS-DIERS-ERS                      
035200             IF WS-DIERS-KVOT < +1                                        
035300               MOVE +1          TO UT2-DIERS-KVOT                         
035400             ELSE                                                         
035500               MOVE WS-DIERS-KVOT TO UT2-DIERS-KVOT                       
035600             END-IF                                                       
035700                                                                          
035800             MOVE '552'            TO UT2-IDPTYP                          
035900             MOVE 'A'              TO UT2-IDVTYP                          
036000             MOVE SPAR-IDARTNR     TO UT2-IDARTNR                         
036100             MOVE SPAR-KDERS       TO UT2-KDERS                           
036200             MOVE SPAR-KDPRODSL    TO UT2-KDPRODSL                        
036300             MOVE WS-TIERSDAT-KONV TO UT2-DAREGDAT                        
036400             ADD +1                TO ANTAL                               
036500             PERFORM S11-SKRIV-W33553                                     
036600             PERFORM IMS-GNP-WDD702                                       
036700         END-PERFORM                                                      
036800       ELSE                                                               
036900         MOVE ZERO              TO UT2-IDARTNR-TILLK                      
037000         MOVE ZERO              TO UT2-DIERS-KVOT                         
037100         MOVE ZERO              TO UT2-KDPRODSL-TILLK                     
037200         MOVE ZERO              TO UT2-KDERS-TILLK                        
037300         MOVE '552'             TO UT2-IDPTYP                             
037400         MOVE 'A'               TO UT2-IDVTYP                             
037500         MOVE SPAR-IDARTNR      TO UT2-IDARTNR                            
037600         MOVE SPAR-KDPRODSL     TO UT2-KDPRODSL                           
037700         MOVE SPAR-KDERS        TO UT2-KDERS                              
037800         MOVE WS-TIERSDAT-KONV  TO UT2-DAREGDAT                           
037900         ADD +1                 TO ANTAL                                  
038000         PERFORM S11-SKRIV-W33553                                         
038100       END-IF                                                             
038200     ELSE                                                                 
038300       MOVE ZERO                TO UT2-IDARTNR-TILLK                      
038400       MOVE ZERO                TO UT2-DIERS-KVOT                         
038500       MOVE ZERO                TO UT2-KDPRODSL-TILLK                     
038600       MOVE ZERO                TO UT2-KDERS-TILLK                        
038700       MOVE '552'               TO UT2-IDPTYP                             
038800       MOVE 'A'                 TO UT2-IDVTYP                             
038900       MOVE SPAR-IDARTNR        TO UT2-IDARTNR                            
039000       MOVE SPAR-KDERS          TO UT2-KDERS                              
039100       MOVE SPAR-KDPRODSL       TO UT2-KDPRODSL                           
039200       MOVE WS-TIERSDAT-KONV    TO UT2-DAREGDAT                           
039300       ADD +1                   TO ANTAL                                  
039400       PERFORM S11-SKRIV-W33553                                           
039500     END-IF                                                               
039600     .                                                                    
039700     EJECT                                                                
039800 BB-SKAPA-553-554-POST SECTION.                                           
039900     SKIP2                                                                
040000     MOVE SPAR-IDARTNR TO W-IDARTNR                                       
040100     PERFORM IMS-GU-WDD701                                                
040200     IF SEGMENT-FINNS                                                     
040300       MOVE ERSA-DIERS-ERS TO WS-DIERS-ERS                                
040400       IF WS-TIERSDAT-KONV = ZERO                                         
040500*--- OM DET INTE ÄR NOLL SÅ FINNS DET ETT DATUM PÅ WDK6(W33549)           
040600*--- SOM SKALL ANVÄNDAS.                                                  
040700         PERFORM S02-HAEMTA-DATUM                                         
040800       END-IF                                                             
040900       PERFORM IMS-GNP-WDD702-FIRST                                       
041000       IF SEGMENT-FINNS                                                   
041100         PERFORM UNTIL SEGMENT-SAKNAS                                     
041200           IF ERSA-FLTEXT = NEJ                                           
041300             MOVE ERSA-IDARTNR-TILLK TO UT3-IDARTNR-TILLK                 
041400                                        W-K6-IDARTNR                      
041500             PERFORM IMS-GU-WDK601                                        
041600             IF SEGMENT-FINNS                                             
041700               MOVE ART-KDPRODSL     TO UT2-KDPRODSL-TILLK                
041800               MOVE ART-KDERS-UTG    TO SPAR-KDERS-TILLK                  
041900               PERFORM IMS-GNP-WDK611                                     
042000               IF SEGMENT-FINNS                                           
042100                 MOVE CLAG-KDERS       TO UT2-KDERS-TILLK                 
042200               ELSE                                                       
042300                 MOVE SPAR-KDERS-TILLK TO UT2-KDERS-TILLK                 
042400               END-IF                                                     
042500                                                                          
042600             ELSE                                                         
042700               MOVE ZERO             TO UT2-KDPRODSL-TILLK                
042800               MOVE ZERO             TO UT2-KDERS-TILLK                   
042900             END-IF                                                       
043000             COMPUTE WS-DIERS-KVOT ROUNDED =                              
043100                     ERSA-DIERS-TILLK / WS-DIERS-ERS                      
043200             IF WS-DIERS-KVOT < +1                                        
043300               MOVE +1              TO UT3-DIERS-KVOT                     
043400             ELSE                                                         
043500               MOVE WS-DIERS-KVOT   TO UT3-DIERS-KVOT                     
043600             END-IF                                                       
043700             MOVE '553'            TO UT3-IDPTYP                          
043800             MOVE 'A'              TO UT3-IDVTYP                          
043900             MOVE SPAR-IDARTNR     TO UT3-IDARTNR                         
044000             MOVE SPAR-KDPRODSL    TO UT3-KDPRODSL                        
044100             MOVE SPAR-KDERS       TO UT3-KDERS                           
044200             MOVE WS-TIERSDAT-KONV TO UT2-DAREGDAT                        
044300           ELSE                                                           
044400             MOVE '554'            TO UT4-IDPTYP                          
044500             MOVE 'A'              TO UT4-IDVTYP                          
044600             MOVE SPAR-IDARTNR     TO UT4-IDARTNR                         
044700             MOVE SPAR-KDPRODSL    TO UT4-KDPRODSL                        
044800             MOVE SPAR-KDERS       TO UT4-KDERS                           
044900             MOVE ERSA-BEERS       TO UT4-BEERS                           
045000           END-IF                                                         
045100           ADD +1                TO ANTAL                                 
045200           PERFORM S11-SKRIV-W33553                                       
045300           PERFORM IMS-GNP-WDD702                                         
045400         END-PERFORM                                                      
045500       END-IF                                                             
045600     END-IF                                                               
045700     .                                                                    
045800     EJECT                                                                
045900 Z-FINIT SECTION.                                                         
046000     CLOSE W33553                                                         
046100     SKIP2                                                                
046200     MOVE 'S' TO POSTSUM-OPKOD                                            
046300     CALL POSTSUM USING POSTSUM-PARM                                      
046400     .                                                                    
046500     EJECT                                                                
046600 S02-HAEMTA-DATUM SECTION.                                                
046700                                                                          
046800     PERFORM IMS-GNP-WDD704                                               
046900     IF SEGMENT-FINNS                                                     
047000                                                                          
047100       IF ERSA-TIERSDAT-REG IS NUMERIC                                    
047200         MOVE ERSA-TIERSDAT-REG TO WS-TIERSDAT-AAVVD                      
047300*        KONVERTERA TILL ÅÅÅÅMMDD WS-TIERSDAT-KONV                        
047400         MOVE WS-TIERSDAT-AAVVD  TO DAT-I-TIDATUM                         
047500         MOVE 'AAVVD' TO DAT-KDDATFORM                                    
047600         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
047700                               DAT-O-TIDATUM DAT-KDSVAR                   
047800         IF DAT-KDSVAR-OK                                                 
047900           MOVE DAT-TIAAMMDD TO WS-TIERSDAT-KONV                          
048000*          LÄGG IN ÅRTUSENDET                                             
048100           IF  WS-TIERSDAT-2AA  > 50                                      
048200             MOVE 19 TO   WS-TIERSDAT-1AA                                 
048300           ELSE                                                           
048400             MOVE 20 TO   WS-TIERSDAT-1AA                                 
048500           END-IF                                                         
048600         ELSE                                                             
048700           MOVE ZERO TO WS-TIERSDAT-KONV                                  
048800         END-IF                                                           
048900       ELSE                                                               
049000         MOVE ZERO TO WS-TIERSDAT-KONV                                    
049100       END-IF                                                             
049200     ELSE                                                                 
049300       MOVE ZERO TO WS-TIERSDAT-KONV                                      
049400     END-IF                                                               
049500     .                                                                    
049600     EJECT                                                                
049700 S03-HAEMTA-KDERS-UTGDAT SECTION.                                         
049800*        KONVERTERA TILL ÅÅÅÅMMDD WS-TIERSDAT-KONV                        
049900     MOVE WS-TIERSDAT-AAVVD  TO DAT-I-TIDATUM                             
050000     MOVE 'AAVVD'            TO DAT-KDDATFORM                             
050100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
050200                           DAT-O-TIDATUM DAT-KDSVAR                       
050300     IF DAT-KDSVAR-OK                                                     
050400       MOVE DAT-TIAAMMDD TO WS-TIERSDAT-KONV                              
050500*          LÄGG IN ÅRTUSENDET                                             
050600       IF  WS-TIERSDAT-2AA  > 50                                          
050700         MOVE 19 TO   WS-TIERSDAT-1AA                                     
050800       ELSE                                                               
050900         MOVE 20 TO   WS-TIERSDAT-1AA                                     
051000       END-IF                                                             
051100     ELSE                                                                 
051200       MOVE ZERO TO WS-TIERSDAT-KONV                                      
051300     END-IF                                                               
051400                                                                          
051500     .                                                                    
051600     EJECT                                                                
051700                                                                          
051800 S11-SKRIV-W33553 SECTION.                                                
051900     SKIP2                                                                
052000     IF UT-IDPTYP = '550'                                                 
052100       WRITE UT-POST FROM UT-AREA                                         
052200     ELSE                                                                 
052300       IF UT-IDPTYP = '551'                                               
052400         WRITE UT1-POST FROM UT-AREA                                      
052500       ELSE                                                               
052600         IF UT-IDPTYP = '552'                                             
052700           WRITE UT2-POST FROM UT-AREA                                    
052800         ELSE                                                             
052900           IF UT-IDPTYP = '553'                                           
053000             WRITE UT3-POST FROM UT-AREA                                  
053100           ELSE                                                           
053200             IF UT-IDPTYP = '554'                                         
053300               WRITE UT4-POST FROM UT-AREA                                
053400             ELSE                                                         
053500               IF UT-IDPTYP = '555'                                       
053600                 WRITE UT5-POST FROM UT-AREA                              
053700               ELSE                                                       
053800                 IF UT-IDPTYP = '556'                                     
053900                   WRITE UT6-POST FROM UT-AREA                            
054000                 END-IF                                                   
054100               END-IF                                                     
054200             END-IF                                                       
054300           END-IF                                                         
054400         END-IF                                                           
054500       END-IF                                                             
054600     END-IF                                                               
054700                                                                          
054800     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
054900     MOVE 'W33553' TO POSTSUM-FDNAMN                                      
055000     MOVE 'W33552D1' TO POSTSUM-DDNAMN2                                   
055100     CALL POSTSUM USING POSTSUM-PARM                                      
055200     .                                                                    
055300     EJECT                                                                
055400* --- IMS SEKTIONER ---                                                   
055500     SKIP3                                                                
055600 IMS-GN-WLARTC SECTION.                                                   
055700     SKIP2                                                                
055800     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
055900     CALL CBLTDLI USING GN ARTC-PCB DLI-IO-AREA                           
056000     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
056100     PERFORM IMS-STATUSKONTROLL                                           
056200     .                                                                    
056300     EJECT                                                                
056400 IMS-GU-WDD701 SECTION.                                                   
056500                                                                          
056600     STRING 'WLERSA01(IDARTNR  =' W-IDARTNR-X ')'                         
056700          DELIMITED BY SIZE INTO SSA1                                     
056800     MOVE '  GE' TO GODK-STATUSKODER                                      
056900     CALL CBLTDLI USING GU ERSA-PCB DLI-IO-AREA2 SSA1                     
057000     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
057100     PERFORM IMS-STATUSKONTROLL                                           
057200     .                                                                    
057300     SKIP2                                                                
057400 IMS-GNP-WDD702-FIRST SECTION.                                            
057500     MOVE 'WLERSA11*F' TO SSA1                                            
057600     MOVE '  GE' TO GODK-STATUSKODER                                      
057700     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA2 SSA1                    
057800     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
057900     PERFORM IMS-STATUSKONTROLL                                           
058000     .                                                                    
058100     EJECT                                                                
058200 IMS-GNP-WDD702 SECTION.                                                  
058300                                                                          
058400     MOVE 'WLERSA11 ' TO SSA1                                             
058500     MOVE '  GE' TO GODK-STATUSKODER                                      
058600     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA2 SSA1                    
058700     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
058800     PERFORM IMS-STATUSKONTROLL                                           
058900     .                                                                    
059000     EJECT                                                                
059100 IMS-GNP-WDD704 SECTION.                                                  
059200     DISPLAY 'BERSA13'                                                    
059300     MOVE 'WLERSA13 ' TO SSA1                                             
059400     MOVE '  GE' TO GODK-STATUSKODER                                      
059500     CALL CBLTDLI USING GNP ERSA-PCB DLI-IO-AREA SSA1                     
059600     MOVE ERSA-STATUS-CODE TO STATUS-WS                                   
059700     PERFORM IMS-STATUSKONTROLL                                           
059800     DISPLAY 'SERSA13'                                                    
059900     .                                                                    
060000     EJECT                                                                
060100 IMS-GU-WDK601 SECTION.                                                   
060200                                                                          
060300     STRING 'WDK601  (IDARTNR  =' W-WDK6-IDARTNR-X ')'                    
060400             DELIMITED BY SIZE INTO SSA1                                  
060500     MOVE '  GE' TO GODK-STATUSKODER                                      
060600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
060700     MOVE WDK6-STATUS-CODE  TO STATUS-WS                                  
060800     PERFORM IMS-STATUSKONTROLL                                           
060900     .                                                                    
061000     EJECT                                                                
061100 IMS-GNP-WDK611 SECTION.                                                  
061200                                                                          
061300     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
061400          DELIMITED BY SIZE INTO SSA1                                     
061500     MOVE '  GE'             TO GODK-STATUSKODER                          
061600     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
061700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
061800     PERFORM IMS-STATUSKONTROLL                                           
061900     .                                                                    
062000     EJECT                                                                
062100                                                                          
062200 IMS-STATUSKONTROLL SECTION.                                              
062300     SKIP2                                                                
062400     SET STATUS-IX TO 1                                                   
062500     SEARCH GODK-STATUS                                                   
062600       AT END                                                             
062700         STRING 'FELAKTIG-STATUSKOD FRÅN IMS : ' STATUS-WS                
062800         DELIMITED BY SIZE INTO FELTEXT-STR                               
062900         DISPLAY FELTEXT                                                  
063000         CALL FELLOG                                                      
063100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
063200         CONTINUE                                                         
063300     END-SEARCH                                                           
063400     .                                                                    
