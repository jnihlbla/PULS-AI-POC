000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2254200.                                                
000400*AUTHOR.         STEFAN KIHLBERG.                                         
000500*DATE-WRITTEN.   93/10/20.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER FIL SORTERAD PER ANSKAFFARE MED RESTORDERSALDO OCH         
001100*        AK-SALDO PER ARTIKEL. BERÄKNAR HUR MÅNGA ARTIKLAR MED            
001200*        RESTORDER VARJE ANSKAFFARE MED RESORDERARTIKLAR HAR. HUR         
001300*        MÅNGA AV DESSA RESTORDER SOM TÄCKS AV AK, HUR MÅNGA SOM          
001400*        TÄCKS AV AK + LEVERANSBESKED SAMT HUR MÅNGA SOM INTE             
001500*        TÄCKS.                                                           
001600*        SKRIVER LISTFIL.                                                 
001700*                                                                         
001800*        PROGRAMMET LÄSER      WLINLB (WDD9)                              
001900*                                                                         
002000*    ABENDKODER:                                                          
002100*        U0016 -  . . . .                                                 
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*          --- ARTIKLAR MED RESORDER, SORTERADE PER ANSKAFFARE            
003300     SELECT W22541                     ASSIGN TO W22542D1.                
003400     SKIP2                                                                
003500*          --- RESTORDER TÄCKTA/EJ TÄCKTA, PER ANSKAFFARE                 
003600     SELECT W22543                     ASSIGN TO W22542D2.                
003700*          --- RESTORDER TÄCKTA/EJ TÄCKTA, VARJE ARTIKEL                  
003800     SELECT W22542                     ASSIGN TO W22542D3.                
003900     EJECT                                                                
004000 DATA DIVISION.                                                           
004100     SKIP3                                                                
004200 FILE SECTION.                                                            
004300     SKIP3                                                                
004400 FD  W22541                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700     SKIP2                                                                
004800*01  -COPY W22541      -L.                                                
004900     SKIP3                                                                
005000 FD  W22542                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300     SKIP2                                                                
005400*01  POST -COPY W22542 -PRE W22542-    -L.                                
005500     SKIP3                                                                
005600 FD  W22543                                                               
005700     RECORDING       F                                                    
005800     BLOCK CONTAINS  0.                                                   
005900     SKIP2                                                                
006000*01  POST -COPY W22543 -PRE  W22543-  -L.                                 
006100     EJECT                                                                
006200 WORKING-STORAGE SECTION.                                                 
006300     SKIP2                                                                
006400                                                                          
006500*    -- CHECKED BY WY2000                                                 
006600 77  IDPGM                       PIC X(8)    VALUE 'W2254200'.            
006700 77  JA                          PIC X       VALUE 'J'.                   
006800 77  NEJ                         PIC X       VALUE 'N'.                   
006900                                                                          
006910*01  -COPY WWDCKONS                                                       
006920                                                                          
007000 77  W22541-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-W22541                       VALUE 'J'.                   
007200                                                                          
007300 77  ROS-TACKT-AV-AKS-SW         PIC X       VALUE 'N'.                   
007400     88  ROS-TACKT-AV-AKS                    VALUE 'J'.                   
007500                                                                          
007600 77  ROS-TACKT-AV-AKS-LEVB-SW    PIC X       VALUE 'N'.                   
007700     88  ROS-TACKT-AV-AKS-LEVB               VALUE 'J'.                   
007800                                                                          
007900     EJECT                                                                
008000                                                                          
008100 01  ARBETSFALT.                                                          
008200     03  WS-IDARTNR              PIC S9(9)   VALUE ZERO  COMP-3.          
008300     03  SPAR-IDANSK             PIC S9(3)   VALUE ZERO  COMP-3.          
008400     03  WS-REST-1-ART           PIC S9(7)   VALUE ZERO  COMP-3.          
008500     03  WS-REST-2-ART           PIC S9(7)   VALUE ZERO  COMP-3.          
008600     03  WS-BSKKVAR-ART          PIC S9(7)   VALUE ZERO  COMP-3.          
008700     03  ANTAL-RO-ANSK           PIC S9(7)   VALUE ZERO  COMP-3.          
008800     03  ANTAL-M-AK-ANSK         PIC S9(7)   VALUE ZERO  COMP-3.          
008900     03  ANTAL-M-AK-LEVB-ANSK    PIC S9(7)   VALUE ZERO  COMP-3.          
009000     03  ANTAL-EJ-TACKTA-ANSK    PIC S9(7)   VALUE ZERO  COMP-3.          
009100     03  ANTAL-M-TEXTB           PIC S9(7)   VALUE ZERO  COMP-3.          
009200                                                                          
009300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009400 01  FILLER REDEFINES DAGENS-DATUM.                                       
009500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009800     EJECT                                                                
009900 01  DYNAMISKA-SUBPROGRAM.                                                
010000*                                                                         
010100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010500     SKIP2                                                                
010600*    --- PARAMETRAR TILL ABEND                                            
010700                                                                          
010800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011000     SKIP2                                                                
011100 01  FELTEXT.                                                             
011200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011400     EJECT                                                                
011500*    --- PARAMETRAR TILL POSTSUM                                          
011600*                                                                         
011700*01  -COPY W0005   -PRE  POSTSUM-                                         
011800     EJECT                                                                
011900 01  W22541-AREA-START           PIC X(24)   VALUE                        
012000                                 'W22541-AREA-START  '.                   
012100     SKIP2                                                                
012200                                                                          
012300*01  AREA -COPY W22541     -PRE W22541-                                   
012400     EJECT                                                                
012500 01  W22542-AREA-START           PIC X(24)   VALUE                        
012600                                 'W22542-AREA-START  '.                   
012700     SKIP2                                                                
012800*01  AREA -COPY W22542     -PRE W22542-                                   
012900     EJECT                                                                
013000 01  W22543-AREA-START           PIC X(24)   VALUE                        
013100                                 'W22543-AREA-START  '.                   
013200     SKIP2                                                                
013300                                                                          
013400*01  AREA -COPY W22543     -PRE W22543-                                   
013500     EJECT                                                                
013600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
013700*                                                                         
013800     EJECT                                                                
013900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
014000     SKIP3                                                                
014100 01  NYCKLAR-TILL-DLI.                                                    
014200     03  W-WDD901KY-X.                                                    
014300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
014400         05  W-IDDC              PIC X(2)   VALUE SPACE.                  
014500     03  W-IDLEVNR-X.                                                     
014600         05  W-IDLEVNR           PIC X(5)   VALUE SPACE.                  
014700     03  W-TILEVBSK-X.                                                    
014800         05  W-TILEVBSK          PIC S9(7)   VALUE ZERO COMP-3.           
014900     03  W-IDLEVBSK-X.                                                    
015000         05  W-IDLEVBSK          PIC S9(1)   VALUE +2   COMP-3.           
015100     SKIP2                                                                
015200*    --- STATUS-KOD FRÅN IMS                                              
015300 01  STATUS-WS                   PIC XX.                                  
015400     88  SEGMENT-FINNS                       VALUE '  '.                  
015500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015700     SKIP2                                                                
015800 01  GODK-STATUSKODER.                                                    
015900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016000     SKIP3                                                                
016100 01  SSA1                        PIC X(64).                               
016200 01  SSA2                        PIC X(64).                               
016300     EJECT                                                                
016400*    --- IMS FUNKTIONSKODER                                               
016500*01  -COPY W0003                                                          
016600     EJECT                                                                
016700*    ---  DLI INPUT-OUTPUT AREA                                           
016800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016900     SKIP3                                                                
017000 01  DLI-IO-AREA.                                                         
017100     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
017200     SKIP3                                                                
017300     03  WLINLB01 REDEFINES IO-AREA.                                      
017400*        05  -COPY WDD901  -PRE WDD901-                                   
017500     SKIP3                                                                
017600     03  WLINLB11 REDEFINES IO-AREA.                                      
017700*        05  -COPY WDD902  -PRE WDD902-                                   
017800     EJECT                                                                
017900     03  WLINLB24 REDEFINES IO-AREA.                                      
018000*        05  -COPY WDD924  -PRE WDD924-                                   
018100     EJECT                                                                
018200     03  WLINLB25 REDEFINES IO-AREA.                                      
018300*        05  -COPY WDD925  -PRE WDD925-                                   
018400     EJECT                                                                
018500 LINKAGE SECTION.                                                         
018600                                                                          
018700     EJECT                                                                
018800*01  -COPY W0008  -PRE INLB-                                              
018900     05  FILLER                  PIC X.                                   
019000     EJECT                                                                
019100 PROCEDURE DIVISION  USING INLB-PCB.                                      
019200     ENTRY 'DLITCBL' USING INLB-PCB.                                      
019300                                                                          
019400     SKIP2                                                                
019500     PERFORM A-INIT                                                       
019600     PERFORM S01-LAES-W22541                                              
019700     MOVE W22541-IDANSK TO SPAR-IDANSK                                    
019800     PERFORM UNTIL END-OF-W22541                                          
019900        PERFORM UNTIL END-OF-W22541 OR                                    
020000                      W22541-IDANSK NOT = SPAR-IDANSK                     
020100           ADD +1 TO ANTAL-RO-ANSK                                        
020200           PERFORM B-TACKS-ROS-AV-AKS                                     
020300           IF ROS-TACKT-AV-AKS                                            
020400              ADD +1 TO ANTAL-M-AK-ANSK                                   
020500              PERFORM G-TAECKT-RO                                         
020600           ELSE                                                           
020700              PERFORM C-TACKS-ROS-AV-AKS-PLUS-LEVB                        
020800              IF ROS-TACKT-AV-AKS-LEVB                                    
020900                 ADD +1 TO ANTAL-M-AK-LEVB-ANSK                           
021000                 PERFORM G-TAECKT-RO                                      
021100              ELSE                                                        
021200                 PERFORM IMS-GET-WDD901                                   
021300                 IF SEGMENT-FINNS                                         
021400                    PERFORM IMS-GET-WDD902-WDD925                         
021500                    IF SEGMENT-FINNS                                      
021600                       ADD +1 TO ANTAL-M-TEXTB                            
021700                       PERFORM G-TAECKT-RO                                
021800                    ELSE                                                  
021900                       ADD +1 TO ANTAL-EJ-TACKTA-ANSK                     
022000                       PERFORM H-EJ-TAECKT-RO                             
022100                    END-IF                                                
022200                 ELSE                                                     
022300                    ADD +1 TO ANTAL-EJ-TACKTA-ANSK                        
022400                    PERFORM H-EJ-TAECKT-RO                                
022500                 END-IF                                                   
022600              END-IF                                                      
022700           END-IF                                                         
022800           PERFORM S01-LAES-W22541                                        
022900           PERFORM D-NOLLSTALL-ARTIKEL-FALT                               
023000        END-PERFORM                                                       
023100        PERFORM F-FLYTTA-TILL-UTFIL                                       
023200        PERFORM S11-SKRIV-W22543                                          
023300        PERFORM E-NOLLSTALL-ANSK-FALT                                     
023400     END-PERFORM                                                          
023500                                                                          
023600                                                                          
023700     PERFORM Z-FINIT                                                      
023800                                                                          
023900     MOVE ZERO TO RETURN-CODE                                             
024000     GOBACK                                                               
024100     .                                                                    
024200     EJECT                                                                
024300 A-INIT SECTION.                                                          
024400                                                                          
024500     OPEN INPUT  W22541                                                   
024600                                                                          
024700     OPEN OUTPUT W22543 W22542                                            
024800     SKIP2                                                                
024900     ACCEPT DAGENS-DATUM  FROM DATE                                       
025000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
025100     .                                                                    
025200     EJECT                                                                
025300                                                                          
025400                                                                          
025500 B-TACKS-ROS-AV-AKS SECTION.                                              
025600                                                                          
025700     COMPUTE WS-REST-1-ART =                                              
025800             W22541-KVROS-ART - W22541-KVAKS-ART                          
025900                              - W22541-KVLS-DISP-ART                      
026000     IF WS-REST-1-ART NOT > ZERO                                          
026100        MOVE JA TO ROS-TACKT-AV-AKS-SW                                    
026200     END-IF                                                               
026300     .                                                                    
026400     EJECT                                                                
026500                                                                          
026600                                                                          
026700 C-TACKS-ROS-AV-AKS-PLUS-LEVB SECTION.                                    
026800                                                                          
026900     PERFORM CA-SUMMERA-LEVBESK                                           
027000     COMPUTE WS-REST-2-ART = WS-REST-1-ART - WS-BSKKVAR-ART               
027100     IF WS-REST-2-ART NOT > ZERO                                          
027200        MOVE JA TO ROS-TACKT-AV-AKS-LEVB-SW                               
027300     END-IF                                                               
027400     .                                                                    
027500     EJECT                                                                
027600                                                                          
027700                                                                          
027800 CA-SUMMERA-LEVBESK SECTION.                                              
027900                                                                          
028000     MOVE W22541-IDARTNR TO W-IDARTNR                                     
028100     MOVE WC-CDC-SE      TO W-IDDC                                        
028200     PERFORM IMS-GET-WDD901                                               
028300     IF SEGMENT-FINNS                                                     
028400        PERFORM IMS-GET-WDD902-WDD924                                     
028500        PERFORM UNTIL SEGMENT-SAKNAS                                      
028600                                                                          
028700           ADD WDD924-LEV-KVAVIS-BSKKVAR    TO WS-BSKKVAR-ART             
028800           PERFORM IMS-GET-WDD902-WDD924                                  
028900        END-PERFORM                                                       
029000     END-IF                                                               
029100     .                                                                    
029200     EJECT                                                                
029300                                                                          
029400 D-NOLLSTALL-ARTIKEL-FALT SECTION.                                        
029500                                                                          
029600     MOVE ZERO                  TO WS-REST-1-ART                          
029700                                   WS-REST-2-ART                          
029800                                   WS-BSKKVAR-ART                         
029900     MOVE NEJ                   TO ROS-TACKT-AV-AKS-SW                    
030000                                   ROS-TACKT-AV-AKS-LEVB-SW               
030100     .                                                                    
030200     EJECT                                                                
030300                                                                          
030400                                                                          
030500 E-NOLLSTALL-ANSK-FALT SECTION.                                           
030600                                                                          
030700     MOVE W22541-IDANSK         TO SPAR-IDANSK                            
030800     MOVE ZERO                  TO ANTAL-RO-ANSK                          
030900                                   ANTAL-M-AK-ANSK                        
031000                                   ANTAL-M-AK-LEVB-ANSK                   
031100                                   ANTAL-EJ-TACKTA-ANSK                   
031200                                   ANTAL-M-TEXTB                          
031300     .                                                                    
031400     EJECT                                                                
031500                                                                          
031600                                                                          
031700 F-FLYTTA-TILL-UTFIL SECTION.                                             
031800                                                                          
031900     MOVE SPAR-IDANSK              TO W22543-IDANSK                       
032000     MOVE ANTAL-RO-ANSK            TO W22543-KVANTAL-RO                   
032100     MOVE ANTAL-M-AK-ANSK          TO W22543-KVANTAL-M-AK                 
032200     MOVE ANTAL-M-AK-LEVB-ANSK     TO W22543-KVANTAL-M-AK-LEVB            
032300     MOVE ANTAL-EJ-TACKTA-ANSK     TO W22543-KVANTAL-EJ-TACKTA            
032400     MOVE ANTAL-M-TEXTB            TO W22543-KVANTAL-M-TEXTB              
032500     .                                                                    
032600     EJECT                                                                
032700 G-TAECKT-RO SECTION.                                                     
032800     MOVE W22541-IDARTNR           TO W22542-IDARTNR                      
032900     MOVE 1                        TO W22542-FLRO                         
033000     MOVE 1                        TO W22542-FLRO-TAECK                   
033100     PERFORM S12-SKRIV-W22542                                             
033200     .                                                                    
033300     EJECT                                                                
033400 H-EJ-TAECKT-RO SECTION.                                                  
033500     MOVE W22541-IDARTNR           TO W22542-IDARTNR                      
033600     MOVE 1                        TO W22542-FLRO                         
033700     MOVE 0                        TO W22542-FLRO-TAECK                   
033800     PERFORM S12-SKRIV-W22542                                             
033900     .                                                                    
034000     EJECT                                                                
034100 Z-FINIT SECTION.                                                         
034200     CLOSE W22541                                                         
034300           W22543                                                         
034400           W22542                                                         
034500     SKIP2                                                                
034600     MOVE 'S' TO POSTSUM-OPKOD                                            
034700     CALL POSTSUM USING POSTSUM-PARM                                      
034800     .                                                                    
034900     EJECT                                                                
035000 S01-LAES-W22541  SECTION.                                                
035100     SKIP2                                                                
035200     READ W22541 INTO W22541-AREA                                         
035300     AT END                                                               
035400        SET END-OF-W22541 TO TRUE                                         
035500                                                                          
035600     NOT AT END                                                           
035700        MOVE 'W22541' TO POSTSUM-FDNAMN                                   
035800        MOVE 'W22542D1' TO POSTSUM-DDNAMN2                                
035900        CALL POSTSUM USING POSTSUM-PARM                                   
036000     END-READ                                                             
036100     .                                                                    
036200     EJECT                                                                
036300 S11-SKRIV-W22543 SECTION.                                                
036400     SKIP2                                                                
036500     WRITE W22543-POST FROM W22543-AREA                                   
036600                                                                          
036700     MOVE 'W22543' TO POSTSUM-FDNAMN                                      
036800     MOVE 'W22542D2' TO POSTSUM-DDNAMN2                                   
036900     CALL POSTSUM USING POSTSUM-PARM                                      
037000     .                                                                    
037100     EJECT                                                                
037200 S12-SKRIV-W22542 SECTION.                                                
037300     SKIP2                                                                
037400     WRITE W22542-POST FROM W22542-AREA                                   
037500                                                                          
037600     MOVE 'W22542' TO POSTSUM-FDNAMN                                      
037700     MOVE 'W22542D3' TO POSTSUM-DDNAMN2                                   
037800     CALL POSTSUM USING POSTSUM-PARM                                      
037900     .                                                                    
038000     EJECT                                                                
038100*S99-ABEND SECTION.                                                       
038200*    SKIP2                                                                
038300*    MOVE 'S' TO POSTSUM-OPKOD                                            
038400*    CALL POSTSUM USING POSTSUM-PARM                                      
038500*    CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
038600*    .                                                                    
038700     EJECT                                                                
038800* --- IMS SEKTIONER ---                                                   
038900     SKIP3                                                                
039000     EJECT                                                                
039100 IMS-GET-WDD901 SECTION.                                                  
039200     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
039300          DELIMITED BY SIZE INTO SSA1                                     
039400     MOVE '  GE' TO GODK-STATUSKODER                                      
039500     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA SSA1                      
039600     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
039700     PERFORM IMS-STATUSKONTROLL                                           
039800     .                                                                    
039900     EJECT                                                                
040000 IMS-GET-WDD902-WDD924 SECTION.                                           
040100     MOVE 'WLINLB11 ' TO SSA1                                             
040200     MOVE 'WLINLB24 ' TO SSA2                                             
040300     MOVE '  GEGAGK' TO GODK-STATUSKODER                                  
040400     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1 SSA2                
040500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
040600     PERFORM IMS-STATUSKONTROLL                                           
040700     .                                                                    
040800     EJECT                                                                
040900 IMS-GET-WDD902-WDD925 SECTION.                                           
041000     MOVE 'WLINLB11 ' TO SSA1                                             
041100     STRING 'WLINLB25(IDLEVBSK =' W-IDLEVBSK-X ')'                        
041200          DELIMITED BY SIZE INTO SSA2                                     
041300     MOVE '  GEGAGK' TO GODK-STATUSKODER                                  
041400     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA SSA1 SSA2                
041500     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
041600     PERFORM IMS-STATUSKONTROLL                                           
041700     .                                                                    
041800     EJECT                                                                
041900 IMS-STATUSKONTROLL SECTION.                                              
042000     SKIP2                                                                
042100     SET STATUS-IX TO 1                                                   
042200     SEARCH GODK-STATUS                                                   
042300       AT END                                                             
042400         MOVE STATUS-WS        TO FELTEXT-STR                             
042500         DISPLAY FELTEXT                                                  
042600         CALL FELLOG                                                      
042700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
042800         CONTINUE                                                         
042900     END-SEARCH                                                           
043000     .                                                                    
