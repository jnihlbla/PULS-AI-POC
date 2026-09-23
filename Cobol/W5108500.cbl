000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5108500.                                                
000300 AUTHOR.         ARCHANA BHAT.                                            
000400 DATE-WRITTEN.   16/04/29.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        THIS PROGRAM CHECKS IF THERE IS A VALID PRICE ON WDK724          
000900*        FOR CHINA AND USA SUPPLIER AND SENDS TO 5111 SCREEN              
001000*        IT READS A FILE WHICH HAS DETAILS OF PARTS                       
001100*        WHICH HAVE SUPPLIER AND DC                                       
001200*                                                                         
001300*        THE PROGRAM READS     WDK6                                       
001400*                              WDK7                                       
001500*        PROGRAMS CALLS 5111                                              
001600*                                                                         
001700*    ABENDCODES:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
002900*          --- PARTS WITH CHINA AND USA SUPPLIERS                         
003000     SELECT W51085                     ASSIGN TO W51085D1.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W51085                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  -COPY W21350      -L.                                                
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400 77  IDPGM                       PIC X(8)    VALUE 'W5108500'.            
004500 77  YES                         PIC X       VALUE 'J'.                   
004600 77  NOO                         PIC X       VALUE 'N'.                   
004700                                                                          
004800 77  W51085-EOF-SW               PIC X       VALUE 'N'.                   
004900     88  END-OF-W51085                       VALUE 'J'.                   
005000     EJECT                                                                
005100 77  PRICE-FOUND-SW              PIC X       VALUE 'N'.                   
005200     88  PRICE-FOUND                         VALUE 'J'.                   
005300     EJECT                                                                
005400 77  WS-REEMBHNT                 PIC S9(2)V9(3)  VALUE ZERO.              
005500 77  WS-RELANDCO-EXP             PIC S9(3)V9(3) COMP-3.                   
005600 77  WS-IDARTNR                  PIC X(9) VALUE SPACES.                   
005700 77  WS-RETULF-SEND              PIC 9(7).                                
005800 77  WS-PRARTBES                 PIC 9(13).                               
005900 77  WS-TIPRLIST-6               PIC  9(6).                               
006000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES TODAYS-DATE.                                        
006200     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006300     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006400     03  TODAYS-DATE-DAY         PIC 9(2).                                
006500     EJECT                                                                
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700*                                                                         
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200     03  W510PRTR                PIC X(8)    VALUE 'W510PRTR'.            
007300     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
007400     EJECT                                                                
007500 01  WS-VARIABLES.                                                        
007600     03 DAGENS-AAMMDD            PIC 9(6).                                
007700     03 W-DAPRLIST-MAX           PIC 9(8)    VALUE 99999999.              
007800     03 W-DAPRLIST               PIC 9(8).                                
007900     03 W-DAPRLIST-X  REDEFINES W-DAPRLIST.                               
008000       05 FILLER                 PIC 9(2).                                
008100       05 W-LISTDATUM            PIC 9(6).                                
008200     03  W-PRARTBEL              PIC S9(8)V9(5) VALUE +0.                 
008300     03  W-RETULF                PIC S9(3)V9(4) VALUE +0.                 
008400     03  W-PRARTBES              PIC S9(7)V9(2) VALUE +0   COMP-3.        
008500     03  W-KDFPKPRI              PIC X(1)       VALUE SPACES.             
008600     03  W-TIPRLIST              PIC 9(6).                                
008700     SKIP2                                                                
008800*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008900                                                                          
009000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009300     SKIP2                                                                
009400 01  ERROR-TEXT.                                                          
009500     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
009600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009700     EJECT                                                                
009800*    --- PARAMETRAR TILL POSTSUM                                          
009900*                                                                         
010000*01  -COPY W0005   -PRE  POSTSUM-                                         
010100     EJECT                                                                
010200 01  FILLER                      PIC X(8)    VALUE 'W510PRTR'.            
010300*01  -COPY W510PRTR                                                       
010400     EJECT                                                                
010500 01  TEST-IDDISTR        PIC 9(5)        COMP-3.                          
010600*01  FILLER  -COPY WWDIST35  -RED TEST-IDDISTR.                           
010700                                                                          
010800 01  IN-AREA-START               PIC X(24)   VALUE                        
010900                                 'IN-AREA-START  '.                       
011000     SKIP2                                                                
011100                                                                          
011200*01  AREA -COPY W21350     -PRE IN-                                       
011300     EJECT                                                                
011400*01 -COPY WWDC03                                                          
011500*                                                                         
011600 01  WS-IDDC-SEND-REC.                                                    
011700     03  WS-IDDC-SEND            PIC X(2).                                
011800     03  WS-IDDC-REC             PIC X(2).                                
011900*    --- AREAS FOR IMS-SECTIONS                                           
012000*                                                                         
012100     EJECT                                                                
012200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012300     SKIP3                                                                
012400 01  KEYS-FOR-DLI.                                                        
012500     03  W-IDARTNR-X.                                                     
012600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
012700     03  W-IDLEVNR-X.                                                     
012800         05    W-IDLEVNR         PIC X(5)    VALUE SPACE.                 
012900     03  W-IDLEVNR-B6-X.                                                  
013000         05  W-IDLEVNR-B6        PIC X(5)    VALUE SPACE.                 
013100     03  W-IDLAND-X.                                                      
013200         05    W-IDLAND          PIC X(2)    VALUE SPACE.                 
013300     03  W-IDGMT-MIN-X.                                                   
013400         05  W-IDDISTR-MIN       PIC S9(5)   VALUE ZERO COMP-3.           
013500         05  W-IDKUNDNR-MIN      PIC S9(7)   VALUE ZERO COMP-3.           
013600     SKIP2                                                                
013700     03  W-IDGMT-MAX-X.                                                   
013800         05  W-IDDISTR-MAX       PIC S9(5)   VALUE ZERO COMP-3.           
013900         05  W-IDKUNDNR-MAX      PIC S9(7)  VALUE +9999999 COMP-3.        
014000     03  W-IDLANDX2-X.                                                    
014100         05    W-IDLANDX2        PIC X(2)    VALUE SPACE.                 
014200     03  W-IDDC-X.                                                        
014300         05 W-IDDC               PIC X(2).                                
014400     SKIP2                                                                
014500*    --- STATUS-KOD FRÅN IMS                                              
014600 01  STATUS-WS                   PIC XX.                                  
014700     88  SEGMENT-FOUND                       VALUE '  '.                  
014800     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
014900     88  SEGMENT-MISSING                     VALUE 'GE'.                  
015000     SKIP2                                                                
015100 01  GOOD-STATUSCODES.                                                    
015200     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
015300     SKIP3                                                                
015400 01  SSA1                        PIC X(64).                               
015500 01  SSA2                        PIC X(64).                               
015600     EJECT                                                                
015700*    --- IMS FUNCTION CODES                                               
015800*01  -COPY W0003                                                          
015900     EJECT                                                                
016000*    ---  DLI INPUT-OUTPUT AREA                                           
016100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
016200 01  DLI-IO-WDK601.                                                       
016300*    03  -COPY WDK601                                                     
016400     EJECT                                                                
016500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
016600 01  DLI-IO-WDK611.                                                       
016700*    03  -COPY WDK611                                                     
016800     EJECT                                                                
016900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
017000 01   DLI-IO-AREA-B601.                                                   
017100*     03  -COPY WDB601                                                    
017200     EJECT                                                                
017300 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
017400 01   DLI-IO-AREA-B617.                                                   
017500*     03  -COPY WDB617                                                    
017600     EJECT                                                                
017700 01  FILLER               PIC X(16)   VALUE 'WDK711 AREA'.                
017800 01   DLI-IO-AREA-K711.                                                   
017900*     03  -COPY WDK711                                                    
018000     EJECT                                                                
018100 01  FILLER               PIC X(16)   VALUE 'WDK724 AREA'.                
018200 01   DLI-IO-AREA-K724.                                                   
018300*     03  -COPY WDK724                                                    
018400     EJECT                                                                
018500 01  FILLER                      PIC X(16)   VALUE 'WDB201-AREA'.         
018600 01  DLI-IO-AREA-WDB2.                                                    
018700*    03  -COPY WDB201  -PRE WDB2-                                         
018800     EJECT                                                                
018900 01  FILLER                    PIC X(16)  VALUE 'WDF101'.                 
019000*01  WLLEVA01 -COPY WDF101                                                
019100     EJECT                                                                
019200                                                                          
019300 01  FILLER                    PIC X(16)  VALUE 'WDF102'.                 
019400*01  WLLEVA11 -COPY WDF102 -PRE LEV-                                      
019500     EJECT                                                                
019600                                                                          
019700*    ---  MSG INPUT-OUTPUT AREA                                           
019800 01  FILLER                      PIC X(16)   VALUE 'MSG-IO-AREA'.         
019900*01  -COPY WMSGAREA                                                       
020000*    --- AREA FÖR KOMMUNIKATION MED DISPATCHER                            
020100 01  FILLER                 PIC X(16)   VALUE 'KOM-DISP-IO-AREA'.         
020200 01  KOM-IO-AREA.                                                         
020300*  03     -COPY WMSGKOM                                                   
020400*                                                                         
020500 01  FILLER                 PIC X(16)   VALUE 'KOM-IO-AREA2'.             
020600 01  KOM-IO-AREA2.                                                        
020700*  03      -COPY W5I11101 -PRE 5111-                                      
020800     EJECT                                                                
020900                                                                          
021000                                                                          
021100 LINKAGE SECTION.                                                         
021200                                                                          
021300*01  -COPY W0009     -PRE MSG-                                            
021400     EJECT                                                                
021500                                                                          
021600*01  -COPY W0009     -PRE ALT-                                            
021700     EJECT                                                                
021800                                                                          
021900*01  -COPY W0008     -PRE KOMA-                                           
022000     05  FILLER                  PIC X.                                   
022100     EJECT                                                                
022200*01  -COPY W0008     -PRE WDK6-                                           
022300     05  FILLER                  PIC X.                                   
022400     EJECT                                                                
022500*01  -COPY W0008     -PRE WDK7-                                           
022600     05  FILLER                  PIC X.                                   
022700     EJECT                                                                
022800*01  -COPY W0008     -PRE WDB6-                                           
022900     05 FILLER                   PIC X.                                   
023000     EJECT                                                                
023100*01  -COPY W0008     -PRE LEV-                                            
023200     05 FILLER                   PIC X.                                   
023300     EJECT                                                                
023400*01  -COPY W0008     -PRE WDB2-                                           
023500     05 FILLER                   PIC X.                                   
023600     EJECT                                                                
023700 PROCEDURE DIVISION  USING MSG-PCB ALT-PCB KOMA-PCB                       
023800                           WDK6-PCB WDK7-PCB WDB6-PCB                     
023900                           LEV-PCB  WDB2-PCB.                             
024000 MAIN SECTION.                                                            
024100     ENTRY 'DLITCBL' USING MSG-PCB ALT-PCB KOMA-PCB                       
024200                           WDK6-PCB WDK7-PCB WDB6-PCB                     
024300                           LEV-PCB  WDB2-PCB.                             
024400                                                                          
024500     PERFORM A-INIT                                                       
024600                                                                          
024700     PERFORM S01-READ-W51085                                              
024800     PERFORM UNTIL END-OF-W51085                                          
024900       MOVE IN-IDLEVNR   TO W-IDLEVNR-B6                                  
025000       PERFORM IMS-GU-WDB601                                              
025100       IF SEGMENT-FOUND                                                   
025200         MOVE IN-IDARTNR TO WS-IDARTNR                                    
025300                            W-IDARTNR                                     
025400         MOVE DCS-IDDC   TO W-IDDC                                        
025500         PERFORM B-CALC-PRICE                                             
025600       END-IF                                                             
025700       PERFORM S01-READ-W51085                                            
025800     END-PERFORM                                                          
025900                                                                          
026000                                                                          
026100     PERFORM Z-FINIT                                                      
026200                                                                          
026300     MOVE ZERO TO RETURN-CODE                                             
026400     GOBACK                                                               
026500     .                                                                    
026600     EJECT                                                                
026700                                                                          
026800 A-INIT SECTION.                                                          
026900     OPEN INPUT  W51085                                                   
027000                                                                          
027100     ACCEPT TODAYS-DATE  FROM DATE                                        
027200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
027300     .                                                                    
027400     EJECT                                                                
027500                                                                          
027600 B-CALC-PRICE SECTION.                                                    
027700     PERFORM BA-CHECK-DISTRICT                                            
027800     MOVE DCS-IDDC               TO PRTR-IDDC                             
027900     MOVE WS-IDARTNR             TO PRTR-IDARTNR                          
028000     IF DIST35-NONVCC-REFILL                                              
028010     OR DIST35-NONVCC-NONVCC-TRANSFER                                     
028020     OR DIST35-NONVCC-VCC-TRANSFER                                        
028200       MOVE 010                    TO PRTR-KDCALL                         
028400       IF DIST35-NONVCC-NONVCC-REFILL                                     
028410       OR DIST35-NONVCC-NONVCC-TRANSFER                                   
028500         MOVE 015                    TO PRTR-KDCALL                       
028600       END-IF                                                             
028700       CALL W510PRTR USING PRTR-W510PRTR WDK6-PCB                         
028800       IF PRTR-KDSVAR = '1'                                               
028900         PERFORM BB-GET-LATEST-PRICE                                      
029000       END-IF                                                             
029100     END-IF                                                               
029200     .                                                                    
029300     EJECT                                                                
029400                                                                          
029500 BA-CHECK-DISTRICT   SECTION.                                             
029600     MOVE DCS-IDDC           TO WS-IDDC-SEND                              
029700     MOVE IN-IDDC            TO WS-IDDC-REC                               
029800* GET DISTRICT USING DC AS SENDING DC AND DC 11 AS RECIEVING DC           
029900     SEARCH ALL WWDC03-IDDISTR                                            
030000       AT END                                                             
030100         MOVE ZERO     TO W-IDDISTR-MIN                                   
030200         MOVE 'EJ TRÄFF I TABELL TEXTXX' TO ERROR-TEXT                    
030300         DISPLAY ERROR-TEXT                                               
030400         CALL FELLOG                                                      
030500       WHEN WWDC03-IDDC-SEND-REC(WWDC03-IX) = WS-IDDC-SEND-REC            
030600           MOVE WWDC03-SOK-IDDISTR(WWDC03-IX) TO W-IDDISTR-MIN            
030700                                                 W-IDDISTR-MAX            
030800     END-SEARCH                                                           
030900**************************************************************            
031000     MOVE W-IDDISTR-MIN       TO TEST-IDDISTR                             
031100     .                                                                    
031200     EJECT                                                                
031300                                                                          
031400 BB-GET-LATEST-PRICE SECTION.                                             
031500     MOVE NOO                      TO PRICE-FOUND-SW                      
031600     PERFORM IMS-GU-WDK711                                                
031700     IF SEGMENT-FOUND                                                     
031800        PERFORM IMS-GNP-WDK724-FIRST                                      
031900        PERFORM UNTIL SEGMENT-MISSING OR PRICE-FOUND                      
032000          IF SEGMENT-FOUND                                                
032100             SUBTRACT SPRL-DAPRLIST-9KOMPL FROM W-DAPRLIST-MAX            
032200             GIVING W-DAPRLIST                                            
032300             MOVE W-LISTDATUM        TO W-TIPRLIST                        
032400             MOVE SPRL-KDFPKPRI      TO W-KDFPKPRI                        
032500             MOVE SPRL-PRARTBEL-PR   TO W-PRARTBEL                        
032600             MOVE YES                TO PRICE-FOUND-SW                    
032700          END-IF                                                          
032800          PERFORM IMS-GNP-WDK724-NEXT                                     
032900       END-PERFORM                                                        
033000     END-IF                                                               
033100     IF PRICE-FOUND                                                       
033200        PERFORM C-WRITE-TRANS-5111                                        
033300        PERFORM D-START-TRANS                                             
033400     END-IF                                                               
033500     .                                                                    
033600     EJECT                                                                
033700                                                                          
033800 C-WRITE-TRANS-5111 SECTION.                                              
033900     MOVE   SPACE                 TO   MSG-KOM-WMSGKOM                    
034000     MOVE   +54                   TO   MSG-KOM-KVLL                       
034100     MOVE   LOW-VALUE             TO   MSG-KOM-KDZ1                       
034200     MOVE   LOW-VALUE             TO   MSG-KOM-KDZ2                       
034300     MOVE   SPACE                 TO   MSG-KOM-KDTRANS                    
034400     MOVE   'IPRIS'               TO   MSG-KOM-IDSNDNOD                   
034500     MOVE   'W5108500'            TO   MSG-KOM-IDSNDJOB                   
034600     MOVE   'W5I11101'            TO   MSG-KOM-IDCPYTXT                   
034700                                                                          
034800     MOVE   SPACE                 TO   MSG-KOM-IDMFSMED                   
034900                                       MSG-KOM-KDSVAR                     
035000     ACCEPT MSG-KOM-TIREGDAT  FROM DATE                                   
035100     ACCEPT MSG-KOM-TIKLOCK   FROM TIME                                   
035200                                                                          
035300     COMPUTE MSG-KVLL = LENGTH OF 5111-W5I11101 + 17                      
035400     MOVE LOW-VALUE               TO MSG-KDZ1                             
035500     MOVE LOW-VALUE               TO MSG-KDZ2                             
035600     MOVE 'W5T111X'               TO MSG-KDTRANS-1                        
035700     MOVE '5111'                  TO MSG-IDTRANS-1                        
035800     MOVE '1'                     TO MSG-KDMFSFOR-1                       
035900     MOVE WS-IDARTNR              TO 5111-IDARTNR-IN                      
036000                                                                          
036100****************** DETTA ÄR INLAGT BARA FÖR ATT FYLLA MIDDEN              
036200     MOVE '+++++'                 TO 5111-IDLEVNR-IN                      
036300     MOVE '+'                     TO 5111-KDPRBEH-IN                      
036400     MOVE '++++++'                TO 5111-REAENDR-IN                      
036500                                                                          
036600     MOVE SPACE                   TO 5111-IDLEVNR-UT                      
036700                                     5111-REAENDR-UT                      
036800     MOVE SPACE                   TO 5111-KDPRBEH-UT                      
036900************************************************************              
037000                                                                          
037100     MOVE 'C'                     TO 5111-KDPRURSP-U                      
037200     MOVE MSG-SIGNON-USERID       TO 5111-IDUSER                          
037300                                                                          
037400     MOVE W-TIPRLIST              TO WS-TIPRLIST-6                        
037500     MOVE WS-TIPRLIST-6           TO 5111-TIPRLIST-U                      
037600                                                                          
037700     PERFORM CA-GET-REEMBHNT                                              
037800     PERFORM CB-GET-RELANDCO-EXP                                          
037900     PERFORM CC-GET-RETULF                                                
038000     PERFORM CD-GET-PRARTBES                                              
038100                                                                          
038200     MOVE WS-PRARTBES(1:8)        TO 5111-PRARTBEL-U(1:8)                 
038300     MOVE '.'                     TO 5111-PRARTBEL-U(9:1)                 
038400     MOVE WS-PRARTBES(9:5)        TO 5111-PRARTBEL-U(10:5)                
038500                                                                          
038600     COMPUTE WS-RETULF-SEND  = 10000 * W-RETULF                           
038700                                                                          
038800     MOVE DCS-KDVALISO            TO 5111-KDVALISO-U                      
038900     MOVE IN-IDLEVNR              TO 5111-IDLEVNR-U                       
039000     MOVE W-KDFPKPRI              TO 5111-KDFPKPRI-U                      
039100                                                                          
039200     MOVE '+'                     TO 5111-FLPRIBES-U                      
039300                                     5111-FLPRIGO-U                       
039400                                                                          
039500     MOVE 5111-W5I11101           TO MSG-INDATA-MINUS-1-TRANSKOD          
039600     .                                                                    
039700     EJECT                                                                
039800                                                                          
039900 CA-GET-REEMBHNT SECTION.                                                 
040000     MOVE DCS-IDDC           TO WS-IDDC-SEND                              
040100     MOVE IN-IDDC            TO WS-IDDC-REC                               
040200* GET DISTRICT USING DC AS SENDING DC AND DC 11 AS RECIEVING DC           
040300     SEARCH ALL WWDC03-IDDISTR                                            
040400       AT END                                                             
040500         MOVE ZERO     TO W-IDDISTR-MIN                                   
040600         MOVE 'EJ TRÄFF I TABELL TEXTXX' TO ERROR-TEXT                    
040700         DISPLAY ERROR-TEXT                                               
040800         CALL FELLOG                                                      
040900       WHEN WWDC03-IDDC-SEND-REC(WWDC03-IX) = WS-IDDC-SEND-REC            
041000           MOVE WWDC03-SOK-IDDISTR(WWDC03-IX) TO W-IDDISTR-MIN            
041100                                                 W-IDDISTR-MAX            
041200     END-SEARCH                                                           
041300**************************************************************            
041400     MOVE ZERO TO W-IDKUNDNR-MIN                                          
041500     PERFORM IMS-GU-WDB201-FIRST                                          
041600     IF SEGMENT-MISSING                                                   
041700       MOVE 0                   TO WS-REEMBHNT                            
041800     ELSE                                                                 
041900       COMPUTE WS-REEMBHNT = WDB2-GMT-REEMBHNT / 100                      
042000     END-IF                                                               
042100     .                                                                    
042200     EJECT                                                                
042300                                                                          
042400 CB-GET-RELANDCO-EXP SECTION.                                             
042500     MOVE 0             TO WS-RELANDCO-EXP                                
042600     MOVE DCS-IDLANDX2  TO W-IDLANDX2                                     
042700     PERFORM IMS-GNP-WDB617                                               
042800     IF SEGMENT-MISSING                                                   
042900       CONTINUE                                                           
043000     ELSE                                                                 
043100       COMPUTE WS-RELANDCO-EXP = PROC-RELANDCO-EXP - 1                    
043200     END-IF                                                               
043300     .                                                                    
043400     EJECT                                                                
043500                                                                          
043600 CC-GET-RETULF SECTION.                                                   
043700****** CHECK IF SUPPLIER IS FOUND PÅ WDF1-LEVERANTÖRSREG                  
043800     MOVE IN-IDLEVNR            TO W-IDLEVNR                              
043900     PERFORM IMS-GU-WLLEVA01                                              
044000     IF SEGMENT-MISSING                                                   
044100       MOVE 1                   TO W-RETULF                               
044200     ELSE                                                                 
044300       MOVE DCS-IDLANDX2 TO W-IDLAND                                      
044400       PERFORM IMS-GNP-WLLEVA11                                           
044500       IF SEGMENT-FOUND                                                   
044600         IF LEV-TULL-TITULF < DAGENS-AAMMDD                               
044700           MOVE LEV-TULL-RETULF-1  TO W-RETULF                            
044800         ELSE                                                             
044900           MOVE LEV-TULL-RETULF-2  TO W-RETULF                            
045000         END-IF                                                           
045100       ELSE                                                               
045200         MOVE 1                    TO W-RETULF                            
045300       END-IF                                                             
045400     END-IF                                                               
045500     .                                                                    
045600     EJECT                                                                
045700                                                                          
045800 CD-GET-PRARTBES SECTION.                                                 
045900     COMPUTE W-PRARTBES ROUNDED = W-PRARTBEL * W-RETULF                   
046000     IF W-PRARTBES = +0                                                   
046100         MOVE +0.01      TO W-PRARTBES                                    
046200     END-IF                                                               
046300     COMPUTE WS-PRARTBES = 100000 * W-PRARTBES                            
046400     COMPUTE WS-PRARTBES ROUNDED = WS-PRARTBES +                          
046500             WS-PRARTBES * WS-REEMBHNT +                                  
046600             WS-PRARTBES * WS-REEMBHNT * WS-RELANDCO-EXP                  
046700     .                                                                    
046800     EJECT                                                                
046900                                                                          
047000 D-START-TRANS SECTION.                                                   
047100     CALL W006KOM USING MSG-PCB                                           
047200                        ALT-PCB                                           
047300                        KOMA-PCB                                          
047400                        MSG-KOM-WMSGKOM                                   
047500                        MSG-IO-AREA                                       
047600                                                                          
047700     MOVE 'W51085'       TO POSTSUM-FDNAMN                                
047800     MOVE 'W5T111X'      TO POSTSUM-DDNAMN2                               
047900     MOVE 'R25'          TO POSTSUM-TRANSTYP                              
048000     CALL POSTSUM USING POSTSUM-PARM                                      
048100     .                                                                    
048200     EJECT                                                                
048300                                                                          
048400 Z-FINIT SECTION.                                                         
048500     CLOSE W51085                                                         
048600     SKIP2                                                                
048700     MOVE 'S' TO POSTSUM-OPKOD                                            
048800     CALL POSTSUM USING POSTSUM-PARM                                      
048900     .                                                                    
049000     EJECT                                                                
049100                                                                          
049200 S01-READ-W51085  SECTION.                                                
049300     READ W51085 INTO IN-AREA                                             
049400     AT END                                                               
049500        MOVE HIGH-VALUE TO IN-AREA                                        
049600        SET END-OF-W51085 TO TRUE                                         
049700                                                                          
049800     NOT AT END                                                           
049900        MOVE 'W51085' TO POSTSUM-FDNAMN                                   
050000        MOVE 'W51085D1' TO POSTSUM-DDNAMN2                                
050100        MOVE SPACE     TO POSTSUM-TRANSTYP                                
050200        CALL POSTSUM USING POSTSUM-PARM                                   
050300     END-READ                                                             
050400     .                                                                    
050500     EJECT                                                                
050600                                                                          
050700 S99-ABEND SECTION.                                                       
050800     SKIP2                                                                
050900     MOVE 'S' TO POSTSUM-OPKOD                                            
051000     CALL POSTSUM USING POSTSUM-PARM                                      
051100     CALL ABEND USING RKOD-ABEND                                          
051200     .                                                                    
051300     EJECT                                                                
051400* --- IMS SECTIONS  ---                                                   
051500                                                                          
051600 IMS-GU-WDB601 SECTION.                                                   
051700     STRING 'WDB601  (IDLEVNDC =' W-IDLEVNR-B6-X ')'                      
051800          DELIMITED BY SIZE INTO SSA1                                     
051900     MOVE '  GE' TO GOOD-STATUSCODES                                      
052000     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
052100     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
052200     PERFORM IMS-STATUSCHECK                                              
052300     .                                                                    
052400                                                                          
052500 IMS-GNP-WDB617    SECTION.                                               
052600     MOVE 'WDB617   ' TO SSA1                                             
052700     MOVE '  GE' TO GOOD-STATUSCODES                                      
052800     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B617 SSA1                
052900     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
053000     PERFORM IMS-STATUSCHECK                                              
053100     .                                                                    
053200                                                                          
053300 IMS-GU-WLLEVA01 SECTION.                                                 
053400     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
053500     DELIMITED BY SIZE INTO SSA1                                          
053600     MOVE '  GE' TO GOOD-STATUSCODES                                      
053700     CALL CBLTDLI USING GU LEV-PCB WLLEVA01 SSA1                          
053800     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
053900     PERFORM IMS-STATUSCHECK                                              
054000     .                                                                    
054100     SKIP3                                                                
054200                                                                          
054300 IMS-GNP-WLLEVA11 SECTION.                                                
054400     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
054500     DELIMITED BY SIZE INTO SSA1                                          
054600     MOVE '  GE' TO GOOD-STATUSCODES                                      
054700     CALL CBLTDLI USING GNP LEV-PCB LEV-WLLEVA11 SSA1                     
054800     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
054900     PERFORM IMS-STATUSCHECK                                              
055000     .                                                                    
055100     EJECT                                                                
055200                                                                          
055300 IMS-GU-WDB201-FIRST SECTION.                                             
055400     STRING 'WDB201  (IDGMT   >=' W-IDGMT-MIN-X                           
055500                    '&IDGMT   <=' W-IDGMT-MAX-X ')'                       
055600          DELIMITED BY SIZE INTO SSA1                                     
055700     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
055800     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-AREA-WDB2 SSA1                 
055900     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
056000     PERFORM IMS-STATUSCHECK                                              
056100     .                                                                    
056200     SKIP3                                                                
056300                                                                          
056400 IMS-GU-WDK711 SECTION.                                                   
056500     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
056600         DELIMITED  BY SIZE INTO SSA1                                     
056700     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
056800         DELIMITED  BY SIZE INTO SSA2                                     
056900     MOVE '  GE' TO GOOD-STATUSCODES                                      
057000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-AREA-K711 SSA1 SSA2            
057100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
057200     PERFORM IMS-STATUSCHECK                                              
057300     .                                                                    
057400     SKIP3                                                                
057500                                                                          
057600 IMS-GNP-WDK724-FIRST SECTION.                                            
057700     MOVE 'WDK724  *F' TO SSA1                                            
057800     MOVE '  GE' TO GOOD-STATUSCODES                                      
057900     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-K724 SSA1                
058000     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
058100     PERFORM IMS-STATUSCHECK                                              
058200     .                                                                    
058300     SKIP3                                                                
058400                                                                          
058500 IMS-GNP-WDK724-NEXT SECTION.                                             
058600     MOVE 'WDK724   ' TO SSA1                                             
058700     MOVE '  GE' TO GOOD-STATUSCODES                                      
058800     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-AREA-K724 SSA1                
058900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
059000     PERFORM IMS-STATUSCHECK                                              
059100     .                                                                    
059200     EJECT                                                                
059300                                                                          
059400 IMS-STATUSCHECK SECTION.                                                 
059500     SET STATUS-IX TO 1                                                   
059600     SEARCH GOOD-STATUS                                                   
059700       AT END                                                             
059800         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
059900           DELIMITED BY SIZE INTO ERROR-TEXT                              
060000         DISPLAY ERROR-TEXT                                               
060100         CALL FELLOG                                                      
060200       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
060300         CONTINUE                                                         
060400     END-SEARCH                                                           
060500     .                                                                    
