000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5530500.                                                
000300 AUTHOR.         BARSHARANI BISHOYE.                                      
000400 DATE-WRITTEN.   22/11/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        VALIDATION OF W55304 FILE FOR STANDARD PRICE PROCESS             
000900*                                                                         
001000*                                                                         
001100*    ABENDCODES:                                                          
001200*        U0016 -  . . . .                                                 
001300*        U1000 -  . . . .                                                 
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- EXCEL INFILE                                               
002400     SELECT W55304                     ASSIGN TO W55305D1.                
002500     SKIP2                                                                
002600*          --- FAILED INFORMATION UTFILE                                  
002700     SELECT W55305                     ASSIGN TO W55305D2.                
002800     EJECT                                                                
002900*          --- ERROR FILE                                                 
003000     SELECT W55305F                    ASSIGN TO W55305D3.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W55304                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900 01  FILLER        PIC X(250).                                            
004000     SKIP3                                                                
004100 FD  W55305                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400 01  UT-POST.                                                             
004500*    03   -COPY WDH801 -PRE  UT-  -L.                                     
004600     EJECT                                                                
004700 FD  W55305F                                                              
004800     RECORDING       V                                                    
004900     BLOCK CONTAINS  0.                                                   
005000 01  W55305-001    PIC X(250).                                            
005100     SKIP2                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400 77  IDPGM                       PIC X(8)    VALUE 'W5530500'.            
005500 77  YES                         PIC X       VALUE 'J'.                   
005600 77  NOO                         PIC X       VALUE 'N'.                   
005700                                                                          
005800 77  W55304-EOF-SW               PIC X       VALUE 'N'.                   
005900     88  END-OF-W55304                       VALUE 'J'.                   
006000     EJECT                                                                
006100 77  WS-COUNTER1                 PIC S9(3)   VALUE ZERO  COMP-3.          
006200 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006300 01  FILLER REDEFINES TODAYS-DATE.                                        
006400     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006500     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006600     03  TODAYS-DATE-DAY         PIC 9(2).                                
006700     EJECT                                                                
006800 77  W-DAGENS-DATUM              PIC 9(8).                                
006900 77  TRANS-TID                   PIC 9(9).                                
007000 01  WS-DAREGDAT                 PIC 9(8).                                
007100 01  WS-TIREGTID                 PIC 9(9).                                
007200                                                                          
007300 01  FELTEXT.                                                             
007400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007600                                                                          
007700 01  GENERAL-SUBPROGRAMS.                                                 
007800*                                                                         
007900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     SKIP2                                                                
008400*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008500                                                                          
008600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
008800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
008900     SKIP2                                                                
009000 01  ERROR-TEXT.                                                          
009100     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
009200     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
009300     EJECT                                                                
009400 01  W001-DAP.                                                            
009500     03  FILLER                  PIC X(165)  VALUE SPACE.                 
009600*    --- PARAMETRAR TILL POSTSUM                                          
009700*                                                                         
009800*01  -COPY W0005   -PRE  POSTSUM-                                         
009900     EJECT                                                                
010000 01  IN-AREA-START               PIC X(24)   VALUE                        
010100                                 'IN-AREA-START  '.                       
010200 01  IN-AREA                     PIC X(250).                              
010300                                                                          
010400 01  FILLER REDEFINES IN-AREA.                                            
010500     03 IN-IDARTNR           PIC 9(9).                                    
010600     03 IN-KDPRIBEH          PIC X.                                       
010700     03 IN-PRDIRLON          PIC S9(4)V9(3)  COMP-3.                      
010800     03 IN-PRDMTRL           PIC S9(6)V9(3)  COMP-3.                      
010900     03 IN-PROVRPAL          PIC S9(4)V9(3)  COMP-3.                      
011000     03 IN-PRARTSTD          PIC S9(7)V9(2)  COMP-3.                      
011100     03 IN-IDUSER            PIC X(8).                                    
011200                                                                          
011300     SKIP2                                                                
011400     EJECT                                                                
011500 01  UT-AREA-START               PIC X(24)   VALUE                        
011600                                 'UT-AREA-START  '.                       
011700 01  UT-AREA.                                                             
011800*    03      -COPY WDH801 -PRE UT-                                        
011900                                                                          
012000     EJECT                                                                
012100                                                                          
012200 01  FELL-HEADER.                                                         
012300     03  FILLER           PIC X(7)  VALUE 'PART NO'.                      
012400     03  FILLER           PIC X     VALUE ';'.                            
012500     03  FILLER           PIC X(4)  VALUE 'DATE'.                         
012600     03  FILLER           PIC X     VALUE ';'.                            
012700     03  FILLER           PIC X(4)  VALUE 'TIME'.                         
012800     03  FILLER           PIC X     VALUE ';'.                            
012900     03  FILLER           PIC X(11) VALUE 'DESCRIPTION'.                  
013000     03  FILLER           PIC X     VALUE ';'.                            
013100     03  FILLER           PIC X(18) VALUE 'PRICE COMMAND CODE'.           
013200     03  FILLER           PIC X     VALUE ';'.                            
013300     03  FILLER           PIC X(13) VALUE 'DIRECT SALARY'.                
013400     03  FILLER           PIC X     VALUE ';'.                            
013500     03  FILLER           PIC X(15) VALUE 'DIRECT MATERIAL'.              
013600     03  FILLER           PIC X     VALUE ';'.                            
013700     03  FILLER           PIC X(15) VALUE 'DIRECT OVERHEAD'.              
013800     03  FILLER           PIC X     VALUE ';'.                            
013900     03  FILLER           PIC X(15) VALUE 'PURCHASED PRICE'.              
014000     03  FILLER           PIC X     VALUE ';'.                            
014100     03  FILLER           PIC X(7)  VALUE 'USER ID'.                      
014200     03  FILLER           PIC X     VALUE ';'.                            
014300     EJECT                                                                
014400                                                                          
014500 01  FELL-DETAIL.                                                         
014600     03  FELL-IDARTNR            PIC X(10).                               
014700     03  FILLER                  PIC X     VALUE ';'.                     
014800     03  FELL-DAREGDAT           PIC X(10).                               
014900     03  FILLER                  PIC X     VALUE ';'.                     
015000     03  FELL-TIREGTID           PIC X(10).                               
015100     03  FILLER                  PIC X     VALUE ';'.                     
015200     03  FELL-DESCR              PIC X(50) VALUE SPACE.                   
015300     03  FILLER                  PIC X     VALUE ';'.                     
015400     03  FELL-KDPRIBEH           PIC X.                                   
015500     03  FILLER                  PIC X     VALUE ';'.                     
015600     03  FELL-PRDIRLON           PIC 9(8).                                
015700     03  FILLER                  PIC X     VALUE ';'.                     
015800     03  FELL-PRDMTRL            PIC 9(10).                               
015900     03  FILLER                  PIC X     VALUE ';'.                     
016000     03  FELL-PROVRPAL           PIC 9(8).                                
016100     03  FILLER                  PIC X     VALUE ';'.                     
016200     03  FELL-PRARTSTD           PIC 9(11).                               
016300     03  FILLER                  PIC X     VALUE ';'.                     
016400     03  FELL-IDUSER             PIC X(8).                                
016500     03  FILLER                  PIC X     VALUE ';'.                     
016600     EJECT                                                                
016700     SKIP2                                                                
016800*    --- AREAS FOR IMS-SECTIONS                                           
016900*                                                                         
017000     EJECT                                                                
017100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017200     SKIP3                                                                
017300 01  KEYS-FOR-DLI.                                                        
017400     03  W-IDARTNR-X.                                                     
017500         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017600     SKIP2                                                                
017700*    --- STATUS-KOD FRÅN IMS                                              
017800 01  STATUS-WS                   PIC XX.                                  
017900     88  SEGMENT-FOUND                       VALUE '  '.                  
018000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
018100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
018200     88  IMS-EJ-OK                           VALUE 'XD'.                  
018300     SKIP2                                                                
018400 01  GOOD-STATUSCODES.                                                    
018500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018600     SKIP3                                                                
018700 01  SSA1                        PIC X(64).                               
018800 01  SSA2                        PIC X(64).                               
018900     EJECT                                                                
019000*    --- IMS FUNCTION CODES                                               
019100*01  -COPY W0003                                                          
019200     EJECT                                                                
019300*    ---  DLI INPUT-OUTPUT AREA                                           
019400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
019500 01  DLI-IO-WDK601.                                                       
019600*    03 WDK601   -COPY WDK601                                             
019700     EJECT                                                                
019800                                                                          
019900 LINKAGE SECTION.                                                         
020000                                                                          
020100                                                                          
020200*01  -COPY W0008  -PRE WDK6-                                              
020300     05  FILLER                  PIC X.                                   
020400     EJECT                                                                
020500 PROCEDURE DIVISION  USING  WDK6-PCB.                                     
020600 MAIN SECTION.                                                            
020700     ENTRY 'DLITCBL' USING  WDK6-PCB.                                     
020800                                                                          
020900                                                                          
021000     PERFORM A-INIT                                                       
021100                                                                          
021200     PERFORM S01-READ-W55304                                              
021300     PERFORM UNTIL END-OF-W55304                                          
021400     PERFORM B-CHECK-DATA                                                 
021500                                                                          
021600       PERFORM S01-READ-W55304                                            
021700     END-PERFORM                                                          
021800                                                                          
021900                                                                          
022000     PERFORM Z-FINIT                                                      
022100                                                                          
022200     MOVE ZERO TO RETURN-CODE                                             
022300     GOBACK                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 A-INIT SECTION.                                                          
022700                                                                          
022800     OPEN INPUT  W55304                                                   
022900                                                                          
023000     OPEN OUTPUT W55305                                                   
023100                 W55305F                                                  
023200                                                                          
023300*    ACCEPT TODAYS-DATE  FROM DATE                                        
023400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
023500     .                                                                    
023600     EJECT                                                                
023700 B-CHECK-DATA SECTION.                                                    
023800     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAREGDAT                      
023900     ACCEPT TRANS-TID FROM TIME                                           
024000     MOVE FUNCTION CURRENT-DATE (9:4) TO WS-TIREGTID(1:4)                 
024100                                                                          
024200     MOVE IN-IDARTNR    TO W-IDARTNR                                      
024300     PERFORM IMS-GET-WDK601                                               
024400     IF SEGMENT-FOUND                                                     
024500       IF ART-KDERS-UTG = 0                                               
024600         PERFORM C-MOVE-DATA                                              
024700         IF ART-KDSORT = 'SW' AND IN-KDPRIBEH = 'U'                       
024800           IF IN-PRDIRLON NOT = ZERO                                      
024900            MOVE 'DIRECT SALARY SHOULD BE ZERO FOR SW PART'               
025000                                    TO  FELL-DESCR                        
025100            ELSE                                                          
025200             IF IN-PRDMTRL NOT = ZERO                                     
025300              MOVE 'DIRECT MATERIAL SHOULD BE ZERO FOR SW PART'           
025400                                    TO  FELL-DESCR                        
025500             ELSE                                                         
025600              IF IN-PROVRPAL NOT = ZERO                                   
025700               MOVE 'DIRECT OVERHEAD SHOULD BE ZERO FOR SW PART'          
025800                                    TO  FELL-DESCR                        
025900              END-IF                                                      
026000             END-IF                                                       
026100           END-IF                                                         
026200           MOVE ZERO  TO  UT-PRI-N-PRDIRLON                               
026300           MOVE ZERO  TO  UT-PRI-N-PRDMTRL                                
026400           MOVE ZERO  TO  UT-PRI-N-PROVRPAL                               
026500           PERFORM CA-MOVE-DATA                                           
026600           PERFORM S11-WRITE-W55305F                                      
026700         ELSE                                                             
026800         PERFORM S11-WRITE-W55305                                         
026900         END-IF                                                           
027000       ELSE                                                               
027100         MOVE 'PART SUPERSEDED'      TO  FELL-DESCR                       
027200         PERFORM CA-MOVE-DATA                                             
027300         PERFORM S11-WRITE-W55305F                                        
027400       END-IF                                                             
027500     ELSE                                                                 
027600       MOVE 'PART MISSING IN PULS'  TO  FELL-DESCR                        
027700       PERFORM CA-MOVE-DATA                                               
027800       PERFORM S11-WRITE-W55305F                                          
027900     END-IF                                                               
028000     PERFORM IMS-GET-WDK601                                               
028100     .                                                                    
028200     EJECT                                                                
028300 C-MOVE-DATA SECTION.                                                     
028400     MOVE IN-IDARTNR       TO UT-PRI-IDARTNR                              
028500     MOVE WS-DAREGDAT      TO UT-PRI-DAREGDAT                             
028600     MOVE WS-TIREGTID(1:4) TO UT-PRI-TIREGTID                             
028700     MOVE 'J'              TO UT-PRI-FLKLAR                               
028800     MOVE 'N'              TO UT-PRI-FLPRFIL                              
028900     MOVE 'N'              TO UT-PRI-FLPRIBES                             
029000     MOVE SPACE            TO UT-PRI-FLPRIGO                              
029100     MOVE IN-IDUSER        TO UT-PRI-IDUSER                               
029200     MOVE IN-KDPRIBEH      TO UT-PRI-KDPRIBEH                             
029300     MOVE ZERO             TO UT-PRI-REDIRLEV                             
029400     MOVE SPACE            TO UT-PRI-O-IDLEVNR-PR                         
029500     MOVE SPACE            TO UT-PRI-O-KDPRURSP                           
029600     MOVE ZERO             TO UT-PRI-O-KDSTATUS-PR                        
029700     MOVE SPACE            TO UT-PRI-O-KDVALISO                           
029800     MOVE ZERO             TO UT-PRI-O-PRARTBEL-PR                        
029900     MOVE ZERO             TO UT-PRI-O-PRARTBES-PR                        
030000     MOVE ZERO             TO UT-PRI-O-SUINLEV-PR                         
030100     MOVE ZERO             TO UT-PRI-O-TIPRLIST                           
030200     MOVE SPACE            TO UT-PRI-N-IDLEVNR-PR                         
030300     MOVE SPACE            TO UT-PRI-N-KDPRURSP                           
030400     MOVE ZERO             TO UT-PRI-N-KDSTATUS-PR                        
030500     MOVE SPACE            TO UT-PRI-N-KDVALISO                           
030600     MOVE ZERO             TO UT-PRI-N-PRARTBEL-PR                        
030700     MOVE ZERO             TO UT-PRI-N-PRARTBES-PR                        
030800     MOVE ZERO             TO UT-PRI-N-SUINLEV-PR                         
030900     MOVE ZERO             TO UT-PRI-N-TIPRLIST                           
031000     MOVE SPACE            TO UT-PRI-O-KDCMD                              
031100     MOVE ZERO             TO UT-PRI-O-PRARTBES                           
031200     MOVE ZERO             TO UT-PRI-O-PRARTSJK                           
031300     MOVE ZERO             TO UT-PRI-O-PRARTSTD                           
031400     MOVE ZERO             TO UT-PRI-O-PRDIRLON                           
031500     MOVE ZERO             TO UT-PRI-O-PRDMTRL                            
031600     MOVE ZERO             TO UT-PRI-O-PRINK                              
031700     MOVE ZERO             TO UT-PRI-O-PRLFKST                            
031800     MOVE ZERO             TO UT-PRI-O-PROVRPAL                           
031900     MOVE ZERO             TO UT-PRI-O-RETULF                             
032000     MOVE SPACE            TO UT-PRI-O-TEARTNOT                           
032100     MOVE SPACE            TO UT-PRI-N-KDCMD                              
032200     MOVE ZERO             TO UT-PRI-N-PRARTBES                           
032300     MOVE ZERO             TO UT-PRI-N-PRARTSJK                           
032400     MOVE ZERO             TO UT-PRI-N-PRARTSTD                           
032500     MOVE IN-PRDIRLON      TO UT-PRI-N-PRDIRLON                           
032600     MOVE IN-PRDMTRL       TO UT-PRI-N-PRDMTRL                            
032700     MOVE IN-PRARTSTD      TO UT-PRI-N-PRINK                              
032800     MOVE ZERO             TO UT-PRI-N-PRLFKST                            
032900     MOVE IN-PROVRPAL      TO UT-PRI-N-PROVRPAL                           
033000     MOVE ZERO             TO UT-PRI-N-RETULF                             
033100     MOVE SPACE            TO UT-PRI-N-TEARTNOT                           
033200     .                                                                    
033300     EJECT                                                                
033400 CA-MOVE-DATA SECTION.                                                    
033500     MOVE IN-IDARTNR        TO FELL-IDARTNR                               
033600     MOVE WS-DAREGDAT       TO FELL-DAREGDAT                              
033700     MOVE WS-TIREGTID(1:4)  TO FELL-TIREGTID                              
033800     MOVE IN-KDPRIBEH       TO FELL-KDPRIBEH                              
033900     MOVE IN-PRDIRLON       TO FELL-PRDIRLON                              
034000     MOVE IN-PRDMTRL        TO FELL-PRDMTRL                               
034100     MOVE IN-PROVRPAL       TO FELL-PROVRPAL                              
034200     MOVE IN-PRARTSTD       TO FELL-PRARTSTD                              
034300     MOVE IN-IDUSER         TO FELL-IDUSER                                
034400     .                                                                    
034500     EJECT                                                                
034600 Z-FINIT SECTION.                                                         
034700     CLOSE W55304                                                         
034800           W55305                                                         
034900           W55305F                                                        
035000     SKIP2                                                                
035100     MOVE 'S' TO POSTSUM-OPKOD                                            
035200     CALL POSTSUM USING POSTSUM-PARM                                      
035300     .                                                                    
035400     EJECT                                                                
035500 S01-READ-W55304  SECTION.                                                
035600     READ W55304 INTO IN-AREA                                             
035700     AT END                                                               
035800        MOVE HIGH-VALUE TO IN-AREA                                        
035900        SET END-OF-W55304 TO TRUE                                         
036000                                                                          
036100     NOT AT END                                                           
036200        MOVE 'W55304' TO POSTSUM-FDNAMN                                   
036300        MOVE 'W55305D1' TO POSTSUM-DDNAMN2                                
036400        CALL POSTSUM USING POSTSUM-PARM                                   
036500     END-READ                                                             
036600     .                                                                    
036700     EJECT                                                                
036800 S11-WRITE-W55305 SECTION.                                                
036900                                                                          
037000     WRITE UT-POST FROM UT-AREA                                           
037100                                                                          
037200     MOVE 'W55305' TO POSTSUM-FDNAMN                                      
037300     MOVE 'W55305D2' TO POSTSUM-DDNAMN2                                   
037400     CALL POSTSUM USING POSTSUM-PARM                                      
037500     .                                                                    
037600     EJECT                                                                
037700 S11-WRITE-W55305F SECTION.                                               
037800                                                                          
037900     IF WS-COUNTER1 = ZERO                                                
038000       MOVE  ' ¤DAPW55305-001' TO W001-DAP                                
038100       WRITE W55305-001   FROM W001-DAP                                   
038200       MOVE  ' ¤DAPW55305'     TO W001-DAP                                
038300       WRITE W55305-001   FROM W001-DAP                                   
038400       WRITE W55305-001   FROM FELL-HEADER                                
038500     END-IF                                                               
038600     WRITE W55305-001   FROM FELL-DETAIL                                  
038700     ADD +1 TO WS-COUNTER1                                                
038800     .                                                                    
038900     SKIP3                                                                
039000 S99-ABEND SECTION.                                                       
039100                                                                          
039200     SKIP2                                                                
039300     MOVE 'S' TO POSTSUM-OPKOD                                            
039400     CALL POSTSUM USING POSTSUM-PARM                                      
039500     CALL ABEND USING RKOD-ABEND                                          
039600     .                                                                    
039700     EJECT                                                                
039800* --- IMS SECTIONS  ---                                                   
039900                                                                          
040000 IMS-GET-WDK601 SECTION.                                                  
040100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
040200         DELIMITED BY SIZE INTO SSA1                                      
040300     MOVE '  GE'          TO GOOD-STATUSCODES                             
040400     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
040500     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
040600     PERFORM IMS-STATUSCHECK                                              
040700     .                                                                    
040800     EJECT                                                                
040900 IMS-STATUSCHECK SECTION.                                                 
041000                                                                          
041100     SET STATUS-IX TO 1                                                   
041200     SEARCH GOOD-STATUS                                                   
041300       AT END                                                             
041400         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
041500           DELIMITED BY SIZE INTO ERROR-TEXT                              
041600         DISPLAY ERROR-TEXT                                               
041700         CALL FELLOG                                                      
041800       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
041900         CONTINUE                                                         
042000     END-SEARCH                                                           
042100     .                                                                    
