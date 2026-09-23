000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF210900.                                                
000300 AUTHOR.         ANDERS HENRIKSSON.                                       
000400 DATE-WRITTEN.   2004-04-02.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*    THE PGM                                                              
000900*    - READS FILE WITH FEEDBACK DATA RECORDS                              
001000*    - SENDS DATA TO VIPS FOR VCCS-WEBSHOP BY:                            
001100*      . D&P                                                              
001200                                                                          
001300 ENVIRONMENT DIVISION.                                                    
001400 INPUT-OUTPUT SECTION.                                                    
001500 FILE-CONTROL.                                                            
001600*          --- FEEDBACK DATA RECORDS                                      
001700     SELECT WF2017                     ASSIGN TO WF2109D1.                
001800     EJECT                                                                
001900 DATA DIVISION.                                                           
002000 FILE SECTION.                                                            
002100 FD  WF2017                                                               
002200     RECORDING       F                                                    
002300     BLOCK CONTAINS  0.                                                   
002400                                                                          
002500*01  -COPY WF2017      -L.                                                
002600     EJECT                                                                
002700                                                                          
002800 WORKING-STORAGE SECTION.                                                 
002900*** - CONSTANTS                                                           
003000 77  IDPGM                       PIC X(8)    VALUE 'WF210900'.            
003100 77  SYSIN-EOF                   PIC X       VALUE 'N'.                   
003200 77  YES                         PIC X       VALUE 'J'.                   
003300 77  NOO                         PIC X       VALUE 'N'.                   
003400 77  WS-IDLEGSEL-VCCS            PIC X(4)    VALUE 'VCCS'.                
003500 77  WS-VIPS                     PIC X(4)    VALUE 'VIPS'.                
003600 77  WS-VSS                      PIC X(4)    VALUE 'VSS'.                 
003700 77  WS-CARDEU                   PIC X(15)   VALUE 'CARD-EU'.             
003800 77  WS-CARDNOTEU                PIC X(15)   VALUE 'CARD-NOT-EU'.         
003900 77  WS-INTERNAL                 PIC X(4)    VALUE 'INT2'.                
004000 77  OK-SW                       PIC X       VALUE 'N'.                   
004100 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
004200 77  FIRST-TIME-SW               PIC X       VALUE 'J'.                   
004300 77  WF2017-EOF-SW               PIC X       VALUE 'N'.                   
004400     88  END-OF-WF2017                       VALUE 'J'.                   
004500 77  WS-ADRESS                   PIC X(50)                                
004600                           VALUE 'CARPARTS.DAP.DISTRDOC'.                 
004700     EJECT                                                                
004800                                                                          
004900 01  WS-IDLANDX3                 PIC X(3)       VALUE SPACE.              
005000 01  WS-DAFINDOC                 PIC S9(8)      COMP VALUE +0.            
005100 01  WS-IDFINDOC                 PIC S9(9)      COMP VALUE +0.            
005200 01  WS-IDREF                    PIC X(15)      VALUE SPACE.              
005300 01  WS-IDPARTNR                 PIC X(9)       VALUE SPACE.              
005400 01  WS-IDPARTNR2                PIC X(9)       VALUE SPACE.              
005500 01  WS-KDVALISO-BET             PIC X(3)       VALUE SPACE.              
005600 01  WS-BEANST                   PIC X(25)      VALUE SPACE.              
005700 01  WS-DAREFDAT                 PIC 9(8)       VALUE ZERO.               
005800 01  WS-SUNTO-TOT                PIC 9(11)V9(2) VALUE ZERO.               
005900 01  WS-PRKURS-BET               PIC 9(6)V9(5)  VALUE ZERO.               
006000 01  WS-QUROWS                   PIC S9(4)      VALUE ZERO COMP-3.        
006100                                                                          
006200 01  ERRTEXT.                                                             
006300     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
006400     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
006500 01  KDRC-DISPLAY                PIC Z(5).                                
006600     EJECT                                                                
006700                                                                          
006800 01  GENERAL-SUBPROGRAMS.                                                 
006900     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007100     EJECT                                                                
007200                                                                          
007300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007800     EJECT                                                                
007900                                                                          
008000*    --- AREOR FÖR KOMMUNIKATION                                          
008100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
008200*01  -COPY WZ01SEND                                                       
008300     EJECT                                                                
008400                                                                          
008500 01  INPUT-AREA                 PIC X(24)   VALUE                         
008600                                'INPUT-AREA     '.                        
008700                                                                          
008800 01  IN-AREA-START            PIC X(24)   VALUE 'IN-AREA-START'.          
008900 01  IN-AREA.                                                             
009000*    03  -COPY WF2017                                                     
009100     EJECT                                                                
009200                                                                          
009300*    --- UT-AREOR                                                         
009400 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
009500                                'OUTPUT-AREA     '.                       
009600 01  HDR-AREA.                                                            
009700*    03  -COPY WZ01REQU                                                   
009800*    03  -COPY WZ04HDR                                                    
009900     EJECT                                                                
010000                                                                          
010100 01  LINE-AREA.                                                           
010200*    03  -COPY WZ01REQU         -PRE VIPSUT-                              
010300*    03  -COPY WF2109           -PRE VIPSUT-                              
010400     EJECT                                                                
010500                                                                          
010600 LINKAGE SECTION.                                                         
010700*01  -COPY W0009   -PRE MSG-                                              
010800     EJECT                                                                
010900                                                                          
011000 PROCEDURE DIVISION  USING MSG-PCB.                                       
011100 MAIN SECTION.                                                            
011200     ENTRY 'DLITCBL' USING MSG-PCB.                                       
011300                                                                          
011400     PERFORM A-INIT                                                       
011500     PERFORM B-EXECUTE                                                    
011600     PERFORM Z-FINIT                                                      
011700                                                                          
011800     MOVE ZERO TO RETURN-CODE                                             
011900     GOBACK                                                               
012000     .                                                                    
012100 A-INIT SECTION.                                                          
012200     OPEN INPUT WF2017                                                    
012300     MOVE YES TO FIRST-TIME-SW                                            
012400     MOVE SPACE TO WS-IDLANDX3                                            
012500     .                                                                    
012600     EJECT                                                                
012700                                                                          
012800 B-EXECUTE SECTION.                                                       
012900     PERFORM S01-READ-WF2017                                              
013000                                                                          
013100     IF END-OF-WF2017                                                     
013200       CONTINUE                                                           
013300     ELSE                                                                 
013400       PERFORM UNTIL END-OF-WF2017                                        
013500         IF FEED-IDLEGSEL = WS-IDLEGSEL-VCCS                              
013600           IF FEED-IDSYSTEM-SEND = WS-VSS                                 
013700             IF FEED-KDPARTGR = WS-CARDEU                                 
013800             OR FEED-KDPARTGR = WS-CARDNOTEU                              
013900             OR FEED-KDFINDOC = WS-INTERNAL                               
014000               CONTINUE                                                   
014100             ELSE                                                         
014200               PERFORM BA-SEND-TO-VIPS                                    
014300             END-IF                                                       
014400           END-IF                                                         
014500         END-IF                                                           
014600         PERFORM S01-READ-WF2017                                          
014700       END-PERFORM                                                        
014800     END-IF                                                               
014900     .                                                                    
015000                                                                          
015100 BA-SEND-TO-VIPS  SECTION.                                                
015200     IF FEED-IDLANDX3-BET = WS-IDLANDX3                                   
015300     AND FEED-IDPARTNR = WS-IDPARTNR2                                     
015400       CONTINUE                                                           
015500     ELSE                                                                 
015600       IF FIRST-TIME-SW = YES                                             
015700         PERFORM S90-SEND-OPEN                                            
015800         MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                               
015900         MOVE NOO TO FIRST-TIME-SW                                        
016000         MOVE ZERO            TO WS-DAFINDOC                              
016100         MOVE ZERO            TO WS-IDFINDOC                              
016200         PERFORM BAA-HANDLE-HEADER                                        
016300         PERFORM S90-PUT-HEADER                                           
016400       ELSE                                                               
016500         PERFORM S90-SEND-CLOSE                                           
016600         MOVE ZERO            TO WZ04-SEND-IDCOM                          
016700         PERFORM S90-SEND-OPEN                                            
016800         MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                               
016900         MOVE ZERO            TO WS-DAFINDOC                              
017000         MOVE ZERO            TO WS-IDFINDOC                              
017100         PERFORM BAA-HANDLE-HEADER                                        
017200         PERFORM S90-PUT-HEADER                                           
017300       END-IF                                                             
017400       MOVE FEED-IDLANDX3-BET TO WS-IDLANDX3                              
017500       MOVE FEED-IDPARTNR       TO WS-IDPARTNR2                           
017600     END-IF                                                               
017700                                                                          
017800     IF FEED-DAFINDOC NOT = WS-DAFINDOC OR                                
017900        FEED-IDFINDOC NOT = WS-IDFINDOC                                   
018000       MOVE FEED-DAFINDOC       TO WS-DAFINDOC                            
018100       MOVE FEED-IDFINDOC       TO WS-IDFINDOC                            
018200       MOVE FEED-IDREF          TO WS-IDREF                               
018300       MOVE FEED-IDEXCUST(1)    TO WS-IDPARTNR                            
018400       MOVE FEED-BEANST         TO WS-BEANST                              
018500       MOVE FEED-DAREFDAT       TO WS-DAREFDAT                            
018600       MOVE FEED-SUNTO-TOT      TO WS-SUNTO-TOT                           
018700       MOVE FEED-KDVALISO-BET TO WS-KDVALISO-BET                          
018800       MOVE FEED-PRKURS-FAKBET     TO WS-PRKURS-BET                       
018900       PERFORM BAB-SEND-HEADER-TO-VIPS                                    
019000       PERFORM BAC-SEND-LINE-TO-VIPS                                      
019100     ELSE                                                                 
019200       PERFORM BAC-SEND-LINE-TO-VIPS                                      
019300     END-IF                                                               
019400     .                                                                    
019500                                                                          
019600 BAA-HANDLE-HEADER SECTION.                                               
019700     MOVE 1                          TO REQU-IDMSGVER                     
019800     MOVE 'R'                        TO REQU-KDPGMACT                     
019900     MOVE IDPGM                      TO REQU-IDUSER                       
020000                                                                          
020100     MOVE 'WEBSHOP-VIPS'             TO HDR-IDOUTTYPE                     
020200     MOVE SPACE                      TO HDR-IDOUTREC                      
020300     MOVE FEED-IDLANDX3-BET(1:2)     TO HDR-IDOUTREC(1:2)                 
020400     MOVE FEED-IDPARTNR              TO HDR-IDOUTREC(3:9)                 
020500     MOVE FEED-IDFINDOC              TO HDR-IDLIST                        
020600     .                                                                    
020700                                                                          
020800 BAB-SEND-HEADER-TO-VIPS SECTION.                                         
020900     MOVE 1                     TO VIPSUT-REQU-IDMSGVER                   
021000     MOVE 'R'                   TO VIPSUT-REQU-KDPGMACT                   
021100     MOVE IDPGM                 TO VIPSUT-REQU-IDUSER                     
021200                                                                          
021300     ADD +1                     TO WS-QUROWS                              
021400     MOVE '001'                 TO VIPSUT-IDPTYP                          
021500     MOVE WS-IDREF              TO VIPSUT-IDREF                           
021600     MOVE WS-IDPARTNR           TO VIPSUT-IDPARTNR                        
021700     MOVE WS-BEANST             TO VIPSUT-BEANST                          
021800     MOVE WS-DAFINDOC           TO VIPSUT-DAREFDAT                        
021900     MOVE WS-SUNTO-TOT          TO VIPSUT-SUNTO-TOT-VIPS                  
022000     MOVE WS-QUROWS             TO VIPSUT-KVORDRAD                        
022100     MOVE 0                     TO VIPSUT-IDREFRAD                        
022200     MOVE ' '                   TO VIPSUT-IDARTNR20                       
022300     MOVE ' '                   TO VIPSUT-BEART                           
022400     MOVE 0                     TO VIPSUT-REARTRAB-VIPS                   
022500     MOVE 0                     TO VIPSUT-KVLEVART-VIPS                   
022600     MOVE 0                     TO VIPSUT-PRARTBTO-VIPS                   
022700     MOVE 0                     TO VIPSUT-SUNTO-VIPS                      
022800     MOVE 0                     TO VIPSUT-PRARTNTO-VIPS                   
022900     MOVE ' '                   TO VIPSUT-KDVAT                           
023000     MOVE WS-KDVALISO-BET       TO VIPSUT-KDVALISO-LOC                    
023100     MOVE WS-PRKURS-BET         TO VIPSUT-PRKURS                          
023200     MOVE WS-IDFINDOC           TO VIPSUT-IDFINDOC                        
023300     MOVE ' '                   TO VIPSUT-BELEVVIL                        
023400                                                                          
023500     PERFORM S90-PUT-LINE                                                 
023600     .                                                                    
023700                                                                          
023800 BAC-SEND-LINE-TO-VIPS SECTION.                                           
023900     IF FEED-IDARTNR-FINANCE = SPACE                                      
024000       MOVE '003'               TO VIPSUT-IDPTYP                          
024100     ELSE                                                                 
024200       MOVE '002'               TO VIPSUT-IDPTYP                          
024300     END-IF                                                               
024400     MOVE FEED-IDREF            TO VIPSUT-IDREF                           
024500     MOVE FEED-IDEXCUST(1)      TO VIPSUT-IDPARTNR                        
024600     MOVE FEED-BEANST           TO VIPSUT-BEANST                          
024700     MOVE FEED-DAREFDAT         TO VIPSUT-DAREFDAT                        
024800     MOVE 0                     TO VIPSUT-SUNTO-TOT-VIPS                  
024900     MOVE 0                     TO VIPSUT-KVORDRAD                        
025000     MOVE FEED-IDREFRAD         TO VIPSUT-IDREFRAD                        
025100     MOVE FEED-IDARTNR-FINANCE(1:20) TO VIPSUT-IDARTNR20                  
025200     MOVE FEED-BEART            TO VIPSUT-BEART                           
025300     MOVE FEED-REARTRAB         TO VIPSUT-REARTRAB-VIPS                   
025400     MOVE FEED-KVLEVART         TO VIPSUT-KVLEVART-VIPS                   
025500     MOVE FEED-PRARTBTO         TO VIPSUT-PRARTBTO-VIPS                   
025600                                   VIPSUT-SUNTO-VIPS                      
025700     MOVE FEED-PRARTNTO         TO VIPSUT-PRARTNTO-VIPS                   
025800     MOVE FEED-IDEXCUST(3)      TO FEED-IDDC                              
025900     IF FEED-IDDC = SPACE                                                 
026000       MOVE FEED-KDVAT          TO VIPSUT-KDVAT                           
026100     ELSE                                                                 
026200       MOVE FEED-IDDC           TO VIPSUT-KDVAT                           
026300     END-IF                                                               
026400     MOVE ' '                   TO VIPSUT-KDVALISO-LOC                    
026500     MOVE 0                     TO VIPSUT-PRKURS                          
026600     MOVE 0                     TO VIPSUT-IDFINDOC                        
026700     MOVE FEED-BELEVVIL         TO VIPSUT-BELEVVIL                        
026800                                                                          
026900     PERFORM S90-PUT-LINE                                                 
027000     .                                                                    
027100     EJECT                                                                
027200                                                                          
027300 Z-FINIT SECTION.                                                         
027400     IF FIRST-TIME-SW = NOO                                               
027500       PERFORM S90-SEND-CLOSE                                             
027600     END-IF                                                               
027700     .                                                                    
027800     EJECT                                                                
027900                                                                          
028000 S01-READ-WF2017  SECTION.                                                
028100     READ WF2017 INTO IN-AREA                                             
028200       AT END                                                             
028300         SET END-OF-WF2017 TO TRUE                                        
028400     END-READ                                                             
028500     .                                                                    
028600     EJECT                                                                
028700                                                                          
028800 S90-SEND-OPEN SECTION.                                                   
028900     MOVE WS-ADRESS                       TO SEND-ADDISPABS               
029000     MOVE 'OPEN'                          TO SEND-KDFUNC                  
029100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
029200                         SEND-OPEN-AREA                                   
029300     IF SEND-KDRC > ZERO                                                  
029400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
029500       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
029600       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
029700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
029800     END-IF                                                               
029900     .                                                                    
030000                                                                          
030100 S90-PUT-HEADER SECTION.                                                  
030200     MOVE 'PUT'                           TO SEND-KDFUNC                  
030300     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
030400     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
030500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
030600                         SEND-KVDLEN                                      
030700                         HDR-AREA                                         
030800     IF SEND-KDRC > ZERO                                                  
030900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
031000       STRING 'WZ01SEND PUT HDR ERROR RC=' KDRC-DISPLAY                   
031100       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
031200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031300     END-IF                                                               
031400     .                                                                    
031500                                                                          
031600 S90-PUT-LINE SECTION.                                                    
031700     MOVE 'PUT'                           TO SEND-KDFUNC                  
031800     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
031900     MOVE LENGTH OF LINE-AREA             TO SEND-KVDLEN                  
032000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
032100                         SEND-KVDLEN                                      
032200                         LINE-AREA                                        
032300     IF SEND-KDRC > ZERO                                                  
032400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
032500       STRING 'WZ01SEND PUT LINE ERROR RC=' KDRC-DISPLAY                  
032600       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
032700       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
032800     END-IF                                                               
032900     .                                                                    
033000                                                                          
033100 S90-SEND-CLOSE SECTION.                                                  
033200     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
033300     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
033400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
033500     .                                                                    
