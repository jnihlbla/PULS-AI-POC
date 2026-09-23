000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     WL018900                                                 
000400 AUTHOR.         GÖRAN KJELLSON   GUIDE                                   
000500 DATE-WRITTEN.   FEBRUARI 2006                                            
000600 DATE-COMPILED.                                                           
000700*    NAME:       'CARPARTS.LDC.TRANSPSUPPL2BG'                            
000800*                                                                         
000900*        WL018900 PROGRAM IS A SIMPLIFIED AND MODIFIED COPY OF            
001000*        THE W4052100 PROGRAM                                             
001100*                                                                         
001200*                                                                         
001300*    FUNKTION.                                                            
001400*        DETTA PROGRAM LÄGGER UT 'EJ FAKTURERADE ORDER'.                  
001500*        LÄSER DATABASERNA WDE6G1, WDE6 OCH WDE4E1.                       
001600*        NYCKLAR ÄR PERSONKOD, ORDERSTATUS, FRAKTKOD, DISTRIKT            
001700*        OCH KUNDNR. PERSONKOD OCH ORDERSTATUS ANVÄNDS VID                
001800*        LÄSNING. ÖVRIGA NYCKLAR TESTAS MOT INLÄSTA VÄRDEN FRÅN           
001900*        ROTSEGMENTET.                                                    
002000*                                                                         
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: WL0189T                                             
002400*        MID:         WL0189I1                                            
002500*                                                                         
002600*    UTDATA.                                                              
002700*        MOD:         WL0189O1                                            
002800                                                                          
002900                                                                          
003000 ENVIRONMENT DIVISION.                                                    
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400 77  IDPGM                       PIC X(08)  VALUE 'WL018900'.             
003500 77  FILLER                      PIC X(08)  VALUE 'FELTEXT:'.             
003600 77  FELTEXT                     PIC X(80)  VALUE SPACE.                  
003700 77  JA                          PIC X      VALUE 'J'.                    
003800 77  YES                         PIC X      VALUE 'Y'.                    
003900 77  NEJ                         PIC X      VALUE 'N'.                    
004000                                                                          
004100 77  WS-KVRADER                  PIC S9(3)  VALUE +0    COMP-3.           
004200                                                                          
004300 77  INDX                        PIC S9(9)  VALUE ZERO.                   
004400 77  CL-INDEX                    PIC S9(9)  VALUE ZERO.                   
004500 77  MAX-LINE                    PIC S9(9)  VALUE +500.                   
004600 77  TRP-INDX                    PIC 9(9)   VALUE ZERO.                   
004700 77  MAX-TRP-INDX                PIC 9(9)   VALUE 100.                    
004800 77  CURR-SECT                   PIC X(08)  VALUE 'CURRSECT'.             
004900 77  CURR-SECTION                PIC X(24)  VALUE SPACE.                  
005000 77  CURR-IMS                    PIC X(08)  VALUE 'CURRIMS '.             
005100 77  CURR-IMS-SECTION            PIC X(24)  VALUE SPACE.                  
005200 77  KDRC-DISPLAY                PIC Z(5).                                
005300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005400                                                                          
005500 77  WS-IDDISTR                  PIC X(4)    VALUE SPACE.                 
005600 77  WS-IDPRODNR                 PIC S9(7)   COMP-3 VALUE ZERO.           
005700                                                                          
005800 77  W-IDDISTR                   PIC S9(5)   VALUE ZERO  COMP-3.          
005900 77  W-IDDC                      PIC  X(2)   VALUE SPACE.                 
006000 01  W-IDKUNDRF                  PIC X(10).                               
006100 01  FILLER  REDEFINES  W-IDKUNDRF.                                       
006200     03  W-IDKUNDRF-1-5          PIC 9(5).                                
006300     03  FILLER                  PIC 9(5).                                
006400                                                                          
006500 77  NYCKLAR-SW                  PIC X       VALUE 'J'.                   
006600     88  NYCKLAR-OK                          VALUE 'J'.                   
006700     88  NYCKLAR-FEL                         VALUE 'N'.                   
006800                                                                          
006900*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007000 01  GENERELLA-SUBPROGRAM.                                                
007100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007300     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007500                                                                          
007510                                                                          
007520 01  MESSAGE-CODES.                                                       
007530     03  ERR-IS-INVALID          PIC X(3)    VALUE '023'.                 
007540     03  ERR-LINES-NOT-FOUND     PIC X(3)    VALUE '027'.                 
007600*    --- AREOR FÖR WEBKOMMUNIKATION                                       
007700*                                                                         
007800*                                                                         
007900 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
008000*01  -COPY WZ01SUB                                                        
008100                                                                          
008200 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
008300 01  REQU-AREA.                                                           
008400*    03  -COPY WZ01REQU                                                   
008500*    03  -COPY WL0189I1                                                   
008600     EJECT                                                                
008700 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
008800     SKIP3                                                                
008900 01  RESP-AREA.                                                           
009000*    03  -COPY WZ01RESP                                                   
009100*    03  -COPY WL0189O1                                                   
009200                                                                          
009300                                                                          
009400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009500                                                                          
009600 01  NYCKLAR-TILL-DLI.                                                    
009700     03  W-WDE6GSEQ-MIN-X.                                                
009800         05  W-IDDC-MIN-E6GSEQ PIC  X(2)    VALUE SPACE.                  
009900         05  W-IDDISTR-MIN-E6GSEQ PIC S9(5) VALUE ZERO  COMP-3.           
010000                                                                          
010100     03  W-WDE6GSEQ-MAX-X.                                                
010200         05  W-IDDC-MAX-E6GSEQ PIC  X(2)    VALUE SPACE.                  
010300         05  W-IDDISTR-MAX-E6GSEQ PIC S9(5) VALUE ZERO  COMP-3.           
010400                                                                          
010500     03    W-WDE4E1KY-MIN-X.                                              
010600         05  W-IDPRODNR-WDE4E-MIN  PIC S9(7)   VALUE ZERO  COMP-3.        
010700         05  FILLER                PIC X(19)   VALUE LOW-VALUE.           
010800                                                                          
010900     03    W-WDE4E1KY-MAX-X.                                              
011000         05  W-IDPRODNR-WDE4E-MAX  PIC S9(7)   VALUE ZERO  COMP-3.        
011100         05  FILLER                PIC X(19)   VALUE HIGH-VALUE.          
011200                                                                          
011300     03  W-IDPRODNR-X.                                                    
011400         05  W-IDPRODNR        PIC S9(7)    VALUE ZERO  COMP-3.           
011500                                                                          
011600     03  W-IDTRPTNR-X.                                                    
011700         05  W-IDTRPTNR        PIC S9(3)    VALUE ZERO  COMP-3.           
011800                                                                          
011900                                                                          
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FINNS                       VALUE '  '.                  
012300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012500                                                                          
012600 01  GODK-STATUSKODER.                                                    
012700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800                                                                          
012900 01  SSA1                        PIC X(128).                              
013000     EJECT                                                                
013100*    --- IMS FUNKTIONSKODER                                               
013200*01  -COPY W0003                                                          
013300     EJECT                                                                
013400*    ---  DLI INPUT-OUTPUT AREA                                           
013500 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
013600                                                                          
013700 01  FILLER                      PIC X(16)   VALUE 'WDE601-AREA'.         
013800 01  DLI-IO-AREA-WDE601.                                                  
013900*    03  -COPY WDE601                                                     
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE 'WDE611-AREA'.         
014200 01  DLI-IO-AREA-WDE611.                                                  
014300*    03  -COPY WDE611                                                     
014400                                                                          
014500 01  FILLER                      PIC X(16)   VALUE 'WDE4E1-AREA'.         
014600 01  DLI-IO-E4E1.                                                         
014700*    03  -COPY WDE4E1                                                     
014800                                                                          
014900 LINKAGE SECTION.                                                         
015000                                                                          
015100*01  -COPY W0009   -PRE MSG-                                              
015200                                                                          
015300*01  -COPY W0008  -PRE WDE6-                                              
015400     05  FILLER                  PIC X.                                   
015500                                                                          
015600*01  -COPY W0008  -PRE WDE4E-                                             
015700     05  FILLER                  PIC X.                                   
015800                                                                          
015900 PROCEDURE DIVISION  USING MSG-PCB WDE6-PCB WDE4E-PCB.                    
016000     ENTRY 'DLITCBL' USING MSG-PCB WDE6-PCB WDE4E-PCB.                    
016100                                                                          
016200     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
016300                                                                          
016400     IF SUB-KDRC = 0                                                      
016500        PERFORM A-INIT                                                    
016600        PERFORM B-KOLLA-NYCKLAR                                           
016700                                                                          
016800        IF NYCKLAR-OK                                                     
016900           PERFORM C-LAES-VISA-INFO                                       
017000        END-IF                                                            
017100                                                                          
017200        PERFORM S02-RETURN-RESPONSE                                       
017300     END-IF                                                               
017400                                                                          
017500     MOVE ZERO TO RETURN-CODE                                             
017600     GOBACK                                                               
017700     .                                                                    
017800                                                                          
017900                                                                          
018000 A-INIT                         SECTION.                                  
018100     MOVE 'A-INIT' TO CURR-SECTION                                        
018200                                                                          
018300     MOVE ALL '+'   TO RESP-AREA                                          
018400     MOVE SPACE     TO RESP-IDMSG-ERROR                                   
018500                       RESP-IDMSG-INFO                                    
018600                       RESP-IDELMT-ERROR                                  
018700     MOVE 001       TO RESP-IDMSGVER                                      
018800     MOVE ALL '+'   TO RESP-WL0189O1                                      
018900     MOVE ZERO      TO RESP-KVRADER                                       
019000                       WS-KVRADER                                         
019100                       W-IDTRPTNR                                         
019200     .                                                                    
019300                                                                          
019400 B-KOLLA-NYCKLAR                SECTION.                                  
019500     MOVE 'B-KOLLA-NYCKLAR' TO CURR-SECTION                               
019600                                                                          
019700     MOVE JA                 TO NYCKLAR-SW                                
019800                                                                          
019900     MOVE REQU-IDDISTR-KEY  TO WS-IDDISTR                                 
020000     INSPECT WS-IDDISTR REPLACING LEADING SPACE BY ZERO                   
020100                                                                          
020200     IF WS-IDDISTR NOT NUMERIC OR                                         
020300        WS-IDDISTR   = ALL '+'                                            
020400                                                                          
020410       MOVE ZERO             TO REQU-IDDISTR-KEY                          
020500*      MOVE NEJ              TO NYCKLAR-SW                                
020600*      MOVE ERR-IS-INVALID   TO RESP-IDMSG-ERROR                          
020700*       IS INVALID ***                                                    
020800*      MOVE 'IDDISTR'        TO RESP-IDELMT-ERROR                         
020900     END-IF                                                               
021000                                                                          
021100     IF NOT REQU-IDDISTR-KEY = ALL '+'                                    
021200        MOVE REQU-IDDISTR-KEY  TO RESP-IDDISTR-KEY                        
021300     END-IF                                                               
021400                                                                          
021500     IF NYCKLAR-OK                                                        
021600       MOVE REQU-IDDISTR-KEY   TO W-IDDISTR                               
021700       MOVE REQU-IDDC-KEY      TO W-IDDC                                  
021800                                                                          
021900       PERFORM BA-FLYTTA-NYCKLAR                                          
022000     END-IF                                                               
022100     .                                                                    
022200                                                                          
022300                                                                          
022400 BA-FLYTTA-NYCKLAR SECTION.                                               
022500     MOVE 'BA-FLYTTA-NYCKLAR' TO CURR-SECTION                             
022600                                                                          
022700     MOVE LOW-VALUE TO W-WDE6GSEQ-MIN-X                                   
022800                                                                          
022900     MOVE HIGH-VALUE TO W-WDE6GSEQ-MAX-X                                  
023000                                                                          
023100     IF W-IDDISTR  > ZERO                                                 
023200       MOVE W-IDDISTR   TO W-IDDISTR-MIN-E6GSEQ                           
023300                           W-IDDISTR-MAX-E6GSEQ                           
023400     ELSE                                                                 
023500       MOVE ZERO        TO W-IDDISTR-MIN-E6GSEQ                           
023600       MOVE +99999      TO W-IDDISTR-MAX-E6GSEQ                           
023700     END-IF                                                               
023800                                                                          
023900     MOVE W-IDDC      TO W-IDDC-MIN-E6GSEQ                                
024000                         W-IDDC-MAX-E6GSEQ                                
024100     .                                                                    
024200                                                                          
024300                                                                          
024400 C-LAES-VISA-INFO               SECTION.                                  
024500     MOVE 'C-LAES-VISA-INFO' TO CURR-SECTION                              
024600                                                                          
024700     PERFORM IMS-01-GU-WDE601-GSEQ                                        
024800     IF SEGMENT-SAKNAS                                                    
024900       MOVE ERR-LINES-NOT-FOUND  TO RESP-IDMSG-ERROR                      
025000*      NOT FOUND  ***                                                     
025100       MOVE 'IDDISTR'    TO RESP-IDELMT-ERROR                             
025200     END-IF                                                               
025300                                                                          
025400     MOVE +1   TO INDX                                                    
025500     MOVE ZERO TO WS-KVRADER                                              
025600                                                                          
025700     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT OR                      
025800                   INDX > MAX-LINE                                        
025900                                                                          
026000       PERFORM IMS-03-GNP-WDE611                                          
026010       IF SEGMENT-FINNS                                                   
026100         PERFORM CC-FLYTTA-TILL-RESP                                      
026200         PERFORM CD-READ-WDE4-AND-MOVE-RESP                               
026300                                                                          
026400         ADD +1 TO INDX                                                   
026500         ADD +1 TO WS-KVRADER                                             
026510       END-IF                                                             
026600                                                                          
026700       PERFORM IMS-02-GN-WDE601-GSEQ                                      
026800     END-PERFORM                                                          
026900     MOVE WS-KVRADER  TO RESP-KVRADER                                     
026910                                                                          
026920     IF WS-KVRADER = ZERO                                                 
026921       MOVE ERR-LINES-NOT-FOUND  TO RESP-IDMSG-ERROR                      
026922*      NOT FOUND  ***                                                     
026923       MOVE 'IDDISTR'    TO RESP-IDELMT-ERROR                             
026930     END-IF                                                               
027000     .                                                                    
027100     EJECT                                                                
027200                                                                          
027300 CC-FLYTTA-TILL-RESP SECTION.                                             
027400     MOVE 'CC-FLYTTA-TILL-RESP' TO CURR-SECTION                           
027500                                                                          
027600     MOVE VORD-IDDISTR        TO RESP-IDDISTR  (INDX)                     
027700     MOVE VORD-IDKUNDNR       TO RESP-IDKUNDNR (INDX)                     
027800     MOVE VORD-IDPRODNR       TO RESP-IDPRODNR (INDX)                     
027900     MOVE VORD-DARFS (3:10)   TO RESP-TIRFS    (INDX)                     
028000     MOVE KOLLI-IDTRPTNR      TO RESP-IDTRPTNR (INDX)                     
028100                                                                          
028200     .                                                                    
028300     EJECT                                                                
028400 CD-READ-WDE4-AND-MOVE-RESP  SECTION.                                     
028500     MOVE 'CD-READ-WDE4-AND-MOVE-RESP' TO CURR-SECTION                    
028600                                                                          
028700     MOVE VORD-IDPRODNR       TO W-IDPRODNR-WDE4E-MIN                     
028800                                 W-IDPRODNR-WDE4E-MAX                     
028900     PERFORM IMS-04-GU-WDE4E                                              
029000                                                                          
029100     MOVE SEQE-IDKUNDRF       TO W-IDKUNDRF                               
029200     MOVE W-IDKUNDRF-1-5      TO RESP-IDORDNR (INDX)                      
029300                                                                          
029400     .                                                                    
029500     EJECT                                                                
029600*    --- DISPATCHER SECTIONS                                              
029700 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
029800     MOVE 'S01-FETCH-REQUEST-ARGUMENT' TO CURR-SECTION                    
029900                                                                          
030000     MOVE 'GETARG'               TO SUB-KDFUNC                            
030100     MOVE 'CARPARTS.LDC.TRANSPSUPPL2BG' TO SUB-ADDISPABS                  
030200     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
030300                                                                          
030400     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
030500                                                                          
030600     IF SUB-KDRC > 0                                                      
030700       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
030800       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
030900       DELIMITED BY SIZE INTO FELTEXT                                     
031000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
031100     END-IF                                                               
031200     .                                                                    
031300                                                                          
031400 S02-RETURN-RESPONSE SECTION.                                             
031500     MOVE 'S02-RETURN-RESPONSE  ' TO CURR-SECTION                         
031600                                                                          
031700     MOVE 'RETURN'                   TO SUB-KDFUNC                        
031800     MOVE LENGTH OF RESP-AREA        TO SUB-KVDLEN                        
031900                                                                          
032000     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
032100                                                                          
032200     IF SUB-KDRC > 0                                                      
032300       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
032400       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
032500       DELIMITED BY SIZE INTO FELTEXT                                     
032600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
032700     END-IF                                                               
032800     .                                                                    
032900                                                                          
033000* --- IMS SEKTIONER ---                                                   
033100                                                                          
033200 IMS-01-GU-WDE601-GSEQ SECTION.                                           
033300     MOVE 'IMS-01-GU-WDE601-GSEQ' TO CURR-IMS-SECTION                     
033400                                                                          
033500     STRING 'WDE601  (WDE6GSEQ>=' W-WDE6GSEQ-MIN-X                        
033600                    '&WDE6GSEQ<=' W-WDE6GSEQ-MAX-X ')'                    
033700            DELIMITED BY SIZE INTO SSA1                                   
033800     MOVE '  GE' TO GODK-STATUSKODER                                      
033900     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-AREA-WDE601 SSA1               
034000     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
034100     PERFORM IMS-STATUSKONTROLL                                           
034200     .                                                                    
034300                                                                          
034400 IMS-02-GN-WDE601-GSEQ SECTION.                                           
034500     MOVE 'IMS-02-GN-WDE601-GSEQ' TO CURR-IMS-SECTION                     
034600                                                                          
034700     STRING 'WDE601  (WDE6GSEQ>=' W-WDE6GSEQ-MIN-X                        
034800                    '&WDE6GSEQ<=' W-WDE6GSEQ-MAX-X ')'                    
034900            DELIMITED BY SIZE INTO SSA1                                   
035000     MOVE '  GBGE' TO GODK-STATUSKODER                                    
035100     CALL CBLTDLI USING GN WDE6-PCB DLI-IO-AREA-WDE601 SSA1               
035200     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
035300     PERFORM IMS-STATUSKONTROLL                                           
035400     .                                                                    
035500                                                                          
035600                                                                          
035700 IMS-03-GNP-WDE611 SECTION.                                               
035800     MOVE 'IMS-03-GNP-WDE611 ' TO CURR-IMS-SECTION                        
035900                                                                          
036000     STRING 'WDE611  (IDTRPTNR>=' W-IDTRPTNR-X ')'                        
036100            DELIMITED BY SIZE INTO SSA1                                   
036200     MOVE '  GE'      TO GODK-STATUSKODER                                 
036300     CALL CBLTDLI USING GNP WDE6-PCB DLI-IO-AREA-WDE611 SSA1              
036400     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
036500     PERFORM IMS-STATUSKONTROLL                                           
036600     .                                                                    
036700                                                                          
036800                                                                          
036900 IMS-04-GU-WDE4E SECTION.                                                 
037000     MOVE 'IMS-04' TO CURR-IMS-SECTION                                    
037100                                                                          
037200     STRING 'WDE4E1  (WDE4E1KY>=' W-WDE4E1KY-MIN-X                        
037300                    '&WDE4E1KY<=' W-WDE4E1KY-MAX-X ')'                    
037400          DELIMITED BY SIZE INTO SSA1                                     
037500     MOVE '  GE' TO GODK-STATUSKODER                                      
037600     CALL CBLTDLI USING GU WDE4E-PCB DLI-IO-E4E1 SSA1                     
037700     MOVE WDE4E-STATUS-CODE TO STATUS-WS                                  
037800     PERFORM IMS-STATUSKONTROLL                                           
037900     .                                                                    
038000                                                                          
038100                                                                          
038200 IMS-STATUSKONTROLL             SECTION.                                  
038300                                                                          
038400     SET STATUS-IX TO 1                                                   
038500     SEARCH GODK-STATUS                                                   
038600       AT END                                                             
038700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
038800         DELIMITED BY SIZE INTO FELTEXT                                   
038900         CALL FELLOG                                                      
039000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
039100         CONTINUE                                                         
039200     END-SEARCH                                                           
039300     .                                                                    
