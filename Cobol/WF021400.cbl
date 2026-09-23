000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF021400.                                                
000400 AUTHOR.         HENRIKSSON ANDERS.                                       
000500 DATE-WRITTEN.   02/02/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME                                                                 
000900*        CARPARTS.BILLIT.RECEIVE5                                         
001000*    FUNCTION:                                                            
001100*      - RECEIVES LINES FROM THE COMMUNICATION REGISTER                   
001200*      - READS ALL LINES VIA WZ01RECV                                     
001300*      - STORES ALL LINES IN TEMPORARY LINE REGISTER                      
001400*      - PGM WF020200 (CARPARTS.BILLIT.VALIDATE) WILL BE STARTED          
001500*        IF NO UNCONTROLLED BUNDLES EXISTS                                
001600*        (EXCEPT THE BUNDLE WICH JUST HAS BEEN PROCESSED)                 
001700*                                                                         
001800*        THE PROGRAM INSERTS/UPDATES ROWS IN TABLE T01TRAW                
001900*        THE PROGRAM INSERTS         ROWS IN TABLE T01TBUN                
002000*        THE PROGRAM READS           ROWS IN TABLE T01SYST                
002100*                                                                         
002200*    INDATA.                                                              
002300*        TRANSAKTION: WF0214X                                             
002400*        REQUEST:     HEADER ONLY                                         
002500*                                                                         
002600*    OUTDATA.                                                             
002700*        TRANSAKTION: WF0202X                                             
002800*        REQUEST:     HEADER ONLY                                         
002900*                                                                         
003000*        SÄNDNING VIA WZ01  TILL MAIL                                     
003100*        MOD:         WZ01MAIL (VIA WZ01)                                 
003200                                                                          
003300 ENVIRONMENT DIVISION.                                                    
003400     EJECT                                                                
003500                                                                          
003600 DATA DIVISION.                                                           
003700 WORKING-STORAGE SECTION.                                                 
003800 77  IDPGM                       PIC X(08)   VALUE 'WF021400'.            
003900                                                                          
004000*    --- WORK FIELD FOR ERROR MESSAGES CALLING ABEND                      
004100 77  ERRORTEXT                   PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300                                                                          
004400 77  WS-IX2                      PIC S9(7) VALUE +0     COMP-3.           
004500 77  MAX-LINES                   PIC S9(7) VALUE +80000 COMP-3.           
004600                                                                          
004700 77  YES                         PIC X       VALUE 'J'.                   
004800 77  NOO                         PIC X       VALUE 'N'.                   
004900 77  WS-BUNDLES-NOT-CONTROLLED   PIC X.                                   
005000 77  WS-TIME-WAIT                PIC S9(9)   COMP VALUE +0001.            
005100     EJECT                                                                
005200                                                                          
005300*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005600     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
005700     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
005800     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
005900                                                                          
006000*    --- PARAMETERS TO ABEND                                              
006100                                                                          
006200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006400 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
006500 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006600     EJECT                                                                
006700                                                                          
006800*    --- AREAS FOR COMMUNICATION                                          
006900 01  FILLER                      PIC X(16)   VALUE 'RECV-CONTROL'.        
007000                                                                          
007100 01  -COPY WZ01RECV                                                       
007200     EJECT                                                                
007300                                                                          
007400 01  FILLER                      PIC X(16)   VALUE 'RECV-AREA'.           
007500 01  RECV-AREA.                                                           
007600*    03  -COPY WZ01REQU -PRE IN-                                          
007700*    03  -COPY WF0214I1 -PRE MID-WF0214I1-                                
007800     EJECT                                                                
007900                                                                          
008000 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
008100 01  -COPY WZ01SEND                                                       
008200     EJECT                                                                
008300                                                                          
008400 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
008500 01  SEND-AREA.                                                           
008600*    03  -COPY WZ01REQU                                                   
008700     EJECT                                                                
008800                                                                          
008900 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
009000       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009100                                                                          
009200 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
009300 01  DB2-WS.                                                              
009400     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
009500         88  INSERT-OK                       VALUE 000.                   
009600         88  CURSOR-OK                       VALUE 000.                   
009700         88  LINES-FOUND                     VALUE 000.                   
009800         88  LINES-MISSING                   VALUE 100.                   
009900         88  RESOURCE-WRONG                  VALUE 904.                   
010000                                                                          
010100     03  GOOD-SQLCODECODES.                                               
010200         05  GOOD-SQLCODE OCCURS 5                                        
010300             INDEXED BY SQLCODE-IX PIC 9(3).                              
010400     EJECT                                                                
010500                                                                          
010600 01  FILLER                    PIC X(16) VALUE 'WS-AREA         '.        
010700 01  WS-AREA.                                                             
010800     03 WS-DATUM               PIC X(8)  VALUE SPACE.                     
010900     03 WS-KLOCKAN             PIC 9(10) VALUE ZERO.                      
011000     03 WS-KLOCKAN-COMP        PIC S9(10) COMP-3 VALUE ZERO.              
011100                                                                          
011200     03 WS-IDLEGSEL            PIC X(4)  VALUE SPACE.                     
011300     03 WS-IDBUNDLE            PIC X(15) VALUE SPACE.                     
011400     03 WS-DAREGDAT            PIC X(8)  VALUE SPACE.                     
011500     03 WS-TIREGTID            PIC S9(10) COMP-3 VALUE ZERO.              
011600     03 WS-IDREF               PIC X(15) VALUE SPACE.                     
011700     03 WS-DAREFDAT            PIC X(8)  VALUE SPACE.                     
011800     03 WS-IDREFRAD            PIC S9(5) COMP-3 VALUE ZERO.               
011900     03 WS-BEVOLREF            PIC X(10) VALUE SPACE.                     
012000     03 WS-IDLANDX3-SEND       PIC X(3)  VALUE SPACE.                     
012100     03 WS-IDLANDX3-REC        PIC X(3)  VALUE SPACE.                     
012200     03 WS-IDLEVNR             PIC X(5)  VALUE SPACE.                     
012300     03 WS-IDPARTNR            PIC X(9)  VALUE SPACE.                     
012400                                                                          
012500     03 WS-IDEXCUST-1          PIC X(15) VALUE SPACE.                     
012600     03 WS-IDEXCUST-2          PIC X(15) VALUE SPACE.                     
012700     03 WS-IDEXCUST-3          PIC X(15) VALUE SPACE.                     
012800     03 WS-IDOPTION-1          PIC X(15) VALUE SPACE.                     
012900     03 WS-IDOPTION-2          PIC X(15) VALUE SPACE.                     
013000     03 WS-IDOPTION-3          PIC X(15) VALUE SPACE.                     
013100     03 WS-IDOPTION-4          PIC X(15) VALUE SPACE.                     
013200     03 WS-IDOPTION-5          PIC X(15) VALUE SPACE.                     
013300     03 WS-IDAPPEND            PIC X(8)  VALUE SPACE.                     
013400     03 WS-IDARTNR-FINANCE     PIC X(50) VALUE SPACE.                     
013500     03 WS-IDSTATNR            PIC S9(9) COMP-3 VALUE ZERO.               
013600     03 WS-VKORDBTO-KOLLI      PIC S9(6)V9(1) COMP-3 VALUE ZERO.          
013700     03 WS-VKARTNTO            PIC S9(4)V9(3) COMP-3 VALUE ZERO.          
013800     03 WS-PRARTBTO            PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
013900     03 WS-PRARTNTO            PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
014000     03 WS-REARTRAB            PIC S9(2)V9(2) COMP-3 VALUE ZERO.          
014100                                                                          
014200     03 WS-KVBEART             PIC S9(7) COMP-3 VALUE ZERO.               
014300     03 WS-KVLEVART            PIC S9(7) COMP-3 VALUE ZERO.               
014400     03 WS-BEART               PIC X(25) VALUE SPACE.                     
014500     03 WS-FLSOFT              PIC X(1)  VALUE SPACE.                     
014600     03 WS-FLSPECPR            PIC X(1)  VALUE SPACE.                     
014700     03 WS-FLFREE              PIC X(1)  VALUE SPACE.                     
014800     03 WS-FLPRIV              PIC X(1)  VALUE SPACE.                     
014900     03 WS-KDVAT               PIC X(2)  VALUE SPACE.                     
015000     03 WS-KDVALISO            PIC X(3)  VALUE SPACE.                     
015100     03 WS-KDINVFRQ            PIC X(4)  VALUE SPACE.                     
015200     03 WS-KDFINDOC            PIC X(4)  VALUE SPACE.                     
015300     03 WS-IDBREAK-1           PIC X(8)  VALUE SPACE.                     
015400     03 WS-IDBREAK-2           PIC X(8)  VALUE SPACE.                     
015500                                                                          
015600     03 WS-IDSEQ-1             PIC X(8)  VALUE SPACE.                     
015700     03 WS-IDSEQ-2             PIC X(8)  VALUE SPACE.                     
015800     03 WS-IDSEQ-3             PIC X(8)  VALUE SPACE.                     
015900     03 WS-KDARTURS            PIC X(2)  VALUE SPACE.                     
016000     03 WS-KDANMORS            PIC X(2)  VALUE SPACE.                     
016100     03 WS-IDFAKREF            PIC S9(9) COMP-3 VALUE ZERO.               
016200     03 WS-DAFAKREF            PIC 9(8)  VALUE ZERO.                      
016300     03 WS-DAFAKREF-2          PIC X(8)  VALUE SPACE.                     
016400     03 WS-IDDC                PIC X(2)  VALUE SPACE.                     
016500     03 WS-KDFRAKT             PIC S9(3) COMP-3 VALUE ZERO.               
016600     03 WS-BELEVVIL            PIC X(35) VALUE SPACE.                     
016700     03 WS-IDACCNT-1           PIC X(15) VALUE SPACE.                     
016800     03 WS-IDACCNT-2           PIC X(15) VALUE SPACE.                     
016900     03 WS-IDACCNT-3           PIC X(15) VALUE SPACE.                     
017000     03 WS-IDACCNT-4           PIC X(15) VALUE SPACE.                     
017100     03 WS-IDSYSTEM-SEND       PIC X(4)  VALUE SPACE.                     
017200     03 WS-IDSYSTEM-REC        PIC X(4)  VALUE SPACE.                     
017300     03 WS-FILLER              PIC X(100) VALUE SPACE.                    
017400     03 WS-BEANST              PIC X(25) VALUE SPACE.                     
017500     03 WS-IDUSER              PIC X(8)  VALUE SPACE.                     
017600     03 WS-BETEXT              PIC X(125) VALUE SPACE.                    
017700     03 WS-BETEXT-CRE          PIC X(100) VALUE SPACE.                    
017800     03 WS-FLFEL               PIC X(1)  VALUE SPACE.                     
017900     03 WS-IDFELKOD            PIC X(3)  VALUE SPACE.                     
018000     03 WS-BEFEL               PIC X(50) VALUE SPACE.                     
018100     03 WS-IDARTNR-CNTRL       PIC X(2)  VALUE SPACE.                     
018110     03 WS-FLPCOO              PIC X(1)  VALUE SPACE.                     
018120     03 WS-IDLEVNR-ART         PIC X(5)  VALUE SPACE.                     
018130     03 WS-IDTRACK-1           PIC X(25) VALUE SPACE.                     
018140     03 WS-KVANT-TRACK-1       PIC S9(7) COMP-3 VALUE ZERO.               
018150     03 WS-IDTRACK-2           PIC X(25) VALUE SPACE.                     
018160     03 WS-KVANT-TRACK-2       PIC S9(7) COMP-3 VALUE ZERO.               
018170     03 WS-IDTRACK-3           PIC X(25) VALUE SPACE.                     
018180     03 WS-KVANT-TRACK-3       PIC S9(7) COMP-3 VALUE ZERO.               
018190     03 WS-IDTRACK-4           PIC X(25) VALUE SPACE.                     
018191     03 WS-KVANT-TRACK-4       PIC S9(7) COMP-3 VALUE ZERO.               
018192     03 WS-IDTRACK-5           PIC X(25) VALUE SPACE.                     
018193     03 WS-KVANT-TRACK-5       PIC S9(7) COMP-3 VALUE ZERO.               
018194     03 WS-KDPRMOD             PIC X(2)  VALUE SPACE.                     
018200                                                                          
018300     03 WS-ERRORTEXT           PIC X(50) VALUE SPACE.                     
018400                                                                          
018500     03 SYST-KDBEHS            PIC X(1)  VALUE SPACE.                     
018600                                                                          
018700 01  FILLER                    PIC X(16)    VALUE 'T01TRAW-AREA'.         
018800*01  -COPY T01TRAW -PRE T01TRAW-                                          
018900     EJECT                                                                
019000                                                                          
019100 01  FILLER                    PIC X(16)    VALUE 'T01TBUN-AREA'.         
019200*01  -COPY T01TBUN -PRE T01TBUN-                                          
019300     EJECT                                                                
019400                                                                          
019500 01  FILLER                    PIC X(16)    VALUE 'T01SYST-AREA'.         
019600*01  -COPY T01SYST -PRE T01SYST-                                          
019700     EJECT                                                                
019800                                                                          
019900     EXEC SQL INCLUDE T01TRAW END-EXEC.                                   
020000     EJECT                                                                
020100                                                                          
020200     EXEC SQL INCLUDE T01TBUN END-EXEC.                                   
020300     EJECT                                                                
020400                                                                          
020500     EXEC SQL INCLUDE T01SYST END-EXEC.                                   
020600     EJECT                                                                
020700                                                                          
020800 LINKAGE SECTION.                                                         
020900                                                                          
021000 PROCEDURE DIVISION.                                                      
021100 MAIN SECTION.                                                            
021200     PERFORM S01-READ-OPEN                                                
021300                                                                          
021400     PERFORM S02-READ-MESSAGE                                             
021500     MOVE MID-WF0214I1-IDBUNDLE TO WS-IDBUNDLE                            
021600     IF RECV-KDRC = ZERO                                                  
021700       PERFORM A-INIT                                                     
021800       PERFORM UNTIL RECV-KDRC > ZERO OR WS-IX2 > MAX-LINES               
021900         IF MID-WF0214I1-IDBUNDLE NOT = WS-IDBUNDLE                       
022000* MORE THAN ONE BUNDLEID IN THE SAME SESSION NOT ALLOWED                  
022100           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
022200         END-IF                                                           
022300         PERFORM B-LINES                                                  
022400         PERFORM C-INSERT-UPDATE-T01TRAW                                  
022500         PERFORM S02-READ-MESSAGE                                         
022600       END-PERFORM                                                        
022700* TO MANY LINES ADD MORE LINES TO MAX-LINES OR A RESTART                  
022800       IF WS-IX2 > MAX-LINES                                              
022900         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
023000       END-IF                                                             
023100       PERFORM D-INSERT-T01TBUN                                           
023200                                                                          
023300* CHECKS IF PROGRAM WF020200 IS STILL RUNNING                             
023400       PERFORM DB2-SELECT-T01SYST                                         
023500       IF SYST-KDBEHS = SPACE                                             
023600         PERFORM E-START-PGM-WF020200                                     
023700       ELSE                                                               
023800* THE PROCESS IN WF020200 WAS RUNNING AND                                 
023900* BLOCK WF021400 FROM STARTING WF020200                                   
024000         MOVE 'F' TO SYST-KDBEHS                                          
024100         PERFORM DB2-UPDATE-T01SYST                                       
024200       END-IF                                                             
024300     END-IF                                                               
024400                                                                          
024500     PERFORM S03-READ-CLOSE                                               
024600     MOVE ZERO TO RETURN-CODE                                             
024700     GOBACK                                                               
024800     .                                                                    
024900     EJECT                                                                
025000                                                                          
025100 A-INIT SECTION.                                                          
025200     CALL W009WAIT USING WS-TIME-WAIT                                     
025300     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-DATUM                        
025400     MOVE FUNCTION CURRENT-DATE (9:8)  TO WS-KLOCKAN                      
025500     MOVE WS-KLOCKAN                   TO WS-KLOCKAN-COMP                 
025600                                                                          
025700     MOVE 'N'                          TO WS-FLFEL                        
025800     MOVE SPACE                        TO WS-IDFELKOD                     
025900                                          WS-BEFEL                        
026000     MOVE ZERO                         TO WS-IX2                          
026100                                                                          
026200     INITIALIZE GOOD-SQLCODECODES                                         
026300     .                                                                    
026400     EJECT                                                                
026500                                                                          
026600 B-LINES SECTION.                                                         
026700     MOVE MID-WF0214I1-IDLEGSEL        TO WS-IDLEGSEL                     
026800     MOVE MID-WF0214I1-IDBUNDLE        TO WS-IDBUNDLE                     
026900     MOVE MID-WF0214I1-IDREF           TO WS-IDREF                        
027000     MOVE MID-WF0214I1-DAREFDAT        TO WS-DAREFDAT                     
027100     MOVE MID-WF0214I1-IDREFRAD        TO WS-IDREFRAD                     
027200     MOVE MID-WF0214I1-BEVOLREF        TO WS-BEVOLREF                     
027300     MOVE MID-WF0214I1-IDLANDX3-SEND   TO WS-IDLANDX3-SEND                
027400     MOVE MID-WF0214I1-IDLANDX3-REC    TO WS-IDLANDX3-REC                 
027500     MOVE MID-WF0214I1-IDLEVNR         TO WS-IDLEVNR                      
027600     MOVE MID-WF0214I1-IDPARTNR        TO WS-IDPARTNR                     
027700     MOVE MID-WF0214I1-IDEXCUST(1)     TO WS-IDEXCUST-1                   
027800     MOVE MID-WF0214I1-IDEXCUST(2)     TO WS-IDEXCUST-2                   
027900     MOVE MID-WF0214I1-IDEXCUST(3)     TO WS-IDEXCUST-3                   
028000     MOVE MID-WF0214I1-IDOPTION(1)     TO WS-IDOPTION-1                   
028100     MOVE MID-WF0214I1-IDOPTION(2)     TO WS-IDOPTION-2                   
028200     MOVE MID-WF0214I1-IDOPTION(3)     TO WS-IDOPTION-3                   
028300     MOVE MID-WF0214I1-IDOPTION(4)     TO WS-IDOPTION-4                   
028400     MOVE MID-WF0214I1-IDOPTION(5)     TO WS-IDOPTION-5                   
028500     MOVE MID-WF0214I1-IDAPPEND        TO WS-IDAPPEND                     
028600     MOVE MID-WF0214I1-IDARTNR-FINANCE TO WS-IDARTNR-FINANCE              
028700     MOVE MID-WF0214I1-IDSTATNR        TO WS-IDSTATNR                     
028800     MOVE MID-WF0214I1-VKORDBTO-KOLLI  TO WS-VKORDBTO-KOLLI               
028900     MOVE MID-WF0214I1-VKARTNTO        TO WS-VKARTNTO                     
029000     MOVE MID-WF0214I1-PRARTBTO        TO WS-PRARTBTO                     
029100     MOVE MID-WF0214I1-PRARTNTO        TO WS-PRARTNTO                     
029200     MOVE MID-WF0214I1-REARTRAB        TO WS-REARTRAB                     
029300     MOVE MID-WF0214I1-KVBEART         TO WS-KVBEART                      
029400     MOVE MID-WF0214I1-KVLEVART        TO WS-KVLEVART                     
029500     MOVE MID-WF0214I1-BEART           TO WS-BEART                        
029600     MOVE MID-WF0214I1-FLSOFT          TO WS-FLSOFT                       
029700     MOVE MID-WF0214I1-FLSPECPR        TO WS-FLSPECPR                     
029800     MOVE MID-WF0214I1-FLFREE          TO WS-FLFREE                       
029900     MOVE MID-WF0214I1-FLPRIV          TO WS-FLPRIV                       
030000     MOVE MID-WF0214I1-KDVAT           TO WS-KDVAT                        
030100     MOVE MID-WF0214I1-KDVALISO        TO WS-KDVALISO                     
030200     MOVE MID-WF0214I1-KDINVFRQ        TO WS-KDINVFRQ                     
030300     MOVE MID-WF0214I1-KDFINDOC        TO WS-KDFINDOC                     
030400     MOVE MID-WF0214I1-IDBREAK(1)      TO WS-IDBREAK-1                    
030500     MOVE MID-WF0214I1-IDBREAK(2)      TO WS-IDBREAK-2                    
030600     MOVE MID-WF0214I1-IDSEQ(1)        TO WS-IDSEQ-1                      
030700     MOVE MID-WF0214I1-IDSEQ(2)        TO WS-IDSEQ-2                      
030800     MOVE MID-WF0214I1-IDSEQ(3)        TO WS-IDSEQ-3                      
030900     MOVE MID-WF0214I1-KDARTURS        TO WS-KDARTURS                     
031000     MOVE MID-WF0214I1-KDANMORS        TO WS-KDANMORS                     
031100     MOVE MID-WF0214I1-IDFAKREF        TO WS-IDFAKREF                     
031200     MOVE MID-WF0214I1-DAFAKREF        TO WS-DAFAKREF                     
031300     MOVE MID-WF0214I1-IDDC            TO WS-IDDC                         
031400     MOVE MID-WF0214I1-KDFRAKT         TO WS-KDFRAKT                      
031500     MOVE MID-WF0214I1-BELEVVIL        TO WS-BELEVVIL                     
031600     MOVE MID-WF0214I1-IDACCNT(1)      TO WS-IDACCNT-1                    
031700     MOVE MID-WF0214I1-IDACCNT(2)      TO WS-IDACCNT-2                    
031800     MOVE MID-WF0214I1-IDACCNT(3)      TO WS-IDACCNT-3                    
031900     MOVE MID-WF0214I1-IDACCNT(4)      TO WS-IDACCNT-4                    
032000     MOVE MID-WF0214I1-IDSYSTEM-SEND   TO WS-IDSYSTEM-SEND                
032100     MOVE MID-WF0214I1-IDSYSTEM-REC    TO WS-IDSYSTEM-REC                 
032200     MOVE MID-WF0214I1-BEANST          TO WS-BEANST                       
032300     MOVE MID-WF0214I1-IDUSER          TO WS-IDUSER                       
032400     MOVE MID-WF0214I1-BETEXT          TO WS-BETEXT                       
032500     MOVE SPACE                        TO WS-BETEXT-CRE                   
032600     MOVE SPACE                        TO WS-IDARTNR-CNTRL                
032610     MOVE SPACE                        TO WS-FLPCOO                       
032620     MOVE SPACE                        TO WS-IDLEVNR-ART                  
032630     MOVE SPACE                        TO WS-IDTRACK-1                    
032640     MOVE ZERO                         TO WS-KVANT-TRACK-1                
032650     MOVE SPACE                        TO WS-IDTRACK-2                    
032660     MOVE ZERO                         TO WS-KVANT-TRACK-2                
032670     MOVE SPACE                        TO WS-IDTRACK-3                    
032680     MOVE ZERO                         TO WS-KVANT-TRACK-3                
032690     MOVE SPACE                        TO WS-IDTRACK-4                    
032691     MOVE ZERO                         TO WS-KVANT-TRACK-4                
032692     MOVE SPACE                        TO WS-IDTRACK-5                    
032693     MOVE ZERO                         TO WS-KVANT-TRACK-5                
032694     MOVE SPACE                        TO WS-KDPRMOD                      
032700     ADD 1                             TO WS-IX2                          
032800     .                                                                    
032900     EJECT                                                                
033000                                                                          
033100 C-INSERT-UPDATE-T01TRAW SECTION.                                         
033200     MOVE WS-DAFAKREF TO WS-DAFAKREF-2                                    
033300     PERFORM DB2-INSERT-T01TRAW                                           
033400     .                                                                    
033500     EJECT                                                                
033600                                                                          
033700 D-INSERT-T01TBUN SECTION.                                                
033800     PERFORM DB2-INSERT-T01TBUN                                           
033900     .                                                                    
034000     EJECT                                                                
034100                                                                          
034200 E-START-PGM-WF020200 SECTION.                                            
034300     MOVE IN-REQU-IDMSGVER TO REQU-IDMSGVER                               
034400     MOVE IN-REQU-KDPGMACT TO REQU-KDPGMACT                               
034500     MOVE IN-REQU-IDUSER   TO REQU-IDUSER                                 
034600     PERFORM S04-SEND-OPEN                                                
034700     PERFORM S05-SEND-MESSAGE                                             
034800     PERFORM S06-SEND-CLOSE                                               
034900     .                                                                    
035000     EJECT                                                                
035100                                                                          
035200*    --- DISPATCHER-SECTIONS                                              
035300 S01-READ-OPEN SECTION.                                                   
035400     MOVE 'OPEN'                      TO RECV-KDFUNC                      
035500     MOVE 'CARPARTS.BILLIT.RECEIVE5'  TO RECV-ADDISPABS                   
035600     CALL WZ01RECV USING RECV-CONTROL-AREA                                
035700                         RECV-OPEN-AREA                                   
035800     IF RECV-KDRC > 0                                                     
035900       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
036000       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
036100       DELIMITED BY SIZE INTO ERRORTEXT                                   
036200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036300     END-IF                                                               
036400     .                                                                    
036500                                                                          
036600 S02-READ-MESSAGE SECTION.                                                
036700     MOVE 'GET'                           TO RECV-KDFUNC                  
036800     MOVE LENGTH OF RECV-AREA             TO RECV-KVDLEN                  
036900     CALL WZ01RECV USING RECV-CONTROL-AREA                                
037000                         RECV-KVDLEN                                      
037100                         RECV-AREA                                        
037200     IF RECV-KDRC > 1                                                     
037300       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
037400       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
037500       DELIMITED BY SIZE INTO ERRORTEXT                                   
037600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
037700     END-IF                                                               
037800     .                                                                    
037900                                                                          
038000 S03-READ-CLOSE SECTION.                                                  
038100     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
038200     CALL WZ01RECV USING RECV-CONTROL-AREA                                
038300                                                                          
038400     IF RECV-KDRC > 0                                                     
038500       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
038600       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
038700       DELIMITED BY SIZE INTO ERRORTEXT                                   
038800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
038900     END-IF                                                               
039000     .                                                                    
039100     EJECT                                                                
039200                                                                          
039300 S04-SEND-OPEN SECTION.                                                   
039400     MOVE 'OPEN'                        TO SEND-KDFUNC                    
039500     MOVE 'CARPARTS.BILLIT.VALIDATE'    TO SEND-ADDISPABS                 
039600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
039700                         SEND-OPEN-AREA                                   
039800     IF SEND-KDRC > 0                                                     
039900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
040000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
040100       DELIMITED BY SIZE INTO ERRORTEXT                                   
040200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
040300     END-IF                                                               
040400     .                                                                    
040500                                                                          
040600 S05-SEND-MESSAGE SECTION.                                                
040700     MOVE 'PUT'                           TO SEND-KDFUNC                  
040800     MOVE LENGTH OF SEND-AREA             TO SEND-KVDLEN                  
040900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
041000                         SEND-KVDLEN                                      
041100                         SEND-AREA                                        
041200     IF SEND-KDRC > 0                                                     
041300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
041400       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
041500       DELIMITED BY SIZE INTO ERRORTEXT                                   
041600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
041700     END-IF                                                               
041800     .                                                                    
041900                                                                          
042000 S06-SEND-CLOSE SECTION.                                                  
042100     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
042200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
042300                                                                          
042400     IF SEND-KDRC > 0                                                     
042500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
042600       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
042700       DELIMITED BY SIZE INTO ERRORTEXT                                   
042800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
042900     END-IF                                                               
043000     .                                                                    
043100     EJECT                                                                
043200                                                                          
043300 DB2-INSERT-T01TRAW SECTION.                                              
043400     SKIP2                                                                
043500     MOVE 000    TO GOOD-SQLCODECODES                                     
043600     EXEC SQL                                                             
043700         INSERT INTO T01TRAW                                              
043800         (IDLEGSEL                                                        
043900         ,IDBUNDLE                                                        
044000         ,DAREGDAT                                                        
044100         ,TIREGTID                                                        
044200         ,IDREF                                                           
044300         ,DAREFDAT                                                        
044400         ,IDREFRAD                                                        
044500         ,BEVOLREF                                                        
044600         ,IDLANDX3_SEND                                                   
044700         ,IDLANDX3_REC                                                    
044800         ,IDLEVNR                                                         
044900         ,IDPARTNR                                                        
045000         ,IDEXCUST_1                                                      
045100         ,IDEXCUST_2                                                      
045200         ,IDEXCUST_3                                                      
045300         ,IDOPTION_1                                                      
045400         ,IDOPTION_2                                                      
045500         ,IDOPTION_3                                                      
045600         ,IDOPTION_4                                                      
045700         ,IDOPTION_5                                                      
045800         ,IDAPPEND                                                        
045900         ,IDARTNR_FINANCE                                                 
046000         ,IDSTATNR                                                        
046100         ,VKORDBTO_KOLLI                                                  
046200         ,VKARTNTO                                                        
046300         ,PRARTBTO                                                        
046400         ,PRARTNTO                                                        
046500         ,REARTRAB                                                        
046600         ,KVBEART                                                         
046700         ,KVLEVART                                                        
046800         ,BEART                                                           
046900         ,FLSOFT                                                          
047000         ,FLSPECPR                                                        
047100         ,FLFREE                                                          
047200         ,FLPRIV                                                          
047300         ,KDVAT                                                           
047400         ,KDVALISO                                                        
047500         ,KDINVFRQ                                                        
047600         ,KDFINDOC                                                        
047700         ,IDBREAK_1                                                       
047800         ,IDBREAK_2                                                       
047900         ,IDSEQ_1                                                         
048000         ,IDSEQ_2                                                         
048100         ,IDSEQ_3                                                         
048200         ,KDARTURS                                                        
048300         ,KDANMORS                                                        
048400         ,IDFAKREF                                                        
048500         ,DAFAKREF                                                        
048600         ,IDDC                                                            
048700         ,KDFRAKT                                                         
048800         ,BELEVVIL                                                        
048900         ,IDACCNT_1                                                       
049000         ,IDACCNT_2                                                       
049100         ,IDACCNT_3                                                       
049200         ,IDACCNT_4                                                       
049300         ,IDSYSTEM_SEND                                                   
049400         ,IDSYSTEM_REC                                                    
049500         ,FILLER                                                          
049600         ,IDFELKOD                                                        
049700         ,BEFEL                                                           
049800         ,BEANST                                                          
049900         ,IDUSER                                                          
050000         ,BETEXT                                                          
050100         ,BETEXT_CRE                                                      
050200         ,IDARTNR_CNTRL                                                   
050210         ,FLPCOO                                                          
050220         ,IDLEVNR_ART                                                     
050230         ,IDTRACK_1                                                       
050240         ,KVANT_TRACK_1                                                   
050250         ,IDTRACK_2                                                       
050260         ,KVANT_TRACK_2                                                   
050270         ,IDTRACK_3                                                       
050280         ,KVANT_TRACK_3                                                   
050290         ,IDTRACK_4                                                       
050291         ,KVANT_TRACK_4                                                   
050292         ,IDTRACK_5                                                       
050293         ,KVANT_TRACK_5                                                   
050294         ,KDPRMOD                                                         
050295         )                                                                
050300         VALUES(:WS-IDLEGSEL                                              
050400               ,:WS-IDBUNDLE                                              
050500               ,:WS-DATUM                                                 
050600               ,:WS-KLOCKAN-COMP                                          
050700               ,:WS-IDREF                                                 
050800               ,:WS-DAREFDAT                                              
050900               ,:WS-IDREFRAD                                              
051000               ,:WS-BEVOLREF                                              
051100               ,:WS-IDLANDX3-SEND                                         
051200               ,:WS-IDLANDX3-REC                                          
051300               ,:WS-IDLEVNR                                               
051400               ,:WS-IDPARTNR                                              
051500               ,:WS-IDEXCUST-1                                            
051600               ,:WS-IDEXCUST-2                                            
051700               ,:WS-IDEXCUST-3                                            
051800               ,:WS-IDOPTION-1                                            
051900               ,:WS-IDOPTION-2                                            
052000               ,:WS-IDOPTION-3                                            
052100               ,:WS-IDOPTION-4                                            
052200               ,:WS-IDOPTION-5                                            
052300               ,:WS-IDAPPEND                                              
052400               ,:WS-IDARTNR-FINANCE                                       
052500               ,:WS-IDSTATNR                                              
052600               ,:WS-VKORDBTO-KOLLI                                        
052700               ,:WS-VKARTNTO                                              
052800               ,:WS-PRARTBTO                                              
052900               ,:WS-PRARTNTO                                              
053000               ,:WS-REARTRAB                                              
053100               ,:WS-KVBEART                                               
053200               ,:WS-KVLEVART                                              
053300               ,:WS-BEART                                                 
053400               ,:WS-FLSOFT                                                
053500               ,:WS-FLSPECPR                                              
053600               ,:WS-FLFREE                                                
053700               ,:WS-FLPRIV                                                
053800               ,:WS-KDVAT                                                 
053900               ,:WS-KDVALISO                                              
054000               ,:WS-KDINVFRQ                                              
054100               ,:WS-KDFINDOC                                              
054200               ,:WS-IDBREAK-1                                             
054300               ,:WS-IDBREAK-2                                             
054400               ,:WS-IDSEQ-1                                               
054500               ,:WS-IDSEQ-2                                               
054600               ,:WS-IDSEQ-3                                               
054700               ,:WS-KDARTURS                                              
054800               ,:WS-KDANMORS                                              
054900               ,:WS-IDFAKREF                                              
055000               ,:WS-DAFAKREF-2                                            
055100               ,:WS-IDDC                                                  
055200               ,:WS-KDFRAKT                                               
055300               ,:WS-BELEVVIL                                              
055400               ,:WS-IDACCNT-1                                             
055500               ,:WS-IDACCNT-2                                             
055600               ,:WS-IDACCNT-3                                             
055700               ,:WS-IDACCNT-4                                             
055800               ,:WS-IDSYSTEM-SEND                                         
055900               ,:WS-IDSYSTEM-REC                                          
056000               ,:WS-FILLER                                                
056100               ,:WS-IDFELKOD                                              
056200               ,:WS-BEFEL                                                 
056300               ,:WS-BEANST                                                
056400               ,:WS-IDUSER                                                
056500               ,:WS-BETEXT                                                
056600               ,:WS-BETEXT-CRE                                            
056700               ,:WS-IDARTNR-CNTRL                                         
056710               ,:WS-FLPCOO                                                
056720               ,:WS-IDLEVNR-ART                                           
056730               ,:WS-IDTRACK-1                                             
056740               ,:WS-KVANT-TRACK-1                                         
056750               ,:WS-IDTRACK-2                                             
056760               ,:WS-KVANT-TRACK-2                                         
056770               ,:WS-IDTRACK-3                                             
056780               ,:WS-KVANT-TRACK-3                                         
056790               ,:WS-IDTRACK-4                                             
056791               ,:WS-KVANT-TRACK-4                                         
056792               ,:WS-IDTRACK-5                                             
056793               ,:WS-KVANT-TRACK-5                                         
056794               ,:WS-KDPRMOD                                               
056795               )                                                          
056800     END-EXEC                                                             
056900                                                                          
057000     MOVE SQLCODE TO SQLCODE-WS                                           
057100     PERFORM DB2-STATUS-CONTROL                                           
057200     .                                                                    
057300     EJECT                                                                
057400                                                                          
057500 DB2-INSERT-T01TBUN SECTION.                                              
057600     MOVE 000    TO GOOD-SQLCODECODES                                     
057700     EXEC SQL                                                             
057800         INSERT INTO T01TBUN                                              
057900         (IDLEGSEL                                                        
058000         ,IDBUNDLE                                                        
058100         ,DAREGDAT                                                        
058200         ,TIREGTID                                                        
058300         ,FLFEL                                                           
058400         ,FLKNTRL                                                         
058500         ,FLKLAR)                                                         
058600         VALUES(:WS-IDLEGSEL                                              
058700               ,:WS-IDBUNDLE                                              
058800               ,:WS-DATUM                                                 
058900               ,:WS-KLOCKAN-COMP                                          
059000               ,:NOO                                                      
059100               ,:NOO                                                      
059200               ,:NOO)                                                     
059300     END-EXEC                                                             
059400                                                                          
059500     MOVE SQLCODE TO SQLCODE-WS                                           
059600     PERFORM DB2-STATUS-CONTROL                                           
059700     .                                                                    
059800     EJECT                                                                
059900                                                                          
060000 DB2-SELECT-T01SYST SECTION.                                              
060100     MOVE 000100    TO GOOD-SQLCODECODES                                  
060200     EXEC SQL                                                             
060300       SELECT   KDBEHS                                                    
060400                                                                          
060500       INTO    :SYST-KDBEHS                                               
060600                                                                          
060700       FROM     T01SYST                                                   
060800                                                                          
060900     END-EXEC                                                             
061000                                                                          
061100     MOVE SQLCODE      TO SQLCODE-WS                                      
061200     PERFORM DB2-STATUS-CONTROL                                           
061300     .                                                                    
061400     EJECT                                                                
061500                                                                          
061600 DB2-UPDATE-T01SYST SECTION.                                              
061700     MOVE 000     TO GOOD-SQLCODECODES                                    
061800     EXEC SQL UPDATE T01SYST                                              
061900                                                                          
062000       SET KDBEHS   = :SYST-KDBEHS                                        
062100                                                                          
062200     END-EXEC                                                             
062300                                                                          
062400     MOVE SQLCODE TO SQLCODE-WS                                           
062500     PERFORM DB2-STATUS-CONTROL                                           
062600     .                                                                    
062700     EJECT                                                                
062800                                                                          
062900 DB2-STATUS-CONTROL SECTION.                                              
063000     SET SQLCODE-IX TO 1                                                  
063100     SEARCH GOOD-SQLCODE                                                  
063200       AT END                                                             
063300          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
063400          DELIMITED BY SIZE INTO ERRORTEXT                                
063500          CALL ABEND USING RKOD-ABEND-DB2                                 
063600       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
063700     END-SEARCH                                                           
063800     .                                                                    
