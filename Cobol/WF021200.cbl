000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF021200.                                                
000400 AUTHOR.         HENRIKSSON ANDERS.                                       
000500 DATE-WRITTEN.   02/02/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME                                                                 
000900*        CARPARTS.BILLIT.RECEIVE3                                         
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
002300*        TRANSAKTION: WF0212X                                             
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
003800 77  IDPGM                       PIC X(08)   VALUE 'WF021200'.            
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
007700*    03  -COPY WF0212I1 -PRE MID-WF0212I1-                                
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
017300     03 WS-BEANST              PIC X(25) VALUE SPACE.                     
017400     03 WS-IDUSER              PIC X(8)  VALUE SPACE.                     
017500     03 WS-BETEXT              PIC X(125) VALUE SPACE.                    
017600     03 WS-BETEXT-CRE          PIC X(100) VALUE SPACE.                    
017700     03 WS-FILLER              PIC X(100) VALUE SPACE.                    
017800     03 WS-FLFEL               PIC X(1)  VALUE SPACE.                     
017900     03 WS-IDFELKOD            PIC X(3)  VALUE SPACE.                     
018000     03 WS-BEFEL               PIC X(50) VALUE SPACE.                     
018100     03 WS-IDARTNR-CNTRL       PIC X(2)  VALUE SPACE.                     
018200     03 WS-FLPCOO              PIC X(1)  VALUE SPACE.                     
018210     03 WS-IDLEVNR-ART         PIC X(5)  VALUE SPACE.                     
018220     03 WS-IDTRACK-1           PIC X(25) VALUE SPACE.                     
018230     03 WS-KVANT-TRACK-1       PIC S9(7) COMP-3 VALUE ZERO.               
018240     03 WS-IDTRACK-2           PIC X(25) VALUE SPACE.                     
018250     03 WS-KVANT-TRACK-2       PIC S9(7) COMP-3 VALUE ZERO.               
018260     03 WS-IDTRACK-3           PIC X(25) VALUE SPACE.                     
018270     03 WS-KVANT-TRACK-3       PIC S9(7) COMP-3 VALUE ZERO.               
018280     03 WS-IDTRACK-4           PIC X(25) VALUE SPACE.                     
018290     03 WS-KVANT-TRACK-4       PIC S9(7) COMP-3 VALUE ZERO.               
018291     03 WS-IDTRACK-5           PIC X(25) VALUE SPACE.                     
018292     03 WS-KVANT-TRACK-5       PIC S9(7) COMP-3 VALUE ZERO.               
018293     03 WS-KDPRMOD             PIC X(2)  VALUE SPACE.                     
018300                                                                          
018400     03 WS-ERRORTEXT           PIC X(50) VALUE SPACE.                     
018500                                                                          
018600     03 SYST-KDBEHS            PIC X(1)  VALUE SPACE.                     
018700                                                                          
018800 01  FILLER                    PIC X(16)    VALUE 'T01TRAW-AREA'.         
018900*01  -COPY T01TRAW -PRE T01TRAW-                                          
019000     EJECT                                                                
019100                                                                          
019200 01  FILLER                    PIC X(16)    VALUE 'T01TBUN-AREA'.         
019300*01  -COPY T01TBUN -PRE T01TBUN-                                          
019400     EJECT                                                                
019500                                                                          
019600 01  FILLER                    PIC X(16)    VALUE 'T01SYST-AREA'.         
019700*01  -COPY T01SYST -PRE T01SYST-                                          
019800     EJECT                                                                
019900                                                                          
020000     EXEC SQL INCLUDE T01TRAW END-EXEC.                                   
020100     EJECT                                                                
020200                                                                          
020300     EXEC SQL INCLUDE T01TBUN END-EXEC.                                   
020400     EJECT                                                                
020500                                                                          
020600     EXEC SQL INCLUDE T01SYST END-EXEC.                                   
020700     EJECT                                                                
020800                                                                          
020900 LINKAGE SECTION.                                                         
021000                                                                          
021100 PROCEDURE DIVISION.                                                      
021200 MAIN SECTION.                                                            
021300     PERFORM S01-READ-OPEN                                                
021400                                                                          
021500     PERFORM S02-READ-MESSAGE                                             
021600     MOVE MID-WF0212I1-IDBUNDLE TO WS-IDBUNDLE                            
021700     IF RECV-KDRC = ZERO                                                  
021800       PERFORM A-INIT                                                     
021900       PERFORM UNTIL RECV-KDRC > ZERO OR WS-IX2 > MAX-LINES               
022000         IF MID-WF0212I1-IDBUNDLE NOT = WS-IDBUNDLE                       
022100* MORE THAN ONE BUNDLEID IN THE SAME SESSION NOT ALLOWED                  
022200           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
022300         END-IF                                                           
022400         PERFORM B-LINES                                                  
022500         PERFORM C-INSERT-UPDATE-T01TRAW                                  
022600         PERFORM S02-READ-MESSAGE                                         
022700       END-PERFORM                                                        
022800* TO MANY LINES ADD MORE LINES TO MAX-LINES OR A RESTART                  
022900       IF WS-IX2 > MAX-LINES                                              
023000         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
023100       END-IF                                                             
023200       PERFORM D-INSERT-T01TBUN                                           
023300                                                                          
023400* CHECKS IF PROGRAM WF020200 IS STILL RUNNING                             
023500       PERFORM DB2-SELECT-T01SYST                                         
023600       IF SYST-KDBEHS = SPACE                                             
023700         PERFORM E-START-PGM-WF020200                                     
023800       ELSE                                                               
023900* THE PROCESS IN WF020200 WAS RUNNING AND                                 
024000* BLOCK WF021200 FROM STARTING WF020200                                   
024100         MOVE 'F' TO SYST-KDBEHS                                          
024200         PERFORM DB2-UPDATE-T01SYST                                       
024300       END-IF                                                             
024400     END-IF                                                               
024500                                                                          
024600     PERFORM S03-READ-CLOSE                                               
024700     MOVE ZERO TO RETURN-CODE                                             
024800     GOBACK                                                               
024900     .                                                                    
025000     EJECT                                                                
025100                                                                          
025200 A-INIT SECTION.                                                          
025300     CALL W009WAIT USING WS-TIME-WAIT                                     
025400     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-DATUM                        
025500     MOVE FUNCTION CURRENT-DATE (9:8)  TO WS-KLOCKAN                      
025600     MOVE WS-KLOCKAN                   TO WS-KLOCKAN-COMP                 
025700                                                                          
025800     MOVE 'N'                          TO WS-FLFEL                        
025900     MOVE SPACE                        TO WS-IDFELKOD                     
026000                                          WS-BEFEL                        
026100     MOVE ZERO                         TO WS-IX2                          
026200                                                                          
026300     INITIALIZE GOOD-SQLCODECODES                                         
026400     .                                                                    
026500     EJECT                                                                
026600                                                                          
026700 B-LINES SECTION.                                                         
026800     MOVE MID-WF0212I1-IDLEGSEL        TO WS-IDLEGSEL                     
026900     MOVE MID-WF0212I1-IDBUNDLE        TO WS-IDBUNDLE                     
027000     MOVE MID-WF0212I1-IDREF           TO WS-IDREF                        
027100     MOVE MID-WF0212I1-DAREFDAT        TO WS-DAREFDAT                     
027200     MOVE MID-WF0212I1-IDREFRAD        TO WS-IDREFRAD                     
027300     MOVE MID-WF0212I1-BEVOLREF        TO WS-BEVOLREF                     
027400     MOVE MID-WF0212I1-IDLANDX3-SEND   TO WS-IDLANDX3-SEND                
027500     MOVE MID-WF0212I1-IDLANDX3-REC    TO WS-IDLANDX3-REC                 
027600     MOVE MID-WF0212I1-IDLEVNR         TO WS-IDLEVNR                      
027700     MOVE MID-WF0212I1-IDPARTNR        TO WS-IDPARTNR                     
027800     MOVE MID-WF0212I1-IDEXCUST(1)     TO WS-IDEXCUST-1                   
027900     MOVE MID-WF0212I1-IDEXCUST(2)     TO WS-IDEXCUST-2                   
028000     MOVE MID-WF0212I1-IDEXCUST(3)     TO WS-IDEXCUST-3                   
028100     MOVE MID-WF0212I1-IDOPTION(1)     TO WS-IDOPTION-1                   
028200     MOVE MID-WF0212I1-IDOPTION(2)     TO WS-IDOPTION-2                   
028300     MOVE MID-WF0212I1-IDOPTION(3)     TO WS-IDOPTION-3                   
028400     MOVE MID-WF0212I1-IDOPTION(4)     TO WS-IDOPTION-4                   
028500     MOVE MID-WF0212I1-IDOPTION(5)     TO WS-IDOPTION-5                   
028600     MOVE MID-WF0212I1-IDAPPEND        TO WS-IDAPPEND                     
028700     MOVE MID-WF0212I1-IDARTNR-FINANCE TO WS-IDARTNR-FINANCE              
028800     MOVE MID-WF0212I1-IDSTATNR        TO WS-IDSTATNR                     
028900     MOVE MID-WF0212I1-VKORDBTO-KOLLI  TO WS-VKORDBTO-KOLLI               
029000     MOVE MID-WF0212I1-VKARTNTO        TO WS-VKARTNTO                     
029100     MOVE MID-WF0212I1-PRARTBTO        TO WS-PRARTBTO                     
029200     MOVE MID-WF0212I1-PRARTNTO        TO WS-PRARTNTO                     
029300     MOVE MID-WF0212I1-REARTRAB        TO WS-REARTRAB                     
029400     MOVE MID-WF0212I1-KVBEART         TO WS-KVBEART                      
029500     MOVE MID-WF0212I1-KVLEVART        TO WS-KVLEVART                     
029600     MOVE MID-WF0212I1-BEART           TO WS-BEART                        
029700     MOVE MID-WF0212I1-FLSOFT          TO WS-FLSOFT                       
029800     MOVE MID-WF0212I1-FLSPECPR        TO WS-FLSPECPR                     
029900     MOVE MID-WF0212I1-FLFREE          TO WS-FLFREE                       
030000     MOVE MID-WF0212I1-FLPRIV          TO WS-FLPRIV                       
030100     MOVE MID-WF0212I1-KDVAT           TO WS-KDVAT                        
030200     MOVE MID-WF0212I1-KDVALISO        TO WS-KDVALISO                     
030300     MOVE MID-WF0212I1-KDINVFRQ        TO WS-KDINVFRQ                     
030400     MOVE MID-WF0212I1-KDFINDOC        TO WS-KDFINDOC                     
030500     MOVE MID-WF0212I1-IDBREAK(1)      TO WS-IDBREAK-1                    
030600     MOVE MID-WF0212I1-IDBREAK(2)      TO WS-IDBREAK-2                    
030700     MOVE MID-WF0212I1-IDSEQ(1)        TO WS-IDSEQ-1                      
030800     MOVE MID-WF0212I1-IDSEQ(2)        TO WS-IDSEQ-2                      
030900     MOVE MID-WF0212I1-IDSEQ(3)        TO WS-IDSEQ-3                      
031000     MOVE MID-WF0212I1-KDARTURS        TO WS-KDARTURS                     
031100     MOVE MID-WF0212I1-KDANMORS        TO WS-KDANMORS                     
031200     MOVE MID-WF0212I1-IDFAKREF        TO WS-IDFAKREF                     
031300     MOVE MID-WF0212I1-DAFAKREF        TO WS-DAFAKREF                     
031400     MOVE MID-WF0212I1-IDDC            TO WS-IDDC                         
031500     MOVE MID-WF0212I1-KDFRAKT         TO WS-KDFRAKT                      
031600     MOVE MID-WF0212I1-BELEVVIL        TO WS-BELEVVIL                     
031700     MOVE MID-WF0212I1-IDACCNT(1)      TO WS-IDACCNT-1                    
031800     MOVE MID-WF0212I1-IDACCNT(2)      TO WS-IDACCNT-2                    
031900     MOVE MID-WF0212I1-IDACCNT(3)      TO WS-IDACCNT-3                    
032000     MOVE MID-WF0212I1-IDACCNT(4)      TO WS-IDACCNT-4                    
032100     MOVE MID-WF0212I1-IDSYSTEM-SEND   TO WS-IDSYSTEM-SEND                
032200     MOVE MID-WF0212I1-IDSYSTEM-REC    TO WS-IDSYSTEM-REC                 
032300     MOVE MID-WF0212I1-BEANST          TO WS-BEANST                       
032400     MOVE MID-WF0212I1-IDUSER          TO WS-IDUSER                       
032500     MOVE MID-WF0212I1-BETEXT          TO WS-BETEXT                       
032600     MOVE MID-WF0212I1-BETEXT-CRE      TO WS-BETEXT-CRE                   
032700     MOVE SPACE                        TO WS-IDARTNR-CNTRL                
032800     MOVE SPACE                        TO WS-FLPCOO                       
032810     MOVE SPACE                        TO WS-IDLEVNR-ART                  
032820     MOVE SPACE                        TO WS-IDTRACK-1                    
032830     MOVE ZERO                         TO WS-KVANT-TRACK-1                
032840     MOVE SPACE                        TO WS-IDTRACK-2                    
032850     MOVE ZERO                         TO WS-KVANT-TRACK-2                
032860     MOVE SPACE                        TO WS-IDTRACK-3                    
032870     MOVE ZERO                         TO WS-KVANT-TRACK-3                
032880     MOVE SPACE                        TO WS-IDTRACK-4                    
032890     MOVE ZERO                         TO WS-KVANT-TRACK-4                
032891     MOVE SPACE                        TO WS-IDTRACK-5                    
032892     MOVE ZERO                         TO WS-KVANT-TRACK-5                
032893     MOVE SPACE                        TO WS-KDPRMOD                      
032900     ADD 1                             TO WS-IX2                          
033000     .                                                                    
033100     EJECT                                                                
033200                                                                          
033300 C-INSERT-UPDATE-T01TRAW SECTION.                                         
033400     MOVE WS-DAFAKREF TO WS-DAFAKREF-2                                    
033500     PERFORM DB2-INSERT-T01TRAW                                           
033600     .                                                                    
033700     EJECT                                                                
033800                                                                          
033900 D-INSERT-T01TBUN SECTION.                                                
034000     PERFORM DB2-INSERT-T01TBUN                                           
034100     .                                                                    
034200     EJECT                                                                
034300                                                                          
034400 E-START-PGM-WF020200 SECTION.                                            
034500     MOVE IN-REQU-IDMSGVER TO REQU-IDMSGVER                               
034600     MOVE IN-REQU-KDPGMACT TO REQU-KDPGMACT                               
034700     MOVE IN-REQU-IDUSER   TO REQU-IDUSER                                 
034800     PERFORM S04-SEND-OPEN                                                
034900     PERFORM S05-SEND-MESSAGE                                             
035000     PERFORM S06-SEND-CLOSE                                               
035100     .                                                                    
035200     EJECT                                                                
035300                                                                          
035400*    --- DISPATCHER-SECTIONS                                              
035500 S01-READ-OPEN SECTION.                                                   
035600     MOVE 'OPEN'                      TO RECV-KDFUNC                      
035700     MOVE 'CARPARTS.BILLIT.RECEIVE3'  TO RECV-ADDISPABS                   
035800     CALL WZ01RECV USING RECV-CONTROL-AREA                                
035900                         RECV-OPEN-AREA                                   
036000     IF RECV-KDRC > 0                                                     
036100       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
036200       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
036300       DELIMITED BY SIZE INTO ERRORTEXT                                   
036400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036500     END-IF                                                               
036600     .                                                                    
036700                                                                          
036800 S02-READ-MESSAGE SECTION.                                                
036900     MOVE 'GET'                           TO RECV-KDFUNC                  
037000     MOVE LENGTH OF RECV-AREA             TO RECV-KVDLEN                  
037100     CALL WZ01RECV USING RECV-CONTROL-AREA                                
037200                         RECV-KVDLEN                                      
037300                         RECV-AREA                                        
037400     IF RECV-KDRC > 1                                                     
037500       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
037600       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
037700       DELIMITED BY SIZE INTO ERRORTEXT                                   
037800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
037900     END-IF                                                               
038000     .                                                                    
038100                                                                          
038200 S03-READ-CLOSE SECTION.                                                  
038300     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
038400     CALL WZ01RECV USING RECV-CONTROL-AREA                                
038500                                                                          
038600     IF RECV-KDRC > 0                                                     
038700       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
038800       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
038900       DELIMITED BY SIZE INTO ERRORTEXT                                   
039000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039100     END-IF                                                               
039200     .                                                                    
039300     EJECT                                                                
039400                                                                          
039500 S04-SEND-OPEN SECTION.                                                   
039600     MOVE 'OPEN'                        TO SEND-KDFUNC                    
039700     MOVE 'CARPARTS.BILLIT.VALIDATE'    TO SEND-ADDISPABS                 
039800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
039900                         SEND-OPEN-AREA                                   
040000     IF SEND-KDRC > 0                                                     
040100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
040200       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
040300       DELIMITED BY SIZE INTO ERRORTEXT                                   
040400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
040500     END-IF                                                               
040600     .                                                                    
040700                                                                          
040800 S05-SEND-MESSAGE SECTION.                                                
040900     MOVE 'PUT'                           TO SEND-KDFUNC                  
041000     MOVE LENGTH OF SEND-AREA             TO SEND-KVDLEN                  
041100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
041200                         SEND-KVDLEN                                      
041300                         SEND-AREA                                        
041400     IF SEND-KDRC > 0                                                     
041500       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
041600       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
041700       DELIMITED BY SIZE INTO ERRORTEXT                                   
041800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
041900     END-IF                                                               
042000     .                                                                    
042100                                                                          
042200 S06-SEND-CLOSE SECTION.                                                  
042300     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
042400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
042500                                                                          
042600     IF SEND-KDRC > 0                                                     
042700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
042800       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
042900       DELIMITED BY SIZE INTO ERRORTEXT                                   
043000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
043100     END-IF                                                               
043200     .                                                                    
043300     EJECT                                                                
043400                                                                          
043500 DB2-INSERT-T01TRAW SECTION.                                              
043600     SKIP2                                                                
043700     MOVE 000    TO GOOD-SQLCODECODES                                     
043800     EXEC SQL                                                             
043900         INSERT INTO T01TRAW                                              
044000         (IDLEGSEL                                                        
044100         ,IDBUNDLE                                                        
044200         ,DAREGDAT                                                        
044300         ,TIREGTID                                                        
044400         ,IDREF                                                           
044500         ,DAREFDAT                                                        
044600         ,IDREFRAD                                                        
044700         ,BEVOLREF                                                        
044800         ,IDLANDX3_SEND                                                   
044900         ,IDLANDX3_REC                                                    
045000         ,IDLEVNR                                                         
045100         ,IDPARTNR                                                        
045200         ,IDEXCUST_1                                                      
045300         ,IDEXCUST_2                                                      
045400         ,IDEXCUST_3                                                      
045500         ,IDOPTION_1                                                      
045600         ,IDOPTION_2                                                      
045700         ,IDOPTION_3                                                      
045800         ,IDOPTION_4                                                      
045900         ,IDOPTION_5                                                      
046000         ,IDAPPEND                                                        
046100         ,IDARTNR_FINANCE                                                 
046200         ,IDSTATNR                                                        
046300         ,VKORDBTO_KOLLI                                                  
046400         ,VKARTNTO                                                        
046500         ,PRARTBTO                                                        
046600         ,PRARTNTO                                                        
046700         ,REARTRAB                                                        
046800         ,KVBEART                                                         
046900         ,KVLEVART                                                        
047000         ,BEART                                                           
047100         ,FLSOFT                                                          
047200         ,FLSPECPR                                                        
047300         ,FLFREE                                                          
047400         ,FLPRIV                                                          
047500         ,KDVAT                                                           
047600         ,KDVALISO                                                        
047700         ,KDINVFRQ                                                        
047800         ,KDFINDOC                                                        
047900         ,IDBREAK_1                                                       
048000         ,IDBREAK_2                                                       
048100         ,IDSEQ_1                                                         
048200         ,IDSEQ_2                                                         
048300         ,IDSEQ_3                                                         
048400         ,KDARTURS                                                        
048500         ,KDANMORS                                                        
048600         ,IDFAKREF                                                        
048700         ,DAFAKREF                                                        
048800         ,IDDC                                                            
048900         ,KDFRAKT                                                         
049000         ,BELEVVIL                                                        
049100         ,IDACCNT_1                                                       
049200         ,IDACCNT_2                                                       
049300         ,IDACCNT_3                                                       
049400         ,IDACCNT_4                                                       
049500         ,IDSYSTEM_SEND                                                   
049600         ,IDSYSTEM_REC                                                    
049700         ,FILLER                                                          
049800         ,IDFELKOD                                                        
049900         ,BEFEL                                                           
050000         ,BEANST                                                          
050100         ,IDUSER                                                          
050200         ,BETEXT                                                          
050300         ,BETEXT_CRE                                                      
050400         ,IDARTNR_CNTRL                                                   
050500         ,FLPCOO                                                          
050510         ,IDLEVNR_ART                                                     
050520         ,IDTRACK_1                                                       
050530         ,KVANT_TRACK_1                                                   
050540         ,IDTRACK_2                                                       
050550         ,KVANT_TRACK_2                                                   
050560         ,IDTRACK_3                                                       
050570         ,KVANT_TRACK_3                                                   
050580         ,IDTRACK_4                                                       
050590         ,KVANT_TRACK_4                                                   
050591         ,IDTRACK_5                                                       
050592         ,KVANT_TRACK_5                                                   
050593         ,KDPRMOD                                                         
050594         )                                                                
050600         VALUES(:WS-IDLEGSEL                                              
050700               ,:WS-IDBUNDLE                                              
050800               ,:WS-DATUM                                                 
050900               ,:WS-KLOCKAN-COMP                                          
051000               ,:WS-IDREF                                                 
051100               ,:WS-DAREFDAT                                              
051200               ,:WS-IDREFRAD                                              
051300               ,:WS-BEVOLREF                                              
051400               ,:WS-IDLANDX3-SEND                                         
051500               ,:WS-IDLANDX3-REC                                          
051600               ,:WS-IDLEVNR                                               
051700               ,:WS-IDPARTNR                                              
051800               ,:WS-IDEXCUST-1                                            
051900               ,:WS-IDEXCUST-2                                            
052000               ,:WS-IDEXCUST-3                                            
052100               ,:WS-IDOPTION-1                                            
052200               ,:WS-IDOPTION-2                                            
052300               ,:WS-IDOPTION-3                                            
052400               ,:WS-IDOPTION-4                                            
052500               ,:WS-IDOPTION-5                                            
052600               ,:WS-IDAPPEND                                              
052700               ,:WS-IDARTNR-FINANCE                                       
052800               ,:WS-IDSTATNR                                              
052900               ,:WS-VKORDBTO-KOLLI                                        
053000               ,:WS-VKARTNTO                                              
053100               ,:WS-PRARTBTO                                              
053200               ,:WS-PRARTNTO                                              
053300               ,:WS-REARTRAB                                              
053400               ,:WS-KVBEART                                               
053500               ,:WS-KVLEVART                                              
053600               ,:WS-BEART                                                 
053700               ,:WS-FLSOFT                                                
053800               ,:WS-FLSPECPR                                              
053900               ,:WS-FLFREE                                                
054000               ,:WS-FLPRIV                                                
054100               ,:WS-KDVAT                                                 
054200               ,:WS-KDVALISO                                              
054300               ,:WS-KDINVFRQ                                              
054400               ,:WS-KDFINDOC                                              
054500               ,:WS-IDBREAK-1                                             
054600               ,:WS-IDBREAK-2                                             
054700               ,:WS-IDSEQ-1                                               
054800               ,:WS-IDSEQ-2                                               
054900               ,:WS-IDSEQ-3                                               
055000               ,:WS-KDARTURS                                              
055100               ,:WS-KDANMORS                                              
055200               ,:WS-IDFAKREF                                              
055300               ,:WS-DAFAKREF-2                                            
055400               ,:WS-IDDC                                                  
055500               ,:WS-KDFRAKT                                               
055600               ,:WS-BELEVVIL                                              
055700               ,:WS-IDACCNT-1                                             
055800               ,:WS-IDACCNT-2                                             
055900               ,:WS-IDACCNT-3                                             
056000               ,:WS-IDACCNT-4                                             
056100               ,:WS-IDSYSTEM-SEND                                         
056200               ,:WS-IDSYSTEM-REC                                          
056300               ,:WS-FILLER                                                
056400               ,:WS-IDFELKOD                                              
056500               ,:WS-BEFEL                                                 
056600               ,:WS-BEANST                                                
056700               ,:WS-IDUSER                                                
056800               ,:WS-BETEXT                                                
056900               ,:WS-BETEXT-CRE                                            
057000               ,:WS-IDARTNR-CNTRL                                         
057100               ,:WS-FLPCOO                                                
057110               ,:WS-IDLEVNR-ART                                           
057120               ,:WS-IDTRACK-1                                             
057130               ,:WS-KVANT-TRACK-1                                         
057140               ,:WS-IDTRACK-2                                             
057150               ,:WS-KVANT-TRACK-2                                         
057160               ,:WS-IDTRACK-3                                             
057170               ,:WS-KVANT-TRACK-3                                         
057180               ,:WS-IDTRACK-4                                             
057190               ,:WS-KVANT-TRACK-4                                         
057191               ,:WS-IDTRACK-5                                             
057192               ,:WS-KVANT-TRACK-5                                         
057193               ,:WS-KDPRMOD                                               
057194               )                                                          
057200     END-EXEC                                                             
057300                                                                          
057400     MOVE SQLCODE TO SQLCODE-WS                                           
057500     PERFORM DB2-STATUS-CONTROL                                           
057600     .                                                                    
057700     EJECT                                                                
057800                                                                          
057900 DB2-INSERT-T01TBUN SECTION.                                              
058000     MOVE 000    TO GOOD-SQLCODECODES                                     
058100     EXEC SQL                                                             
058200         INSERT INTO T01TBUN                                              
058300         (IDLEGSEL                                                        
058400         ,IDBUNDLE                                                        
058500         ,DAREGDAT                                                        
058600         ,TIREGTID                                                        
058700         ,FLFEL                                                           
058800         ,FLKNTRL                                                         
058900         ,FLKLAR)                                                         
059000         VALUES(:WS-IDLEGSEL                                              
059100               ,:WS-IDBUNDLE                                              
059200               ,:WS-DATUM                                                 
059300               ,:WS-KLOCKAN-COMP                                          
059400               ,:NOO                                                      
059500               ,:NOO                                                      
059600               ,:NOO)                                                     
059700     END-EXEC                                                             
059800                                                                          
059900     MOVE SQLCODE TO SQLCODE-WS                                           
060000     PERFORM DB2-STATUS-CONTROL                                           
060100     .                                                                    
060200     EJECT                                                                
060300                                                                          
060400 DB2-SELECT-T01SYST SECTION.                                              
060500     MOVE 000100    TO GOOD-SQLCODECODES                                  
060600     EXEC SQL                                                             
060700       SELECT   KDBEHS                                                    
060800                                                                          
060900       INTO    :SYST-KDBEHS                                               
061000                                                                          
061100       FROM     T01SYST                                                   
061200                                                                          
061300     END-EXEC                                                             
061400                                                                          
061500     MOVE SQLCODE      TO SQLCODE-WS                                      
061600     PERFORM DB2-STATUS-CONTROL                                           
061700     .                                                                    
061800     EJECT                                                                
061900                                                                          
062000 DB2-UPDATE-T01SYST SECTION.                                              
062100     MOVE 000     TO GOOD-SQLCODECODES                                    
062200     EXEC SQL UPDATE T01SYST                                              
062300                                                                          
062400       SET KDBEHS   = :SYST-KDBEHS                                        
062500                                                                          
062600     END-EXEC                                                             
062700                                                                          
062800     MOVE SQLCODE TO SQLCODE-WS                                           
062900     PERFORM DB2-STATUS-CONTROL                                           
063000     .                                                                    
063100     EJECT                                                                
063200                                                                          
063300 DB2-STATUS-CONTROL SECTION.                                              
063400     SET SQLCODE-IX TO 1                                                  
063500     SEARCH GOOD-SQLCODE                                                  
063600       AT END                                                             
063700          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
063800          DELIMITED BY SIZE INTO ERRORTEXT                                
063900          CALL ABEND USING RKOD-ABEND-DB2                                 
064000       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
064100     END-SEARCH                                                           
064200     .                                                                    
