000100 PROCESS DYNAM                                                            
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     WF020100.                                                
000400 AUTHOR.         HENRIKSSON ANDERS.                                       
000500 DATE-WRITTEN.   02/02/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    NAME                                                                 
000900*        CARPARTS.BILLIT.RECEIVE                                          
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
002300*        TRANSAKTION: WF0201X                                             
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
003800 77  IDPGM                       PIC X(08)   VALUE 'WF020100'.            
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
005000                                                                          
005100 77  WS-TIME-WAIT                PIC S9(9)   COMP VALUE +0001.            
005200     EJECT                                                                
005300                                                                          
005400*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
005500 01  GENERAL-SUBPROGRAMS.                                                 
005600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005700     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
005800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
005900     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
006000                                                                          
006100*    --- PARAMETERS TO ABEND                                              
006200                                                                          
006300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006400 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006500 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
006600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006700     EJECT                                                                
006800                                                                          
006900*    --- AREAS FOR COMMUNICATION                                          
007000 01  FILLER                      PIC X(16)   VALUE 'RECV-CONTROL'.        
007100                                                                          
007200 01  -COPY WZ01RECV                                                       
007300     EJECT                                                                
007400                                                                          
007500 01  FILLER                      PIC X(16)   VALUE 'RECV-AREA'.           
007600 01  RECV-AREA.                                                           
007700*    03  -COPY WZ01REQU -PRE IN-                                          
007800*    03  -COPY WF0201I1 -PRE MID-WF0201I1-                                
007900     EJECT                                                                
008000                                                                          
008100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
008200 01  -COPY WZ01SEND                                                       
008300     EJECT                                                                
008400                                                                          
008500 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
008600 01  SEND-AREA.                                                           
008700*    03  -COPY WZ01REQU                                                   
008800     EJECT                                                                
008900                                                                          
009000 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
009100       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009200                                                                          
009300 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
009400 01  DB2-WS.                                                              
009500     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
009600         88  INSERT-OK                       VALUE 000.                   
009700         88  CURSOR-OK                       VALUE 000.                   
009800         88  LINES-FOUND                     VALUE 000.                   
009900         88  LINES-MISSING                   VALUE 100.                   
010000         88  RESOURCE-WRONG                  VALUE 904.                   
010100                                                                          
010200     03  GOOD-SQLCODECODES.                                               
010300         05  GOOD-SQLCODE OCCURS 5                                        
010400             INDEXED BY SQLCODE-IX PIC 9(3).                              
010500     EJECT                                                                
010600                                                                          
010700 01  FILLER                    PIC X(16) VALUE 'WS-AREA         '.        
010800 01  WS-AREA.                                                             
010900     03 WS-DATUM               PIC X(8)  VALUE SPACE.                     
011000     03 WS-KLOCKAN             PIC 9(10) VALUE ZERO.                      
011100     03 WS-KLOCKAN-COMP        PIC S9(10) COMP-3 VALUE ZERO.              
011200                                                                          
011300     03 WS-IDLEGSEL            PIC X(4)  VALUE SPACE.                     
011400     03 WS-IDBUNDLE            PIC X(15) VALUE SPACE.                     
011500     03 WS-DAREGDAT            PIC X(8)  VALUE SPACE.                     
011600     03 WS-TIREGTID            PIC S9(10) COMP-3 VALUE ZERO.              
011700     03 WS-IDREF               PIC X(15) VALUE SPACE.                     
011800     03 WS-DAREFDAT            PIC X(8)  VALUE SPACE.                     
011900     03 WS-IDREFRAD            PIC S9(5) COMP-3 VALUE ZERO.               
012000     03 WS-BEVOLREF            PIC X(10) VALUE SPACE.                     
012100     03 WS-IDLANDX3-SEND       PIC X(3)  VALUE SPACE.                     
012200     03 WS-IDLANDX3-REC        PIC X(3)  VALUE SPACE.                     
012300     03 WS-IDLEVNR             PIC X(5)  VALUE SPACE.                     
012400     03 WS-IDPARTNR            PIC X(9)  VALUE SPACE.                     
012500                                                                          
012600     03 WS-IDEXCUST-1          PIC X(15) VALUE SPACE.                     
012700     03 WS-IDEXCUST-2          PIC X(15) VALUE SPACE.                     
012800     03 WS-IDEXCUST-3          PIC X(15) VALUE SPACE.                     
012900     03 WS-IDOPTION-1          PIC X(15) VALUE SPACE.                     
013000     03 WS-IDOPTION-2          PIC X(15) VALUE SPACE.                     
013100     03 WS-IDOPTION-3          PIC X(15) VALUE SPACE.                     
013200     03 WS-IDOPTION-4          PIC X(15) VALUE SPACE.                     
013300     03 WS-IDOPTION-5          PIC X(15) VALUE SPACE.                     
013400     03 WS-IDAPPEND            PIC X(8)  VALUE SPACE.                     
013500     03 WS-IDARTNR-FINANCE     PIC X(50) VALUE SPACE.                     
013600     03 WS-IDSTATNR            PIC S9(9) COMP-3 VALUE ZERO.               
013700     03 WS-VKORDBTO-KOLLI      PIC S9(6)V9(1) COMP-3 VALUE ZERO.          
013800     03 WS-VKARTNTO            PIC S9(4)V9(3) COMP-3 VALUE ZERO.          
013900     03 WS-PRARTBTO            PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
014000     03 WS-PRARTNTO            PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
014100     03 WS-REARTRAB            PIC S9(2)V9(2) COMP-3 VALUE ZERO.          
014200                                                                          
014300     03 WS-KVBEART             PIC S9(7) COMP-3 VALUE ZERO.               
014400     03 WS-KVLEVART            PIC S9(7) COMP-3 VALUE ZERO.               
014500     03 WS-BEART               PIC X(25) VALUE SPACE.                     
014600     03 WS-FLSOFT              PIC X(1)  VALUE SPACE.                     
014700     03 WS-FLSPECPR            PIC X(1)  VALUE SPACE.                     
014800     03 WS-FLFREE              PIC X(1)  VALUE SPACE.                     
014900     03 WS-FLPRIV              PIC X(1)  VALUE SPACE.                     
015000     03 WS-KDVAT               PIC X(2)  VALUE SPACE.                     
015100     03 WS-KDVALISO            PIC X(3)  VALUE SPACE.                     
015200     03 WS-KDINVFRQ            PIC X(4)  VALUE SPACE.                     
015300     03 WS-KDFINDOC            PIC X(4)  VALUE SPACE.                     
015400     03 WS-IDBREAK-1           PIC X(8)  VALUE SPACE.                     
015500     03 WS-IDBREAK-2           PIC X(8)  VALUE SPACE.                     
015600                                                                          
015700     03 WS-IDSEQ-1             PIC X(8)  VALUE SPACE.                     
015800     03 WS-IDSEQ-2             PIC X(8)  VALUE SPACE.                     
015900     03 WS-IDSEQ-3             PIC X(8)  VALUE SPACE.                     
016000     03 WS-KDARTURS            PIC X(2)  VALUE SPACE.                     
016100     03 WS-KDANMORS            PIC X(2)  VALUE SPACE.                     
016200     03 WS-IDFAKREF            PIC S9(9) COMP-3 VALUE ZERO.               
016300     03 WS-DAFAKREF            PIC 9(8)  VALUE ZERO.                      
016400     03 WS-DAFAKREF-2          PIC X(8)  VALUE SPACE.                     
016500     03 WS-IDDC                PIC X(2)  VALUE SPACE.                     
016600     03 WS-KDFRAKT             PIC S9(3) COMP-3 VALUE ZERO.               
016700     03 WS-BELEVVIL            PIC X(35) VALUE SPACE.                     
016800     03 WS-IDACCNT-1           PIC X(15) VALUE SPACE.                     
016900     03 WS-IDACCNT-2           PIC X(15) VALUE SPACE.                     
017000     03 WS-IDACCNT-3           PIC X(15) VALUE SPACE.                     
017100     03 WS-IDACCNT-4           PIC X(15) VALUE SPACE.                     
017200     03 WS-IDSYSTEM-SEND       PIC X(4)  VALUE SPACE.                     
017300     03 WS-IDSYSTEM-REC        PIC X(4)  VALUE SPACE.                     
017400     03 WS-BEANST              PIC X(25) VALUE SPACE.                     
017500     03 WS-IDUSER              PIC X(8)  VALUE SPACE.                     
017600     03 WS-BETEXT              PIC X(125) VALUE SPACE.                    
017700     03 WS-BETEXT-CRE          PIC X(100) VALUE SPACE.                    
017800     03 WS-FILLER              PIC X(100) VALUE SPACE.                    
017900     03 WS-FLFEL               PIC X(1)  VALUE SPACE.                     
018000     03 WS-IDFELKOD            PIC X(3)  VALUE SPACE.                     
018100     03 WS-BEFEL               PIC X(50) VALUE SPACE.                     
018200     03 WS-IDARTNR-CNTRL       PIC X(2)  VALUE SPACE.                     
018300     03 WS-FLPCOO              PIC X(1)  VALUE SPACE.                     
018310     03 WS-IDLEVNR-ART         PIC X(5)  VALUE SPACE.                     
018320     03 WS-IDTRACK-1           PIC X(25) VALUE SPACE.                     
018321     03 WS-KVANT-TRACK-1       PIC S9(7) COMP-3 VALUE ZERO.               
018330     03 WS-IDTRACK-2           PIC X(25) VALUE SPACE.                     
018331     03 WS-KVANT-TRACK-2       PIC S9(7) COMP-3 VALUE ZERO.               
018340     03 WS-IDTRACK-3           PIC X(25) VALUE SPACE.                     
018341     03 WS-KVANT-TRACK-3       PIC S9(7) COMP-3 VALUE ZERO.               
018350     03 WS-IDTRACK-4           PIC X(25) VALUE SPACE.                     
018351     03 WS-KVANT-TRACK-4       PIC S9(7) COMP-3 VALUE ZERO.               
018360     03 WS-IDTRACK-5           PIC X(25) VALUE SPACE.                     
018370     03 WS-KVANT-TRACK-5       PIC S9(7) COMP-3 VALUE ZERO.               
018380     03 WS-KDPRMOD             PIC X(2)  VALUE SPACE.                     
018400                                                                          
018500     03 WS-ERRORTEXT           PIC X(50) VALUE SPACE.                     
018600                                                                          
018700     03 SYST-KDBEHS            PIC X(1)  VALUE SPACE.                     
018800                                                                          
018900 01  FILLER                    PIC X(16)    VALUE 'T01TRAW-AREA'.         
019000*01  -COPY T01TRAW -PRE T01TRAW-                                          
019100     EJECT                                                                
019200                                                                          
019300 01  FILLER                    PIC X(16)    VALUE 'T01TBUN-AREA'.         
019400*01  -COPY T01TBUN -PRE T01TBUN-                                          
019500     EJECT                                                                
019600                                                                          
019700 01  FILLER                    PIC X(16)    VALUE 'T01SYST-AREA'.         
019800*01  -COPY T01SYST -PRE T01SYST-                                          
019900     EJECT                                                                
020000                                                                          
020100     EXEC SQL INCLUDE T01TRAW END-EXEC.                                   
020200     EJECT                                                                
020300                                                                          
020400     EXEC SQL INCLUDE T01TBUN END-EXEC.                                   
020500     EJECT                                                                
020600                                                                          
020700     EXEC SQL INCLUDE T01SYST END-EXEC.                                   
020800     EJECT                                                                
020900                                                                          
021000 LINKAGE SECTION.                                                         
021100                                                                          
021200 PROCEDURE DIVISION.                                                      
021300 MAIN SECTION.                                                            
021400     PERFORM S01-READ-OPEN                                                
021500                                                                          
021600     PERFORM S02-READ-MESSAGE                                             
021700     MOVE MID-WF0201I1-IDBUNDLE TO WS-IDBUNDLE                            
021800     IF RECV-KDRC = ZERO                                                  
021900       PERFORM A-INIT                                                     
022000       PERFORM UNTIL RECV-KDRC > ZERO OR WS-IX2 > MAX-LINES               
022100         IF MID-WF0201I1-IDBUNDLE NOT = WS-IDBUNDLE                       
022200* MORE THAN ONE BUNDLEID IN THE SAME SESSION NOT ALLOWED                  
022300           CALL ABEND USING RKOD-ABEND-WITH-DUMP                          
022400         END-IF                                                           
022500         PERFORM B-LINES                                                  
022600         PERFORM C-INSERT-UPDATE-T01TRAW                                  
022700         PERFORM S02-READ-MESSAGE                                         
022800       END-PERFORM                                                        
022900* TO MANY LINES ADD MORE LINES TO MAX-LINES OR A RESTART                  
023000       IF WS-IX2 > MAX-LINES                                              
023100         CALL ABEND USING RKOD-ABEND-WITH-DUMP                            
023200       END-IF                                                             
023300       PERFORM D-INSERT-T01TBUN                                           
023400                                                                          
023500* CHECKS IF PROGRAM WF020200 IS STILL RUNNING                             
023600       PERFORM DB2-SELECT-T01SYST                                         
023700       IF SYST-KDBEHS = SPACE                                             
023800         PERFORM E-START-PGM-WF020200                                     
023900       ELSE                                                               
024000* THE PROCESS IN WF020200 WAS RUNNING AND                                 
024100* BLOCK WF020100 FROM STARTING WF020200                                   
024200         MOVE 'F' TO SYST-KDBEHS                                          
024300         PERFORM DB2-UPDATE-T01SYST                                       
024400       END-IF                                                             
024500     END-IF                                                               
024600                                                                          
024700     PERFORM S03-READ-CLOSE                                               
024800     MOVE ZERO TO RETURN-CODE                                             
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200                                                                          
025300 A-INIT SECTION.                                                          
025400     CALL W009WAIT USING WS-TIME-WAIT                                     
025500     MOVE FUNCTION CURRENT-DATE (1:8)  TO WS-DATUM                        
025600     MOVE FUNCTION CURRENT-DATE (9:8)  TO WS-KLOCKAN                      
025700     MOVE WS-KLOCKAN                   TO WS-KLOCKAN-COMP                 
025800                                                                          
025900     MOVE 'N'                          TO WS-FLFEL                        
026000     MOVE SPACE                        TO WS-IDFELKOD                     
026100                                          WS-BEFEL                        
026200     MOVE ZERO                         TO WS-IX2                          
026300                                                                          
026400     INITIALIZE GOOD-SQLCODECODES                                         
026500     .                                                                    
026600     EJECT                                                                
026700                                                                          
026800 B-LINES SECTION.                                                         
026900     MOVE MID-WF0201I1-IDLEGSEL        TO WS-IDLEGSEL                     
027000     MOVE MID-WF0201I1-IDBUNDLE        TO WS-IDBUNDLE                     
027100     MOVE MID-WF0201I1-IDREF           TO WS-IDREF                        
027200     MOVE MID-WF0201I1-DAREFDAT        TO WS-DAREFDAT                     
027300     MOVE MID-WF0201I1-IDREFRAD        TO WS-IDREFRAD                     
027400     MOVE MID-WF0201I1-BEVOLREF        TO WS-BEVOLREF                     
027500     MOVE MID-WF0201I1-IDLANDX3-SEND   TO WS-IDLANDX3-SEND                
027600     MOVE MID-WF0201I1-IDLANDX3-REC    TO WS-IDLANDX3-REC                 
027700     MOVE MID-WF0201I1-IDLEVNR         TO WS-IDLEVNR                      
027800     MOVE MID-WF0201I1-IDPARTNR        TO WS-IDPARTNR                     
027900     MOVE MID-WF0201I1-IDEXCUST(1)     TO WS-IDEXCUST-1                   
028000     MOVE MID-WF0201I1-IDEXCUST(2)     TO WS-IDEXCUST-2                   
028100     MOVE MID-WF0201I1-IDEXCUST(3)     TO WS-IDEXCUST-3                   
028200     MOVE MID-WF0201I1-IDOPTION(1)     TO WS-IDOPTION-1                   
028300     MOVE MID-WF0201I1-IDOPTION(2)     TO WS-IDOPTION-2                   
028400     MOVE MID-WF0201I1-IDOPTION(3)     TO WS-IDOPTION-3                   
028500     MOVE MID-WF0201I1-IDOPTION(4)     TO WS-IDOPTION-4                   
028600     MOVE MID-WF0201I1-IDOPTION(5)     TO WS-IDOPTION-5                   
028700     MOVE MID-WF0201I1-IDAPPEND        TO WS-IDAPPEND                     
028800     MOVE MID-WF0201I1-IDARTNR-FINANCE TO WS-IDARTNR-FINANCE              
028900     MOVE MID-WF0201I1-IDSTATNR        TO WS-IDSTATNR                     
029000     MOVE MID-WF0201I1-VKORDBTO-KOLLI  TO WS-VKORDBTO-KOLLI               
029100     MOVE MID-WF0201I1-VKARTNTO        TO WS-VKARTNTO                     
029200     MOVE MID-WF0201I1-PRARTBTO        TO WS-PRARTBTO                     
029300     MOVE MID-WF0201I1-PRARTNTO        TO WS-PRARTNTO                     
029400     MOVE MID-WF0201I1-REARTRAB        TO WS-REARTRAB                     
029500     MOVE MID-WF0201I1-KVBEART         TO WS-KVBEART                      
029600     MOVE MID-WF0201I1-KVLEVART        TO WS-KVLEVART                     
029700     MOVE MID-WF0201I1-BEART           TO WS-BEART                        
029800     MOVE MID-WF0201I1-FLSOFT          TO WS-FLSOFT                       
029900     MOVE MID-WF0201I1-FLSPECPR        TO WS-FLSPECPR                     
030000     MOVE MID-WF0201I1-FLFREE          TO WS-FLFREE                       
030100     MOVE MID-WF0201I1-FLPRIV          TO WS-FLPRIV                       
030200     MOVE MID-WF0201I1-KDVAT           TO WS-KDVAT                        
030300     MOVE MID-WF0201I1-KDVALISO        TO WS-KDVALISO                     
030400     MOVE MID-WF0201I1-KDINVFRQ        TO WS-KDINVFRQ                     
030500     MOVE MID-WF0201I1-KDFINDOC        TO WS-KDFINDOC                     
030600     MOVE MID-WF0201I1-IDBREAK(1)      TO WS-IDBREAK-1                    
030700     MOVE MID-WF0201I1-IDBREAK(2)      TO WS-IDBREAK-2                    
030800     MOVE MID-WF0201I1-IDSEQ(1)        TO WS-IDSEQ-1                      
030900     MOVE MID-WF0201I1-IDSEQ(2)        TO WS-IDSEQ-2                      
031000     MOVE MID-WF0201I1-IDSEQ(3)        TO WS-IDSEQ-3                      
031100     MOVE MID-WF0201I1-KDARTURS        TO WS-KDARTURS                     
031200     MOVE MID-WF0201I1-KDANMORS        TO WS-KDANMORS                     
031300     MOVE MID-WF0201I1-IDFAKREF        TO WS-IDFAKREF                     
031400     MOVE MID-WF0201I1-DAFAKREF        TO WS-DAFAKREF                     
031500     MOVE MID-WF0201I1-IDDC            TO WS-IDDC                         
031600     MOVE MID-WF0201I1-KDFRAKT         TO WS-KDFRAKT                      
031700     MOVE MID-WF0201I1-BELEVVIL        TO WS-BELEVVIL                     
031800     MOVE MID-WF0201I1-IDACCNT(1)      TO WS-IDACCNT-1                    
031900     MOVE MID-WF0201I1-IDACCNT(2)      TO WS-IDACCNT-2                    
032000     MOVE MID-WF0201I1-IDACCNT(3)      TO WS-IDACCNT-3                    
032100     MOVE MID-WF0201I1-IDACCNT(4)      TO WS-IDACCNT-4                    
032200     MOVE MID-WF0201I1-IDSYSTEM-SEND   TO WS-IDSYSTEM-SEND                
032300     MOVE MID-WF0201I1-IDSYSTEM-REC    TO WS-IDSYSTEM-REC                 
032400     MOVE MID-WF0201I1-BEANST          TO WS-BEANST                       
032500     MOVE MID-WF0201I1-IDUSER          TO WS-IDUSER                       
032600     MOVE MID-WF0201I1-BETEXT          TO WS-BETEXT                       
032700     MOVE SPACE                        TO WS-BETEXT-CRE                   
032800     MOVE MID-WF0201I1-IDARTNR-CNTRL   TO WS-IDARTNR-CNTRL                
032900     MOVE MID-WF0201I1-FLPCOO          TO WS-FLPCOO                       
032910     MOVE MID-WF0201I1-IDLEVNR-ART     TO WS-IDLEVNR-ART                  
032920     MOVE MID-WF0201I1-IDTRACK(1)      TO WS-IDTRACK-1                    
032930     MOVE MID-WF0201I1-KVTRACK-LEV(1)  TO WS-KVANT-TRACK-1                
032940     MOVE MID-WF0201I1-IDTRACK(2)      TO WS-IDTRACK-2                    
032950     MOVE MID-WF0201I1-KVTRACK-LEV(2)  TO WS-KVANT-TRACK-2                
032960     MOVE MID-WF0201I1-IDTRACK(3)      TO WS-IDTRACK-3                    
032970     MOVE MID-WF0201I1-KVTRACK-LEV(3)  TO WS-KVANT-TRACK-3                
032980     MOVE MID-WF0201I1-IDTRACK(4)      TO WS-IDTRACK-4                    
032990     MOVE MID-WF0201I1-KVTRACK-LEV(4)  TO WS-KVANT-TRACK-4                
032991     MOVE MID-WF0201I1-IDTRACK(5)      TO WS-IDTRACK-5                    
032992     MOVE MID-WF0201I1-KVTRACK-LEV(5)  TO WS-KVANT-TRACK-5                
032993     MOVE MID-WF0201I1-KDPRMOD         TO WS-KDPRMOD                      
033000     ADD 1                             TO WS-IX2                          
033100     .                                                                    
033200     EJECT                                                                
033300                                                                          
033400 C-INSERT-UPDATE-T01TRAW SECTION.                                         
033500     MOVE WS-DAFAKREF TO WS-DAFAKREF-2                                    
033600     PERFORM DB2-INSERT-T01TRAW                                           
033700     .                                                                    
033800     EJECT                                                                
033900                                                                          
034000 D-INSERT-T01TBUN SECTION.                                                
034100     PERFORM DB2-INSERT-T01TBUN                                           
034200     .                                                                    
034300     EJECT                                                                
034400                                                                          
034500 E-START-PGM-WF020200 SECTION.                                            
034600     MOVE IN-REQU-IDMSGVER TO REQU-IDMSGVER                               
034700     MOVE IN-REQU-KDPGMACT TO REQU-KDPGMACT                               
034800     MOVE IN-REQU-IDUSER   TO REQU-IDUSER                                 
034900     PERFORM S04-SEND-OPEN                                                
035000     PERFORM S05-SEND-MESSAGE                                             
035100     PERFORM S06-SEND-CLOSE                                               
035200     .                                                                    
035300     EJECT                                                                
035400                                                                          
035500*    --- DISPATCHER-SECTIONS                                              
035600 S01-READ-OPEN SECTION.                                                   
035700     MOVE 'OPEN'                      TO RECV-KDFUNC                      
035800     MOVE 'CARPARTS.BILLIT.RECEIVE'   TO RECV-ADDISPABS                   
035900     CALL WZ01RECV USING RECV-CONTROL-AREA                                
036000                         RECV-OPEN-AREA                                   
036100     IF RECV-KDRC > 0                                                     
036200       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
036300       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
036400       DELIMITED BY SIZE INTO ERRORTEXT                                   
036500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
036600     END-IF                                                               
036700     .                                                                    
036800                                                                          
036900 S02-READ-MESSAGE SECTION.                                                
037000     MOVE 'GET'                           TO RECV-KDFUNC                  
037100     MOVE LENGTH OF RECV-AREA             TO RECV-KVDLEN                  
037200     CALL WZ01RECV USING RECV-CONTROL-AREA                                
037300                         RECV-KVDLEN                                      
037400                         RECV-AREA                                        
037500     IF RECV-KDRC > 1                                                     
037600       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
037700       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
037800       DELIMITED BY SIZE INTO ERRORTEXT                                   
037900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
038000     END-IF                                                               
038100     .                                                                    
038200                                                                          
038300 S03-READ-CLOSE SECTION.                                                  
038400     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
038500     CALL WZ01RECV USING RECV-CONTROL-AREA                                
038600                                                                          
038700     IF RECV-KDRC > 0                                                     
038800       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
038900       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
039000       DELIMITED BY SIZE INTO ERRORTEXT                                   
039100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039200     END-IF                                                               
039300     .                                                                    
039400     EJECT                                                                
039500                                                                          
039600 S04-SEND-OPEN SECTION.                                                   
039700     MOVE 'OPEN'                        TO SEND-KDFUNC                    
039800     MOVE 'CARPARTS.BILLIT.VALIDATE'    TO SEND-ADDISPABS                 
039900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
040000                         SEND-OPEN-AREA                                   
040100     IF SEND-KDRC > 0                                                     
040200       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
040300       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
040400       DELIMITED BY SIZE INTO ERRORTEXT                                   
040500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
040600     END-IF                                                               
040700     .                                                                    
040800                                                                          
040900 S05-SEND-MESSAGE SECTION.                                                
041000     MOVE 'PUT'                           TO SEND-KDFUNC                  
041100     MOVE LENGTH OF SEND-AREA             TO SEND-KVDLEN                  
041200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
041300                         SEND-KVDLEN                                      
041400                         SEND-AREA                                        
041500     IF SEND-KDRC > 0                                                     
041600       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
041700       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
041800       DELIMITED BY SIZE INTO ERRORTEXT                                   
041900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
042000     END-IF                                                               
042100     .                                                                    
042200                                                                          
042300 S06-SEND-CLOSE SECTION.                                                  
042400     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
042500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
042600                                                                          
042700     IF SEND-KDRC > 0                                                     
042800       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
042900       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
043000       DELIMITED BY SIZE INTO ERRORTEXT                                   
043100       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
043200     END-IF                                                               
043300     .                                                                    
043400     EJECT                                                                
043500                                                                          
043600 DB2-INSERT-T01TRAW SECTION.                                              
043700     SKIP2                                                                
043800     MOVE 000    TO GOOD-SQLCODECODES                                     
043900     EXEC SQL                                                             
044000         INSERT INTO T01TRAW                                              
044100          (IDLEGSEL                                                       
044200          ,IDBUNDLE                                                       
044300          ,DAREGDAT                                                       
044400          ,TIREGTID                                                       
044500          ,IDREF                                                          
044600          ,DAREFDAT                                                       
044700          ,IDREFRAD                                                       
044800          ,BEVOLREF                                                       
044900          ,IDLANDX3_SEND                                                  
045000          ,IDLANDX3_REC                                                   
045100          ,IDLEVNR                                                        
045200          ,IDPARTNR                                                       
045300          ,IDEXCUST_1                                                     
045400          ,IDEXCUST_2                                                     
045500          ,IDEXCUST_3                                                     
045600          ,IDOPTION_1                                                     
045700          ,IDOPTION_2                                                     
045800          ,IDOPTION_3                                                     
045900          ,IDOPTION_4                                                     
046000          ,IDOPTION_5                                                     
046100          ,IDAPPEND                                                       
046200          ,IDARTNR_FINANCE                                                
046300          ,IDSTATNR                                                       
046400          ,VKORDBTO_KOLLI                                                 
046500          ,VKARTNTO                                                       
046600          ,PRARTBTO                                                       
046700          ,PRARTNTO                                                       
046800          ,REARTRAB                                                       
046900          ,KVBEART                                                        
047000          ,KVLEVART                                                       
047100          ,BEART                                                          
047200          ,FLSOFT                                                         
047300          ,FLSPECPR                                                       
047400          ,FLFREE                                                         
047500          ,FLPRIV                                                         
047600          ,KDVAT                                                          
047700          ,KDVALISO                                                       
047800          ,KDINVFRQ                                                       
047900          ,KDFINDOC                                                       
048000          ,IDBREAK_1                                                      
048100          ,IDBREAK_2                                                      
048200          ,IDSEQ_1                                                        
048300          ,IDSEQ_2                                                        
048400          ,IDSEQ_3                                                        
048500          ,KDARTURS                                                       
048600          ,KDANMORS                                                       
048700          ,IDFAKREF                                                       
048800          ,DAFAKREF                                                       
048900          ,IDDC                                                           
049000          ,KDFRAKT                                                        
049100          ,BELEVVIL                                                       
049200          ,IDACCNT_1                                                      
049300          ,IDACCNT_2                                                      
049400          ,IDACCNT_3                                                      
049500          ,IDACCNT_4                                                      
049600          ,IDSYSTEM_SEND                                                  
049700          ,IDSYSTEM_REC                                                   
049800          ,FILLER                                                         
049900          ,IDFELKOD                                                       
050000          ,BEFEL                                                          
050100          ,BEANST                                                         
050200          ,IDUSER                                                         
050300          ,BETEXT                                                         
050400          ,BETEXT_CRE                                                     
050500          ,IDARTNR_CNTRL                                                  
050600          ,FLPCOO                                                         
050610          ,IDLEVNR_ART                                                    
050630          ,IDTRACK_1                                                      
050640          ,KVANT_TRACK_1                                                  
050650          ,IDTRACK_2                                                      
050660          ,KVANT_TRACK_2                                                  
050670          ,IDTRACK_3                                                      
050680          ,KVANT_TRACK_3                                                  
050690          ,IDTRACK_4                                                      
050691          ,KVANT_TRACK_4                                                  
050692          ,IDTRACK_5                                                      
050700          ,KVANT_TRACK_5                                                  
050701          ,KDPRMOD)                                                       
050710         VALUES(:WS-IDLEGSEL                                              
050800               ,:WS-IDBUNDLE                                              
050900               ,:WS-DATUM                                                 
051000               ,:WS-KLOCKAN-COMP                                          
051100               ,:WS-IDREF                                                 
051200               ,:WS-DAREFDAT                                              
051300               ,:WS-IDREFRAD                                              
051400               ,:WS-BEVOLREF                                              
051500               ,:WS-IDLANDX3-SEND                                         
051600               ,:WS-IDLANDX3-REC                                          
051700               ,:WS-IDLEVNR                                               
051800               ,:WS-IDPARTNR                                              
051900               ,:WS-IDEXCUST-1                                            
052000               ,:WS-IDEXCUST-2                                            
052100               ,:WS-IDEXCUST-3                                            
052200               ,:WS-IDOPTION-1                                            
052300               ,:WS-IDOPTION-2                                            
052400               ,:WS-IDOPTION-3                                            
052500               ,:WS-IDOPTION-4                                            
052600               ,:WS-IDOPTION-5                                            
052700               ,:WS-IDAPPEND                                              
052800               ,:WS-IDARTNR-FINANCE                                       
052900               ,:WS-IDSTATNR                                              
053000               ,:WS-VKORDBTO-KOLLI                                        
053100               ,:WS-VKARTNTO                                              
053200               ,:WS-PRARTBTO                                              
053300               ,:WS-PRARTNTO                                              
053400               ,:WS-REARTRAB                                              
053500               ,:WS-KVBEART                                               
053600               ,:WS-KVLEVART                                              
053700               ,:WS-BEART                                                 
053800               ,:WS-FLSOFT                                                
053900               ,:WS-FLSPECPR                                              
054000               ,:WS-FLFREE                                                
054100               ,:WS-FLPRIV                                                
054200               ,:WS-KDVAT                                                 
054300               ,:WS-KDVALISO                                              
054400               ,:WS-KDINVFRQ                                              
054500               ,:WS-KDFINDOC                                              
054600               ,:WS-IDBREAK-1                                             
054700               ,:WS-IDBREAK-2                                             
054800               ,:WS-IDSEQ-1                                               
054900               ,:WS-IDSEQ-2                                               
055000               ,:WS-IDSEQ-3                                               
055100               ,:WS-KDARTURS                                              
055200               ,:WS-KDANMORS                                              
055300               ,:WS-IDFAKREF                                              
055400               ,:WS-DAFAKREF-2                                            
055500               ,:WS-IDDC                                                  
055600               ,:WS-KDFRAKT                                               
055700               ,:WS-BELEVVIL                                              
055800               ,:WS-IDACCNT-1                                             
055900               ,:WS-IDACCNT-2                                             
056000               ,:WS-IDACCNT-3                                             
056100               ,:WS-IDACCNT-4                                             
056200               ,:WS-IDSYSTEM-SEND                                         
056300               ,:WS-IDSYSTEM-REC                                          
056400               ,:WS-FILLER                                                
056500               ,:WS-IDFELKOD                                              
056600               ,:WS-BEFEL                                                 
056700               ,:WS-BEANST                                                
056800               ,:WS-IDUSER                                                
056900               ,:WS-BETEXT                                                
057000               ,:WS-BETEXT-CRE                                            
057100               ,:WS-IDARTNR-CNTRL                                         
057200               ,:WS-FLPCOO                                                
057210               ,:WS-IDLEVNR-ART                                           
057220               ,:WS-IDTRACK-1                                             
057230               ,:WS-KVANT-TRACK-1                                         
057240               ,:WS-IDTRACK-2                                             
057250               ,:WS-KVANT-TRACK-2                                         
057260               ,:WS-IDTRACK-3                                             
057270               ,:WS-KVANT-TRACK-3                                         
057280               ,:WS-IDTRACK-4                                             
057290               ,:WS-KVANT-TRACK-4                                         
057291               ,:WS-IDTRACK-5                                             
057292               ,:WS-KVANT-TRACK-5                                         
057293               ,:WS-KDPRMOD)                                              
057300     END-EXEC                                                             
057400                                                                          
057500     MOVE SQLCODE TO SQLCODE-WS                                           
057600     PERFORM DB2-STATUS-CONTROL                                           
057700     .                                                                    
057800     EJECT                                                                
057900                                                                          
058000 DB2-INSERT-T01TBUN SECTION.                                              
058100     MOVE 000    TO GOOD-SQLCODECODES                                     
058200     EXEC SQL                                                             
058300         INSERT INTO T01TBUN                                              
058400         (IDLEGSEL,                                                       
058500          IDBUNDLE,                                                       
058600          DAREGDAT,                                                       
058700          TIREGTID,                                                       
058800          FLFEL,                                                          
058900          FLKNTRL,                                                        
059000          FLKLAR)                                                         
059100         VALUES(:WS-IDLEGSEL,                                             
059200                :WS-IDBUNDLE,                                             
059300                :WS-DATUM,                                                
059400                :WS-KLOCKAN-COMP,                                         
059500                :NOO,                                                     
059600                :NOO,                                                     
059700                :NOO)                                                     
059800     END-EXEC                                                             
059900                                                                          
060000     MOVE SQLCODE TO SQLCODE-WS                                           
060100     PERFORM DB2-STATUS-CONTROL                                           
060200     .                                                                    
060300     EJECT                                                                
060400                                                                          
060500 DB2-SELECT-T01SYST SECTION.                                              
060600     MOVE 000100    TO GOOD-SQLCODECODES                                  
060700     EXEC SQL                                                             
060800       SELECT   KDBEHS                                                    
060900                                                                          
061000       INTO    :SYST-KDBEHS                                               
061100                                                                          
061200       FROM     T01SYST                                                   
061300                                                                          
061400     END-EXEC                                                             
061500                                                                          
061600     MOVE SQLCODE      TO SQLCODE-WS                                      
061700     PERFORM DB2-STATUS-CONTROL                                           
061800     .                                                                    
061900     EJECT                                                                
062000                                                                          
062100 DB2-UPDATE-T01SYST SECTION.                                              
062200     MOVE 000     TO GOOD-SQLCODECODES                                    
062300     EXEC SQL UPDATE T01SYST                                              
062400                                                                          
062500       SET KDBEHS   = :SYST-KDBEHS                                        
062600                                                                          
062700     END-EXEC                                                             
062800                                                                          
062900     MOVE SQLCODE TO SQLCODE-WS                                           
063000     PERFORM DB2-STATUS-CONTROL                                           
063100     .                                                                    
063200     EJECT                                                                
063300                                                                          
063400 DB2-STATUS-CONTROL SECTION.                                              
063500     SET SQLCODE-IX TO 1                                                  
063600     SEARCH GOOD-SQLCODE                                                  
063700       AT END                                                             
063800          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
063900          DELIMITED BY SIZE INTO ERRORTEXT                                
064000          CALL ABEND USING RKOD-ABEND-DB2                                 
064100       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
064200     END-SEARCH                                                           
064300     .                                                                    
