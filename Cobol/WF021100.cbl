000101 PROCESS DYNAM                                                            
000201 ID DIVISION.                                                             
000301 PROGRAM-ID.     WF021100.                                                
000401 AUTHOR.         HENRIKSSON ANDERS.                                       
000501 DATE-WRITTEN.   02/02/11.                                                
000601 DATE-COMPILED.                                                           
000701                                                                          
000801*    NAME                                                                 
000901*        CARPARTS.BILLIT.RECEIVE2                                         
001001*    FUNCTION:                                                            
001101*      - RECEIVES LINES FROM THE COMMUNICATION REGISTER                   
001201*      - READS ALL LINES VIA WZ01RECV                                     
001301*      - STORES ALL LINES IN TEMPORARY LINE REGISTER                      
001401*      - PGM WF020200 (CARPARTS.BILLIT.VALIDATE) WILL BE STARTED          
001501*        IF NO UNCONTROLLED BUNDLES EXISTS                                
001601*        (EXCEPT THE BUNDLE WICH JUST HAS BEEN PROCESSED)                 
001701*                                                                         
001801*        THE PROGRAM INSERTS/UPDATES ROWS IN TABLE T01TRAW                
001901*        THE PROGRAM INSERTS         ROWS IN TABLE T01TBUN                
002001*        THE PROGRAM READS           ROWS IN TABLE T01SYST                
002101*                                                                         
002201*    INDATA.                                                              
002301*        TRANSAKTION: WF0211X                                             
002401*        REQUEST:     HEADER ONLY                                         
002501*                                                                         
002601*    OUTDATA.                                                             
002701*        TRANSAKTION: WF0202X                                             
002801*        REQUEST:     HEADER ONLY                                         
002901*                                                                         
003001*        SÄNDNING VIA WZ01  TILL MAIL                                     
003101*        MOD:         WZ01MAIL (VIA WZ01)                                 
003201                                                                          
003301 ENVIRONMENT DIVISION.                                                    
003401     EJECT                                                                
003501                                                                          
003601 DATA DIVISION.                                                           
003701 WORKING-STORAGE SECTION.                                                 
003801 77  IDPGM                       PIC X(08)   VALUE 'WF021100'.            
003901                                                                          
004001*    --- WORK FIELD FOR ERROR MESSAGES CALLING ABEND                      
004101 77  ERRORTEXT                   PIC X(80) VALUE SPACE.                   
004201 77  KDRC-DISPLAY                PIC Z(5).                                
004301                                                                          
004401 77  WS-IX2                      PIC S9(7) VALUE +0     COMP-3.           
004501 77  MAX-LINES                   PIC S9(7) VALUE +80000 COMP-3.           
004601                                                                          
004701 77  YES                         PIC X       VALUE 'J'.                   
004801 77  NOO                         PIC X       VALUE 'N'.                   
004901 77  WS-BUNDLES-NOT-CONTROLLED   PIC X.                                   
005001 77  WS-TIME-WAIT                PIC S9(9)   COMP VALUE +0001.            
005101     EJECT                                                                
005201                                                                          
005301*    --- SUBPROGRAMS OCH PARAMETER AREAS                                  
005401 01  GENERAL-SUBPROGRAMS.                                                 
005501     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005601     03  WZ01RECV                PIC X(8)    VALUE 'WZ01RECV'.            
005701     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
005801     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
005901                                                                          
006001*    --- PARAMETERS TO ABEND                                              
006101                                                                          
006201 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
006301 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
006401 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
006501 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
006601     EJECT                                                                
006701                                                                          
006801*    --- AREAS FOR COMMUNICATION                                          
006901 01  FILLER                      PIC X(16)   VALUE 'RECV-CONTROL'.        
007001                                                                          
007101 01  -COPY WZ01RECV                                                       
007201     EJECT                                                                
007301                                                                          
007401 01  FILLER                      PIC X(16)   VALUE 'RECV-AREA'.           
007501 01  RECV-AREA.                                                           
007601*    03  -COPY WZ01REQU -PRE IN-                                          
007701*    03  -COPY WF0211I1 -PRE MID-WF0211I1-                                
007801     EJECT                                                                
007901                                                                          
008001 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
008101 01  -COPY WZ01SEND                                                       
008201     EJECT                                                                
008301                                                                          
008401 01  FILLER                      PIC X(16)   VALUE 'SEND-AREA'.           
008501 01  SEND-AREA.                                                           
008601*    03  -COPY WZ01REQU                                                   
008701     EJECT                                                                
008801                                                                          
008901 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
009001       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
009101                                                                          
009201 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
009301 01  DB2-WS.                                                              
009401     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
009501         88  INSERT-OK                       VALUE 000.                   
009601         88  CURSOR-OK                       VALUE 000.                   
009701         88  LINES-FOUND                     VALUE 000.                   
009801         88  LINES-MISSING                   VALUE 100.                   
009901         88  RESOURCE-WRONG                  VALUE 904.                   
010001                                                                          
010101     03  GOOD-SQLCODECODES.                                               
010201         05  GOOD-SQLCODE OCCURS 5                                        
010301             INDEXED BY SQLCODE-IX PIC 9(3).                              
010401     EJECT                                                                
010501                                                                          
010601 01  FILLER                    PIC X(16) VALUE 'WS-AREA         '.        
010701 01  WS-AREA.                                                             
010801     03 WS-DATUM               PIC X(8)  VALUE SPACE.                     
010901     03 WS-KLOCKAN             PIC 9(10) VALUE ZERO.                      
011001     03 WS-KLOCKAN-COMP        PIC S9(10) COMP-3 VALUE ZERO.              
011101                                                                          
011201     03 WS-IDLEGSEL            PIC X(4)  VALUE SPACE.                     
011301     03 WS-IDBUNDLE            PIC X(15) VALUE SPACE.                     
011401     03 WS-DAREGDAT            PIC X(8)  VALUE SPACE.                     
011501     03 WS-TIREGTID            PIC S9(10) COMP-3 VALUE ZERO.              
011601     03 WS-IDREF               PIC X(15) VALUE SPACE.                     
011701     03 WS-DAREFDAT            PIC X(8)  VALUE SPACE.                     
011801     03 WS-IDREFRAD            PIC S9(5) COMP-3 VALUE ZERO.               
011901     03 WS-BEVOLREF            PIC X(10) VALUE SPACE.                     
012001     03 WS-IDLANDX3-SEND       PIC X(3)  VALUE SPACE.                     
012101     03 WS-IDLANDX3-REC        PIC X(3)  VALUE SPACE.                     
012201     03 WS-IDLEVNR             PIC X(5)  VALUE SPACE.                     
012301     03 WS-IDPARTNR            PIC X(9)  VALUE SPACE.                     
012401                                                                          
012501     03 WS-IDEXCUST-1          PIC X(15) VALUE SPACE.                     
012601     03 WS-IDEXCUST-2          PIC X(15) VALUE SPACE.                     
012701     03 WS-IDEXCUST-3          PIC X(15) VALUE SPACE.                     
012801     03 WS-IDOPTION-1          PIC X(15) VALUE SPACE.                     
012901     03 WS-IDOPTION-2          PIC X(15) VALUE SPACE.                     
013001     03 WS-IDOPTION-3          PIC X(15) VALUE SPACE.                     
013101     03 WS-IDOPTION-4          PIC X(15) VALUE SPACE.                     
013201     03 WS-IDOPTION-5          PIC X(15) VALUE SPACE.                     
013301     03 WS-IDAPPEND            PIC X(8)  VALUE SPACE.                     
013401     03 WS-IDARTNR-FINANCE     PIC X(50) VALUE SPACE.                     
013501     03 WS-IDSTATNR            PIC S9(9) COMP-3 VALUE ZERO.               
013601     03 WS-VKORDBTO-KOLLI      PIC S9(6)V9(1) COMP-3 VALUE ZERO.          
013701     03 WS-VKARTNTO            PIC S9(4)V9(3) COMP-3 VALUE ZERO.          
013801     03 WS-PRARTBTO            PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
013901     03 WS-PRARTNTO            PIC S9(7)V9(2) COMP-3 VALUE ZERO.          
014001     03 WS-REARTRAB            PIC S9(2)V9(2) COMP-3 VALUE ZERO.          
014101                                                                          
014201     03 WS-KVBEART             PIC S9(7) COMP-3 VALUE ZERO.               
014301     03 WS-KVLEVART            PIC S9(7) COMP-3 VALUE ZERO.               
014401     03 WS-BEART               PIC X(25) VALUE SPACE.                     
014501     03 WS-FLSOFT              PIC X(1)  VALUE SPACE.                     
014601     03 WS-FLSPECPR            PIC X(1)  VALUE SPACE.                     
014701     03 WS-FLFREE              PIC X(1)  VALUE SPACE.                     
014801     03 WS-FLPRIV              PIC X(1)  VALUE SPACE.                     
014901     03 WS-KDVAT               PIC X(2)  VALUE SPACE.                     
015001     03 WS-KDVALISO            PIC X(3)  VALUE SPACE.                     
015101     03 WS-KDINVFRQ            PIC X(4)  VALUE SPACE.                     
015201     03 WS-KDFINDOC            PIC X(4)  VALUE SPACE.                     
015301     03 WS-IDBREAK-1           PIC X(8)  VALUE SPACE.                     
015401     03 WS-IDBREAK-2           PIC X(8)  VALUE SPACE.                     
015501                                                                          
015601     03 WS-IDSEQ-1             PIC X(8)  VALUE SPACE.                     
015701     03 WS-IDSEQ-2             PIC X(8)  VALUE SPACE.                     
015801     03 WS-IDSEQ-3             PIC X(8)  VALUE SPACE.                     
015901     03 WS-KDARTURS            PIC X(2)  VALUE SPACE.                     
016001     03 WS-KDANMORS            PIC X(2)  VALUE SPACE.                     
016101     03 WS-IDFAKREF            PIC S9(9) COMP-3 VALUE ZERO.               
016201     03 WS-DAFAKREF            PIC 9(8)  VALUE ZERO.                      
016301     03 WS-DAFAKREF-2          PIC X(8)  VALUE SPACE.                     
016401     03 WS-IDDC                PIC X(2)  VALUE SPACE.                     
016501     03 WS-KDFRAKT             PIC S9(3) COMP-3 VALUE ZERO.               
016601     03 WS-BELEVVIL            PIC X(35) VALUE SPACE.                     
016701     03 WS-IDACCNT-1           PIC X(15) VALUE SPACE.                     
016801     03 WS-IDACCNT-2           PIC X(15) VALUE SPACE.                     
016901     03 WS-IDACCNT-3           PIC X(15) VALUE SPACE.                     
017001     03 WS-IDACCNT-4           PIC X(15) VALUE SPACE.                     
017101     03 WS-IDSYSTEM-SEND       PIC X(4)  VALUE SPACE.                     
017201     03 WS-IDSYSTEM-REC        PIC X(4)  VALUE SPACE.                     
017301     03 WS-FILLER              PIC X(100) VALUE SPACE.                    
017401     03 WS-BEANST              PIC X(25) VALUE SPACE.                     
017501     03 WS-IDUSER              PIC X(8)  VALUE SPACE.                     
017601     03 WS-BETEXT              PIC X(125) VALUE SPACE.                    
017701     03 WS-BETEXT-CRE          PIC X(100) VALUE SPACE.                    
017801     03 WS-FLFEL               PIC X(1)  VALUE SPACE.                     
017901     03 WS-IDFELKOD            PIC X(3)  VALUE SPACE.                     
018001     03 WS-BEFEL               PIC X(50) VALUE SPACE.                     
018101     03 WS-IDARTNR-CNTRL       PIC X(2)  VALUE SPACE.                     
018201     03 WS-FLPCOO              PIC X(1)  VALUE SPACE.                     
018301     03 WS-IDLEVNR-ART         PIC X(5)  VALUE SPACE.                     
018302     03 WS-IDTRACK-1           PIC X(25) VALUE SPACE.                     
018303     03 WS-KVANT-TRACK-1       PIC S9(7) COMP-3 VALUE ZERO.               
018304     03 WS-IDTRACK-2           PIC X(25) VALUE SPACE.                     
018305     03 WS-KVANT-TRACK-2       PIC S9(7) COMP-3 VALUE ZERO.               
018306     03 WS-IDTRACK-3           PIC X(25) VALUE SPACE.                     
018307     03 WS-KVANT-TRACK-3       PIC S9(7) COMP-3 VALUE ZERO.               
018308     03 WS-IDTRACK-4           PIC X(25) VALUE SPACE.                     
018309     03 WS-KVANT-TRACK-4       PIC S9(7) COMP-3 VALUE ZERO.               
018310     03 WS-IDTRACK-5           PIC X(25) VALUE SPACE.                     
018320     03 WS-KVANT-TRACK-5       PIC S9(7) COMP-3 VALUE ZERO.               
018330     03 WS-KDPRMOD             PIC X(2)  VALUE SPACE.                     
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
021700     MOVE MID-WF0211I1-IDBUNDLE TO WS-IDBUNDLE                            
021800     IF RECV-KDRC = ZERO                                                  
021900       PERFORM A-INIT                                                     
022000       PERFORM UNTIL RECV-KDRC > ZERO OR WS-IX2 > MAX-LINES               
022100         IF MID-WF0211I1-IDBUNDLE NOT = WS-IDBUNDLE                       
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
024100* BLOCK WF021100 FROM STARTING WF020200                                   
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
026900     MOVE MID-WF0211I1-IDLEGSEL        TO WS-IDLEGSEL                     
027000     MOVE MID-WF0211I1-IDBUNDLE        TO WS-IDBUNDLE                     
027100     MOVE MID-WF0211I1-IDREF           TO WS-IDREF                        
027200     MOVE MID-WF0211I1-DAREFDAT        TO WS-DAREFDAT                     
027300     MOVE MID-WF0211I1-IDREFRAD        TO WS-IDREFRAD                     
027400     MOVE MID-WF0211I1-BEVOLREF        TO WS-BEVOLREF                     
027500     MOVE MID-WF0211I1-IDLANDX3-SEND   TO WS-IDLANDX3-SEND                
027600     MOVE MID-WF0211I1-IDLANDX3-REC    TO WS-IDLANDX3-REC                 
027700     MOVE MID-WF0211I1-IDLEVNR         TO WS-IDLEVNR                      
027800     MOVE MID-WF0211I1-IDPARTNR        TO WS-IDPARTNR                     
027900     MOVE MID-WF0211I1-IDEXCUST(1)     TO WS-IDEXCUST-1                   
028000     MOVE MID-WF0211I1-IDEXCUST(2)     TO WS-IDEXCUST-2                   
028100     MOVE MID-WF0211I1-IDEXCUST(3)     TO WS-IDEXCUST-3                   
028200     MOVE MID-WF0211I1-IDOPTION(1)     TO WS-IDOPTION-1                   
028300     MOVE MID-WF0211I1-IDOPTION(2)     TO WS-IDOPTION-2                   
028400     MOVE MID-WF0211I1-IDOPTION(3)     TO WS-IDOPTION-3                   
028500     MOVE MID-WF0211I1-IDOPTION(4)     TO WS-IDOPTION-4                   
028600     MOVE MID-WF0211I1-IDOPTION(5)     TO WS-IDOPTION-5                   
028700     MOVE MID-WF0211I1-IDAPPEND        TO WS-IDAPPEND                     
028800     MOVE MID-WF0211I1-IDARTNR-FINANCE TO WS-IDARTNR-FINANCE              
028900     MOVE MID-WF0211I1-IDSTATNR        TO WS-IDSTATNR                     
029000     MOVE MID-WF0211I1-VKORDBTO-KOLLI  TO WS-VKORDBTO-KOLLI               
029100     MOVE MID-WF0211I1-VKARTNTO        TO WS-VKARTNTO                     
029200     MOVE MID-WF0211I1-PRARTBTO        TO WS-PRARTBTO                     
029300     MOVE MID-WF0211I1-PRARTNTO        TO WS-PRARTNTO                     
029400     MOVE MID-WF0211I1-REARTRAB        TO WS-REARTRAB                     
029500     MOVE MID-WF0211I1-KVBEART         TO WS-KVBEART                      
029600     MOVE MID-WF0211I1-KVLEVART        TO WS-KVLEVART                     
029700     MOVE MID-WF0211I1-BEART           TO WS-BEART                        
029800     MOVE MID-WF0211I1-FLSOFT          TO WS-FLSOFT                       
029900     MOVE MID-WF0211I1-FLSPECPR        TO WS-FLSPECPR                     
030000     MOVE MID-WF0211I1-FLFREE          TO WS-FLFREE                       
030100     MOVE MID-WF0211I1-FLPRIV          TO WS-FLPRIV                       
030201     IF WS-FLSOFT = 'Y'                                                   
030301       EVALUATE MID-WF0211I1-KDVAT                                        
030401         WHEN '70'                                                        
030501           MOVE '60'                   TO WS-KDVAT                        
030601         WHEN '90'                                                        
030701           MOVE '80'                   TO WS-KDVAT                        
030801**** RUSSIA SW SHOULD HAVE 20% VAT                                        
030901**** TIS DOESN'T SEND DISTRICT NUMBERS SO WE NEED TO USE PARMA            
031001           IF MID-WF0211I1-IDPARTNR = '357086'                            
031101             MOVE 'RU'                 TO WS-KDVAT                        
031201           END-IF                                                         
031301         WHEN OTHER                                                       
031401           MOVE MID-WF0211I1-KDVAT     TO WS-KDVAT                        
031501       END-EVALUATE                                                       
031601     ELSE                                                                 
031701**** NORTHERN IRELAND AND TIS CUSTOMER                                    
031801       IF (MID-WF0211I1-IDEXCUST(1) = '1378'                              
031902       AND MID-WF0211I1-IDEXCUST(2) = '22004')                            
031903       OR MID-WF0211I1-IDEXCUST(1) = '11822004'                           
032001         MOVE '70'                     TO WS-KDVAT                        
032101       ELSE                                                               
033001         MOVE MID-WF0211I1-KDVAT       TO WS-KDVAT                        
033101       END-IF                                                             
033201     END-IF                                                               
033300     MOVE MID-WF0211I1-KDVALISO        TO WS-KDVALISO                     
033400     MOVE MID-WF0211I1-KDINVFRQ        TO WS-KDINVFRQ                     
033500     MOVE MID-WF0211I1-KDFINDOC        TO WS-KDFINDOC                     
033600     MOVE MID-WF0211I1-IDBREAK(1)      TO WS-IDBREAK-1                    
033700     MOVE MID-WF0211I1-IDBREAK(2)      TO WS-IDBREAK-2                    
033800     MOVE MID-WF0211I1-IDSEQ(1)        TO WS-IDSEQ-1                      
033900     MOVE MID-WF0211I1-IDSEQ(2)        TO WS-IDSEQ-2                      
034000     MOVE MID-WF0211I1-IDSEQ(3)        TO WS-IDSEQ-3                      
034100     MOVE MID-WF0211I1-KDARTURS        TO WS-KDARTURS                     
034200     MOVE MID-WF0211I1-KDANMORS        TO WS-KDANMORS                     
034300     MOVE MID-WF0211I1-IDFAKREF        TO WS-IDFAKREF                     
034400     MOVE MID-WF0211I1-DAFAKREF        TO WS-DAFAKREF                     
034500     MOVE MID-WF0211I1-IDDC            TO WS-IDDC                         
034600     MOVE MID-WF0211I1-KDFRAKT         TO WS-KDFRAKT                      
034700     MOVE MID-WF0211I1-BELEVVIL        TO WS-BELEVVIL                     
034800     MOVE MID-WF0211I1-IDACCNT(1)      TO WS-IDACCNT-1                    
034900     MOVE MID-WF0211I1-IDACCNT(2)      TO WS-IDACCNT-2                    
035000     MOVE MID-WF0211I1-IDACCNT(3)      TO WS-IDACCNT-3                    
035100     MOVE MID-WF0211I1-IDACCNT(4)      TO WS-IDACCNT-4                    
035200     MOVE MID-WF0211I1-IDSYSTEM-SEND   TO WS-IDSYSTEM-SEND                
035300     MOVE MID-WF0211I1-IDSYSTEM-REC    TO WS-IDSYSTEM-REC                 
035400     MOVE MID-WF0211I1-BEANST          TO WS-BEANST                       
035500     MOVE MID-WF0211I1-IDUSER          TO WS-IDUSER                       
035600     MOVE MID-WF0211I1-BELEVVIL        TO WS-BETEXT                       
035700     MOVE SPACE                        TO WS-BETEXT-CRE                   
035800     MOVE SPACE                        TO WS-IDARTNR-CNTRL                
035900     MOVE SPACE                        TO WS-FLPCOO                       
036001     MOVE SPACE                        TO WS-IDLEVNR-ART                  
036002     MOVE SPACE                        TO WS-IDTRACK-1                    
036003     MOVE ZERO                         TO WS-KVANT-TRACK-1                
036004     MOVE SPACE                        TO WS-IDTRACK-2                    
036005     MOVE ZERO                         TO WS-KVANT-TRACK-2                
036006     MOVE SPACE                        TO WS-IDTRACK-3                    
036007     MOVE ZERO                         TO WS-KVANT-TRACK-3                
036008     MOVE SPACE                        TO WS-IDTRACK-4                    
036009     MOVE ZERO                         TO WS-KVANT-TRACK-4                
036010     MOVE SPACE                        TO WS-IDTRACK-5                    
036020     MOVE ZERO                         TO WS-KVANT-TRACK-5                
036030     MOVE SPACE                        TO WS-KDPRMOD                      
036100     ADD 1                             TO WS-IX2                          
036200     .                                                                    
036300     EJECT                                                                
036400                                                                          
036500 C-INSERT-UPDATE-T01TRAW SECTION.                                         
036600     MOVE WS-DAFAKREF TO WS-DAFAKREF-2                                    
036700     PERFORM DB2-INSERT-T01TRAW                                           
036800     .                                                                    
036900     EJECT                                                                
037000                                                                          
037100 D-INSERT-T01TBUN SECTION.                                                
037200     PERFORM DB2-INSERT-T01TBUN                                           
037300     .                                                                    
037400     EJECT                                                                
037500                                                                          
037600 E-START-PGM-WF020200 SECTION.                                            
037700     MOVE IN-REQU-IDMSGVER TO REQU-IDMSGVER                               
037800     MOVE IN-REQU-KDPGMACT TO REQU-KDPGMACT                               
037900     MOVE IN-REQU-IDUSER   TO REQU-IDUSER                                 
038000     PERFORM S04-SEND-OPEN                                                
038100     PERFORM S05-SEND-MESSAGE                                             
038200     PERFORM S06-SEND-CLOSE                                               
038300     .                                                                    
038400     EJECT                                                                
038500                                                                          
038600*    --- DISPATCHER-SECTIONS                                              
038700 S01-READ-OPEN SECTION.                                                   
038800     MOVE 'OPEN'                      TO RECV-KDFUNC                      
038900     MOVE 'CARPARTS.BILLIT.RECEIVE2'  TO RECV-ADDISPABS                   
039000     CALL WZ01RECV USING RECV-CONTROL-AREA                                
039100                         RECV-OPEN-AREA                                   
039200     IF RECV-KDRC > 0                                                     
039300       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
039400       STRING 'WZ01RECV OPEN ERROR RC=' KDRC-DISPLAY                      
039500       DELIMITED BY SIZE INTO ERRORTEXT                                   
039600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
039700     END-IF                                                               
039800     .                                                                    
039900                                                                          
040000 S02-READ-MESSAGE SECTION.                                                
040100     MOVE 'GET'                           TO RECV-KDFUNC                  
040200     MOVE LENGTH OF RECV-AREA             TO RECV-KVDLEN                  
040300     CALL WZ01RECV USING RECV-CONTROL-AREA                                
040400                         RECV-KVDLEN                                      
040500                         RECV-AREA                                        
040600     IF RECV-KDRC > 1                                                     
040700       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
040800       STRING 'WZ01RECV GET ERROR RC=' KDRC-DISPLAY                       
040900       DELIMITED BY SIZE INTO ERRORTEXT                                   
041000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
041100     END-IF                                                               
041200     .                                                                    
041300                                                                          
041400 S03-READ-CLOSE SECTION.                                                  
041500     MOVE 'CLOSE'                    TO RECV-KDFUNC                       
041600     CALL WZ01RECV USING RECV-CONTROL-AREA                                
041700                                                                          
041800     IF RECV-KDRC > 0                                                     
041900       MOVE RECV-KDRC TO KDRC-DISPLAY                                     
042000       STRING 'WZ01RECV CLOSE ERROR RC=' KDRC-DISPLAY                     
042100       DELIMITED BY SIZE INTO ERRORTEXT                                   
042200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
042300     END-IF                                                               
042400     .                                                                    
042500     EJECT                                                                
042600                                                                          
042700 S04-SEND-OPEN SECTION.                                                   
042800     MOVE 'OPEN'                        TO SEND-KDFUNC                    
042900     MOVE 'CARPARTS.BILLIT.VALIDATE'    TO SEND-ADDISPABS                 
043000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
043100                         SEND-OPEN-AREA                                   
043200     IF SEND-KDRC > 0                                                     
043300       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
043400       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
043500       DELIMITED BY SIZE INTO ERRORTEXT                                   
043600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
043700     END-IF                                                               
043800     .                                                                    
043900                                                                          
044000 S05-SEND-MESSAGE SECTION.                                                
044100     MOVE 'PUT'                           TO SEND-KDFUNC                  
044200     MOVE LENGTH OF SEND-AREA             TO SEND-KVDLEN                  
044300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
044400                         SEND-KVDLEN                                      
044500                         SEND-AREA                                        
044600     IF SEND-KDRC > 0                                                     
044700       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
044800       STRING 'WZ01SEND GET ERROR RC=' KDRC-DISPLAY                       
044900       DELIMITED BY SIZE INTO ERRORTEXT                                   
045000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
045100     END-IF                                                               
045200     .                                                                    
045300                                                                          
045400 S06-SEND-CLOSE SECTION.                                                  
045500     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
045600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
045700                                                                          
045800     IF SEND-KDRC > 0                                                     
045900       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
046000       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
046100       DELIMITED BY SIZE INTO ERRORTEXT                                   
046200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
046300     END-IF                                                               
046400     .                                                                    
046500     EJECT                                                                
046600                                                                          
046700 DB2-INSERT-T01TRAW SECTION.                                              
046800     SKIP2                                                                
046900     MOVE 000    TO GOOD-SQLCODECODES                                     
047000     EXEC SQL                                                             
047100         INSERT INTO T01TRAW                                              
047200         (IDLEGSEL                                                        
047300         ,IDBUNDLE                                                        
047400         ,DAREGDAT                                                        
047500         ,TIREGTID                                                        
047600         ,IDREF                                                           
047700         ,DAREFDAT                                                        
047800         ,IDREFRAD                                                        
047900         ,BEVOLREF                                                        
048000         ,IDLANDX3_SEND                                                   
048100         ,IDLANDX3_REC                                                    
048200         ,IDLEVNR                                                         
048300         ,IDPARTNR                                                        
048400         ,IDEXCUST_1                                                      
048500         ,IDEXCUST_2                                                      
048600         ,IDEXCUST_3                                                      
048700         ,IDOPTION_1                                                      
048800         ,IDOPTION_2                                                      
048900         ,IDOPTION_3                                                      
049000         ,IDOPTION_4                                                      
049100         ,IDOPTION_5                                                      
049200         ,IDAPPEND                                                        
049300         ,IDARTNR_FINANCE                                                 
049400         ,IDSTATNR                                                        
049500         ,VKORDBTO_KOLLI                                                  
049600         ,VKARTNTO                                                        
049700         ,PRARTBTO                                                        
049800         ,PRARTNTO                                                        
049900         ,REARTRAB                                                        
050000         ,KVBEART                                                         
050100         ,KVLEVART                                                        
050200         ,BEART                                                           
050300         ,FLSOFT                                                          
050400         ,FLSPECPR                                                        
050500         ,FLFREE                                                          
050600         ,FLPRIV                                                          
050700         ,KDVAT                                                           
050800         ,KDVALISO                                                        
050900         ,KDINVFRQ                                                        
051000         ,KDFINDOC                                                        
051100         ,IDBREAK_1                                                       
051200         ,IDBREAK_2                                                       
051300         ,IDSEQ_1                                                         
051400         ,IDSEQ_2                                                         
051500         ,IDSEQ_3                                                         
051600         ,KDARTURS                                                        
051700         ,KDANMORS                                                        
051800         ,IDFAKREF                                                        
051900         ,DAFAKREF                                                        
052000         ,IDDC                                                            
052100         ,KDFRAKT                                                         
052200         ,BELEVVIL                                                        
052300         ,IDACCNT_1                                                       
052400         ,IDACCNT_2                                                       
052500         ,IDACCNT_3                                                       
052600         ,IDACCNT_4                                                       
052700         ,IDSYSTEM_SEND                                                   
052800         ,IDSYSTEM_REC                                                    
052900         ,FILLER                                                          
053000         ,IDFELKOD                                                        
053100         ,BEFEL                                                           
053200         ,BEANST                                                          
053300         ,IDUSER                                                          
053400         ,BETEXT                                                          
053500         ,BETEXT_CRE                                                      
053600         ,IDARTNR_CNTRL                                                   
053700         ,FLPCOO                                                          
053802         ,IDLEVNR_ART                                                     
053803         ,IDTRACK_1                                                       
053804         ,KVANT_TRACK_1                                                   
053805         ,IDTRACK_2                                                       
053806         ,KVANT_TRACK_2                                                   
053807         ,IDTRACK_3                                                       
053808         ,KVANT_TRACK_3                                                   
053809         ,IDTRACK_4                                                       
053810         ,KVANT_TRACK_4                                                   
053820         ,IDTRACK_5                                                       
053900         ,KVANT_TRACK_5                                                   
053901         ,KDPRMOD                                                         
053902         )                                                                
053910         VALUES(:WS-IDLEGSEL                                              
054000               ,:WS-IDBUNDLE                                              
054100               ,:WS-DATUM                                                 
054200               ,:WS-KLOCKAN-COMP                                          
054300               ,:WS-IDREF                                                 
054400               ,:WS-DAREFDAT                                              
054500               ,:WS-IDREFRAD                                              
054600               ,:WS-BEVOLREF                                              
054700               ,:WS-IDLANDX3-SEND                                         
054800               ,:WS-IDLANDX3-REC                                          
054900               ,:WS-IDLEVNR                                               
055000               ,:WS-IDPARTNR                                              
055100               ,:WS-IDEXCUST-1                                            
055200               ,:WS-IDEXCUST-2                                            
055300               ,:WS-IDEXCUST-3                                            
055400               ,:WS-IDOPTION-1                                            
055500               ,:WS-IDOPTION-2                                            
055600               ,:WS-IDOPTION-3                                            
055700               ,:WS-IDOPTION-4                                            
055800               ,:WS-IDOPTION-5                                            
055900               ,:WS-IDAPPEND                                              
056000               ,:WS-IDARTNR-FINANCE                                       
056100               ,:WS-IDSTATNR                                              
056200               ,:WS-VKORDBTO-KOLLI                                        
056300               ,:WS-VKARTNTO                                              
056400               ,:WS-PRARTBTO                                              
056500               ,:WS-PRARTNTO                                              
056600               ,:WS-REARTRAB                                              
056700               ,:WS-KVBEART                                               
056800               ,:WS-KVLEVART                                              
056900               ,:WS-BEART                                                 
057000               ,:WS-FLSOFT                                                
057100               ,:WS-FLSPECPR                                              
057200               ,:WS-FLFREE                                                
057300               ,:WS-FLPRIV                                                
057400               ,:WS-KDVAT                                                 
057500               ,:WS-KDVALISO                                              
057600               ,:WS-KDINVFRQ                                              
057700               ,:WS-KDFINDOC                                              
057800               ,:WS-IDBREAK-1                                             
057900               ,:WS-IDBREAK-2                                             
058000               ,:WS-IDSEQ-1                                               
058100               ,:WS-IDSEQ-2                                               
058200               ,:WS-IDSEQ-3                                               
058300               ,:WS-KDARTURS                                              
058400               ,:WS-KDANMORS                                              
058500               ,:WS-IDFAKREF                                              
058600               ,:WS-DAFAKREF-2                                            
058700               ,:WS-IDDC                                                  
058800               ,:WS-KDFRAKT                                               
058900               ,:WS-BELEVVIL                                              
059000               ,:WS-IDACCNT-1                                             
059100               ,:WS-IDACCNT-2                                             
059200               ,:WS-IDACCNT-3                                             
059300               ,:WS-IDACCNT-4                                             
059400               ,:WS-IDSYSTEM-SEND                                         
059500               ,:WS-IDSYSTEM-REC                                          
059600               ,:WS-FILLER                                                
059700               ,:WS-IDFELKOD                                              
059800               ,:WS-BEFEL                                                 
059900               ,:WS-BEANST                                                
060000               ,:WS-IDUSER                                                
060100               ,:WS-BETEXT                                                
060200               ,:WS-BETEXT-CRE                                            
060300               ,:WS-IDARTNR-CNTRL                                         
060400               ,:WS-FLPCOO                                                
060501               ,:WS-IDLEVNR-ART                                           
060502               ,:WS-IDTRACK-1                                             
060503               ,:WS-KVANT-TRACK-1                                         
060504               ,:WS-IDTRACK-2                                             
060505               ,:WS-KVANT-TRACK-2                                         
060506               ,:WS-IDTRACK-3                                             
060507               ,:WS-KVANT-TRACK-3                                         
060508               ,:WS-IDTRACK-4                                             
060509               ,:WS-KVANT-TRACK-4                                         
060510               ,:WS-IDTRACK-5                                             
060520               ,:WS-KVANT-TRACK-5                                         
060530               ,:WS-KDPRMOD                                               
060540               )                                                          
060600     END-EXEC                                                             
060700                                                                          
060800     MOVE SQLCODE TO SQLCODE-WS                                           
060900     PERFORM DB2-STATUS-CONTROL                                           
061000     .                                                                    
061100     EJECT                                                                
061200                                                                          
061300 DB2-INSERT-T01TBUN SECTION.                                              
061400     MOVE 000    TO GOOD-SQLCODECODES                                     
061500     EXEC SQL                                                             
061600         INSERT INTO T01TBUN                                              
061700         (IDLEGSEL                                                        
061800         ,IDBUNDLE                                                        
061900         ,DAREGDAT                                                        
062000         ,TIREGTID                                                        
062100         ,FLFEL                                                           
062200         ,FLKNTRL                                                         
062300         ,FLKLAR)                                                         
062400         VALUES(:WS-IDLEGSEL                                              
062500               ,:WS-IDBUNDLE                                              
062600               ,:WS-DATUM                                                 
062700               ,:WS-KLOCKAN-COMP                                          
062800               ,:NOO                                                      
062900               ,:NOO                                                      
063000               ,:NOO)                                                     
063100     END-EXEC                                                             
063200                                                                          
063300     MOVE SQLCODE TO SQLCODE-WS                                           
063400     PERFORM DB2-STATUS-CONTROL                                           
063500     .                                                                    
063600     EJECT                                                                
063700                                                                          
063800 DB2-SELECT-T01SYST SECTION.                                              
063900     MOVE 000100    TO GOOD-SQLCODECODES                                  
064000     EXEC SQL                                                             
064100       SELECT   KDBEHS                                                    
064200                                                                          
064300       INTO    :SYST-KDBEHS                                               
064400                                                                          
064500       FROM     T01SYST                                                   
064600                                                                          
064700     END-EXEC                                                             
064800                                                                          
064900     MOVE SQLCODE      TO SQLCODE-WS                                      
065000     PERFORM DB2-STATUS-CONTROL                                           
065100     .                                                                    
065200     EJECT                                                                
065300                                                                          
065400 DB2-UPDATE-T01SYST SECTION.                                              
065500     MOVE 000     TO GOOD-SQLCODECODES                                    
065600     EXEC SQL UPDATE T01SYST                                              
065700                                                                          
065800       SET KDBEHS   = :SYST-KDBEHS                                        
065900                                                                          
066000     END-EXEC                                                             
066100                                                                          
066200     MOVE SQLCODE TO SQLCODE-WS                                           
066300     PERFORM DB2-STATUS-CONTROL                                           
066400     .                                                                    
066500     EJECT                                                                
066600                                                                          
066700 DB2-STATUS-CONTROL SECTION.                                              
066800     SET SQLCODE-IX TO 1                                                  
066900     SEARCH GOOD-SQLCODE                                                  
067000       AT END                                                             
067100          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
067200          DELIMITED BY SIZE INTO ERRORTEXT                                
067300          CALL ABEND USING RKOD-ABEND-DB2                                 
067400       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS CONTINUE               
067500     END-SEARCH                                                           
067600     .                                                                    
