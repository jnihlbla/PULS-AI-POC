000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF211400.                                                
000300 AUTHOR.         ARCHANA BHAT                                             
000400 DATE-WRITTEN.   2021-11-16.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THE PGM                                                          
001000*        - READS FILE WITH DOCUMENT DATA RECORDS                          
001100*        - SENDS INVOICE DATA RECORDS FOR VCCS TO ECOM THRU API           
001200*                                                                         
001300 ENVIRONMENT DIVISION.                                                    
001400                                                                          
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800*          --- DOCUMENT DATA RECORDS                                      
001900     SELECT WF2011                     ASSIGN TO WF2114D1.                
002000     EJECT                                                                
002100 DATA DIVISION.                                                           
002200                                                                          
002300 FILE SECTION.                                                            
002400 FD  WF2011                                                               
002500     RECORDING       V                                                    
002600     BLOCK CONTAINS  0.                                                   
002700                                                                          
002800 01  IN-DATA.                                                             
002900     03  IN-IDPTYP               PIC X(3).                                
003000     03  FILLER                  PIC X(16).                               
003100     03  IN-IDLEGSEL             PIC X(4).                                
003200     03  FILLER                  PIC X(3668).                             
003300     03  IN-IDSYSTEM-SEND        PIC X(4).                                
003400     03  FILLER                  PIC X(2000).                             
003500     EJECT                                                                
003600                                                                          
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900 77  IDPGM                       PIC X(8)    VALUE 'WF211400'.            
004000 77  YES                         PIC X(1)    VALUE 'J'.                   
004100 77  NOO                         PIC X(1)    VALUE 'N'.                   
004200 77  WS-ZERO                     PIC X(5)    VALUE '00000'.               
004300 77  WS-NINE                     PIC X(5)    VALUE '99999'.               
004400 77  WS-IDLEGSEL-VCCS            PIC X(4)    VALUE 'VCCS'.                
004500 77  WS-IDPARTNR                 PIC X(9)    VALUE SPACE.                 
004600 77  WS-DOC-HEAD                 PIC X(3)    VALUE '1  '.                 
004700 77  WS-DOC-LINE                 PIC X(3)    VALUE '2  '.                 
004800 77  WS-DOC-FOOTER               PIC X(3)    VALUE '3  '.                 
004900 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
005000 77  WS-NUM-SEG                  PIC 9(6)    VALUE ZERO.                  
005100 77  INDX                        PIC 9(3)    VALUE ZERO.                  
005200 77  WS-USER-KEY                 PIC X(50)   VALUE SPACES.                
005400 77  WS-SAVE-IDFINDOC            PIC 9(9)   VALUE ZERO.                   
005500 77  WS-CNT                      PIC 9(2)   VALUE ZERO.                   
005600 77  WS-INV-ALPHA                PIC X(9)   VALUE SPACES.                 
005700 77  WS-INV-ALPHA1               PIC X(9)   VALUE SPACES.                 
005800                                                                          
005900 77  WF2011-FIRST-RECORD         PIC X       VALUE ' '.                   
006000     88  FIRST-RECORD                        VALUE 'J'.                   
006100     88  OTHER-RECORDS                       VALUE 'N'.                   
006200                                                                          
006300 77  WF2011-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-WF2011                       VALUE 'J'.                   
006500                                                                          
006600 01  WS-PARCEL-ID.                                                        
006700     03 WS-IDDISTR               PIC X(4).                                
006800     03 WS-IDDISTR-N  REDEFINES WS-IDDISTR                                
006900                                 PIC 9(4).                                
007000     03 WS-IDKUNDNR              PIC X(6).                                
007100     03 WS-IDKUNDNR-N REDEFINES WS-IDKUNDNR                               
007200                                 PIC 9(6).                                
007300     03 WS-IDORDER               PIC X(7).                                
007400     03 WS-IDORDER-N REDEFINES WS-IDORDER                                 
007500                                 PIC 9(7).                                
007600     03 WS-IDKOLLI               PIC X(5).                                
007700     03 WS-IDKOLLI-N REDEFINES WS-IDKOLLI                                 
007800                                 PIC 9(5).                                
007900 01  ERRTEXT.                                                             
008000     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
008100     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
008200 01  KDRC-DISPLAY                PIC Z(5).                                
008300     EJECT                                                                
008400                                                                          
008500 01  GENERAL-SUBPROGRAMS.                                                 
008600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
008700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008800     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009000     EJECT                                                                
009100                                                                          
009200*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
009300                                                                          
009400 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009500 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009600 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009700     EJECT                                                                
009800                                                                          
009900*    --- AREOR FÖR KOMMUNIKATION                                          
010000 01  FILLER                  PIC X(16)   VALUE 'SEND-CONTROL'.            
010100*01  -COPY WZ01SEND                                                       
010200     EJECT                                                                
010300                                                                          
010400*    --- IN-AREOR                                                         
010500 01  INPUT-AREA                 PIC X(24)   VALUE                         
010600                                'INPUT-AREA     '.                        
010700 01  IN-AREA-HEAD.                                                        
010800*    03  -COPY WF201101                                                   
010900                                                                          
011000 01  IN-AREA-LINE.                                                        
011100*    03  -COPY WF201102                                                   
011200                                                                          
011300 01  IN-AREA-FOOT.                                                        
011400*    03  -COPY WF201103                                                   
011500                                                                          
011600 01  IN-AREA-APPX.                                                        
011700*    03  -COPY WF201104                                                   
011800     EJECT                                                                
011900                                                                          
012000*    --- UT-AREOR                                                         
012100 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
012200                                'OUTPUT-AREA     '.                       
012300     EJECT                                                                
012400                                                                          
012500*    01  -COPY WAPIINFO                                                   
012600 01  ECOM-REQ-AREA.                                                       
012700*    03  -COPY WECOMF00                                                   
012800*    03  -COPY WECOMF01                                                   
012900     EJECT                                                                
013000*       IMS SECTION                                                       
013100                                                                          
013200 01  FILLER                     PIC X(16)   VALUE 'IMS-WS'.               
013300 01  KEYS-FOR-DLI.                                                        
013400                                                                          
013500     03 W-WDGXKEY-0103-X.                                                 
013600        05  W-IDHTYP-0103       PIC X(4)    VALUE '0103'.                 
013700        05  FILLER              PIC X(26)   VALUE LOW-VALUE.              
013800     03 W-KY0104-X.                                                       
013900        05  W-ADDISPABS         PIC X(50)                                 
014000                                VALUE 'APIOUT.ECOM.NSCINVOICE'.           
014100*                                                                         
014200*              STATUS-KOD FRÅN IMS                                        
014300 01  STATUS-WS                   PIC XX.                                  
014400     88  SEGMENT-FOUND                       VALUE '  '.                  
014500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
014600     SKIP2                                                                
014700 01  GOOD-STATUSCODES.                                                    
014800     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014900     SKIP3                                                                
015000 01  SSA1                        PIC X(64).                               
015100 01  SSA2                        PIC X(96).                               
015200     EJECT                                                                
015300*            -- IMS FUNKTIONSKODER                                        
015400*01  -COPY W0003                                                          
015500     EJECT                                                                
015600*            --  DLI INPUT-OUTPUT AREA                                    
015700 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDGX0104'.               
015800 01  DLI-IO-WDGX0104.                                                     
015900*    03   -COPY WDGX0104                                                  
016000                                                                          
016100                                                                          
016200 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDR501'.                 
016300 01  DLI-IO-WDR501.                                                       
016400*    03   -COPY WDGX01                                                    
016500 LINKAGE SECTION.                                                         
016600*01  -COPY W0009   -PRE MSG-                                              
016700     EJECT                                                                
016800*01  -COPY W0009   -PRE DISTRDOC-                                         
016900     EJECT                                                                
017000*01  -COPY W0008   -PRE WDR5-                                             
017100     05  FILLER         PIC X.                                            
017200     EJECT                                                                
017300                                                                          
017400 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB WDR5-PCB.                 
017500 MAIN SECTION.                                                            
017600                                                                          
017700     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB WDR5-PCB.                 
017800                                                                          
017900     PERFORM A-INIT                                                       
018000     PERFORM B-EXECUTE                                                    
018100     PERFORM Z-FINIT                                                      
018200                                                                          
018300     MOVE ZERO TO RETURN-CODE                                             
018400     GOBACK                                                               
018500     .                                                                    
018600                                                                          
018700 A-INIT SECTION.                                                          
018800     OPEN INPUT WF2011                                                    
018900     MOVE 0    TO WS-NUM-SEG                                              
019000     MOVE 1    TO INDX                                                    
019100     .                                                                    
019200                                                                          
019300 B-EXECUTE SECTION.                                                       
019400                                                                          
019500                                                                          
019600     PERFORM S01-READ-WF2011                                              
019700                                                                          
019800     IF END-OF-WF2011                                                     
019900       CONTINUE                                                           
020000     ELSE                                                                 
020100       MOVE YES TO WF2011-FIRST-RECORD                                    
020200                                                                          
020300       PERFORM UNTIL END-OF-WF2011                                        
020400         IF WS-USER-KEY = SPACES                                          
020500           PERFORM S11-GET-API-KEY                                        
020600         END-IF                                                           
020700                                                                          
020800         IF IN-IDLEGSEL = WS-IDLEGSEL-VCCS                                
020900           IF IN-IDPTYP = WS-DOC-HEAD                                     
021000           AND IN-IDSYSTEM-SEND = 'ECOM'                                  
021100             IF FIRST-RECORD                                              
021200               MOVE NOO TO WF2011-FIRST-RECORD                            
021300             ELSE                                                         
021400               IF WZ04-SEND-IDCOM > ZERO                                  
021500                 PERFORM S90-SEND-CLOSE                                   
021600                 MOVE ZERO TO WZ04-SEND-IDCOM                             
021700               END-IF                                                     
021800             END-IF                                                       
021900             PERFORM BA-HANDLE-HEADER                                     
022000                                                                          
022100           ELSE                                                           
022200             IF IN-IDPTYP = WS-DOC-LINE                                   
022300               PERFORM BB-HANDLE-LINES                                    
022400             ELSE                                                         
022500               IF IN-IDPTYP = WS-DOC-FOOTER                               
022600                 PERFORM BC-HANDLE-FOOTER                                 
022700                 MOVE ZERO   TO WS-NUM-SEG                                
022800                 MOVE 1      TO INDX                                      
022900               END-IF                                                     
023000             END-IF                                                       
023100           END-IF                                                         
023200         END-IF                                                           
023300         PERFORM S01-READ-WF2011                                          
023400       END-PERFORM                                                        
023500       IF WZ04-SEND-IDCOM > ZERO                                          
023600         PERFORM S90-SEND-CLOSE                                           
023700       END-IF                                                             
023800     END-IF                                                               
023900     .                                                                    
024000                                                                          
024100 BA-HANDLE-HEADER SECTION.                                                
024200                                                                          
024300     MOVE 0104-IDAPI               TO IDAPI                               
024400     COMPUTE IDAPI-LEN = FUNCTION BYTE-LENGTH (                           
024500                         FUNCTION TRIM (IDAPI))                           
024600     MOVE 0104-IDPATH-API          TO IDPATH-API                          
024700     COMPUTE IDPATH-API-LEN = FUNCTION BYTE-LENGTH (                      
024800                         FUNCTION TRIM (IDPATH-API))                      
024900     MOVE 0104-IDPTYP-API          TO IDPTYP-API                          
025000     COMPUTE IDPTYP-API-LEN = FUNCTION BYTE-LENGTH (                      
025100                         FUNCTION TRIM (IDPTYP-API))                      
025200                                                                          
025300     IF WZ04-SEND-IDCOM = ZERO                                            
025400       PERFORM S90-SEND-OPEN                                              
025500       MOVE SEND-IDCOM             TO WZ04-SEND-IDCOM                     
025600     END-IF                                                               
025700                                                                          
025800     DISPLAY WAPIINFO                                                     
025900     PERFORM S90-PUT-HEADER                                               
026000                                                                          
026100**** MOVE REQUEST HDR WECOMF00                                            
026200*                                                                         
026300     MOVE IN-DATA                  TO IN-AREA-HEAD                        
026400*** REMOVE LEADING ZEROS FROM INVOICE NUMBER                              
026500     MOVE 0                        TO WS-CNT                              
026600     MOVE HEAD-IDFINDOC            TO WS-INV-ALPHA                        
026700     INSPECT WS-INV-ALPHA TALLYING WS-CNT FOR LEADING ZEROS               
026800     ADD 1                         TO WS-CNT                              
026900     MOVE WS-INV-ALPHA(WS-CNT:)    TO WS-INV-ALPHA1                       
027000     MOVE WS-INV-ALPHA1            TO ECOM-REQ-IDFINDOC                   
027100***                                                                       
027200     COMPUTE ECOM-REQ-DOCREF-LTH = FUNCTION BYTE-LENGTH (                 
027300                           FUNCTION TRIM (ECOM-REQ-IDFINDOC))             
027400     MOVE WS-USER-KEY              TO ECOM-REQ-USER-KEY                   
027500     COMPUTE ECOM-REQ-USER-KEY-LTH = FUNCTION BYTE-LENGTH (               
027600                           FUNCTION TRIM (ECOM-REQ-USER-KEY))             
028000*                                                                         
028100**** MOVE REQUEST BODY WECOMF01                                           
028200     IF HEAD-KDFINDOC = 'CR'                                              
028300       MOVE 'CREDIT'               TO ECOM-HDR-KDFINDOC                   
028400       MOVE 6                      TO ECOM-HDR-KDFINDOC-LTH               
028500     ELSE                                                                 
028600       MOVE 'INVOICE'              TO ECOM-HDR-KDFINDOC                   
028700       MOVE 7                      TO ECOM-HDR-KDFINDOC-LTH               
028800     END-IF                                                               
028900     MOVE WS-INV-ALPHA1            TO ECOM-HDR-IDFINDOC                   
029000     COMPUTE ECOM-HDR-IDFINDOC-LTH = FUNCTION BYTE-LENGTH (               
029100                           FUNCTION TRIM (ECOM-HDR-IDFINDOC))             
029200     MOVE HEAD-DAFINDOC            TO ECOM-HDR-DAFINDOC                   
029300     COMPUTE ECOM-HDR-DAFINDOC-LTH = FUNCTION BYTE-LENGTH (               
029400                           FUNCTION TRIM (ECOM-HDR-DAFINDOC))             
029500     MOVE HEAD-KDVALISO            TO ECOM-HDR-KDVALISO                   
029600     COMPUTE ECOM-HDR-KDVALISO-LTH = FUNCTION BYTE-LENGTH (               
029700                           FUNCTION TRIM (ECOM-HDR-KDVALISO))             
029800     MOVE HEAD-IDVAT-RESP          TO ECOM-HDR-IDVAT-RESP                 
029900     COMPUTE ECOM-HDR-IDVAT-RESP-LTH = FUNCTION BYTE-LENGTH (             
030000                           FUNCTION TRIM (ECOM-HDR-IDVAT-RESP))           
030100     MOVE HEAD-IDVAT-BET           TO ECOM-HDR-IDVAT-BET                  
030200     COMPUTE ECOM-HDR-IDVAT-BET-LTH = FUNCTION BYTE-LENGTH (              
030300                           FUNCTION TRIM (ECOM-HDR-IDVAT-BET))            
030400     MOVE '1441'                   TO ECOM-HDR-IDPARTNR-FROM              
030500     MOVE 4                        TO ECOM-HDR-IDPARTNR-FROM-LTH          
030600     MOVE HEAD-IDPARTNR            TO ECOM-HDR-IDPARTNR-TO                
030700     COMPUTE ECOM-HDR-IDPARTNR-TO-LTH = FUNCTION BYTE-LENGTH (            
030800                           FUNCTION TRIM (ECOM-HDR-IDPARTNR-TO))          
030900     MOVE HEAD-PRKURS              TO ECOM-HDR-PRKURS                     
031000     MOVE HEAD-IDFINDOC            TO WS-SAVE-IDFINDOC                    
031100     .                                                                    
031200                                                                          
031300 BB-HANDLE-LINES SECTION.                                                 
031400                                                                          
031500     MOVE IN-DATA                  TO IN-AREA-LINE                        
031600                                                                          
031700     IF LINE-IDFINDOC = WS-SAVE-IDFINDOC                                  
031800       MOVE LINE-IDARTNR-FINANCE     TO ECOM-LINE-IDARTNR(INDX)           
031900       COMPUTE ECOM-LINE-IDARTNR-LTH(INDX) = FUNCTION BYTE-LENGTH         
032000                     (FUNCTION TRIM (ECOM-LINE-IDARTNR(INDX)))            
032100       MOVE LINE-BEART               TO ECOM-LINE-BEART(INDX)             
032200       COMPUTE ECOM-LINE-BEART-LTH(INDX) = FUNCTION BYTE-LENGTH (         
032300                      FUNCTION TRIM (ECOM-LINE-BEART(INDX)))              
032400       MOVE LINE-IDREF               TO ECOM-LINE-IDREF(INDX)             
032500       COMPUTE ECOM-LINE-IDREF-LTH(INDX) = FUNCTION BYTE-LENGTH (         
032600                      FUNCTION TRIM (ECOM-LINE-IDREF(INDX)))              
032700       MOVE LINE-IDDC                TO ECOM-LINE-IDDC(INDX)              
032800       COMPUTE ECOM-LINE-IDDC-LTH(INDX) = FUNCTION BYTE-LENGTH (          
032900                     FUNCTION TRIM (ECOM-LINE-IDDC(INDX)))                
033000       MOVE LINE-IDEXCUST-2          TO ECOM-LINE-IDEXCUST-2(INDX)        
033100       COMPUTE ECOM-LINE-IDEXCUST-2-LTH(INDX) =                           
033200         FUNCTION BYTE-LENGTH (FUNCTION TRIM                              
033300                             (ECOM-LINE-IDEXCUST-2(INDX)))                
033400       MOVE LINE-KVLEVART            TO ECOM-LINE-KVLEVART(INDX)          
033500       MOVE 'PIECES'                 TO ECOM-LINE-UOM (INDX)              
033600       MOVE 6                        TO ECOM-LINE-UOM-LTH(INDX)           
033700       MOVE LINE-PRARTBTO            TO ECOM-LINE-PRARTBTO(INDX)          
033800       MOVE LINE-PRARTNTO            TO ECOM-LINE-PRARTNTO(INDX)          
033900       MOVE LINE-SUNTO               TO ECOM-LINE-SUNTO(INDX)             
034000       IF ECOM-HDR-KDFINDOC = 'CREDIT'                                    
034100         MOVE 1                      TO                                   
034200                              ECOM-LINE-KDANMORS-NUM(INDX)                
034300         MOVE LINE-KDANMORS          TO ECOM-LINE-KDANMORS(INDX)          
034400         COMPUTE ECOM-LINE-KDANMORS-LTH(INDX) =                           
034500                  FUNCTION BYTE-LENGTH (FUNCTION TRIM                     
034600                                (ECOM-LINE-KDANMORS(INDX)))               
034700         MOVE 1                      TO                                   
034800                              ECOM-LINE-DAFAKREF-NUM(INDX)                
034900         MOVE LINE-DAFAKREF          TO ECOM-LINE-DAFAKREF(INDX)          
035000         COMPUTE ECOM-LINE-DAFAKREF-LTH(INDX) =                           
035100                  FUNCTION BYTE-LENGTH (FUNCTION TRIM                     
035200                                (ECOM-LINE-DAFAKREF(INDX)))               
035300         MOVE 1                      TO                                   
035400                              ECOM-LINE-IDFAKREF-NUM(INDX)                
035500*** REMOVE LEADING ZEROS FROM INVOICE NUMBER                              
035600         MOVE LINE-IDFAKREF          TO WS-INV-ALPHA                      
035700         MOVE 0                      TO WS-CNT                            
035800         INSPECT WS-INV-ALPHA TALLYING WS-CNT FOR LEADING ZEROS           
035900         ADD 1                       TO WS-CNT                            
036000         MOVE WS-INV-ALPHA(WS-CNT:)  TO WS-INV-ALPHA1                     
036100         MOVE WS-INV-ALPHA1          TO ECOM-LINE-IDFAKREF(INDX)          
036200***                                                                       
036300         COMPUTE ECOM-LINE-IDFAKREF-LTH(INDX) =                           
036400                  FUNCTION BYTE-LENGTH (FUNCTION TRIM                     
036500                                (ECOM-LINE-IDFAKREF(INDX)))               
036600*** PARCEL IDENTIFIER (DISTR + CUST + ORDER + CASENO=0)                   
036700         MOVE LINE-IDEXCUST-1          TO WS-IDDISTR                      
036800         MOVE LINE-IDEXCUST-2          TO WS-IDKUNDNR                     
036900         MOVE LINE-IDREF               TO WS-IDORDER                      
037000         MOVE ZERO                     TO WS-IDKOLLI                      
037100         IF WS-IDDISTR  = SPACE                                           
037200           MOVE ZERO              TO WS-IDDISTR-N                         
037300         ELSE                                                             
037400           COMPUTE WS-IDDISTR-N  = FUNCTION NUMVAL(WS-IDDISTR)            
037500         END-IF                                                           
037600         IF WS-IDKUNDNR = SPACE                                           
037700           MOVE ZERO              TO WS-IDKUNDNR-N                        
037800         ELSE                                                             
037900           COMPUTE WS-IDKUNDNR-N = FUNCTION NUMVAL(WS-IDKUNDNR)           
038000         END-IF                                                           
038100         IF WS-IDORDER  = SPACE                                           
038200           MOVE ZERO              TO WS-IDORDER-N                         
038300         ELSE                                                             
038400           COMPUTE WS-IDORDER-N  = FUNCTION NUMVAL(WS-IDORDER)            
038500         END-IF                                                           
038600         COMPUTE WS-IDKOLLI-N  = FUNCTION NUMVAL(WS-IDKOLLI)              
038700         MOVE WS-PARCEL-ID           TO ECOM-LINE-PARCEL-ID(INDX)         
038800         COMPUTE ECOM-LINE-PARCEL-ID-LTH(INDX) =                          
038900                     FUNCTION BYTE-LENGTH (FUNCTION TRIM                  
039000                                     (ECOM-LINE-PARCEL-ID(INDX)))         
039100       ELSE                                                               
039200         MOVE 0                      TO                                   
039300                              ECOM-LINE-KDANMORS-NUM(INDX)                
039400                              ECOM-LINE-DAFAKREF-NUM(INDX)                
039500                              ECOM-LINE-IDFAKREF-NUM(INDX)                
039600         MOVE SPACES                 TO                                   
039700                              ECOM-LINE-KDANMORS(INDX)                    
039800                              ECOM-LINE-DAFAKREF(INDX)                    
039900                              ECOM-LINE-IDFAKREF(INDX)                    
040000*** PARCEL IDENTIFIER (DISTR + CUST + ORDER + CASENO)                     
040100         MOVE LINE-IDEXCUST-1          TO WS-IDDISTR                      
040200         MOVE LINE-IDEXCUST-2          TO WS-IDKUNDNR                     
040300         MOVE LINE-IDREF               TO WS-IDORDER                      
040400         MOVE LINE-IDOPTION-2          TO WS-IDKOLLI                      
040500         IF WS-IDDISTR  = SPACE                                           
040600           MOVE ZERO              TO WS-IDDISTR-N                         
040700         ELSE                                                             
040800           COMPUTE WS-IDDISTR-N  = FUNCTION NUMVAL(WS-IDDISTR)            
040900         END-IF                                                           
041000         IF WS-IDKUNDNR = SPACE                                           
041100           MOVE ZERO                 TO WS-IDKUNDNR-N                     
041200         ELSE                                                             
041300           COMPUTE WS-IDKUNDNR-N = FUNCTION NUMVAL(WS-IDKUNDNR)           
041400         END-IF                                                           
041500         IF WS-IDORDER  = SPACE                                           
041600           MOVE ZERO                 TO WS-IDORDER-N                      
041700         ELSE                                                             
041800           COMPUTE WS-IDORDER-N  = FUNCTION NUMVAL(WS-IDORDER)            
041900         END-IF                                                           
042000         IF WS-IDKOLLI = SPACE                                            
042100           MOVE ZERO                 TO WS-IDKOLLI-N                      
042200         ELSE                                                             
042300           COMPUTE WS-IDKOLLI-N  = FUNCTION NUMVAL(WS-IDKOLLI)            
042400         END-IF                                                           
042500         MOVE WS-PARCEL-ID           TO ECOM-LINE-PARCEL-ID(INDX)         
042600         COMPUTE ECOM-LINE-PARCEL-ID-LTH(INDX) =                          
042700                         FUNCTION BYTE-LENGTH (FUNCTION TRIM              
042800                                   (ECOM-LINE-PARCEL-ID(INDX)))           
042900***                                                                       
043000       END-IF                                                             
043100       MOVE LINE-REARTRAB            TO ECOM-LINE-REARTRAB(INDX)          
043200       MOVE LINE-REVAT               TO ECOM-LINE-REVAT(INDX)             
043300       MOVE LINE-BEVOLREF            TO ECOM-LINE-BEVOLREF(INDX)          
043400       COMPUTE ECOM-LINE-BEVOLREF-LTH(INDX) =                             
043500                  FUNCTION BYTE-LENGTH (FUNCTION TRIM                     
043600                                (ECOM-LINE-BEVOLREF(INDX)))               
043700       COMPUTE WS-NUM-SEG = WS-NUM-SEG + 1                                
043800       COMPUTE INDX  = INDX + 1                                           
043900     END-IF                                                               
044000     .                                                                    
044100                                                                          
044200 BC-HANDLE-FOOTER SECTION.                                                
044300                                                                          
044400*** NUMBER OF LINES IN THE INVOICE                                        
044500     MOVE WS-NUM-SEG                 TO ECOM-LINE-ITEMS-NUM               
044600                                                                          
044700     MOVE IN-DATA                    TO IN-AREA-FOOT                      
044800     IF FOOT-IDFINDOC = WS-SAVE-IDFINDOC                                  
044900       MOVE FOOT-SUNTO-TOT           TO ECOM-FOOT-SUNTO-TOT               
045000       MOVE FOOT-SUBTO-TOT           TO ECOM-FOOT-SUBTO-TOT               
045100       MOVE FOOT-SUVAT-BILLIT-TOT    TO ECOM-FOOT-SUVAT-BILLIT-TOT        
045200                                                                          
045300       PERFORM S90-PUT-ECOM-REQ                                           
045400     END-IF                                                               
045500     .                                                                    
045600                                                                          
045700 Z-FINIT SECTION.                                                         
045800     CLOSE WF2011                                                         
045900     .                                                                    
046000                                                                          
046100 S01-READ-WF2011  SECTION.                                                
046200     READ WF2011                                                          
046300       AT END                                                             
046400         SET END-OF-WF2011 TO TRUE                                        
046500     END-READ                                                             
046600     .                                                                    
046700 S11-GET-API-KEY  SECTION.                                                
046800                                                                          
046900     IF   IN-IDLEGSEL = WS-IDLEGSEL-VCCS                                  
047000     AND  IN-IDSYSTEM-SEND = 'ECOM'                                       
047100       PERFORM IMS-GU-WDGX0104                                            
047200       IF SEGMENT-FOUND                                                   
047400         MOVE 0104-IDUSERKEY   TO WS-USER-KEY                             
047500       ELSE                                                               
047600         DISPLAY 'INVALID API USER KEY'                                   
047700         CALL FELLOG                                                      
047800       END-IF                                                             
047900     END-IF                                                               
048000     .                                                                    
048100 S90-SEND-OPEN SECTION.                                                   
048200     MOVE 'OPEN'                          TO SEND-KDFUNC                  
048300     MOVE W-ADDISPABS                     TO SEND-ADDISPABS               
048400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
048500                         SEND-OPEN-AREA                                   
048600     IF SEND-KDRC > ZERO                                                  
048700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
048800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
048900       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
049000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
049100     END-IF                                                               
049200     .                                                                    
049300                                                                          
049400 S90-PUT-HEADER SECTION.                                                  
049500     MOVE 'PUT'                           TO SEND-KDFUNC                  
049600     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
049700     MOVE LENGTH OF WAPIINFO              TO SEND-KVDLEN                  
049800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
049900                         SEND-KVDLEN                                      
050000                         WAPIINFO                                         
050100     IF SEND-KDRC > ZERO                                                  
050200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
050300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
050400       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
050500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
050600     END-IF                                                               
050700     .                                                                    
050800                                                                          
050900 S90-PUT-ECOM-REQ SECTION.                                                
051000     MOVE 'PUT'                           TO SEND-KDFUNC                  
051100     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
051200     MOVE LENGTH OF ECOM-REQ-AREA         TO SEND-KVDLEN                  
051300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
051400                         SEND-KVDLEN                                      
051500                         ECOM-REQ-AREA                                    
051600     IF SEND-KDRC > ZERO                                                  
051700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
051800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
051900       DELIMITED BY SIZE INTO ERRTEXT-STR                                 
052000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
052100     END-IF                                                               
052200     .                                                                    
052300                                                                          
052400 S90-SEND-CLOSE SECTION.                                                  
052500     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
052600     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
052700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
052800     .                                                                    
052900                                                                          
053000 IMS-GU-WDGX0104  SECTION.                                                
053100                                                                          
053200     STRING 'WDR501  (WDGXKEY  =' W-WDGXKEY-0103-X ')'                    
053300          DELIMITED BY SIZE INTO SSA1                                     
053400     STRING 'WDGX0104(ADDISPAB =' W-KY0104-X ')'                          
053500          DELIMITED BY SIZE INTO SSA2                                     
053600     MOVE '  GE' TO GOOD-STATUSCODES                                      
053700     CALL CBLTDLI USING GU WDR5-PCB DLI-IO-WDGX0104 SSA1 SSA2             
053800     MOVE WDR5-STATUS-CODE TO STATUS-WS                                   
053900     PERFORM IMS-STATUSCHECK                                              
054000     .                                                                    
054100     EJECT                                                                
054200 IMS-STATUSCHECK    SECTION.                                              
054300                                                                          
054400     SET STATUS-IX TO 1                                                   
054500     SEARCH GOOD-STATUS                                                   
054600       AT END                                                             
054700         STRING 'INVALID STATUS FROM IMS:' STATUS-WS                      
054800         DELIMITED BY SIZE INTO ERRTEXT                                   
054900         CALL FELLOG                                                      
055000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
055100         CONTINUE                                                         
055200     END-SEARCH                                                           
055300     .                                                                    
055400     SKIP2                                                                
