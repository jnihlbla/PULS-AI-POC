000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4286000.                                                
000400 AUTHOR.         LARS CALAIS.                                             
000500 DATE-WRITTEN.   95/10/30.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNCTION:                                                            
000900*        READS DISCREPANCY FILE DEPENDING ON SYSIN DATA VIA SOP           
001000*        FROM MPP W40703.                                                 
001100*        OUTPUT FILE IS PRINTED BY E+-PGM ON LIST TO:                     
001200*        INVENTORY DEPT OR                                                
001300*        PACKING   DEPT.                                                  
001400*                                                                         
001500*        THE PROGRAM READS     WLKREE (VIA KREI)                          
001600*                                                                         
001700*    ABENDCODES:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200                                                                          
002300 ENVIRONMENT DIVISION.                                                    
002400                                                                          
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800                                                                          
002900*          --- SYSIN FROM JCL                                             
003000                                                                          
003100     SELECT INDATA                     ASSIGN TO SYSIN.                   
003200                                                                          
003300*          --- FILE TO INVENTORY LIST                                     
003400     SELECT W42861                     ASSIGN TO W42860D1.                
003500                                                                          
003600*          --- FILE TO PACKING LIST                                       
003700     SELECT W42863                     ASSIGN TO W42860D2.                
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000                                                                          
004100 FILE SECTION.                                                            
004200                                                                          
004300 FD INDATA                                                                
004400     LABEL RECORD STANDARD                                                
004500     RECORDING  F                                                         
004600     BLOCK CONTAINS 0.                                                    
004700                                                                          
004800 01  INPOST                  PIC X(80).                                   
004900                                                                          
005000 FD  W42861                                                               
005100     RECORDING       V                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  POST -COPY W42861A -PRE  INVA-  -L.                                  
005500*01  POST -COPY W42861B -PRE  INVB-  -L.                                  
005600                                                                          
005700 FD  W42863                                                               
005800     RECORDING       V                                                    
005900     BLOCK CONTAINS  0.                                                   
006000                                                                          
006100*01  POST -COPY W42863A -PRE  FORA-  -L.                                  
006200*01  POST -COPY W42863B -PRE  FORB-  -L.                                  
006300*01  POST -COPY W42863C -PRE  FORC-  -L.                                  
006400     EJECT                                                                
006500 WORKING-STORAGE SECTION.                                                 
006600                                                                          
006601*    -COPY WY2000W6                                                       
006602     SKIP3                                                                
006603*    -COPY WY2000W1                                                       
006604     SKIP3                                                                
006700 77  IDPGM                       PIC X(8)    VALUE 'W4286000'.            
006800 77  YES                         PIC X       VALUE 'Y'.                   
006900 77  NOO                         PIC X       VALUE 'N'.                   
007000 77  WS-TIAAMMDD-FOM             PIC S9(7)              COMP-3.           
007100 77  WS-TIAAMMDD-TOM             PIC S9(7)              COMP-3.           
007200 77  WS-IDARTNR-FOM              PIC S9(9)              COMP-3.           
007300 77  WS-IDARTNR-TOM              PIC S9(9)              COMP-3.           
007400 77  WS-IDDISTR-FOM              PIC S9(5)              COMP-3.           
007500 77  WS-IDDISTR-TOM              PIC S9(5)              COMP-3.           
007600 77  WS-IDKUNDNR-FOM             PIC S9(7)              COMP-3.           
007610 77  WS-IDKUNDNR-TOM             PIC S9(7)              COMP-3.           
007700 77  WS-TILEVANM                 PIC S9(7)              COMP-3.           
007800 77  WS-KDANMORS-FOM             PIC X(2).                                
007900 77  WS-KDANMORS-TOM             PIC X(2).                                
008000 77  IX                          PIC S9(2)             COMP SYNC.         
008100 77  EOF                         PIC X.                                   
008200                                                                          
008300     EJECT                                                                
008400 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
008500 01  WS-TIAAPP-TODAY             PIC 9(4).                                
008600 01  FILLER REDEFINES WS-TIAAPP-TODAY.                                    
008700     03  AA-TODAY                PIC 9(2).                                
008800     03  PP-TODAY                PIC 9(2).                                
008900                                                                          
009000 01  REF-TIAAMMDD                PIC 9(6)    VALUE ZERO.                  
009100 01  FILLER REDEFINES REF-TIAAMMDD.                                       
009200     03  REF-AAR                 PIC 9(2).                                
009300     03  REF-MAANAD              PIC 9(2).                                
009400     03  REF-DAG                 PIC 9(2).                                
009500     EJECT                                                                
009600 01  GENERAL-SUBPROGRAM.                                                  
009700*                                                                         
009800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
010200     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010300     EJECT                                                                
010400 01  FILLER                  PIC X(10)   VALUE 'INAREA'.                  
010500 01  INAREA                  PIC X(80).                                   
010600 01    IN-IDARTNR            PIC X(9).                                    
010700 01    IN-IDDC               PIC X(2).                                    
010800 01    IN-IDDISTR            PIC X(4).                                    
010900 01    IN-IDKUNDNR           PIC X(6).                                    
011000 01    IN-KDANMORS           PIC X(2).                                    
011100 01    IN-TIAAPP-TOM         PIC X(4).                                    
011200 01    IN-IDLISTTYP          PIC X(3).                                    
011300 01    IN-IDFTG              PIC X(2).                                    
011400 01    IN-TIAAMMDD-FOM       PIC X(6).                                    
011500 01    IN-TIAAMMDD-TOM       PIC X(6).                                    
011600*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
011700                                                                          
011800 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
011900 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012000                                                                          
012100 01  ERRTEXT.                                                             
012200     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
012300     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
012400     EJECT                                                                
012500*    --- PARAMETRAR TILL POSTSUM                                          
012600*                                                                         
012700*01  -COPY W0005   -PRE  POSTSUM-                                         
012800     EJECT                                                                
012900 01  POST-SUM-START              PIC X(24)   VALUE                        
013000                                 'POST-SUM-START  '.                      
013100                                                                          
013200                                                                          
013300*01  -COPY WDATAREA                                                       
013400     EJECT                                                                
013500 01  INVA-AREA-START             PIC X(24)   VALUE                        
013600                                 'INVA-AREA-START  '.                     
013700*01  AREA -COPY W42861A     -PRE INA-                                     
013800     EJECT                                                                
013900 01  INVB-AREA-START             PIC X(24)   VALUE                        
014000                                 'INVB-AREA-START  '.                     
014100*01  AREA -COPY W42861B     -PRE INB-                                     
014200     EJECT                                                                
014300 01  FORA-AREA-START             PIC X(24)   VALUE                        
014400                                 'FORA-AREA-START  '.                     
014500*01  AREA -COPY W42863A     -PRE PAA-                                     
014600     EJECT                                                                
014700 01  FORB-AREA-START             PIC X(24)   VALUE                        
014800                                 'FORB-AREA-START  '.                     
014900*01  AREA -COPY W42863B     -PRE PAB-                                     
015000     EJECT                                                                
015100 01  FORC-AREA-START             PIC X(24)   VALUE                        
015200                                 'FORC-AREA-START  '.                     
015300*01  AREA -COPY W42863C     -PRE PAC-                                     
015400     EJECT                                                                
015500*    --- AREAS FOR IMS-SECTIONS                                           
015600*                                                                         
015700     EJECT                                                                
015800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015900                                                                          
016000 01  KEYS-TILL-DLI.                                                       
016100     03  W-WDA2DSEQ-X.                                                    
016200         05  W-IDFTG             PIC  9(2).                               
016300         05  W-IDARTNR-A2        PIC S9(9)                 COMP-3.        
016400                                                                          
016500     03  W-IDSKYLT-X.                                                     
016600         05  W-IDSKYLT           PIC X(3)    VALUE 'GB '.                 
016700                                                                          
016800     03  W-IDARTNR-X.                                                     
016900         05  W-IDARTNR           PIC S9(9)                 COMP-3.        
017000                                                                          
017100                                                                          
017200*    --- STATUS-KOD FRÅN IMS                                              
017300 01  STATUS-WS                   PIC XX.                                  
017400     88  SEGMENT-FOUND                       VALUE '  '.                  
017500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
017600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
017700     88  SEGMENT-END                         VALUE 'GB'.                  
017800                                                                          
017900 01  GOOD-STATUSCODES.                                                    
018000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018100                                                                          
018200 01  SSA1                        PIC X(64).                               
018300 01  SSA2                        PIC X(64).                               
018400     EJECT                                                                
018500*    --- IMS FUNCTION CODES                                               
018600*01  -COPY W0003                                                          
018700     EJECT                                                                
018800*    ---  DLI INPUT-OUTPUT AREA                                           
018900 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
019000                                                                          
019100 01  DLI-IO-AREA.                                                         
019200                                                                          
019300     03  WLKREE11.                                                        
019400*        05  -COPY WDA211                                                 
019500                                                                          
019600     03  WLKREE01.                                                        
019700*        05  -COPY WDA201                                                 
019800     EJECT                                                                
019900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA-2'.        
020000                                                                          
020100 01  DLI-IO-AREA-2.                                                       
020200     03  IO-AREA-2               PIC X(150)  VALUE SPACE.                 
020300                                                                          
020400     03  WLBENA11 REDEFINES IO-AREA-2.                                    
020500*        05  -COPY WDD311                                                 
020600     EJECT                                                                
020700                                                                          
020800 LINKAGE SECTION.                                                         
020900                                                                          
021000     EJECT                                                                
021100*01  -COPY W0008  -PRE KREE-                                              
021200     05  FILLER                  PIC X.                                   
021300     EJECT                                                                
021400*01  -COPY W0008  -PRE BENA-                                              
021500     05  FILLER                  PIC X.                                   
021600     EJECT                                                                
021700 PROCEDURE DIVISION  USING KREE-PCB                                       
021800                           BENA-PCB.                                      
021900 MAIN SECTION.                                                            
022000     ENTRY 'DLITCBL' USING KREE-PCB                                       
022100                           BENA-PCB.                                      
022200                                                                          
022300                                                                          
022400     PERFORM A-INIT                                                       
022500     PERFORM B-TAKE-CARE-OF-INPUT                                         
022600                                                                          
022700     EVALUATE IN-IDLISTTYP                                                
022800     WHEN 'INV'                                                           
022900       PERFORM C-GET-INV-DATA                                             
023000     WHEN 'FOR'                                                           
023100       PERFORM D-GET-PACK-DATA                                            
023200       PERFORM S63A-WRITE-W42863A                                         
023300     END-EVALUATE                                                         
023400                                                                          
023500     PERFORM Z-FINIT                                                      
023600                                                                          
023700     MOVE ZERO TO RETURN-CODE                                             
023800     GOBACK                                                               
023900     .                                                                    
024000     EJECT                                                                
024100 A-INIT SECTION.                                                          
024200                                                                          
024300     OPEN OUTPUT W42861                                                   
024400                 W42863                                                   
024500                                                                          
024600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
024700                                                                          
024800     ACCEPT REF-TIAAMMDD  FROM DATE                                       
024900     ACCEPT TODAYS-DATE   FROM DATE                                       
025000***  SET LIMIT FOR SELECTING DATA, ONE YEAR (12 PERIODS) BACK.            
025020     IF REF-AAR = 00                                                      
025030       MOVE 99                  TO REF-AAR                                
025040     ELSE                                                                 
025100       COMPUTE REF-AAR = REF-AAR - 1                                      
025110     END-IF                                                               
025200                                                                          
025300     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
025400     MOVE TODAYS-DATE           TO DAT-I-TIDATUM                          
025500                                                                          
025600     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
025700                     DAT-O-TIDATUM DAT-KDSVAR                             
025800                                                                          
025900     IF DAT-KDSVAR-OK                                                     
026000       MOVE DAT-TIAAPP          TO WS-TIAAPP-TODAY                        
026100     ELSE                                                                 
026200       MOVE 'FEL I DATUMKONVERTERINGEN AV DAGENS-DATUM'                   
026300                                TO ERRTEXT-STR                            
026400       DISPLAY ERRTEXT                                                    
026500       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
026600     END-IF                                                               
026700     .                                                                    
026800     EJECT                                                                
026900 B-TAKE-CARE-OF-INPUT           SECTION.                                  
027000                                                                          
027100     OPEN INPUT INDATA                                                    
027200     READ INDATA NEXT RECORD INTO INAREA                                  
027300       AT END MOVE YES          TO EOF                                    
027400     END-READ                                                             
027500     CLOSE INDATA                                                         
027600                                                                          
027610     DISPLAY 'INAREA: ' INAREA                                            
027700     UNSTRING INAREA DELIMITED BY SPACE INTO IN-IDARTNR                   
027800                                             IN-IDDC                      
027900                                             IN-IDDISTR                   
028000                                             IN-IDKUNDNR                  
028100                                             IN-KDANMORS                  
028200                                             IN-TIAAPP-TOM                
028300                                             IN-IDLISTTYP                 
028400                                             IN-IDFTG                     
028500                                             IN-TIAAMMDD-FOM              
028600                                             IN-TIAAMMDD-TOM              
028700                                                                          
028900     DISPLAY 'IDARTNR      = '               IN-IDARTNR                   
029000     DISPLAY 'IDDC         = '               IN-IDDC                      
029100     DISPLAY 'IDDISTR      = '               IN-IDDISTR                   
029200     DISPLAY 'IDKUNDNR     = '               IN-IDKUNDNR                  
029300     DISPLAY 'KDANMORS     = '               IN-KDANMORS                  
029400     DISPLAY 'TIAAPP-TOM   = '               IN-TIAAPP-TOM                
029500     DISPLAY 'IDLISTTYP    = '               IN-IDLISTTYP                 
029600     DISPLAY 'IDFTG        = '               IN-IDFTG                     
029700     DISPLAY 'TIAAMMDD-FOM = '               IN-TIAAMMDD-FOM              
029800     DISPLAY 'TIAAMMDD-TOM = '               IN-TIAAMMDD-TOM              
029900                                                                          
030000     MOVE IN-IDARTNR            TO W-IDARTNR                              
030100                                   W-IDARTNR-A2                           
030200                                   WS-IDARTNR-FOM                         
030300                                   WS-IDARTNR-TOM                         
030400                                                                          
030500     MOVE IN-IDFTG              TO W-IDFTG                                
030600                                                                          
030800     PERFORM BB-TIAAPP-TOM-CONTR                                          
030900     PERFORM BD-IDKUNDNR-CONTR                                            
031000     PERFORM BC-IDDISTR-CONTR                                             
031100     PERFORM BF-KDANMORS-CONTR                                            
031200     .                                                                    
031300     EJECT                                                                
033600 BB-TIAAPP-TOM-CONTR            SECTION.                                  
033700                                                                          
033800     IF IN-TIAAPP-TOM NOT = ALL '+'                                       
033801       MOVE IN-TIAAPP-TOM   TO TMP1-YYPP                                  
033802       MOVE 9913            TO TMP2-YYPP                                  
033803       PERFORM WY2000P6                                                   
033810       IF TMP1-YYPP < TMP2-YYPP                                           
033900         MOVE 'AAPP  '          TO DAT-KDDATFORM                          
034000         MOVE IN-TIAAPP-TOM     TO DAT-I-TIDATUM                          
034100                                                                          
034200         CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                  
034300                         DAT-O-TIDATUM DAT-KDSVAR                         
034400                                                                          
034500         IF DAT-KDSVAR-OK                                                 
034600           MOVE DAT-TIAAMMDD    TO WS-TIAAMMDD-TOM                        
034700         ELSE                                                             
034800           MOVE 'FEL I DATUMKONVERTERINGEN AV TOM-DATUM'                  
034900                                  TO ERRTEXT-STR                          
035000           DISPLAY ERRTEXT                                                
035100           CALL ABEND USING RKOD-ABEND-NO-DUMP                            
035200         END-IF                                                           
035300       ELSE                                                               
035400         MOVE 999999            TO WS-TIAAMMDD-TOM                        
035500       END-IF                                                             
035510     END-IF                                                               
035600     .                                                                    
035700     EJECT                                                                
035800 BC-IDDISTR-CONTR               SECTION.                                  
035900                                                                          
036000     IF IN-IDDISTR > ZERO                                                 
036100       MOVE IN-IDDISTR          TO WS-IDDISTR-FOM                         
036200                                   WS-IDDISTR-TOM                         
036300     ELSE                                                                 
036400       MOVE ZERO                TO WS-IDDISTR-FOM                         
036500       MOVE +99999              TO WS-IDDISTR-TOM                         
036600     END-IF                                                               
036700     .                                                                    
036800     EJECT                                                                
036900 BD-IDKUNDNR-CONTR              SECTION.                                  
037000                                                                          
037100     IF IN-IDKUNDNR > ZERO                                                
037200       MOVE IN-IDKUNDNR         TO WS-IDKUNDNR-FOM                        
037300                                   WS-IDKUNDNR-TOM                        
037400     ELSE                                                                 
037500       MOVE ZERO                TO WS-IDKUNDNR-FOM                        
037600       MOVE +9999999            TO WS-IDKUNDNR-TOM                        
037700     END-IF                                                               
037800     .                                                                    
037900     EJECT                                                                
038000 BF-KDANMORS-CONTR              SECTION.                                  
038100                                                                          
038200     IF IN-KDANMORS NUMERIC                                               
038300       MOVE IN-KDANMORS         TO WS-KDANMORS-FOM                        
038400                                   WS-KDANMORS-TOM                        
038500     ELSE                                                                 
038600       IF IN-IDLISTTYP = 'FOR'                                            
038700         MOVE '42'              TO WS-KDANMORS-FOM                        
038800         MOVE '43'              TO WS-KDANMORS-TOM                        
038900       ELSE                                                               
039000         MOVE '00'              TO WS-KDANMORS-FOM                        
039100         MOVE '99'              TO WS-KDANMORS-TOM                        
039200       END-IF                                                             
039300     END-IF                                                               
039400     .                                                                    
039500     EJECT                                                                
039600 C-GET-INV-DATA                 SECTION.                                  
039700                                                                          
039800     PERFORM IMS-GET-BENA                                                 
039900                                                                          
040000     PERFORM CA-CREATE-61A-RECORD                                         
040100                                                                          
040200     PERFORM IMS-GET-KREE                                                 
040210     MOVE IN-TIAAMMDD-FOM             TO WS-TIAAMMDD-FOM                  
040220     MOVE IN-TIAAMMDD-TOM             TO WS-TIAAMMDD-TOM                  
040300     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                         
040301       MOVE LEV-DALEVANM (3:6) TO TMP1-YYMMDD                             
040302       MOVE WS-TIAAMMDD-FOM    TO TMP2-YYMMDD                             
040303       MOVE WS-TIAAMMDD-TOM    TO TMP3-YYMMDD                             
040304       PERFORM WY2000Q1                                                   
040400       IF TMP1-YYMMDD >= TMP2-YYMMDD AND                                  
040410          TMP1-YYMMDD <= TMP3-YYMMDD                                      
040500         IF LEV-IDDC = IN-IDDC                                            
040600           IF LEV-KDANMORS >= WS-KDANMORS-FOM                             
040700             IF LEV-KDANMORS <= WS-KDANMORS-TOM                           
041000               IF ANM-IDDISTR >= WS-IDDISTR-FOM                           
041100                 IF ANM-IDDISTR <= WS-IDDISTR-TOM                         
041200                   IF ANM-IDKUNDNR >= WS-IDKUNDNR-FOM                     
041300                     IF ANM-IDKUNDNR <= WS-IDKUNDNR-TOM                   
041400                       PERFORM CB-CREATE-61B-RECORD                       
041500                     END-IF                                               
041600                   END-IF                                                 
041700                 END-IF                                                   
041800               END-IF                                                     
042100             END-IF                                                       
042200           END-IF                                                         
042300         END-IF                                                           
042400       END-IF                                                             
042500       PERFORM IMS-GET-KREE                                               
042600     END-PERFORM                                                          
042700     .                                                                    
042800     EJECT                                                                
042900 CA-CREATE-61A-RECORD           SECTION.                                  
043000                                                                          
043100     MOVE '61A'                 TO INA-SORT-IDPTYP                        
043200     MOVE ZERO                  TO INA-SORT-TIKNOTA                       
043300                                   INA-SORT-TIRETANK                      
043400                                   INA-SORT-TILEVANM                      
043500     MOVE W-IDARTNR             TO INA-IDARTNR                            
043600     MOVE IN-IDDC               TO INA-IDDC                               
043700     IF IN-KDANMORS NUMERIC                                               
043800       MOVE IN-KDANMORS         TO INA-SORT-KDANMORS                      
043900     END-IF                                                               
044000                                                                          
044100     IF IN-IDDISTR > ZERO                                                 
044200       MOVE IN-IDDISTR          TO INA-IDDISTR                            
044300       MOVE IN-IDKUNDNR         TO INA-IDKUNDNR                           
044400     ELSE                                                                 
044500       MOVE ZERO                TO INA-IDDISTR                            
044600                                   INA-IDKUNDNR                           
044700     END-IF                                                               
044800                                                                          
044900     MOVE REF-TIAAMMDD          TO INA-TIAAMMDD-FOM                       
045000     MOVE TODAYS-DATE           TO INA-TIAAMMDD-TOM                       
045100     MOVE TEXT-BEART            TO INA-BEART                              
045200     PERFORM S61A-WRITE-W42861A                                           
045300     .                                                                    
045400     EJECT                                                                
045500 CB-CREATE-61B-RECORD           SECTION.                                  
045600                                                                          
045700     MOVE '61B'                 TO INB-SORT-IDPTYP                        
045800     MOVE LEV-KDANMORS          TO INB-SORT-KDANMORS                      
045900     MOVE LEV-TIKNOTA           TO INB-SORT-TIKNOTA                       
046000     MOVE LEV-DALEVANM (3:6)    TO INB-SORT-TILEVANM                      
046100     MOVE LEV-FLDIRLEV          TO INB-FLDIRLEV                           
046200     MOVE LEV-IDFAKT            TO INB-IDFAKT                             
046300     MOVE LEV-IDKNOTNR          TO INB-IDKNOTNR                           
046400     MOVE LEV-KVLEVANM          TO INB-KVLEVANM                           
046500     MOVE LEV-PRARTBTO          TO INB-PRARTBTO                           
046600     MOVE LEV-TIFAKT            TO INB-TIFAKT                             
046700     MOVE LEV-TIINLINL          TO INB-TIINLINL                           
046800                                                                          
046900     IF LEV-IDLOPNRM > ZERO                                               
047000       MOVE ANM-DARETANK (3:6)  TO INB-SORT-TIRETANK                      
047100     ELSE                                                                 
047200       MOVE ZERO                TO INB-SORT-TIRETANK                      
047300     END-IF                                                               
047400     MOVE ANM-IDDISTR           TO INB-IDDISTR                            
047500     MOVE ANM-IDKUNDNR          TO INB-IDKUNDNR                           
047600     MOVE ANM-IDRAPPNR          TO INB-IDRAPPNR                           
047700     PERFORM S61B-WRITE-W42861B                                           
047800     .                                                                    
047900     EJECT                                                                
048000 D-GET-PACK-DATA                SECTION.                                  
048100                                                                          
048200     PERFORM IMS-GET-BENA                                                 
048300                                                                          
048400     PERFORM DA-CREATE-63A-RECORD                                         
048500                                                                          
048600     PERFORM IMS-GET-KREE                                                 
048700     PERFORM UNTIL SEGMENT-MISSING OR SEGMENT-END                         
048701       MOVE LEV-DALEVANM(3:6)   TO TMP1-YYMMDD                            
048702       MOVE REF-TIAAMMDD        TO TMP2-YYMMDD                            
048703       PERFORM WY2000P1                                                   
048800       IF TMP1-YYMMDD >= TMP2-YYMMDD                                      
048900         IF LEV-IDDC = IN-IDDC                                            
049000           IF LEV-KDANMORS >= WS-KDANMORS-FOM                             
049100             IF LEV-KDANMORS <= WS-KDANMORS-TOM                           
049200*              IF LEV-TILEVANM >= WS-TIAAMMDD-FOM                         
049300*                IF LEV-TILEVANM <= WS-TIAAMMDD-TOM                       
049400                   IF ANM-IDDISTR >= WS-IDDISTR-FOM                       
049500                     IF ANM-IDDISTR <= WS-IDDISTR-TOM                     
049600                       IF ANM-IDKUNDNR >= WS-IDKUNDNR-FOM                 
049700                         IF ANM-IDKUNDNR <= WS-IDKUNDNR-TOM               
049800                           PERFORM DB-CREATE-63B-RECORD                   
049900                           PERFORM DC-CREATE-63C-RECORD                   
050000                         END-IF                                           
050100                       END-IF                                             
050200                     END-IF                                               
050300                   END-IF                                                 
050400*                END-IF                                                   
050500*              END-IF                                                     
050600             END-IF                                                       
050700           END-IF                                                         
050800         END-IF                                                           
050900       END-IF                                                             
051000       PERFORM IMS-GET-KREE                                               
051100     END-PERFORM                                                          
051200     .                                                                    
051300     EJECT                                                                
051400 DA-CREATE-63A-RECORD           SECTION.                                  
051500                                                                          
051600     MOVE '63A'                 TO PAA-SORT-IDPTYP                        
051700     MOVE W-IDARTNR             TO PAA-SORT-IDARTNR                       
051800     MOVE ZERO                  TO PAA-SORT-TIAAPP                        
051900     MOVE IN-IDDC               TO PAA-IDDC                               
052000     MOVE TEXT-BEART            TO PAA-BEART                              
052100                                                                          
052200     IF IN-IDDISTR > ZERO                                                 
052300       MOVE IN-IDDISTR          TO PAA-SORT-IDDISTR                       
052400       MOVE IN-IDKUNDNR         TO PAA-SORT-IDKUNDNR                      
052500     ELSE                                                                 
052600       MOVE ZERO                TO PAA-SORT-IDDISTR                       
052700                                   PAA-SORT-IDKUNDNR                      
052800     END-IF                                                               
052900                                                                          
052910     IF AA-TODAY = 00                                                     
052920       MOVE 99       TO AA-TODAY                                          
052930     ELSE                                                                 
053000       COMPUTE AA-TODAY = AA-TODAY - 1                                    
053010     END-IF                                                               
053100     COMPUTE PP-TODAY = PP-TODAY + 1                                      
053120     IF PP-TODAY > +12                                                    
053300       ADD +1                   TO AA-TODAY                               
053400       MOVE +1                  TO PP-TODAY                               
053500     END-IF                                                               
053600                                                                          
053700     MOVE +1                    TO IX                                     
053800     PERFORM UNTIL IX > +12                                               
053900       MOVE WS-TIAAPP-TODAY     TO PAA-TIAAPP       (IX)                  
054000       MOVE ZERO                TO PAA-SULEVANT-PER (IX)                  
054100       ADD +1                   TO IX                                     
054200                                   PP-TODAY                               
054220       IF PP-TODAY > +12                                                  
054400         ADD +1                 TO AA-TODAY                               
054500         MOVE +1                TO PP-TODAY                               
054600       END-IF                                                             
054700     END-PERFORM                                                          
054800     .                                                                    
054900     EJECT                                                                
055000 DB-CREATE-63B-RECORD           SECTION.                                  
055100                                                                          
055200     MOVE '63B'                 TO PAB-SORT-IDPTYP                        
055300     MOVE W-IDARTNR             TO PAB-SORT-IDARTNR                       
055400     PERFORM DBA-KONVERT-TILEVANM                                         
055500     MOVE ANM-IDDISTR           TO PAB-SORT-IDDISTR                       
055600     MOVE ANM-IDKUNDNR          TO PAB-SORT-IDKUNDNR                      
055700     MOVE ANM-IDRAPPNR          TO PAB-IDRAPPNR                           
055800     MOVE LEV-KDFRAKT           TO PAB-KDFRAKT                            
055900     MOVE LEV-KDEMBLEV          TO PAB-KDEMBLEV                           
056000     MOVE LEV-KVLEVANM          TO PAB-KVLEVANM                           
056100     MOVE LEV-PRARTBTO          TO PAB-PRARTBTO                           
056200     MOVE LEV-IDKOLLI           TO PAB-IDKOLLI                            
056300     MOVE LEV-KDFAKTYP          TO PAB-KDFAKTYP                           
056400     MOVE LEV-IDFAKT            TO PAB-IDFAKT                             
056500     MOVE LEV-TIFAKT            TO PAB-TIFAKT                             
056600     MOVE LEV-DALEVANM (3:6)    TO PAB-TILEVANM                           
056700                                                                          
056800     PERFORM S63B-WRITE-W42863B                                           
056900     PERFORM S01-UPDATE-W42863A                                           
057000     .                                                                    
057100     EJECT                                                                
057200 DBA-KONVERT-TILEVANM           SECTION.                                  
057300                                                                          
057400     MOVE 'AAMMDD'              TO DAT-KDDATFORM                          
057500     MOVE ANM-DALEVANM (3:6)    TO DAT-I-TIDATUM                          
057600                                                                          
057700     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
057800                     DAT-O-TIDATUM DAT-KDSVAR                             
057900                                                                          
058000     IF DAT-KDSVAR-OK                                                     
058100       MOVE DAT-TIAAPP          TO PAB-SORT-TIAAPP                        
058200     ELSE                                                                 
058300       MOVE 'FEL I DATUMKONVERTERINGE AV LEV-DALEVANM'                    
058400                              TO ERRTEXT-STR                              
058500       DISPLAY ERRTEXT                                                    
058600       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
058700     END-IF                                                               
058800     .                                                                    
058900     EJECT                                                                
059000 DC-CREATE-63C-RECORD           SECTION.                                  
059100                                                                          
059200     MOVE '63C'                 TO PAC-SORT-IDPTYP                        
059300     MOVE W-IDARTNR             TO PAC-SORT-IDARTNR                       
059400     MOVE ZERO                  TO PAC-SORT-TIAAPP                        
059500     MOVE ANM-IDDISTR           TO PAC-SORT-IDDISTR                       
059600     MOVE ANM-IDKUNDNR          TO PAC-SORT-IDKUNDNR                      
059700     MOVE ANM-IDRAPPNR          TO PAC-IDRAPPNR                           
059800     MOVE LEV-KDFRAKT           TO PAC-KDFRAKT                            
059900     MOVE LEV-KDEMBLEV          TO PAC-KDEMBLEV                           
060000     MOVE LEV-KVLEVANM          TO PAC-KVLEVANM                           
060100     MOVE LEV-PRARTBTO          TO PAC-PRARTBTO                           
060200     MOVE LEV-IDKOLLI           TO PAC-IDKOLLI                            
060300     MOVE LEV-KDFAKTYP          TO PAC-KDFAKTYP                           
060400     MOVE LEV-IDFAKT            TO PAC-IDFAKT                             
060500     MOVE LEV-TIFAKT            TO PAC-TIFAKT                             
060600     MOVE LEV-DALEVANM (3:6)    TO PAC-TILEVANM                           
060700                                                                          
060800     PERFORM S63C-WRITE-W42863C                                           
060900     .                                                                    
061000     EJECT                                                                
061100 Z-FINIT                        SECTION.                                  
061200     CLOSE W42861                                                         
061300           W42863                                                         
061400                                                                          
061500     MOVE 'S' TO POSTSUM-OPKOD                                            
061600     CALL POSTSUM USING POSTSUM-PARM                                      
061700     .                                                                    
061800     EJECT                                                                
061900 S01-UPDATE-W42863A             SECTION.                                  
062000                                                                          
062100     MOVE +1                    TO IX                                     
062200     PERFORM UNTIL IX > +12                                               
062300       IF DAT-TIAAPP = PAA-TIAAPP (IX)                                    
062400         ADD LEV-KVLEVANM       TO PAA-SULEVANT-PER (IX)                  
062500       END-IF                                                             
062600       ADD +1                   TO IX                                     
062700     END-PERFORM                                                          
062800     .                                                                    
062900     EJECT                                                                
063000 S61A-WRITE-W42861A             SECTION.                                  
063100                                                                          
063200     WRITE INVA-POST FROM INA-AREA                                        
063300                                                                          
063400     MOVE INA-SORT-IDPTYP       TO POSTSUM-TRANSTYP                       
063500     MOVE 'W42861'              TO POSTSUM-FDNAMN                         
063600     MOVE 'W42860D1'            TO POSTSUM-DDNAMN2                        
063700     CALL POSTSUM USING POSTSUM-PARM                                      
063800     .                                                                    
063900     EJECT                                                                
064000 S61B-WRITE-W42861B             SECTION.                                  
064100                                                                          
064200     WRITE INVB-POST FROM INB-AREA                                        
064300                                                                          
064400     MOVE INB-SORT-IDPTYP       TO POSTSUM-TRANSTYP                       
064500     MOVE 'W42861'              TO POSTSUM-FDNAMN                         
064600     MOVE 'W42860D1'            TO POSTSUM-DDNAMN2                        
064700     CALL POSTSUM USING POSTSUM-PARM                                      
064800     .                                                                    
064900     EJECT                                                                
065000 S63A-WRITE-W42863A             SECTION.                                  
065100                                                                          
065200     WRITE FORA-POST FROM PAA-AREA                                        
065300                                                                          
065400     MOVE PAA-SORT-IDPTYP       TO POSTSUM-TRANSTYP                       
065500     MOVE 'W42863'              TO POSTSUM-FDNAMN                         
065600     MOVE 'W42860D1'            TO POSTSUM-DDNAMN2                        
065700     CALL POSTSUM USING POSTSUM-PARM                                      
065800     .                                                                    
065900     EJECT                                                                
066000 S63B-WRITE-W42863B             SECTION.                                  
066100                                                                          
066200     WRITE FORB-POST FROM PAB-AREA                                        
066300                                                                          
066400     MOVE PAB-SORT-IDPTYP       TO POSTSUM-TRANSTYP                       
066500     MOVE 'W42863'              TO POSTSUM-FDNAMN                         
066600     MOVE 'W42860D1'            TO POSTSUM-DDNAMN2                        
066700     CALL POSTSUM USING POSTSUM-PARM                                      
066800     .                                                                    
066900     EJECT                                                                
067000 S63C-WRITE-W42863C             SECTION.                                  
067100                                                                          
067200     WRITE FORC-POST FROM PAC-AREA                                        
067300                                                                          
067400     MOVE PAC-SORT-IDPTYP       TO POSTSUM-TRANSTYP                       
067500     MOVE 'W42863'              TO POSTSUM-FDNAMN                         
067600     MOVE 'W42860D1'            TO POSTSUM-DDNAMN2                        
067700     CALL POSTSUM USING POSTSUM-PARM                                      
067800     .                                                                    
067900     EJECT                                                                
068000* --- IMS SECTIONS  ---                                                   
068100                                                                          
068200     EJECT                                                                
068300 IMS-GET-KREE                   SECTION.                                  
068400                                                                          
068500     STRING 'WLKREE11*D(WDA2DSEQ =' W-WDA2DSEQ-X ')'                      
068600            DELIMITED BY SIZE INTO SSA1                                   
068700     MOVE 'WLKREE01 '           TO SSA2                                   
068800     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
068900     CALL CBLTDLI USING GN KREE-PCB DLI-IO-AREA SSA1 SSA2                 
069000     MOVE KREE-STATUS-CODE TO STATUS-WS                                   
069100     PERFORM IMS-STATUSCHECK                                              
069200     .                                                                    
069300     EJECT                                                                
069400 IMS-GET-BENA                   SECTION.                                  
069500                                                                          
069600     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
069700            DELIMITED BY SIZE INTO SSA1                                   
069800     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
069900            DELIMITED BY SIZE INTO SSA2                                   
070000     MOVE '  GE' TO GOOD-STATUSCODES                                      
070100     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA-2 SSA1 SSA2               
070200     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
070300     PERFORM IMS-STATUSCHECK                                              
070400     .                                                                    
070500     EJECT                                                                
070600 IMS-STATUSCHECK                SECTION.                                  
070700                                                                          
070800     SET STATUS-IX TO 1                                                   
070900     SEARCH GOOD-STATUS                                                   
071000       AT END                                                             
071100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
071200           DELIMITED BY SIZE INTO ERRTEXT-STR                             
071300         DISPLAY ERRTEXT                                                  
071400         CALL FELLOG                                                      
071500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
071600         CONTINUE                                                         
071700     END-SEARCH                                                           
071800     .                                                                    
071810     EJECT                                                                
071900*    -COPY WY2000Q1                                                       
071910     EJECT                                                                
072000*    -COPY WY2000P1                                                       
072010     EJECT                                                                
072100*    -COPY WY2000P6                                                       
