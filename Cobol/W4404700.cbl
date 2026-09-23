000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W4404700.                                                
000400 AUTHOR.         INGER NILSSON                                            
000500     DATE-WRITTEN.   FEB   89.                                            
000600*                                                                         
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGNOS LEVERANSFÖRMÅGA RESTORDER                                
001100*                                                                         
001200*        RESTORDERRADER KOMPLETTERAS MED TIDISPIN FRÅN                    
001300*        ARTIKELREGISTRET.                                                
001400*                                                                         
001500*    INDATA.                                                              
001600*                                                                         
001700*        W44061 - SEKVENSIELL FIL WDA5                                    
001800*        WDK6                                                             
001900*                                                                         
002000*                                                                         
002100*    UTDATA.                                                              
002200*                                                                         
002300*        W44047 - FIL TILL PGM W44041                                     
002400*                                                                         
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP3                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200     SELECT W44061  ASSIGN TO W44047D1.                                   
003300     SELECT W44047  ASSIGN TO W44047D2.                                   
003400     SELECT SORTFIL ASSIGN TO W44047SD.                                   
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700                                                                          
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W44061                                                               
004100     LABEL RECORD STANDARD                                                
004200     RECORDING F                                                          
004300     BLOCK CONTAINS 0.                                                    
004400*01  W44061-POST  -COPY W44060     -L                                     
004500     EJECT                                                                
004600 FD  W44047                                                               
004700     LABEL RECORD STANDARD                                                
004800     RECORDING F                                                          
004900     BLOCK CONTAINS 0.                                                    
005000*01  W44047-POST  -COPY W440011    -L                                     
005100     EJECT                                                                
005200 SD  SORTFIL                                                              
005300     LABEL RECORD STANDARD                                                
005400     RECORDING F                                                          
005500     BLOCK CONTAINS 0.                                                    
005600*01  SORTPOST     -COPY W44060     -PRE S-                                
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900     SKIP2                                                                
006000*    -COPY WY2000W1                                                       
006100     SKIP3                                                                
006200*      ---- ARBETSVARIABLER                                               
006300 01    INFIL-SLUT                PIC X(03)   VALUE SPACE.                 
006400 01    SORTFIL-SLUT              PIC X(03)   VALUE SPACE.                 
006500                                                                          
006600 01    W-TIDISPIN                PIC S9(7)   VALUE +0  COMP-3.            
006700 01    WS-TIAAVVD.                                                        
006800    03 WS-TIAAVV                 PIC 9(04)  VALUE ZERO.                   
006900    03 FILLER                    PIC 9(01)  VALUE ZERO.                   
007000 01    W-TIAAVV                  PIC 9(04).                               
007100 01    W-DAT-TIAAVV              PIC 9(04).                               
007200 01    FILLER REDEFINES W-DAT-TIAAVV.                                     
007300    03 W-DAT-TIAA                PIC 9(2).                                
007400    03 W-DAT-TIVV                PIC 9(2).                                
007500 01    W-DAT-TIAAMMDD            PIC 9(06)   VALUE ZERO.                  
007600 77    IDDISTR-WS                PIC X(4)    VALUE SPACE.                 
007700 77    IDARTNR-WS                PIC X(9)    VALUE SPACE.                 
007800 77    KDRAPRIO-WS               PIC X(3)    VALUE SPACE.                 
007900 77    W-KDGK                    PIC S9      VALUE +0    COMP-3.          
008000 77    W-KDLTK                   PIC S9      VALUE +0    COMP-3.          
008100 77    W-KVROS                   PIC S9(7)V99 VALUE +0  COMP-3.           
008200 77    W-SPAR-IDDC               PIC X(2)     VALUE SPACE.                
008300 77    W-SPAR-IDARTNR            PIC S9(9)    VALUE +0  COMP-3.           
008400 77    W-SPAR-KDSTARAD           PIC X        VALUE SPACE.                
008500 77    W-REROFORD                PIC S9(3)V9(3) VALUE +0 COMP-3.          
008600                                                                          
008700 01    W-TILEVBSK                PIC 9(05).                               
008800 01    FILLER REDEFINES W-TILEVBSK.                                       
008900   03  FILLER                    PIC 9.                                   
009000   03  W-TILEVBSK-TIAA           PIC 9(02).                               
009100   03  W-TILEVBSK-TIVV           PIC 9(02).                               
009200     EJECT                                                                
009300*      ---- KONSTANTER                                                    
009400                                                                          
009500 77    JA                        PIC X       VALUE 'J'.                   
009600 77    NEJ                       PIC X       VALUE 'N'.                   
009700 77    W-END                     PIC X       VALUE 'N'.                   
009800 77    INDATA                    PIC X       VALUE 'J'.                   
009900     88 INDATA-OK                            VALUE 'J'.                   
010000                                                                          
010100*      ---- SUBPROGRAM OCH PARAMETER-AREOR                                
010200                                                                          
010300 01    DYNAMISKA-SUBPROGRAM.                                              
010400   03    WDATKONV               PIC X(08)    VALUE 'WDATKONV'.            
010500   03    ABEND                  PIC X(08)    VALUE 'ABEND   '.            
010600   03    FELLOG                 PIC X(08)    VALUE 'FELLOG  '.            
010700   03    POSTSUM                PIC X(08)    VALUE 'POSTSUM '.            
010800   03    CBLTDLI                PIC X(08)    VALUE 'CBLTDLI '.            
010900                                                                          
011000*      ---- PARAMETRAR TILL ABEND                                         
011100 01  RETURKODER.                                                          
011200   03  RKOD-ABEND-UTAN-DUMP     PIC S9(04) COMP SYNC VALUE +16.           
011300     EJECT                                                                
011400*      ---- PARAMETRAR TILL DATUMKORT                                     
011500                                                                          
011600*01  -COPY WDATAREA                                                       
011700     EJECT                                                                
011800*      ---- PARAMETRAR TILL POSTSUM                                       
011900                                                                          
012000*01  -COPY W0005    -PRE POSTSUM-                                         
012100     EJECT                                                                
012200 01  FILLER              PIC X(16) VALUE 'W44047'.                        
012300*01  W44047-POST  -COPY W440011    -PRE UT-                               
012400     EJECT                                                                
012500*      ---- NYCKLAR OCH SÖKFÄLT TILL DLI                                  
012600 01    NYCKLAR-TILL-DLI.                                                  
012700   03    W-IDARTNR-X.                                                     
012800     05    W-IDARTNR             PIC S9(9)  COMP-3  VALUE +0.             
012900   03    W-WDD901KY-X.                                                    
012910     05    W-IDARTNR-D9          PIC S9(9)  COMP-3  VALUE +0.             
012920     05    W-IDDC-D9             PIC X(2)   VALUE SPACE.                  
013000   03    W-IDLEVNR-X.                                                     
013100     05    W-IDLEVNR             PIC  X(5)  VALUE SPACE.                  
013200   03    W-IDLEVBSK-X.                                                    
013300     05    W-IDLEVBSK            PIC S9(1)  COMP-3  VALUE +2.             
013400   03    W-IDLEVBSK-4-X.                                                  
013500     05    W-IDLEVBSK-4          PIC S9(1)  COMP-3  VALUE +4.             
013600   03    W-IDSKYLT-X.                                                     
013700     05    W-IDSKYLT             PIC X(03)          VALUE SPACE.          
013800     EJECT                                                                
013900******************************************************************        
014000*                                                                         
014100*        ARBETS-AREOR TILL IMS-SEKTIONERNA                                
014200*                                                                         
014300 01    IMS-WS.                                                            
014400   03    FILLER                  PIC X(16)   VALUE 'IMS-WS     '.         
014500     SKIP3                                                                
014600*                        **** STATUS-KOD FRÅN IMS                         
014700   03    STATUS-WS               PIC XX.                                  
014800     88    SEGMENT-FINNS                     VALUE '  '.                  
014900     88    SEGMENT-SAKNAS                    VALUE 'GE'.                  
015000     SKIP3                                                                
015100   03    GODK-STATUSKODER.                                                
015200     05    GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
015300     SKIP3                                                                
015400 01    SSA1                      PIC X(224).                              
015500 01    SSA2                      PIC X(224).                              
015600 01    SSA3                      PIC X(224).                              
015700     EJECT                                                                
015800*                            IMS FUNKTIONSKODER                           
015900*01    -COPY W0003                                                        
016000     EJECT                                                                
016100* ---         DLI INOUT OUTPUT AREA                                       
016200* ---         DLI-IO-AREA                                                 
016300                                                                          
016400 01    FILLER                PIC X(16)   VALUE 'DLI-IO-ARTC01'.           
016500 01    DLI-IO-ARTC01.                                                     
016600*  03    WLARTC01 -COPY WDK601                                            
016700     EJECT                                                                
016800                                                                          
016900 01    FILLER                PIC X(16)   VALUE 'DLI-IO-ARTC11'.           
017000 01    DLI-IO-ARTC11.                                                     
017100*  03    WLARTC11 -COPY WDK611                                            
017200     EJECT                                                                
017300                                                                          
017400 01    FILLER                PIC X(16)   VALUE 'DLI-IO-TEXTD3'.           
017500 01    DLI-IO-TEXTD3.                                                     
017600*  03    TEXT-AREA -COPY WDD311                                           
017700     EJECT                                                                
017800                                                                          
017900 01    FILLER                PIC X(16)   VALUE 'DLI-IO-INLB25'.           
018000 01    DLI-IO-INLB25.                                                     
018100*  03    WLINLB25  -COPY WDD925                                           
018200     EJECT                                                                
018300 LINKAGE SECTION.                                                         
018400     SKIP2                                                                
018500*01    -COPY W0008     -PRE ARTC-                                         
018600     05  FILLER                  PIC X.                                   
018700     EJECT                                                                
018800*01    -COPY W0008     -PRE BENA-                                         
018900     05  FILLER                  PIC X.                                   
019000     EJECT                                                                
019100*01    -COPY W0008     -PRE INLB-                                         
019200     05  FILLER                  PIC X.                                   
019300     EJECT                                                                
019400 PROCEDURE DIVISION USING ARTC-PCB BENA-PCB INLB-PCB.                     
019500     ENTRY 'DLITCBL' USING ARTC-PCB BENA-PCB INLB-PCB.                    
019600                                                                          
019700     PERFORM A-INIT                                                       
019800                                                                          
019900     SORT SORTFIL ASCENDING KEY S-RAD-IDARTNR                             
020000                                S-RAD-IDDC                                
020100                                S-RAD-KDSTARAD                            
020200                                S-RAD-KDRAPRIO                            
020300                                                                          
020400           INPUT  PROCEDURE B-IN-BEHANDLING                               
020500           OUTPUT PROCEDURE C-UT-BEHANDLING                               
020600                                                                          
020700     IF SORT-RETURN > 0                                                   
020800        DISPLAY '*** W4404700  - FEL VID SORTERING ***'                   
020900        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
021000     ELSE                                                                 
021100        PERFORM Z-FINIT                                                   
021200        MOVE ZERO TO RETURN-CODE                                          
021300        GOBACK                                                            
021400     END-IF                                                               
021500     .                                                                    
021600     EJECT                                                                
021700 A-INIT SECTION.                                                          
021800                                                                          
021900     OPEN INPUT W44061                                                    
022000         OUTPUT W44047                                                    
022100                                                                          
022200     MOVE 'IDAG  '  TO DAT-KDDATFORM                                      
022300     CALL WDATKONV USING DAT-KDDATFORM                                    
022400                         DAT-I-TIDATUM                                    
022500                         DAT-O-TIDATUM                                    
022600                         DAT-KDSVAR                                       
022700                                                                          
022800     MOVE DAT-TIAA  TO W-DAT-TIAA                                         
022900     MOVE DAT-TIVV  TO W-DAT-TIVV                                         
023000     MOVE DAT-TIAAMMDD TO W-DAT-TIAAMMDD                                  
023100     .                                                                    
023200     EJECT                                                                
023300 B-IN-BEHANDLING SECTION.                                                 
023400                                                                          
023500     READ W44061 INTO S-RAD-W44060 AT END                                 
023600                 MOVE 'EOF' TO INFIL-SLUT                                 
023700     END-READ                                                             
023800                                                                          
023900     PERFORM UNTIL INFIL-SLUT = 'EOF'                                     
024000                                                                          
024100        IF S-RAD-KDSTARAD = '1' OR '2'                                    
024200***  NEDASTÅENDE TEST PÅ TPOTYP ÄR INLAGD FÖR ATT UNDVIKA                 
024300***  ATT FÅ JÄTTELISTOR UNDER P23-UPPBYGGNADEN. DEN SKALL                 
024400***  TAS BORT NÄR P23 ÄR INTRODUCERAD. (MAGGAN 1998-05-04)                
024500          IF S-RAD-KDTPOTYP NOT = 2                                       
024600           RELEASE S-SORTPOST                                             
024700          END-IF                                                          
024800        END-IF                                                            
024900                                                                          
025000        READ W44061 INTO S-RAD-W44060 AT END                              
025100                    MOVE 'EOF' TO INFIL-SLUT                              
025200        END-READ                                                          
025300     END-PERFORM                                                          
025400     .                                                                    
025500     EJECT                                                                
025600 C-UT-BEHANDLING SECTION.                                                 
025700                                                                          
025800     RETURN SORTFIL AT END                                                
025900       MOVE 'EOS' TO SORTFIL-SLUT                                         
026000     END-RETURN                                                           
026100                                                                          
026200     PERFORM UNTIL SORTFIL-SLUT = 'EOS'                                   
026300       MOVE S-RAD-IDARTNR  TO W-SPAR-IDARTNR                              
026400       PERFORM UNTIL SORTFIL-SLUT      = 'EOS'       OR                   
026500                     S-RAD-IDARTNR NOT = W-SPAR-IDARTNR                   
026600         MOVE S-RAD-IDDC     TO W-SPAR-IDDC                               
026700         MOVE +0 TO UT-TILEVBSK UT-REROFORD                               
026800         MOVE SPACE TO UT-TELEVBSK-EXT                                    
026900                       UT-TELEVBSK-EXT2                                   
027000         PERFORM UNTIL  SORTFIL-SLUT       = 'EOS'            OR          
027100                   NOT (S-RAD-IDARTNR      = W-SPAR-IDARTNR   AND         
027200                        S-RAD-IDDC         = W-SPAR-IDDC)     OR          
027300                        S-RAD-KDSTARAD NOT = '1'                          
027400           PERFORM CD-SKRIV-UTFIL                                         
027500           RETURN SORTFIL AT END                                          
027600             MOVE 'EOS' TO SORTFIL-SLUT                                   
027700           END-RETURN                                                     
027800         END-PERFORM                                                      
027900         IF S-RAD-KDSTARAD = '2'                                          
028000           PERFORM CA-LAES-IDLEVNR                                        
028100           PERFORM CB-LAES-TIDISPIN                                       
028200           MOVE    W-TILEVBSK TO UT-TILEVBSK                              
028300           PERFORM CC-LAES-LEVBESKED-TEXT                                 
028400           PERFORM UNTIL SORTFIL-SLUT       = 'EOS'            OR         
028500                    NOT (S-RAD-IDARTNR      = W-SPAR-IDARTNR   AND        
028600                         S-RAD-IDDC         = W-SPAR-IDDC)     OR         
028700                         S-RAD-KDSTARAD NOT = '2'                         
028800             PERFORM CD-SKRIV-UTFIL                                       
028900             RETURN SORTFIL AT END                                        
029000               MOVE 'EOS' TO SORTFIL-SLUT                                 
029100             END-RETURN                                                   
029200           END-PERFORM                                                    
029300         END-IF                                                           
029400                                                                          
029500       END-PERFORM                                                        
029600     END-PERFORM                                                          
029700     .                                                                    
029800     EJECT                                                                
029900 CA-LAES-IDLEVNR SECTION.                                                 
030000                                                                          
030100     MOVE    S-RAD-IDARTNR  TO W-IDARTNR                                  
030200     PERFORM IMS-GU-ARTC01                                                
030300     .                                                                    
030400     EJECT                                                                
030500 CB-LAES-TIDISPIN  SECTION.                                               
030600                                                                          
030700     MOVE    S-RAD-IDARTNR    TO W-IDARTNR                                
030800     PERFORM IMS-GNP-ARTC11                                               
030900     IF SEGMENT-FINNS                                                     
031000       MOVE CLAG-TIDISPIN     TO W-TIDISPIN                               
031100       MOVE W-TIDISPIN        TO DAT-I-TIDATUM                            
031200       MOVE 'AAMMDD'          TO DAT-KDDATFORM                            
031300       CALL WDATKONV USING       DAT-KDDATFORM                            
031400                                 DAT-I-TIDATUM                            
031500                                 DAT-O-TIDATUM                            
031600                                 DAT-KDSVAR                               
031700       IF DAT-KDSVAR-OK                                                   
031800         MOVE DAT-TIAAVVD     TO WS-TIAAVVD                               
031900       ELSE                                                               
032000         MOVE ZERO            TO WS-TIAAVVD                               
032100       END-IF                                                             
032200       MOVE WS-TIAAVV         TO W-TILEVBSK                               
032300     ELSE                                                                 
032400       MOVE +0                TO W-TIDISPIN                               
032500     END-IF                                                               
032600     .                                                                    
032700     EJECT                                                                
032800 CC-LAES-LEVBESKED-TEXT SECTION.                                          
032900                                                                          
033000     MOVE S-RAD-IDARTNR     TO W-IDARTNR-D9                               
033010     MOVE S-RAD-IDDC        TO W-IDDC-D9                                  
033100     IF S-RAD-IDLEVNR NOT = SPACE                                         
033200       MOVE S-RAD-IDLEVNR   TO W-IDLEVNR                                  
033300     ELSE                                                                 
033400       MOVE ART-IDLEVNR     TO W-IDLEVNR                                  
033500     END-IF                                                               
033600     PERFORM IMS-GU-INLB25                                                
033700     IF SEGMENT-FINNS                                                     
033800       MOVE W-DAT-TIAAMMDD   TO TMP1-YYMMDD                               
033900       MOVE INFO-TIBORT      TO TMP2-YYMMDD                               
034000       PERFORM WY2000P1                                                   
034100       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
034200         MOVE SPACE         TO UT-TELEVBSK-EXT                            
034300                               UT-TELEVBSK-EXT2                           
034400       ELSE                                                               
034500         MOVE INFO-TELEVBSK TO UT-TELEVBSK-EXT                            
034600         PERFORM IMS-GN-INLB25-EXT2                                       
034700         IF SEGMENT-FINNS                                                 
034800            MOVE INFO-TELEVBSK TO UT-TELEVBSK-EXT2                        
034900         ELSE                                                             
035000            MOVE SPACE         TO UT-TELEVBSK-EXT2                        
035100         END-IF                                                           
035200       END-IF                                                             
035300     END-IF                                                               
035400     .                                                                    
035500     EJECT                                                                
035600 CD-SKRIV-UTFIL SECTION.                                                  
035700                                                                          
035800     MOVE S-RAD-IDDISTR          TO UT-IDDISTR                            
035900     MOVE S-RAD-IDKUNDNR         TO UT-IDKUNDNR                           
036000     MOVE S-RAD-IDKUNDRF         TO UT-IDKUNDRF                           
036100     MOVE S-RAD-IDARTNR          TO UT-IDARTNR                            
036200                                     W-IDARTNR                            
036300     MOVE S-RAD-IDDC             TO UT-IDDC                               
036400     MOVE S-RAD-KDFRAKT          TO UT-KDFRAKT                            
036500     MOVE S-RAD-KDORDKL          TO UT-KDORDKL                            
036600     MOVE S-RAD-KDSTARAD         TO UT-KDSTARAD                           
036700     MOVE S-RAD-KDRAPRIO         TO UT-KDRAPRIO                           
036800     MOVE S-RAD-KVART            TO UT-KVART                              
036900***** FIX FÖR ATT HÄMTA DEALERNET PRIS TL 040712                          
037000     IF  S-RAD-PRAVCOST > 0                                               
037001       MOVE S-RAD-PRAVCOST         TO UT-PRARTNTO                         
037002     ELSE                                                                 
037010       IF S-RAD-PRARTNTO > 0                                              
037100         MOVE S-RAD-PRARTNTO       TO UT-PRARTNTO                         
037200       ELSE                                                               
037300         MOVE S-RAD-PRARTNTO-LOC   TO UT-PRARTNTO                         
037400         IF S-RAD-PRARTNTO-LOC = 0                                        
037500           MOVE S-RAD-PRARTNTO-LOCPREL TO UT-PRARTNTO                     
037600         END-IF                                                           
037700       END-IF                                                             
037710     END-IF                                                               
037800******                                                                    
037900     MOVE S-RAD-DARODAT (3:6)    TO UT-TIRODAT                            
038000     MOVE S-RAD-TITPO            TO UT-TITPO                              
038100     MOVE +0                     TO UT-REROFORD                           
038200     IF S-RAD-IDDISTR > +1090                                             
038300        MOVE 'GB '               TO W-IDSKYLT                             
038400     ELSE                                                                 
038500        MOVE 'S  '               TO W-IDSKYLT                             
038600     END-IF                                                               
038700     PERFORM IMS-GU-BENA-TEXT                                             
038800     MOVE TEXT-BEART             TO UT-BEART                              
038900     WRITE W44047-POST FROM UT-W44047-POST                                
039000                                                                          
039100     MOVE 'W44047'               TO POSTSUM-FDNAMN                        
039200     MOVE 'W44047D2'             TO POSTSUM-DDNAMN2                       
039300     CALL POSTSUM USING POSTSUM-PARM                                      
039400     .                                                                    
039500     EJECT                                                                
039600 Z-FINIT SECTION.                                                         
039700                                                                          
039800     CLOSE W44061 W44047                                                  
039900     .                                                                    
040000     EJECT                                                                
040100* IMS SEKTIONER                                                           
040200                                                                          
040300 IMS-GU-ARTC01 SECTION.                                                   
040400                                                                          
040500     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
040600            DELIMITED BY SIZE INTO SSA1                                   
040700     MOVE '  ' TO GODK-STATUSKODER                                        
040800     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-ARTC01 SSA1                    
040900     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
041000     PERFORM IMS-STATUSKONTROLL                                           
041100     .                                                                    
041200     EJECT                                                                
041300 IMS-GNP-ARTC11 SECTION.                                                  
041400                                                                          
041500     MOVE 'WLARTC11 '           TO SSA1                                   
041600     MOVE '  GE' TO GODK-STATUSKODER                                      
041700     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-ARTC11 SSA1                   
041800     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
041900     PERFORM IMS-STATUSKONTROLL                                           
042000     .                                                                    
042100     EJECT                                                                
042200 IMS-GU-BENA-TEXT SECTION.                                                
042300                                                                          
042400     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
042500            DELIMITED BY SIZE INTO SSA1                                   
042600     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
042700            DELIMITED BY SIZE INTO SSA2                                   
042800     MOVE '    ' TO GODK-STATUSKODER                                      
042900     CALL CBLTDLI USING GU BENA-PCB DLI-IO-TEXTD3 SSA1 SSA2               
043000     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
043100     PERFORM IMS-STATUSKONTROLL                                           
043200     .                                                                    
043300     EJECT                                                                
043400 IMS-GU-INLB25    SECTION.                                                
043500                                                                          
043600     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
043700            DELIMITED BY SIZE INTO SSA1                                   
043800     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
043900            DELIMITED BY SIZE INTO SSA2                                   
044000     STRING 'WLINLB25(IDLEVBSK =' W-IDLEVBSK-X ')'                        
044100            DELIMITED BY SIZE INTO SSA3                                   
044200     MOVE '  GE' TO GODK-STATUSKODER                                      
044300     CALL CBLTDLI USING GU INLB-PCB DLI-IO-INLB25 SSA1 SSA2 SSA3          
044400     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
044500     PERFORM IMS-STATUSKONTROLL                                           
044600     .                                                                    
044700     EJECT                                                                
044800 IMS-GN-INLB25-EXT2 SECTION.                                              
044900                                                                          
045000     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
045100            DELIMITED BY SIZE INTO SSA1                                   
045200     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
045300            DELIMITED BY SIZE INTO SSA2                                   
045400     STRING 'WLINLB25(IDLEVBSK =' W-IDLEVBSK-4-X ')'                      
045500            DELIMITED BY SIZE INTO SSA3                                   
045600     MOVE '  GE' TO GODK-STATUSKODER                                      
045700     CALL CBLTDLI USING GN INLB-PCB DLI-IO-INLB25 SSA1 SSA2 SSA3          
045800     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
045900     PERFORM IMS-STATUSKONTROLL                                           
046000     .                                                                    
046100     EJECT                                                                
046200 IMS-STATUSKONTROLL SECTION.                                              
046300                                                                          
046400     SET STATUS-IX TO 1                                                   
046500     SEARCH GODK-STATUS AT END CALL FELLOG                                
046600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
046700     END-SEARCH                                                           
046800     .                                                                    
046900     EJECT                                                                
047000*    -COPY WY2000P1                                                       
