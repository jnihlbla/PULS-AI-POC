000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W4797400.                                                 
000400 AUTHOR.        SVANTE BJÖRKBERG.                                         
000500 DATE-WRITTEN.  AUG 1987.                                                 
000600                                                                          
000700*REMARKS.                                                                 
000800*                                                                         
000900*    FUNKTION:                                                            
001000*                                                                         
001100*        PROGRAMMET KOMPLETTERAR FILEN FÖR PACKUNDERLAG, HISTORIK.        
001200*                                                                         
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016    - OM RETURKOD FRÅN SORT                                 
001600     SKIP2                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000*                                                                         
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*--- INFIL:                                                               
002400*                                                                         
002500     SELECT W47973                       ASSIGN TO W47974D1.              
002600     SKIP2                                                                
002700*--- UTFIL:                                                               
002800*                                                                         
002900     SELECT W47974                       ASSIGN TO W47974D2.              
003000     SELECT W47975                       ASSIGN TO W47974D3.              
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W47973                                                               
003700     LABEL RECORD    STANDARD                                             
003800     RECORDING       V                                                    
003900     BLOCK CONTAINS 0.                                                    
004000     SKIP2                                                                
004100*01  IN-POST -COPY W479A01     -L.                                        
004200     SKIP3                                                                
004300 FD  W47974                                                               
004400     LABEL RECORD    STANDARD                                             
004500     RECORDING       V                                                    
004600     BLOCK CONTAINS 0.                                                    
004700     SKIP2                                                                
004800*01  UT-W47974 -COPY W479A01    -L.                                       
004900     EJECT                                                                
005000 FD  W47975                                                               
005100     LABEL RECORD    STANDARD                                             
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS 0.                                                    
005400     SKIP2                                                                
005500*01  UT-W47975 -COPY W4797401   -L.                                       
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800*    -- CHECKED BY WY2000                                                 
005900     SKIP3                                                                
006000*                                                                         
006100 77  PROGRAM-NAMN                PIC X(6) VALUE 'W47974'.                 
006200     SKIP3                                                                
006300*   ----  KONSTANTER                                                      
006400                                                                          
006500 77  JA                          PIC X       VALUE 'J'.                   
006600 77  NEJ                         PIC X       VALUE 'N'.                   
006700*                                      GENERERAT PROGRAM-NAMN.            
006800     SKIP3                                                                
006900 01  RETURKODER.                                                          
007000*                                                                         
007100     03  RKOD                    PIC S9(4)   COMP SYNC VALUE ZERO.        
007200     03  RKOD-16                 PIC S9(4)   COMP SYNC VALUE +16.         
007300     SKIP3                                                                
007400 01  DYNAMISKA-SUBPROGRAM.                                                
007500*                                                                         
007600     03  W4797410                PIC X(8)    VALUE 'W4739210'.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007900     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
008000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
008100     SKIP3                                                                
008200 01  END-OF-FILE-SWITCHAR.                                                
008300*                                                                         
008400     03  INFIL-EOF               PIC X(1)    VALUE 'N'.                   
008500     EJECT                                                                
008600                                                                          
008700 01  FILLER                      PIC X(24)   VALUE                        
008800                                            'WDATAREA-START  '.           
008900*01  -COPY WDATAREA                                                       
009000     EJECT                                                                
009100                                                                          
009200 01  FILLER                      PIC X(24)   VALUE                        
009300                                            'POSTSUM-START   '.           
009400                                                                          
009500*01  -COPY W0005        -PRE POSTSUM-                                     
009600     EJECT                                                                
009700                                                                          
009800 01  FILLER                      PIC X(24)   VALUE                        
009900                                            'EMB-AREA-START  '.           
010000 01  IN-AREA                     PIC X(500).                              
010100*01  AREA -COPY W479A01    -RED IN-AREA -PRE INA01-.                      
010200     EJECT                                                                
010300                                                                          
010400*01  AREA -COPY W479A11    -RED IN-AREA -PRE INA11-.                      
010500     EJECT                                                                
010600                                                                          
010700*01  AREA -COPY W4797402   -RED IN-AREA -PRE UTA03-.                      
010800     EJECT                                                                
010900                                                                          
011000 01  FILLER                      PIC X(24)   VALUE                        
011100                                            'KAT-AREA-START  '.           
011200*01  AREA -COPY W4797401                -PRE UTKAT-.                      
011300     EJECT                                                                
011400* * * * * * * * * * * * * * * * * * * *                                   
011500*                                     *                                   
011600*  ARBETS-AREOR FÖR IMS-SEKTIONERNA   *                                   
011700*                                     *                                   
011800* * * * * * * * * * * * * * * * * * * *                                   
011900 01  FILLER                 PIC X(24) VALUE 'IMS-WS-START'.               
012000                                                                          
012100*   ----  NYCKLAR OCH SÖKFÄLT TILL DLI                                    
012200                                                                          
012300 01  W-IDGMT-X.                                                           
012400   03  W-IDDISTR            PIC S9(5)   COMP-3.                           
012500   03  W-IDKUNDNR           PIC S9(7)   COMP-3.                           
012600                                                                          
012700 01  W-WDB101KY-X.                                                        
012800   03  W-WDB1-IDPARTNR      PIC  X(9)  VALUE SPACE.                       
012900   03  W-WDB1-IDFTG         PIC  9(2)  VALUE ZERO.                        
013000                                                                          
013100 01  W-WDE4ESEQ-X.                                                        
013200    03 W-IDPRODNR-ESEQ   PIC S9(7) VALUE ZERO COMP-3.                     
013310*                                                                         
013320 01     W-IDDC-B6-X.                                                      
013330    03  W-IDDC-B6                      PIC X(2).                          
013340*                                                                         
013400 01  STATUS-WS              PIC X(2).                                     
013500   88  SEGMENT-FINNS                    VALUE '  '.                       
013600   88  SEGMENT-SAKNAS                   VALUE 'GE'.                       
013700     SKIP3                                                                
013800                                                                          
013900 01  GODK-STATUSKODER.                                                    
014000   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
014100     SKIP3                                                                
014200                                                                          
014300 01  SSA1                   PIC X(64).                                    
014400 01  SSA2                   PIC X(64).                                    
014500     EJECT                                                                
014600                                                                          
014700*01  -COPY W0003                                                          
014800     EJECT                                                                
014900                                                                          
015000 01  FILLER                 PIC X(24) VALUE 'IMS-WS-START'.               
015100 01  DLI-IO-AREA.                                                         
015200*    03  -COPY WDB201                                                     
015300     EJECT                                                                
015400 01  FILLER                      PIC X(16)  VALUE 'WDB2-AREA'.            
015500*01  -COPY WDB101                                                         
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)  VALUE 'WDE4-AREA'.            
015800 01  DLI-IO-WDE401.                                                       
015900*03  -COPY WDE401                                                         
016000     EJECT                                                                
016010     EJECT                                                                
016020 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
016030 01   DLI-IO-AREA-B601.                                                   
016040*     03  -COPY WDB601                                                    
016050                                                                          
016100                                                                          
016200 LINKAGE SECTION.                                                         
016300                                                                          
016400*01  -COPY W0008                 -PRE GMTA-                               
016500     05  FILLER            PIC X.                                         
016600     SKIP2                                                                
016700*01  -COPY W0008                 -PRE BETC-                               
016800     05  FILLER            PIC X.                                         
016900     EJECT                                                                
017000*01  -COPY W0008                 -PRE WDE4-                               
017100     05  FILLER            PIC X.                                         
017200     EJECT                                                                
017210*01  -COPY W0008                 -PRE WDB6-                               
017220     05  FILLER            PIC X.                                         
017230     EJECT                                                                
017300                                                                          
017400 PROCEDURE DIVISION  USING GMTA-PCB BETC-PCB WDE4-PCB WDB6-PCB.           
017500 MAIN SECTION.                                                            
017600     ENTRY 'DLITCBL' USING GMTA-PCB BETC-PCB WDE4-PCB WDB6-PCB.           
017700                                                                          
017800     PERFORM A-INIT                                                       
017900                                                                          
018000     PERFORM B-LAS-INFIL                                                  
018100                                                                          
018200     PERFORM UNTIL INFIL-EOF = JA                                         
018300       IF     INA01-IDPTYP = 'A01'                                        
018400         PERFORM C-KOMPLETTERA-A01                                        
018500         PERFORM D-SKRIV-W47975                                           
018600       END-IF                                                             
018700                                                                          
018800       PERFORM F-SKRIV-W47974                                             
018900                                                                          
019000       IF INA01-IDPTYP = 'A01'                                            
019100         PERFORM G-SKRIV-W47974-A03                                       
019200       END-IF                                                             
019300                                                                          
019400       PERFORM B-LAS-INFIL                                                
019500     END-PERFORM                                                          
019600                                                                          
019700     PERFORM Z-FINIT                                                      
019800     MOVE ZERO TO RETURN-CODE                                             
019900     GOBACK.                                                              
020000     EJECT                                                                
020100                                                                          
020200 A-INIT         SECTION.                                                  
020300     OPEN INPUT  W47973                                                   
020400     OPEN OUTPUT W47974 W47975                                            
020500                                                                          
020600     MOVE 'IDAG  '          TO DAT-KDDATFORM                              
020700                                                                          
020800     CALL WDATKONV USING DAT-KDDATFORM                                    
020900                         DAT-I-TIDATUM                                    
021000                         DAT-O-TIDATUM                                    
021100                         DAT-KDSVAR                                       
021200                                                                          
021300     MOVE DAT-TIAAMMDD      TO UTKAT-TIFAKT                               
021400     .                                                                    
021500     SKIP2                                                                
021600 B-LAS-INFIL    SECTION.                                                  
021700                                                                          
021800     READ W47973 INTO IN-AREA                                             
021900          AT END MOVE JA TO INFIL-EOF                                     
022000     END-READ                                                             
022100                                                                          
022200     IF INFIL-EOF = NEJ                                                   
022300       MOVE 'W47973'        TO POSTSUM-FDNAMN                             
022400       MOVE 'W47974D1'      TO POSTSUM-DDNAMN2                            
022500       MOVE INA01-IDPTYP    TO POSTSUM-TRANSTYP                           
022600       CALL POSTSUM USING POSTSUM-PARM                                    
022700     END-IF.                                                              
022800     EJECT                                                                
022900                                                                          
023000 C-KOMPLETTERA-A01   SECTION.                                             
023100                                                                          
023200     IF INA01-BEGMT = SPACE    OR                                         
023300        INA01-ADGMT = SPACE                                               
023400       PERFORM CA-LAS-WDB201                                              
023500                                                                          
023600       IF SEGMENT-FINNS                                                   
023700         IF GMT-ADGMT = SPACE   AND                                       
023800            GMT-BEGMT = SPACE                                             
023801                                                                          
023802           MOVE INA01-IDDC            TO W-IDDC-B6                        
023810           PERFORM IMS-GU-WDB601                                          
023900                                                                          
024000           MOVE GMT-IDPARTNR     TO W-WDB1-IDPARTNR                       
024100           MOVE DCS-IDFTG        TO W-WDB1-IDFTG                          
024200           PERFORM IMS-GU-BETC-WDB101                                     
024300           IF INA01-BEGMT = SPACE                                         
024400             MOVE BET-BEBETRAD-1 TO INA01-BEGMT-RAD1                      
024500             MOVE BET-BEBETRAD-2 TO INA01-BEGMT-RAD2                      
024600           END-IF                                                         
024700                                                                          
024800           IF INA01-ADGMT = SPACE                                         
024900             MOVE BET-ADBETRAD-1 TO INA01-ADGMT-GATA                      
025000             MOVE BET-ADBETRAD-2 TO INA01-ADGMT-PADR                      
025100             MOVE BET-BELAND-SVE TO INA01-ADGMT-LAND                      
025200           END-IF                                                         
025300         ELSE                                                             
025400                                                                          
025500           IF INA01-BEGMT = SPACE                                         
025600             MOVE GMT-BEGMT      TO INA01-BEGMT                           
025700           END-IF                                                         
025800                                                                          
025900           IF INA01-ADGMT = SPACE                                         
026000             MOVE GMT-ADGMT      TO INA01-ADGMT                           
026100           END-IF                                                         
026200         END-IF                                                           
026300       END-IF                                                             
026400     END-IF                                                               
026500     IF INA01-IDUSER = SPACE OR = '00000000'                              
026600       MOVE INA01-IDPRODNR       TO W-IDPRODNR-ESEQ                       
026700       PERFORM IMS-GU-WDE401-ESEQ                                         
026800       IF SEGMENT-FINNS                                                   
026900         MOVE KORD-IDUSER        TO INA01-IDUSER                          
027000         IF INA01-IDUSER = '00000000'                                     
027100           MOVE SPACE            TO INA01-IDUSER                          
027200         END-IF                                                           
027300       END-IF                                                             
027400     END-IF                                                               
027500     .                                                                    
027600 CA-LAS-WDB201  SECTION.                                                  
027700                                                                          
027800     MOVE INA01-IDDISTR          TO W-IDDISTR                             
027900     MOVE INA01-IDKUNDNR         TO W-IDKUNDNR                            
028000     PERFORM IMS-GU-GMTA-WDB201                                           
028100     .                                                                    
028200     SKIP2                                                                
028300 D-SKRIV-W47975 SECTION.                                                  
028400                                                                          
028500     MOVE INA01-IDDISTR   TO UTKAT-IDDISTR                                
028600     MOVE INA01-IDKUNDNR  TO UTKAT-IDKUNDNR                               
028700     MOVE INA01-IDDC      TO UTKAT-IDDC                                   
028800     MOVE INA01-IDKUNDRF  TO UTKAT-IDKUNDRF                               
028900     MOVE 'WDL111'        TO UTKAT-IDSEGM                                 
029000                                                                          
029100     WRITE UT-W47975 FROM UTKAT-AREA                                      
029200                                                                          
029300     MOVE 'W47975'        TO POSTSUM-FDNAMN                               
029400     MOVE 'W47974D3'      TO POSTSUM-DDNAMN2                              
029500     MOVE SPACE           TO POSTSUM-TRANSTYP                             
029600     CALL POSTSUM USING POSTSUM-PARM.                                     
029700     SKIP2                                                                
029800                                                                          
029900 F-SKRIV-W47974 SECTION.                                                  
030000                                                                          
030100     WRITE UT-W47974 FROM INA01-AREA                                      
030200                                                                          
030300     MOVE 'W47974'        TO POSTSUM-FDNAMN                               
030400     MOVE 'W47974D2'      TO POSTSUM-DDNAMN2                              
030500     MOVE INA01-IDPTYP    TO POSTSUM-TRANSTYP                             
030600     CALL POSTSUM USING POSTSUM-PARM.                                     
030700     SKIP2                                                                
030800                                                                          
030900 G-SKRIV-W47974-A03 SECTION.                                              
031000                                                                          
031100     MOVE 'A03'           TO UTA03-IDPTYP                                 
031200     MOVE UTKAT-TIFAKT    TO UTA03-TIFAKT                                 
031300                                                                          
031400     WRITE UT-W47974 FROM UTA03-AREA                                      
031500                                                                          
031600     MOVE 'W47974'        TO POSTSUM-FDNAMN                               
031700     MOVE 'W47974D2'      TO POSTSUM-DDNAMN2                              
031800     MOVE 'A03'           TO POSTSUM-TRANSTYP                             
031900     CALL POSTSUM USING POSTSUM-PARM.                                     
032000     SKIP2                                                                
032100                                                                          
032200 Z-FINIT        SECTION.                                                  
032300                                                                          
032400     CLOSE W47973                                                         
032500           W47974                                                         
032600           W47975                                                         
032700                                                                          
032800     MOVE 'S'             TO POSTSUM-OPKOD                                
032900     CALL POSTSUM USING POSTSUM-PARM.                                     
033000     SKIP2                                                                
033100                                                                          
033200 IMS-GU-BETC-WDB101  SECTION.                                             
033300                                                                          
033400     STRING 'WLBETC01(WDB101KY= ' W-WDB101KY-X ')'                        
033500            DELIMITED BY SIZE INTO SSA1                                   
033600     MOVE '  GE' TO GODK-STATUSKODER                                      
033700     CALL CBLTDLI USING GU BETC-PCB BET-WDB101 SSA1                       
033800     MOVE BETC-STATUS-CODE TO STATUS-WS                                   
033900     PERFORM IMS-STATUSKONTROLL.                                          
034000     SKIP3                                                                
034100 IMS-GU-GMTA-WDB201  SECTION.                                             
034200                                                                          
034300     STRING 'WLGMTA01(IDGMT   = ' W-IDGMT-X ')'                           
034400            DELIMITED BY SIZE INTO SSA1                                   
034500     MOVE '  GE' TO GODK-STATUSKODER                                      
034600     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA SSA1                      
034700     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
034800     PERFORM IMS-STATUSKONTROLL.                                          
034900     SKIP3                                                                
035000 IMS-GU-WDE401-ESEQ SECTION.                                              
035100*    DISPLAY 'E401-ESEQ'                                                  
035200     STRING 'WDE401  (WDE4ESEQ =' W-WDE4ESEQ-X ')'                        
035300          DELIMITED BY SIZE INTO SSA1                                     
035400     MOVE '  GE' TO GODK-STATUSKODER                                      
035500     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE401 SSA1                    
035600     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
035700     PERFORM IMS-STATUSKONTROLL                                           
035800     .                                                                    
035900     SKIP3                                                                
035910                                                                          
035920 IMS-GU-WDB601    SECTION.                                                
035930     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
035940          DELIMITED BY SIZE INTO SSA1                                     
035950     MOVE '  '   TO GODK-STATUSKODER                                      
035960     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
035970     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
035980     PERFORM IMS-STATUSKONTROLL                                           
035990     .                                                                    
036000 IMS-STATUSKONTROLL  SECTION.                                             
036100                                                                          
036200     SET STATUS-IX TO 1                                                   
036300     SEARCH GODK-STATUS                                                   
036400       AT END CALL FELLOG                                                 
036500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS NEXT SENTENCE             
036600     END-SEARCH.                                                          
