000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6126400.                                                
000300 AUTHOR.         MONICA BERGSTRÖM                                         
000400 DATE-WRITTEN.   97/06/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER WD06-DATABASEN VECKOVIS FÖR ATT SAMLA SDC-HISTORIK         
000900*        OM ARTIKLAR TILL USA/CAN MED VISS IDDISTR.                       
001000*        OM ARTIKLAR TILL USA/CAN MED VISS IDDISTR.                       
001100*                                                                         
001200*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
001300*                                                                         
001400* ÄNDRING 97-12-02     JOHAN L                                            
001500*         LAGT IN TEST FÖR R32 DÅ DET ÄR FÖRLORADE KOLLIN,                
001600*         DVS, TIINLINL = 0 SÅ MAN SLIPPER S0C7                           
001700*                                                                         
001800* ÄNDRING 98-02-24     JOHAN L                                            
001900*         TAGIT BORT 4-VECKORS-GRÄNS FÖR R30 TILL FIL W61262              
002000*                                                                         
002100* ÄNDRING 00-10-19     JOHAN L                                            
002200*   SKAPAR SRS INFORMATION ÅT W517V1                                      
002300*                                                                         
002320* ÄNDRING 18-04-18     SANTHOSHKUMAR A                                    
002330*   ADDED DIST35-CDC-IN-REFILL FOR FOLLOWUP REPORTS FOR INDIA NDC         
002400*   JIRA #2374                                                            
002410*                                                                         
002420*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     SKIP2                                                                
003300*          --- PT=R30                                                     
003400     SELECT W61262                    ASSIGN TO W61264D1.                 
003500     SKIP2                                                                
003600*          --- R30 PASSERADE                                              
003700     SELECT W61263                    ASSIGN TO W61264D2.                 
003800     SKIP2                                                                
003900*          --- R32 UNDER VECKAN                                           
004000     SELECT W61264                    ASSIGN TO W61264D3.                 
004100     SKIP2                                                                
004200*          --- R32 UNDER VECKAN INKL TRANSF                               
004300     SELECT W61265                    ASSIGN TO W61264D4.                 
004400     SKIP2                                                                
004500*          --- R32 OCH 310:OR (SRS INFORMATION)                           
004600     SELECT W61270                    ASSIGN TO W61264D5.                 
004700     EJECT                                                                
004800 DATA DIVISION.                                                           
004900     SKIP2                                                                
005000 FILE SECTION.                                                            
005100     SKIP3                                                                
005200 FD  W61262                                                               
005300     RECORDING       F                                                    
005400     BLOCK CONTAINS  0.                                                   
005500                                                                          
005600*01  POST -COPY W61262  -PRE UT1- -L.                                     
005700     SKIP3                                                                
005800 FD  W61263                                                               
005900     RECORDING       F                                                    
006000     BLOCK CONTAINS  0.                                                   
006100                                                                          
006200*01  POST -COPY W61263  -PRE  UT2-  -L.                                   
006300     SKIP3                                                                
006400 FD  W61264                                                               
006500     RECORDING       F                                                    
006600     BLOCK CONTAINS  0.                                                   
006700                                                                          
006800*01  POST -COPY W61264  -PRE  UT3-  -L.                                   
006900     SKIP3                                                                
007000 FD  W61265                                                               
007100     RECORDING       F                                                    
007200     BLOCK CONTAINS  0.                                                   
007300                                                                          
007400*01  POST -COPY W61265  -PRE  UT4-  -L.                                   
007500     SKIP3                                                                
007600 FD  W61270                                                               
007700     RECORDING       F                                                    
007800     BLOCK CONTAINS  0.                                                   
007900                                                                          
008000*01  POST -COPY W51712  -PRE  UT5-  -L.                                   
008100     EJECT                                                                
008200 WORKING-STORAGE SECTION.                                                 
008300                                                                          
008400                                                                          
008500*    -COPY WY2000W1                                                       
008600     SKIP3                                                                
008700 77  IDPGM                       PIC X(8)    VALUE 'W6126400'.            
008800 77  JA                          PIC X       VALUE 'J'.                   
008900 77  NEJ                         PIC X       VALUE 'N'.                   
009000     EJECT                                                                
009100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
009200 01  FILLER REDEFINES DAGENS-DATUM.                                       
009300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
009400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
009500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
009600     SKIP2                                                                
009700 01  DAGENS-AAVV                 PIC 9(4).                                
009800 01  FILLER REDEFINES DAGENS-AAVV.                                        
009900     03  DAG-AAR                 PIC 9(2).                                
010000     03  DAG-VV                  PIC 9(2).                                
010100     SKIP2                                                                
010200 01  SPAR-IDARTNR                PIC 9(8)    VALUE ZERO.                  
010300 01  INLEVNR-BERAK-X             PIC 9(15).                               
010400 01  INLEVNR-BERAK REDEFINES INLEVNR-BERAK-X.                             
010500     03 FILLER                   PIC 9.                                   
010600     03 INLEV-DATUM              PIC 9(6).                                
010700     03 INLEV-KLOCKA             PIC 9(4).                                
010800     03 FILLER                   PIC 9(4).                                
010900 01  FLSAMMA-DATUM               PIC X.                                   
011000     SKIP2                                                                
011100 01  VECKO-FAELT.                                                         
011200     03  ANTAL-VECKOR            PIC S9(3)   COMP-3.                      
011300     03  VECKO-NR                PIC S9(5)   COMP-3.                      
011400                                                                          
011500*01  TESTAREA -COPY WWDIST35                                              
011600                                                                          
011700       EJECT                                                              
011800 01  DYNAMISKA-SUBPROGRAM.                                                
011900*                                                                         
012000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
012100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
012400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
012600     03  W009VADD                PIC X(8)    VALUE 'W009VADD'.            
012800     SKIP2                                                                
012900*    --- PARAMETRAR TILL ABEND                                            
013000                                                                          
013100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
013300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
013400     SKIP2                                                                
013500 01  FELTEXT.                                                             
013600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
013700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
013800     EJECT                                                                
013900*    --- PARAMETRAR TILL DATKORT                                          
014000*                                                                         
014100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61264'.              
014200     SKIP2                                                                
014300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
014400     SKIP2                                                                
014500*01  -COPY WDATKORT                                                       
014600     EJECT                                                                
014700*    --- PARAMETRAR TILL POSTSUM                                          
014800*                                                                         
014900*01  -COPY W0005   -PRE  POSTSUM-                                         
015000     EJECT                                                                
015100*01  -COPY WDATAREA                                                       
015200     EJECT                                                                
015300 01  W61262-AREA-START           PIC X(24)   VALUE                        
015400                                 'W61262-AREA-START  '.                   
015500     SKIP2                                                                
015600                                                                          
015700*01  AREA -COPY W61262    -PRE WUT1-                                      
015800     EJECT                                                                
015900 01  W61263-AREA-START           PIC X(24)   VALUE                        
016000                                 'W61263-AREA-START  '.                   
016100     SKIP2                                                                
016200                                                                          
016300*01  AREA -COPY W61263     -PRE WUT2-                                     
016400     EJECT                                                                
016500 01  W61264-AREA-START           PIC X(24)   VALUE                        
016600                                 'W61264-AREA-START  '.                   
016700     SKIP2                                                                
016800                                                                          
016900*01  AREA -COPY W61264     -PRE WUT3-                                     
017000     EJECT                                                                
017100 01  W61265-AREA-START           PIC X(24)   VALUE                        
017200                                 'W61265-AREA-START  '.                   
017300     SKIP2                                                                
017400                                                                          
017500*01  AREA -COPY W61265     -PRE WUT4-                                     
017600     EJECT                                                                
017700 01  W61270-AREA-START           PIC X(24)   VALUE                        
017800                                 'W61270-AREA-START  '.                   
017900     SKIP2                                                                
018000                                                                          
018100*01  AREA -COPY W51712     -PRE WUT5-                                     
018200     EJECT                                                                
018300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
018400*                                                                         
018500     EJECT                                                                
018600 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018700     SKIP3                                                                
018800 01  NYCKLAR-TILL-DLI.                                                    
018900     03  W-IDARTNR-X.                                                     
019000         05  W-IDARTNR           PIC S9(8)   VALUE ZERO COMP-3.           
019100     03  W-WDL612KY-X.                                                    
019200         05  W-DAREGDAT          PIC  9(8)   VALUE ZERO.                  
019300         05  W-TIREGTID          PIC S9(7)   VALUE ZERO COMP-3.           
019400     03  W-IDDC-B6-X.                                                     
019500         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
019600     SKIP2                                                                
019700*    --- STATUS-KOD FRÅN IMS                                              
019800 01  STATUS-WS                   PIC XX.                                  
019900     88  SEGMENT-FINNS                       VALUE '  '.                  
020000     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
020100     SKIP2                                                                
020200 01  GODK-STATUSKODER.                                                    
020300     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
020400     SKIP3                                                                
020500 01  SSA1                        PIC X(64).                               
020600 01  SSA2                        PIC X(64).                               
020700     EJECT                                                                
020800*    --- IMS FUNKTIONSKODER                                               
020900*01  -COPY W0003                                                          
021000     EJECT                                                                
021100*    ---  DLI INPUT-OUTPUT AREA                                           
021200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLINLC01'.                    
021300                                                                          
021400 01 DLI-IO-AREA.                                                          
021500     03 IO-AREA     PIC X(600) VALUE SPACE.                               
021600         03 DLI-IO-WLINLC01 REDEFINES IO-AREA.                            
021700*            05 -COPY WDL601 -PRE INLC-                                   
021800     EJECT                                                                
021900         03 DLI-IO-WLINLC11 REDEFINES IO-AREA.                            
022000*            05 -COPY WDL611 -PRE INLC-                                   
022100     EJECT                                                                
022200         03 DLI-IO-WLINLC12 REDEFINES IO-AREA.                            
022300             05 -COPY WDL612 -PRE INLC-                                   
022400     EJECT                                                                
022500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
022600 01   DLI-IO-AREA-B601.                                                   
022700*     03  -COPY WDB601                                                    
022800     EJECT                                                                
022900 LINKAGE SECTION.                                                         
023000                                                                          
023100     EJECT                                                                
023200*01  -COPY W0008  -PRE INLC-                                              
023300     05  FILLER                  PIC X.                                   
023400     EJECT                                                                
023500*01  -COPY W0008      -PRE WDB6-                                          
023600     05  FILLER                  PIC X.                                   
023700     EJECT                                                                
023800 PROCEDURE DIVISION  USING INLC-PCB WDB6-PCB.                             
023900 MAIN SECTION.                                                            
024000     ENTRY 'DLITCBL' USING INLC-PCB WDB6-PCB.                             
024100                                                                          
024200     PERFORM A-INIT                                                       
024300     PERFORM IMS-GET-INLC                                                 
024400     PERFORM UNTIL SEGMENT-SAKNAS                                         
024500       EVALUATE INLC-SEG-NAME-FB                                          
024600       WHEN 'WDL601  '                                                    
024700         MOVE INLC-ART-IDARTNR TO SPAR-IDARTNR                            
024800       WHEN 'WDL611  '                                                    
024900         MOVE INLC-INL-IDDISTR TO DIST35-IDDISTR                          
025000                                                                          
025100         MOVE INLC-INL-IDDC  TO W-IDDC-B6                                 
025200         PERFORM IMS-GU-WDB601                                            
025300*                                                                         
025400*        IF NDC OR LDC  ERSÄTTS AV                                        
025500*                       IF DCS-FLINLHIST = JA                             
025600*                                                                         
025700         IF DCS-FLINLHIST = JA                                            
025800             IF INLC-INL-IDPTYP = 'R30'                                   
025900                MOVE INLC-INL-TIBERANK TO TMP1-YYMMDD                     
026000                MOVE DAGENS-DATUM        TO TMP2-YYMMDD                   
026100                PERFORM WY2000P1                                          
026200                IF TMP1-YYMMDD < TMP2-YYMMDD                              
026300                    PERFORM B-SKAPA-PASSERADE                             
026400                END-IF                                                    
026500                IF DIST35-REFILL-NA OR DIST35-REFILL-NA-JAP               
026600                                    OR DIST35-REFILL-JP                   
026700                                    OR DIST35-CDC-AU-REFILL               
026701                                    OR DIST35-CDC-NONVCC-REFILL           
026702                                    OR DIST35-VCC-NONVCC-REFILL           
026910                                    OR DIST35-NONVCC-CDC-REFILL           
026930                                    OR DIST35-REFILL-INOM-NDC             
026940                                    OR DIST35-NONVCC-NONVCC-REFILL        
026950                                    OR DIST35-NONVCC-VCC-REFILL           
026702                                    OR DIST35-VCC-NONVCC-TRANSFER         
026940                                 OR DIST35-NONVCC-NONVCC-TRANSFER         
026950                                    OR DIST35-NONVCC-VCC-TRANSFER         
027000                    PERFORM C-SKAPA-VAENTADE                              
027100                END-IF                                                    
027200             END-IF                                                       
027300             IF (INLC-INL-IDPTYP = 'R32'                                  
027400                 AND INLC-INL-TIINLINL > ZERO)                            
027500*   IGNORERA FÖRLORADE KOLLIN (DVS. TINILINL SATT TILL NOLL)              
027600                IF DIST35-REFILL-NA OR DIST35-REFILL-NA-JAP               
027700                                    OR DIST35-REFILL-JP                   
027800                                    OR DIST35-CDC-AU-REFILL               
027900                    PERFORM D-SKAPA-VECKANS                               
028000                END-IF                                                    
028100             END-IF                                                       
028200             IF (INLC-INL-IDPTYP = 'R32'                                  
028300                 AND INLC-INL-TIINLINL > ZERO)                            
028400             OR  INLC-INL-IDPTYP = '310'                                  
028500                IF DIST35-REFILL-NA OR DIST35-REFILL-NA-JAP               
028600                                    OR DIST35-REFILL-JP                   
028700                                    OR DIST35-CDC-AU-REFILL               
028800                                    OR DIST35-NA-TRANSFER                 
028810                                    OR DIST35-REFILL-INOM-NDC             
028811                                    OR DIST35-NONVCC-NONVCC-REFILL        
028820                                    OR DIST35-NA-NDC-RETURNS              
028900                                    OR DIST35-PACIFIC-TRANSFER            
028910                                    OR DIST35-REFILL-INOM-JP              
029000                                    OR DIST35-CDC-LDC-REFILL              
029100                                    OR DIST35-CDC-1C-REFILL               
029200                                    OR DIST35-CDC-GB-3A-REFILL            
029300                                    OR DIST35-CDC-ES-REFILL               
029310                                    OR DIST35-CDC-IT-REFILL               
029320                                    OR DIST35-CDC-NL-REFILL               
029330                                    OR DIST35-CDC-AT-REFILL               
029600                                    OR DIST35-CN-TRANSFER                 
029610                                    OR DIST35-NONVCC-CDC-REFILL           
029612                                    OR DIST35-CDC-NONVCC-REFILL           
029613                                    OR DIST35-VCC-NONVCC-REFILL           
029614                                    OR DIST35-NONVCC-VCC-REFILL           
029613                                 OR DIST35-NONVCC-NONVCC-TRANSFER         
029613                                    OR DIST35-VCC-NONVCC-TRANSFER         
029614                                    OR DIST35-NONVCC-VCC-TRANSFER         
029700                    PERFORM E-SKAPA-UPPF                                  
029800                END-IF                                                    
029900             END-IF                                                       
030000         END-IF                                                           
030100         IF (INLC-INL-IDPTYP = ('R30' OR '310')                           
030200         AND INLC-INL-KDRT = 0)                                           
030300*   SKAPAR SRS INFORMATION ÅT W517V1    /ÄT 001019 JOHAN L                
030400            IF (DCS-NDC-NA                                                
030500               AND (INLC-INL-IDLEVNR = '63517' OR '63518'                 
030600                               OR '63519' OR '63520' OR '1441 '))         
030700            OR ((DCS-NDC-PF OR DCS-NDC-OTHERS OR DCS-NDC-SA)              
030800               AND (INLC-INL-IDLEVNR = '15230' OR '7844 '                 
030900                                               OR '1441 '))               
031000            OR ((DCS-NDC-CN OR (DCS-SDC AND DCS-IDLANDX2 = 'CN'))         
031100            AND (INLC-INL-IDLEVNR = 'CHN04' OR 'AEFZT' OR 'AEQD2'         
031200                                 OR 'CHN07' OR '1441 '))                  
031300                   PERFORM G-SKAPA-SRS-INFORMATION                        
031400            END-IF                                                        
031500         END-IF                                                           
031600                                                                          
031700       WHEN 'WDL612  '                                                    
031800         MOVE INLC-ORD-TIBERANK TO TMP1-YYMMDD                            
031900         MOVE DAGENS-DATUM        TO TMP2-YYMMDD                          
032000         PERFORM WY2000P1                                                 
032100         IF INLC-ORD-IDLOPNRM = 0 AND                                     
032200            TMP1-YYMMDD < TMP2-YYMMDD                                     
032300             PERFORM F-SKAPA-PASSERADE-EXT                                
032400         END-IF                                                           
032500                                                                          
032600       WHEN OTHER                                                         
032700         CONTINUE                                                         
032800       END-EVALUATE                                                       
032900                                                                          
033000       PERFORM IMS-GET-INLC                                               
033100     END-PERFORM                                                          
033200                                                                          
033300     PERFORM Z-FINIT                                                      
033400                                                                          
033500     MOVE ZERO TO RETURN-CODE                                             
033600     GOBACK                                                               
033700     .                                                                    
033800     EJECT                                                                
033900 A-INIT SECTION.                                                          
034000                                                                          
034100     OPEN OUTPUT W61262                                                   
034200                 W61263                                                   
034300                 W61264                                                   
034400                 W61265                                                   
034500                 W61270                                                   
034600                                                                          
034700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
034800     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
034900     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
035000     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
035001**???                                                                     
035010*    MOVE 181123   TO DAGENS-DATUM                                        
035020*    MOVE 47       TO K-VECKA                                             
035030**???                                                                     
035100     MOVE IDPGM       TO POSTSUM-PROGNAMN                                 
035200                                                                          
035300     .                                                                    
035400     EJECT                                                                
035500                                                                          
035600 B-SKAPA-PASSERADE SECTION.                                               
035700     MOVE INLC-INL-IDDC      TO WUT2-IDDC                                 
035800     MOVE SPAR-IDARTNR       TO WUT2-IDARTNR                              
035900     MOVE INLC-INL-IDKUNDRF  TO WUT2-IDKUNDRF                             
036000     MOVE INLC-INL-IDLEVNR   TO WUT2-IDLEVNR                              
036100     MOVE INLC-INL-KVAVIS    TO WUT2-KVAVIS                               
036200     MOVE INLC-INL-TIBERANK  TO WUT2-TIBERANK                             
036300                                                                          
036400     PERFORM S12-SKRIV-W61263                                             
036500     .                                                                    
036600     EJECT                                                                
036700 C-SKAPA-VAENTADE SECTION.                                                
036800                                                                          
036900     IF NOT  INLC-INL-KDFRAKT = 17 OR 19                                  
037000                                                                          
037100         MOVE INLC-INL-IDDC       TO WUT1-IDDC                            
037200         MOVE INLC-INL-IDFAKT     TO WUT1-IDFAKT                          
037300         MOVE INLC-INL-IDORDNR5   TO WUT1-IDORDNR5                        
037400         MOVE INLC-INL-IDKOLLI    TO WUT1-IDKOLLI                         
037500         MOVE INLC-INL-IDKUNDNR   TO WUT1-IDKUNDNR                        
037600         MOVE INLC-INL-PRARTNTO   TO WUT1-PRARTNTO                        
037700         MOVE INLC-INL-PRKURS     TO WUT1-PRKURS                          
037800         MOVE SPAR-IDARTNR        TO WUT1-IDARTNR                         
037900         MOVE INLC-INL-KDVALISO   TO WUT1-KDVALISO                        
038000         MOVE INLC-INL-KVAVIS     TO WUT1-KVAVIS                          
038100         MOVE INLC-INL-TIBERANK   TO WUT1-TIBERANK                        
038200         MOVE SPACE               TO WUT1-FLNYART                         
038300                                                                          
038400         PERFORM S11-SKRIV-W61262                                         
038500     END-IF                                                               
038600     .                                                                    
038700     EJECT                                                                
038800 D-SKAPA-VECKANS  SECTION.                                                
038810     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
038900     MOVE INLC-INL-TIINLINL TO DAT-I-TIDATUM                              
039000     CALL WDATKONV USING DAT-KDDATFORM                                    
039100                         DAT-I-TIDATUM                                    
039200                         DAT-O-TIDATUM                                    
039300                         DAT-KDSVAR                                       
039400     IF DAT-KDSVAR-OK                                                     
039500         IF DAT-TIAA-VECKA = D-AAR AND                                    
039600            DAT-TIVV = K-VECKA                                            
039700             MOVE INLC-INL-IDDC       TO WUT3-IDDC                        
039800             MOVE INLC-INL-KDFRAKT    TO WUT3-KDFRAKT                     
039900             MOVE INLC-INL-TIINLINL   TO WUT3-TIINLINL                    
040000             MOVE INLC-INL-TIINLITI   TO WUT3-TIINLITI                    
040100             MOVE INLC-INL-DAINLEV    TO WUT3-DAINLEV                     
040200             PERFORM S13-SKRIV-W61264                                     
040300         END-IF                                                           
040400     ELSE                                                                 
040500         DISPLAY 'FEL FRÅN DATKONV (D-SKAPA-VECKANS)'                     
040600         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
040700     END-IF                                                               
040800     .                                                                    
040900     EJECT                                                                
041000 E-SKAPA-UPPF     SECTION.                                                
041100     IF INLC-INL-IDPTYP = 'R32'                                           
041200       MOVE 'AAMMDD'          TO DAT-KDDATFORM                            
041300       MOVE INLC-INL-TIINLINL TO DAT-I-TIDATUM                            
041400       CALL WDATKONV USING DAT-KDDATFORM                                  
041500                           DAT-I-TIDATUM                                  
041600                           DAT-O-TIDATUM                                  
041700                           DAT-KDSVAR                                     
041800       IF DAT-KDSVAR-OK                                                   
041900          IF DAT-TIAA-VECKA = D-AAR                                       
042000          AND DAT-TIVV = K-VECKA                                          
042100             IF INLC-INL-KVANTMOT > 0                                     
042200               PERFORM EA-SKRIV-UPPF                                      
042300             END-IF                                                       
042400          END-IF                                                          
042500       ELSE                                                               
042600          DISPLAY 'FEL FRÅN DATKONV (E-SKAPA-UPPF)'                       
042700          CALL ABEND USING RKOD-ABEND-UTAN-DUMP                           
042800       END-IF                                                             
042900     ELSE                                                                 
043000*      -- 310:OR                                                          
043100       PERFORM EA-SKRIV-UPPF                                              
043200     END-IF                                                               
043300     .                                                                    
043400     EJECT                                                                
043500 EA-SKRIV-UPPF      SECTION.                                              
043600                                                                          
043700     MOVE INLC-INL-IDPTYP         TO WUT4-IDPTYP                          
043800     MOVE INLC-INL-IDDC           TO WUT4-IDDC                            
043900     MOVE INLC-INL-IDDISTR        TO WUT4-IDDISTR                         
044000     MOVE INLC-INL-FLPRIO         TO WUT4-FLPRIO                          
044100     MOVE INLC-INL-KDFRAKT        TO WUT4-KDFRAKT                         
044200     MOVE SPAR-IDARTNR            TO WUT4-IDARTNR                         
044300     MOVE INLC-INL-PRARTNTO       TO WUT4-PRARTNTO                        
044400     MOVE INLC-INL-PRKURS         TO WUT4-PRKURS                          
044500     MOVE INLC-INL-KDVALISO       TO WUT4-KDVALISO                        
044600     MOVE INLC-INL-KVANTMOT       TO WUT4-KVANTMOT                        
044700     MOVE INLC-INL-TIINLMOT       TO WUT4-TIINLMOT                        
044800     MOVE INLC-INL-TIINLMTI       TO WUT4-TIINLMTI                        
044900     MOVE INLC-INL-TIINLINL       TO WUT4-TIINLINL                        
045000     MOVE INLC-INL-TIINLITI       TO WUT4-TIINLITI                        
045100                                                                          
045200     PERFORM S14-SKRIV-W61265                                             
045300     .                                                                    
045400     EJECT                                                                
045500 F-SKAPA-PASSERADE-EXT SECTION.                                           
045600     MOVE INLC-ORD-IDDC      TO WUT2-IDDC                                 
045700     MOVE SPAR-IDARTNR       TO WUT2-IDARTNR                              
045800     MOVE INLC-ORD-IDKUNDRF  TO WUT2-IDKUNDRF                             
045900     MOVE INLC-ORD-IDLEVNR   TO WUT2-IDLEVNR                              
046000     MOVE INLC-ORD-KVBEART   TO WUT2-KVAVIS                               
046100     MOVE INLC-ORD-TIBERANK  TO WUT2-TIBERANK                             
046200                                                                          
046300     PERFORM S12-SKRIV-W61263                                             
046400     .                                                                    
046500     EJECT                                                                
046600 G-SKAPA-SRS-INFORMATION SECTION.                                         
046700     MOVE SPAR-IDARTNR       TO WUT5-IDARTNR                              
046800     MOVE INLC-INL-IDDC      TO WUT5-IDDC                                 
046900     MOVE INLC-INL-KVAVIS    TO WUT5-KVAKS                                
047000     MOVE ZEROES             TO WUT5-KVLS                                 
047100     MOVE ZEROES             TO WUT5-KVEFRS                               
047200     MOVE ZEROES             TO WUT5-KVRESS                               
047300     MOVE ZEROES             TO WUT5-KVOKS                                
047400                                                                          
047500     PERFORM S15-SKRIV-W61270                                             
047600     .                                                                    
047700     EJECT                                                                
047800 Z-FINIT SECTION.                                                         
047900     CLOSE W61262                                                         
048000           W61263                                                         
048100           W61264                                                         
048200           W61265                                                         
048300           W61270                                                         
048400     SKIP2                                                                
048500     MOVE 'S' TO POSTSUM-OPKOD                                            
048600     CALL POSTSUM USING POSTSUM-PARM                                      
048700     .                                                                    
048800     EJECT                                                                
048900 S11-SKRIV-W61262 SECTION.                                                
049000                                                                          
049100     WRITE UT1-POST FROM WUT1-AREA                                        
049200                                                                          
049300     MOVE INLC-INL-IDPTYP TO POSTSUM-TRANSTYP                             
049400     MOVE 'W61262' TO POSTSUM-FDNAMN                                      
049500     MOVE 'W61264D1' TO POSTSUM-DDNAMN2                                   
049600     CALL POSTSUM USING POSTSUM-PARM                                      
049700     .                                                                    
049800     EJECT                                                                
049900 S12-SKRIV-W61263 SECTION.                                                
050000                                                                          
050100     WRITE UT2-POST FROM WUT2-AREA                                        
050200                                                                          
050300     MOVE 'SEN'         TO POSTSUM-TRANSTYP                               
050400     MOVE 'W61263' TO POSTSUM-FDNAMN                                      
050500     MOVE 'W61264D2' TO POSTSUM-DDNAMN2                                   
050600     CALL POSTSUM USING POSTSUM-PARM                                      
050700     .                                                                    
050800     EJECT                                                                
050900 S13-SKRIV-W61264 SECTION.                                                
051000                                                                          
051100     WRITE UT3-POST FROM WUT3-AREA                                        
051200                                                                          
051300     MOVE INLC-INL-IDPTYP TO POSTSUM-TRANSTYP                             
051400     MOVE 'W61264' TO POSTSUM-FDNAMN                                      
051500     MOVE 'W61264D3' TO POSTSUM-DDNAMN2                                   
051600     CALL POSTSUM USING POSTSUM-PARM                                      
051700     .                                                                    
051800     EJECT                                                                
051900 S14-SKRIV-W61265 SECTION.                                                
052000                                                                          
052100     WRITE UT4-POST FROM WUT4-AREA                                        
052200                                                                          
052300     MOVE INLC-INL-IDPTYP TO POSTSUM-TRANSTYP                             
052400     MOVE 'W61265' TO POSTSUM-FDNAMN                                      
052500     MOVE 'W61264D4' TO POSTSUM-DDNAMN2                                   
052600     CALL POSTSUM USING POSTSUM-PARM                                      
052700     .                                                                    
052800     EJECT                                                                
052900 S15-SKRIV-W61270 SECTION.                                                
053000                                                                          
053100     WRITE UT5-POST FROM WUT5-AREA                                        
053200                                                                          
053300     MOVE INLC-INL-IDPTYP TO POSTSUM-TRANSTYP                             
053400     MOVE 'W61270' TO POSTSUM-FDNAMN                                      
053500     MOVE 'W61264D5' TO POSTSUM-DDNAMN2                                   
053600     CALL POSTSUM USING POSTSUM-PARM                                      
053700     .                                                                    
053800     EJECT                                                                
053900* --- IMS SEKTIONER ---                                                   
054000     SKIP3                                                                
054100     EJECT                                                                
054200 IMS-GET-INLC SECTION.                                                    
054300                                                                          
054400     CALL CBLTDLI USING GN INLC-PCB DLI-IO-AREA                           
054500     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
054600     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
054700     PERFORM IMS-STATUSKONTROLL                                           
054800     .                                                                    
054900     EJECT                                                                
055000                                                                          
055100 IMS-GU-WDB601    SECTION.                                                
055200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
055300          DELIMITED BY SIZE INTO SSA1                                     
055400     MOVE '  ' TO GODK-STATUSKODER                                        
055500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
055600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
055700     PERFORM IMS-STATUSKONTROLL                                           
055800     .                                                                    
055900     EJECT                                                                
056000 IMS-STATUSKONTROLL SECTION.                                              
056100                                                                          
056200     SET STATUS-IX TO 1                                                   
056300     SEARCH GODK-STATUS                                                   
056400       AT END                                                             
056500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
056600           DELIMITED BY SIZE INTO FELTEXT                                 
056700         DISPLAY FELTEXT                                                  
056800         CALL FELLOG                                                      
056900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
057000         CONTINUE                                                         
057100     END-SEARCH                                                           
057200     .                                                                    
057300     EJECT                                                                
057400*    -COPY WY2000P1                                                       
