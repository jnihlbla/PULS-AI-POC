000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6125100.                                                
000300 AUTHOR.         JOHAN LINDKVIST.                                         
000400 DATE-WRITTEN.   98/11/16.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER WD06-DATABASEN DAGLIGEN FÖR ATT SAMLA SDC-HISTORIK         
000900*        OM ARTIKLAR TILL JPN/AUS MED VISS IDDISTR.                       
001000*        OBS !  KÖRS DAGTID  OBS !                                        
001100*                                                                         
001200*        TRE UTFILER:                                                     
001300*        W6125X : VARJE DAG. ALLA PT=310                                  
001400*        W6125Y : VARJE DAG. VISSA R30 OCH R32 SAMT 310:ER                
001500*                                                                         
001600*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
001700*                                                                         
001800*    ABENDKODER:                                                          
001900*        U0016 -  . . . .                                                 
002000*        U1000 -  . . . .                                                 
002100*                                                                         
002200                                                                          
002300     SKIP3                                                                
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     SKIP2                                                                
003000*          --- PT=310                                                     
003100     SELECT W61251                    ASSIGN TO W61251D1.                 
003200     SKIP2                                                                
003300*          --- R30 OCH R32                                                
003400     SELECT W61253                    ASSIGN TO W61251D3.                 
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W61251                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  POST -COPY W61251 -PRE UT1- -L.                                      
004500     SKIP3                                                                
004600 FD  W61253                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  POST -COPY W61253  -PRE  UT3-  -L.                                   
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400                                                                          
005500*    -- CHECKED BY WY2000                                                 
005600 77  IDPGM                       PIC X(8)    VALUE 'W6125100'.            
005700 77  JA                          PIC X       VALUE 'J'.                   
005800 77  NEJ                         PIC X       VALUE 'N'.                   
005900     EJECT                                                                
006000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES DAGENS-DATUM.                                       
006200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006500                                                                          
006600 01  SPAR-IDARTNR                PIC 9(8)    VALUE ZERO.                  
006700 01  INLEVNR-BERAK-X             PIC 9(16).                               
006800 01  INLEVNR-BERAK REDEFINES INLEVNR-BERAK-X.                             
006900     03 INLEV-SEKEL              PIC 9(2).                                
007000     03 INLEV-DATUM              PIC 9(6).                                
007100     03 INLEV-KLOCKA             PIC 9(4).                                
007200     03 FILLER                   PIC 9(4).                                
007300 01  FLSAMMA-DATUM               PIC X.                                   
007400                                                                          
007500*01  TESTAREA -COPY WWDIST35                                              
007600                                                                          
007700     EJECT                                                                
007800*      --- VALID IDDC CODES                                               
007900*                                                                         
008000*01    -COPY WWDC99                                                       
008100       EJECT                                                              
008200 01  DYNAMISKA-SUBPROGRAM.                                                
008300*                                                                         
008400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008700     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
008800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009000     SKIP2                                                                
009100*    --- PARAMETRAR TILL ABEND                                            
009200                                                                          
009300 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009400 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009500 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009600     SKIP2                                                                
009700 01  FELTEXT.                                                             
009800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010000     EJECT                                                                
010100*    --- PARAMETRAR TILL DATKORT                                          
010200*                                                                         
010300 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61251'.              
010400     SKIP2                                                                
010500 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
010600     SKIP2                                                                
010700*01  -COPY WDATKORT                                                       
010800     EJECT                                                                
010900*    --- PARAMETRAR TILL POSTSUM                                          
011000*                                                                         
011100*01  -COPY W0005   -PRE  POSTSUM-                                         
011200     EJECT                                                                
011300*01  -COPY WDATAREA                                                       
011400     EJECT                                                                
011500 01  W61251-AREA-START           PIC X(24)   VALUE                        
011600                                 'W61251-AREA-START  '.                   
011700     SKIP2                                                                
011800                                                                          
011900*01  AREA -COPY W61251    -PRE UT1-                                       
012000     EJECT                                                                
012100 01  W61253-AREA-START           PIC X(24)   VALUE                        
012200                                 'W61253-AREA-START  '.                   
012300     SKIP2                                                                
012400                                                                          
012500*01  AREA -COPY W61253     -PRE UT3-                                      
012600     EJECT                                                                
012700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012800*                                                                         
012900     EJECT                                                                
013000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013100     SKIP3                                                                
013200 01  NYCKLAR-TILL-DLI.                                                    
013300     03  W-IDARTNR-X.                                                     
013400         05  W-IDARTNR           PIC S9(8)   VALUE ZERO COMP-3.           
013500     03  W-WDL612KY-X.                                                    
013600         05  W-DAREGDAT          PIC 9(8)    VALUE ZERO.                  
013700         05  W-TIREGTID          PIC S9(7)   VALUE ZERO COMP-3.           
013800     SKIP2                                                                
013900*    --- STATUS-KOD FRÅN IMS                                              
014000 01  STATUS-WS                   PIC XX.                                  
014100     88  SEGMENT-FINNS                       VALUE '  '.                  
014200     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
014300     SKIP2                                                                
014400 01  GODK-STATUSKODER.                                                    
014500     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
014600     SKIP3                                                                
014700 01  SSA1                        PIC X(64).                               
014800 01  SSA2                        PIC X(64).                               
014900     EJECT                                                                
015000*    --- IMS FUNKTIONSKODER                                               
015100*01  -COPY W0003                                                          
015200     EJECT                                                                
015300*    ---  DLI INPUT-OUTPUT AREA                                           
015400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLINLC01'.                    
015500                                                                          
015600 01 DLI-IO-AREA.                                                          
015700     03 IO-AREA     PIC X(600) VALUE SPACE.                               
015800         03 DLI-IO-WLINLC01 REDEFINES IO-AREA.                            
015900*            05 -COPY WDL601 -PRE INLC-                                   
016000     EJECT                                                                
016100         03 DLI-IO-WLINLC11 REDEFINES IO-AREA.                            
016200*            05 -COPY WDL611 -PRE INLC-                                   
016300     EJECT                                                                
016400         03 DLI-IO-WLINLC12 REDEFINES IO-AREA.                            
016500             05 -COPY WDL612 -PRE INLC-                                   
016600     EJECT                                                                
016700 LINKAGE SECTION.                                                         
016800                                                                          
016900     EJECT                                                                
017000*01  -COPY W0008  -PRE INLC-                                              
017100     05  FILLER                  PIC X.                                   
017200     EJECT                                                                
017300 PROCEDURE DIVISION  USING INLC-PCB.                                      
017400 MAIN SECTION.                                                            
017500     ENTRY 'DLITCBL' USING INLC-PCB.                                      
017600                                                                          
017700     PERFORM A-INIT                                                       
017800     PERFORM IMS-GET-INLC                                                 
017900     PERFORM UNTIL SEGMENT-SAKNAS                                         
018000       EVALUATE INLC-SEG-NAME-FB                                          
018100       WHEN 'WDL601  '                                                    
018200         MOVE INLC-ART-IDARTNR  TO  SPAR-IDARTNR                          
018300       WHEN 'WDL611  '                                                    
018400         MOVE INLC-INL-IDDISTR  TO DIST35-IDDISTR                         
018500                                                                          
018600         MOVE INLC-INL-IDDC   TO WS-IDDC                                  
018700         IF (((NDC-PACIFIC) AND                                           
018800              (DIST35-REFILL-JP                                           
018900            OR DIST35-CDC-AU-REFILL                                       
019000            OR DIST35-NONVCC-VCC-REFILL                                   
019000            OR DIST35-NONVCC-VCC-TRANSFER                                 
019100            OR DIST35-PACIFIC-TRANSFER                                    
019100            OR DIST35-REFILL-INOM-NONVCC-NDC                              
019110            OR DIST35-REFILL-INOM-JP)) OR                                 
                                                                                
019200             ((NDC-PACIFIC AND XDC-NON-VCC-OWNED) AND                     
019300              (DIST35-CDC-NONVCC-REFILL                                   
019400            OR DIST35-VCC-NONVCC-REFILL                                   
019400            OR DIST35-NONVCC-NONVCC-REFILL                                
019400            OR DIST35-NONVCC-NONVCC-TRANSFER                              
019400            OR DIST35-VCC-NONVCC-TRANSFER)) OR                            
                                                                                
019200             ((NDC-NX AND XDC-NON-VCC-OWNED AND NDC-AE) AND               
019300              (DIST35-CDC-NONVCC-REFILL                                   
019400            OR DIST35-VCC-NONVCC-REFILL                                   
019400            OR DIST35-NONVCC-NONVCC-REFILL                                
019400            OR DIST35-NONVCC-NONVCC-TRANSFER                              
019400            OR DIST35-VCC-NONVCC-TRANSFER)) OR                            
                                                                                
019510             ((NDC-CN OR LDC-CN) AND                                      
019530              (DIST35-REFILL-CN                                           
019540            OR DIST35-REFILL-INOM-NONVCC-NDC                              
019550            OR DIST35-NONVCC-NONVCC-REFILL                                
019560            OR DIST35-CN-TRANSFER)) OR                                    
                                                                                
019600              (CDC-SE AND                                                 
019710               DIST35-NONVCC-CDC-REFILL) ) AND                            
                                                                                
019800            (INLC-INL-IDPTYP = 'R30' OR '310' OR 'R32')                   
019900                                                                          
020000             PERFORM B-LAGRA-TESTDATA                                     
020100             PERFORM C-TILLDELNING                                        
020200                                                                          
020300             IF INLC-INL-IDPTYP = '310'                                   
020400               IF NDC-PACIFIC                                             
021200                 PERFORM S11-SKRIV-W61251                                 
021300               END-IF                                                     
021400               PERFORM S13-SKRIV-W61253                                   
021500             END-IF                                                       
021600                                                                          
021700             IF (INLC-INL-IDPTYP    =  'R30')        AND                  
021800                ((INLC-INL-KDFRAKT  =  17 OR 19)     OR                   
021900                 (INLEV-DATUM       =  DAGENS-DATUM))                     
022000               PERFORM S13-SKRIV-W61253                                   
022100             END-IF                                                       
022200                                                                          
022300             IF (INLC-INL-IDPTYP    = 'R32'         AND                   
022400                  INLC-INL-FLMAKUL  = 'N'           AND                   
022500                  FLSAMMA-DATUM     = NEJ)                                
022600                 IF INLC-INL-TIINLINL = DAGENS-DATUM                      
022700                   IF INLC-INL-KVANTMOT = 0                               
022800                     CONTINUE                                             
022900                   ELSE                                                   
023000                     PERFORM S13-SKRIV-W61253                             
023100                   END-IF                                                 
023200                 END-IF                                                   
023300             END-IF                                                       
023400                                                                          
023500         ELSE                                                             
023600           CONTINUE                                                       
023700         END-IF                                                           
023800                                                                          
023900       WHEN OTHER                                                         
024000         CONTINUE                                                         
024100       END-EVALUATE                                                       
024200                                                                          
024300       PERFORM IMS-GET-INLC                                               
024400     END-PERFORM                                                          
024500                                                                          
024600     PERFORM Z-FINIT                                                      
024700                                                                          
024800     MOVE ZERO TO RETURN-CODE                                             
024900     GOBACK                                                               
025000     .                                                                    
025100     EJECT                                                                
025200 A-INIT SECTION.                                                          
025300                                                                          
025400     OPEN OUTPUT W61251                                                   
025500                 W61253                                                   
025600                                                                          
025700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
025800     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
025900     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
026000     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
026100     MOVE IDPGM       TO POSTSUM-PROGNAMN                                 
026200                                                                          
026300     .                                                                    
026400     EJECT                                                                
026500                                                                          
026600 B-LAGRA-TESTDATA SECTION.                                                
026700     SUBTRACT INLC-INL-DAINLEV FROM 9999999999999999                      
026800                                GIVING INLEVNR-BERAK-X                    
026900                                                                          
027000     IF    (INLEV-DATUM  =  INLC-INL-TIINLINL) AND                        
027100           (INLEV-DATUM  =  INLC-INL-TIINLMOT)                            
027200         MOVE JA TO FLSAMMA-DATUM                                         
027300     ELSE                                                                 
027400         MOVE NEJ TO FLSAMMA-DATUM                                        
027500     END-IF                                                               
027600                                                                          
027700     .                                                                    
027800     EJECT                                                                
027900 C-TILLDELNING SECTION.                                                   
028000     MOVE SPAR-IDARTNR        TO  UT1-SHIST-IDARTNR                       
028100     MOVE INLC-INL-FLPRIO     TO  UT1-SHIST-FLPRIO                        
028200     MOVE INLC-INL-IDDC       TO  UT1-SHIST-IDDC                          
028300     MOVE INLC-INL-IDFAKT     TO  UT1-SHIST-IDFAKT                        
028400     MOVE INLC-INL-IDKOLLI    TO  UT1-SHIST-IDKOLLI                       
028500     MOVE INLC-INL-KDVALISO   TO  UT1-SHIST-KDVALISO                      
028600     MOVE INLC-INL-KVAVIS     TO  UT1-SHIST-KVAVIS                        
028700     MOVE INLC-INL-PRARTNTO   TO  UT1-SHIST-PRARTNTO                      
028800     MOVE INLC-INL-PRKURS     TO  UT1-SHIST-PRKURS                        
028900     MOVE INLC-INL-IDORDNR5   TO  UT1-SHIST-IDORDNR5                      
029000     MOVE INLC-INL-IDKUNDNR   TO  UT1-SHIST-IDKUNDNR                      
029100                                                                          
029200     MOVE INLC-INL-IDPTYP     TO  UT3-SHIST-IDPTYP                        
029300     MOVE INLC-INL-IDDC       TO  UT3-SHIST-IDDC                          
029400     MOVE INLC-INL-IDDISTR    TO  UT3-SHIST-IDDISTR                       
029500     MOVE INLC-INL-IDFAKT     TO  UT3-SHIST-IDFAKT                        
029600     MOVE INLC-INL-IDKOLLI    TO  UT3-SHIST-IDKOLLI                       
029700     MOVE INLC-INL-IDKUNDNR   TO  UT3-SHIST-IDKUNDNR                      
029800     MOVE SPAR-IDARTNR        TO  UT3-SHIST-IDARTNR                       
029900     MOVE INLC-INL-PRARTNTO   TO  UT3-SHIST-PRARTNTO                      
030000     MOVE INLC-INL-PRKURS     TO  UT3-SHIST-PRKURS                        
030100     MOVE INLC-INL-KDVALISO   TO  UT3-SHIST-KDVALISO                      
030200     MOVE INLC-INL-KVAVIS     TO  UT3-SHIST-KVAVIS                        
030300     MOVE INLC-INL-KVANTMOT   TO  UT3-SHIST-KVANTMOT                      
030400     MOVE INLC-INL-TIBERANK   TO  UT3-SHIST-TIBERANK                      
030500     MOVE INLC-INL-FLPRIO     TO  UT3-SHIST-FLPRIO                        
030600     MOVE INLC-INL-KDFRAKT    TO  UT3-SHIST-KDFRAKT                       
030700     MOVE INLC-INL-IDORDNR5   TO  UT3-SHIST-IDORDNR5                      
030800     MOVE INLC-INL-TIINLMOT   TO  UT3-SHIST-TIINLMOT                      
030900     MOVE INLC-INL-TIINLMTI   TO  UT3-SHIST-TIINLMTI                      
031000     MOVE INLC-INL-TIINLINL   TO  UT3-SHIST-TIINLINL                      
031100     MOVE INLC-INL-TIINLITI   TO  UT3-SHIST-TIINLITI                      
031200     .                                                                    
031300 Z-FINIT SECTION.                                                         
031400     CLOSE W61251                                                         
031500           W61253                                                         
031600     SKIP2                                                                
031700     MOVE 'S' TO POSTSUM-OPKOD                                            
031800     CALL POSTSUM USING POSTSUM-PARM                                      
031900     .                                                                    
032000     EJECT                                                                
032100 S11-SKRIV-W61251 SECTION.                                                
032200                                                                          
032300     WRITE UT1-POST FROM UT1-AREA                                         
032400                                                                          
032500     MOVE INLC-INL-IDPTYP TO POSTSUM-TRANSTYP                             
032600     MOVE 'W61251' TO POSTSUM-FDNAMN                                      
032700     MOVE 'W61251D1' TO POSTSUM-DDNAMN2                                   
032800     CALL POSTSUM USING POSTSUM-PARM                                      
032900     .                                                                    
033000     EJECT                                                                
033100 S13-SKRIV-W61253 SECTION.                                                
033200                                                                          
033300     WRITE UT3-POST FROM UT3-AREA                                         
033400                                                                          
033500     MOVE INLC-INL-IDPTYP TO POSTSUM-TRANSTYP                             
033600     MOVE 'W61253' TO POSTSUM-FDNAMN                                      
033700     MOVE 'W61251D3' TO POSTSUM-DDNAMN2                                   
033800     CALL POSTSUM USING POSTSUM-PARM                                      
033900     .                                                                    
034000     EJECT                                                                
034100 S99-ABEND SECTION.                                                       
034200                                                                          
034300     SKIP2                                                                
034400     MOVE 'S' TO POSTSUM-OPKOD                                            
034500     CALL POSTSUM USING POSTSUM-PARM                                      
034600     CALL ABEND USING RKOD-ABEND                                          
034700     .                                                                    
034800     EJECT                                                                
034900* --- IMS SEKTIONER ---                                                   
035000     SKIP3                                                                
035100     EJECT                                                                
035200 IMS-GET-INLC SECTION.                                                    
035300                                                                          
035400     CALL CBLTDLI USING GN INLC-PCB DLI-IO-AREA                           
035500     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
035600     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
035700     PERFORM IMS-STATUSKONTROLL                                           
035800     .                                                                    
035900     EJECT                                                                
036000 IMS-STATUSKONTROLL SECTION.                                              
036100                                                                          
036200     SET STATUS-IX TO 1                                                   
036300     SEARCH GODK-STATUS                                                   
036400       AT END                                                             
036500         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
036600           DELIMITED BY SIZE INTO FELTEXT                                 
036700         DISPLAY FELTEXT                                                  
036800         CALL FELLOG                                                      
036900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
037000         CONTINUE                                                         
037100     END-SEARCH                                                           
037200     .                                                                    
