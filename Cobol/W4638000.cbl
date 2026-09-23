000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4638000.                                                
000300 AUTHOR.         BO SVENSSON.                                             
000400 DATE-WRITTEN.   97/08/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        SB.                                                              
000900*        SKAPAR POSTER FÖR EJ HELT LEVERERADE DIREKTLEVERANS-             
001000*        ORDERRADER FÖR NDC/DDC TILL FIL.                                 
001100*                                                                         
001200*        PROGRAMMET LÄSER      WDQ3   SB                                  
001300*                              WDE4                                       
001310*                              WDF6                                       
001320*                              WDQ2                                       
001400*                                                                         
001500*    ABENDKODER:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- EJ LEVERERADE DIRLEVRADER, NDC.                            
002800     SELECT W46380                     ASSIGN TO W46380D1.                
002900*          --- EJ LEVERERADE DIRLEVRADER, DDC.                            
003000     SELECT W46382                     ASSIGN TO W46380D2.                
003100     EJECT                                                                
003200 DATA DIVISION.                                                           
003300     SKIP2                                                                
003400 FILE SECTION.                                                            
003500     SKIP3                                                                
003600 FD  W46380                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  POST -COPY W46380 -PRE  UT1-  -L.                                    
004100     EJECT                                                                
004200 FD  W46382                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  POST -COPY W46382 -PRE  UT2-  -L.                                    
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000                                                                          
005100*    -COPY WY2000W1                                                       
005200     SKIP3                                                                
005300*    -- CHECKED BY WY2000                                                 
005400 77  IDPGM                       PIC X(8)    VALUE 'W4638000'.            
005500 77  JA                          PIC X       VALUE 'J'.                   
005600 77  NEJ                         PIC X       VALUE 'N'.                   
005610 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005620 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005700     EJECT                                                                
005800 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005900 01  FILLER REDEFINES DAGENS-DATUM.                                       
006000     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006100     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006200     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006300 01  WS-TIORDREG-FIL             PIC 9(6)    VALUE ZERO.                  
006400     EJECT                                                                
006500*      --- VALID IDDC CODES                                               
006700*01    -COPY WWDC99                                                       
006800       EJECT                                                              
006810*      --- VALID DDGS                                                     
006820*01    -COPY WWLEV06                                                      
006830       EJECT                                                              
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000*                                                                         
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007400     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007500     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007700     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
007800     SKIP2                                                                
007900*    --- PARAMETRAR TILL ABEND                                            
008000                                                                          
008100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008400     SKIP2                                                                
008500 01  FELTEXT.                                                             
008600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008800     EJECT                                                                
008900*    --- PARAMETRAR TILL DATKORT                                          
009000*                                                                         
009100 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W46380'.              
009200     SKIP2                                                                
009300 01  DATUMKORT-ID                PIC X(6)    VALUE '000001'.              
009400     SKIP2                                                                
009500*01  -COPY WDATKORT                                                       
009600     EJECT                                                                
009700*    --- PARAMETRAR TILL POSTSUM                                          
009800*                                                                         
009900*01  -COPY W0005   -PRE  POSTSUM-                                         
010000     EJECT                                                                
010100*                                                                         
010200*01  -COPY WDAGAREA                                                       
010300     EJECT                                                                
010400 01  UT-AREA-START               PIC X(24)   VALUE                        
010500                                 'UT-AREA-START  '.                       
010600     SKIP2                                                                
010700                                                                          
010800*01  AREA -COPY W46380     -PRE UT-                                       
010900     EJECT                                                                
011000                                                                          
011100*01  AREA -COPY W46382     -PRE UTDDC-                                    
011200     EJECT                                                                
011300                                                                          
011400*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011500*                                                                         
011600     EJECT                                                                
011700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011800     SKIP3                                                                
011900 01  NYCKLAR-TILL-DLI.                                                    
012000     03  W-IDORDER-X.                                                     
012100         05  W-IDORDER           PIC S9(7)    COMP-3 VALUE ZERO.          
012101     03  W-WDQ211KY-X.                                                    
012102         05  W-IDDC              PIC X(2)     VALUE SPACE.                
012103         05  W-IDLEVNR           PIC X(5)     VALUE SPACE.                
012110     03  W-WDQ301KY-X.                                                    
012120         05  W-WDQ301KY          PIC X(12)    VALUE SPACE.                
012200     03  W-WDE4KEY-X.                                                     
012300         05  W-IDDISTR           PIC S9(5)    COMP-3 VALUE ZERO.          
012400         05  W-IDKUNDNR          PIC S9(7)    COMP-3 VALUE ZERO.          
012500         05  W-IDORDNR5          PIC 9(5)            VALUE ZERO.          
012600         05  FILLER              PIC X(5)            VALUE SPACE.         
012700         05  W-IDPRODNR          PIC S9(7)    COMP-3 VALUE ZERO.          
012800         05  W-IDPLKLST          PIC S9(3)    COMP-3 VALUE ZERO.          
012810     03  W-IDPRODNR-F6-X.                                                 
012811         05  W-IDPRODNR-F6       PIC S9(7)    COMP-3 VALUE ZERO.          
012900     SKIP2                                                                
013000*    --- STATUS-KOD FRÅN IMS                                              
013100 01  STATUS-WS                   PIC XX.                                  
013200     88  SEGMENT-FINNS                       VALUE '  '.                  
013300     88  SEGMENT-SLUT                        VALUE 'GB'.                  
013400     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013410 01  F6-STATUS-WS                PIC XX.                                  
013420     88  F6-SEGMENT-FINNS                    VALUE '  '.                  
013440     88  F6-SEGMENT-SAKNAS                   VALUE 'GE'.                  
013500     SKIP2                                                                
013600 01  GODK-STATUSKODER.                                                    
013700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013800     SKIP3                                                                
013900 01  SSA1                        PIC X(64).                               
014000 01  SSA2                        PIC X(64).                               
014100     EJECT                                                                
014200*    --- IMS FUNKTIONSKODER                                               
014300*01  -COPY W0003                                                          
014400     EJECT                                                                
014500*    ---  DLI INPUT-OUTPUT AREA                                           
014600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ3'.                        
014700 01  DLI-IO-WDQ3.                                                         
014800*    03  -COPY WDQ301                                                     
014900     EJECT                                                                
015000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE401'.                      
015100 01  DLI-IO-WDE401.                                                       
015200*    03  -COPY WDE401                                                     
015300     EJECT                                                                
015400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE411'.                      
015500 01  DLI-IO-WDE411.                                                       
015600*    03  -COPY WDE411                                                     
015700     EJECT                                                                
015710 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF601'.                      
015720 01  DLI-IO-WDF601.                                                       
015730*    03  -COPY WDF601                                                     
015740     EJECT                                                                
015750 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDQ211'.                      
015760 01  DLI-IO-WDQ211.                                                       
015770*    03  -COPY WDQ211                                                     
015780     EJECT                                                                
015800 LINKAGE SECTION.                                                         
015900                                                                          
016000     EJECT                                                                
016100*01  -COPY W0008  -PRE WDQ3-                                              
016200     05  FILLER                  PIC X.                                   
016300     EJECT                                                                
016400*01  -COPY W0008  -PRE WDE4-                                              
016500     05  FILLER                  PIC X.                                   
016600     EJECT                                                                
016610*01  -COPY W0008  -PRE WDF6-                                              
016620     05  FILLER                  PIC X.                                   
016630     EJECT                                                                
016640*01  -COPY W0008  -PRE WDQ2-                                              
016650     05  FILLER                  PIC X.                                   
016660     EJECT                                                                
016700 PROCEDURE DIVISION  USING WDQ3-PCB WDE4-PCB WDF6-PCB WDQ2-PCB.           
016800 MAIN SECTION.                                                            
016900     ENTRY 'DLITCBL' USING WDQ3-PCB WDE4-PCB WDF6-PCB WDQ2-PCB.           
017000                                                                          
017100                                                                          
017200     PERFORM A-INIT                                                       
017300                                                                          
017400     PERFORM IMS-GET-WDQ3                                                 
017500     PERFORM UNTIL SEGMENT-SLUT                                           
017600       EVALUATE WDQ3-SEG-NAME-FB                                          
017700         WHEN 'WDQ301'                                                    
017800           MOVE ODEL-IDDC       TO WS-IDDC                                
017900           IF  (NDC-US OR NDC-CA OR GOOD-DDC OR CDC)                      
018000           AND ODEL-IDLEVNR NOT = SPACE                                   
018100           AND ODEL-KDODELSTA = 'U'                                       
018200               PERFORM B-KOLLA-ORDER                                      
018300           END-IF                                                         
018400       END-EVALUATE                                                       
018500       PERFORM IMS-GET-WDQ3                                               
018600     END-PERFORM                                                          
018700     PERFORM Z-FINIT                                                      
018800                                                                          
018900     MOVE ZERO TO RETURN-CODE                                             
019000     GOBACK                                                               
019100     .                                                                    
019200     EJECT                                                                
019300 A-INIT SECTION.                                                          
019310     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
019400                                                                          
019500     OPEN OUTPUT W46380                                                   
019600                 W46382                                                   
019700                                                                          
019800     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
019900     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
020000     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
020100     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
020200     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020300     PERFORM AA-BERAKNA-TESTDATUM                                         
020400     .                                                                    
020500     EJECT                                                                
020600 AA-BERAKNA-TESTDATUM SECTION.                                            
020610     MOVE 'AA-BER-TESTDATA ' TO CURRENT-SECTION                           
020700                                                                          
020800     MOVE 003 TO DAG-KDCALL.                                              
020900     MOVE DAGENS-DATUM TO DAG-TIAAMMDD-TOM                                
021000     MOVE 20           TO DAG-KVKALDAG                                    
021100     IF   DAGENS-DATUM > 500000                                           
021200         MOVE 19 TO DAG-TISEKEL-TOM                                       
021300     ELSE                                                                 
021400         MOVE 20 TO DAG-TISEKEL-TOM                                       
021500     END-IF                                                               
021600                                                                          
021700     CALL WDAGKONV USING                                                  
021800          DAG-KDCALL,                                                     
021900          DAG-DATUM-AREA,                                                 
022000          DAG-KDSVAR                                                      
022100                                                                          
022200     IF DAG-KDSVAR = SPACE                                                
022300        MOVE DAG-TIAAMMDD-FOM TO WS-TIORDREG-FIL                          
022400     ELSE                                                                 
022500        MOVE RKOD-ABEND-MED-DUMP TO RKOD-ABEND                            
022600        MOVE '*** FEL FRÅN DAGKONV ***' TO FELTEXT-STR                    
022700        PERFORM S99-ABEND                                                 
022800     END-IF                                                               
022900                                                                          
023000     .                                                                    
023100     EJECT                                                                
023200 B-KOLLA-ORDER SECTION.                                                   
023210     MOVE 'B-KOLLA-ORDER   ' TO CURRENT-SECTION                           
023300                                                                          
023400     MOVE ODEL-IDDISTR  TO W-IDDISTR                                      
023500     MOVE ODEL-IDKUNDNR TO W-IDKUNDNR                                     
023600     MOVE ODEL-IDORDNR7 TO W-IDORDNR5                                     
023700     MOVE ODEL-IDPRODNR TO W-IDPRODNR                                     
023710                           W-IDPRODNR-F6                                  
023800     MOVE ODEL-IDPLKLST TO W-IDPLKLST                                     
023801                                                                          
023810     MOVE ODEL-IDORDER  TO W-IDORDER                                      
023830     MOVE ODEL-IDDC     TO W-IDDC                                         
023840     MOVE ODEL-IDLEVNR  TO W-IDLEVNR                                      
023900                                                                          
024000     PERFORM IMS-GU-WDQ211                                                
024001     IF SEGMENT-SAKNAS                                                    
024002        MOVE ZERO       TO DIRL-TISKEPPN-DDC                              
024003     END-IF                                                               
024010                                                                          
024020     PERFORM IMS-GET-WDE4-KORD                                            
024100     PERFORM UNTIL SEGMENT-SAKNAS                                         
024200         PERFORM IMS-GET-WDE4-ORAD                                        
024300         IF  SEGMENT-FINNS                                                
024400*        AND ORAD-KVBEART NOT = ORAD-KVLEVART                             
024500         AND ORAD-KVBEART > ORAD-KVLEVART + ORAD-KVANNANT                 
024600           MOVE ODEL-IDDC         TO WS-IDDC                              
024610           MOVE ODEL-IDLEVNR      TO LEV06-IDLEVNR                        
024700           IF GOOD-DDC OR LEV06-DDGS                                      
025520             PERFORM IMS-GU-WDF601                                        
025530             IF F6-SEGMENT-SAKNAS                                         
025600                MOVE ORAD-IDARTNR      TO UTDDC-IDARTNR                   
025700                MOVE ODEL-IDDC         TO UTDDC-IDDC                      
025800                MOVE ODEL-IDGMTREF     TO UTDDC-IDGMTREF                  
025900                MOVE ODEL-IDLEVNR      TO UTDDC-IDLEVNR                   
026000                MOVE ODEL-IDPRODNR     TO UTDDC-IDPRODNR                  
026100                MOVE ORAD-KDORDKL      TO UTDDC-KDORDKL                   
026200                MOVE ORAD-KVBEART      TO UTDDC-KVBEART                   
026300                MOVE ORAD-KVLEVART     TO UTDDC-KVLEVART                  
026400                MOVE ORAD-TIUTSKR      TO UTDDC-TIUTSKR                   
026500                MOVE ORAD-IDPURAD      TO UTDDC-IDPURAD                   
026600                MOVE ORAD-KDANNULL     TO UTDDC-KDANNULL                  
026700                MOVE ODEL-TIUTSTID     TO UTDDC-TIUTSTID                  
026800                MOVE ODEL-TIREGDAT     TO TMP1-YYMMDD                     
026900                MOVE WS-TIORDREG-FIL   TO TMP2-YYMMDD                     
027000                PERFORM WY2000P1                                          
027100                IF TMP1-YYMMDD <= TMP2-YYMMDD                             
027200                    MOVE JA            TO UTDDC-FLJANEJ                   
027300                ELSE                                                      
027400                    MOVE NEJ           TO UTDDC-FLJANEJ                   
027500                END-IF                                                    
027510                MOVE DIRL-TISKEPPN-DDC TO UTDDC-TISKEPPN-DDC              
027600                PERFORM S12-SKRIV-W46382                                  
027700             END-IF                                                       
027710           END-IF                                                         
027800           IF NDC-US OR NDC-CA                                            
027900             MOVE ODEL-IDDC         TO UT-IDDC                            
028000             MOVE ODEL-IDGMTREF     TO UT-IDGMTREF                        
028100             MOVE ODEL-IDPRODNR     TO UT-IDPRODNR                        
028200             MOVE ORAD-IDARTNR      TO UT-IDARTNR                         
028300             MOVE ORAD-KVAVBART     TO UT-KVBEART                         
028400             MOVE ORAD-KVLEVART     TO UT-KVLEVART                        
028500             MOVE ORAD-TIUTSKR      TO UT-TIUTSKR                         
028600             MOVE ODEL-TIREGDAT     TO TMP1-YYMMDD                        
028700             MOVE WS-TIORDREG-FIL   TO TMP2-YYMMDD                        
028800             PERFORM WY2000P1                                             
028900             IF  TMP1-YYMMDD <= TMP2-YYMMDD                               
029000                 MOVE JA            TO UT-FLJANEJ                         
029100             ELSE                                                         
029200                 MOVE NEJ           TO UT-FLJANEJ                         
029300             END-IF                                                       
029400             IF UT-KVBEART NOT = UT-KVLEVART                              
029500                 PERFORM S11-SKRIV-W46380                                 
029600             END-IF                                                       
029700           END-IF                                                         
029800         END-IF                                                           
029900     END-PERFORM                                                          
030000     .                                                                    
030100     EJECT                                                                
030200 Z-FINIT SECTION.                                                         
030300     CLOSE W46380                                                         
030400           W46382                                                         
030500     SKIP2                                                                
030600     MOVE 'S' TO POSTSUM-OPKOD                                            
030700     CALL POSTSUM USING POSTSUM-PARM                                      
030800     .                                                                    
030900     EJECT                                                                
031000 S11-SKRIV-W46380 SECTION.                                                
031100                                                                          
031200     WRITE UT1-POST FROM UT-AREA                                          
031300                                                                          
031400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
031500     MOVE 'W46380'   TO POSTSUM-FDNAMN                                    
031600     MOVE 'W46380D1' TO POSTSUM-DDNAMN2                                   
031700     CALL POSTSUM USING POSTSUM-PARM                                      
031800     .                                                                    
031900     EJECT                                                                
032000 S12-SKRIV-W46382 SECTION.                                                
032100                                                                          
032200     WRITE UT2-POST FROM UTDDC-AREA                                       
032300                                                                          
032400     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
032500     MOVE 'W46382'   TO POSTSUM-FDNAMN                                    
032600     MOVE 'W46380D2' TO POSTSUM-DDNAMN2                                   
032700     CALL POSTSUM USING POSTSUM-PARM                                      
032800     .                                                                    
032900     EJECT                                                                
033000 S99-ABEND SECTION.                                                       
033100                                                                          
033200     SKIP2                                                                
033300     MOVE 'S' TO POSTSUM-OPKOD                                            
033400     CALL POSTSUM USING POSTSUM-PARM                                      
033500     CALL ABEND USING RKOD-ABEND                                          
033600     .                                                                    
033700     EJECT                                                                
033800* --- IMS SEKTIONER ---                                                   
033900     SKIP3                                                                
034000     EJECT                                                                
034100 IMS-GET-WDQ3   SECTION.                                                  
034110     MOVE 'IMS-GET-WDQ3    ' TO CURRENT-IMS-SECTION                       
034200                                                                          
034300     CALL CBLTDLI USING GN WDQ3-PCB DLI-IO-WDQ3                           
034400     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
034500     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
034600     PERFORM IMS-STATUSKONTROLL                                           
034700     .                                                                    
034800     EJECT                                                                
034900 IMS-GET-WDE4-KORD SECTION.                                               
034910     MOVE 'IMS-GET-WDE4KORD' TO CURRENT-IMS-SECTION                       
035000                                                                          
035100     STRING 'WDE401  (WDE401KY =' W-WDE4KEY-X ')'                         
035200          DELIMITED BY SIZE INTO SSA1                                     
035300     MOVE '  GE' TO GODK-STATUSKODER                                      
035400     CALL CBLTDLI USING GU WDE4-PCB DLI-IO-WDE401 SSA1                    
035500     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
035600     PERFORM IMS-STATUSKONTROLL                                           
035700     .                                                                    
035800     EJECT                                                                
035900 IMS-GET-WDE4-ORAD SECTION.                                               
035910     MOVE 'IMS-GET-WDE4ORAD' TO CURRENT-IMS-SECTION                       
036000                                                                          
036100     MOVE 'WDE411  ' TO SSA1                                              
036200     MOVE '  GE' TO GODK-STATUSKODER                                      
036300     CALL CBLTDLI USING GNP WDE4-PCB DLI-IO-WDE411 SSA1                   
036400     MOVE WDE4-STATUS-CODE TO STATUS-WS                                   
036500     PERFORM IMS-STATUSKONTROLL                                           
036600     .                                                                    
036700     EJECT                                                                
036710 IMS-GU-WDF601    SECTION.                                                
036720                                                                          
036721     MOVE 'IMS-GU-WDF601   ' TO CURRENT-IMS-SECTION                       
036730     STRING 'WDF601  (IDPRODNR =' W-IDPRODNR-F6-X ')'                     
036740          DELIMITED BY SIZE INTO SSA1                                     
036750     MOVE '  GE' TO GODK-STATUSKODER                                      
036760     CALL CBLTDLI USING GU WDF6-PCB DLI-IO-WDF601 SSA1                    
036770     MOVE WDF6-STATUS-CODE TO F6-STATUS-WS                                
036780     PERFORM IMS-STATUSKONTROLL-F6                                        
036790     .                                                                    
036791     EJECT                                                                
036792 IMS-GU-WDQ211    SECTION.                                                
036793                                                                          
036794     MOVE 'IMS-GU-WDQ211   ' TO CURRENT-IMS-SECTION                       
036795     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
036796          DELIMITED BY SIZE INTO SSA1                                     
036797     STRING 'WDQ211  (WDQ211KY =' W-WDQ211KY-X ')'                        
036798          DELIMITED BY SIZE INTO SSA2                                     
036799     MOVE '  GE' TO GODK-STATUSKODER                                      
036800     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-WDQ211 SSA1 SSA2               
036801     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
036802     PERFORM IMS-STATUSKONTROLL                                           
036803     .                                                                    
036804     EJECT                                                                
036810 IMS-STATUSKONTROLL SECTION.                                              
036900                                                                          
037000     SET STATUS-IX TO 1                                                   
037100     SEARCH GODK-STATUS                                                   
037200       AT END                                                             
037300         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
037400           DELIMITED BY SIZE INTO FELTEXT                                 
037500         DISPLAY FELTEXT                                                  
037600         CALL FELLOG                                                      
037700       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
037800         CONTINUE                                                         
037900     END-SEARCH                                                           
038000     .                                                                    
038100     EJECT                                                                
038110 IMS-STATUSKONTROLL-F6 SECTION.                                           
038120                                                                          
038130     SET STATUS-IX TO 1                                                   
038140     SEARCH GODK-STATUS                                                   
038150       AT END                                                             
038160         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' F6-STATUS-WS             
038170           DELIMITED BY SIZE INTO FELTEXT                                 
038180         DISPLAY FELTEXT                                                  
038190         CALL FELLOG                                                      
038191       WHEN GODK-STATUS (STATUS-IX) = F6-STATUS-WS                        
038192         CONTINUE                                                         
038193     END-SEARCH                                                           
038194     .                                                                    
038195     EJECT                                                                
038200*    -COPY WY2000P1                                                       
