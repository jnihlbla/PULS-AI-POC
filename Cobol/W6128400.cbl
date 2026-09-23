000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W6128400.                                                
000300 AUTHOR.         JOHAN LINDKVIST                                          
000400 DATE-WRITTEN.   97/11/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        LÄSER WD06-DATABASEN PERIODVIS FÖR ATT SAMLA SDC-HISTORIK        
000900*        OM ARTIKLAR TILL USA/CAN MED VISS IDDISTR.                       
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
001200*        PROGRAMMET LÄSER      WLINLC (WDL6)                              
001300*                                                                         
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200*          --- R32 UNDER PERIODEN INKL TRANSF                             
002300     SELECT W61284                    ASSIGN TO W61284D1.                 
002400     SKIP2                                                                
002500 DATA DIVISION.                                                           
002600     SKIP2                                                                
002700 FILE SECTION.                                                            
002800 FD  W61284                                                               
002900     RECORDING       F                                                    
003000     BLOCK CONTAINS  0.                                                   
003100                                                                          
003200*01  POST -COPY W61265  -PRE  UT-  -L.                                    
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500                                                                          
003600     SKIP3                                                                
003700*    -COPY WY2000W1                                                       
003800     SKIP3                                                                
003900 77  IDPGM                       PIC X(8)    VALUE 'W6128400'.            
004000 77  JA                          PIC X       VALUE 'J'.                   
004100 77  NEJ                         PIC X       VALUE 'N'.                   
004200                                                                          
004300 01  WS-GRANS-DATUM              PIC S9(7)   COMP-3.                      
004400                                                                          
004500 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
004600 01  FILLER REDEFINES DAGENS-DATUM.                                       
004700     03  DAGENS-DATUM-AAR        PIC 9(2).                                
004800     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
004900     03  DAGENS-DATUM-DAG        PIC 9(2).                                
005000     SKIP2                                                                
005100                                                                          
005200 01  DATE-HANDLING.                                                       
005300     03  WS-FIRST-TIAAVV         PIC 9(4) VALUE ZERO.                     
005400     03  WS-TIAARP               PIC 9(4) VALUE ZERO.                     
005500     SKIP2                                                                
005600                                                                          
005700                                                                          
005800 01  SPAR-IDDC                   PIC X(2)    VALUE SPACE.                 
005900                                                                          
006000 01  SPAR-IDARTNR                PIC 9(8)    VALUE ZERO.                  
006100*01  TESTAREA -COPY WWDIST35                                              
006200                                                                          
006300     EJECT                                                                
006400*      --- VALID IDDC CODES                                               
006500*                                                                         
006600 01  DYNAMISKA-SUBPROGRAM.                                                
006700*                                                                         
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
007400     SKIP2                                                                
007500*    --- PARAMETRAR TILL ABEND                                            
007600                                                                          
007700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008000     SKIP2                                                                
008100 01  FELTEXT.                                                             
008200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008400     EJECT                                                                
008500*    --- PARAMETRAR TILL DATKORT                                          
008600*                                                                         
008700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W61284'.              
008800     SKIP2                                                                
008900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
009000     SKIP2                                                                
009100*01  -COPY WDATKORT                                                       
009200     EJECT                                                                
009300*    --- PARAMETRAR TILL POSTSUM                                          
009400*                                                                         
009500*01  -COPY W0005   -PRE  POSTSUM-                                         
009600     EJECT                                                                
009700*    --- PARAMETRAR TILL WDATKONV                                         
009800*                                                                         
009900*01  -COPY WDATAREA                                                       
010000     EJECT                                                                
010100 01  W61284-AREA-START           PIC X(24)   VALUE                        
010200                                 'W61284-AREA-START  '.                   
010300     SKIP2                                                                
010400                                                                          
010500*01  AREA -COPY W61265     -PRE WUT-                                      
010600     EJECT                                                                
010700*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
010800*                                                                         
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011100     SKIP3                                                                
011200 01  NYCKLAR-TILL-DLI.                                                    
011300     03  W-IDARTNR-X.                                                     
011400         05  W-IDARTNR           PIC S9(8)   VALUE ZERO COMP-3.           
011500     03  W-WDL612KY-X.                                                    
011600         05  W-DAREGDAT          PIC 9(8)    VALUE ZERO.                  
011700         05  W-TIREGTID          PIC S9(7)   VALUE ZERO COMP-3.           
011800     03  W-IDDC-B6-X.                                                     
011900         05  W-IDDC-B6           PIC X(2)   VALUE SPACE.                  
012000     SKIP2                                                                
012100*    --- STATUS-KOD FRÅN IMS                                              
012200 01  STATUS-WS                   PIC XX.                                  
012300     88  SEGMENT-FINNS                       VALUE '  '.                  
012400     88  SEGMENT-SAKNAS                      VALUE 'GB'.                  
012500     SKIP2                                                                
012600 01  GODK-STATUSKODER.                                                    
012700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800     SKIP3                                                                
012900 01  SSA1                        PIC X(64).                               
013000 01  SSA2                        PIC X(64).                               
013100     EJECT                                                                
013200*    --- IMS FUNKTIONSKODER                                               
013300*01  -COPY W0003                                                          
013400     EJECT                                                                
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLINLC01'.                    
013700                                                                          
013800 01 DLI-IO-AREA.                                                          
013900     03 IO-AREA     PIC X(600) VALUE SPACE.                               
014000         03 DLI-IO-WLINLC01 REDEFINES IO-AREA.                            
014100*            05 -COPY WDL601 -PRE INLC-                                   
014200     EJECT                                                                
014300         03 DLI-IO-WLINLC11 REDEFINES IO-AREA.                            
014400*            05 -COPY WDL611 -PRE INLC-                                   
014500     EJECT                                                                
014600         03 DLI-IO-WLINLC12 REDEFINES IO-AREA.                            
014700             05 -COPY WDL612 -PRE INLC-                                   
014800     EJECT                                                                
014900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
015000 01   DLI-IO-AREA-B601.                                                   
015100*     03  -COPY WDB601                                                    
015200     EJECT                                                                
015300 LINKAGE SECTION.                                                         
015400                                                                          
015500     EJECT                                                                
015600*01  -COPY W0008  -PRE INLC-                                              
015700     05  FILLER                  PIC X.                                   
015800     EJECT                                                                
015900*01  -COPY W0008      -PRE WDB6-                                          
016000     05  FILLER                  PIC X.                                   
016100     EJECT                                                                
016200     EJECT                                                                
016300 PROCEDURE DIVISION  USING INLC-PCB WDB6-PCB.                             
016400 MAIN SECTION.                                                            
016500     ENTRY 'DLITCBL' USING INLC-PCB WDB6-PCB.                             
016600                                                                          
016700     PERFORM A-INIT                                                       
016800     PERFORM IMS-GET-INLC                                                 
016900     PERFORM UNTIL SEGMENT-SAKNAS                                         
017000       EVALUATE INLC-SEG-NAME-FB                                          
017100       WHEN 'WDL601  '                                                    
017200         MOVE INLC-ART-IDARTNR TO SPAR-IDARTNR                            
017300       WHEN 'WDL611  '                                                    
017400                                                                          
017500         IF INLC-INL-IDDC NOT = SPAR-IDDC                                 
017600           MOVE INLC-INL-IDDC TO W-IDDC-B6                                
017700                                 SPAR-IDDC                                
017800           PERFORM IMS-GU-WDB601                                          
017900         END-IF                                                           
018000                                                                          
018100         IF DCS-FLINLHIST = JA                                            
018200                                                                          
018300           MOVE INLC-INL-IDDISTR TO DIST35-IDDISTR                        
018400                                                                          
018500* IGNORERA FÖRLORADE KOLLIN (DVS. TINILINL SATT TILL NOLL)                
018600* MEN ALLA 310-POSTER TAS MED FÖR ATT BERÄKNA ANTAL NOT-YET-BINNED        
018700           IF (INLC-INL-IDPTYP = 'R32'                                    
018800               AND INLC-INL-TIINLINL > ZERO)                              
018900           OR  INLC-INL-IDPTYP = '310'                                    
019000                                                                          
019100                IF DIST35-REFILL-NA                                       
019200                OR DIST35-REFILL-NA-JAP                                   
019301                OR DIST35-REFILL-JP                                       
019302                OR DIST35-CDC-AU-REFILL                                   
019310                OR DIST35-CDC-NONVCC-REFILL                               
019320                OR DIST35-VCC-NONVCC-REFILL                               
019500                OR DIST35-NA-TRANSFER                                     
019510                OR DIST35-REFILL-INOM-NDC                                 
019511                OR DIST35-NONVCC-NONVCC-REFILL                            
019520                OR DIST35-NA-NDC-RETURNS                                  
019600                OR DIST35-PACIFIC-TRANSFER                                
019610                OR DIST35-REFILL-INOM-JP                                  
019700                OR DIST35-CDC-LDC-REFILL                                  
019800                OR DIST35-CDC-1C-REFILL                                   
019900                OR DIST35-CDC-GB-3A-REFILL                                
020000                OR DIST35-CDC-ES-REFILL                                   
020100                OR DIST35-CDC-IT-REFILL                                   
020200                OR DIST35-CDC-NL-REFILL                                   
020300                OR DIST35-CDC-AT-REFILL                                   
020600                OR DIST35-CN-TRANSFER                                     
020700                OR DIST35-NONVCC-CDC-REFILL                               
020710                OR DIST35-NONVCC-VCC-REFILL                               
019320                OR DIST35-VCC-NONVCC-TRANSFER                             
019511                OR DIST35-NONVCC-NONVCC-TRANSFER                          
020710                OR DIST35-NONVCC-VCC-TRANSFER                             
020800                    PERFORM B-SKAPA-PERIODENS                             
020900                END-IF                                                    
021000             END-IF                                                       
021100         END-IF                                                           
021200                                                                          
021300       WHEN OTHER                                                         
021400         CONTINUE                                                         
021500       END-EVALUATE                                                       
021600                                                                          
021700       PERFORM IMS-GET-INLC                                               
021800     END-PERFORM                                                          
021900                                                                          
022000     PERFORM Z-FINIT                                                      
022100                                                                          
022200     MOVE ZERO TO RETURN-CODE                                             
022300     GOBACK                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 A-INIT SECTION.                                                          
022700                                                                          
022800     OPEN OUTPUT W61284                                                   
022900                                                                          
023000     MOVE IDPGM       TO POSTSUM-PROGNAMN                                 
023100                                                                          
023200     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
023300     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
023400     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
023500     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
023600                                                                          
023700     MOVE DAGENS-DATUM      TO DAT-I-TIDATUM                              
023800     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
023900     CALL WDATKONV       USING DAT-KDDATFORM                              
024000                               DAT-I-TIDATUM                              
024100                               DAT-O-TIDATUM                              
024200                               DAT-KDSVAR                                 
024300     IF DAT-KDSVAR-OK                                                     
024400        MOVE DAT-TIAARP     TO WS-TIAARP                                  
024500     ELSE                                                                 
024600        DISPLAY 'FEL FRÅN DATKONV (1) (A-INIT)'                           
024700        CALL ABEND       USING RKOD-ABEND-UTAN-DUMP                       
024800     END-IF                                                               
024900                                                                          
025000     MOVE 'AARP  '          TO DAT-KDDATFORM                              
025100     MOVE WS-TIAARP         TO DAT-I-TIDATUM                              
025200                                                                          
025300     CALL WDATKONV       USING DAT-KDDATFORM                              
025400                               DAT-I-TIDATUM                              
025500                               DAT-O-TIDATUM                              
025600                               DAT-KDSVAR                                 
025700                                                                          
025800     IF DAT-KDSVAR-OK                                                     
025900        MOVE DAT-TIAAMMDD   TO WS-GRANS-DATUM                             
026000        DISPLAY '*** DATUM : ' WS-GRANS-DATUM                             
026100     ELSE                                                                 
026200        DISPLAY 'FEL FRÅN DATKONV (2) (A-INIT)'                           
026300        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
026400     END-IF                                                               
026500                                                                          
026600     .                                                                    
026700     EJECT                                                                
026800                                                                          
026900 B-SKAPA-PERIODENS SECTION.                                               
027000                                                                          
027100     IF INLC-INL-IDPTYP = 'R32'                                           
027200       MOVE INLC-INL-TIINLINL   TO TMP1-YYMMDD                            
027300       MOVE WS-GRANS-DATUM      TO TMP2-YYMMDD                            
027400       PERFORM WY2000P1                                                   
027500       IF TMP1-YYMMDD >= TMP2-YYMMDD                                      
027600       AND INLC-INL-KVANTMOT > 0                                          
027700         PERFORM BA-SKRIV-PERIODENS                                       
027800       END-IF                                                             
027900     ELSE                                                                 
028000*      -- 310:OR                                                          
028100       PERFORM BA-SKRIV-PERIODENS                                         
028200     END-IF                                                               
028300                                                                          
028400     .                                                                    
028500     EJECT                                                                
028600 BA-SKRIV-PERIODENS SECTION.                                              
028700                                                                          
028800     MOVE INLC-INL-IDPTYP         TO WUT-IDPTYP                           
028900     MOVE INLC-INL-IDDC           TO WUT-IDDC                             
029000     MOVE INLC-INL-IDDISTR        TO WUT-IDDISTR                          
029100     MOVE INLC-INL-FLPRIO         TO WUT-FLPRIO                           
029200     MOVE INLC-INL-KDFRAKT        TO WUT-KDFRAKT                          
029300     MOVE SPAR-IDARTNR            TO WUT-IDARTNR                          
029400     MOVE INLC-INL-PRARTNTO       TO WUT-PRARTNTO                         
029500     MOVE INLC-INL-PRKURS         TO WUT-PRKURS                           
029600     MOVE INLC-INL-KDVALISO       TO WUT-KDVALISO                         
029700     MOVE INLC-INL-KVANTMOT       TO WUT-KVANTMOT                         
029800     MOVE INLC-INL-TIINLMOT       TO WUT-TIINLMOT                         
029900     MOVE INLC-INL-TIINLMTI       TO WUT-TIINLMTI                         
030000     MOVE INLC-INL-TIINLINL       TO WUT-TIINLINL                         
030100     MOVE INLC-INL-TIINLITI       TO WUT-TIINLITI                         
030200                                                                          
030300     PERFORM S11-SKRIV-W61284                                             
030400     .                                                                    
030500     EJECT                                                                
030600 Z-FINIT SECTION.                                                         
030700     CLOSE W61284                                                         
030800     SKIP2                                                                
030900     MOVE 'S' TO POSTSUM-OPKOD                                            
031000     CALL POSTSUM USING POSTSUM-PARM                                      
031100     .                                                                    
031200     EJECT                                                                
031300 S11-SKRIV-W61284 SECTION.                                                
031400                                                                          
031500     WRITE UT-POST FROM WUT-AREA                                          
031600                                                                          
031700     MOVE INLC-INL-IDPTYP TO POSTSUM-TRANSTYP                             
031800     MOVE 'W61284' TO POSTSUM-FDNAMN                                      
031900     MOVE 'W61284D1' TO POSTSUM-DDNAMN2                                   
032000     CALL POSTSUM USING POSTSUM-PARM                                      
032100     .                                                                    
032200     EJECT                                                                
032300* --- IMS SEKTIONER ---                                                   
032400     SKIP3                                                                
032500     EJECT                                                                
032600 IMS-GET-INLC SECTION.                                                    
032700                                                                          
032800     CALL CBLTDLI USING GN INLC-PCB DLI-IO-AREA                           
032900     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
033000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
033100     PERFORM IMS-STATUSKONTROLL                                           
033200     .                                                                    
033300     EJECT                                                                
033400                                                                          
033500 IMS-GU-WDB601    SECTION.                                                
033600     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
033700          DELIMITED BY SIZE INTO SSA1                                     
033800     MOVE '  ' TO GODK-STATUSKODER                                        
033900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
034000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
034100     PERFORM IMS-STATUSKONTROLL                                           
034200     .                                                                    
034300     EJECT                                                                
034400 IMS-STATUSKONTROLL SECTION.                                              
034500                                                                          
034600     SET STATUS-IX TO 1                                                   
034700     SEARCH GODK-STATUS                                                   
034800       AT END                                                             
034900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
035000           DELIMITED BY SIZE INTO FELTEXT                                 
035100         DISPLAY FELTEXT                                                  
035200         CALL FELLOG                                                      
035300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
035400         CONTINUE                                                         
035500     END-SEARCH                                                           
035600     .                                                                    
035700     EJECT                                                                
035800     EJECT                                                                
035900*    -COPY WY2000P1                                                       
