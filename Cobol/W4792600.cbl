000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4792600.                                                
000300 AUTHOR.         STEFAN KIHLBERG.                                         
000400 DATE-WRITTEN.   98/10/17.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        VÄLJER UT ORDRAR SOM SKALL TAS BORT FRÅN WDQ5, SKRIVER           
000900*        FIL                                                              
001000*                                                                         
001100*        PROGRAMMET LÄSER      WLORQP (WDQ5)                              
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- ORDRAR SOM SKALL RENSAS FRÅN WDQ5                          
002600     SELECT W47926                     ASSIGN TO W47926D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W47926                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  POST -COPY W47926 -PRE  UT-  -L.                                     
003700     EJECT                                                                
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000*    -COPY WY2000W1                                                       
004100     SKIP3                                                                
004200 77  IDPGM                       PIC X(8)    VALUE 'W4792600'.            
004300 77  JA                          PIC X       VALUE 'J'.                   
004400 77  NEJ                         PIC X       VALUE 'N'.                   
004500                                                                          
004600 01  FILLER                      PIC X(16)   VALUE 'ARBETSFALT'.          
004700 01  ARBETSFALT.                                                          
004800                                                                          
004900     03 WS-SISTA-TIUPPDAT        PIC 9(6)    VALUE ZERO.                  
005400                                                                          
005401     03 TIDAT-DEL-TIREGDAT-US    PIC 9(6)    VALUE ZERO.                  
005402     03 TIDAT-DEL-TIREGDAT-CA    PIC 9(6)    VALUE ZERO.                  
005403     03 TIDAT-DEL-TIUPPDAT-US    PIC 9(6)    VALUE ZERO.                  
005404     03 TIDAT-DEL-TIUPPDAT-CA    PIC 9(6)    VALUE ZERO.                  
005408                                                                          
005410     03 KVDAGAR-SPAR-TIREGDAT-US PIC 9(2)    VALUE 14.                    
005411     03 KVDAGAR-SPAR-TIREGDAT-CA PIC 9(2)    VALUE 14.                    
005412     03 KVDAGAR-SPAR-TIUPPDAT-US PIC 9(2)    VALUE 06.                    
005413     03 KVDAGAR-SPAR-TIUPPDAT-CA PIC 9(2)    VALUE 06.                    
005450                                                                          
005500                                                                          
005600                                                                          
005700 01  FILLER         PIC X(16) VALUE 'SPAR-HUVUD   '.                      
005800 01  WS-SPARAT-ORDERHUVUD.                                                
005900*    03  -COPY WDQ501 -PRE SPAR-HUV-                                      
006000     EJECT                                                                
006100                                                                          
006200                                                                          
006300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006400 01  FILLER REDEFINES DAGENS-DATUM.                                       
006500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006800     EJECT                                                                
006900*      --- VALID IDDC CODES                                               
007000*                                                                         
007100*01    -COPY WWDC99                                                       
007100*01    -COPY WWDCKONS                                                     
007200       EJECT                                                              
007400 01  FILLER                      PIC X(24)   VALUE 'SWITCHAR'.            
007500                                                                          
007600 77  FIRST-POST-SW               PIC X   VALUE 'J'.                       
007700     88  FIRST-POST                      VALUE 'J'.                       
007800     88  NOT-FIRST-POST                  VALUE 'N'.                       
007900                                                                          
008000 77  BORTTAG-SW                  PIC X   VALUE 'N'.                       
008100     88  TAG-BORT                        VALUE 'J'.                       
008200     88  SPARA                           VALUE 'N'.                       
008300     EJECT                                                                
008400                                                                          
008500 01  FILLER                      PIC X(24)   VALUE 'DYNAM SUBPGM'.        
008600 01  DYNAMISKA-SUBPROGRAM.                                                
008700*                                                                         
008800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009200     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
009300     SKIP2                                                                
009400*    --- PARAMETRAR TILL ABEND                                            
009500                                                                          
009600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009700 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009800 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009900     SKIP2                                                                
010000 01  FELTEXT.                                                             
010100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010300     EJECT                                                                
010400*    --- PARAMETRAR TILL POSTSUM                                          
010500*                                                                         
010600*01  -COPY W0005   -PRE  POSTSUM-                                         
010700     EJECT                                                                
010800*01  -COPY WORKAREA                                                       
010900     EJECT                                                                
011000 01  UT-AREA-START               PIC X(24)   VALUE                        
011100                                 'UT-AREA-START  '.                       
011200     SKIP2                                                                
011300                                                                          
011400*01  AREA -COPY W47926     -PRE UT-                                       
011500     EJECT                                                                
011600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011700*                                                                         
011800     EJECT                                                                
011900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012000     SKIP3                                                                
012100 01  NYCKLAR-TILL-DLI.                                                    
012200     03  W-WDQ501KY-X.                                                    
012300         05  W-WDQ501KY          PIC X(17)    VALUE SPACE.                
012400     SKIP2                                                                
012500*    --- STATUS-KOD FRÅN IMS                                              
012600 01  STATUS-WS                   PIC XX.                                  
012700     88  SEGMENT-FINNS                       VALUE '  '.                  
012800     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012900     SKIP2                                                                
013000 01  GODK-STATUSKODER.                                                    
013100     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013200     SKIP3                                                                
013300 01  SSA1                        PIC X(64).                               
013400 01  SSA2                        PIC X(64).                               
013500     EJECT                                                                
013600*    --- IMS FUNKTIONSKODER                                               
013700*01  -COPY W0003                                                          
013800     EJECT                                                                
013900*    ---  DLI INPUT-OUTPUT AREA                                           
014000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLORQP'.                      
014100 01  DLI-IO-WLORQP.                                                       
014200*    03  -COPY WDQ501                                                     
014300     EJECT                                                                
014400 LINKAGE SECTION.                                                         
014500                                                                          
014600                                                                          
014700*01  -COPY W0008  -PRE ORQP-                                              
014800     05  FILLER                  PIC X.                                   
014900     EJECT                                                                
015000 PROCEDURE DIVISION  USING ORQP-PCB.                                      
015100 MAIN SECTION.                                                            
015200     ENTRY 'DLITCBL' USING ORQP-PCB.                                      
015300                                                                          
015400                                                                          
015500     PERFORM A-INIT                                                       
015600                                                                          
015700     PERFORM IMS-GET-ORQP                                                 
015800     PERFORM UNTIL SEGMENT-SLUT                                           
015900        EVALUATE ORQP-SEG-NAME-FB                                         
016000         WHEN 'WDQ501'                                                    
016100           IF RADB-IDARTNR = ZERO                                         
016200              IF FIRST-POST                                               
016300                 MOVE NEJ                TO FIRST-POST-SW                 
016400              ELSE                                                        
016500                 PERFORM B-BESLUT-OM-BORTTAG-AV-ORDER                     
016600              END-IF                                                      
016700              MOVE RADB-WDQ501 TO SPAR-HUV-RADB-WDQ501                    
016800              MOVE ZERO        TO WS-SISTA-TIUPPDAT                       
016900              MOVE NEJ         TO BORTTAG-SW                              
017000           ELSE                                                           
017100              MOVE RADB-TIUPPDAT       TO TMP1-YYMMDD                     
017200              MOVE WS-SISTA-TIUPPDAT   TO TMP2-YYMMDD                     
017300              PERFORM WY2000P1                                            
017400              IF TMP1-YYMMDD > TMP2-YYMMDD                                
017500                 MOVE RADB-TIUPPDAT  TO                                   
017600                              WS-SISTA-TIUPPDAT                           
017700              END-IF                                                      
017800           END-IF                                                         
017900        END-EVALUATE                                                      
018000        PERFORM IMS-GET-ORQP                                              
018100     END-PERFORM                                                          
018101                                                                          
018110     PERFORM B-BESLUT-OM-BORTTAG-AV-ORDER                                 
018120                                                                          
018200     PERFORM Z-FINIT                                                      
018300                                                                          
018400     MOVE ZERO TO RETURN-CODE                                             
018500     GOBACK                                                               
018600     .                                                                    
018700     EJECT                                                                
018800                                                                          
018900                                                                          
019000 A-INIT SECTION.                                                          
019100                                                                          
019200     OPEN OUTPUT W47926                                                   
019300                                                                          
019400     ACCEPT DAGENS-DATUM  FROM DATE                                       
019600                                                                          
019700     MOVE 003                       TO WORK-KDCALL                        
019800     MOVE WC-NDC-US-RU              TO WORK-IDDC                          
019900     MOVE KVDAGAR-SPAR-TIREGDAT-US  TO WORK-KVWORKD                       
020000     MOVE DAGENS-DATUM              TO WORK-TIAAMMDD-TOM                  
020100     CALL WORKDAY                   USING WORK-KDCALL                     
020200                                         WORK-DATE-AREA                   
020300                                         WORK-KDSVAR                      
020400     IF WORK-KDSVAR-OK                                                    
020500        MOVE WORK-TIAAMMDD-FOM      TO TIDAT-DEL-TIREGDAT-US              
020600     ELSE                                                                 
020700       DISPLAY 'FEL FRÅN WORKDAY 1 CA SECTION.'                           
020800       CALL FELLOG                                                        
020900     END-IF                                                               
021000                                                                          
021050                                                                          
021100     MOVE 003                       TO WORK-KDCALL                        
021200     MOVE WC-NDC-US-RU              TO WORK-IDDC                          
021300     MOVE KVDAGAR-SPAR-TIUPPDAT-US  TO WORK-KVWORKD                       
021400     MOVE DAGENS-DATUM              TO WORK-TIAAMMDD-TOM                  
021500     CALL WORKDAY                   USING WORK-KDCALL                     
021600                                          WORK-DATE-AREA                  
021700                                          WORK-KDSVAR                     
021800     IF WORK-KDSVAR-OK                                                    
021900        MOVE WORK-TIAAMMDD-FOM      TO TIDAT-DEL-TIUPPDAT-US              
022000     ELSE                                                                 
022100       DISPLAY 'FEL FRÅN WORKDAY 2 CA SECTION.'                           
022200       CALL FELLOG                                                        
022300     END-IF                                                               
022400                                                                          
022500                                                                          
022600     MOVE 003                      TO WORK-KDCALL                         
022700     MOVE WC-NDC-CA                TO WORK-IDDC                           
022800     MOVE KVDAGAR-SPAR-TIREGDAT-CA TO WORK-KVWORKD                        
022900     MOVE DAGENS-DATUM             TO WORK-TIAAMMDD-TOM                   
023000     CALL WORKDAY                  USING WORK-KDCALL                      
023100                                         WORK-DATE-AREA                   
023200                                         WORK-KDSVAR                      
023300     IF WORK-KDSVAR-OK                                                    
023400        MOVE WORK-TIAAMMDD-FOM  TO TIDAT-DEL-TIREGDAT-CA                  
023500     ELSE                                                                 
023600       DISPLAY 'FEL FRÅN WORKDAY 3 A SECTION.'                            
023700       CALL FELLOG                                                        
023800     END-IF                                                               
023900                                                                          
024000     MOVE 003                      TO WORK-KDCALL                         
024100     MOVE WC-NDC-CA                TO WORK-IDDC                           
024200     MOVE KVDAGAR-SPAR-TIUPPDAT-CA TO WORK-KVWORKD                        
024300     MOVE DAGENS-DATUM            TO WORK-TIAAMMDD-TOM                    
024400     CALL WORKDAY                  USING WORK-KDCALL                      
024500                                         WORK-DATE-AREA                   
024600                                         WORK-KDSVAR                      
024700     IF WORK-KDSVAR-OK                                                    
024800        MOVE WORK-TIAAMMDD-FOM  TO TIDAT-DEL-TIUPPDAT-CA                  
024900     ELSE                                                                 
025000       DISPLAY 'FEL FRÅN WORKDAY 4 CA SECTION.'                           
025100       CALL FELLOG                                                        
025200     END-IF                                                               
025201                                                                          
025250     DISPLAY 'SISTA SPARDAG REGDAT US =' TIDAT-DEL-TIREGDAT-US            
025251     DISPLAY 'SISTA SPARDAG REGDAT CA =' TIDAT-DEL-TIREGDAT-CA            
025252     DISPLAY 'SISTA SPARDAG UPPDAT US =' TIDAT-DEL-TIUPPDAT-US            
025253     DISPLAY 'SISTA SPARDAG UPPDAT CA =' TIDAT-DEL-TIUPPDAT-CA            
025290                                                                          
025291                                                                          
025292     DISPLAY 'ANTAL SPARDAGAR REGDAT US ='                                
025293                                 KVDAGAR-SPAR-TIREGDAT-US                 
025294     DISPLAY 'ANTAL SPARDAGAR REGDAT CA ='                                
025295                                 KVDAGAR-SPAR-TIREGDAT-CA                 
025296     DISPLAY 'ANTAL SPARDAGAR UPPDAT US ='                                
025297                                 KVDAGAR-SPAR-TIUPPDAT-US                 
025298     DISPLAY 'ANTAL SPARDAGAR UPPDAT CA ='                                
025299                                 KVDAGAR-SPAR-TIUPPDAT-CA                 
025310     .                                                                    
025400     EJECT                                                                
025500                                                                          
025600                                                                          
025700 B-BESLUT-OM-BORTTAG-AV-ORDER SECTION.                                    
025800                                                                          
025840                                                                          
025900     MOVE SPAR-HUV-RADB-IDDC  TO WS-IDDC                                  
026000     IF NDC-US                                                            
026100       IF WS-SISTA-TIUPPDAT > ZERO                                        
026200          MOVE WS-SISTA-TIUPPDAT        TO TMP1-YYMMDD                    
026300          MOVE TIDAT-DEL-TIUPPDAT-US    TO TMP2-YYMMDD                    
026400          PERFORM WY2000P1                                                
026500          IF TMP1-YYMMDD < TMP2-YYMMDD                                    
026600             MOVE JA          TO BORTTAG-SW                               
026700          END-IF                                                          
026800       ELSE                                                               
026810          IF SPAR-HUV-RADB-TIREGDAT > ZERO                                
026900             MOVE SPAR-HUV-RADB-TIREGDAT   TO TMP1-YYMMDD                 
027010             MOVE TIDAT-DEL-TIREGDAT-US    TO TMP2-YYMMDD                 
027100             PERFORM WY2000P1                                             
027200             IF TMP1-YYMMDD < TMP2-YYMMDD                                 
027300                MOVE JA          TO BORTTAG-SW                            
027400             END-IF                                                       
027410          END-IF                                                          
027500       END-IF                                                             
027600     ELSE                                                                 
027700       IF NDC-CA                                                          
027800          IF WS-SISTA-TIUPPDAT > ZERO                                     
027900             MOVE WS-SISTA-TIUPPDAT        TO TMP1-YYMMDD                 
028010             MOVE TIDAT-DEL-TIUPPDAT-CA    TO TMP2-YYMMDD                 
028100             PERFORM WY2000P1                                             
028200             IF TMP1-YYMMDD < TMP2-YYMMDD                                 
028300                MOVE JA          TO BORTTAG-SW                            
028400             END-IF                                                       
028500          ELSE                                                            
028510             IF SPAR-HUV-RADB-TIREGDAT > ZERO                             
028600                MOVE SPAR-HUV-RADB-TIREGDAT TO TMP1-YYMMDD                
028710                MOVE TIDAT-DEL-TIREGDAT-CA    TO TMP2-YYMMDD              
028800                PERFORM WY2000P1                                          
028900                IF TMP1-YYMMDD < TMP2-YYMMDD                              
029000                   MOVE JA          TO BORTTAG-SW                         
029100                END-IF                                                    
029200             END-IF                                                       
029210          END-IF                                                          
029300       END-IF                                                             
029400     END-IF                                                               
029500                                                                          
029600     IF TAG-BORT                                                          
029700        MOVE SPAR-HUV-RADB-IDORDER TO UT-IDORDER                          
029800        PERFORM S11-SKRIV-W47926                                          
029900     END-IF                                                               
030000     .                                                                    
030100     EJECT                                                                
030200                                                                          
030300                                                                          
030400 Z-FINIT SECTION.                                                         
030500     CLOSE W47926                                                         
030600     SKIP2                                                                
030700     MOVE 'S' TO POSTSUM-OPKOD                                            
030800     CALL POSTSUM USING POSTSUM-PARM                                      
030900     .                                                                    
031000     EJECT                                                                
031100                                                                          
031200                                                                          
031300 S11-SKRIV-W47926 SECTION.                                                
031400                                                                          
031500     WRITE UT-POST FROM UT-AREA                                           
031600                                                                          
031700     MOVE 'W47926' TO POSTSUM-FDNAMN                                      
031800     MOVE 'W47926D1' TO POSTSUM-DDNAMN2                                   
031900     CALL POSTSUM USING POSTSUM-PARM                                      
032000     .                                                                    
032100     EJECT                                                                
032200                                                                          
032300                                                                          
032400 S99-ABEND SECTION.                                                       
032500                                                                          
032600     SKIP2                                                                
032700     MOVE 'S' TO POSTSUM-OPKOD                                            
032800     CALL POSTSUM USING POSTSUM-PARM                                      
032900     CALL ABEND USING RKOD-ABEND                                          
033000     .                                                                    
033100     EJECT                                                                
033200* --- IMS SEKTIONER ---                                                   
033300                                                                          
033400                                                                          
033500 IMS-GET-ORQP   SECTION.                                                  
033600                                                                          
033700     CALL CBLTDLI USING GN ORQP-PCB DLI-IO-WLORQP                         
033800     MOVE ORQP-STATUS-CODE TO STATUS-WS                                   
033900     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
034000     PERFORM IMS-STATUSKONTROLL                                           
034100     .                                                                    
034200     EJECT                                                                
034300 IMS-STATUSKONTROLL SECTION.                                              
034400                                                                          
034500     SET STATUS-IX TO 1                                                   
034600     SEARCH GODK-STATUS                                                   
034700       AT END                                                             
034800         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
034900           DELIMITED BY SIZE INTO FELTEXT                                 
035000         DISPLAY FELTEXT                                                  
035100         CALL FELLOG                                                      
035200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
035300         CONTINUE                                                         
035400     END-SEARCH                                                           
035500     .                                                                    
035600     EJECT                                                                
035700*    -COPY WY2000P1                                                       
