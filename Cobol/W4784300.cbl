000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W4784300.                                                
000400 AUTHOR.         GÖRAN KJELLSON                                           
000500 DATE-WRITTEN.   NOVEMBER  2011                                           
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SKAPAR EXTRAKTFIL FRÅN W47842                                    
001100*        URVALSREGLER:                                                    
001200*        KDFAKTDC PÅ WDB6                                                 
001300*        BLANK = INGET EXTRAKT                                            
001400*            A = EXTRAKT FÖR SAMTLIGA DISTRIKT OCH KUNDER                 
001500*          ÖVR = EXTRAKT OM FLLDCKND (WDB2) = JA                          
001600*                                                                         
001700*        INFILEN ÄR SORTERAD PÅ DISTRIKT, KUND                            
001800*                                                                         
001900*                                                                         
002000*    ABENDKODER:                                                          
002100*        U0016 -  . . . .                                                 
002200*        U1000 -  . . . .                                                 
002300*                                                                         
002400*                                                                         
002500                                                                          
002600                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003100     SKIP2                                                                
003200*          --- DAGENS ORDERDELAR                                          
003300     SELECT W47842                     ASSIGN TO W47843D1.                
003400                                                                          
003500*          --- RENSAD FIL FÖR D&P-LISTOR                                  
003600     SELECT W47843                     ASSIGN TO W47843D2.                
003700                                                                          
003800     EJECT                                                                
003900 DATA DIVISION.                                                           
004000 FILE SECTION.                                                            
004100                                                                          
004200 FD  W47842                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600*01  -COPY W47842      -L.                                                
004700                                                                          
004800                                                                          
004900 FD  W47843                                                               
005000     RECORDING       F                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01  UT-POST  -COPY W47843      -L.                                       
005400                                                                          
005500                                                                          
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800 77  IDPGM                       PIC X(8)    VALUE 'W4784300'.            
005900 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
006000 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
006100 77  FILLER                      PIC X(8)    VALUE 'ERRORTEX'.            
006200 77  ERROR-TEXT                  PIC X(80)   VALUE SPACE.                 
006300                                                                          
006400 77  WDB6-IX                     PIC 9(3)    VALUE ZERO.                  
006500 77  WDB6-IX-MAX                 PIC 9(3)    VALUE 100.                   
006600                                                                          
006700                                                                          
006800 77  JA                          PIC X       VALUE 'J'.                   
006900 77  NEJ                         PIC X       VALUE 'N'.                   
007000                                                                          
007100 77  AKTUELL-SW                  PIC X       VALUE 'N'.                   
007200     88  SKRIV-AKTUELL                       VALUE 'J'.                   
007300 77  EOF-W47842-SW               PIC X       VALUE 'N'.                   
007400     88  EOF-W47842                          VALUE 'J'.                   
007500                                                                          
007600 01  FILLER               PIC X(16)   VALUE 'WDB601 TABELL'.              
007700 01  WDB601-TABELL.                                                       
007800     03  WDB601-TAB  OCCURS 100.                                          
007900*        05  -COPY WDB601 -PRE TAB-                                       
008000                                                                          
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200*                                                                         
008300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008610     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
008700                                                                          
008710                                                                          
008720*    --- PARAMETERS FOR SUBPROGRAM WL10WBDC                               
008730                                                                          
008740 01  FILLER                      PIC X(16)  VALUE 'WL10WBDC AREA'.        
008750*01  -COPY WL10WBDC                                                       
008760                                                                          
008800*    --- PARAMETRAR TILL ABEND                                            
008900                                                                          
009000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009200                                                                          
009300 01  FELTEXT.                                                             
009400     03  FILLER                  PIC X(8)    VALUE 'FELTEXT '.            
009500     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009600     SKIP2                                                                
009700                                                                          
009800                                                                          
009900*    --- PARAMETRAR TILL POSTSUM                                          
010000*                                                                         
010100*01  -COPY W0005   -PRE  POSTSUM-                                         
010200                                                                          
010300                                                                          
010400 01  FILLER          PIC X(24)   VALUE 'W47842-AREA'.                     
010500                                                                          
010600 01  W47842-AREA.                                                         
010700*    03  AREA -COPY W47842     -PRE IN-                                   
010800                                                                          
010900                                                                          
011000 01  FILLER          PIC X(24)   VALUE 'W47843-AREA'.                     
011100                                                                          
011200 01  W47843-AREA.                                                         
011300*    03  AREA -COPY W47843     -PRE UT-                                   
011400                                                                          
011500                                                                          
011600 01  FILLER          PIC X(24)   VALUE 'W47968B-AREA'.                    
011700                                                                          
011800                                                                          
011900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012000*                                                                         
012100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
012200                                                                          
012300 01  NYCKLAR-TILL-DLI.                                                    
012400     03  W-IDGMT-X.                                                       
012500         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
012600         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
012700                                                                          
012800                                                                          
012900*    --- STATUS-KOD FRÅN IMS                                              
013000 01  STATUS-WS                   PIC XX.                                  
013100     88  SEGMENT-FINNS                       VALUE '  '.                  
013200     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
013300     88  BASEN-SLUT                          VALUE 'GB'.                  
013400                                                                          
013500 01  GODK-STATUSKODER.                                                    
013600     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013700                                                                          
013800 01  SSA1                        PIC X(128).                              
013900                                                                          
014000                                                                          
014100*    --- IMS FUNKTIONSKODER                                               
014200*01  -COPY W0003                                                          
014300                                                                          
014400*    ---  DLI INPUT-OUTPUT AREA                                           
014500 01  FILLER               PIC X(16)   VALUE 'WDB201 AREA'.                
014600 01  DLI-IO-WDB201.                                                       
014700*    03  -COPY WDB201                                                     
014800                                                                          
014900 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
015000 01  DLI-IO-WDB601.                                                       
015100*    03  -COPY WDB601                                                     
015200                                                                          
015300                                                                          
015400 LINKAGE SECTION.                                                         
015500                                                                          
015600*01  -COPY W0008   -PRE WDB2-                                             
015700     05  FILLER                  PIC X.                                   
015800                                                                          
015900*01  -COPY W0008   -PRE WDB6-                                             
016000     05  FILLER                  PIC X.                                   
016100                                                                          
016200 PROCEDURE DIVISION  USING WDB2-PCB WDB6-PCB.                             
016300 MAIN SECTION.                                                            
016400     ENTRY 'DLITCBL' USING WDB2-PCB WDB6-PCB.                             
016500                                                                          
016600                                                                          
016700     PERFORM A-INIT                                                       
016800     PERFORM S01-LAES-W47842                                              
016900     PERFORM UNTIL EOF-W47842                                             
017000                                                                          
017100        PERFORM B-KOLLA-OM-AKTUELL                                        
017200                                                                          
017300        IF SKRIV-AKTUELL                                                  
017400           PERFORM C-REDIGERA-SKRIV-W47843                                
017500        END-IF                                                            
017600                                                                          
017700        PERFORM S01-LAES-W47842                                           
017800     END-PERFORM                                                          
017900                                                                          
018000     PERFORM Z-FINIT                                                      
018100                                                                          
018200     MOVE ZERO TO RETURN-CODE                                             
018300     GOBACK                                                               
018400     .                                                                    
018500                                                                          
018600                                                                          
018700 A-INIT SECTION.                                                          
018800     MOVE 'A-INIT          ' TO CURRENT-SECTION.                          
018900                                                                          
019000     OPEN INPUT  W47842                                                   
019100     OPEN OUTPUT W47843                                                   
019200                                                                          
019300     MOVE IDPGM         TO POSTSUM-PROGNAMN                               
019400     MOVE ZERO          TO WDB6-IX                                        
019500                                                                          
019600     PERFORM IMS-01-GN-WDB601                                             
019700     PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT                           
019800                OR WDB6-IX = WDB6-IX-MAX                                  
019900        ADD 1              TO WDB6-IX                                     
020000        MOVE DLI-IO-WDB601 TO WDB601-TAB(WDB6-IX)                         
020100        PERFORM IMS-01-GN-WDB601                                          
020200     END-PERFORM                                                          
020300     DISPLAY 'ANTAL WDB6-POSTER ' WDB6-IX                                 
020400     .                                                                    
020500                                                                          
020600 B-KOLLA-OM-AKTUELL SECTION.                                              
020700     MOVE 'B-KOLLA-OM-AKTUE' TO CURRENT-SECTION                           
020800                                                                          
020900     MOVE NEJ TO AKTUELL-SW                                               
021000                                                                          
021100     IF WDB6-IX > WDB6-IX-MAX                                             
021200     OR IN-IDDC NOT = TAB-DCS-IDDC(WDB6-IX)                               
021300        MOVE  1  TO WDB6-IX                                               
021400        PERFORM UNTIL WDB6-IX > WDB6-IX-MAX                               
021500                   OR TAB-DCS-IDDC(WDB6-IX) = IN-IDDC                     
021600                                                                          
021700           ADD 1 TO WDB6-IX                                               
021800        END-PERFORM                                                       
021900     END-IF                                                               
022000                                                                          
022100     IF WDB6-IX NOT > WDB6-IX-MAX                                         
022200        IF TAB-DCS-KDFAKTDC(WDB6-IX) NOT = SPACE AND                      
022300           IN-FLDIRLEV = NEJ                                              
022400                                                                          
022500           IF TAB-DCS-KDFAKTDC(WDB6-IX) = 'A'                             
022600              MOVE JA TO AKTUELL-SW                                       
022700           ELSE                                                           
022800              IF IN-IDDISTR NOT = W-IDDISTR OR                            
022900                 IN-IDKUNDNR NOT = W-IDKUNDNR                             
023000                                                                          
023100                 MOVE IN-IDDISTR TO W-IDDISTR                             
023200                 MOVE IN-IDKUNDNR TO W-IDKUNDNR                           
023300                 PERFORM IMS-02-GU-WDB201                                 
023400              END-IF                                                      
023500              IF GMT-FLLDCKND = JA                                        
023600                 MOVE JA TO AKTUELL-SW                                    
023700              END-IF                                                      
023800           END-IF                                                         
023900        END-IF                                                            
024000     END-IF                                                               
024100                                                                          
024200     .                                                                    
024300                                                                          
024400 C-REDIGERA-SKRIV-W47843  SECTION.                                        
024500     MOVE 'C-REDIGERA-SKRIV' TO CURRENT-SECTION                           
024600                                                                          
024601     MOVE IN-IDDC                   TO WBDC-IDDC                          
024610     CALL WL10WBDC USING WBDC-AREA                                        
024640     IF WBDC-FLWEBDC = JA                                                 
024670        MOVE WBDC-KDMFUP            TO UT-KDMFUP                          
024810        MOVE IN-IDDC                TO UT-IDDC                            
024900        MOVE TAB-DCS-ADCITY IN TAB-DCS-ADPOST-PNRORT(WDB6-IX)             
025000                                    TO UT-ADCITY                          
025100        MOVE IN-IDPRC               TO UT-IDPRC                           
025200        MOVE IN-KDORDKL             TO UT-KDORDKL                         
025300        MOVE IN-TIDATUM             TO UT-TIDATUM                         
025400        MOVE IN-TIAAVV              TO UT-TIAAVV                          
025500        MOVE IN-TIAARP              TO UT-TIAARP                          
025600        MOVE IN-TIRFSTID            TO UT-TIRFSTID                        
025700        MOVE IN-KVORDER             TO UT-KVORDER                         
025800        MOVE IN-KVORDER-UTSKR       TO UT-KVORDER-UTSKR                   
025900        MOVE IN-KVORDER-PACK        TO UT-KVORDER-PACK                    
026000        MOVE IN-KVORDRAD            TO UT-KVORDRAD                        
026100        MOVE IN-KVORDRAD-UTSKR      TO UT-KVORDRAD-UTSKR                  
026200        MOVE IN-KVORDRAD-PACK       TO UT-KVORDRAD-PACK                   
026300        MOVE IN-KVKOLLI             TO UT-KVKOLLI                         
026400        MOVE IN-KVKOLLI-LAST        TO UT-KVKOLLI-LAST                    
026500                                                                          
026600        PERFORM S02-SKRIV-W47843                                          
026610     END-IF                                                               
026700     .                                                                    
026800                                                                          
026900 Z-FINIT SECTION.                                                         
027000     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
027100                                                                          
027200     CLOSE W47842                                                         
027300           W47843                                                         
027400                                                                          
027500     MOVE 'S' TO POSTSUM-OPKOD                                            
027600     CALL POSTSUM USING POSTSUM-PARM                                      
027700     .                                                                    
027800                                                                          
027900 S01-LAES-W47842  SECTION.                                                
028000     MOVE 'S01-LAES-W47842 ' TO CURRENT-SECTION                           
028100                                                                          
028200     READ W47842    INTO IN-AREA                                          
028300                                                                          
028400     AT END                                                               
028500        MOVE JA TO EOF-W47842-SW                                          
028600                                                                          
028700     NOT AT END                                                           
028800        MOVE 'W47842'                     TO POSTSUM-FDNAMN               
028900        MOVE 'W47843D1'                   TO POSTSUM-DDNAMN2              
029000        MOVE 'IN'                         TO POSTSUM-TRANSTYP             
029100        CALL POSTSUM USING POSTSUM-PARM                                   
029200     END-READ                                                             
029300     .                                                                    
029400                                                                          
029500 S02-SKRIV-W47843   SECTION.                                              
029600     MOVE 'S0S-SKRIV-W47843' TO CURRENT-SECTION                           
029700                                                                          
029800     WRITE UT-POST    FROM W47843-AREA                                    
029900                                                                          
030000     MOVE 'W47968B'                       TO POSTSUM-FDNAMN               
030100     MOVE 'W47963D3'                      TO POSTSUM-DDNAMN2              
030200     MOVE 'UT'                            TO POSTSUM-TRANSTYP             
030300     CALL POSTSUM USING POSTSUM-PARM                                      
030400     .                                                                    
030500                                                                          
030600                                                                          
030700                                                                          
030800* --- IMS SEKTIONER ---                                                   
030900                                                                          
031000                                                                          
031100 IMS-01-GN-WDB601    SECTION.                                             
031200     MOVE 'IMS-01' TO CURRENT-IMS-SECTION                                 
031300                                                                          
031400     MOVE 'WDB601  '          TO SSA1                                     
031500     MOVE '  GEGB'            TO GODK-STATUSKODER                         
031600     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
031700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
031800     PERFORM IMS-STATUSKONTROLL                                           
031900     .                                                                    
032000                                                                          
032100 IMS-02-GU-WDB201      SECTION.                                           
032200     MOVE 'IMS-02' TO CURRENT-IMS-SECTION                                 
032300                                                                          
032400     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
032500          DELIMITED BY SIZE INTO SSA1                                     
032600                                                                          
032700     MOVE '  GE'              TO GODK-STATUSKODER                         
032800     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
032900     MOVE WDB2-STATUS-CODE    TO STATUS-WS                                
033000     PERFORM IMS-STATUSKONTROLL                                           
033100     IF SEGMENT-SAKNAS                                                    
033200         MOVE SPACE TO GMT-FLLDCKND                                       
033300     END-IF                                                               
033400     .                                                                    
033500 IMS-STATUSKONTROLL SECTION.                                              
033600                                                                          
033700     SET STATUS-IX TO 1                                                   
033800     SEARCH GODK-STATUS                                                   
033900       AT END                                                             
034000         MOVE 'FEL STATUSKOD FRÅN IMS ' TO FELTEXT-STR                    
034100         DISPLAY FELTEXT                                                  
034200         CALL FELLOG                                                      
034300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
034400         CONTINUE                                                         
034500     END-SEARCH                                                           
034600     .                                                                    
