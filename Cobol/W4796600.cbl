000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4796600.                                                
000300 AUTHOR.         KJELL ANDRÉ.                                             
000400 DATE-WRITTEN.   OKTOBER 2011.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*    PROGRAMMET LÄSER EXTRAKTFIL MED FAKTURAINFORMATION PER DC            
000900*    SOM REDIGERAS OM FÖR D&P                                             
001000*    SOM LAGRAR DET FÖR MANAGEMENT FOLLOW-UP I XDC-SYSTEMET               
001100*                                                                         
001200*    PROGRAMMET KÖRS BÅDE VID VECKOSLUT OCH PERIODSLUT, SÅ                
001300*    FOLLOW-UP-RAPPORTERNAS NAMN ÄR OLIKA BEROENDE PÅ KÖRNINGS-           
001400*    TILLFÄLLE OCH LÄSES IN VIA "SYSIN" I JCL:EN.                         
001500*    VECKOSLUT............: W47963-001 (INVDISTR-W)                       
001600*    PERIODSLUT...........: W47963-002 (INVDISTR-P)                       
001700*    PERIODSLUT MANAGEMENT: W47963-003 (MAN-INVDISTR-P)                   
001800                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200*    --- FAKTURAINFORMATION SENASTE VECKAN/PERIODEN PER DC                
002300     SELECT W47968B                    ASSIGN TO W47966D1.                
002400                                                                          
002500*    --- RAPPORTNAMN VIA SYSIN                                            
002600     SELECT RPTNAME                    ASSIGN TO W47966D2.                
002700                                                                          
002800*    --- FAKTURAINFO SENASTE VECKAN/PERIODEN PER DC  TO D&P               
002900     SELECT W47968C                    ASSIGN TO W47966D3.                
003000                                                                          
003100                                                                          
003200 DATA DIVISION.                                                           
003300                                                                          
003400 FILE SECTION.                                                            
003500 FD  W47968B                                                              
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01 -COPY W47968B   -L.                                                   
004000                                                                          
004100                                                                          
004200 FD  RPTNAME                                                              
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500                                                                          
004600 01  FILLER          PIC X(80).                                           
004700                                                                          
004800                                                                          
004900 FD  W47968C                                                              
005000     RECORDING       V                                                    
005100     BLOCK CONTAINS  0.                                                   
005200                                                                          
005300*01 POST  -COPY W479631  -PRE 68C-   -L.                                  
005400                                                                          
005500                                                                          
005600 WORKING-STORAGE SECTION.                                                 
005700                                                                          
005800 77  IDPGM                       PIC X(8)    VALUE 'W4796600'.            
005900 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
006000 77  CURRENT-DP-SECTION          PIC X(16)   VALUE SPACE.                 
006100 77  CURRENT-IDDC                PIC X(2)    VALUE SPACE.                 
006200 77  CURRENT-IDDISTR             PIC 9(5)    VALUE ZERO.                  
006300 77  CURRENT-IDKUNDNR            PIC 9(7)    VALUE ZERO.                  
006400 77  CURRENT-KDORDKL             PIC 9(1)    VALUE ZERO.                  
006500 77  CURRENT-KDMFUP              PIC X(2)    VALUE SPACE.                 
006600 77  CURRENT-ADCITY              PIC X(20)   VALUE SPACE.                 
006700                                                                          
006800 77  JA                          PIC X(1)    VALUE 'J'.                   
006900 77  NEJ                         PIC X(1)    VALUE 'N'.                   
007000                                                                          
007100 77  SW-VECKA-PERIOD             PIC X(1)    VALUE SPACE.                 
007200     88 VECKA                                VALUE 'V'.                   
007300     88 PERIOD                               VALUE 'P'.                   
007400     88 PERIOD-MGMT                          VALUE 'M'.                   
007500                                                                          
007600                                                                          
007700 77  W47968B-EOF-SW              PIC X       VALUE 'N'.                   
007800     88  END-OF-W47968B                      VALUE 'J'.                   
007900                                                                          
008000 77  RPTNAME-EOF-SW              PIC X       VALUE 'N'.                   
008100     88  END-OF-RPTNAME                      VALUE 'J'.                   
008200                                                                          
008300 77  DC-SW                       PIC X       VALUE 'J'.                   
008400     88  FORSTA-DC                           VALUE 'J'.                   
008500                                                                          
008600 01  ARBETSVARIABLER.                                                     
008700     03  WS-SULEVANT-OK          PIC 9(7)    VALUE ZERO.                  
008800     03  WS-SURADER-OK           PIC 9(7)    VALUE ZERO.                  
008900     03  WS-SUKOLLI-OK           PIC 9(7)    VALUE ZERO.                  
009000     03  WS-KVORDER-OK           PIC 9(7)    VALUE ZERO.                  
009100     03  WS-VKORDBTO-OK          PIC 9(7)V9  VALUE ZERO.                  
009200     03  WS-PRARTNTO-OK          PIC 9(7)V99 VALUE ZERO.                  
009300                                                                          
009400     03  WS-SULEVANT-KU          PIC 9(7)    VALUE ZERO.                  
009500     03  WS-SURADER-KU           PIC 9(7)    VALUE ZERO.                  
009600     03  WS-SUKOLLI-KU           PIC 9(7)    VALUE ZERO.                  
009700     03  WS-KVORDER-KU           PIC 9(7)    VALUE ZERO.                  
009800     03  WS-VKORDBTO-KU          PIC 9(7)V9  VALUE ZERO.                  
009900     03  WS-PRARTNTO-KU          PIC 9(7)V99 VALUE ZERO.                  
010000                                                                          
010100     03  WS-SULEVANT-DI          PIC 9(7)    VALUE ZERO.                  
010200     03  WS-SURADER-DI           PIC 9(7)    VALUE ZERO.                  
010300     03  WS-SUKOLLI-DI           PIC 9(7)    VALUE ZERO.                  
010400     03  WS-KVORDER-DI           PIC 9(7)    VALUE ZERO.                  
010500     03  WS-VKORDBTO-DI          PIC 9(7)V9  VALUE ZERO.                  
010600     03  WS-PRARTNTO-DI          PIC 9(7)V99 VALUE ZERO.                  
010700                                                                          
010800     03  WS-SULEVANT-TOT         PIC 9(7)    VALUE ZERO.                  
010900     03  WS-SURADER-TOT          PIC 9(7)    VALUE ZERO.                  
011000     03  WS-SUKOLLI-TOT          PIC 9(7)    VALUE ZERO.                  
011100     03  WS-KVORDER-TOT          PIC 9(7)    VALUE ZERO.                  
011200     03  WS-VKORDBTO-TOT         PIC 9(7)V9  VALUE ZERO.                  
011300     03  WS-PRARTNTO-TOT         PIC 9(7)V99 VALUE ZERO.                  
011400                                                                          
011500                                                                          
011600 01  ERRTEXT.                                                             
011700     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
011800     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
011900 01  KDRC-DISPLAY                PIC Z(5).                                
012000                                                                          
012100                                                                          
012200 01  GENERAL-SUBPROGRAMS.                                                 
012300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012400                                                                          
012500                                                                          
012600 01  DAGENS-DATUM                PIC 9(6)  VALUE ZERO.                    
012700 01  FILLER REDEFINES DAGENS-DATUM.                                       
012800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
013000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
013100 01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
013200                                                                          
013300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
013400                                                                          
013500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013800                                                                          
013900*    --- IN-AREOR                                                         
014000 01  FILLER                      PIC X(16)   VALUE                        
014100                                'INPUT-AREA     '.                        
014200*01  AREA  -COPY W47968B        -PRE INPUT-                               
014300                                                                          
014400                                                                          
014500 01  FILLER                      PIC X(16)   VALUE                        
014600                                'RPTNAME-AREA   '.                        
014700 01  RPTNAME-AREA.                                                        
014800     03 REPORT-NAME              PIC X(15).                               
014900     03 FILLER                   PIC X(65).                               
015000                                                                          
015100*    --- UT-AREOR                                                         
015200 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
015300                                'OUTPUT-AREA     '.                       
015400 01  W001-DAP.                                                            
015500     03  FILLER                  PIC X(165)  VALUE SPACE.                 
015600                                                                          
015700                                                                          
015800 01  W47968C-AREA.                                                        
015900*    03 -COPY W479631 -PRE OUT-                                           
016000                                                                          
016100                                                                          
016200                                                                          
016300 PROCEDURE DIVISION.                                                      
016400 MAIN SECTION.                                                            
016500                                                                          
016600     PERFORM A-INIT                                                       
016700     PERFORM S01-READ-W47968B                                             
016800                                                                          
016900     PERFORM UNTIL END-OF-W47968B                                         
017000                                                                          
017100        IF INPUT-IDDC-LEV NOT = CURRENT-IDDC                              
017200           PERFORM B-NEW-IDDC                                             
017300        END-IF                                                            
017400                                                                          
017500        IF INPUT-IDDISTR = CURRENT-IDDISTR                                
017600           IF INPUT-IDKUNDNR = CURRENT-IDKUNDNR                           
017700              IF INPUT-KDORDKL = CURRENT-KDORDKL                          
017800                 CONTINUE                                                 
017900              ELSE                                                        
018000* NY ORDEKLASS                                                            
018100                 PERFORM S10-KDORDKL-TOT                                  
018200                 MOVE INPUT-KDORDKL TO CURRENT-KDORDKL                    
018300              END-IF                                                      
018400           ELSE                                                           
018500* NY KUND                                                                 
018600              PERFORM S10-KDORDKL-TOT                                     
018700              PERFORM S20-IDKUNDNR-TOT                                    
018800              MOVE INPUT-KDORDKL    TO CURRENT-KDORDKL                    
018900              MOVE INPUT-IDKUNDNR   TO CURRENT-IDKUNDNR                   
019000           END-IF                                                         
019100        ELSE                                                              
019200* NYTT DISTRIKT                                                           
019300           PERFORM S10-KDORDKL-TOT                                        
019400           PERFORM S20-IDKUNDNR-TOT                                       
019500           PERFORM S30-IDDISTR-TOT                                        
019600           MOVE INPUT-KDORDKL       TO CURRENT-KDORDKL                    
019700           MOVE INPUT-IDKUNDNR      TO CURRENT-IDKUNDNR                   
019800           MOVE INPUT-IDDISTR       TO CURRENT-IDDISTR                    
019900        END-IF                                                            
020000                                                                          
020100        ADD INPUT-KVLEVART2 TO WS-SULEVANT-OK                             
020200                               WS-SULEVANT-KU                             
020300                               WS-SULEVANT-DI                             
020400                               WS-SULEVANT-TOT                            
020500        ADD INPUT-KVRADER   TO WS-SURADER-OK                              
020600                               WS-SURADER-KU                              
020700                               WS-SURADER-DI                              
020800                               WS-SURADER-TOT                             
020900        ADD INPUT-KVKOLLI   TO WS-SUKOLLI-OK                              
021000                               WS-SUKOLLI-KU                              
021100                               WS-SUKOLLI-DI                              
021200                               WS-SUKOLLI-TOT                             
021300        ADD INPUT-KVORDER   TO WS-KVORDER-OK                              
021400                               WS-KVORDER-KU                              
021500                               WS-KVORDER-DI                              
021600                               WS-KVORDER-TOT                             
021700        ADD INPUT-VKORDBTO  TO WS-VKORDBTO-OK                             
021800                               WS-VKORDBTO-KU                             
021900                               WS-VKORDBTO-DI                             
022000                               WS-VKORDBTO-TOT                            
022100        ADD INPUT-PRARTNTO  TO WS-PRARTNTO-OK                             
022200                             WS-PRARTNTO-KU                               
022300                               WS-PRARTNTO-DI                             
022400                               WS-PRARTNTO-TOT                            
022500                                                                          
022600        PERFORM S01-READ-W47968B                                          
022700     END-PERFORM                                                          
022800                                                                          
022900     IF CURRENT-IDDC NOT = SPACE                                          
023000        PERFORM S10-KDORDKL-TOT                                           
023100        PERFORM S20-IDKUNDNR-TOT                                          
023200        PERFORM S30-IDDISTR-TOT                                           
023300        PERFORM S40-DC-TOTAL                                              
023400     END-IF                                                               
023500                                                                          
023600     PERFORM Z-FINIT                                                      
023700                                                                          
023800     MOVE ZERO TO RETURN-CODE                                             
023900     GOBACK                                                               
024000     .                                                                    
024100                                                                          
024200 A-INIT SECTION.                                                          
024300     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
024400                                                                          
024500     OPEN INPUT  W47968B                                                  
024600     OPEN OUTPUT W47968C                                                  
024700     ACCEPT DAGENS-DATUM     FROM DATE                                    
024800     ACCEPT DAGENS-TID       FROM TIME                                    
024900                                                                          
025000*    -- HÄMTA RAPPORTENS NAMN FRÅN SYSIN I JCL:EN                         
025100     OPEN INPUT RPTNAME                                                   
025200     READ RPTNAME INTO RPTNAME-AREA                                       
025300       AT END                                                             
025400         SET END-OF-RPTNAME TO TRUE                                       
025500     END-READ                                                             
025600     IF END-OF-RPTNAME                                                    
025700       STRING 'DD R46966D2 IS EMPTY'                                      
025800              DELIMITED BY SIZE INTO ERRTEXT-STR                          
025900       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
026000     END-IF                                                               
026100     CLOSE RPTNAME                                                        
026200                                                                          
026300     IF REPORT-NAME = 'W47966-001'                                        
026400        SET VECKA  TO TRUE                                                
026500     ELSE                                                                 
026600        IF REPORT-NAME = 'W47966-002'                                     
026700           SET PERIOD TO TRUE                                             
026800        ELSE                                                              
026900           SET PERIOD-MGMT TO TRUE                                        
027000        END-IF                                                            
027100     END-IF                                                               
027200     .                                                                    
027300                                                                          
027400 B-NEW-IDDC    SECTION.                                                   
027500     MOVE 'B-NEW-IDDC      ' TO CURRENT-SECTION                           
027600                                                                          
027700     IF FORSTA-DC                                                         
027800        MOVE NEJ TO DC-SW                                                 
027900     ELSE                                                                 
028000        PERFORM S10-KDORDKL-TOT                                           
028100        PERFORM S20-IDKUNDNR-TOT                                          
028200        PERFORM S30-IDDISTR-TOT                                           
028300        PERFORM S40-DC-TOTAL                                              
028400     END-IF                                                               
028500     MOVE INPUT-IDDC-LEV TO CURRENT-IDDC                                  
028600     MOVE INPUT-IDDISTR  TO CURRENT-IDDISTR                               
028700     MOVE INPUT-IDKUNDNR TO CURRENT-IDKUNDNR                              
028800     MOVE INPUT-KDORDKL  TO CURRENT-KDORDKL                               
028900     MOVE INPUT-KDMFUP   TO CURRENT-KDMFUP                                
029000     MOVE INPUT-ADCITY   TO CURRENT-ADCITY                                
029100                                                                          
029200     MOVE ZERO           TO WS-SULEVANT-OK                                
029300                            WS-SULEVANT-KU                                
029400                            WS-SULEVANT-DI                                
029500                            WS-SULEVANT-TOT                               
029600                            WS-SURADER-OK                                 
029700                            WS-SURADER-KU                                 
029800                            WS-SURADER-DI                                 
029900                            WS-SURADER-TOT                                
030000                            WS-SUKOLLI-OK                                 
030100                            WS-SUKOLLI-KU                                 
030200                            WS-SUKOLLI-DI                                 
030300                            WS-SUKOLLI-TOT                                
030400                            WS-KVORDER-OK                                 
030500                            WS-KVORDER-KU                                 
030600                            WS-KVORDER-DI                                 
030700                            WS-KVORDER-TOT                                
030800                            WS-VKORDBTO-OK                                
030900                            WS-VKORDBTO-KU                                
031000                            WS-VKORDBTO-DI                                
031100                            WS-VKORDBTO-TOT                               
031200                            WS-PRARTNTO-OK                                
031300                            WS-PRARTNTO-KU                                
031400                            WS-PRARTNTO-DI                                
031500                            WS-PRARTNTO-TOT                               
031600                                                                          
031700     PERFORM S02-SKRIV-W47968C-DAP                                        
031800     .                                                                    
031900                                                                          
032000 S01-READ-W47968B SECTION.                                                
032100     MOVE 'S01-READ-W47968B ' TO CURRENT-SECTION                          
032200                                                                          
032300     READ W47968B INTO INPUT-W47968B                                      
032400       AT END                                                             
032500         SET END-OF-W47968B TO TRUE                                       
032600     END-READ                                                             
032700     .                                                                    
032800                                                                          
032900                                                                          
033000 S02-SKRIV-W47968C-REC SECTION.                                           
033100     MOVE 'S02-SKRIV-REC'        TO CURRENT-SECTION.                      
033200                                                                          
033300     WRITE 68C-POST FROM W47968C-AREA                                     
033400     .                                                                    
033500                                                                          
033600 S02-SKRIV-W47968C-DAP SECTION.                                           
033700     MOVE 'S02-SKRIV-DAP'        TO CURRENT-SECTION.                      
033800                                                                          
033900     IF VECKA                                                             
034000       MOVE '¤DAPINVDISTR-W'       TO W001-DAP                            
034100     ELSE                                                                 
034200        IF PERIOD                                                         
034300           MOVE '¤DAPINVDISTR-P'       TO W001-DAP                        
034400        ELSE                                                              
034500           MOVE '¤DAPMAN-INVDISTR-P'   TO W001-DAP                        
034600        END-IF                                                            
034700     END-IF                                                               
034800     WRITE 68C-POST FROM W001-DAP                                         
034900                                                                          
035000     MOVE SPACE TO W001-DAP                                               
035100     IF VECKA                                                             
035200       STRING '¤DAP' CURRENT-IDDC ' W47966-001'                           
035300       DELIMITED BY SIZE INTO W001-DAP                                    
035400                                                                          
035500     ELSE                                                                 
035600        IF PERIOD                                                         
035700           STRING '¤DAP' CURRENT-IDDC ' W47966-002'                       
035800           DELIMITED BY SIZE INTO W001-DAP                                
035900        ELSE                                                              
036000           STRING '¤DAP' CURRENT-KDMFUP CURRENT-IDDC ' W47966-003'        
036100           DELIMITED BY SIZE INTO W001-DAP                                
036200        END-IF                                                            
036300     END-IF                                                               
036400     WRITE 68C-POST FROM W001-DAP                                         
036500     .                                                                    
036600 S10-KDORDKL-TOT  SECTION.                                                
036700     MOVE 'S10-KDORDKL-TOT   ' TO CURRENT-SECTION                         
036800                                                                          
036900     MOVE '1'                TO OUT-IDAFPRCD                              
037000     MOVE INPUT-KDMFUP       TO OUT-KDMFUP                                
037100     MOVE CURRENT-IDDC       TO OUT-IDDC                                  
037200     MOVE CURRENT-IDDISTR    TO OUT-IDDISTR                               
037300     MOVE CURRENT-IDKUNDNR   TO OUT-IDKUNDNR                              
037400     MOVE CURRENT-KDORDKL    TO OUT-KDORDKL                               
037500     MOVE CURRENT-KDMFUP     TO OUT-KDMFUP                                
037600     MOVE CURRENT-ADCITY     TO OUT-ADCITY                                
037700                                                                          
037800     IF VECKA                                                             
037900        MOVE INPUT-TIAAVV    TO OUT-TIAAVV                                
038000     ELSE                                                                 
038100        MOVE INPUT-TIAARP    TO OUT-TIAARP                                
038200     END-IF                                                               
038300     MOVE WS-SULEVANT-OK     TO OUT-SULEVANT                              
038400     MOVE WS-SURADER-OK      TO OUT-SURADER                               
038500     MOVE WS-SUKOLLI-OK      TO OUT-SUKOLLI                               
038600     MOVE WS-KVORDER-OK      TO OUT-KVORDER                               
038700     MOVE WS-VKORDBTO-OK     TO OUT-VKORDBTO-KOLLI                        
038800     IF PERIOD-MGMT                                                       
038900        MOVE WS-PRARTNTO-OK  TO OUT-SUACKSTD                              
039000     ELSE                                                                 
039100        MOVE ZERO            TO OUT-SUACKSTD                              
039200     END-IF                                                               
039300                                                                          
039400     PERFORM S02-SKRIV-W47968C-REC                                        
039500                                                                          
039600     MOVE ZERO               TO WS-SULEVANT-OK                            
039700                                WS-SURADER-OK                             
039800                                WS-SUKOLLI-OK                             
039900                                WS-KVORDER-OK                             
040000                                WS-VKORDBTO-OK                            
040100                                WS-PRARTNTO-OK                            
040200     .                                                                    
040300                                                                          
040400 S20-IDKUNDNR-TOT SECTION.                                                
040500     MOVE 'S20-IDKUNDNR-TOT  ' TO CURRENT-SECTION                         
040600                                                                          
040700     MOVE '2'                TO OUT-IDAFPRCD                              
040800                                                                          
040900     MOVE INPUT-KDMFUP       TO OUT-KDMFUP                                
041000     MOVE CURRENT-IDDC       TO OUT-IDDC                                  
041100     MOVE CURRENT-IDDISTR    TO OUT-IDDISTR                               
041200     MOVE CURRENT-IDKUNDNR   TO OUT-IDKUNDNR                              
041300     MOVE CURRENT-KDORDKL    TO OUT-KDORDKL                               
041400     MOVE CURRENT-KDMFUP     TO OUT-KDMFUP                                
041500     MOVE CURRENT-ADCITY     TO OUT-ADCITY                                
041600                                                                          
041700     IF VECKA                                                             
041800        MOVE INPUT-TIAAVV    TO OUT-TIAAVV                                
041900     ELSE                                                                 
042000        MOVE INPUT-TIAARP    TO OUT-TIAARP                                
042100     END-IF                                                               
042200     MOVE WS-SULEVANT-KU     TO OUT-SULEVANT                              
042300     MOVE WS-SURADER-KU      TO OUT-SURADER                               
042400     MOVE WS-SUKOLLI-KU      TO OUT-SUKOLLI                               
042500     MOVE WS-KVORDER-KU      TO OUT-KVORDER                               
042600     MOVE WS-VKORDBTO-KU     TO OUT-VKORDBTO-KOLLI                        
042700     IF PERIOD-MGMT                                                       
042800        MOVE WS-PRARTNTO-KU  TO OUT-SUACKSTD                              
042900     ELSE                                                                 
043000        MOVE ZERO            TO OUT-SUACKSTD                              
043100     END-IF                                                               
043200                                                                          
043300     PERFORM S02-SKRIV-W47968C-REC                                        
043400                                                                          
043500     MOVE ZERO               TO WS-SULEVANT-KU                            
043600                                WS-SURADER-KU                             
043700                                WS-SUKOLLI-KU                             
043800                                WS-KVORDER-KU                             
043900                                WS-VKORDBTO-KU                            
044000                                WS-PRARTNTO-KU                            
044100     .                                                                    
044200                                                                          
044300 S30-IDDISTR-TOT  SECTION.                                                
044400     MOVE 'S30-IDDISTR-TOT   ' TO CURRENT-SECTION                         
044500                                                                          
044600     MOVE '3'                TO OUT-IDAFPRCD                              
044700                                                                          
044800     MOVE INPUT-KDMFUP       TO OUT-KDMFUP                                
044900     MOVE CURRENT-IDDC       TO OUT-IDDC                                  
045000     MOVE CURRENT-IDDISTR    TO OUT-IDDISTR                               
045100     MOVE CURRENT-IDKUNDNR   TO OUT-IDKUNDNR                              
045200     MOVE CURRENT-KDORDKL    TO OUT-KDORDKL                               
045300     MOVE CURRENT-KDMFUP     TO OUT-KDMFUP                                
045400     MOVE CURRENT-ADCITY     TO OUT-ADCITY                                
045500                                                                          
045600     IF VECKA                                                             
045700        MOVE INPUT-TIAAVV    TO OUT-TIAAVV                                
045800     ELSE                                                                 
045900        MOVE INPUT-TIAARP    TO OUT-TIAARP                                
046000     END-IF                                                               
046100     MOVE WS-SULEVANT-DI     TO OUT-SULEVANT                              
046200     MOVE WS-SURADER-DI      TO OUT-SURADER                               
046300     MOVE WS-SUKOLLI-DI      TO OUT-SUKOLLI                               
046400     MOVE WS-KVORDER-DI      TO OUT-KVORDER                               
046500     MOVE WS-VKORDBTO-DI     TO OUT-VKORDBTO-KOLLI                        
046600     IF PERIOD-MGMT                                                       
046700        MOVE WS-PRARTNTO-DI  TO OUT-SUACKSTD                              
046800     ELSE                                                                 
046900        MOVE ZERO            TO OUT-SUACKSTD                              
047000     END-IF                                                               
047100                                                                          
047200     PERFORM S02-SKRIV-W47968C-REC                                        
047300                                                                          
047400     MOVE ZERO               TO WS-SULEVANT-DI                            
047500                                WS-SURADER-DI                             
047600                                WS-SUKOLLI-DI                             
047700                                WS-KVORDER-DI                             
047800                                WS-VKORDBTO-DI                            
047900                                WS-PRARTNTO-DI                            
048000                                                                          
048100     .                                                                    
048200                                                                          
048300 S40-DC-TOTAL     SECTION.                                                
048400     MOVE 'S40-DC-TOTAL    ' TO CURRENT-SECTION                           
048500                                                                          
048600     MOVE '4'                TO OUT-IDAFPRCD                              
048700                                                                          
048800     MOVE INPUT-KDMFUP       TO OUT-KDMFUP                                
048900     MOVE CURRENT-IDDC       TO OUT-IDDC                                  
049000     MOVE CURRENT-IDDISTR    TO OUT-IDDISTR                               
049100     MOVE CURRENT-IDKUNDNR   TO OUT-IDKUNDNR                              
049200     MOVE CURRENT-KDORDKL    TO OUT-KDORDKL                               
049300     MOVE CURRENT-KDMFUP     TO OUT-KDMFUP                                
049400     MOVE CURRENT-ADCITY     TO OUT-ADCITY                                
049500                                                                          
049600     IF VECKA                                                             
049700        MOVE INPUT-TIAAVV    TO OUT-TIAAVV                                
049800     ELSE                                                                 
049900        MOVE INPUT-TIAARP    TO OUT-TIAARP                                
050000     END-IF                                                               
050100     MOVE WS-SULEVANT-TOT    TO OUT-SULEVANT                              
050200     MOVE WS-SURADER-TOT     TO OUT-SURADER                               
050300     MOVE WS-SUKOLLI-TOT     TO OUT-SUKOLLI                               
050400     MOVE WS-KVORDER-TOT     TO OUT-KVORDER                               
050500     MOVE WS-VKORDBTO-TOT    TO OUT-VKORDBTO-KOLLI                        
050600     IF PERIOD-MGMT                                                       
050700        MOVE WS-PRARTNTO-TOT TO OUT-SUACKSTD                              
050800     ELSE                                                                 
050900        MOVE ZERO            TO OUT-SUACKSTD                              
051000     END-IF                                                               
051100                                                                          
051200     PERFORM S02-SKRIV-W47968C-REC                                        
051300                                                                          
051400     MOVE ZERO               TO WS-SULEVANT-TOT                           
051500                                WS-SURADER-TOT                            
051600                                WS-SUKOLLI-TOT                            
051700                                WS-KVORDER-TOT                            
051800                                WS-VKORDBTO-TOT                           
051900                                WS-PRARTNTO-TOT                           
052000                                                                          
052100     .                                                                    
052200                                                                          
052300 Z-FINIT SECTION.                                                         
052400     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
052500                                                                          
052600                                                                          
052700     CLOSE W47968B                                                        
052800           W47968C                                                        
052900     .                                                                    
