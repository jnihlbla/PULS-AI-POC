000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4796700.                                                
000300 AUTHOR.         GÖRAN KJELLSON                                           
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
001500*    PERIODSLUT: W47963-004                                               
001600                                                                          
001700 ENVIRONMENT DIVISION.                                                    
001800 INPUT-OUTPUT SECTION.                                                    
001900 FILE-CONTROL.                                                            
002000*    --- FAKTURAINFORMATION SENASTE VECKAN/PERIODEN PER DC                
002100     SELECT W47968B                    ASSIGN TO W47967D1.                
002200                                                                          
002300*    --- RAPPORTNAMN VIA SYSIN                                            
002400     SELECT RPTNAME                    ASSIGN TO W47967D2.                
002500                                                                          
002600*    --- FAKTURAINFO SENASTE VECKAN/PERIODEN PER DC  TO D&P               
002700     SELECT W47968D                    ASSIGN TO W47967D3.                
002800                                                                          
002900                                                                          
003000 DATA DIVISION.                                                           
003100                                                                          
003200 FILE SECTION.                                                            
003300 FD  W47968B                                                              
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01 -COPY W47968B   -L.                                                   
003800                                                                          
003900                                                                          
004000 FD  RPTNAME                                                              
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400 01  FILLER          PIC X(80).                                           
004500                                                                          
004600                                                                          
004700 FD  W47968D                                                              
004800     RECORDING       V                                                    
004900     BLOCK CONTAINS  0.                                                   
005000                                                                          
005100*01 POST  -COPY W479631  -PRE 68D-   -L.                                  
005200                                                                          
005300                                                                          
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600 77  IDPGM                       PIC X(8)    VALUE 'W4796700'.            
005700 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
005800 77  CURRENT-DP-SECTION          PIC X(16)   VALUE SPACE.                 
005900 77  CURRENT-KDMFUP              PIC X(2)    VALUE ZERO.                  
006000 77  CURRENT-IDDC                PIC X(2)    VALUE SPACE.                 
006100 77  CURRENT-IDDISTR             PIC 9(5)    VALUE ZERO.                  
006200 77  CURRENT-KDORDKL             PIC 9(1)    VALUE ZERO.                  
006300 77  CURRENT-ADCITY              PIC X(20)   VALUE SPACE.                 
006400                                                                          
006500 77  JA                          PIC X(1)    VALUE 'J'.                   
006600 77  NEJ                         PIC X(1)    VALUE 'N'.                   
006700                                                                          
006800 77  W47968B-EOF-SW              PIC X       VALUE 'N'.                   
006900     88  END-OF-W47968B                      VALUE 'J'.                   
007000                                                                          
007100 77  RPTNAME-EOF-SW              PIC X       VALUE 'N'.                   
007200     88  END-OF-RPTNAME                      VALUE 'J'.                   
007300                                                                          
007400 77  KDMFUP-SW                   PIC X       VALUE 'J'.                   
007500     88  FORSTA-KDMFUP                       VALUE 'J'.                   
007600                                                                          
007700 01  ARBETSVARIABLER.                                                     
007800     03  WS-SULEVANT-OK          PIC 9(7)    VALUE ZERO.                  
007900     03  WS-SURADER-OK           PIC 9(7)    VALUE ZERO.                  
008000     03  WS-SUKOLLI-OK           PIC 9(7)    VALUE ZERO.                  
008100     03  WS-KVORDER-OK           PIC 9(7)    VALUE ZERO.                  
008200     03  WS-VKORDBTO-OK          PIC 9(7)V9  VALUE ZERO.                  
008300     03  WS-PRARTNTO-OK          PIC 9(9)V99 VALUE ZERO.                  
008400                                                                          
008500     03  WS-SULEVANT-DI          PIC 9(7)    VALUE ZERO.                  
008600     03  WS-SURADER-DI           PIC 9(7)    VALUE ZERO.                  
008700     03  WS-SUKOLLI-DI           PIC 9(7)    VALUE ZERO.                  
008800     03  WS-KVORDER-DI           PIC 9(7)    VALUE ZERO.                  
008900     03  WS-VKORDBTO-DI          PIC 9(7)V9  VALUE ZERO.                  
009000     03  WS-PRARTNTO-DI          PIC 9(9)V99 VALUE ZERO.                  
009100                                                                          
009200     03  WS-SULEVANT-DC          PIC 9(7)    VALUE ZERO.                  
009300     03  WS-SURADER-DC           PIC 9(7)    VALUE ZERO.                  
009400     03  WS-SUKOLLI-DC           PIC 9(7)    VALUE ZERO.                  
009500     03  WS-KVORDER-DC           PIC 9(7)    VALUE ZERO.                  
009600     03  WS-VKORDBTO-DC          PIC 9(7)V9  VALUE ZERO.                  
009700     03  WS-PRARTNTO-DC          PIC 9(9)V99 VALUE ZERO.                  
009800                                                                          
009900     03  WS-SULEVANT-TOT         PIC 9(7)    VALUE ZERO.                  
010000     03  WS-SURADER-TOT          PIC 9(7)    VALUE ZERO.                  
010100     03  WS-SUKOLLI-TOT          PIC 9(7)    VALUE ZERO.                  
010200     03  WS-KVORDER-TOT          PIC 9(7)    VALUE ZERO.                  
010300     03  WS-VKORDBTO-TOT         PIC 9(7)V9  VALUE ZERO.                  
010400     03  WS-PRARTNTO-TOT         PIC 9(9)V99 VALUE ZERO.                  
010500                                                                          
010600                                                                          
010700 01  ERRTEXT.                                                             
010800     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
010900     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
011000 01  KDRC-DISPLAY                PIC Z(5).                                
011100                                                                          
011200                                                                          
011300 01  GENERAL-SUBPROGRAMS.                                                 
011400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011500                                                                          
011600                                                                          
011700 01  DAGENS-DATUM                PIC 9(6)  VALUE ZERO.                    
011800 01  FILLER REDEFINES DAGENS-DATUM.                                       
011900     03  DAGENS-DATUM-AAR        PIC 9(2).                                
012000     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
012100     03  DAGENS-DATUM-DAG        PIC 9(2).                                
012200 01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
012300                                                                          
012400*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
012500                                                                          
012600 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012700 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012800 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012900                                                                          
013000*    --- IN-AREOR                                                         
013100 01  FILLER                      PIC X(16)   VALUE                        
013200                                'INPUT-AREA     '.                        
013300*01  AREA  -COPY W47968B        -PRE INPUT-                               
013400                                                                          
013500                                                                          
013600 01  FILLER                      PIC X(16)   VALUE                        
013700                                'RPTNAME-AREA   '.                        
013800 01  RPTNAME-AREA.                                                        
013900     03 REPORT-NAME              PIC X(15).                               
014000     03 FILLER                   PIC X(65).                               
014100                                                                          
014200*    --- UT-AREOR                                                         
014300 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
014400                                'OUTPUT-AREA     '.                       
014500 01  W001-DAP.                                                            
014600     03  FILLER                  PIC X(165)  VALUE SPACE.                 
014700                                                                          
014800                                                                          
014900 01  W47968D-AREA.                                                        
015000*    03 -COPY W479631 -PRE OUT-                                           
015100                                                                          
015200                                                                          
015300                                                                          
015400 PROCEDURE DIVISION.                                                      
015500 MAIN SECTION.                                                            
015600                                                                          
015700     PERFORM A-INIT                                                       
015800     PERFORM S01-READ-W47968B                                             
015900                                                                          
016000     PERFORM UNTIL END-OF-W47968B                                         
016100                                                                          
016200        IF INPUT-KDMFUP NOT = CURRENT-KDMFUP                              
016300           PERFORM B-NEW-MFUP-GROUP                                       
016400        END-IF                                                            
016500                                                                          
016600        IF INPUT-IDDC-LEV = CURRENT-IDDC                                  
016700           IF INPUT-IDDISTR = CURRENT-IDDISTR                             
016800              IF INPUT-KDORDKL = CURRENT-KDORDKL                          
016900                 CONTINUE                                                 
017000              ELSE                                                        
017100* NY ORDEKLASS                                                            
017200                 PERFORM S10-KDORDKL-TOT                                  
017300                 MOVE INPUT-KDORDKL TO CURRENT-KDORDKL                    
017400              END-IF                                                      
017500           ELSE                                                           
017600* NYTT DISTRIKT                                                           
017700              PERFORM S10-KDORDKL-TOT                                     
017800              PERFORM S20-IDDISTR-TOT                                     
017900              MOVE INPUT-KDORDKL    TO CURRENT-KDORDKL                    
018000              MOVE INPUT-IDDISTR    TO CURRENT-IDDISTR                    
018100           END-IF                                                         
018200        ELSE                                                              
018300* NYTT DC                                                                 
018400           PERFORM S10-KDORDKL-TOT                                        
018500           PERFORM S20-IDDISTR-TOT                                        
018600           PERFORM S30-IDDC-TOT                                           
018700           MOVE INPUT-KDORDKL       TO CURRENT-KDORDKL                    
018800           MOVE INPUT-IDDISTR       TO CURRENT-IDDISTR                    
018900           MOVE INPUT-IDDC-LEV      TO CURRENT-IDDC                       
019000           MOVE INPUT-ADCITY        TO CURRENT-ADCITY                     
019100        END-IF                                                            
019200                                                                          
019300        ADD INPUT-KVLEVART2 TO WS-SULEVANT-OK                             
019400                               WS-SULEVANT-DI                             
019500                               WS-SULEVANT-DC                             
019600                               WS-SULEVANT-TOT                            
019700        ADD INPUT-KVRADER   TO WS-SURADER-OK                              
019800                               WS-SURADER-DI                              
019900                               WS-SURADER-DC                              
020000                               WS-SURADER-TOT                             
020100        ADD INPUT-KVKOLLI   TO WS-SUKOLLI-OK                              
020200                               WS-SUKOLLI-DI                              
020300                               WS-SUKOLLI-DC                              
020400                               WS-SUKOLLI-TOT                             
020500        ADD INPUT-KVORDER   TO WS-KVORDER-OK                              
020600                               WS-KVORDER-DI                              
020700                               WS-KVORDER-DC                              
020800                               WS-KVORDER-TOT                             
020900        ADD INPUT-VKORDBTO  TO WS-VKORDBTO-OK                             
021000                               WS-VKORDBTO-DI                             
021100                               WS-VKORDBTO-DC                             
021200                               WS-VKORDBTO-TOT                            
021300        ADD INPUT-PRARTNTO  TO WS-PRARTNTO-OK                             
021400                               WS-PRARTNTO-DI                             
021500                               WS-PRARTNTO-DC                             
021600                               WS-PRARTNTO-TOT                            
021700                                                                          
021800        PERFORM S01-READ-W47968B                                          
021900     END-PERFORM                                                          
022000                                                                          
022100     IF CURRENT-IDDC NOT = SPACE                                          
022200        PERFORM S10-KDORDKL-TOT                                           
022300        PERFORM S20-IDDISTR-TOT                                           
022400        PERFORM S30-IDDC-TOT                                              
022500        PERFORM S40-MFUP-GROUP-TOTAL                                      
022600     END-IF                                                               
022700                                                                          
022800     PERFORM Z-FINIT                                                      
022900                                                                          
023000     MOVE ZERO TO RETURN-CODE                                             
023100     GOBACK                                                               
023200     .                                                                    
023300                                                                          
023400 A-INIT SECTION.                                                          
023500     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
023600                                                                          
023700     OPEN INPUT  W47968B                                                  
023800     OPEN OUTPUT W47968D                                                  
023900     ACCEPT DAGENS-DATUM     FROM DATE                                    
024000     ACCEPT DAGENS-TID       FROM TIME                                    
024100                                                                          
024200*    -- HÄMTA RAPPORTENS NAMN FRÅN SYSIN I JCL:EN                         
024300     OPEN INPUT RPTNAME                                                   
024400     READ RPTNAME INTO RPTNAME-AREA                                       
024500       AT END                                                             
024600         SET END-OF-RPTNAME TO TRUE                                       
024700     END-READ                                                             
024800     IF END-OF-RPTNAME                                                    
024900       STRING 'DD R46966D2 IS EMPTY'                                      
025000              DELIMITED BY SIZE INTO ERRTEXT-STR                          
025100       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
025200     END-IF                                                               
025300     CLOSE RPTNAME                                                        
025400     .                                                                    
025500                                                                          
025600 B-NEW-MFUP-GROUP  SECTION.                                               
025700     MOVE 'B-NEW-MFUP-GROUP ' TO CURRENT-SECTION                          
025800                                                                          
025900     IF FORSTA-KDMFUP                                                     
026000        MOVE NEJ TO KDMFUP-SW                                             
026100     ELSE                                                                 
026200        PERFORM S10-KDORDKL-TOT                                           
026300        PERFORM S20-IDDISTR-TOT                                           
026400        PERFORM S30-IDDC-TOT                                              
026500        PERFORM S40-MFUP-GROUP-TOTAL                                      
026600     END-IF                                                               
026700     MOVE INPUT-KDMFUP   TO CURRENT-KDMFUP                                
026800     MOVE INPUT-IDDC-LEV TO CURRENT-IDDC                                  
026900     MOVE INPUT-IDDISTR  TO CURRENT-IDDISTR                               
027000     MOVE INPUT-KDORDKL  TO CURRENT-KDORDKL                               
027100     MOVE INPUT-ADCITY   TO CURRENT-ADCITY                                
027200                                                                          
027300     MOVE ZERO           TO WS-SULEVANT-OK                                
027400                            WS-SULEVANT-DI                                
027500                            WS-SULEVANT-DC                                
027600                            WS-SULEVANT-TOT                               
027700                            WS-SURADER-OK                                 
027800                            WS-SURADER-DI                                 
027900                            WS-SURADER-DC                                 
028000                            WS-SURADER-TOT                                
028100                            WS-SUKOLLI-OK                                 
028200                            WS-SUKOLLI-DI                                 
028300                            WS-SUKOLLI-DC                                 
028400                            WS-SUKOLLI-TOT                                
028500                            WS-KVORDER-OK                                 
028600                            WS-KVORDER-DI                                 
028700                            WS-KVORDER-DC                                 
028800                            WS-KVORDER-TOT                                
028900                            WS-VKORDBTO-OK                                
029000                            WS-VKORDBTO-DI                                
029100                            WS-VKORDBTO-DC                                
029200                            WS-VKORDBTO-TOT                               
029300                            WS-PRARTNTO-OK                                
029400                            WS-PRARTNTO-DI                                
029500                            WS-PRARTNTO-DC                                
029600                            WS-PRARTNTO-TOT                               
029700                                                                          
029800     PERFORM S02-SKRIV-W47968D-DAP                                        
029900     .                                                                    
030000                                                                          
030100 S01-READ-W47968B SECTION.                                                
030200     MOVE 'S01-READ-W47968B ' TO CURRENT-SECTION                          
030300                                                                          
030400     READ W47968B INTO INPUT-W47968B                                      
030500       AT END                                                             
030600         SET END-OF-W47968B TO TRUE                                       
030700     END-READ                                                             
030800     .                                                                    
030900                                                                          
031000                                                                          
031100 S02-SKRIV-W47968D-REC SECTION.                                           
031200     MOVE 'S02-SKRIV-REC'        TO CURRENT-SECTION.                      
031300                                                                          
031400     WRITE 68D-POST FROM W47968D-AREA                                     
031500     .                                                                    
031600                                                                          
031700 S02-SKRIV-W47968D-DAP SECTION.                                           
031800     MOVE 'S02-SKRIV-DAP'        TO CURRENT-SECTION.                      
031900                                                                          
032000     MOVE '¤DAPMAN-INVDC-P'      TO W001-DAP                              
032100     WRITE 68D-POST FROM W001-DAP                                         
032200                                                                          
032300     MOVE SPACE TO W001-DAP                                               
032400     STRING '¤DAP' CURRENT-KDMFUP ' W47967-001'                           
032500     DELIMITED BY SIZE INTO W001-DAP                                      
032600                                                                          
032700     WRITE 68D-POST FROM W001-DAP                                         
032800     .                                                                    
032900                                                                          
033000 S10-KDORDKL-TOT  SECTION.                                                
033100     MOVE 'S10-KDORDKL-TOT   ' TO CURRENT-SECTION                         
033200                                                                          
033300     MOVE '1'                TO OUT-IDAFPRCD                              
033400     MOVE CURRENT-KDMFUP     TO OUT-KDMFUP                                
033500     MOVE CURRENT-IDDC       TO OUT-IDDC                                  
033600     MOVE CURRENT-IDDISTR    TO OUT-IDDISTR                               
033700     MOVE CURRENT-KDORDKL    TO OUT-KDORDKL                               
033800     MOVE CURRENT-ADCITY     TO OUT-ADCITY                                
033900                                                                          
034000     MOVE INPUT-TIAARP       TO OUT-TIAARP                                
034100     MOVE WS-SULEVANT-OK     TO OUT-SULEVANT                              
034200     MOVE WS-SURADER-OK      TO OUT-SURADER                               
034300     MOVE WS-SUKOLLI-OK      TO OUT-SUKOLLI                               
034400     MOVE WS-KVORDER-OK      TO OUT-KVORDER                               
034500     MOVE WS-VKORDBTO-OK     TO OUT-VKORDBTO-KOLLI                        
034600     MOVE WS-PRARTNTO-OK     TO OUT-SUACKSTD                              
034700                                                                          
034800     PERFORM S02-SKRIV-W47968D-REC                                        
034900                                                                          
035000     MOVE ZERO               TO WS-SULEVANT-OK                            
035100                                WS-SURADER-OK                             
035200                                WS-SUKOLLI-OK                             
035300                                WS-KVORDER-OK                             
035400                                WS-VKORDBTO-OK                            
035500                                WS-PRARTNTO-OK                            
035600     .                                                                    
035700                                                                          
035800 S20-IDDISTR-TOT  SECTION.                                                
035900     MOVE 'S20-IDDISTR-TOT   ' TO CURRENT-SECTION                         
036000                                                                          
036100     MOVE '2'                TO OUT-IDAFPRCD                              
036200                                                                          
036300     MOVE CURRENT-KDMFUP     TO OUT-KDMFUP                                
036400     MOVE CURRENT-IDDC       TO OUT-IDDC                                  
036500     MOVE CURRENT-IDDISTR    TO OUT-IDDISTR                               
036600     MOVE CURRENT-KDORDKL    TO OUT-KDORDKL                               
036700     MOVE CURRENT-ADCITY     TO OUT-ADCITY                                
036800                                                                          
036900     MOVE INPUT-TIAARP       TO OUT-TIAARP                                
037000     MOVE WS-SULEVANT-DI     TO OUT-SULEVANT                              
037100     MOVE WS-SURADER-DI      TO OUT-SURADER                               
037200     MOVE WS-SUKOLLI-DI      TO OUT-SUKOLLI                               
037300     MOVE WS-KVORDER-DI      TO OUT-KVORDER                               
037400     MOVE WS-VKORDBTO-DI     TO OUT-VKORDBTO-KOLLI                        
037500     MOVE WS-PRARTNTO-DI     TO OUT-SUACKSTD                              
037600                                                                          
037700     PERFORM S02-SKRIV-W47968D-REC                                        
037800                                                                          
037900     MOVE ZERO               TO WS-SULEVANT-DI                            
038000                                WS-SURADER-DI                             
038100                                WS-SUKOLLI-DI                             
038200                                WS-KVORDER-DI                             
038300                                WS-VKORDBTO-DI                            
038400                                WS-PRARTNTO-DI                            
038500     .                                                                    
038600                                                                          
038700 S30-IDDC-TOT     SECTION.                                                
038800     MOVE 'S30-IDDC-TOT      ' TO CURRENT-SECTION                         
038900                                                                          
039000     MOVE '3'                TO OUT-IDAFPRCD                              
039100                                                                          
039200     MOVE CURRENT-KDMFUP     TO OUT-KDMFUP                                
039300     MOVE CURRENT-IDDC       TO OUT-IDDC                                  
039400     MOVE CURRENT-IDDISTR    TO OUT-IDDISTR                               
039500     MOVE CURRENT-KDORDKL    TO OUT-KDORDKL                               
039600     MOVE CURRENT-ADCITY     TO OUT-ADCITY                                
039700                                                                          
039800     MOVE INPUT-TIAARP       TO OUT-TIAARP                                
039900     MOVE WS-SULEVANT-DC     TO OUT-SULEVANT                              
040000     MOVE WS-SURADER-DC      TO OUT-SURADER                               
040100     MOVE WS-SUKOLLI-DC      TO OUT-SUKOLLI                               
040200     MOVE WS-KVORDER-DC      TO OUT-KVORDER                               
040300     MOVE WS-VKORDBTO-DC     TO OUT-VKORDBTO-KOLLI                        
040400     MOVE WS-PRARTNTO-DC     TO OUT-SUACKSTD                              
040500                                                                          
040600     PERFORM S02-SKRIV-W47968D-REC                                        
040700                                                                          
040800     MOVE ZERO               TO WS-SULEVANT-DC                            
040900                                WS-SURADER-DC                             
041000                                WS-SUKOLLI-DC                             
041100                                WS-KVORDER-DC                             
041200                                WS-VKORDBTO-DC                            
041300                                WS-PRARTNTO-DC                            
041400                                                                          
041500     .                                                                    
041600                                                                          
041700 S40-MFUP-GROUP-TOTAL SECTION.                                            
041800     MOVE 'S40-MFUP-GROUP-TOTAL' TO CURRENT-SECTION                       
041900                                                                          
042000     MOVE '4'                TO OUT-IDAFPRCD                              
042100                                                                          
042200     MOVE CURRENT-KDMFUP     TO OUT-KDMFUP                                
042300     MOVE CURRENT-IDDC       TO OUT-IDDC                                  
042400     MOVE CURRENT-IDDISTR    TO OUT-IDDISTR                               
042500     MOVE CURRENT-KDORDKL    TO OUT-KDORDKL                               
042600     MOVE CURRENT-ADCITY     TO OUT-ADCITY                                
042700                                                                          
042800     MOVE INPUT-TIAARP       TO OUT-TIAARP                                
042900     MOVE WS-SULEVANT-TOT    TO OUT-SULEVANT                              
043000     MOVE WS-SURADER-TOT     TO OUT-SURADER                               
043100     MOVE WS-SUKOLLI-TOT     TO OUT-SUKOLLI                               
043200     MOVE WS-KVORDER-TOT     TO OUT-KVORDER                               
043300     MOVE WS-VKORDBTO-TOT    TO OUT-VKORDBTO-KOLLI                        
043400     MOVE WS-PRARTNTO-TOT    TO OUT-SUACKSTD                              
043500                                                                          
043600     PERFORM S02-SKRIV-W47968D-REC                                        
043700                                                                          
043800     MOVE ZERO               TO WS-SULEVANT-TOT                           
043900                                WS-SURADER-TOT                            
044000                                WS-SUKOLLI-TOT                            
044100                                WS-KVORDER-TOT                            
044200                                WS-VKORDBTO-TOT                           
044300                                WS-PRARTNTO-TOT                           
044400                                                                          
044500     .                                                                    
044600                                                                          
044700 Z-FINIT SECTION.                                                         
044800     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
044900                                                                          
045000                                                                          
045100     CLOSE W47968B                                                        
045200           W47968D                                                        
045300     .                                                                    
045400                                                                          
