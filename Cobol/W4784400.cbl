000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4784400.                                                
000300 AUTHOR.         GÖRAN KJELLSON                                           
000400 DATE-WRITTEN.   NOVEMBER 2011.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*    PROGRAMMET LÄSER EXTRAKTFIL MED FAKTURAINFORMATION PER DC.           
000900*    FÖR VARJE DC SKICKAS DATA TILL D&P, VIA DAP3,                        
001000*    SOM LAGRAR DET FÖR MANAGEMENT FOLLOW-UP I XDC-SYSTEMET               
001100*                                                                         
001200*    PROGRAMMET KÖRS DAGLIGEN SAMT VID VECKO- OCH PERIODSLUT, SÅ          
001300*    FOLLOW-UP-RAPPORTERNAS NAMN ÄR OLIKA BEROENDE PÅ KÖRNINGS-           
001400*    TILLFÄLLE OCH LÄSES IN VIA "SYSIN" I JCL:EN.                         
001500*    DAGLIG...............: W47844-001                                    
001510*    VECKOSLUT............: W47844-002                                    
001600*    PERIODSLUT...........: W47844-003                                    
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900 INPUT-OUTPUT SECTION.                                                    
002000 FILE-CONTROL.                                                            
002100*    --- LEVERANSINFORMATION SENASTE DAG/VECKAN/PERIODEN PER DC           
002200     SELECT W47843                     ASSIGN TO W47844D1.                
002300                                                                          
002400*    --- RAPPORTNAMN VIA SYSIN                                            
002500     SELECT RPTNAME                    ASSIGN TO W47844D2.                
002600                                                                          
002710*    --- LEVERANSINFORMATION SENASTE D/V/P TILL DAP3                      
002720     SELECT W47844                     ASSIGN TO W47844D3.                
002721                                                                          
002722                                                                          
002800 DATA DIVISION.                                                           
002900                                                                          
003000 FILE SECTION.                                                            
003100 FD  W47843                                                               
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400                                                                          
003500*01 -COPY W47843    -L.                                                   
003600                                                                          
003700                                                                          
003800 FD  RPTNAME                                                              
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200 01  FILLER          PIC X(80).                                           
004400                                                                          
004500                                                                          
004530 FD  W47844                                                               
004540     RECORDING       V                                                    
004550     BLOCK CONTAINS  0.                                                   
004551                                                                          
004552*01 POST  -COPY W47844   -PRE OUT-   -L.                                  
004553                                                                          
004554                                                                          
004555 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W4784400'.            
004800 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
004900 77  CURRENT-DP-SECTION          PIC X(16)   VALUE SPACE.                 
004910                                                                          
005000 77  CURRENT-IDDC                PIC X(2)    VALUE SPACE.                 
005001 77  CURRENT-ADCITY              PIC X(20)   VALUE SPACE.                 
005010 77  CURRENT-IDPRC               PIC X(4)    VALUE SPACE.                 
005030 77  CURRENT-KDORDKL             PIC 9(1)    VALUE ZERO.                  
005040 77  CURRENT-TIRFSTID            PIC 9(4)    VALUE ZERO.                  
005100                                                                          
005200 77  JA                          PIC X(1)    VALUE 'J'.                   
005300 77  NEJ                         PIC X(1)    VALUE 'N'.                   
005400                                                                          
005410 77  SW-VECKA-PERIOD             PIC X(1)    VALUE SPACE.                 
005420     88 DAG                                  VALUE 'D'.                   
005421     88 VECKA                                VALUE 'V'.                   
005430     88 PERIOD                               VALUE 'P'.                   
005800                                                                          
005900 77  W47843-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W47843                       VALUE 'J'.                   
006100                                                                          
006200 77  RPTNAME-EOF-SW              PIC X       VALUE 'N'.                   
006300     88  END-OF-RPTNAME                      VALUE 'J'.                   
006400                                                                          
006500 77  DC-SW                       PIC X       VALUE 'J'.                   
006600     88  FORSTA-DC                           VALUE 'J'.                   
006700                                                                          
006710 01  ARBETSVARIABLER.                                                     
006720     03  WS-KVORDER              PIC 9(7)      VALUE ZERO.                
006730     03  WS-KVORDER-UTSKR        PIC 9(7)      VALUE ZERO.                
006740     03  WS-KVORDER-PACK         PIC 9(7)      VALUE ZERO.                
006750     03  WS-KVORDRAD             PIC 9(7)      VALUE ZERO.                
006760     03  WS-KVORDRAD-UTSKR       PIC 9(7)      VALUE ZERO.                
006761     03  WS-KVORDRAD-PACK        PIC 9(7)      VALUE ZERO.                
006762     03  WS-KVKOLLI              PIC 9(7)      VALUE ZERO.                
006763     03  WS-KVKOLLI-LAST         PIC 9(7)      VALUE ZERO.                
006764     03  WS-PROCENT              PIC 9(3)V9(4) VALUE ZERO.                
006770                                                                          
006818                                                                          
006820                                                                          
006900 01  ERRTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
007100     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200 01  KDRC-DISPLAY                PIC Z(5).                                
007300                                                                          
007400                                                                          
007500 01  GENERAL-SUBPROGRAMS.                                                 
007600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007700     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
007800                                                                          
007900                                                                          
008000 01  DAGENS-DATUM                PIC 9(6)  VALUE ZERO.                    
008100 01  FILLER REDEFINES DAGENS-DATUM.                                       
008200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008500 01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
008600                                                                          
008700*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008800                                                                          
008900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
009100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
009200                                                                          
009300                                                                          
009900*    --- IN-AREOR                                                         
010000 01  FILLER                      PIC X(16)   VALUE                        
010100                                'INPUT-AREA     '.                        
010200*01  AREA  -COPY W47843         -PRE IN-                                  
010300                                                                          
010400                                                                          
010500 01  FILLER                      PIC X(16)   VALUE                        
010600                                'RPTNAME-AREA   '.                        
010700 01  RPTNAME-AREA.                                                        
010800     03 REPORT-NAME              PIC X(15).                               
010900     03 FILLER                   PIC X(65).                               
011000                                                                          
011100*    --- UT-AREOR                                                         
011200 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
011300                                'OUTPUT-AREA     '.                       
011400 01  W001-DAP.                                                            
011500     03  FILLER                  PIC X(165)  VALUE SPACE.                 
011600                                                                          
011700                                                                          
011800 01  W47844-AREA.                                                         
011900*    03 -COPY W47844  -PRE OUT-                                           
012100                                                                          
012200                                                                          
012300                                                                          
012400 PROCEDURE DIVISION.                                                      
012900 MAIN SECTION.                                                            
013220                                                                          
013300     PERFORM A-INIT                                                       
013400     PERFORM S01-READ-W47843                                              
013500                                                                          
013600     PERFORM UNTIL END-OF-W47843                                          
013610                                                                          
013620        IF IN-IDDC NOT = CURRENT-IDDC                                     
013710           PERFORM B-NEW-IDDC                                             
014600        END-IF                                                            
014700                                                                          
014800        IF IN-IDPRC    = CURRENT-IDPRC    AND                             
014900           IN-KDORDKL  = CURRENT-KDORDKL  AND                             
015000           IN-TIRFSTID = CURRENT-TIRFSTID                                 
015100                                                                          
015101           CONTINUE                                                       
015102        ELSE                                                              
015103* NY TIRFSTID                                                             
015104           PERFORM S10-TIRFSTID-TOT                                       
015105           MOVE IN-IDPRC         TO CURRENT-IDPRC                         
015106           MOVE IN-KDORDKL       TO CURRENT-KDORDKL                       
015107           MOVE IN-TIRFSTID      TO CURRENT-TIRFSTID                      
015108        END-IF                                                            
015109                                                                          
015110        ADD IN-KVORDER           TO WS-KVORDER                            
015120        ADD IN-KVORDER-UTSKR     TO WS-KVORDER-UTSKR                      
015130        ADD IN-KVORDER-PACK      TO WS-KVORDER-PACK                       
015140        ADD IN-KVORDRAD          TO WS-KVORDRAD                           
015150        ADD IN-KVORDRAD-UTSKR    TO WS-KVORDRAD-UTSKR                     
015160        ADD IN-KVORDRAD-PACK     TO WS-KVORDRAD-PACK                      
015170        ADD IN-KVKOLLI           TO WS-KVKOLLI                            
015171        ADD IN-KVKOLLI-LAST      TO WS-KVKOLLI-LAST                       
015172                                                                          
015173        PERFORM S01-READ-W47843                                           
015200     END-PERFORM                                                          
015300                                                                          
015310     IF CURRENT-IDDC NOT = SPACE                                          
015400        PERFORM S10-TIRFSTID-TOT                                          
015500     END-IF                                                               
015600                                                                          
015700     PERFORM Z-FINIT                                                      
015800                                                                          
015900     MOVE ZERO TO RETURN-CODE                                             
016000     GOBACK                                                               
016100     .                                                                    
016200                                                                          
016300 A-INIT SECTION.                                                          
016400     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
016500                                                                          
016600     OPEN INPUT  W47843                                                   
016610     OPEN OUTPUT W47844                                                   
016700     ACCEPT DAGENS-DATUM     FROM DATE                                    
016800     ACCEPT DAGENS-TID       FROM TIME                                    
016900                                                                          
017000*    -- HÄMTA RAPPORTENS NAMN FRÅN SYSIN I JCL:EN                         
017100     OPEN INPUT RPTNAME                                                   
017200     READ RPTNAME INTO RPTNAME-AREA                                       
017300       AT END                                                             
017400         SET END-OF-RPTNAME TO TRUE                                       
017500     END-READ                                                             
017600     IF END-OF-RPTNAME                                                    
017700       STRING 'DD R46966D2 IS EMPTY'                                      
017800              DELIMITED BY SIZE INTO ERRTEXT-STR                          
017900       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
018000     END-IF                                                               
018100     CLOSE RPTNAME                                                        
018200                                                                          
018210     IF REPORT-NAME = 'W47844-001'                                        
018220        SET DAG    TO TRUE                                                
018230     ELSE                                                                 
018231        IF REPORT-NAME = 'W47844-002'                                     
018240           SET VECKA  TO TRUE                                             
018241        ELSE                                                              
018242           SET PERIOD TO TRUE                                             
018243        END-IF                                                            
018250     END-IF                                                               
018300     .                                                                    
018400                                                                          
018500 B-NEW-IDDC    SECTION.                                                   
018600     MOVE 'B-NEW-IDDC      ' TO CURRENT-SECTION                           
018700                                                                          
018710     IF FORSTA-DC                                                         
018720        MOVE NEJ TO DC-SW                                                 
018730     ELSE                                                                 
018731        PERFORM S10-TIRFSTID-TOT                                          
018750     END-IF                                                               
018760           MOVE IN-IDDC          TO CURRENT-IDDC                          
018770           MOVE IN-ADCITY        TO CURRENT-ADCITY                        
018780           MOVE IN-IDPRC         TO CURRENT-IDPRC                         
018790           MOVE IN-KDORDKL       TO CURRENT-KDORDKL                       
018791           MOVE IN-TIRFSTID      TO CURRENT-TIRFSTID                      
018792                                                                          
018793     MOVE ZERO           TO WS-KVORDER                                    
018794                            WS-KVORDER-UTSKR                              
018795                            WS-KVORDER-PACK                               
018796                            WS-KVORDRAD                                   
018797                            WS-KVORDRAD-UTSKR                             
018798                            WS-KVORDRAD-PACK                              
018799                            WS-KVKOLLI                                    
018800                            WS-KVKOLLI-LAST                               
018900                                                                          
018901     PERFORM S02-SKRIV-W47844-DAP                                         
018902     .                                                                    
018903                                                                          
022000 S01-READ-W47843  SECTION.                                                
022100     MOVE 'S01-READ-W47843  ' TO CURRENT-SECTION                          
022200                                                                          
022300     READ W47843  INTO IN-W47843                                          
022400       AT END                                                             
022500         SET END-OF-W47843  TO TRUE                                       
022600     END-READ                                                             
022700     .                                                                    
022800                                                                          
022900 S02-SKRIV-W47844-REC SECTION.                                            
032700     MOVE 'S02-SKRIV-REC'        TO CURRENT-SECTION.                      
032800                                                                          
032900     WRITE OUT-POST FROM W47844-AREA                                      
033000     .                                                                    
033100                                                                          
033200 S02-SKRIV-W47844-DAP SECTION.                                            
033300     MOVE 'S02-SKRIV-DAP'        TO CURRENT-SECTION.                      
033400                                                                          
033800     IF DAG                                                               
033801       MOVE '¤DAPDELRFS-D'         TO W001-DAP                            
034000     ELSE                                                                 
034100        IF VECKA                                                          
034110           MOVE '¤DAPDELRFS-W'         TO W001-DAP                        
034300        ELSE                                                              
034310           MOVE '¤DAPDELRFS-P'         TO W001-DAP                        
034500        END-IF                                                            
034600     END-IF                                                               
034700     WRITE OUT-POST FROM W001-DAP                                         
034701                                                                          
034702     IF DAG                                                               
034703       STRING '¤DAP' CURRENT-IDDC 'W47844-001'                            
034704       DELIMITED BY SIZE INTO W001-DAP                                    
034705                                                                          
034706     ELSE                                                                 
034707        IF VECKA                                                          
034708           STRING '¤DAP' CURRENT-IDDC 'W47844-002'                        
034709           DELIMITED BY SIZE INTO W001-DAP                                
034710        ELSE                                                              
034711           STRING '¤DAP' CURRENT-IDDC 'W47844-003'                        
034712           DELIMITED BY SIZE INTO W001-DAP                                
034713        END-IF                                                            
034714     END-IF                                                               
034715     WRITE OUT-POST FROM W001-DAP                                         
034720     .                                                                    
034721 S10-TIRFSTID-TOT SECTION.                                                
034722     MOVE 'S10-TIRFSTID-TOT  ' TO CURRENT-SECTION                         
034723                                                                          
034724     MOVE '1'                   TO OUT-IDAFPRCD                           
034726     MOVE CURRENT-IDDC          TO OUT-IDDC                               
034727     MOVE CURRENT-ADCITY        TO OUT-ADCITY                             
034728     MOVE CURRENT-IDPRC         TO OUT-IDPRC                              
034729     MOVE CURRENT-KDORDKL       TO OUT-KDORDKL                            
034730     MOVE CURRENT-TIRFSTID(1:2) TO OUT-TIRFSTID(1:2)                      
034740     MOVE ':'                   TO OUT-TIRFSTID(3:1)                      
034750     MOVE CURRENT-TIRFSTID(3:2) TO OUT-TIRFSTID(4:2)                      
034760     MOVE IN-TIDATUM            TO OUT-TIDATUM                            
034770                                                                          
034780     IF DAG OR VECKA                                                      
034790        MOVE IN-TIAAVV          TO OUT-TIAAVV                             
034800     ELSE                                                                 
034900        MOVE IN-TIAARP          TO OUT-TIAARP                             
035000     END-IF                                                               
035100     MOVE WS-KVORDER            TO OUT-KVORDER                            
035200     MOVE WS-KVORDER-UTSKR      TO OUT-KVORDER-UTSKR                      
035300     MOVE WS-KVORDER-PACK       TO OUT-KVORDER-PACK                       
035400     IF WS-KVORDER > ZERO                                                 
035500        COMPUTE WS-PROCENT ROUNDED =                                      
035600         100 * (WS-KVORDER-UTSKR / WS-KVORDER)                            
035700        COMPUTE OUT-KVORDER-UTSKR-PROC ROUNDED = WS-PROCENT * 1           
035800                                                                          
035900        COMPUTE WS-PROCENT ROUNDED =                                      
036000         100 * (WS-KVORDER-PACK  / WS-KVORDER)                            
036100        COMPUTE OUT-KVORDER-PACK-PROC ROUNDED = WS-PROCENT * 1            
036200     ELSE                                                                 
036300        MOVE ZERO               TO OUT-KVORDER-UTSKR-PROC                 
036400                                   OUT-KVORDER-PACK-PROC                  
036500     END-IF                                                               
036600                                                                          
036700     MOVE WS-KVORDRAD           TO OUT-KVORDRAD                           
036800     MOVE WS-KVORDRAD-UTSKR     TO OUT-KVORDRAD-UTSKR                     
036900     MOVE WS-KVORDRAD-PACK      TO OUT-KVORDRAD-PACK                      
037000     IF WS-KVORDRAD > ZERO                                                
037100        COMPUTE WS-PROCENT ROUNDED =                                      
037200         100 * (WS-KVORDRAD-UTSKR / WS-KVORDRAD)                          
037300        COMPUTE OUT-KVORDRAD-UTSKR-PROC ROUNDED = WS-PROCENT * 1          
037400                                                                          
037500        COMPUTE WS-PROCENT ROUNDED =                                      
037600         100 * (WS-KVORDRAD-PACK / WS-KVORDRAD)                           
037700        COMPUTE OUT-KVORDRAD-PACK-PROC ROUNDED = WS-PROCENT * 1           
037800     ELSE                                                                 
037900        MOVE ZERO               TO OUT-KVORDRAD-UTSKR-PROC                
038000                                   OUT-KVORDRAD-PACK-PROC                 
038100     END-IF                                                               
038200                                                                          
038300     MOVE WS-KVKOLLI            TO OUT-KVKOLLI                            
038400     MOVE WS-KVKOLLI-LAST       TO OUT-KVKOLLI-LAST                       
038500     IF WS-KVKOLLI > ZERO                                                 
038600        COMPUTE WS-PROCENT ROUNDED =                                      
038700         100 * (WS-KVKOLLI-LAST / WS-KVKOLLI)                             
038800        COMPUTE OUT-KVKOLLI-PROC ROUNDED = WS-PROCENT * 1                 
038900     ELSE                                                                 
039000        MOVE ZERO               TO OUT-KVKOLLI-PROC                       
039001     END-IF                                                               
039002                                                                          
039003     PERFORM S02-SKRIV-W47844-REC                                         
039004                                                                          
039005     MOVE ZERO           TO WS-KVORDER                                    
039006                            WS-KVORDER-UTSKR                              
039007                            WS-KVORDER-PACK                               
039008                            WS-KVORDRAD                                   
039009                            WS-KVORDRAD-UTSKR                             
039010                            WS-KVORDRAD-PACK                              
039020                            WS-KVKOLLI                                    
039021                            WS-KVKOLLI-LAST                               
039022     .                                                                    
039023                                                                          
039024                                                                          
039025 Z-FINIT SECTION.                                                         
039026     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
039027                                                                          
039028                                                                          
039029     CLOSE W47843                                                         
039030           W47844                                                         
039031     .                                                                    
039032                                                                          
