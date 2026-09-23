000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4784500.                                                
000300 AUTHOR.         GÖRAN KJELLSON                                           
000400 DATE-WRITTEN.   NOVEMBER 2011.                                           
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*    PROGRAMMET LÄSER EXTRAKTFIL MED FAKTURAINFORMATION PER KMFUP.        
000900*    FÖR VARJE KDMFUP SKICKAS DATA TILL D&P, VIA DAP3,                    
001000*    SOM LAGRAR DET FÖR MANAGEMENT FOLLOW-UP I XDC-SYSTEMET               
001100*                                                                         
001200*    PROGRAMMET KÖRS DAGLIGEN SAMT VID VECKO- OCH PERIODSLUT, SÅ          
001300*    FOLLOW-UP-RAPPORTERNAS NAMN ÄR OLIKA BEROENDE PÅ KÖRNINGS-           
001400*    TILLFÄLLE OCH LÄSES IN VIA "SYSIN" I JCL:EN.                         
001500*    VECKOSLUT............: W47845-001                                    
001600*    PERIODSLUT...........: W47845-002                                    
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900 INPUT-OUTPUT SECTION.                                                    
002000 FILE-CONTROL.                                                            
002100*    --- LEVERANSINFORMATION SENASTE DAG/VECKAN/PERIODEN PER DC           
002200     SELECT W47843                     ASSIGN TO W47845D1.                
002300                                                                          
002400*    --- RAPPORTNAMN VIA SYSIN                                            
002500     SELECT RPTNAME                    ASSIGN TO W47845D2.                
002600                                                                          
002710*    --- LEVERANSINFORMATION SENASTE D/V/P TILL DAP3                      
002720     SELECT W47845                     ASSIGN TO W47845D3.                
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
004530 FD  W47845                                                               
004540     RECORDING       V                                                    
004550     BLOCK CONTAINS  0.                                                   
004551                                                                          
004552*01 POST  -COPY W47844   -PRE OUT-   -L.                                  
004553                                                                          
004554                                                                          
004555 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W4784500'.            
004800 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
004900 77  CURRENT-DP-SECTION          PIC X(16)   VALUE SPACE.                 
004910                                                                          
005000 77  CURRENT-KDMFUP              PIC X(2)    VALUE ZERO.                  
005010 77  CURRENT-IDDC                PIC X(2)    VALUE SPACE.                 
005020 77  CURRENT-ADCITY              PIC X(20)   VALUE SPACE.                 
005030 77  CURRENT-KDORDKL             PIC 9(1)    VALUE ZERO.                  
005040 77  CURRENT-TIRFSTID            PIC 9(4)    VALUE ZERO.                  
005100                                                                          
005200 77  JA                          PIC X(1)    VALUE 'J'.                   
005300 77  NEJ                         PIC X(1)    VALUE 'N'.                   
005400                                                                          
005410 77  SW-VECKA-PERIOD             PIC X(1)    VALUE SPACE.                 
005420     88 VECKA                                VALUE 'V'.                   
005430     88 PERIOD                               VALUE 'P'.                   
005800                                                                          
005900 77  W47843-EOF-SW               PIC X       VALUE 'N'.                   
006000     88  END-OF-W47843                       VALUE 'J'.                   
006100                                                                          
006200 77  RPTNAME-EOF-SW              PIC X       VALUE 'N'.                   
006300     88  END-OF-RPTNAME                      VALUE 'J'.                   
006400                                                                          
006500 77  KDMFUP-SW                   PIC X       VALUE 'J'.                   
006600     88  FORSTA-KDMFUP                       VALUE 'J'.                   
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
007710     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
007800                                                                          
007900                                                                          
008000 01  DAGENS-DATUM                PIC 9(6)  VALUE ZERO.                    
008100 01  FILLER REDEFINES DAGENS-DATUM.                                       
008200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
008300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008500 01  DAGENS-TID                  PIC 9(8)    VALUE ZERO.                  
008600                                                                          
008700*    --- PARAMETERS FOR SUBPROGRAM WL10WBDC                               
008800                                                                          
008810 01  FILLER                      PIC X(16)  VALUE 'WL10WBDC AREA'.        
008900*01  -COPY WL10WBDC                                                       
008910                                                                          
008920*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
008930                                                                          
008940 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
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
011500     03  FILLER                  PIC X(004)  VALUE SPACE.                 
011510     03  W001-KDMFUP             PIC X(002)  VALUE SPACE.                 
011520     03  FILLER                  PIC X(159)  VALUE SPACE.                 
011600                                                                          
011700                                                                          
011800 01  W47845-AREA.                                                         
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
013620        IF IN-KDMFUP NOT = CURRENT-KDMFUP                                 
013630           PERFORM B-NEW-KDMFUP                                           
014600        END-IF                                                            
014700                                                                          
014800        IF IN-IDDC     = CURRENT-IDDC     AND                             
014900           IN-KDORDKL  = CURRENT-KDORDKL  AND                             
015000           IN-TIRFSTID = CURRENT-TIRFSTID                                 
015100                                                                          
015101           CONTINUE                                                       
015102        ELSE                                                              
015103* NY TIRFSTID                                                             
015104           PERFORM S10-TIRFSTID-TOT                                       
015105           MOVE IN-IDDC          TO CURRENT-IDDC                          
015106           MOVE IN-ADCITY        TO CURRENT-ADCITY                        
015107           MOVE IN-KDORDKL       TO CURRENT-KDORDKL                       
015108           MOVE IN-TIRFSTID      TO CURRENT-TIRFSTID                      
015109        END-IF                                                            
015110                                                                          
015111        ADD IN-KVORDER           TO WS-KVORDER                            
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
016610     OPEN OUTPUT W47845                                                   
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
018210     IF REPORT-NAME = 'W47845-001'                                        
018220        SET VECKA  TO TRUE                                                
018230     ELSE                                                                 
018240        SET PERIOD TO TRUE                                                
018250     END-IF                                                               
018300     .                                                                    
018400                                                                          
018500 B-NEW-KDMFUP  SECTION.                                                   
018600     MOVE 'B-NEW-KDMFUP    ' TO CURRENT-SECTION                           
018700                                                                          
018710     IF FORSTA-KDMFUP                                                     
018720        MOVE NEJ TO KDMFUP-SW                                             
018730     ELSE                                                                 
018731        PERFORM S10-TIRFSTID-TOT                                          
018750     END-IF                                                               
018751                                                                          
018760     MOVE IN-KDMFUP      TO CURRENT-KDMFUP                                
018770     MOVE IN-IDDC        TO CURRENT-IDDC                                  
018771     MOVE IN-ADCITY      TO CURRENT-ADCITY                                
018780     MOVE IN-KDORDKL     TO CURRENT-KDORDKL                               
018781     MOVE IN-TIRFSTID    TO CURRENT-TIRFSTID                              
018782                                                                          
018783     MOVE ZERO           TO WS-KVORDER                                    
018784                            WS-KVORDER-UTSKR                              
018785                            WS-KVORDER-PACK                               
018786                            WS-KVORDRAD                                   
018787                            WS-KVORDRAD-UTSKR                             
018788                            WS-KVORDRAD-PACK                              
018789                            WS-KVKOLLI                                    
018790                            WS-KVKOLLI-LAST                               
018791                                                                          
018793     PERFORM S02-SKRIV-W47845-DAP                                         
018800     .                                                                    
018810                                                                          
022000 S01-READ-W47843  SECTION.                                                
022100     MOVE 'S01-READ-W47843  ' TO CURRENT-SECTION                          
022200                                                                          
022300     READ W47843  INTO IN-W47843                                          
022400       AT END                                                             
022500         SET END-OF-W47843  TO TRUE                                       
022600     END-READ                                                             
022700     .                                                                    
022800                                                                          
022900 S02-SKRIV-W47845-REC SECTION.                                            
032700     MOVE 'S02-SKRIV-REC'        TO CURRENT-SECTION.                      
032800                                                                          
032900     WRITE OUT-POST FROM W47845-AREA                                      
033000     .                                                                    
033100                                                                          
033200 S02-SKRIV-W47845-DAP SECTION.                                            
033300     MOVE 'S02-SKRIV-DAP'        TO CURRENT-SECTION.                      
033400                                                                          
033800     IF VECKA                                                             
033801       MOVE '¤DAPMAN-DELRFS-W'     TO W001-DAP                            
034000     ELSE                                                                 
034100       MOVE '¤DAPMAN-DELRFS-P'     TO W001-DAP                            
034600     END-IF                                                               
034700     WRITE OUT-POST FROM W001-DAP                                         
034701                                                                          
034702*    DISPLAY '****** IN-IDDC      ' IN-IDDC                               
034703     MOVE IN-IDDC                  TO WBDC-IDDC                           
034704     CALL WL10WBDC USING WBDC-AREA                                        
034705*    DISPLAY '****** WBDC-FLWEBDC ' WBDC-FLWEBDC                          
034706*    DISPLAY '****** WBDC-KDMFUP  ' WBDC-KDMFUP                           
034707     IF WBDC-FLWEBDC = JA                                                 
034708        IF VECKA                                                          
034709           MOVE '¤DAP__W47845-001' TO W001-DAP                            
034710           MOVE WBDC-KDMFUP        TO W001-KDMFUP                         
034713        ELSE                                                              
034714           MOVE '¤DAP__W47845-002' TO W001-DAP                            
034715           MOVE WBDC-KDMFUP        TO W001-KDMFUP                         
034732        END-IF                                                            
034733     END-IF                                                               
034734     WRITE OUT-POST FROM W001-DAP                                         
034735     .                                                                    
034736 S10-TIRFSTID-TOT SECTION.                                                
034737     MOVE 'S10-TIRFSTID-TOT  ' TO CURRENT-SECTION                         
034738                                                                          
034739     MOVE '1'                   TO OUT-IDAFPRCD                           
034741     MOVE CURRENT-IDDC          TO OUT-IDDC                               
034750     MOVE CURRENT-ADCITY        TO OUT-ADCITY                             
034760     MOVE IN-IDPRC              TO OUT-IDPRC                              
034770     MOVE CURRENT-KDORDKL       TO OUT-KDORDKL                            
034780     MOVE CURRENT-TIRFSTID(1:2) TO OUT-TIRFSTID(1:2)                      
034790     MOVE ':'                   TO OUT-TIRFSTID(3:1)                      
034800     MOVE CURRENT-TIRFSTID(3:2) TO OUT-TIRFSTID(4:2)                      
034900     MOVE IN-TIDATUM            TO OUT-TIDATUM                            
035000                                                                          
035100     IF VECKA                                                             
035200        MOVE IN-TIAAVV          TO OUT-TIAAVV                             
035300     ELSE                                                                 
035400        MOVE IN-TIAARP          TO OUT-TIAARP                             
035500     END-IF                                                               
035600     MOVE WS-KVORDER            TO OUT-KVORDER                            
035700     MOVE WS-KVORDER-UTSKR      TO OUT-KVORDER-UTSKR                      
035800     MOVE WS-KVORDER-PACK       TO OUT-KVORDER-PACK                       
035900     IF WS-KVORDER > ZERO                                                 
036000        COMPUTE WS-PROCENT ROUNDED =                                      
036100         100 * (WS-KVORDER-UTSKR / WS-KVORDER)                            
036110        COMPUTE OUT-KVORDER-UTSKR-PROC ROUNDED = WS-PROCENT * 1           
036120                                                                          
036200        COMPUTE WS-PROCENT ROUNDED =                                      
036300         100 * (WS-KVORDER-PACK  / WS-KVORDER)                            
036310        COMPUTE OUT-KVORDER-PACK-PROC ROUNDED = WS-PROCENT * 1            
036400     ELSE                                                                 
036500        MOVE ZERO               TO OUT-KVORDER-UTSKR-PROC                 
036600                                   OUT-KVORDER-PACK-PROC                  
036700     END-IF                                                               
036800                                                                          
036900     MOVE WS-KVORDRAD           TO OUT-KVORDRAD                           
037000     MOVE WS-KVORDRAD-UTSKR     TO OUT-KVORDRAD-UTSKR                     
037100     MOVE WS-KVORDRAD-PACK      TO OUT-KVORDRAD-PACK                      
037200     IF WS-KVORDRAD > ZERO                                                
037300        COMPUTE WS-PROCENT ROUNDED =                                      
037400         100 * (WS-KVORDRAD-UTSKR / WS-KVORDRAD)                          
037410        COMPUTE OUT-KVORDRAD-UTSKR-PROC ROUNDED = WS-PROCENT * 1          
037420                                                                          
037500        COMPUTE WS-PROCENT ROUNDED =                                      
037600         100 * (WS-KVORDRAD-PACK / WS-KVORDRAD)                           
037610        COMPUTE OUT-KVORDRAD-PACK-PROC ROUNDED = WS-PROCENT * 1           
037700     ELSE                                                                 
037800        MOVE ZERO               TO OUT-KVORDRAD-UTSKR-PROC                
037900                                   OUT-KVORDRAD-PACK-PROC                 
038000     END-IF                                                               
038100                                                                          
038200     MOVE WS-KVKOLLI            TO OUT-KVKOLLI                            
038300     MOVE WS-KVKOLLI-LAST       TO OUT-KVKOLLI-LAST                       
038400     IF WS-KVKOLLI > ZERO                                                 
038500        COMPUTE WS-PROCENT ROUNDED =                                      
038600         100 * (WS-KVKOLLI-LAST / WS-KVKOLLI)                             
038610        COMPUTE OUT-KVKOLLI-PROC ROUNDED = WS-PROCENT * 1                 
038700     ELSE                                                                 
038800        MOVE ZERO               TO OUT-KVKOLLI-PROC                       
038801     END-IF                                                               
038802                                                                          
038803     PERFORM S02-SKRIV-W47845-REC                                         
038804                                                                          
038805     MOVE ZERO           TO WS-KVORDER                                    
038806                            WS-KVORDER-UTSKR                              
038807                            WS-KVORDER-PACK                               
038808                            WS-KVORDRAD                                   
038809                            WS-KVORDRAD-UTSKR                             
038810                            WS-KVORDRAD-PACK                              
038820                            WS-KVKOLLI                                    
038821                            WS-KVKOLLI-LAST                               
038822     .                                                                    
038823                                                                          
038824                                                                          
038825 Z-FINIT SECTION.                                                         
038826     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
038827                                                                          
038828                                                                          
038829     CLOSE W47843                                                         
038830           W47845                                                         
038831     .                                                                    
038832                                                                          
