000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WF211100.                                                
000300 AUTHOR.         ANDERS HENRIKSSON                                        
000400 DATE-WRITTEN.   2006-01-16.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        THE PGM                                                          
001000*        - READS FILE WITH DOCUMENT DATA RECORDS                          
001210*        - WRITES DOCUMENT DATA RECORDS FOR VCCS,VCXX TO A FILE.          
001220*        - USES WZ14DAP4 IN THE JCL TO SEND THE FILE TO                   
001230*          DISTRIBUTION & PRINT                                           
001300*                                                                         
001400 ENVIRONMENT DIVISION.                                                    
001500                                                                          
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900*          --- DOCUMENT DATA RECORDS                                      
002000     SELECT WF2011                     ASSIGN TO WF2111D1.                
002010*          --- DOCUMENT DATA RECORDS - OUTPUT                             
002020     SELECT WF2111                     ASSIGN TO WF2111D2.                
002100     EJECT                                                                
002200 DATA DIVISION.                                                           
002300                                                                          
002400 FILE SECTION.                                                            
002500 FD  WF2011                                                               
002600     RECORDING       V                                                    
002700     BLOCK CONTAINS  0.                                                   
002800                                                                          
002900 01  IN-DATA.                                                             
003000     03  IN-IDPTYP               PIC X(3).                                
003100     03  FILLER                  PIC X(16).                               
003200     03  IN-IDLEGSEL             PIC X(4).                                
003300     03  FILLER                  PIC X(5672).                             
003400     EJECT                                                                
003410 FD  WF2111                                                               
003420     RECORDING       V                                                    
003430     BLOCK CONTAINS  0.                                                   
003440                                                                          
003450 01  WF2111-001                  PIC X(3000).                             
003460     EJECT                                                                
003500                                                                          
003600 WORKING-STORAGE SECTION.                                                 
003700                                                                          
003800 77  IDPGM                       PIC X(8)    VALUE 'WF211100'.            
003900 77  YES                         PIC X(1)    VALUE 'J'.                   
004000 77  NOO                         PIC X(1)    VALUE 'N'.                   
004100 77  WS-ZERO                     PIC X(5)    VALUE '00000'.               
004200 77  WS-NINE                     PIC X(5)    VALUE '99999'.               
004800 77  WS-DOC-HEAD                 PIC X(3)    VALUE '1  '.                 
004900 77  WS-DOC-LINE                 PIC X(3)    VALUE '2  '.                 
005000 77  WS-DOC-LINE-SOFT            PIC X(8)    VALUE '2   SOFT'.            
005100 77  WS-DOC-GROUP                PIC X(3)    VALUE '2.1'.                 
005200 77  WS-DOC-GROUP-SOFT           PIC X(8)    VALUE '2.1 SOFT'.            
005300 77  WS-DOC-FOOTER               PIC X(3)    VALUE '3  '.                 
005500 77  WS-IDEXCUST-2-OLD           PIC X(15)   VALUE SPACE.                 
005600 77  WS-IDFINDOC-OLD             PIC 9(9)    VALUE ZERO.                  
005700 77  WS-IDREF-OLD                PIC X(15)   VALUE SPACE.                 
005800 77  WS-DAREFDAT-OLD             PIC 9(8)    VALUE ZERO.                  
006200 77  WS-HEAD-PRKURS            PIC S9(6)V9(5) COMP-3 VALUE ZERO.          
006300 77  WS-SUNTO-SND-LOC          PIC S9(11)V9(2) COMP-3 VALUE ZERO.         
006400 77  WS-IDLANDX3-BET             PIC X(3)    VALUE SPACE.                 
006500 77  WS-KDFRAKT                  PIC 9(2)    VALUE ZERO.                  
006600                                                                          
006700 77  WF2011-FIRST-RECORD         PIC X       VALUE ' '.                   
006800     88  FIRST-RECORD                        VALUE 'J'.                   
006900     88  OTHER-RECORDS                       VALUE 'N'.                   
007000                                                                          
007100 77  WF2011-EOF-SW               PIC X       VALUE 'N'.                   
007200     88  END-OF-WF2011                       VALUE 'J'.                   
007300                                                                          
007400 77  NEW-HEAD-SW                 PIC X       VALUE 'J'.                   
007500     88  NEW-HEAD                            VALUE 'J'.                   
007600     88  OTHER-ROW                           VALUE 'N'.                   
007700                                                                          
007800 77  WS-FOOT-LOC-SW              PIC X       VALUE 'N'.                   
007900     88  NEW-FOOT-LOC                        VALUE 'J'.                   
008000     88  NO-FOOT-LOC                         VALUE 'N'.                   
008100                                                                          
008200 77  WS-FOOT-SND-SW              PIC X       VALUE 'N'.                   
008300     88  NEW-FOOT-SND                        VALUE 'J'.                   
008400     88  NO-FOOT-SND                         VALUE 'N'.                   
008500                                                                          
008600 01  ERRTEXT.                                                             
008700     03  FILLER                  PIC X(8)    VALUE 'ERRTEXT'.             
008800     03  ERRTEXT-STR             PIC X(72)   VALUE SPACE.                 
008900 01  KDRC-DISPLAY                PIC Z(5).                                
009000     EJECT                                                                
009100                                                                          
009200 01  GENERAL-SUBPROGRAMS.                                                 
009300     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009400     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
009500     EJECT                                                                
009600                                                                          
009700*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
009800                                                                          
009900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010200     EJECT                                                                
010300                                                                          
010800                                                                          
010900*    --- IN-AREOR                                                         
011000 01  INPUT-AREA                 PIC X(24)   VALUE                         
011100                                'INPUT-AREA     '.                        
011200 01  IN-AREA-HEAD.                                                        
011300*    03  -COPY WF201101                                                   
011400                                                                          
011500 01  IN-AREA-LINE.                                                        
011600*    03  -COPY WF201102                                                   
011700                                                                          
011800 01  IN-AREA-FOOT.                                                        
011900*    03  -COPY WF201103                                                   
012000                                                                          
012100 01  IN-AREA-APPX.                                                        
012200*    03  -COPY WF201104                                                   
012300     EJECT                                                                
012400                                                                          
012410 01  W001-DAP.                                                            
012420     03  FILLER              PIC X(165)  VALUE SPACE.                     
012430     EJECT                                                                
012500*    --- UT-AREOR                                                         
012600 01  OUTPUT-AREA                 PIC X(24)   VALUE                        
012700                                'OUTPUT-AREA     '.                       
012710                                                                          
012800 01  WS-HDR-AREA-1.                                                       
012900     03 WS-IDOUTTYPE             PIC X(15).                               
013000     EJECT                                                                
013100 01  WS-HDR-AREA-2.                                                       
013110     03 WS-IDOUTREC              PIC X(30).                               
013120     EJECT                                                                
013130 01  WS-HDR-AREA-3.                                                       
013140     03 WS-IDLIST                PIC X(10).                               
013150     EJECT                                                                
013200                                                                          
013300 01  DOC-HEAD-AREA.                                                       
013400*    03  -COPY WF211101                                                   
013500     EJECT                                                                
013600                                                                          
013700 01  DOC-LINE-AREA.                                                       
013800*    03  -COPY WF211102                                                   
013900     EJECT                                                                
014000                                                                          
014100 01  DOC-FOOT-AREA.                                                       
014200*    03  -COPY WF211103                                                   
014300     EJECT                                                                
014400                                                                          
014500 LINKAGE SECTION.                                                         
014700     EJECT                                                                
014800                                                                          
014900 PROCEDURE DIVISION.                                                      
015000 MAIN SECTION.                                                            
015100                                                                          
015400     PERFORM A-INIT                                                       
015500     PERFORM B-EXECUTE                                                    
015600     PERFORM Z-FINIT                                                      
015700                                                                          
015800     MOVE ZERO TO RETURN-CODE                                             
015900     GOBACK                                                               
016000     .                                                                    
016100                                                                          
016200 A-INIT SECTION.                                                          
016300     OPEN INPUT  WF2011                                                   
016310     OPEN OUTPUT WF2111                                                   
016400     MOVE NOO TO WS-FOOT-LOC-SW                                           
016500     MOVE NOO TO WS-FOOT-SND-SW                                           
016600     .                                                                    
016700                                                                          
016800 B-EXECUTE SECTION.                                                       
016900     PERFORM S01-READ-WF2011                                              
017000                                                                          
017100     IF END-OF-WF2011                                                     
017200       CONTINUE                                                           
017300     ELSE                                                                 
017400       MOVE YES TO WF2011-FIRST-RECORD                                    
017500                                                                          
017600       PERFORM UNTIL END-OF-WF2011                                        
018200           IF IN-IDPTYP = WS-DOC-HEAD                                     
018300             MOVE YES TO NEW-HEAD-SW                                      
018400             IF FIRST-RECORD                                              
018500               MOVE NOO TO WF2011-FIRST-RECORD                            
019100             END-IF                                                       
019200             PERFORM BA-HANDLE-HEADER                                     
019300           ELSE                                                           
019400             IF IN-IDPTYP = WS-DOC-LINE                                   
019500               PERFORM BB-HANDLE-LINES                                    
019600             ELSE                                                         
019700               IF IN-IDPTYP = WS-DOC-FOOTER                               
019800                 PERFORM BC-HANDLE-FOOTER                                 
019900               END-IF                                                     
020000             END-IF                                                       
020100           END-IF                                                         
020300         PERFORM S01-READ-WF2011                                          
020400       END-PERFORM                                                        
020800     END-IF                                                               
020900     .                                                                    
021000                                                                          
021100 BA-HANDLE-HEADER SECTION.                                                
021200     MOVE SPACE                       TO WS-IDEXCUST-2-OLD                
021600     MOVE IN-DATA                     TO IN-AREA-HEAD                     
021700     MOVE 'FINANCIAL INFO'            TO WS-IDOUTTYPE                     
021800     MOVE SPACE                       TO WS-IDOUTREC                      
021900*                                                                         
022000     MOVE HEAD-IDLANDX3-SEND(1:2)     TO WS-IDOUTREC(1:2)                 
022100     MOVE HEAD-IDLANDX3-BET(1:2)      TO WS-IDOUTREC(3:2)                 
022200     MOVE HEAD-IDPARTNR               TO WS-IDOUTREC(5:9)                 
022300* TO ADD KDFRAKT/FC.                                                      
022400     MOVE HEAD-KDFRAKT                TO WS-KDFRAKT                       
022500     MOVE WS-KDFRAKT                  TO WS-IDOUTREC(14:2)                
022600                                                                          
022700     MOVE FUNCTION CURRENT-DATE(3:10) TO WS-IDLIST                        
022800     MOVE HEAD-IDLANDX3-BET TO WS-IDLANDX3-BET                            
022900                                                                          
023500     PERFORM S90-WRITE-HEADER                                             
023600                                                                          
023700     MOVE HEAD-IDPTYP                TO FI-HEAD-IDAFPRCD                  
023800     MOVE HEAD-IDLEGSEL              TO FI-HEAD-IDLEGSEL                  
023900     MOVE HEAD-BEFORMS               TO FI-HEAD-BEFORMS                   
024000     MOVE HEAD-IDLANDX3-SEND(1:2)    TO FI-HEAD-IDLANDX3-SEND             
024100     MOVE HEAD-IDLANDX3-BET(1:2)     TO FI-HEAD-IDLANDX3-BET              
024200     IF HEAD-KDFINDOC = 'CR'                                              
024210     OR HEAD-KDFINDOC = 'INT2'                                            
024300       MOVE SPACE                    TO FI-HEAD-IDLEVNR                   
024400       MOVE SPACE                    TO HEAD-IDLEVNR                      
024500     ELSE                                                                 
024600       MOVE HEAD-IDLEVNR             TO FI-HEAD-IDLEVNR                   
024700     END-IF                                                               
024800     MOVE HEAD-IDFINDOC              TO FI-HEAD-IDFINDOC                  
024900     MOVE HEAD-DAFINDOC              TO FI-HEAD-DAFINDOC                  
025000     MOVE HEAD-IDVAT-LEG             TO FI-HEAD-IDVAT-LEG                 
025100     MOVE HEAD-IDVAT-AGENT           TO FI-HEAD-IDVAT-AGENT               
025200     MOVE HEAD-IDVAT-RESP            TO FI-HEAD-IDVAT-RESP                
025300     MOVE HEAD-KDVALISO              TO FI-HEAD-KDVALISO                  
025400     IF HEAD-KDVALISO-LOC NOT = HEAD-KDVALISO                             
025500       MOVE HEAD-PRKURS-LOC          TO FI-HEAD-PRKURS                    
025600                                        WS-HEAD-PRKURS                    
025700     ELSE                                                                 
025800       MOVE 1                        TO FI-HEAD-PRKURS                    
025900                                        WS-HEAD-PRKURS                    
026000     END-IF                                                               
026100     MOVE HEAD-IDPARTNR              TO FI-HEAD-IDPARTNR                  
026200     MOVE HEAD-BEBETVIL              TO FI-HEAD-BEBETVIL                  
026300     MOVE HEAD-IDVAT-BET             TO FI-HEAD-IDVAT-BET                 
026400     MOVE HEAD-SUNTO-PART            TO FI-HEAD-SUNTO-PART                
026500     MOVE HEAD-SUBTO-PART            TO FI-HEAD-SUBTO-PART                
026600     MOVE HEAD-BEANST                TO FI-HEAD-BEANST                    
026700     MOVE HEAD-IDUSER                TO FI-HEAD-IDUSER                    
026800     MOVE HEAD-BETEXT-5              TO FI-HEAD-BETEXT-5                  
026900     MOVE HEAD-BETEXT-6              TO FI-HEAD-BETEXT-6                  
027000     MOVE HEAD-BETEXT-7              TO FI-HEAD-BETEXT-7                  
027100     MOVE HEAD-BETEXT-8              TO FI-HEAD-BETEXT-8                  
027200                                                                          
027300     .                                                                    
027400                                                                          
027500 BB-HANDLE-LINES SECTION.                                                 
027600     MOVE IN-DATA                     TO IN-AREA-LINE                     
027700                                                                          
027800*    -- IF NEW GROUP,                                                     
027900*          MARK IDAFPRCD WITH '2.1'                                       
028000     IF LINE-IDEXCUST-2   = WS-IDEXCUST-2-OLD                             
028100     AND LINE-IDFINDOC  = WS-IDFINDOC-OLD                                 
028200     AND LINE-IDREF     = WS-IDREF-OLD                                    
028300     AND LINE-DAREFDAT  = WS-DAREFDAT-OLD                                 
028400       IF HEAD-FLSOFT     = 'N'                                           
028500         MOVE LINE-IDPTYP       TO FI-LINE-IDAFPRCD                       
028600       ELSE                                                               
028700         MOVE WS-DOC-LINE-SOFT  TO FI-LINE-IDAFPRCD                       
028800       END-IF                                                             
028900     ELSE                                                                 
029000       IF HEAD-FLSOFT     = 'N'                                           
029100         MOVE WS-DOC-GROUP      TO FI-LINE-IDAFPRCD                       
029200       ELSE                                                               
029300         MOVE WS-DOC-GROUP-SOFT TO FI-LINE-IDAFPRCD                       
029400       END-IF                                                             
029500       MOVE LINE-IDEXCUST-2     TO WS-IDEXCUST-2-OLD                      
029600       MOVE LINE-IDREF          TO WS-IDREF-OLD                           
029700       MOVE LINE-DAREFDAT       TO WS-DAREFDAT-OLD                        
029800       MOVE LINE-IDFINDOC       TO WS-IDFINDOC-OLD                        
029900     END-IF                                                               
030000     MOVE LINE-IDEXCUST-1       TO FI-LINE-IDEXCUST-1                     
030100     MOVE LINE-IDEXCUST-2       TO FI-LINE-IDEXCUST-2                     
030200     MOVE LINE-IDBUNDLE         TO FI-LINE-IDBUNDLE                       
030300     MOVE LINE-IDREF            TO FI-LINE-IDREF                          
030400     MOVE LINE-BEVOLREF         TO FI-LINE-BEVOLREF                       
030500     MOVE LINE-IDARTNR-FINANCE  TO FI-LINE-IDARTNR-FINANCE                
030600     MOVE LINE-BEART            TO FI-LINE-BEART                          
030700     MOVE LINE-KVBEART          TO FI-LINE-KVBEART                        
030800     MOVE LINE-KVLEVART         TO FI-LINE-KVLEVART                       
030900     MOVE LINE-PRARTBTO         TO FI-LINE-PRARTBTO                       
031000     MOVE LINE-PRARTNTO         TO FI-LINE-PRARTNTO                       
031100     MOVE LINE-REARTRAB         TO FI-LINE-REARTRAB                       
031200     IF LINE-FLSPECPR = 'Y' OR 'J'                                        
031300       MOVE '*'                 TO FI-LINE-FLSPECPR                       
031400     ELSE                                                                 
031500       MOVE SPACE               TO FI-LINE-FLSPECPR                       
031600     END-IF                                                               
031700     MOVE LINE-SUNTO            TO FI-LINE-SUNTO                          
031800     MOVE LINE-REVAT            TO FI-LINE-REVAT                          
031900     MOVE LINE-SUVAT-BILLIT     TO FI-LINE-SUVAT-BILLIT                   
032000     MOVE LINE-SUBTO            TO FI-LINE-SUBTO                          
032100     MOVE LINE-VKARTNTO         TO FI-LINE-VKARTNTO                       
032200     MOVE LINE-VKORDBTO-KOLLI   TO FI-LINE-VKORDBTO-KOLLI                 
032300     MOVE LINE-KDARTURS         TO FI-LINE-KDARTURS                       
032400     MOVE LINE-IDSTATNR         TO FI-LINE-IDSTATNR                       
032500     MOVE LINE-KDFRAKT          TO FI-LINE-KDFRAKT                        
032600     MOVE LINE-IDACCNT-1        TO FI-LINE-IDACCNT-1                      
032700     MOVE LINE-IDACCNT-2        TO FI-LINE-IDACCNT-2                      
032800     MOVE LINE-IDACCNT-3        TO FI-LINE-IDACCNT-3                      
032900     MOVE LINE-KDANMORS         TO FI-LINE-KDANMORS                       
033000     MOVE LINE-IDFAKREF         TO FI-LINE-IDFAKREF                       
033100     MOVE LINE-DAFAKREF         TO FI-LINE-DAFAKREF                       
033200     MOVE LINE-DAREFDAT         TO FI-LINE-DAREFDAT                       
033300     MOVE LINE-IDDC             TO FI-LINE-IDDC                           
033400     MOVE LINE-BELEVVIL         TO FI-LINE-BELEVVIL                       
033500     MOVE HEAD-BEANST           TO FI-LINE-BEANST                         
033600     MOVE HEAD-IDUSER           TO FI-LINE-IDUSER                         
033700     IF LINE-FLPCOO   = 'Y' OR 'J'                                        
033800       MOVE '*'                 TO FI-LINE-FLPCOO                         
033900     ELSE                                                                 
034000       MOVE SPACE               TO FI-LINE-FLPCOO                         
034100     END-IF                                                               
034200     MOVE LINE-IDOPTION-1       TO FI-LINE-IDOPTION-1                     
034300     MOVE LINE-IDOPTION-2       TO FI-LINE-IDOPTION-2                     
034400     MOVE LINE-IDOPTION-3       TO FI-LINE-IDOPTION-3                     
034500     MOVE LINE-IDOPTION-4       TO FI-LINE-IDOPTION-4                     
034600     MOVE LINE-IDOPTION-5       TO FI-LINE-IDOPTION-5                     
034700                                                                          
034800*    --- GET DOC-HEAD-DATA FROM BILLIT-LINE-DATA                          
034900     IF NEW-HEAD                                                          
035000       MOVE LINE-IDEXCUST-1     TO FI-HEAD-IDEXCUST-1                     
035100       MOVE LINE-IDEXCUST-2     TO FI-HEAD-IDEXCUST-2                     
035200       MOVE LINE-BELEVVIL       TO FI-HEAD-BELEVVIL                       
035300       MOVE LINE-IDDC           TO FI-HEAD-IDDC                           
035400* FÖR DESSA KODER SKA MOMSTEXTER ALLTID                                   
035500* LÄGGAS UT PÅ DOKUMENTET                                                 
035600       IF LINE-KDVAT = 'BD'                                               
035700       OR LINE-KDVAT = 'NZ'                                               
035800         MOVE LINE-BEVAT        TO FI-HEAD-BEVAT                          
035900       ELSE                                                               
036000         MOVE SPACE             TO FI-HEAD-BEVAT                          
036100       END-IF                                                             
036200                                                                          
036300       PERFORM S90-WRITE-DOC-HEAD                                         
036400       MOVE NOO                 TO NEW-HEAD-SW                            
036500     END-IF                                                               
036600                                                                          
036700     PERFORM S90-WRITE-DOC-LINE                                           
036800     .                                                                    
036900                                                                          
037000 BC-HANDLE-FOOTER SECTION.                                                
037100     MOVE IN-DATA               TO IN-AREA-FOOT                           
037200                                                                          
037300     MOVE FOOT-IDPTYP           TO FI-FOOT-IDAFPRCD                       
037400     MOVE FOOT-SUNTO-TOT        TO WS-SUNTO-SND-LOC                       
037500     MOVE FOOT-SUNTO-TOT        TO FI-FOOT-SUNTO-TOT                      
037600     MOVE FOOT-SUVAT-BILLIT-TOT TO FI-FOOT-SUVAT-BILLIT-TOT               
037700     MOVE FOOT-SUBTO-TOT        TO FI-FOOT-SUBTO-TOT                      
037800     MOVE FOOT-KDVALISO         TO FI-FOOT-KDVALISO                       
037900     MOVE FOOT-KDVALISO         TO FI-FOOT-KDVALISO-TXT                   
038000*                                                                         
038100* THIS A WAY TO PUT LOCAL CURRENCY ON THE INVOICE                         
038200     IF FOOT-KDVALISO-LOC NOT = FOOT-KDVALISO                             
038300       MOVE FOOT-SUNTO-TOT-LOC  TO FI-FOOT-SUNTO-TOT-LOC                  
038400       MOVE FOOT-SUNTO-TOT-LOC  TO WS-SUNTO-SND-LOC                       
038500       MOVE FOOT-SUBTO-TOT-LOC  TO FI-FOOT-SUBTO-TOT-LOC                  
038600       MOVE FOOT-SUVAT-BILLIT-TOT-LOC                                     
038700                                TO FI-FOOT-SUVAT-BILLIT-TOT-LOC           
038800       MOVE FOOT-KDVALISO-LOC   TO FI-FOOT-KDVALISO-LOC                   
038900       MOVE FOOT-KDVALISO-LOC   TO FI-FOOT-KDVALISO-TXT                   
039000       MOVE 'J'                 TO WS-FOOT-LOC-SW                         
039100     ELSE                                                                 
039200       MOVE ZERO                TO FI-FOOT-SUNTO-TOT-LOC                  
039300                                   FI-FOOT-SUBTO-TOT-LOC                  
039400                                   FI-FOOT-SUVAT-BILLIT-TOT-LOC           
039500       MOVE SPACE               TO FI-FOOT-KDVALISO-LOC                   
039600     END-IF                                                               
039700*                                                                         
039800* THIS A WAY TO PUT SENDING CURRENCY ON THE INVOICE                       
039900     IF FOOT-KDVALISO-SND = FOOT-KDVALISO                                 
040000       MOVE ZERO                TO FI-FOOT-SUNTO-TOT-SND                  
040100                                   FI-FOOT-SUBTO-TOT-SND                  
040200                                   FI-FOOT-SUVAT-BILLIT-TOT-SND           
040300       MOVE SPACE               TO FI-FOOT-KDVALISO-SND                   
040400     ELSE                                                                 
040500       IF FOOT-KDVALISO-SND = FOOT-KDVALISO-LOC                           
040600         MOVE ZERO                TO FI-FOOT-SUNTO-TOT-SND                
040700                                     FI-FOOT-SUBTO-TOT-SND                
040800                                     FI-FOOT-SUVAT-BILLIT-TOT-SND         
040900         MOVE SPACE               TO FI-FOOT-KDVALISO-SND                 
041000       ELSE                                                               
041100         MOVE FOOT-SUNTO-TOT-SND  TO FI-FOOT-SUNTO-TOT-SND                
041200         MOVE FOOT-SUBTO-TOT-SND  TO FI-FOOT-SUBTO-TOT-SND                
041300         MOVE FOOT-SUVAT-BILLIT-TOT-SND                                   
041400                                  TO FI-FOOT-SUVAT-BILLIT-TOT-SND         
041500         MOVE FOOT-KDVALISO-SND   TO FI-FOOT-KDVALISO-SND                 
041600         MOVE 'J'                 TO WS-FOOT-SND-SW                       
041700         IF FOOT-SUNTO-TOT-SND = ZERO                                     
041800           MOVE ZERO TO FI-FOOT-PRKURS-SND                                
041900         ELSE                                                             
042000           COMPUTE FI-FOOT-PRKURS-SND ROUNDED = WS-SUNTO-SND-LOC /        
042100                                                FOOT-SUNTO-TOT-SND        
042200         END-IF                                                           
042300       END-IF                                                             
042400     END-IF                                                               
042500                                                                          
042600     IF WS-FOOT-LOC-SW = 'J'                                              
042700       PERFORM BH-HANDLE-FOOT-LOC                                         
042800     END-IF                                                               
042900                                                                          
043000     IF WS-FOOT-SND-SW = 'J'                                              
043100       PERFORM BI-HANDLE-FOOT-SND                                         
043200     END-IF                                                               
043300                                                                          
043400     PERFORM S90-WRITE-DOC-FOOT                                           
043500                                                                          
043600     MOVE NOO TO WS-FOOT-LOC-SW                                           
043700     MOVE NOO TO WS-FOOT-SND-SW                                           
043800     .                                                                    
043900                                                                          
044000 BH-HANDLE-FOOT-LOC SECTION.                                              
044100*    -- IF NEW LOCAL CURRENCY DIFFERENT FROM INVOICE CURRENCY             
044200     MOVE FOOT-SUNTO-TOT-LOC  TO FI-FOOT-SUNTO-TOT-LOC                    
044300     MOVE FOOT-SUBTO-TOT-LOC  TO FI-FOOT-SUBTO-TOT-LOC                    
044400     MOVE FOOT-SUVAT-BILLIT-TOT-LOC                                       
044500                              TO FI-FOOT-SUVAT-BILLIT-TOT-LOC             
044600     MOVE FOOT-KDVALISO-LOC   TO FI-FOOT-KDVALISO-LOC                     
044700     .                                                                    
044800*                                                                         
044900 BI-HANDLE-FOOT-SND SECTION.                                              
045000*    -- IF NEW SENDING CURRENCY DIFFERENT FROM INVOICE CURRENCY           
045100     MOVE FOOT-SUNTO-TOT-SND  TO FI-FOOT-SUNTO-TOT-SND                    
045200     MOVE FOOT-SUBTO-TOT-SND  TO FI-FOOT-SUBTO-TOT-SND                    
045300     MOVE FOOT-SUVAT-BILLIT-TOT-SND                                       
045400                              TO FI-FOOT-SUVAT-BILLIT-TOT-SND             
045500     MOVE FOOT-KDVALISO-SND   TO FI-FOOT-KDVALISO-SND                     
045600     .                                                                    
045700*                                                                         
045800 Z-FINIT SECTION.                                                         
045900     CLOSE WF2011                                                         
045910           WF2111                                                         
046000     .                                                                    
046100                                                                          
046200 S01-READ-WF2011  SECTION.                                                
046300     READ WF2011                                                          
046400       AT END                                                             
046500         SET END-OF-WF2011 TO TRUE                                        
046600     END-READ                                                             
046700     .                                                                    
046710                                                                          
046720 S90-WRITE-HEADER SECTION.                                                
046730                                                                          
046740     PERFORM S91-WRITE-DAP1-S                                             
046750     PERFORM S91-WRITE-DAP2-S                                             
046760     PERFORM S91-WRITE-DAP3-S                                             
046770     .                                                                    
046780 S90-WRITE-DOC-HEAD SECTION.                                              
046790                                                                          
046800     WRITE WF2111-001   FROM DOC-HEAD-AREA                                
046900     .                                                                    
047000 S90-WRITE-DOC-LINE SECTION.                                              
047100                                                                          
047200     WRITE WF2111-001   FROM DOC-LINE-AREA                                
047300     .                                                                    
047400 S90-WRITE-DOC-FOOT SECTION.                                              
047500                                                                          
047600     WRITE WF2111-001   FROM DOC-FOOT-AREA                                
047700     .                                                                    
050600 S91-WRITE-DAP1-S SECTION.                                                
050700                                                                          
050800     STRING ' ¤DAP' WS-HDR-AREA-1                                         
050900            DELIMITED BY SIZE INTO W001-DAP                               
051000     WRITE WF2111-001     FROM W001-DAP                                   
051100                                                                          
051200     MOVE SPACE TO W001-DAP                                               
051300     .                                                                    
051400                                                                          
051500 S91-WRITE-DAP2-S SECTION.                                                
051600                                                                          
051700     STRING ' ¤DAP' WS-HDR-AREA-2                                         
051800            DELIMITED BY SIZE INTO W001-DAP                               
051900     WRITE WF2111-001         FROM W001-DAP                               
052000                                                                          
052100     MOVE SPACE TO W001-DAP                                               
052200     .                                                                    
052300 S91-WRITE-DAP3-S SECTION.                                                
052400                                                                          
052500     STRING ' ¤DAP' WS-HDR-AREA-3                                         
052600            DELIMITED BY SIZE INTO W001-DAP                               
052700     WRITE WF2111-001         FROM W001-DAP                               
052800                                                                          
052900     MOVE SPACE TO W001-DAP                                               
053000     .                                                                    
