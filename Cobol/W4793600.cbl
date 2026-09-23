000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4793600.                                                
000300 AUTHOR.         BERT ANDERSSON.                                          
000400 DATE-WRITTEN.   08/07/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*        LÄSER IN FIL W47935 OCH SKAPAR DAP POSTER                        
001000*                                                                         
001100*        PROGRAMMET SKRIVER "LISTAN NIL PICKS AND DEVIATIONS              
001200*        PERIODICALLY FOLLOW UP" TILL D&P.                                
001300*                                                                         
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*          --- FIL MED NIL PICKS AND DEVIATIONS                           
002400     SELECT W47935                     ASSIGN TO W47936D1.                
002500     SKIP2                                                                
002600*          --- UTFIL TILL W47935                                          
002700     SELECT W47936                     ASSIGN TO W47936D2.                
002800     SKIP2                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300 FD  W47935                                                               
003400     RECORDING       F                                                    
003500     BLOCK CONTAINS  0.                                                   
003600                                                                          
003700*01  -COPY W47932       -L.                                               
003800     SKIP3                                                                
003900                                                                          
004000 FD  W47936                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  POST -COPY W47932   -PRE OUT-   -L.                                  
004500     SKIP3                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W4793600'.            
004900                                                                          
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200 77  WS-ADRESS                   PIC X(50)                                
005300                   VALUE 'CARPARTS.DAP.DISTRDOC'.                         
005400 77  WZ04-SEND-IDCOM             PIC S9(9)   COMP VALUE +0.               
005500 77  FILLER                      PIC X(8)    VALUE 'PGM-POS:'.            
005600 77  PGM-POS                     PIC X(64)   VALUE SPACE.                 
005700 77  FILLER                      PIC X(8)   VALUE 'ERRORTEX'.             
005800 77  ERROR-TEXT                  PIC X(80)  VALUE SPACE.                  
005900 77  KDRC-DISPLAY                PIC Z(5).                                
006000 77  WS-IDDC                     PIC X(2)   VALUE SPACE.                  
006100 77  WS-KDMFUP                   PIC X(2)   VALUE SPACE.                  
006200 77  WS-DCS-IDLANDX2             PIC X(2)   VALUE SPACE.                  
006300 77  WS-DCS-ADGMT-PADR           PIC X(20)  VALUE SPACE.                  
006400*****                                                                     
006500 77  WS-TOT-ANTAL-AVVIKELSE-RADER   PIC 9(6)   VALUE ZERO.                
006600 77  WS-IN-ANTAL-AVVIKELSE-RADER    PIC 9(6)   VALUE ZERO.                
006700*****                                                                     
006800 77  WS-TOTALT-ANTAL-RADER       PIC 9(6)    VALUE ZERO.                  
006900 77  WS-IN-TOTALT-ANTAL-RADER    PIC 9(6)    VALUE ZERO.                  
007000*****                                                                     
007100 77  WS-TOT-PROCENT              PIC 9(3)V9(3) VALUE ZERO.                
007300*****                                                                     
007400 77  WS-TIAARP                   PIC 9(4)   VALUE ZERO.                   
007500*****                                                                     
007600 01  WS-YYMMDDHHMM.                                                       
007700     03 WS-YYMMDD                PIC  9(6).                               
007800     03 WS-TIME                  PIC  9(4).                               
007900                                                                          
008000 01  WS-AAVV                     PIC  9(4).                               
008100                                                                          
008200 01  WS-HHMMSSTH                 PIC  9(8).                               
008300 01  FILLER REDEFINES WS-HHMMSSTH.                                        
008400       03  WS-HHMM               PIC 9(4).                                
008500       03  WS-SSTH               PIC 9(4).                                
008600 01  WS-IDDC-WEB                 PIC X(2) VALUE SPACE.                    
008700*---------------------------------------                                  
008800                                                                          
008900 01  W-IDDC-B6-X.                                                         
009000     03 W-IDDC-B6        PIC X(2).                                        
009100                                                                          
009200     EJECT                                                                
009300*---------------------------------------                                  
009400                                                                          
009500     SKIP2                                                                
009600 01  FELTEXT.                                                             
009700     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009800     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009900                                                                          
010000 77  W47935-EOF-SW               PIC X       VALUE 'N'.                   
010100     88  END-OF-W47935                       VALUE 'J'.                   
010200     EJECT                                                                
010300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
010400 01  FILLER REDEFINES DAGENS-DATUM.                                       
010500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
010600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
010700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
010800     EJECT                                                                
010900 01  DYNAMISKA-SUBPROGRAM.                                                
011000*                                                                         
011100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011300     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
011400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011500     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
011600     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011800     03  ABEND-CODE          PIC S9(4)   COMP  VALUE +0  SYNC.            
011900     EJECT                                                                
012000*    --- PARAMETERS TO ABEND                                              
012100                                                                          
012200 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
012300 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
012400 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
012500                                                                          
012600*    --- PARAMETRAR TILL DATKORT                                          
012700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W47936'.              
012800     SKIP2                                                                
012900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013000     SKIP2                                                                
013100*01  -COPY WDATKORT                                                       
013200     EJECT                                                                
013300*    --- PARAMETRAR TILL WDATKONV                                         
013400 01  FILLER                  PIC X(16) VALUE 'WDATAREA'.                  
013500*01  -COPY WDATAREA                                                       
013600     EJECT                                                                
013700*    --- PARAMETRAR TILL POSTSUM                                          
013800*                                                                         
013900*01  -COPY W0005   -PRE  POSTSUM-                                         
014000     EJECT                                                                
014100 01  FILLER                      PIC X(16)   VALUE 'WZ01SEND'.            
014200*01  -COPY WZ01SEND                                                       
014300     EJECT                                                                
014400 01  IN-AREA-START               PIC X(16)   VALUE                        
014500                                             'IN-AREA-START'.             
014600     SKIP2                                                                
014700                                                                          
014800*01  AREA -COPY W47932      -PRE IN-                                      
014900     EJECT                                                                
015000 01  FILLER                      PIC X(16) VALUE 'OUT-AREA-START'.        
015100     SKIP2                                                                
015200*01  AREA -COPY W47932      -PRE OUT-                                     
015300     EJECT                                                                
015400 01  FILLER                     PIC X(16)  VALUE 'HDR-AREA-START'.        
015500     SKIP2                                                                
015600 01  HDR-AREA.                                                            
015700*   03  -COPY WZ01REQU -PRE HDR-                                          
015800*   03  -COPY WZ04HDR                                                     
015900*                                                                         
016000 01  LINE-AREA                   PIC X(25)  VALUE 'WEB-LINE-AREA'.        
016100 01  DOC-LINE-AREA.                                                       
016200*    03 -COPY W479321 -PRE WEB-LINE-                                      
016300     EJECT                                                                
016400     SKIP3                                                                
016500 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
016600 01   DLI-IO-AREA-B601.                                                   
016700*     03  -COPY WDB601                                                    
016800                                                                          
016900                                                                          
017000 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017100*    --- STATUS-KOD FRÅN IMS                                              
017200 01  STATUS-WS                   PIC XX.                                  
017300     88  SEGMENT-FINNS                       VALUE '  '.                  
017400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
017500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
017600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
017700     88  IMS-EJ-OK                           VALUE 'XD'.                  
017800     SKIP2                                                                
017900 01  GODK-STATUSKODER.                                                    
018000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
018100     SKIP3                                                                
018200 01  SSA1                        PIC X(64).                               
018300 01  SSA2                        PIC X(64).                               
018400     EJECT                                                                
018500*    --- IMS FUNKTIONSKODER                                               
018600*01  -COPY W0003                                                          
018700     EJECT                                                                
018800                                                                          
018900 LINKAGE SECTION.                                                         
019000*01  -COPY W0009   -PRE MSG-                                              
019100     EJECT                                                                
019200 01  DAP-PCB              PIC X.                                          
019300     EJECT                                                                
019400*01  -COPY W0008 -PRE WDB6-                                               
019500     05  FILLER           PIC X.                                          
019600                                                                          
019700 PROCEDURE DIVISION  USING MSG-PCB DAP-PCB WDB6-PCB.                      
019800 MAIN SECTION.                                                            
019900     ENTRY 'DLITCBL' USING MSG-PCB DAP-PCB WDB6-PCB.                      
020000                                                                          
020100     SKIP2                                                                
020200     PERFORM A-INIT                                                       
020300     PERFORM S01-LAES-W47935                                              
020400                                                                          
020500     IF NOT END-OF-W47935                                                 
020600       MOVE IN-IDDC TO W-IDDC-B6                                          
020700       PERFORM IMS-GU-WDB601                                              
020800       PERFORM BA-SKAPA-HEADER                                            
020900       MOVE IN-IDDC    TO WS-IDDC-WEB                                     
021000       MOVE DCS-IDLANDX2           TO WS-DCS-IDLANDX2                     
021100       MOVE DCS-ADGMT-PADR(11:20)  TO WS-DCS-ADGMT-PADR                   
021200                                                                          
021300       PERFORM UNTIL END-OF-W47935                                        
021400         MOVE IN-IDDC TO W-IDDC-B6                                        
021500         PERFORM IMS-GU-WDB601                                            
021600         PERFORM B-WEB-LISTA                                              
021700         MOVE IN-IDDC  TO WS-IDDC-WEB                                     
021800         PERFORM S01-LAES-W47935                                          
021900       END-PERFORM                                                        
022000                                                                          
022100       PERFORM C-AVSLUTA-WEB-LISTA                                        
022200       PERFORM S05-SEND-CLOSE                                             
022300     END-IF                                                               
022400     PERFORM Z-FINIT                                                      
022500     MOVE ZERO TO RETURN-CODE                                             
022600     GOBACK                                                               
022700     .                                                                    
022800     EJECT                                                                
022900 A-INIT SECTION.                                                          
023000     SKIP2                                                                
023100                                                                          
023200     MOVE NEJ              TO W47935-EOF-SW                               
023300     OPEN INPUT W47935                                                    
023400                                                                          
023500     OPEN OUTPUT W47936                                                   
023600*                                                                         
023700     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
023800     MOVE D-AAR                TO  DAGENS-DATUM-AAR                       
023900     MOVE D-MAANAD             TO  DAGENS-DATUM-MAANAD                    
024000     MOVE D-DAG                TO  DAGENS-DATUM-DAG                       
024100*                                                                         
024200     ACCEPT WS-YYMMDD      FROM DATE                                      
024300     ACCEPT WS-HHMMSSTH    FROM TIME                                      
024400     MOVE WS-HHMM          TO WS-TIME                                     
024500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
024600**   OMVANDLA DAGENS-DATUM TILL ÅR, DAGNR                                 
024700     MOVE DAGENS-DATUM        TO DAT-I-TIDATUM                            
024800     MOVE 'AAMMDD'            TO DAT-KDDATFORM                            
024900                                                                          
025000     CALL WDATKONV USING   DAT-KDDATFORM,                                 
025100                           DAT-I-TIDATUM,                                 
025200                           DAT-O-TIDATUM,                                 
025300                           DAT-KDSVAR                                     
025400                                                                          
025500     IF DAT-KDSVAR-OK                                                     
025600       MOVE DAT-TIAARP          TO WS-TIAARP                              
025700     ELSE                                                                 
025800       MOVE 'FEL FRÅN WDATKONV I A-SECTION'                               
025900       TO ERROR-TEXT                                                      
026000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
026100     END-IF                                                               
026200                                                                          
026300     MOVE ZERO              TO WS-IN-ANTAL-AVVIKELSE-RADER                
026400**                                                                        
026500     MOVE ZERO              TO WS-IN-ANTAL-AVVIKELSE-RADER                
026600**                                                                        
026800     .                                                                    
026900     EJECT                                                                
027000 B-WEB-LISTA  SECTION.                                                    
027100     MOVE     'B-WEB-LISTA   '            TO PGM-POS                      
027200                                                                          
027300     IF IN-IDDC NOT = WS-IDDC-WEB                                         
027400       MOVE IN-IDDC TO WS-IDDC-WEB                                        
027500       PERFORM BB-MOVE-LINES                                              
027600       PERFORM S05-PUT-REPORT-LINE                                        
027700*                                                                         
027800       PERFORM BC-WRITE-W47936-LIST                                       
027900       PERFORM S05-SEND-CLOSE                                             
028000       PERFORM BE-NOLLA-WS                                                
028100                                                                          
028200       PERFORM BA-SKAPA-HEADER                                            
028300       PERFORM BD-ADDERA-DATA                                             
028400     ELSE                                                                 
028500*                                                                         
028600       PERFORM BD-ADDERA-DATA                                             
028700     END-IF                                                               
028800     .                                                                    
028900     SKIP3                                                                
029000 BA-SKAPA-HEADER SECTION.                                                 
029100     MOVE 'BA-SKAPA-HEADER   '            TO PGM-POS                      
029200                                                                          
029300     MOVE 1                          TO HDR-REQU-IDMSGVER                 
029400     MOVE 'E'                        TO HDR-REQU-KDPGMACT                 
029500     MOVE IDPGM                      TO HDR-REQU-IDUSER                   
029600                                                                          
029700     MOVE SPACE                      TO HDR-IDOUTREC                      
029800     MOVE 'NILPICKSDEVPER'           TO HDR-IDOUTTYPE                     
029900     MOVE IN-IDDC                    TO HDR-IDOUTREC(1:2)                 
030000     MOVE 'W47936'                   TO HDR-IDOUTREC(3:8)                 
030100     MOVE WS-YYMMDDHHMM              TO HDR-IDLIST                        
030200                                                                          
030300     PERFORM S05-SEND-OPEN                                                
030400     MOVE SEND-IDCOM TO WZ04-SEND-IDCOM                                   
030500*HDR                                                                      
030600     PERFORM S05-PUT-HEADER                                               
030700     .                                                                    
030800     SKIP3                                                                
030900 BB-MOVE-LINES       SECTION.                                             
031000     MOVE 'BB-MOVE-LINES     '            TO PGM-POS                      
031100                                                                          
031200     MOVE '1'                        TO WEB-LINE-IDAFPRCD                 
031300                                                                          
031400     MOVE WS-IDDC                    TO WEB-LINE-IDDC                     
031500                                                                          
031600     MOVE WS-TIAARP                  TO WEB-LINE-TIAAVV                   
031700                                                                          
031800     MOVE WS-DCS-IDLANDX2            TO WEB-LINE-IDLANDX2                 
031900                                                                          
032000     MOVE WS-DCS-ADGMT-PADR          TO WEB-LINE-ADCITY                   
032100                                                                          
032200     MOVE WS-TOT-ANTAL-AVVIKELSE-RADER                                    
032300       TO WEB-LINE-ANTAL-AVVIKELSE-RADER                                  
032400                                                                          
032500     MOVE WS-TOTALT-ANTAL-RADER                                           
032600       TO WEB-LINE-TOTALT-ANTAL-RADER                                     
032700                                                                          
032800     MOVE WS-TOT-PROCENT             TO WEB-LINE-PROCENT                  
032900     .                                                                    
033000     SKIP3                                                                
033100 BC-WRITE-W47936-LIST  SECTION.                                           
033200     MOVE 'BC-WRITE-W47936-LIST'          TO PGM-POS                      
033300                                                                          
033400     MOVE WS-IDDC                    TO OUT-IDDC                          
033500     MOVE WS-KDMFUP                  TO OUT-KDMFUP                        
033600                                                                          
033700     MOVE WS-TIAARP                  TO OUT-TIAAVV                        
033800**                                                                        
033900     MOVE WS-TOT-ANTAL-AVVIKELSE-RADER                                    
034000                          TO OUT-ANTAL-AVVIKELSE-RADER                    
034100**                                                                        
034200     MOVE WS-TOTALT-ANTAL-RADER                                           
034300                          TO OUT-TOTALT-ANTAL-RADER                       
034400**                                                                        
034500     MOVE WS-TOT-PROCENT             TO OUT-PROCENT                       
034600                                                                          
034700     PERFORM S11-WRITE-W47936                                             
034800     .                                                                    
034900     EJECT                                                                
035000 BE-NOLLA-WS     SECTION.                                                 
035100     MOVE 'BE-NOLLA-WS         '          TO PGM-POS                      
035200**                                                                        
035300     MOVE ZERO              TO WS-IN-ANTAL-AVVIKELSE-RADER                
035400     MOVE ZERO              TO WS-TOT-ANTAL-AVVIKELSE-RADER               
035500**                                                                        
035600     MOVE ZERO              TO WS-IN-TOTALT-ANTAL-RADER                   
035700     MOVE ZERO              TO WS-TOTALT-ANTAL-RADER                      
035800**                                                                        
036000     MOVE ZERO              TO WS-TOT-PROCENT                             
036100**                                                                        
036200     MOVE SPACE             TO OUT-POST                                   
036300     MOVE SPACE             TO OUT-AREA                                   
036400     .                                                                    
036500     EJECT                                                                
036600 BD-ADDERA-DATA SECTION.                                                  
036700     MOVE 'BD-ADDERA-DATA      '          TO PGM-POS                      
036800**                                                                        
036900     MOVE IN-IDDC                    TO WS-IDDC                           
037000     MOVE IN-KDMFUP                  TO WS-KDMFUP                         
037100**                                                                        
037200     MOVE DCS-IDLANDX2               TO WS-DCS-IDLANDX2                   
037300**                                                                        
037400     MOVE DCS-ADGMT-PADR(11:20)      TO WS-DCS-ADGMT-PADR                 
037500*                                                                         
037600     MOVE IN-ANTAL-AVVIKELSE-RADER                                        
037700                          TO WS-IN-ANTAL-AVVIKELSE-RADER                  
037800                                                                          
037900     COMPUTE WS-TOT-ANTAL-AVVIKELSE-RADER =                               
038000       WS-TOT-ANTAL-AVVIKELSE-RADER + WS-IN-ANTAL-AVVIKELSE-RADER         
038100     END-COMPUTE                                                          
038200**                                                                        
038300     MOVE IN-TOTALT-ANTAL-RADER                                           
038400                          TO WS-IN-TOTALT-ANTAL-RADER                     
038500                                                                          
038600     COMPUTE WS-TOTALT-ANTAL-RADER =                                      
038700        WS-TOTALT-ANTAL-RADER + WS-IN-TOTALT-ANTAL-RADER                  
038800     END-COMPUTE                                                          
038900**                                                                        
039200     COMPUTE WS-TOT-PROCENT = WS-TOT-ANTAL-AVVIKELSE-RADER * 100          
039300                              / WS-TOTALT-ANTAL-RADER                     
039400     END-COMPUTE                                                          
039500**                                                                        
039600     .                                                                    
039700     EJECT                                                                
039800 C-AVSLUTA-WEB-LISTA   SECTION.                                           
039900     MOVE 'C-AVSLUTA-WEB-LISTA '          TO PGM-POS                      
040000                                                                          
040100     PERFORM BB-MOVE-LINES                                                
040200     PERFORM S05-PUT-REPORT-LINE                                          
040300     PERFORM BC-WRITE-W47936-LIST                                         
040400     .                                                                    
040500     EJECT                                                                
040600 Z-FINIT SECTION.                                                         
040700                                                                          
040800     CLOSE W47935                                                         
040900           W47936                                                         
041000                                                                          
041100     SKIP2                                                                
041200     MOVE 'S' TO POSTSUM-OPKOD                                            
041300     CALL POSTSUM USING POSTSUM-PARM                                      
041400     .                                                                    
041500     EJECT                                                                
041600 S01-LAES-W47935  SECTION.                                                
041700     MOVE 'S01-LAES-W47935     '          TO PGM-POS                      
041800                                                                          
041900     SKIP2                                                                
042000     READ W47935 INTO IN-AREA                                             
042100     AT END                                                               
042200****    MOVE HIGH-VALUE TO IN-IDDC                                        
042300        SET END-OF-W47935 TO TRUE                                         
042400                                                                          
042500     NOT AT END                                                           
042600        MOVE 'W47936' TO POSTSUM-FDNAMN                                   
042700        MOVE 'W47936D1' TO POSTSUM-DDNAMN2                                
042800        MOVE 'IN-'     TO POSTSUM-TRANSTYP                                
042900        CALL POSTSUM USING POSTSUM-PARM                                   
043000     END-READ                                                             
043100*     DISPLAY 'W47935-EOF-SW:' W47935-EOF-SW                              
043200*     DISPLAY 'POSTSUM-TOTTRANS=' POSTSUM-TOTTRANS                        
043300     .                                                                    
043400     EJECT                                                                
043500 S05-SEND-OPEN SECTION.                                                   
043600     MOVE 'S05-SEND-OPEN       '          TO PGM-POS                      
043700                                                                          
043800     MOVE 'CARPARTS.DAP.DISTRDOC'         TO SEND-ADDISPABS               
043900     MOVE 'OPEN'                          TO SEND-KDFUNC                  
044000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
044100                         SEND-OPEN-AREA                                   
044200     IF SEND-KDRC > ZERO                                                  
044300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
044400       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
044500       DELIMITED BY SIZE INTO ERROR-TEXT                                  
044600       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
044700     END-IF                                                               
044800                                                                          
044900     .                                                                    
045000     SKIP3                                                                
045100 S05-PUT-HEADER SECTION.                                                  
045200     MOVE ' S05-PUT-HEADER     '          TO PGM-POS                      
045300                                                                          
045400     MOVE 'PUT'                           TO SEND-KDFUNC                  
045500     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
045600     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
045700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
045800                         SEND-KVDLEN                                      
045900                         HDR-AREA                                         
046000     IF SEND-KDRC > ZERO                                                  
046100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
046200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
046300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
046400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
046500     END-IF                                                               
046600*    DISPLAY ' HEAD-AREA ' HDR-AREA                                       
046700     .                                                                    
046800     EJECT                                                                
046900 S05-PUT-REPORT-LINE    SECTION.                                          
047000     MOVE 'S05-PUT-REPORT-LINE'           TO PGM-POS                      
047100                                                                          
047200     MOVE 'PUT'                           TO SEND-KDFUNC                  
047300     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
047400     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
047500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
047600                         SEND-KVDLEN                                      
047700                         DOC-LINE-AREA                                    
047800     IF SEND-KDRC > ZERO                                                  
047900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
048000       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
048100       DELIMITED BY SIZE INTO ERROR-TEXT                                  
048200       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
048300     END-IF                                                               
048400     DISPLAY 'DOC-LINE-AREA=' DOC-LINE-AREA                               
048500     .                                                                    
048600     SKIP3                                                                
048700 S05-SEND-CLOSE SECTION.                                                  
048800     MOVE 'S05-SEND-CLOSE     '           TO PGM-POS                      
048900                                                                          
049000     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
049100     MOVE WZ04-SEND-IDCOM                 TO SEND-IDCOM                   
049200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
049300                                                                          
049400     IF SEND-KDRC > ZERO                                                  
049500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
049600       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
049700       DELIMITED BY SIZE INTO ERROR-TEXT                                  
049800       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
049900     END-IF                                                               
050000     .                                                                    
050100 S11-WRITE-W47936 SECTION.                                                
050200     MOVE 'S11-WRITE-W47936   '           TO PGM-POS                      
050300                                                                          
050400     WRITE OUT-POST   FROM OUT-AREA                                       
050500                                                                          
050600     MOVE '101'        TO POSTSUM-TRANSTYP                                
050700     MOVE 'W47936'     TO POSTSUM-FDNAMN                                  
050800     MOVE 'W47936D2'   TO POSTSUM-DDNAMN2                                 
050900     CALL POSTSUM USING POSTSUM-PARM                                      
051000     .                                                                    
051100     EJECT                                                                
051200 IMS-GU-WDB601    SECTION.                                                
051300     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
051400          DELIMITED BY SIZE INTO SSA1                                     
051500     MOVE '  GE' TO GODK-STATUSKODER                                      
051600     CALL CBLTDLI USING GU  WDB6-PCB DLI-IO-AREA-B601 SSA1                
051700     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
051800     PERFORM IMS-STATUSKONTROLL                                           
051900     .                                                                    
052000     EJECT                                                                
052100 IMS-STATUSKONTROLL SECTION.                                              
052200                                                                          
052300     SET STATUS-IX TO 1                                                   
052400     SEARCH GODK-STATUS AT END CALL FELLOG                                
052500        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                          
052600        CONTINUE                                                          
052700     END-SEARCH                                                           
052800     .                                                                    
