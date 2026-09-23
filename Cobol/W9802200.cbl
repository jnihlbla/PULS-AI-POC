000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W9802200.                                                 
000400 AUTHOR.        KJELL ANDRE.                                              
000500     DATE-WRITTEN.  MARS 1985.                                            
000600                                                                          
000700     REMARKS.                                                             
000800                                                                          
000900*    FUNKTION:                                                            
001000*        PROGRAMMET UTGÖR ETT KOMMANDO-INTERFACE TILL SOP.                
001100*        DET KÖRS VIA EN PROCEDUR, OCH LÄSER PARM-INFO                    
001200*        FRÅN EXEC-KORTET. OM PARM = SYSIN LÄSES KOMMANDON                
001300*        FRÅN INPUT-FIL, I ANNAT FALL TAS KOMMANDOT DIREKT                
001400*        FRÅN EXEC-PARAMETERN.                                            
001500*                                                                         
001600*    SUBPROGRAM:                                                          
001700*        W9802000        - STATUS-ÄNDRINGAR AV PROCESSER                  
001800*        W980WSPC        - ÖVRIGA ACCESSER MOT SOP-REGISTRET              
001900*                                                                         
002000     EJECT                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400 FILE-CONTROL.                                                            
002500                                                                          
002600     SELECT KOMMANDOFIL     ASSIGN TO W98022D1.                           
002700                                                                          
002800     SELECT PLANFIL         ASSIGN TO W98022D2.                           
002900                                                                          
003000*    DDNAMN  SOPDD1  ANVÄNDS AV SUBPROGRAM W9802000 OCH W980WSPC          
003100*            SOPDD2   -"-                  W980WSPC                       
003200*            PDSDD1   -"-                  W9802000                       
003300*            PDSDD2   -"-                  W9802000                       
003400*                                                                         
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700 FILE SECTION.                                                            
003800                                                                          
003900 FD  KOMMANDOFIL                                                          
004000     LABEL RECORD STANDARD                                                
004100     RECORDING MODE F                                                     
004200     BLOCK CONTAINS 0 RECORDS.                                            
004300                                                                          
004400 01  FILLER                      PIC X(80).                               
004500     EJECT                                                                
004600                                                                          
004700 FD  PLANFIL                                                              
004800     LABEL RECORD STANDARD                                                
004900     RECORDING MODE F                                                     
005000     BLOCK CONTAINS 0 RECORDS.                                            
005100                                                                          
005200*01  PLANPOST -COPY WSOPPLAN   -L                                         
005300     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005401*    -- CHECKED BY WY2000                                                 
005410     SKIP3                                                                
005500*                                                                         
005600 77  PROGRAM-NAMN                PIC X(8) VALUE 'W9802200'.               
005700                                                                          
005800 01  KONSTANTER.                                                          
005900     03  JA                      PIC X      VALUE 'J'.                    
006000     03  NEJ                     PIC X      VALUE 'N'.                    
006100     03  PROCESS-DIRECT          PIC X      VALUE 'D'.                    
006200     03  PROCESS-ALL             PIC X      VALUE 'A'.                    
006300*                                                                         
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500     03  ABEND                   PIC X(8)   VALUE 'ABEND'.                
006600     03  W980WSPC                PIC X(8)   VALUE 'W980WSPC'.             
006700     03  W9802000                PIC X(8)   VALUE 'W9802000'.             
006800                                                                          
006900*-----  SWITCHAR                                                          
007000 01  EOF-KOMMANDOFIL             PIC X.                                   
007100 01  KOMMANDOFIL-OPEN            PIC X.                                   
007200 01  PLANFIL-OPEN                PIC X.                                   
007300 01  PARM-KOMMANDO               PIC X.                                   
007400 01  PROCESS-SUBPROC             PIC X.                                   
007500                                                                          
007600 01  MIN-STATUS                  PIC X.                                   
007700 01  STATUS-NR                   PIC X.                                   
007800 01  ONSKAD-STATUS               PIC X.                                   
007900 01  FEL-ORD                     PIC X.                                   
008000 01  ADD-SYMBOLER                PIC X.                                   
008100 01  SYMB-FEL                    PIC X.                                   
008200 01  CATALOG-ORD                 PIC X(100).                              
008300                                                                          
008400 01  RKOD                        PIC S9(4) COMP  VALUE ZERO.              
008500 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4) COMP  VALUE +16.               
008600 01  PTR                         PIC S9(4) COMP  VALUE ZERO.              
008700 01  NXT-ORD                     PIC S9(4) COMP  VALUE ZERO.              
008800                                                                          
008900 01  L                           PIC S9(4) COMP VALUE +1.                 
009000 01  L-SPAR                      PIC S9(4).                               
009100 01  L-MAX                       PIC S9(4) COMP VALUE +10.                
009200 01  VILLKORS-TAB.                                                        
009300     03  VILLKOR OCCURS 10       PIC X.                                   
009400                                                                          
009500     EJECT                                                                
009600*----- ARBETS-AREOR FÖR EXPAND-KOMMANDOT                                  
009700                                                                          
009800 01  EXPAND-RAD                  PIC X(105).                              
009900 01  FILLER REDEFINES EXPAND-RAD.                                         
010000     03  FILLER                  PIC X(30).                               
010100     03  EXPAND-STATUS           PIC X(10).                               
010200     03  EXPAND-NOTE             PIC X(30).                               
010300     03  FILLER                  PIC X(5).                                
010400     03  EXPAND-ATTN             PIC X(30).                               
010500                                                                          
010600 01  TID-ZONAT               PIC 9(4).                                    
010700 01  FILLER REDEFINES TID-ZONAT.                                          
010800     03  TID-HH              PIC XX.                                      
010900     03  TID-MM              PIC XX.                                      
011000     EJECT                                                                
011100*----- ARBETS-AREOR FÖR TOLKNING AV ETT KOMMANDO OCH FELUTKRIFTER         
011200                                                                          
011300 01  KOMMANDO-RAD.                                                        
011400     03  KOMMANDO-TEXT           PIC X(72).                               
011500     03  FILLER                  PIC X(8).                                
011600                                                                          
011700 01  KOMMANDO-ORD                PIC X(16).                               
011800                                                                          
011900 01  ORD-TAB.                                                             
012000     03  ORD  OCCURS 8           PIC X(30).                               
012100                                                                          
012200 01  LENGD-TAB.                                                           
012300     03  LENGD OCCURS 8          PIC S9(4) COMP.                          
012400                                                                          
012500 01  WORD                        PIC X(16).                               
012600 01  WORD2                       PIC X(16).                               
012700                                                                          
012800 01  TRASH                       PIC S9(4) COMP.                          
012900                                                                          
013000                                                                          
013100 01  TAL-X.                                                               
013200     03 TAL-DISPLAY              PIC 9(3).                                
013300                                                                          
013400 01  DATUM-X                     PIC X(6).                                
013500                                                                          
013600 01  DISPLAY-STATUS              PIC X(20).                               
013700 01  DISPLAY-TYPE                PIC X(10).                               
013800 01  DISPLAY-FTID                PIC 9B9999.                              
013900 01  DISPLAY-DATUM               PIC 99B99B99.                            
014000 01  DISPLAY-TID1                PIC 99B99.                               
014100 01  DISPLAY-TID2                PIC 99B99.                               
014200     EJECT                                                                
014300 01  FILLER                      PIC X(16) VALUE 'SIX-START'.             
014400 01  SIX                         PIC S9(4) COMP.                          
014500 01  SIX-FULL                    PIC S9(4) COMP  VALUE +10.               
014600 01  STACK.                                                               
014700     03 STACK-PROCESS-PARM OCCURS 10.                                     
014800        05  FILLER               PIC S9(4) COMP.                          
014900        05  STACK-IDPROCESS      PIC X(10).                               
015000                                                                          
015100     EJECT                                                                
015200*01  -COPY WSOPPLAN                                                       
015300     EJECT                                                                
015400*01  -COPY W98020                                                         
015500     SKIP3                                                                
015600 01  TEMP-RAD                    PIC X(80).                               
015700 01  END-TEMP-RAD                PIC X(20).                               
015800 01  FILL                        PIC X(20).                               
015900     SKIP2                                                                
016000 01  RAD-IX                      PIC S9(4).                               
016100     EJECT                                                                
016200*-----  PARAMETRAR TILL W980WSPC (HANTERING AV SOP-REGISTRET)             
016300 01  FUNKTIONSKODER.                                                      
016400     03  FCOPY                   PIC X(4)   VALUE 'COPY'.                 
016500     03  FOPEN                   PIC X(4)   VALUE 'OPEN'.                 
016600     03  FCLSE                   PIC X(4)   VALUE 'CLSE'.                 
016700     03  FQUIT                   PIC X(4)   VALUE 'QUIT'.                 
016800     03  FSAVE                   PIC X(4)   VALUE 'SAVE'.                 
016900     03  FGETF                   PIC X(4)   VALUE 'GETF'.                 
017000     03  FGETN                   PIC X(4)   VALUE 'GETN'.                 
017100     03  FTEST                   PIC X(4)   VALUE 'TEST'.                 
017200     03  FDELK                   PIC X(4)   VALUE 'DELK'.                 
017300     03  FDEL                    PIC X(4)   VALUE 'DEL '.                 
017400     03  FSET                    PIC X(4)   VALUE 'SET '.                 
017500                                                                          
017600 01  WSRKOD                      PIC X.                                   
017700                                                                          
017800*--------  SPECIELLA ARBETS-PARAMETRAR                                    
017900 01  WSPACE-DD-PARM.                                                      
018000     03  WSPACE-DD-LENGD         PIC S9(4)  COMP   VALUE +10.             
018100     03  WSPACE-DD-NAMN          PIC X(8)   VALUE 'SOPDD1  '.             
018200                                                                          
018300 01  WSPACE-DD-PARM2.                                                     
018400     03  WSPACE-DD-LENGD2        PIC S9(4)  COMP   VALUE +10.             
018500     03  WSPACE-DD-NAMN2         PIC X(8)   VALUE 'SOPDD2  '.             
018600                                                                          
018700 01  BLOCK-COUNT-PARM.                                                    
018800     03  BLOCK-COUNT-LENGD       PIC S9(4)  COMP.                         
018900     03  BLOCK-COUNT             PIC X(6).                                
019000                                                                          
019100 01  INFO-NAMN-PARM.                                                      
019200     03  FILLER                  PIC S9(4)  COMP  VALUE +5.               
019300     03  FILLER                  PIC X(3)   VALUE 'INF'.                  
019400                                                                          
019500 01  INFO-VAERDE-PARM.                                                    
019600*    03  -COPY W980INF                                                    
019700                                                                          
019800 01  DATUM-PARM.                                                          
019900     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
020000     03  DATUM-VAERDE            PIC S9(7) COMP-3.                        
020100                                                                          
020200 01  P-PARM.                                                              
020300     03  P-LENGD                 PIC S9(4) COMP.                          
020400     03  P-IDPROCESS             PIC X(10).                               
020500                                                                          
020600 01  TEMP-PARM.                                                           
020700     03  TEMP-LENGD              PIC S9(4) COMP.                          
020800     03  TEMP-IDPROCESS          PIC X(10).                               
020900                                                                          
021000 01  AFTER-PARM.                                                          
021100     03  FILLER                  PIC S9(4) COMP  VALUE +3.                
021200     03  FILLER                  PIC X(1)  VALUE '<'.                     
021300                                                                          
021400 01  AKTQ-PARM.                                                           
021500     03  FILLER                  PIC S9(4) COMP  VALUE +4.                
021600     03  FILLER                  PIC X(2)  VALUE 'AQ'.                    
021700                                                                          
021800 01  ATTN-PARM.                                                           
021900     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
022000     03  FILLER                  PIC X(4)  VALUE 'ATTN'.                  
022100                                                                          
022200 01  BEFORE-PARM.                                                         
022300     03  FILLER                  PIC S9(4) COMP  VALUE +3.                
022400     03  FILLER                  PIC X(1)  VALUE '>'.                     
022500                                                                          
022600 01  CATALOG-PARM.                                                        
022700     03  FILLER                  PIC S9(4) COMP  VALUE +5.                
022800     03  FILLER                  PIC X(3)  VALUE 'CAT'.                   
022900                                                                          
023000 01  CONTAINS-PARM.                                                       
023100     03  FILLER                  PIC S9(4) COMP  VALUE +5.                
023200     03  FILLER                  PIC X(3)  VALUE 'CON'.                   
023300                                                                          
023400 01  DB-STATUS-PARM.                                                      
023500     03  FILLER                  PIC S9(4) COMP  VALUE +8.                
023600     03  FILLER                  PIC X(6)  VALUE 'DBSTAT'.                
023700                                                                          
023800 01  DATUM-LIST-PARM.                                                     
023900     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
024000     03  FILLER                  PIC X(4)  VALUE 'DATL'.                  
024100                                                                          
024200 01  EJ-SAMTIDIGT-PARM.                                                   
024300     03  FILLER                  PIC S9(4) COMP  VALUE +4.                
024400     03  FILLER                  PIC X(2)  VALUE '<>'.                    
024500                                                                          
024600 01  FTID-PARM.                                                           
024700     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
024800     03  FILLER                  PIC X(4)  VALUE 'FTID'.                  
024900                                                                          
025000 01  HOLD-PARM.                                                           
025100     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
025200     03  FILLER                  PIC X(4)  VALUE 'HOLD'.                  
025300                                                                          
025400 01  KALENDER-PARM.                                                       
025500     03  FILLER                  PIC S9(4) COMP  VALUE +7.                
025600     03  FILLER                  PIC X(5)  VALUE '*CAL*'.                 
025700                                                                          
025800 01  MTYP-PARM.                                                           
025900     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
026000     03  FILLER                  PIC X(4)  VALUE 'MTYP'.                  
026100                                                                          
026200 01  NUDAT-PARM.                                                          
026300     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
026400     03  FILLER                  PIC X(4)  VALUE 'CDAT'.                  
026500                                                                          
026600 01  NULL-PARM.                                                           
026700     03  FILLER                  PIC S9(4) COMP  VALUE +2.                
026800                                                                          
026900 01  NOT-CATALOG-PARM.                                                    
027000     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
027100     03  FILLER                  PIC X(4)  VALUE 'NCAT'.                  
027200                                                                          
027300 01  PARENT-PARM.                                                         
027400     03  FILLER                  PIC S9(4) COMP  VALUE +5.                
027500     03  FILLER                  PIC X(3)  VALUE 'PAR'.                   
027600                                                                          
027700 01  PASSQ-PARM.                                                          
027800     03  FILLER                  PIC S9(4) COMP  VALUE +4.                
027900     03  FILLER                  PIC X(2)  VALUE 'DQ'.                    
028000                                                                          
028100 01  PRIO-PARM.                                                           
028200     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
028300     03  FILLER                  PIC X(4)  VALUE 'PRIO'.                  
028400                                                                          
028500 01  RESQ-PARM.                                                           
028600     03  FILLER                  PIC S9(4) COMP  VALUE +4.                
028700     03  FILLER                  PIC X(2)  VALUE 'RQ'.                    
028800                                                                          
028900 01  SOP-PARM.                                                            
029000     03  FILLER                  PIC S9(4) COMP  VALUE +7.                
029100     03  FILLER                  PIC X(7)  VALUE '*SOP*'.                 
029200                                                                          
029300 01  STARTED-LIST-PARM.                                                   
029400     03  FILLER                  PIC S9(4) COMP  VALUE +4.                
029500     03  FILLER                  PIC X(2)  VALUE 'SL'.                    
029600                                                                          
029700 01  STARTTYP-PARM.                                                       
029800     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
029900     03  FILLER                  PIC X(4)  VALUE 'STYP'.                  
030000                                                                          
030100 01  STARTQ-PARM.                                                         
030200     03  FILLER                  PIC S9(4) COMP  VALUE +4.                
030300     03  FILLER                  PIC X(2)  VALUE 'SQ'.                    
030400                                                                          
030500 01  SYMB-PARM.                                                           
030600     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
030700     03  FILLER                  PIC X(4)  VALUE 'SYMB'.                  
030800                                                                          
030900 01  USED-PARM.                                                           
031000     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
031100     03  FILLER                  PIC X(4)  VALUE 'USED'.                  
031200                                                                          
031300 01  USING-PARM.                                                          
031400     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
031500     03  FILLER                  PIC X(4)  VALUE 'USNG'.                  
031600                                                                          
031700 01  VOUT-PARM.                                                           
031800     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
031900     03  FILLER                  PIC X(4)  VALUE 'VOUT'.                  
032000                                                                          
032100 01  ACTMSG-PARM.                                                         
032200     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
032300     03  FILLER                  PIC X(4)  VALUE 'AMSG'.                  
032400                                                                          
032500 01  ABEND-JOB-PARM.                                                      
032600     03  FILLER                  PIC S9(4) COMP  VALUE +6.                
032700     03  FILLER                  PIC X(4)  VALUE 'ABJB'.                  
032800                                                                          
032900*--------  GENERELLA ARBETS-PARAMETRAR                                    
033000 01  NAMN-PARM.                                                           
033100     03  NAMN-LENGD              PIC S9(4) COMP.                          
033200     03  NAMN-VARDE              PIC X(10).                               
033300                                                                          
033400 01  ATTR-PARM.                                                           
033500     03  ATTR-LENGD              PIC S9(4) COMP.                          
033600     03  ATTR-VARDE              PIC X(10).                               
033700                                                                          
033800 01  DATA-PARM.                                                           
033900     03  DATA-LENGD              PIC S9(4) COMP.                          
034000     03  DATA-VARDE              PIC X(50).                               
034100                                                                          
034200     EJECT                                                                
034300 LINKAGE SECTION.                                                         
034400                                                                          
034500 01  EXEC-PARM.                                                           
034600     03  PARM-LAENGD         PIC S9(4) COMP.                              
034700     03  PARM-VAERDE         PIC X(80).                                   
034800     EJECT                                                                
034900 PROCEDURE DIVISION USING EXEC-PARM.                                      
035000     SKIP2                                                                
035100     PERFORM A-INIT-KONTROLL                                              
035200     IF EOF-KOMMANDOFIL = NEJ OR PARM-KOMMANDO = JA                       
035300       IF KOMMANDO-ORD = 'COPY'                                           
035310         DISPLAY ' '                                                      
035320         DISPLAY KOMMANDO-TEXT                                            
035400         PERFORM C-TOLKA-KOMMANDO                                         
035500         PERFORM F-COPY-SOPREG                                            
035600       ELSE                                                               
035700         PERFORM X-OPEN-SOP                                               
035800         PERFORM UNTIL EOF-KOMMANDOFIL = JA AND                           
035900                      PARM-KOMMANDO = NEJ                                 
035910           DISPLAY ' '                                                    
035920           DISPLAY KOMMANDO-TEXT                                          
035930                                                                          
036000           PERFORM C-TOLKA-KOMMANDO                                       
036100           IF SOP20-KDSOPFUNK = 'Q'                                       
036200             PERFORM H-START-STATUS-VILLKOR                               
036300           ELSE IF SOP20-KDSOPFUNK = 'W'                                  
036400             PERFORM I-START-KALENDER-VILLKOR                             
036500           ELSE IF SOP20-KDSOPFUNK = 'Ö'                                  
036600             PERFORM IS-START-SYMBOL-VILLKOR                              
036700           ELSE IF SOP20-KDSOPFUNK = 'Ä'                                  
036800             PERFORM J-SLUT-VILLKOR                                       
036900           ELSE IF VILLKOR (L) = JA                                       
037000             IF SOP20-KDRET = 0                                           
037100               EVALUATE SOP20-KDSOPFUNK                                   
037200                 WHEN 'E' PERFORM D-CALL-W98020                           
037300                 WHEN 'A' PERFORM D-CALL-W98020                           
037400                 WHEN 'P' PERFORM D-CALL-W98020                           
037500                 WHEN 'S' PERFORM D-CALL-W98020                           
037600                 WHEN 'F' PERFORM D-CALL-W98020                           
037700                 WHEN 'O' PERFORM D-CALL-W98020                           
037800                 WHEN 'C' PERFORM D-CALL-W98020                           
037900                 WHEN 'H' PERFORM D-CALL-W98020                           
038000                 WHEN 'R' PERFORM D-CALL-W98020                           
038100                 WHEN 'Z' PERFORM D-CALL-W98020                           
038200                 WHEN 'I' PERFORM D-CALL-W98020                           
038300                 WHEN 'K' PERFORM G-LIST-KALENDER                         
038400                 WHEN 'D' PERFORM K-DELETE-PROCESS                        
038500                 WHEN 'B' PERFORM L-PLAN-PROCESS                          
038600                 WHEN 'X' PERFORM M-EXPAND-PROCESS                        
038700                 WHEN 'V' PERFORM N-SET-PARAMETER                         
038800               END-EVALUATE                                               
038900             END-IF                                                       
039000             IF SOP20-KDRET > 4                                           
039100               DISPLAY 'SOP151E  COMMAND FAILED.'                         
039200             ELSE                                                         
039300               DISPLAY 'SOP150I  COMMAND COMPLETED.'                      
039400             END-IF                                                       
039500           ELSE                                                           
039600             DISPLAY 'SOP153I  COMMAND SKIPPED.'                          
039700           END-IF                                                         
039800           END-IF                                                         
039900           END-IF                                                         
040000           END-IF                                                         
040100           END-IF                                                         
040200           IF PARM-KOMMANDO = NEJ AND EOF-KOMMANDOFIL = NEJ               
040300             PERFORM S1-LAES-KOMMANDOFIL                                  
040400           ELSE                                                           
040500             MOVE NEJ TO PARM-KOMMANDO                                    
040600           END-IF                                                         
040700         END-PERFORM                                                      
040800         PERFORM Y-CLOSE-SOP                                              
040900       END-IF                                                             
041000     END-IF                                                               
041100                                                                          
041200     PERFORM Z-FINIT                                                      
041300     MOVE RKOD TO RETURN-CODE                                             
041400     GOBACK                                                               
041500     .                                                                    
041600     EJECT                                                                
041700 A-INIT-KONTROLL  SECTION.                                                
041800     SKIP2                                                                
041900     MOVE JA TO VILLKOR (1)                                               
042000     MOVE JA TO SOP20-FLBATCH                                             
042100     MOVE SPACE TO SOP20-IDDDPREFIX                                       
042200     MOVE NEJ TO PLANFIL-OPEN                                             
042300                                                                          
042400     UNSTRING PARM-VAERDE DELIMITED  BY '/' INTO KOMMANDO-RAD             
042500     PERFORM  S2-UNSTRING-KOMMANDO                                        
042600                                                                          
042700     IF KOMMANDO-ORD = 'SYSIN'                                            
042800        MOVE NEJ TO PARM-KOMMANDO                                         
042900        MOVE NEJ TO EOF-KOMMANDOFIL                                       
043000        MOVE JA TO KOMMANDOFIL-OPEN                                       
043100        OPEN INPUT KOMMANDOFIL                                            
043200        PERFORM S1-LAES-KOMMANDOFIL                                       
043300     ELSE                                                                 
043400        MOVE JA TO PARM-KOMMANDO                                          
043500        MOVE JA TO EOF-KOMMANDOFIL                                        
043600        MOVE NEJ TO KOMMANDOFIL-OPEN                                      
043700     END-IF                                                               
043800                                                                          
043900     ACCEPT DISPLAY-DATUM FROM DATE                                       
044000     DISPLAY 'AB VOLVO                '                                   
044100       'S O P  -  BATCH COMMAND INTERPRETER            '                  
044200       'DATE  ' DISPLAY-DATUM                                             
044300     .                                                                    
044400     EJECT                                                                
044500 C-TOLKA-KOMMANDO SECTION.                                                
044600     SKIP2                                                                
044900     MOVE ZERO TO SOP20-KDRET                                             
045000                                                                          
045100     IF KOMMANDO-ORD = 'END'                                              
045200        MOVE 'E' TO SOP20-KDSOPFUNK                                       
045300        PERFORM CD-KOLLA-PROCESS                                          
045400     ELSE IF KOMMANDO-ORD = 'ACTIVATE'                                    
045500        MOVE 'A' TO SOP20-KDSOPFUNK                                       
045600        PERFORM CD-KOLLA-PROCESS                                          
045700        PERFORM CE-KOLLA-SYMBOLS                                          
045800     ELSE IF KOMMANDO-ORD = 'PASSIVATE'                                   
045900        MOVE 'P' TO SOP20-KDSOPFUNK                                       
046000        PERFORM CD-KOLLA-PROCESS                                          
046100     ELSE IF KOMMANDO-ORD = 'START'                                       
046200        MOVE 'S' TO SOP20-KDSOPFUNK                                       
046300        PERFORM CD-KOLLA-PROCESS                                          
046400     ELSE IF KOMMANDO-ORD = 'FREE'                                        
046500        MOVE 'F' TO SOP20-KDSOPFUNK                                       
046600        PERFORM CD-KOLLA-PROCESS                                          
046700     ELSE IF KOMMANDO-ORD = 'ORDER'                                       
046800        MOVE 'O' TO SOP20-KDSOPFUNK                                       
046900        PERFORM CD-KOLLA-PROCESS                                          
047000        PERFORM CA-ON-DATE                                                
047100     ELSE IF KOMMANDO-ORD = 'CANCEL'                                      
047200        MOVE 'C' TO SOP20-KDSOPFUNK                                       
047300        PERFORM CD-KOLLA-PROCESS                                          
047400        PERFORM CA-ON-DATE                                                
047500     ELSE IF KOMMANDO-ORD = 'IF-STATUS'                                   
047600        MOVE 'Q'   TO SOP20-KDSOPFUNK                                     
047700     ELSE IF KOMMANDO-ORD = 'IF-CALENDAR'                                 
047800        MOVE 'W'   TO SOP20-KDSOPFUNK                                     
047900     ELSE IF KOMMANDO-ORD = 'IF-SYMBOL'                                   
048000        MOVE 'Ö'   TO SOP20-KDSOPFUNK                                     
048100     ELSE IF KOMMANDO-ORD = 'ENDIF' OR 'END-IF'                           
048200        MOVE 'Ä'   TO SOP20-KDSOPFUNK                                     
048300     ELSE IF KOMMANDO-ORD = ABEND                                         
048400        MOVE 'Z' TO SOP20-KDSOPFUNK                                       
048500        PERFORM CD-KOLLA-PROCESS                                          
048600     ELSE IF KOMMANDO-ORD = 'HOLD'                                        
048700        MOVE 'H' TO SOP20-KDSOPFUNK                                       
048800        PERFORM CD-KOLLA-PROCESS                                          
048900     ELSE IF KOMMANDO-ORD = 'RELEASE'                                     
049000        MOVE 'R' TO SOP20-KDSOPFUNK                                       
049100        PERFORM CD-KOLLA-PROCESS                                          
049200     ELSE IF KOMMANDO-ORD = 'DISPLAY' OR 'INFO'                           
049300        MOVE 'I' TO SOP20-KDSOPFUNK                                       
049400        PERFORM CD-KOLLA-PROCESS                                          
049500     ELSE IF KOMMANDO-ORD = 'PLAN'                                        
049600        MOVE 'B' TO SOP20-KDSOPFUNK                                       
049700        PERFORM CD-KOLLA-PROCESS                                          
049800        PERFORM CA-ON-DATE                                                
049900     ELSE IF KOMMANDO-ORD = 'EXPAND'                                      
050000        MOVE 'X'   TO SOP20-KDSOPFUNK                                     
050100        PERFORM CD-KOLLA-PROCESS                                          
050200        PERFORM CC-EXPAND-OPERANDER                                       
050300     ELSE IF KOMMANDO-ORD = 'COPY'                                        
050400        MOVE ORD (2) TO BLOCK-COUNT                                       
050500        ADD 2 LENGD (2) GIVING BLOCK-COUNT-LENGD                          
050600     ELSE IF KOMMANDO-ORD = 'LIST-CALENDAR'                               
050700        MOVE 'K'  TO SOP20-KDSOPFUNK                                      
050800     ELSE IF KOMMANDO-ORD = 'DELETE'                                      
050900        MOVE 'D'  TO SOP20-KDSOPFUNK                                      
051000        PERFORM CD-KOLLA-PROCESS                                          
051100        PERFORM CB-AND-SUB-PROC                                           
051200     ELSE IF KOMMANDO-ORD = 'SET'                                         
051300        MOVE 'V'  TO SOP20-KDSOPFUNK                                      
051400     ELSE                                                                 
051410        IF VILLKOR (L) = JA                                               
051500          DISPLAY 'SOP152S  COMMAND NOT RECOGNISED.'                      
051600          MOVE SPACE TO SOP20-KDSOPFUNK                                   
051700          MOVE 12 TO SOP20-KDRET                                          
051800          IF RKOD < 12                                                    
051900            MOVE 12 TO RKOD                                               
052000          END-IF                                                          
052010        END-IF                                                            
052100     END-IF                                                               
052200     END-IF                                                               
052300     END-IF                                                               
052400     END-IF                                                               
052500     END-IF                                                               
052600     END-IF                                                               
052700     END-IF                                                               
052800     END-IF                                                               
052900     END-IF                                                               
053000     END-IF                                                               
053100     END-IF                                                               
053200     END-IF                                                               
053300     END-IF                                                               
053400     END-IF                                                               
053500     END-IF                                                               
053600     END-IF                                                               
053700     END-IF                                                               
053800     END-IF                                                               
053900     END-IF                                                               
054000     END-IF                                                               
054100     END-IF                                                               
054200     .                                                                    
054300     EJECT                                                                
054400 CA-ON-DATE SECTION.                                                      
054500     SKIP2                                                                
054600     MOVE ZERO TO SOP20-TIAPDAT                                           
054700     MOVE SPACE TO SOP20-TECATALOG                                        
054800     MOVE NEJ TO FEL-ORD                                                  
054900     MOVE NEJ TO ADD-SYMBOLER                                             
055000                                                                          
055100     PERFORM UNTIL ORD (NXT-ORD) = SPACE OR FEL-ORD = JA                  
055200       IF ORD (NXT-ORD) = 'ON'                                            
055300         ADD 1 TO NXT-ORD                                                 
055400         MOVE ORD (NXT-ORD) TO DATUM-X                                    
055500         IF DATUM-X NOT NUMERIC                                           
055600           DISPLAY 'SOP003E  ACTIVATION/PASSIVATION DATE ' DATUM-X        
055700                ' NOT NUMERIC.'                                           
055800           MOVE SPACE TO SOP20-KDSOPFUNK                                  
055900           IF RKOD < 12                                                   
056000             MOVE 12 TO RKOD                                              
056100           END-IF                                                         
056200           MOVE 12 TO SOP20-KDRET                                         
056300         ELSE                                                             
056400           MOVE DATUM-X TO SOP20-TIAPDAT                                  
056500         END-IF                                                           
056600                                                                          
056700       ELSE IF ORD (NXT-ORD) = 'CALENDAR'                                 
056800         ADD 1 TO NXT-ORD                                                 
056900         MOVE ORD (NXT-ORD) TO SOP20-TECATALOG                            
057000                                                                          
057100       ELSE IF ORD (NXT-ORD) = 'SYMBOLS'                                  
057200         MOVE JA TO ADD-SYMBOLER                                          
057300       ELSE                                                               
057400         MOVE JA TO FEL-ORD                                               
057500       END-IF                                                             
057600       END-IF                                                             
057700       END-IF                                                             
057800                                                                          
057900       ADD 1 TO NXT-ORD                                                   
058000     END-PERFORM                                                          
058100                                                                          
058200     IF ORD (NXT-ORD) NOT = SPACE                                         
058300       DISPLAY 'SOP157S  INVALID WORD: ' ORD (NXT-ORD)                    
058400       IF RKOD < 12                                                       
058500         MOVE 12 TO RKOD                                                  
058600       END-IF                                                             
058700       MOVE 12 TO SOP20-KDRET                                             
058800       ADD 1 TO NXT-ORD                                                   
058900     END-IF                                                               
059000     .                                                                    
059100     EJECT                                                                
059200 CB-AND-SUB-PROC  SECTION.                                                
059300     SKIP2                                                                
059400     MOVE NEJ TO PROCESS-SUBPROC                                          
059500     IF ORD (NXT-ORD) = 'AND'                                             
059600       ADD 1 TO NXT-ORD                                                   
059700       IF ORD (NXT-ORD) = 'DIRECT-SUB'                                    
059800         MOVE PROCESS-DIRECT TO PROCESS-SUBPROC                           
059900         ADD 1 TO NXT-ORD                                                 
060000       ELSE IF ORD (NXT-ORD) = 'ALL-SUB'                                  
060100         MOVE PROCESS-ALL TO PROCESS-SUBPROC                              
060200         ADD 1 TO NXT-ORD                                                 
060300       END-IF                                                             
060400       END-IF                                                             
060500     END-IF                                                               
060600                                                                          
060700     IF ORD (NXT-ORD) NOT = SPACE                                         
060800       DISPLAY 'SOP157S  INVALID WORD: ' ORD (NXT-ORD)                    
060900       IF RKOD < 12                                                       
061000         MOVE 12 TO RKOD                                                  
061100       END-IF                                                             
061200       MOVE 12 TO SOP20-KDRET                                             
061300       ADD 1 TO NXT-ORD                                                   
061400     END-IF                                                               
061500     .                                                                    
061600     EJECT                                                                
061700 CC-EXPAND-OPERANDER  SECTION.                                            
061800     SKIP2                                                                
061900     MOVE PROCESS-DIRECT TO PROCESS-SUBPROC                               
062000     MOVE '1' TO MIN-STATUS                                               
062100     MOVE NEJ TO FEL-ORD                                                  
062200     PERFORM UNTIL ORD (NXT-ORD) = SPACE OR FEL-ORD = JA                  
062300       IF ORD (NXT-ORD) = 'AND'                                           
062400         ADD 1 TO NXT-ORD                                                 
062500         IF ORD (NXT-ORD) = 'ALL-SUB'                                     
062600           MOVE PROCESS-ALL TO PROCESS-SUBPROC                            
062700         ELSE                                                             
062800           MOVE JA TO FEL-ORD                                             
062900         END-IF                                                           
063000       ELSE IF ORD (NXT-ORD) = 'MIN-STATUS'                               
063100         ADD 1 TO NXT-ORD                                                 
063200         IF ORD (NXT-ORD) = 'PASSIVE'                                     
063300            MOVE '0' TO MIN-STATUS                                        
063400         ELSE IF ORD (NXT-ORD) = 'ENDED'                                  
063500            MOVE '1' TO MIN-STATUS                                        
063600         ELSE IF ORD (NXT-ORD) = 'WAITING'                                
063700            MOVE '2' TO MIN-STATUS                                        
063800         ELSE IF ORD (NXT-ORD) = 'STARTED'                                
063900            MOVE '3' TO MIN-STATUS                                        
064000         ELSE                                                             
064100           MOVE JA TO FEL-ORD                                             
064200         END-IF                                                           
064300         END-IF                                                           
064400         END-IF                                                           
064500         END-IF                                                           
064600       ELSE                                                               
064700         MOVE JA TO FEL-ORD                                               
064800       END-IF                                                             
064900       END-IF                                                             
065000       ADD 1 TO NXT-ORD                                                   
065100     END-PERFORM                                                          
065200     IF ORD (NXT-ORD) NOT = SPACE                                         
065300       DISPLAY 'SOP157S  INVALID WORD: ' ORD (NXT-ORD)                    
065400       IF RKOD < 12                                                       
065500         MOVE 12 TO RKOD                                                  
065600       END-IF                                                             
065700       MOVE 12 TO SOP20-KDRET                                             
065800       ADD 1 TO NXT-ORD                                                   
065900     END-IF                                                               
066000     .                                                                    
066100     EJECT                                                                
066200 CD-KOLLA-PROCESS  SECTION.                                               
066300     SKIP2                                                                
066400     IF ORD (NXT-ORD) NOT = SPACE                                         
066500       IF LENGD (NXT-ORD) NOT > 10                                        
066600         MOVE ORD (NXT-ORD) TO SOP20-IDPROCESS                            
066700       ELSE                                                               
066800         DISPLAY 'SOP157S  INVALID WORD: ' ORD (NXT-ORD)                  
066900         IF RKOD < 12                                                     
067000           MOVE 12 TO RKOD                                                
067100         END-IF                                                           
067200         MOVE 12 TO SOP20-KDRET                                           
067300       END-IF                                                             
067400       ADD 1 TO NXT-ORD                                                   
067500     END-IF                                                               
067600     .                                                                    
067700     EJECT                                                                
067800 CE-KOLLA-SYMBOLS   SECTION.                                              
067900     SKIP2                                                                
068000     MOVE NEJ TO ADD-SYMBOLER                                             
068100     IF ORD (NXT-ORD) = 'SYMBOLS'                                         
068200       ADD 1 TO NXT-ORD                                                   
068300       MOVE JA TO ADD-SYMBOLER                                            
068400     END-IF                                                               
068500     .                                                                    
068600     EJECT                                                                
068700 D-CALL-W98020 SECTION.                                                   
068800     SKIP2                                                                
068900     MOVE NEJ TO SYMB-FEL                                                 
069000     IF ADD-SYMBOLER = JA AND (SOP20-KDSOPFUNK = 'A' OR 'O')              
069100       PERFORM DA-ADDERA-SYMBOLER                                         
069200     ELSE                                                                 
069300       MOVE SPACE TO SOP20-TESYMBV                                        
069400     END-IF                                                               
069500     IF ORD (NXT-ORD) NOT = SPACE                                         
069600       DISPLAY 'SOP157S  INVALID WORD: ' ORD (NXT-ORD)                    
069700       IF RKOD < 12                                                       
069800         MOVE 12 TO RKOD                                                  
069900       END-IF                                                             
070000       MOVE 12 TO SOP20-KDRET                                             
070100     ELSE IF SYMB-FEL = NEJ                                               
070200       CALL W9802000 USING SOP20-W98020                                   
070300       IF SOP20-KDRET > RKOD                                              
070400         MOVE SOP20-KDRET TO RKOD                                         
070500       END-IF                                                             
070600       IF SOP20-KDSOPFUNK = 'I' AND SOP20-KDRET = 0                       
070700         PERFORM DB-DISPLAY-INFO                                          
070800       END-IF                                                             
070900     END-IF                                                               
071000     END-IF                                                               
071100     .                                                                    
071200     EJECT                                                                
071300 DA-ADDERA-SYMBOLER     SECTION.                                          
071400     SKIP2                                                                
071500     IF PARM-KOMMANDO = NEJ AND EOF-KOMMANDOFIL = NEJ                     
071600       MOVE +1 TO RAD-IX                                                  
071700       PERFORM S0-LAES-KOMMANDOFIL                                        
071800       PERFORM UNTIL EOF-KOMMANDOFIL = JA OR                              
071900            END-TEMP-RAD = 'END-ORDER' OR 'END-ACTIVATE'                  
071920         DISPLAY TEMP-RAD                                                 
072000         STRING TEMP-RAD DELIMITED BY SIZE                                
072100             INTO SOP20-TESYMBV WITH POINTER RAD-IX                       
072200         PERFORM S0-LAES-KOMMANDOFIL                                      
072300       END-PERFORM                                                        
072310       IF EOF-KOMMANDOFIL = NEJ                                           
072320          DISPLAY TEMP-RAD                                                
072330       END-IF                                                             
072400       IF RAD-IX > 1000                                                   
072500         DISPLAY 'SOP161S - SYMBOL VALUE STRING LONGER THAN'              
072600                 ' 1000 CHARACTERS'                                       
072700         MOVE JA TO SYMB-FEL                                              
072800         MOVE +12 TO SOP20-KDRET                                          
072900       END-IF                                                             
073000     ELSE                                                                 
073100       DISPLAY 'SOP048E - MISSING SYMBOL VALUE STRING'                    
073200       IF SOP20-KDRET < 12                                                
073300         MOVE +12 TO SOP20-KDRET                                          
073400         MOVE +12 TO RKOD                                                 
073500       END-IF                                                             
073600       MOVE JA TO SYMB-FEL                                                
073700     END-IF                                                               
073800     .                                                                    
073900     EJECT                                                                
074000 DB-DISPLAY-INFO SECTION.                                                 
074100     SKIP2                                                                
074200     DISPLAY ' '                                                          
074300     IF SOP20-SYSTEM-PROCESS                                              
074400        MOVE 'SYSTEM'                    TO  DISPLAY-TYPE                 
074500     ELSE IF SOP20-ROUTINE-PROCESS                                        
074600        MOVE 'ROUTINE'                   TO  DISPLAY-TYPE                 
074700     ELSE IF SOP20-JOB-PROCESS                                            
074800        MOVE 'JOB'                       TO  DISPLAY-TYPE                 
074900     ELSE IF SOP20-PROCEDURE-PROCESS                                      
075000        MOVE 'PROCEDURE'                 TO  DISPLAY-TYPE                 
075100     ELSE                                                                 
075200        MOVE SPACE                       TO  DISPLAY-TYPE                 
075300     END-IF                                                               
075400     END-IF                                                               
075500     END-IF                                                               
075600     END-IF                                                               
075700     DISPLAY  '    PROCESS TYPE:          ' DISPLAY-TYPE                  
075800                                                                          
075900     IF SOP20-PASSIVE-STATUS                                              
076000       MOVE 'PASSIVE'                        TO DISPLAY-STATUS            
076100     ELSE IF SOP20-WAITING-STATUS                                         
076200       MOVE 'WAITING'                        TO DISPLAY-STATUS            
076300     ELSE IF SOP20-STARTED-STATUS                                         
076400       MOVE 'STARTED'                        TO DISPLAY-STATUS            
076500     ELSE IF SOP20-ENDED-STATUS                                           
076600       MOVE 'ENDED'                          TO DISPLAY-STATUS            
076700     END-IF                                                               
076800     END-IF                                                               
076900     END-IF                                                               
077000     END-IF                                                               
077100     DISPLAY  '    PROCESS STATUS:        ' DISPLAY-STATUS                
077200                                                                          
077300     IF SOP20-FLHOLD = JA                                                 
077400       DISPLAY  '                           (IT IS HELD)'                 
077500     END-IF                                                               
077600                                                                          
077700     IF SOP20-TEATTN NOT = SPACE                                          
077800       DISPLAY  '    ATTENTION MESSAGE:     ' SOP20-TEATTN                
077900     END-IF                                                               
078000                                                                          
078100     IF SOP20-TEPRED NOT = SPACE                                          
078200       DISPLAY  '    PREDECESSORS:          ' SOP20-TEPRED                
078300     END-IF                                                               
078400                                                                          
078500     IF SOP20-TESUCC NOT = SPACE                                          
078600       DISPLAY  '    SUCCESSORS:            ' SOP20-TESUCC                
078700     END-IF                                                               
078800                                                                          
078900     MOVE SOP20-TIAPDAT-SENAST TO DISPLAY-DATUM                           
079000     INSPECT DISPLAY-DATUM REPLACING ALL SPACE BY '-'                     
079100     DISPLAY  '    LAST ACTI/PASSI-VATED: '  DISPLAY-DATUM                
079200                                                                          
079300     MOVE SOP20-TIEXDAT-SENAST TO DISPLAY-DATUM                           
079400     INSPECT DISPLAY-DATUM REPLACING ALL SPACE BY '-'                     
079500     MOVE SOP20-TIMINUT-START TO DISPLAY-TID1                             
079600     INSPECT DISPLAY-TID1 REPLACING ALL SPACE BY ':'                      
079700     MOVE SOP20-TIMINUT-STOPP TO DISPLAY-TID2                             
079800     INSPECT DISPLAY-TID2 REPLACING ALL SPACE BY ':'                      
079900     MOVE SOP20-TIEXEC-SENAST TO TAL-DISPLAY                              
080000     DISPLAY  '    LAST EXECUTED ON:      '  DISPLAY-DATUM                
080100              ' AT '  DISPLAY-TID1 ' - ' DISPLAY-TID2                     
080200              '  (' TAL-X ' MINUTES)'                                     
080300                                                                          
080400     MOVE SOP20-TIEXEC-MEDEL TO TAL-DISPLAY                               
080500     DISPLAY  '    MEAN EXEC TIME:        ' TAL-X ' MINUTES'              
080600                                                                          
080700     IF SOP20-TEACTQ NOT = SPACE                                          
080800       DISPLAY  '    NEXT ACTIVATION ON:    '  SOP20-TEACTQ               
080900     END-IF                                                               
081000                                                                          
081100     IF SOP20-TEACTQ NOT = SPACE                                          
081200       DISPLAY  '    NEXT PASSIVATION ON:   '  SOP20-TEPASSQ              
081300     END-IF                                                               
081400                                                                          
081500     DISPLAY  '    START-TYPE:            ' SOP20-KDPROCSTRT              
081600                                                                          
081700     IF SOP20-TITMPDUR NOT = 0                                            
081800       DISPLAY  '    TEMP-JCL-DURATION:     '  SOP20-TITMPDUR             
081900     END-IF                                                               
082000                                                                          
082100     DISPLAY  '    CATALOG WORDS:         ' SOP20-TECATALOG               
082200                                                                          
082300     DISPLAY '    PARENT PROCESS:        ' SOP20-IDPROCESS-PARENT         
082400                                                                          
082500     IF SOP20-KDPROCPRIO NOT = SPACE                                      
082600       DISPLAY '    PRIORITY:              ' SOP20-KDPROCPRIO             
082700     END-IF                                                               
082800                                                                          
082900     IF SOP20-KDVOUT  = 'P'                                               
083000       DISPLAY  '    OUTPUT AT VOLVODATA:   PAPER'                        
083100     ELSE IF SOP20-KDVOUT = 'C'                                           
083200       DISPLAY  '    OUTPUT AT VOLVODATA:   COM'                          
083300     ELSE IF SOP20-KDVOUT = 'B'                                           
083400       DISPLAY  '    OUTPUT AT VOLVODATA:   COM AND PAPER'                
083500     END-IF                                                               
083600     END-IF                                                               
083700     END-IF                                                               
083800                                                                          
083900     IF SOP20-TESYMBV NOT = SPACE                                         
084000     DISPLAY ' '                                                          
084100       DISPLAY '    SYMBOL VALUES:         ' SOP20-TESYMBV                
084200     END-IF                                                               
084300                                                                          
084400     IF SOP20-TERES NOT = SPACE                                           
084500     DISPLAY ' '                                                          
084600       DISPLAY '    USES RESOURCES:        ' SOP20-TERES                  
084700     END-IF                                                               
084800                                                                          
084900     IF SOP20-ID-ABEND-JOB NOT = SPACE                                    
085000     DISPLAY ' '                                                          
085100       DISPLAY '    ABEND JOB:             ' SOP20-ID-ABEND-JOB           
085200     END-IF                                                               
085300     .                                                                    
085400     EJECT                                                                
085500 F-COPY-SOPREG SECTION.                                                   
085600     SKIP2                                                                
085700     CALL W980WSPC USING FCOPY WSRKOD WSPACE-DD-PARM                      
085800                   WSPACE-DD-PARM2 BLOCK-COUNT-PARM                       
085900     IF WSRKOD NOT = SPACE                                                
086000       DISPLAY 'SOP151E  COMMAND FAILED.'                                 
086100       MOVE 12 TO RKOD                                                    
086200     ELSE                                                                 
086300       DISPLAY 'SOP150I  COMMAND COMPLETED.'                              
086400     END-IF                                                               
086500     .                                                                    
086600     EJECT                                                                
086700 G-LIST-KALENDER  SECTION.                                                
086800     SKIP2                                                                
086900     MOVE ZERO TO SOP20-KDRET                                             
087000     MOVE 'L' TO SOP20-KDSOPFUNK                                          
087100     DISPLAY ' '                                                          
087200     CALL W980WSPC USING FGETF WSRKOD KALENDER-PARM                       
087300                   DATUM-LIST-PARM DATUM-PARM                             
087400     PERFORM UNTIL WSRKOD NOT = SPACE                                     
087500       MOVE DATUM-VAERDE TO DISPLAY-DATUM SOP20-TIAPDAT                   
087600       CALL W9802000 USING SOP20-W98020                                   
087700       DISPLAY '    ' DISPLAY-DATUM ':   ' SOP20-TECATALOG                
087800                                                                          
087900       CALL W980WSPC USING FGETN WSRKOD KALENDER-PARM                     
088000                   DATUM-LIST-PARM DATUM-PARM                             
088100     END-PERFORM                                                          
088200     CALL W980WSPC USING FQUIT WSRKOD                                     
088300     .                                                                    
088400     EJECT                                                                
088500 H-START-STATUS-VILLKOR    SECTION.                                       
088600     SKIP2                                                                
088700     IF L < L-MAX                                                         
088800       MOVE L TO L-SPAR                                                   
088900       ADD 1 TO L                                                         
089000       MOVE NEJ TO VILLKOR (L)                                            
089100     ELSE                                                                 
089200      DISPLAY 'SOP154S  TOO MANY LEVELS OF IF-STATEMENTS.'                
089300       IF RKOD < 12                                                       
089400         MOVE 12 TO RKOD                                                  
089500       END-IF                                                             
089600     END-IF                                                               
089700                                                                          
089800     IF VILLKOR (L-SPAR) = JA                                             
089900       MOVE SPACE TO ONSKAD-STATUS                                        
090000       IF ORD (3) = 'STARTED'                                             
090100         MOVE 'S' TO ONSKAD-STATUS                                        
090200       ELSE IF ORD (3) = 'ENDED'                                          
090300         MOVE 'E' TO ONSKAD-STATUS                                        
090400       ELSE IF ORD (3) = 'WAITING'                                        
090500         MOVE 'W' TO ONSKAD-STATUS                                        
090600       ELSE IF ORD (3) = 'PASSIVE'                                        
090700         MOVE 'P' TO ONSKAD-STATUS                                        
090800       ELSE                                                               
090900         DISPLAY 'SOP156S  ILLEGAL STATUS ' ORD (3)                       
091000         IF RKOD < 12                                                     
091100           MOVE 12 TO RKOD                                                
091200         END-IF                                                           
091300       END-IF                                                             
091400       END-IF                                                             
091500       END-IF                                                             
091600       END-IF                                                             
091700                                                                          
091800       IF ONSKAD-STATUS NOT = SPACE                                       
091900         MOVE ORD (2) TO NAMN-VARDE                                       
092000         ADD 2 LENGD (2) GIVING NAMN-LENGD                                
092100        CALL W980WSPC USING FGETF WSRKOD NAMN-PARM INFO-NAMN-PARM         
092200             INFO-VAERDE-PARM                                             
092300         IF WSRKOD = SPACE                                                
092400           IF KDPROCSTAT = ONSKAD-STATUS                                  
092500             MOVE JA TO VILLKOR (L)                                       
092600           END-IF                                                         
092700         ELSE                                                             
092800           DISPLAY 'SOP025E  PROCESS' SOP20-IDPROCESS                     
092900                      ' NOT FOUND.'                                       
093000           IF RKOD < 8                                                    
093100             MOVE 8 TO RKOD                                               
093200           END-IF                                                         
093300         END-IF                                                           
093400         CALL W980WSPC USING FQUIT WSRKOD                                 
093500       END-IF                                                             
093600       IF VILLKOR (L) = JA                                                
093700         DISPLAY 'SOP158I  CONDITION IS TRUE.'                            
093800       ELSE                                                               
093900         DISPLAY 'SOP158I  CONDITION IS FALSE.'                           
094000       END-IF                                                             
094100     ELSE                                                                 
094200       DISPLAY 'SOP153I  STATEMENT SKIPPED.'                              
094300     END-IF                                                               
094400     .                                                                    
094500     EJECT                                                                
094600 I-START-KALENDER-VILLKOR  SECTION.                                       
094700     SKIP2                                                                
094800     IF L < L-MAX                                                         
094900       MOVE L TO L-SPAR                                                   
095000       ADD 1 TO L                                                         
095100       MOVE NEJ TO VILLKOR (L)                                            
095200     ELSE                                                                 
095300      DISPLAY 'SOP154S  TOO MANY LEVELS OF IF-STATEMENTS.'                
095400       IF RKOD < 12                                                       
095500         MOVE 12 TO RKOD                                                  
095600       END-IF                                                             
095700     END-IF                                                               
095800                                                                          
095900     IF VILLKOR (L-SPAR) = JA                                             
096000       IF ORD (2) (1:4) = 'NOT-' OR 'NOT.'                                
096100         MOVE NOT-CATALOG-PARM TO ATTR-PARM                               
096200         SUBTRACT 2 FROM LENGD (2) GIVING DATA-LENGD                      
096300         MOVE ORD (2) (5:16) TO DATA-VARDE                                
096400         MOVE JA TO VILLKOR (L)                                           
096500       ELSE                                                               
096600         MOVE CATALOG-PARM TO ATTR-PARM                                   
096700         MOVE ORD (2) TO DATA-VARDE                                       
096800         ADD 2 LENGD (2) GIVING DATA-LENGD                                
096900         MOVE NEJ TO VILLKOR (L)                                          
097000       END-IF                                                             
097100                                                                          
097200       MOVE SPACE TO NAMN-PARM                                            
097300       CALL W980WSPC USING FGETF WSRKOD KALENDER-PARM                     
097400          NUDAT-PARM  NAMN-PARM                                           
097500                                                                          
097600       CALL W980WSPC USING FTEST WSRKOD NAMN-PARM ATTR-PARM               
097700             DATA-PARM                                                    
097800       IF WSRKOD = SPACE                                                  
097900         IF ATTR-PARM = CATALOG-PARM                                      
098000           MOVE JA TO VILLKOR (L)                                         
098100         ELSE                                                             
098200           MOVE NEJ TO VILLKOR (L)                                        
098300         END-IF                                                           
098400       ELSE                                                               
098500         IF ATTR-PARM = NOT-CATALOG-PARM                                  
098600           MOVE CATALOG-PARM TO ATTR-PARM                                 
098700                                                                          
098800           CALL W980WSPC USING FTEST WSRKOD NAMN-PARM ATTR-PARM           
098900                 DATA-PARM                                                
099000           IF WSRKOD = SPACE                                              
099100             MOVE NEJ TO VILLKOR (L)                                      
099200           ELSE                                                           
099300             MOVE JA TO VILLKOR (L)                                       
099400           END-IF                                                         
099500         END-IF                                                           
099600       END-IF                                                             
099700       CALL W980WSPC USING FQUIT WSRKOD                                   
099800       IF VILLKOR (L) = JA                                                
099900         DISPLAY 'SOP158I  CONDITION IS TRUE.'                            
100000       ELSE                                                               
100100         DISPLAY 'SOP158I  CONDITION IS FALSE.'                           
100200       END-IF                                                             
100300     ELSE                                                                 
100400       DISPLAY 'SOP153I  STATEMENT SKIPPED.'                              
100500     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800 IS-START-SYMBOL-VILLKOR   SECTION.                                       
100900     SKIP2                                                                
101000     IF L < L-MAX                                                         
101100       MOVE L TO L-SPAR                                                   
101200       ADD 1 TO L                                                         
101300       MOVE NEJ TO VILLKOR (L)                                            
101400     ELSE                                                                 
101500      DISPLAY 'SOP154S  TOO MANY LEVELS OF IF-STATEMENTS.'                
101600       IF RKOD < 12                                                       
101700         MOVE 12 TO RKOD                                                  
101800       END-IF                                                             
101900     END-IF                                                               
102000                                                                          
102100     IF VILLKOR (L-SPAR) = JA                                             
102200       IF ORD(2) NOT = SPACE AND ORD(3) NOT = SPACE                       
102300         MOVE +0 TO NAMN-LENGD                                            
102400         MOVE ORD (2) TO NAMN-VARDE                                       
102500         ADD 2 LENGD (2) GIVING NAMN-LENGD                                
102600         UNSTRING ORD(3) DELIMITED BY ')'                                 
102700            INTO ORD(5)                                                   
102800         MOVE +0 TO LENGD (3)                                             
102900         UNSTRING ORD(5) DELIMITED BY '('                                 
103000            INTO ORD (3) COUNT IN LENGD (3)                               
103100                 ORD (4)                                                  
103200         IF LENGD (3) = 0                                                 
103300           DISPLAY 'SOP159E MISSING PARAMETERS IN IF-SYMBOL'              
103400           IF RKOD < 8                                                    
103500             MOVE 8 TO RKOD                                               
103600           END-IF                                                         
103700           IF SOP20-KDRET < 8                                             
103800             MOVE 8 TO SOP20-KDRET                                        
103900           END-IF                                                         
104000         ELSE                                                             
104100           MOVE '&' TO ATTR-VARDE                                         
104200           MOVE +2 TO PTR                                                 
104300           STRING ORD (3) DELIMITED BY SPACE                              
104400               INTO ATTR-VARDE WITH POINTER PTR                           
104500           ADD 3 LENGD (3) GIVING ATTR-LENGD                              
104600           MOVE SPACE TO DATA-VARDE                                       
104700           CALL W980WSPC USING FGETF WSRKOD NAMN-PARM ATTR-PARM           
104800                   DATA-PARM                                              
104900           IF WSRKOD = SPACE                                              
105000             IF DATA-VARDE = ORD(4)                                       
105100               MOVE JA TO VILLKOR (L)                                     
105200             ELSE                                                         
105300               MOVE NEJ TO VILLKOR (L)                                    
105400             END-IF                                                       
105500           ELSE                                                           
105600             DISPLAY 'SOP160E  SYMBOL  ' ORD(3)                           
105700                        ' NOT FOUND.'                                     
105800             IF RKOD < 8                                                  
105900               MOVE 8 TO RKOD                                             
106000             END-IF                                                       
106100             IF SOP20-KDRET < 8                                           
106200               MOVE 8 TO SOP20-KDRET                                      
106300             END-IF                                                       
106400           END-IF                                                         
106500         END-IF                                                           
106600         CALL W980WSPC USING FQUIT WSRKOD                                 
106700         IF VILLKOR (L) = JA                                              
106800           DISPLAY 'SOP158I  CONDITION IS TRUE.'                          
106900         ELSE                                                             
107000           DISPLAY 'SOP158I  CONDITION IS FALSE.'                         
107100         END-IF                                                           
107200       ELSE                                                               
107300         DISPLAY 'SOP159E MISSING PARAMETERS IN IF-SYMBOL'                
107400       END-IF                                                             
107500     ELSE                                                                 
107600       DISPLAY 'SOP153I STATEMENT SKIPPED'                                
107700     END-IF                                                               
107800     .                                                                    
107900     EJECT                                                                
108000 J-SLUT-VILLKOR     SECTION.                                              
108100     SKIP2                                                                
108200     IF L > 1                                                             
108300       SUBTRACT 1 FROM L                                                  
108400     ELSE                                                                 
108500       DISPLAY 'SOP155S  UNMATCHED ENDIF.'                                
108600       IF RKOD < 12                                                       
108700        MOVE 12 TO RKOD                                                   
108800       END-IF                                                             
108900     END-IF                                                               
109000     .                                                                    
109100     EJECT                                                                
109200 K-DELETE-PROCESS SECTION.                                                
109300     SKIP2                                                                
109400     MOVE ZERO TO SOP20-KDRET                                             
109500     MOVE SOP20-IDPROCESS TO P-IDPROCESS                                  
109600     MOVE ZERO TO P-LENGD                                                 
109700     INSPECT P-IDPROCESS TALLYING P-LENGD                                 
109800     FOR CHARACTERS BEFORE INITIAL SPACE                                  
109900     ADD 2 TO P-LENGD                                                     
110000                                                                          
110100     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
110200           PARENT-PARM NAMN-PARM                                          
110300     IF WSRKOD = SPACE                                                    
110400       CALL W980WSPC USING FTEST WSRKOD NAMN-PARM                         
110500             CONTAINS-PARM P-PARM                                         
110600                                                                          
110700       IF WSRKOD = SPACE                                                  
110800     DISPLAY 'SOP162S  CANNOT DELETE ' P-IDPROCESS '. IT IS USED'         
110900             ' BY PROCESS ' NAMN-VARDE                                    
111000         IF RKOD < 12                                                     
111100          MOVE 12 TO RKOD SOP20-KDRET                                     
111200         END-IF                                                           
111300       ELSE                                                               
111400         PERFORM KA-DELETE-PROCESS                                        
111500       END-IF                                                             
111600                                                                          
111700     ELSE                                                                 
111800       PERFORM KA-DELETE-PROCESS                                          
111900     END-IF                                                               
112000     .                                                                    
112100     EJECT                                                                
112200 KA-DELETE-PROCESS SECTION.                                               
112300     SKIP2                                                                
112400     MOVE ZERO TO SIX                                                     
112500     PERFORM S5-PUSH-STACK                                                
112600     MOVE SPACE TO P-IDPROCESS                                            
112700     CALL W980WSPC USING FGETF WSRKOD                                     
112800            STACK-PROCESS-PARM (SIX)                                      
112900            CONTAINS-PARM P-PARM                                          
113000     PERFORM UNTIL SIX NOT > 0                                            
113100       IF PROCESS-SUBPROC NOT = NEJ                                       
113200         PERFORM UNTIL WSRKOD NOT = SPACE                                 
113300           IF PROCESS-SUBPROC = PROCESS-ALL                               
113400             PERFORM S5-PUSH-STACK                                        
113500             MOVE SPACE TO P-IDPROCESS                                    
113600             CALL W980WSPC USING FGETF WSRKOD                             
113700                STACK-PROCESS-PARM (SIX) CONTAINS-PARM P-PARM             
113800           ELSE                                                           
113900             PERFORM KAA-SUB-DELETE                                       
114000             MOVE SPACE TO P-IDPROCESS                                    
114100             CALL W980WSPC USING FGETN WSRKOD                             
114200                STACK-PROCESS-PARM (SIX) CONTAINS-PARM P-PARM             
114300           END-IF                                                         
114400         END-PERFORM                                                      
114500       END-IF                                                             
114600       MOVE STACK-PROCESS-PARM (SIX) TO P-PARM                            
114700       PERFORM KAA-SUB-DELETE                                             
114800       PERFORM S6-POP-STACK                                               
114900       IF SIX > 0                                                         
115000         MOVE SPACE TO P-IDPROCESS                                        
115100         CALL W980WSPC USING FGETN WSRKOD                                 
115200                STACK-PROCESS-PARM (SIX) CONTAINS-PARM P-PARM             
115300       END-IF                                                             
115400     END-PERFORM                                                          
115500     CALL W980WSPC USING FSAVE WSRKOD                                     
115600     IF SOP20-KDRET > RKOD                                                
115700       MOVE SOP20-KDRET TO RKOD                                           
115800     END-IF                                                               
115900     .                                                                    
116000     EJECT                                                                
116100 KAA-SUB-DELETE  SECTION.                                                 
116200     SKIP2                                                                
116300     CALL W980WSPC USING FGETF WSRKOD P-PARM                              
116400          INFO-NAMN-PARM DATA-PARM                                        
116500     IF WSRKOD NOT = SPACE                                                
116600       DISPLAY 'SOP025E  ' P-IDPROCESS ' NOT FOUND.'                      
116700       MOVE 8 TO SOP20-KDRET                                              
116800     ELSE                                                                 
116900       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
117000             INFO-NAMN-PARM                                               
117100                                                                          
117200*----- RENSA AFTER-BEROENDEN                                              
117300       CALL W980WSPC USING FGETF WSRKOD P-PARM                            
117400             AFTER-PARM DATA-PARM                                         
117500       IF WSRKOD = SPACE                                                  
117600         PERFORM UNTIL WSRKOD NOT = SPACE                                 
117700           CALL W980WSPC USING FDEL WSRKOD DATA-PARM                      
117800                 BEFORE-PARM P-PARM                                       
117900           CALL W980WSPC USING FGETN WSRKOD P-PARM                        
118000                 AFTER-PARM DATA-PARM                                     
118100         END-PERFORM                                                      
118200       END-IF                                                             
118300       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
118400             AFTER-PARM                                                   
118500                                                                          
118600*----- RENSA BEFORE-BEROENDEN                                             
118700       CALL W980WSPC USING FGETF WSRKOD P-PARM                            
118800             BEFORE-PARM DATA-PARM                                        
118900       IF WSRKOD = SPACE                                                  
119000         PERFORM UNTIL WSRKOD NOT = SPACE                                 
119100           CALL W980WSPC USING FDEL WSRKOD DATA-PARM                      
119200                 AFTER-PARM P-PARM                                        
119300           CALL W980WSPC USING FGETN WSRKOD P-PARM                        
119400                 BEFORE-PARM DATA-PARM                                    
119500         END-PERFORM                                                      
119600       END-IF                                                             
119700       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
119800             BEFORE-PARM                                                  
119900                                                                          
120000*----- RENSA EJ-SAMTIDIGT-BEROENDEN                                       
120100       CALL W980WSPC USING FGETF WSRKOD P-PARM                            
120200             EJ-SAMTIDIGT-PARM DATA-PARM                                  
120300       IF WSRKOD = SPACE                                                  
120400         PERFORM UNTIL WSRKOD NOT = SPACE                                 
120500           CALL W980WSPC USING FDEL WSRKOD DATA-PARM                      
120600                 EJ-SAMTIDIGT-PARM P-PARM                                 
120700           CALL W980WSPC USING FGETN WSRKOD P-PARM                        
120800                 EJ-SAMTIDIGT-PARM DATA-PARM                              
120900         END-PERFORM                                                      
121000       END-IF                                                             
121100       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
121200             EJ-SAMTIDIGT-PARM                                            
121300                                                                          
121400*----- RENSA RESURSER                                                     
121500       CALL W980WSPC USING FGETF WSRKOD P-PARM                            
121600             USING-PARM DATA-PARM                                         
121700       IF WSRKOD = SPACE                                                  
121800         PERFORM UNTIL WSRKOD NOT = SPACE                                 
121900           CALL W980WSPC USING FTEST WSRKOD DATA-PARM                     
122000                 RESQ-PARM P-PARM                                         
122100* --  RENSA RESURSKÖER                                                    
122200           IF WSRKOD = SPACE                                              
122300             CALL W980WSPC USING FDEL WSRKOD DATA-PARM                    
122400                   RESQ-PARM P-PARM                                       
122500             CALL W980WSPC USING FGETF WSRKOD DATA-PARM                   
122600                   RESQ-PARM TEMP-PARM                                    
122700             IF WSRKOD NOT = SPACE                                        
122800               CALL W980WSPC USING FDELK WSRKOD DATA-PARM                 
122900                     RESQ-PARM                                            
123000             END-IF                                                       
123100           END-IF                                                         
123200* --  RENSA RESURSOCKUPERING                                              
123300           CALL W980WSPC USING FTEST WSRKOD DATA-PARM                     
123400                 USED-PARM P-PARM                                         
123500           IF WSRKOD = SPACE                                              
123600             CALL W980WSPC USING FDEL WSRKOD DATA-PARM                    
123700                   USED-PARM P-PARM                                       
123800             CALL W980WSPC USING FGETF WSRKOD DATA-PARM                   
123900                   USED-PARM TEMP-PARM                                    
124000             IF WSRKOD NOT = SPACE                                        
124100               CALL W980WSPC USING FDELK WSRKOD DATA-PARM                 
124200                     USED-PARM                                            
124300             END-IF                                                       
124400           END-IF                                                         
124500           CALL W980WSPC USING FGETN WSRKOD P-PARM                        
124600                 USING-PARM DATA-PARM                                     
124700         END-PERFORM                                                      
124800       END-IF                                                             
124900       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
125000             USING-PARM                                                   
125100                                                                          
125200*----- RENSA CONTAINS-BEROENDEN                                           
125300       CALL W980WSPC USING FGETF WSRKOD P-PARM                            
125400             CONTAINS-PARM DATA-PARM                                      
125500       IF WSRKOD = SPACE                                                  
125600         PERFORM UNTIL WSRKOD NOT = SPACE                                 
125700           CALL W980WSPC USING FDEL WSRKOD DATA-PARM                      
125800                 PARENT-PARM P-PARM                                       
125900           CALL W980WSPC USING FGETN WSRKOD P-PARM                        
126000                 CONTAINS-PARM DATA-PARM                                  
126100         END-PERFORM                                                      
126200       END-IF                                                             
126300       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
126400             CONTAINS-PARM                                                
126500                                                                          
126600                                                                          
126700*----- RENSA AKTIVERINGS-KÖ                                               
126800       CALL W980WSPC USING FGETF WSRKOD P-PARM                            
126900             AKTQ-PARM     DATA-PARM                                      
127000       IF WSRKOD = SPACE                                                  
127100         PERFORM UNTIL WSRKOD NOT = SPACE                                 
127200           MOVE +6 TO DATA-LENGD                                          
127300           CALL W980WSPC USING FDEL WSRKOD DATA-PARM                      
127400                 AKTQ-PARM   P-PARM                                       
127500           CALL W980WSPC USING FGETN WSRKOD P-PARM                        
127600                 AKTQ-PARM     DATA-PARM                                  
127700         END-PERFORM                                                      
127800       END-IF                                                             
127900       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
128000             AKTQ-PARM                                                    
128100                                                                          
128200*----- RENSA PASSIVERINGS-KÖ                                              
128300       CALL W980WSPC USING FGETF WSRKOD P-PARM                            
128400             PASSQ-PARM    DATA-PARM                                      
128500       IF WSRKOD = SPACE                                                  
128600         PERFORM UNTIL WSRKOD NOT = SPACE                                 
128700           CALL W980WSPC USING FDEL WSRKOD DATA-PARM                      
128800                 PASSQ-PARM  P-PARM                                       
128900           CALL W980WSPC USING FGETN WSRKOD P-PARM                        
129000                 PASSQ-PARM    DATA-PARM                                  
129100         END-PERFORM                                                      
129200       END-IF                                                             
129300       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
129400             PASSQ-PARM                                                   
129500                                                                          
129600*----- RENSA SYMBOL-LISTOR                                                
129700       CALL W980WSPC USING FGETF WSRKOD P-PARM                            
129800             SYMB-PARM    DATA-PARM                                       
129900       IF WSRKOD = SPACE                                                  
130000         PERFORM UNTIL WSRKOD NOT = SPACE                                 
130100           CALL W980WSPC USING FDELK WSRKOD P-PARM                        
130200                 DATA-PARM                                                
130300           CALL W980WSPC USING FGETN WSRKOD P-PARM                        
130400                 SYMB-PARM    DATA-PARM                                   
130500         END-PERFORM                                                      
130600       END-IF                                                             
130700       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
130800             SYMB-PARM                                                    
130900                                                                          
131000*----- RENSA PARENT (MENINGSFULLT ENDAST FÖR HUVUDPROCESSEN)              
131100       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
131200               PARENT-PARM                                                
131300                                                                          
131400       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
131500               STARTQ-PARM                                                
131600                                                                          
131700       CALL W980WSPC USING FDEL  WSRKOD STARTED-LIST-PARM                 
131800               NULL-PARM P-PARM                                           
131900                                                                          
132000       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
132100               ATTN-PARM                                                  
132200                                                                          
132300       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
132400               HOLD-PARM                                                  
132500                                                                          
132600       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
132700               CATALOG-PARM                                               
132800                                                                          
132900       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
133000               NOT-CATALOG-PARM                                           
133100                                                                          
133200       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
133300               STARTTYP-PARM                                              
133400                                                                          
133500       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
133600               MTYP-PARM                                                  
133700                                                                          
133800       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
133900               VOUT-PARM                                                  
134000                                                                          
134100       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
134200               PRIO-PARM                                                  
134300                                                                          
134400       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
134500               FTID-PARM                                                  
134600                                                                          
134700       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
134800               ACTMSG-PARM                                                
134900                                                                          
135000       CALL W980WSPC USING FDELK WSRKOD P-PARM                            
135100               ABEND-JOB-PARM                                             
135200                                                                          
135300       DISPLAY '    ' P-IDPROCESS ' DELETED.'                             
135400     END-IF                                                               
135500     .                                                                    
135600     EJECT                                                                
135700 L-PLAN-PROCESS   SECTION.                                                
135800     SKIP2                                                                
135900     IF PLANFIL-OPEN = NEJ                                                
136000       OPEN OUTPUT PLANFIL                                                
136100       MOVE JA TO PLANFIL-OPEN                                            
136200     END-IF                                                               
136300                                                                          
136400     MOVE SOP20-IDPROCESS TO P-IDPROCESS                                  
136500     MOVE ZERO TO P-LENGD                                                 
136600     INSPECT P-IDPROCESS TALLYING P-LENGD                                 
136700             FOR CHARACTERS BEFORE INITIAL SPACE                          
136800     ADD 2 TO P-LENGD                                                     
136900                                                                          
137000     MOVE ZERO TO SIX                                                     
137100     PERFORM S5-PUSH-STACK                                                
137200     MOVE SPACE TO P-IDPROCESS                                            
137300     CALL W980WSPC USING FGETF WSRKOD                                     
137400            STACK-PROCESS-PARM (SIX)                                      
137500            CONTAINS-PARM P-PARM                                          
137600     PERFORM UNTIL SIX NOT > 0                                            
137700       PERFORM UNTIL WSRKOD NOT = SPACE                                   
137800         PERFORM LA-CALL-W98020                                           
137900         IF SOP20-KDPROCSTAT = 'A'                                        
138000           PERFORM LB-SKRIV-AKTIVERING                                    
138100           PERFORM S5-PUSH-STACK                                          
138200           MOVE SPACE TO P-IDPROCESS                                      
138300           CALL W980WSPC USING FGETF WSRKOD                               
138400                STACK-PROCESS-PARM (SIX) CONTAINS-PARM P-PARM             
138500         ELSE                                                             
138600           MOVE SPACE TO P-IDPROCESS                                      
138700           CALL W980WSPC USING FGETN WSRKOD                               
138800                STACK-PROCESS-PARM (SIX) CONTAINS-PARM P-PARM             
138900         END-IF                                                           
139000       END-PERFORM                                                        
139100       PERFORM S6-POP-STACK                                               
139200       IF SIX > 0                                                         
139300         MOVE SPACE TO P-IDPROCESS                                        
139400         CALL W980WSPC USING FGETN WSRKOD                                 
139500                STACK-PROCESS-PARM (SIX) CONTAINS-PARM P-PARM             
139600       END-IF                                                             
139700     END-PERFORM                                                          
139800     CALL W980WSPC USING FQUIT WSRKOD                                     
139900     .                                                                    
140000     EJECT                                                                
140100 LA-CALL-W98020  SECTION.                                                 
140200     SKIP2                                                                
140300     MOVE 'B' TO SOP20-KDSOPFUNK                                          
140400     MOVE P-IDPROCESS TO SOP20-IDPROCESS                                  
140500     CALL W9802000 USING SOP20-W98020                                     
140600     IF SOP20-KDRET > RKOD                                                
140700       MOVE SOP20-KDRET TO RKOD                                           
140800     END-IF                                                               
140900     .                                                                    
141000     EJECT                                                                
141100 LB-SKRIV-AKTIVERING   SECTION.                                           
141200     SKIP2                                                                
141300     MOVE SOP20-IDPROCESS     TO SOP-PROC-NAME                            
141400     MOVE SOP20-TIAPDAT       TO SOP-ACTPASS-DATE                         
141500     MOVE SOP20-KDPROCSTRT    TO SOP-START-TYPE                           
141600     MOVE SOP20-TECATALOG     TO SOP-CATALOG                              
141700     MOVE SOP20-TEPRED        TO SOP-PREDECESSORS                         
141800     MOVE SOP20-TESUCC        TO SOP-SUCCESSORS                           
141900     MOVE SOP20-TIAPDAT-SENAST TO SOP-LAST-ACTPASS-DATE                   
142000     MOVE SOP20-TIEXDAT-SENAST TO SOP-LAST-EXEC-DATE                      
142100     MOVE SOP20-TIMINUT-START  TO SOP-LAST-START-TIME                     
142200     MOVE SOP20-TIMINUT-STOPP  TO SOP-LAST-END-TIME                       
142300     MOVE SOP20-TIEXEC-SENAST  TO SOP-LAST-EXEC-TIME                      
142400     MOVE SOP20-TIEXEC-MEDEL   TO SOP-MEAN-EXEC-TIME                      
142500     MOVE SOP20-IDPROCESS-PARENT TO SOP-PARENT-PROC-NAME                  
142600     MOVE SOP20-KDPROCMTYP     TO SOP-PROC-TYPE                           
142700     MOVE SOP20-KDPROCPRIO     TO SOP-PROC-PRIO                           
142800     MOVE SOP20-KDVOUT         TO SOP-VD-OUTPUT                           
142900     MOVE ZERO                 TO SOP-OUT-TIME-LIMIT                      
143000                                                                          
143100     WRITE PLANPOST FROM SOP-RECORD                                       
143200     .                                                                    
143300     EJECT                                                                
143400 M-EXPAND-PROCESS SECTION.                                                
143500     SKIP2                                                                
143600     MOVE ZERO TO SOP20-KDRET                                             
143700     MOVE SOP20-IDPROCESS TO P-IDPROCESS                                  
143800     MOVE ZERO TO P-LENGD                                                 
143900     INSPECT P-IDPROCESS TALLYING P-LENGD                                 
144000         FOR CHARACTERS BEFORE INITIAL SPACE                              
144100     ADD 2 TO P-LENGD                                                     
144200     DISPLAY ' '                                                          
144300     PERFORM MA-SUB-EXPAND                                                
144400                                                                          
144500     IF STATUS-NR NOT < MIN-STATUS                                        
144600       MOVE ZERO TO SIX                                                   
144700       PERFORM S5-PUSH-STACK                                              
144800       MOVE SPACE TO P-IDPROCESS                                          
144900       CALL W980WSPC USING FGETF WSRKOD                                   
145000            STACK-PROCESS-PARM (SIX)                                      
145100            CONTAINS-PARM P-PARM                                          
145200       PERFORM UNTIL SIX NOT > 0                                          
145300         PERFORM UNTIL WSRKOD NOT = SPACE                                 
145400           PERFORM MA-SUB-EXPAND                                          
145500           IF PROCESS-SUBPROC = PROCESS-ALL                               
145600           AND STATUS-NR NOT < MIN-STATUS                                 
145700             PERFORM S5-PUSH-STACK                                        
145800             MOVE SPACE TO P-IDPROCESS                                    
145900             CALL W980WSPC USING FGETF WSRKOD                             
146000               STACK-PROCESS-PARM (SIX) CONTAINS-PARM P-PARM              
146100             IF WSRKOD = SPACE                                            
146200               DISPLAY ' '                                                
146300             END-IF                                                       
146400           ELSE                                                           
146500             MOVE SPACE TO P-IDPROCESS                                    
146600             CALL W980WSPC USING FGETN WSRKOD                             
146700               STACK-PROCESS-PARM (SIX) CONTAINS-PARM P-PARM              
146800             IF WSRKOD NOT = SPACE                                        
146900               DISPLAY ' '                                                
147000             END-IF                                                       
147100           END-IF                                                         
147200         END-PERFORM                                                      
147300         PERFORM S6-POP-STACK                                             
147400         IF SIX > 0                                                       
147500           MOVE SPACE TO P-IDPROCESS                                      
147600           CALL W980WSPC USING FGETN WSRKOD                               
147700                STACK-PROCESS-PARM (SIX) CONTAINS-PARM P-PARM             
147800           IF WSRKOD NOT = SPACE                                          
147900             DISPLAY ' '                                                  
148000           END-IF                                                         
148100         END-IF                                                           
148200       END-PERFORM                                                        
148300     ELSE                                                                 
148400       DISPLAY ' '                                                        
148500     END-IF                                                               
148600     CALL W980WSPC USING FQUIT WSRKOD                                     
148700     IF SOP20-KDRET > RKOD                                                
148800       MOVE SOP20-KDRET TO RKOD                                           
148900     END-IF                                                               
149000     .                                                                    
149100     EJECT                                                                
149200 MA-SUB-EXPAND  SECTION.                                                  
149300     SKIP2                                                                
149400     MOVE P-IDPROCESS TO SOP20-IDPROCESS                                  
149500     MOVE 'J' TO SOP20-KDSOPFUNK                                          
149600     CALL W9802000 USING SOP20-W98020                                     
149700                                                                          
149800     IF SOP20-PASSIVE-STATUS                                              
149900       MOVE '0' TO STATUS-NR                                              
150000     ELSE IF SOP20-ENDED-STATUS                                           
150100       MOVE '1' TO STATUS-NR                                              
150200     ELSE IF SOP20-WAITING-STATUS                                         
150300       MOVE '2' TO STATUS-NR                                              
150400     ELSE                                                                 
150500       MOVE '3' TO STATUS-NR                                              
150600     END-IF                                                               
150700     END-IF                                                               
150800     END-IF                                                               
150900                                                                          
151000     IF STATUS-NR NOT < MIN-STATUS                                        
151100       MOVE SPACE TO EXPAND-RAD                                           
151200                                                                          
151300       COMPUTE PTR = 4 * SIX + 1                                          
151400       STRING P-IDPROCESS DELIMITED BY SIZE                               
151500            INTO EXPAND-RAD WITH POINTER PTR                              
151600                                                                          
151700       IF SOP20-PASSIVE-STATUS                                            
151800         MOVE 'PASSIVE' TO EXPAND-STATUS                                  
151900       ELSE IF SOP20-WAITING-STATUS                                       
152000         MOVE 'WAITING' TO EXPAND-STATUS                                  
152100         IF SOP20-TEPRED   NOT = SPACE                                    
152200           STRING 'FOR ' SOP20-TEPRED   DELIMITED BY SIZE                 
152300            INTO EXPAND-NOTE                                              
152400         END-IF                                                           
152500       ELSE IF SOP20-STARTED-STATUS                                       
152600         MOVE 'STARTED' TO EXPAND-STATUS                                  
152700         MOVE SOP20-TIMINUT-START TO TID-ZONAT                            
152800         STRING 'AT ' TID-HH ':' TID-MM DELIMITED BY SIZE                 
152900                 INTO EXPAND-NOTE                                         
153000       ELSE IF SOP20-ENDED-STATUS                                         
153100         MOVE 'ENDED' TO EXPAND-STATUS                                    
153200         MOVE SOP20-TIMINUT-STOPP TO TID-ZONAT                            
153300         STRING 'AT ' TID-HH ':' TID-MM DELIMITED BY SIZE                 
153400                 INTO EXPAND-NOTE                                         
153500       END-IF                                                             
153600       END-IF                                                             
153700       END-IF                                                             
153800       END-IF                                                             
153900                                                                          
154000       MOVE 1 TO PTR                                                      
154100       IF SOP20-TITMPDUR NOT = 0                                          
154200         MOVE 'TMP-JCL.' TO EXPAND-ATTN                                   
154300         MOVE 10 TO PTR                                                   
154400       END-IF                                                             
154500       IF SOP20-FLHOLD = JA                                               
154600         STRING 'HELD.' DELIMITED BY SIZE                                 
154700            INTO EXPAND-ATTN WITH POINTER PTR                             
154800         ADD 6 TO PTR                                                     
154900       END-IF                                                             
155000       STRING SOP20-TEATTN DELIMITED BY SIZE                              
155100            INTO EXPAND-ATTN WITH POINTER PTR                             
155200                                                                          
155300       DISPLAY EXPAND-RAD                                                 
155400     END-IF                                                               
155500     .                                                                    
155600     EJECT                                                                
155700 N-SET-PARAMETER   SECTION.                                               
155800     SKIP2                                                                
155900     IF ORD (2) = 'DB-STATUS'                                             
156000       IF ORD (3) NOT = SPACE                                             
156100         MOVE ORD (3) TO DATA-VARDE                                       
156200         ADD 2 LENGD (3) GIVING DATA-LENGD                                
156300         CALL W980WSPC USING FSET WSRKOD                                  
156400           SOP-PARM DB-STATUS-PARM DATA-PARM                              
156500       ELSE                                                               
156600         CALL W980WSPC USING FDELK WSRKOD                                 
156700           SOP-PARM DB-STATUS-PARM                                        
156800       END-IF                                                             
156900       CALL W980WSPC USING FSAVE WSRKOD                                   
157000     ELSE IF ORD (2) = 'VALUE'                                            
157100       IF ORD (3) NOT = SPACE                                             
157200         MOVE ORD (3) TO NAMN-VARDE                                       
157300         ADD 2 LENGD (3) GIVING NAMN-LENGD                                
157400         CALL W980WSPC USING FGETF WSRKOD NAMN-PARM INFO-NAMN-PARM        
157500                       INFO-VAERDE-PARM                                   
157600         IF WSRKOD = SPACE                                                
157700           MOVE ORD(3) TO SOP20-IDPROCESS                                 
157800           IF PARM-KOMMANDO = NEJ AND EOF-KOMMANDOFIL = NEJ               
157900             MOVE +1 TO RAD-IX                                            
158000             PERFORM S0-LAES-KOMMANDOFIL                                  
158100             PERFORM UNTIL EOF-KOMMANDOFIL = JA OR                        
158200                     END-TEMP-RAD = 'END-SET'                             
158220               DISPLAY TEMP-RAD                                           
158300               STRING TEMP-RAD DELIMITED BY SIZE                          
158400                   INTO SOP20-TESYMBV WITH POINTER RAD-IX                 
158500               PERFORM S0-LAES-KOMMANDOFIL                                
158600             END-PERFORM                                                  
158700             IF RAD-IX > 1000                                             
158800               DISPLAY 'SOP161S - SYMBOL VALUE STRING LONGER THAN'        
158900                       ' 1000 CHARACTERS'                                 
159000             ELSE                                                         
159100               CALL W9802000 USING SOP20-W98020                           
159200             END-IF                                                       
159210             IF EOF-KOMMANDOFIL = NEJ                                     
159211               DISPLAY TEMP-RAD                                           
159220             END-IF                                                       
159300           ELSE                                                           
159400             DISPLAY 'SOP048E - MISSING SYMBOL VALUE STRING'              
159500             IF SOP20-KDRET < 8                                           
159600               MOVE +8 TO SOP20-KDRET                                     
159700             END-IF                                                       
159800           END-IF                                                         
159900         ELSE                                                             
160000           DISPLAY 'SOP025E - PROCESS ' ORD (3) ' NOT FOUND'              
160100           IF SOP20-KDRET < 8                                             
160200             MOVE +8 TO SOP20-KDRET                                       
160300           END-IF                                                         
160400         END-IF                                                           
160500       ELSE                                                               
160600         DISPLAY 'SOP001S - BLANK PROCESS NAME NOT ALLOWED'               
160700         IF SOP20-KDRET < 8                                               
160800           MOVE +8 TO SOP20-KDRET                                         
160900         END-IF                                                           
161000       END-IF                                                             
161100     ELSE                                                                 
161200       DISPLAY 'SOP157S  INVALID WORD: ' ORD (2)                          
161300       IF RKOD < 12                                                       
161400         MOVE 12 TO RKOD                                                  
161500       END-IF                                                             
161600       IF SOP20-KDRET < 8                                                 
161700         MOVE +8 TO SOP20-KDRET                                           
161800       END-IF                                                             
161900     END-IF                                                               
162000     END-IF                                                               
162100     .                                                                    
162200     EJECT                                                                
162300 X-OPEN-SOP  SECTION.                                                     
162400     SKIP2                                                                
162500     MOVE '0' TO SOP20-KDSOPFUNK                                          
162600     CALL W9802000 USING SOP20-W98020                                     
162700     .                                                                    
162800     SKIP3                                                                
162900 Y-CLOSE-SOP SECTION.                                                     
163000     SKIP2                                                                
163100     MOVE '9' TO SOP20-KDSOPFUNK                                          
163200     CALL W9802000 USING SOP20-W98020                                     
163300     .                                                                    
163400     SKIP3                                                                
163500 Z-FINIT     SECTION.                                                     
163600     SKIP2                                                                
163700     IF KOMMANDOFIL-OPEN = JA                                             
163800       CLOSE KOMMANDOFIL                                                  
163900     END-IF                                                               
164000     IF PLANFIL-OPEN = JA                                                 
164100       CLOSE PLANFIL                                                      
164200     END-IF                                                               
164300     .                                                                    
164400     EJECT                                                                
164500 S0-LAES-KOMMANDOFIL SECTION.                                             
164600     SKIP2                                                                
164700     READ KOMMANDOFIL INTO TEMP-RAD                                       
164800       AT END MOVE JA TO EOF-KOMMANDOFIL                                  
164900       NOT AT END                                                         
164910         IF TEMP-RAD (72:1) = SPACE AND TEMP-RAD (73:8) NUMERIC           
164920           MOVE SPACE TO TEMP-RAD (73:8)                                  
164930         END-IF                                                           
165000         MOVE 1 TO PTR                                                    
165100         INSPECT TEMP-RAD TALLYING PTR                                    
165200           FOR LEADING SPACE                                              
165300          UNSTRING TEMP-RAD                                               
165400             DELIMITED BY ALL SPACE                                       
165500             INTO END-TEMP-RAD                                            
165600             WITH POINTER PTR                                             
165700     END-READ                                                             
165800     .                                                                    
165900     SKIP3                                                                
166000 S1-LAES-KOMMANDOFIL SECTION.                                             
166100     SKIP2                                                                
166200     READ KOMMANDOFIL INTO KOMMANDO-RAD                                   
166300       AT END MOVE JA TO EOF-KOMMANDOFIL                                  
166400     END-READ                                                             
166500     IF EOF-KOMMANDOFIL = NEJ                                             
166600        PERFORM S2-UNSTRING-KOMMANDO                                      
166700     END-IF                                                               
166800     .                                                                    
166900     SKIP3                                                                
167000 S2-UNSTRING-KOMMANDO SECTION.                                            
167100     SKIP2                                                                
167200     MOVE 1 TO PTR                                                        
167300     INSPECT KOMMANDO-TEXT TALLYING PTR                                   
167400       FOR LEADING SPACE                                                  
167500     MOVE SPACE TO ORD-TAB                                                
167600     MOVE 2 TO NXT-ORD                                                    
167700                                                                          
167800     UNSTRING KOMMANDO-TEXT DELIMITED BY ALL SPACE                        
167900       INTO KOMMANDO-ORD                                                  
168000            ORD (2) COUNT IN LENGD (2)                                    
168100            ORD (3) COUNT IN LENGD (3)                                    
168200            ORD (4) COUNT IN LENGD (4)                                    
168300            ORD (5) COUNT IN LENGD (5)                                    
168400            ORD (6) COUNT IN LENGD (6)                                    
168500            ORD (7) COUNT IN LENGD (7)                                    
168600            ORD (8) COUNT IN LENGD (8)                                    
168700       WITH POINTER PTR                                                   
168800     .                                                                    
168900     EJECT                                                                
169000 S5-PUSH-STACK  SECTION.                                                  
169100     SKIP2                                                                
169200     ADD 1 TO SIX                                                         
169300     IF SIX > SIX-FULL                                                    
169400       DISPLAY 'SOP023S  TOO MANY LEVELS OF PROCESSES. '                  
169500                 '(MAX 10 ALLOWED)'                                       
169600       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
169700     ELSE                                                                 
169800       MOVE P-PARM TO STACK-PROCESS-PARM (SIX)                            
169900     END-IF                                                               
170000     .                                                                    
170100     EJECT                                                                
170200 S6-POP-STACK  SECTION.                                                   
170300     SKIP2                                                                
170400     SUBTRACT 1 FROM SIX                                                  
170500     IF SIX < 0                                                           
170600       DISPLAY  'SOP026S  SEVERE ERROR - 22 SECTION S6.'                  
170700       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
170800     END-IF                                                               
170900     IF SIX > 0                                                           
171000       MOVE STACK-PROCESS-PARM (SIX) TO P-PARM                            
171100     END-IF                                                               
171200     .                                                                    
171300     EJECT                                                                
