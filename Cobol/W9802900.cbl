000100*COMPOPT AMODE=ANY ISPFPGM=YES                                            
000200 ID  DIVISION.                                                            
000300     SKIP2                                                                
000400 PROGRAM-ID.    W9802900.                                                 
000500 AUTHOR.       KARIN OLSSON.                                              
000600     DATE-WRITTEN.  SEP 1993.                                             
000700                                                                          
000800     REMARKS.                                                             
000900     SKIP2                                                                
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        EXAMINE ALL STARTED JOBS IN SOP AND CHECK FOR                    
001300*        POTENTIALLY UNHANDLED PROBLEMS.                                  
001310*        IF SO, INITIATE MESSAGE AND VINST CASE.                          
001320*                                                                         
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000                                                                          
002100*  ---  FIL MED MEDDELANDE TILL JOUR-ANSVARIG                             
002200     SELECT MSGFILE                    ASSIGN TO W98029D1.                
002300                                                                          
002400*  ---  FIL MED DATA FÖR ATT GENERA ETT PROBLEM CASE                      
002500     SELECT CASEFILE                   ASSIGN TO W98029D2.                
002600     EJECT                                                                
002700 DATA DIVISION.                                                           
002800     SKIP3                                                                
002900 FILE SECTION.                                                            
003000     SKIP3                                                                
003100 FD  MSGFILE                                                              
003200     RECORDING       F                                                    
003300     BLOCK CONTAINS  0.                                                   
003400     SKIP2                                                                
003500 01  MSG-POST        PIC X(80).                                           
003600                                                                          
003700     SKIP3                                                                
003800 FD  CASEFILE                                                             
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100     SKIP2                                                                
004200 01  CASE-POST       PIC X(80).                                           
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500*    -COPY WY2000W1                                                       
004600     SKIP3                                                                
004700*                                                                         
004800 77  PROGRAM-NAMN                PIC X(8)   VALUE 'W9802900'.             
004900     SKIP2                                                                
005000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005200 77  YES                         PIC X       VALUE 'Y'.                   
005300 77  JAA                         PIC X       VALUE 'J'.                   
005400 77  NOO                         PIC X       VALUE 'N'.                   
005500 77  PASSIVE-STATUS              PIC X       VALUE 'P'.                   
005600 77  WAITING-STATUS              PIC X       VALUE 'W'.                   
005700 77  STARTED-STATUS              PIC X       VALUE 'S'.                   
005800 77  ENDED-STATUS                PIC X       VALUE 'E'.                   
005900                                                                          
006000 01  SOP-OPEN                    PIC X       VALUE 'N'.                   
006100     EJECT                                                                
006200 01  TID-ZONAT                   PIC 9(4).                                
006300 01  FILLER REDEFINES TID-ZONAT.                                          
006400     03  TID-HH                  PIC XX.                                  
006500     03  TID-MM                  PIC XX.                                  
006600                                                                          
006700 01  JOBB-NAMN.                                                           
006800     03  JOBB-NAMN-1-4           PIC X(4).                                
006900     03  JOBB-NAMN-5             PIC X.                                   
007000     03  JOBB-NAMN-6-8           PIC X(3).                                
007100                                                                          
007200     EJECT                                                                
007300 01  DYNAMIC-SUBPROGRAMS.                                                 
007400     03  ABEND                   PIC X(8)    VALUE 'ABEND  '.             
007500     03  ISPLINK                 PIC X(8)    VALUE 'ISPLINK'.             
007600     03  W980WSPC                PIC X(8)    VALUE 'W980WSPC'.            
007700     03  W9802000                PIC X(8)    VALUE 'W9802000'.            
007800     03  W009WAIT                PIC X(8)    VALUE 'W009WAIT'.            
007900     EJECT                                                                
008000 01  VAR-LIST.                                                            
008100     03  SOPJOBST                PIC X.                                   
008200     03 N-SOPJOBST               PIC X(8)    VALUE 'SOPJOBST'.            
008300     03 L-SOPJOBST               PIC S9(9) COMP  VALUE +1.                
008400     EJECT                                                                
008500 01  ISP-SELECT                  PIC X(8)    VALUE 'SELECT  '.            
008600 01  VDEFINE                     PIC X(8)    VALUE 'VDEFINE '.            
008700 01  VGET                        PIC X(8)    VALUE 'VGET    '.            
008800 01  VPUT                        PIC X(8)    VALUE 'VPUT    '.            
008900                                                                          
009000 01  CHAR                        PIC X(8)    VALUE 'CHAR    '.            
009100 01  FIXED                       PIC X(8)    VALUE 'FIXED   '.            
009200 01  VDEFINE-OPT                 PIC X(16)                                
009300                             VALUE '(COPY NOBSCAN)'.                      
009400 01  NOPARM                      PIC X       VALUE SPACE.                 
009500 01  SHARED                      PIC X(8)    VALUE 'SHARED  '.            
009600 01  PROFILE                     PIC X(8)    VALUE 'PROFILE '.            
009700                                                                          
009800 01  L-STATUS-CMD                PIC S9(9) COMP  VALUE +24.               
009900 01  STATUS-CMD.                                                          
010000     03  FILLER                  PIC X(14)                                
010100         VALUE 'CMD(%SOPJOBST '.                                          
010200     03  STATUS-JOBNAMN.                                                  
010300         05  FILLER              PIC X(4).                                
010400         05  STATUS-JOBNAMN-POS5 PIC X.                                   
010500         05  FILLER              PIC X(3).                                
010600     03  FILLER                  PIC XX  VALUE ' )'.                      
010700                                                                          
010800     EJECT                                                                
010900*01  -COPY W98020                                                         
011000     EJECT                                                                
011100*---------------------- PARAMETRAR VID ANROP AV W980WSPC                  
011200                                                                          
011300 01  FUNKTIONSKODER.                                                      
011400     03  FGETF                   PIC X(4)    VALUE 'GETF'.                
011500     03  FGETN                   PIC X(4)    VALUE 'GETN'.                
011600     03  FSAVE                   PIC X(4)    VALUE 'SAVE'.                
011700     03  FQUIT                   PIC X(4)    VALUE 'QUIT'.                
011800     03  FTEST                   PIC X(4)    VALUE 'TEST'.                
011900     03  FDELK                   PIC X(4)    VALUE 'DELK'.                
012000     03  FSET                    PIC X(4)    VALUE 'SET '.                
012100                                                                          
012200 01  WSRKOD                  PIC X.                                       
012300                                                                          
012400*---------------------  GENERELLA ARBETS-PARAMETRAR                       
012500 01  NAMN-PARM.                                                           
012600     03  NAMN-LENGD              PIC S9(4)  COMP.                         
012700     03  NAMN-VAERDE             PIC X(10).                               
012800                                                                          
012900 01  ATTR-PARM.                                                           
013000     03  ATTR-LENGD              PIC S9(4)  COMP.                         
013100     03  ATTR-VAERDE             PIC X(10).                               
013200                                                                          
013300 01  DATA-PARM.                                                           
013400     03  DATA-LENGD              PIC S9(4)  COMP.                         
013500     03  DATA-VAERDE             PIC X(20).                               
013600                                                                          
013700     EJECT                                                                
013800 01  INFO-VAERDE-PARM.                                                    
013900*    03   -COPY W980INF                                                   
014000     EJECT                                                                
014100*---------------------  FÄLT FÖR HANTERING AV DATUM-DATA                  
014200 01  DATUM-ZONAT                 PIC 9(6).                                
014300 01  DATUM-PARM.                                                          
014400     03  FILLER                  PIC S9(4)  COMP  VALUE +6.               
014500     03  DATUM-PACKAT            PIC S9(7)  COMP-3.                       
014600                                                                          
014700*---------------------  FÄLT FÖR HANTERING AV DATUM-DATA                  
014800 01  NUDAT-PARM.                                                          
014900     03  FILLER                  PIC S9(4)  COMP  VALUE +6.               
015000     03  NUDAT                   PIC S9(7)  COMP-3.                       
015100                                                                          
015200*---------------------  STATUS PÅ DEN AKTUELLA SOPBASEN                   
015300 01  DB-STATUS-VAERDE-PARM.                                               
015400     03  FILLER                  PIC S9(4)  COMP  VALUE +6.               
015500     03  DB-STATUS               PIC X(4).                                
015600     EJECT                                                                
015700*---------------------  PROCESS SOM BEARBETAS FÖR TILLFÄLLET.             
015800 01  P-PARM.                                                              
015900     03  P-LENGD                 PIC S9(4)  COMP.                         
016000     03  P-IDPROCESS             PIC X(10).                               
016100                                                                          
016200     EJECT                                                                
016300 01  CONTAINS-PARM.                                                       
016400     03  FILLER                  PIC S9(4) COMP VALUE +5.                 
016500     03  FILLER                  PIC X(3)  VALUE 'CON'.                   
016600                                                                          
016700 01  PARENT-PARM.                                                         
016800     03  FILLER                  PIC S9(4) COMP VALUE +5.                 
016900     03  FILLER                  PIC X(3)  VALUE 'PAR'.                   
017000                                                                          
017100 01  STARTED-LIST-PARM.                                                   
017200     03  FILLER                  PIC S9(4) COMP VALUE +4.                 
017300     03  FILLER                  PIC X(2)       VALUE 'SL'.               
017400                                                                          
017500 01  NULL-PARM.                                                           
017600     03  FILLER                  PIC S9(4) COMP VALUE +2.                 
017700                                                                          
017800 01  NUDAT-NAMN-PARM.                                                     
017900     03  FILLER                  PIC S9(4) COMP VALUE +6.                 
018000     03  FILLER                  PIC X(4)       VALUE 'CDAT'.             
018100                                                                          
018200 01  DATUM-LIST-PARM.                                                     
018300     03  FILLER                  PIC S9(4) COMP VALUE +6.                 
018400     03  FILLER                  PIC X(4)       VALUE 'DATL'.             
018500                                                                          
018600 01  STARTTYP-PARM.                                                       
018700     03  FILLER                  PIC S9(4) COMP VALUE +6.                 
018800     03  FILLER                  PIC X(4)       VALUE 'STYP'.             
018900                                                                          
019000 01  TEMP-VARAKTIGHET-PARM.                                               
019100     03  FILLER                  PIC S9(4) COMP VALUE +6.                 
019200     03  FILLER                  PIC X(4)       VALUE 'TEMP'.             
019300                                                                          
019400 01  INFO-NAMN-PARM.                                                      
019500     03  FILLER                  PIC S9(4) COMP VALUE +5.                 
019600     03  FILLER                  PIC X(3)       VALUE 'INF'.              
019700                                                                          
019800 01  MTYP-PARM.                                                           
019900     03  FILLER                  PIC S9(4) COMP VALUE +6.                 
020000     03  FILLER                  PIC X(4)       VALUE 'MTYP'.             
020100                                                                          
020200 01  DB-STATUS-PARM.                                                      
020300     03  FILLER                  PIC S9(4) COMP VALUE +8.                 
020400     03  FILLER                  PIC X(6)       VALUE 'DBSTAT'.           
020500                                                                          
020600 01  SOP-PARM.                                                            
020700     03  FILLER                  PIC S9(4) COMP VALUE +7.                 
020800     03  FILLER                  PIC X(5)       VALUE '*SOP*'.            
020900                                                                          
021000 01  PRIO-PARM.                                                           
021100     03  FILLER                  PIC S9(4) COMP VALUE +6.                 
021200     03  FILLER                  PIC X(5)       VALUE 'PRIO'.             
021300                                                                          
021400     EJECT                                                                
021500 01  MSG-AREA.                                                            
021510     03  FILLER                  PIC X          VALUE SPACE.              
021600     03  MSG-DATE                PIC 9(6).                                
021610     03  FILLER                  PIC X          VALUE SPACE.              
021620     03  MSG-HH                  PIC 99.                                  
021630     03  FILLER                  PIC X          VALUE ':'.                
021640     03  MSG-MM                  PIC 99.                                  
021650     03  FILLER                  PIC X          VALUE SPACE.              
021700     03  MSG-PARENT              PIC X(10)      VALUE SPACE.              
021800     03  MSG-PROCESS             PIC X(16)      VALUE SPACE.              
021900     03  MSG-MEDD                PIC X(40)      VALUE SPACE.              
022000     SKIP3                                                                
022100 01  MAX-TAB                     PIC S9(4) VALUE 500.                     
022200 01  ANTAL                       PIC S9(4).                               
022300     EJECT                                                                
022400 01  KLOCKSLAG.                                                           
022500     03  HHMM                    PIC 9(4).                                
022600     03  FILLER REDEFINES HHMM.                                           
022700       05   KLOCKSLAG-HH         PIC XX.                                  
022800       05   KLOCKSLAG-MM         PIC XX.                                  
022900     03   FILLER                 PIC X(4).                                
023000                                                                          
023100 01  TID1                        PIC S9(7)  COMP-3.                       
023200 01  LONG-TIEXEC                 PIC S9(7)  COMP-3.                       
023300 01  TIEXEC                      PIC S9(7)  COMP-3.                       
023400 01  TIMMAR                      PIC S9(3)  COMP-3.                       
023500 01  MINUTER                     PIC S9(3)  COMP-3.                       
023600                                                                          
023700 01  WDATUM                      PIC S9(7)  COMP-3.                       
023800                                                                          
023900 01  WCDATE                      PIC X(12).                               
024000 01  WTIMESTAMP.                                                          
024100     03  WTS-CCYY                PIC X(4).                                
024200     03  FILLER                  PIC X     VALUE '-'.                     
024300     03  WTS-MM                  PIC X(2).                                
024400     03  FILLER                  PIC X     VALUE '-'.                     
024500     03  WTS-DD                  PIC X(2).                                
024600     03  FILLER                  PIC X     VALUE SPACE.                   
024700     03  WTS-TIM                 PIC X(2).                                
024800     03  FILLER                  PIC X     VALUE ':'.                     
024900     03  WTS-MIN                 PIC X(2).                                
025000                                                                          
025100 01  WPRIO                       PIC X.                                   
025200                                                                          
025300                                                                          
025400 01  5-SEKUNDER                  PIC S9(9)  BINARY VALUE 500.             
025500                                                                          
025600     EJECT                                                                
025700 PROCEDURE DIVISION.                                                      
025800 MAIN SECTION.                                                            
025900     PERFORM A-INIT                                                       
026000     PERFORM X-OPEN-SOPREG                                                
026100     PERFORM C-EXAMINE-STARTED-PROCESSES                                  
026200     PERFORM Z-FINIT                                                      
026300                                                                          
026400     MOVE ZERO TO RETURN-CODE                                             
026500     GOBACK                                                               
026600     .                                                                    
026700     EJECT                                                                
026800 A-INIT SECTION.                                                          
026900     SKIP2                                                                
027000     MOVE FUNCTION CURRENT-DATE(1:12) TO WCDATE                           
027100     MOVE WCDATE(1:4)  TO WTS-CCYY                                        
027200     MOVE WCDATE(5:2)  TO WTS-MM                                          
027300     MOVE WCDATE(7:2)  TO WTS-DD                                          
027310     MOVE WCDATE(3:6)  TO                        MSG-DATE                 
027400     MOVE WCDATE(9:2)  TO WTS-TIM KLOCKSLAG-HH   MSG-HH                   
027500     MOVE WCDATE(11:2) TO WTS-MIN KLOCKSLAG-MM   MSG-MM                   
027510                                                                          
027600                                                                          
027700     MOVE SPACE TO SOP20-IDDDPREFIX                                       
027800     IF RETURN-CODE NOT > 8                                               
027900       MOVE SPACE TO SOPJOBST                                             
028000       CALL  ISPLINK USING VDEFINE N-SOPJOBST SOPJOBST CHAR               
028100                           L-SOPJOBST VDEFINE-OPT                         
028200     END-IF                                                               
028300     OPEN OUTPUT MSGFILE CASEFILE                                         
028400     .                                                                    
028500     EJECT                                                                
028600 X-OPEN-SOPREG  SECTION.                                                  
028700     SKIP2                                                                
028800     MOVE '0'   TO SOP20-KDSOPFUNK                                        
028900     CALL W9802000 USING SOP20-W98020                                     
029000     IF SOP20-KDRET > 4                                                   
029100       DISPLAY 'ERROR FRÅN ÖPPNA SOP'                                     
029200       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
029300     END-IF                                                               
029400     MOVE SPACE TO DB-STATUS                                              
029500     CALL W980WSPC USING FGETF WSRKOD SOP-PARM                            
029600                   DB-STATUS-PARM DB-STATUS-VAERDE-PARM                   
029700     CALL W980WSPC USING FQUIT WSRKOD                                     
029800     .                                                                    
029900     EJECT                                                                
030000 C-EXAMINE-STARTED-PROCESSES SECTION.                                     
030100     SKIP2                                                                
030200     MOVE SPACE TO NAMN-VAERDE                                            
030300     CALL W980WSPC USING FGETF WSRKOD STARTED-LIST-PARM                   
030400                   NULL-PARM NAMN-PARM                                    
030500     IF WSRKOD = SPACE                                                    
030600       DISPLAY  ' '                                                       
030700       PERFORM UNTIL WSRKOD NOT = SPACE                                   
030800         PERFORM CA-GET-PROCESS-INFO                                      
030900         MOVE SPACE TO NAMN-VAERDE                                        
031000         CALL W980WSPC USING FGETN WSRKOD STARTED-LIST-PARM               
031100                       NULL-PARM NAMN-PARM                                
031200       END-PERFORM                                                        
031300     ELSE                                                                 
031400       DISPLAY  ' '                                                       
031500       DISPLAY 'NO STARTED PROCESSES'                                     
031600       DISPLAY  ' '                                                       
031700     END-IF                                                               
031800     CALL W980WSPC USING FSAVE WSRKOD                                     
031900     .                                                                    
032000     EJECT                                                                
032100 CA-GET-PROCESS-INFO      SECTION.                                        
032200     SKIP2                                                                
032300     PERFORM CAB-FETCH-PARENT-INFO                                        
032400                                                                          
032500     MOVE 'J' TO SOP20-KDSOPFUNK                                          
032600     MOVE NAMN-VAERDE TO SOP20-IDPROCESS                                  
032700     MOVE ZERO TO SOP20-TIAPDAT                                           
032800     CALL W9802000 USING SOP20-W98020                                     
032900                                                                          
033000*    -------- WAITING (HELD / READY TO START)                             
033100     IF SOP20-KDPROCSTAT = WAITING-STATUS                                 
033200       MOVE NAMN-VAERDE TO MSG-PROCESS                                    
033300       IF SOP20-FLHOLD = JAA                                              
033400         MOVE 'HELD WAITING READY'  TO MSG-MEDD                           
033500       ELSE                                                               
033600         MOVE SOP20-TEATTN TO MSG-MEDD                                    
033700       END-IF                                                             
033800       DISPLAY MSG-AREA                                                   
033900       PERFORM CAA-WRITE-MINICALL                                         
034000       PERFORM CAD-WRITE-PROBLEM-CASE                                     
034100     END-IF                                                               
034200                                                                          
034300*    -------- KOLLA JOB ELLER RUTINER MED START-TYPE SUB                  
034400                                                                          
034500     IF ((SOP20-ROUTINE-PROCESS AND SOP20-KDPROCSTRT = 'SUB')             
034600      OR SOP20-JOB-PROCESS)                                               
034700      AND SOP20-KDPROCSTAT = STARTED-STATUS                               
034800                                                                          
034900       MOVE NAMN-VAERDE TO STATUS-JOBNAMN                                 
035000       IF DB-STATUS = 'TEST'                                              
035100         MOVE 'T' TO STATUS-JOBNAMN-POS5                                  
035200       END-IF                                                             
035300       CALL ISPLINK USING ISP-SELECT L-STATUS-CMD STATUS-CMD              
035400       CALL ISPLINK USING VGET N-SOPJOBST SHARED                          
035500                                                                          
035600       EVALUATE TRUE                                                      
035700                                                                          
035800*    -------- JOBB SOM LAGTS I HOLD AV TM ELLER SÅ                        
035900         WHEN SOPJOBST = 'H'                                              
036000*          -- KOLLA JOBB-STATUS EN GÅNG TILL FÖR SÄKERHETS                
036100*          -- SKULL (DET KAN HA VARIT PÅ VÄG IN I SYSPLEX)                
036200           CALL W009WAIT USING 5-SEKUNDER                                 
036300           CALL ISPLINK USING ISP-SELECT L-STATUS-CMD                     
036400                              STATUS-CMD                                  
036500           CALL ISPLINK USING VGET N-SOPJOBST SHARED                      
036600           IF SOPJOBST = 'H'                                              
036700             MOVE NAMN-VAERDE TO MSG-PROCESS                              
036800             MOVE 'HELD ON INPUT QUEUE' TO MSG-MEDD                       
036900             DISPLAY MSG-AREA                                             
037000             PERFORM CAA-WRITE-MINICALL                                   
037100             PERFORM CAD-WRITE-PROBLEM-CASE                               
037200           END-IF                                                         
037300                                                                          
037400*    -------- JOBB SOM GÅR OCH HAR LONG EXEC-TIME                         
037500*             SIGNALERA OM DETTA VARAT ALLTFÖR LÄNGE                      
037600         WHEN SOPJOBST = 'X' OR 'I'                                       
037700           IF SOP20-TEATTN NOT = SPACE                                    
037800             MOVE +0 TO ANTAL                                             
037900             INSPECT SOP20-TEATTN TALLYING ANTAL FOR                      
038000                     ALL 'LONG EXEC TIME'                                 
038100             IF ANTAL > 0                                                 
038200               PERFORM CAC-COMPUTE-EXEC-TIME                              
038300               IF TIEXEC > SOP20-TIEXEC-MEDEL * 1.5 + 60                  
038400                 MOVE NAMN-VAERDE TO MSG-PROCESS                          
038500                 MOVE 'LONG EXEC TIME' TO MSG-MEDD                        
038600                 DISPLAY MSG-AREA                                         
038700                 PERFORM CAA-WRITE-MINICALL                               
038800                 PERFORM CAD-WRITE-PROBLEM-CASE                           
038900               END-IF                                                     
039000             END-IF                                                       
039100           END-IF                                                         
039200                                                                          
039300         WHEN SOPJOBST = '?' OR 'O'                                       
039400           IF SOP20-TEATTN NOT = SPACE                                    
039500             MOVE +0 TO ANTAL                                             
039600             INSPECT SOP20-TEATTN TALLYING ANTAL FOR                      
039700                     ALL 'LONG EXEC TIME'                                 
039800             IF ANTAL > 0                                                 
039900                                                                          
040000*    --------- JOBB SOM INTE GÅR I MASKINEN, MED LONG EXEC-TIME           
040100                                                                          
040200               IF SOP20-JOB-PROCESS                                       
040300                 PERFORM CAB-FETCH-PARENT-INFO                            
040400                 IF KDPROCSTAT = STARTED-STATUS OR LOW-VALUE              
040500                   MOVE NAMN-VAERDE TO MSG-PROCESS                        
040600                   MOVE 'NO JOB JCL ERROR? + LONG EXEC TIME'              
040700                        TO MSG-MEDD                                       
040800                   DISPLAY MSG-AREA                                       
040900                   PERFORM CAA-WRITE-MINICALL                             
041000                   PERFORM CAD-WRITE-PROBLEM-CASE                         
041100                 END-IF                                                   
041200               ELSE                                                       
041300                                                                          
041400*    ----------- RUTINER (FILEMONJOBB) MED LONG EXEC-TIME                 
041500*                SOM VARAT ALLTFÖR LÄNGE                                  
041600                                                                          
041700                 PERFORM CAC-COMPUTE-EXEC-TIME                            
041800                 IF TIEXEC > SOP20-TIEXEC-MEDEL * 1.5 + 120               
041900                                                                          
042000                     PERFORM CAB-FETCH-PARENT-INFO                        
042100                     IF KDPROCSTAT = STARTED-STATUS                       
042200*       -------------- RTN LONG EXEC OCH HAR AKTIV FÖRÄLDER               
042300                       MOVE NAMN-VAERDE TO MSG-PROCESS                    
042400                       MOVE 'LONG EXEC TIME' TO MSG-MEDD                  
042500                       DISPLAY MSG-AREA                                   
042600                       PERFORM CAA-WRITE-MINICALL                         
042700                       PERFORM CAD-WRITE-PROBLEM-CASE                     
042800                     END-IF                                               
042900                 END-IF                                                   
043000               END-IF                                                     
043100             ELSE                                                         
043200               MOVE +0 TO ANTAL                                           
043300               INSPECT SOP20-TEATTN TALLYING ANTAL FOR                    
043400                       ALL 'ABEND'                                        
043500               IF ANTAL > 0                                               
043600*    ----------- JOBB SOM ABENDAT                                         
043700                                                                          
043800                 PERFORM CAB-FETCH-PARENT-INFO                            
043900                 EVALUATE TRUE                                            
044000                   WHEN  KDPROCSTAT = STARTED-STATUS                      
044100                                   OR WAITING-STATUS                      
044200*    -------------   JOB SOM ABENDAT OCH HAR AKTIV FÖRÄLDER               
044300*                    SIGNALERA OM DETTA VARAT ALLTFÖR LÄNGE               
044400                                                                          
044500                     PERFORM CAC-COMPUTE-EXEC-TIME                        
044600                     IF TIEXEC > SOP20-TIEXEC-MEDEL * 1.5 + 60            
044700                       MOVE NAMN-VAERDE TO MSG-PROCESS                    
044800                       MOVE 'LONG TIME ABENDED' TO MSG-MEDD               
044900                       DISPLAY MSG-AREA                                   
045000                       PERFORM CAA-WRITE-MINICALL                         
045100                     END-IF                                               
045200                                                                          
045300                   WHEN  KDPROCSTAT = ENDED-STATUS                        
045400                                   OR PASSIVE-STATUS                      
045500*    -------------   JOB SOM ABENDAT OCH HAR INAKTIV FÖRÄLDER             
045600*                    JOBBET HAR "STÄLLTS", ALLTSÅ INGEN SIGNAL            
045700                     CONTINUE                                             
045800                                                                          
045900                                                                          
046000                   WHEN  KDPROCSTAT = LOW-VALUE                           
046100*    -------------   JOB SOM SAKNAR FÖRÄLDER OCH HAR ABENDAT.             
046200*                    SIGNALERA OM DETTA VARAT ALLTFÖR LÄNGE.              
046300*                    DETTA ÄR ANTAGLIGEN STYR-JOBB AV NÅGOT SLAG          
046400*                    OCH BÖR RAPPORTERAS FORTARE ÄN ANDRA JOBB            
046500                     PERFORM CAC-COMPUTE-EXEC-TIME                        
046600                     IF TIEXEC > SOP20-TIEXEC-MEDEL * 1.5                 
046700                       MOVE NAMN-VAERDE TO MSG-PROCESS                    
046800                       MOVE 'LONG TIME ABENDED' TO MSG-MEDD               
046900                       DISPLAY MSG-AREA                                   
047000                       PERFORM CAA-WRITE-MINICALL                         
047100                     END-IF                                               
047200                                                                          
047300                 END-EVALUATE                                             
047400               ELSE                                                       
047500                 IF SOP20-JOB-PROCESS                                     
047600*    -------------   JOB SOM EJ GÅR I MASKINEN UTAN ABEND-MSG,            
047700*                    UTAN LONG-EXEC MEN NÅGOT ANNAT MEDDELANDE            
047800                   MOVE NAMN-VAERDE TO MSG-PROCESS                        
047900                   MOVE SOP20-TEATTN  TO MSG-MEDD                         
048000                   DISPLAY MSG-AREA                                       
048100                   PERFORM CAA-WRITE-MINICALL                             
048200                   PERFORM CAD-WRITE-PROBLEM-CASE                         
048300                 END-IF                                                   
048400               END-IF                                                     
048500             END-IF                                                       
048600           ELSE                                                           
048700             IF SOP20-JOB-PROCESS                                         
048800*    -------------   JOB SOM EJ GÅR I MASKINEN UTAN NÅGOT MSG             
048900               PERFORM CAB-FETCH-PARENT-INFO                              
049000               IF KDPROCSTAT = STARTED-STATUS OR LOW-VALUE                
049100*    -------------   JOB SOM EJ GÅR I MASKINEN UTAN NÅGOT MSG             
049200*                    OCH HAR AKTIV FÖRÄLDER                               
049300                                                                          
049400                 IF SOPJOBST = '?'                                        
049500*                  -- KOLLA JOBB-STATUS EN GÅNG TILL FÖR SÄKERHETS        
049600*                  -- SKULL (DET KAN HA VARIT PÅ VÄG IN)                  
049700                   CALL W009WAIT USING 5-SEKUNDER                         
049800                   CALL ISPLINK USING ISP-SELECT L-STATUS-CMD             
049900                                     STATUS-CMD                           
050000                   CALL ISPLINK USING VGET N-SOPJOBST SHARED              
050100                 END-IF                                                   
050200                 IF SOPJOBST = '?' OR 'O'                                 
050300                   MOVE NAMN-VAERDE TO MSG-PROCESS                        
050400                   MOVE 'STILL NO JOB, JCL ERROR?' TO MSG-MEDD            
050500                   DISPLAY MSG-AREA                                       
050600                   PERFORM CAA-WRITE-MINICALL                             
050700                   PERFORM CAD-WRITE-PROBLEM-CASE                         
050800                 END-IF                                                   
050900               END-IF                                                     
051000             END-IF                                                       
051100           END-IF                                                         
051200                                                                          
051300         WHEN OTHER                                                       
051400           CONTINUE                                                       
051500       END-EVALUATE                                                       
051600     END-IF                                                               
051700     .                                                                    
051800     EJECT                                                                
051900 CAA-WRITE-MINICALL     SECTION.                                          
052000     SKIP2                                                                
052100     PERFORM S12-WRITE-MSGFILE                                            
052200     .                                                                    
052300     EJECT                                                                
052400 CAB-FETCH-PARENT-INFO    SECTION.                                        
052500     SKIP2                                                                
052600*    -- HÄMTA INFO OM FÖRÄLDERN TILL EN PROCESS.                          
052700*    -- OM FÖRÄLDER SAKNAS, ANGES DET GENOM LOW-VALUE                     
052800                                                                          
052900     MOVE LOW-VALUE TO INF-VAERDE                                         
053000                                                                          
053100*    -- HÄMTA FÖRÄLDERNS NAMN                                             
053200     MOVE SPACE TO ATTR-VAERDE                                            
053300     CALL W980WSPC USING FGETF WSRKOD NAMN-PARM                           
053400               PARENT-PARM ATTR-PARM                                      
053500                                                                          
053600     IF WSRKOD = SPACE                                                    
053700*      -- FÖRÄLDER FINNS - HÄMTA INFO                                     
053800       MOVE LOW-VALUE TO INF-VAERDE                                       
053900       CALL W980WSPC USING FGETF WSRKOD ATTR-PARM                         
054000         INFO-NAMN-PARM INFO-VAERDE-PARM                                  
054100       IF WSRKOD NOT = SPACE                                              
054200         DISPLAY 'ERROR WHEN READING ' ATTR-VAERDE                        
054300                 ' INF'                                                   
054400         CALL ABEND USING RKOD-ABEND-NO-DUMP                              
054500       END-IF                                                             
054600     END-IF                                                               
054700*    -- SPARA EV. FÖRÄLDERS NAMN (RUTINNAMN)                              
054800     IF ATTR-VAERDE = LOW-VALUE                                           
054900       MOVE SPACE       TO MSG-PARENT                                     
055000     ELSE                                                                 
055100       MOVE ATTR-VAERDE TO MSG-PARENT                                     
055200     END-IF                                                               
055300     .                                                                    
055400     EJECT                                                                
055500 CAC-COMPUTE-EXEC-TIME  SECTION.                                          
055600     SKIP2                                                                
055700     MOVE HHMM TO SOP20-TIMINUT-STOPP                                     
055800     DIVIDE SOP20-TIMINUT-START BY 100                                    
055900            GIVING TIMMAR REMAINDER MINUTER                               
056000     COMPUTE TID1 = TIMMAR * 60 + MINUTER                                 
056100     DIVIDE SOP20-TIMINUT-STOPP BY 100                                    
056200            GIVING TIMMAR REMAINDER MINUTER                               
056300     COMPUTE TIEXEC = TIMMAR * 60 + MINUTER                               
056400     ACCEPT WDATUM FROM DATE                                              
056500     MOVE WDATUM                  TO TMP1-YYMMDD                          
056600     MOVE SOP20-TIEXDAT-SENAST    TO TMP2-YYMMDD                          
056700     PERFORM WY2000P1                                                     
056800     IF TMP1-YYMMDD > TMP2-YYMMDD                                         
056900        ADD 1440 TO TIEXEC                                                
057000     END-IF                                                               
057100     COMPUTE TIEXEC = TIEXEC - TID1 + 1                                   
057200     .                                                                    
057300     EJECT                                                                
057400 CAD-WRITE-PROBLEM-CASE  SECTION.                                         
057500                                                                          
057600     MOVE SPACE TO DATA-VAERDE                                            
057700     CALL W980WSPC USING FGETF WSRKOD NAMN-PARM                           
057800         PRIO-PARM DATA-PARM                                              
057900     IF WSRKOD = SPACE                                                    
058000       MOVE DATA-VAERDE (1:1) TO WPRIO                                    
058100     ELSE                                                                 
058200       CALL W980WSPC USING FGETF WSRKOD ATTR-PARM                         
058300           PRIO-PARM DATA-PARM                                            
058400       IF WSRKOD = SPACE                                                  
058500         MOVE DATA-VAERDE (1:1) TO WPRIO                                  
058600       ELSE                                                               
058700         MOVE SPACE TO WPRIO                                              
058800       END-IF                                                             
058900     END-IF                                                               
059000                                                                          
059100     WRITE CASE-POST FROM NAMN-VAERDE                                     
059200     WRITE CASE-POST FROM WTIMESTAMP                                      
059300     WRITE CASE-POST FROM WPRIO                                           
059400     WRITE CASE-POST FROM MSG-AREA                                        
059500     WRITE CASE-POST FROM MSG-PARENT                                      
059600     .                                                                    
059700     EJECT                                                                
059800 Z-FINIT     SECTION.                                                     
059900     SKIP2                                                                
060000     MOVE '9'   TO SOP20-KDSOPFUNK                                        
060100     CALL W9802000 USING SOP20-W98020                                     
060200     IF SOP20-KDRET > 4                                                   
060300       DISPLAY 'ERROR FRÅN STÄNGA SOP'                                    
060400       CALL ABEND USING RKOD-ABEND-NO-DUMP                                
060500     END-IF                                                               
060600     CLOSE MSGFILE CASEFILE                                               
060700     .                                                                    
060800     EJECT                                                                
060900 S12-WRITE-MSGFILE    SECTION.                                            
061000     SKIP2                                                                
061100     WRITE MSG-POST FROM MSG-AREA                                         
061200     .                                                                    
061300     EJECT                                                                
061400*    -COPY WY2000P1                                                       
