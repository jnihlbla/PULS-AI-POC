000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2247100.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   13/10/18.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700                                                                          
000800*    FUNCTION:                                                            
000900*        INPUT FILE                                                       
000901*        - PARAMETER FROM SCREEN 2115 WITH SUPPLIER SHIP AND              
000910*          KVVECKOR-AT AND KVVECKOR-LT.                                   
000920*        - PARAMETER FROM SCREEN 2111 WITH SUPPLIER SHIP AND              
000930*          KVDAGAR-TT.                                                    
000940*                                                                         
001000*        UPDATE KVVECKOR-AT, KVVECKOR-LT AND KVDAGAR-TT ON WDK611         
001100*        FOR ALL PARTNO CONNECTED TO THE SUPPLIER SHIP.                   
001240*                                                                         
001500*        THE PROGRAM UPDATES WDK611 AND WDG3 HTYP=2213/14                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- PARATMETER FROM SCREEN 2111 OR 2115                        
002600     SELECT W22471                     ASSIGN TO W22471D1.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP3                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W22471                                                               
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  RECORD -COPY W2247301 -PRE  IN-  -L.                                 
003700                                                                          
003800 WORKING-STORAGE SECTION.                                                 
003900                                                                          
004000 77  IDPGM                       PIC X(8)    VALUE 'W2247100'.            
004010 77  CURRENT-SECTION             PIC X(30)   VALUE SPACE.                 
004020 77  DBS-SECTION                 PIC X(30)   VALUE SPACE.                 
004021                                                                          
004022 77  W-UPDATE-WDK611           PIC X(01)   VALUE SPACE.                   
004023                                                                          
004030 01  W-KVVECKOR-AT               PIC X(02).                               
004040 01  FILLER REDEFINES W-KVVECKOR-AT.                                      
004041     03  W-KVVECKOR-AT-NUM       PIC 9(02).                               
004050                                                                          
004060 01  W-KVVECKOR-LT               PIC X(02).                               
004070 01  FILLER REDEFINES W-KVVECKOR-LT.                                      
004080     03  W-KVVECKOR-LT-NUM       PIC 9(02).                               
004090                                                                          
004091 01  W-KVDAGAR-TT                PIC X(02).                               
004092 01  FILLER REDEFINES W-KVDAGAR-TT.                                       
004093     03  W-KVDAGAR-TT-NUM        PIC 9(02).                               
004094                                                                          
004100 01  CHKP-VAR.                                                            
004200     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004300     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004400     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004500     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004600     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004700     03 CHKP-MAX                 PIC S9(3)   VALUE +500 COMP-3.           
004710 01  W-KVPOST-IN                 PIC S9(9)  VALUE ZERO COMP SYNC.         
004800                                                                          
004900 77  YES                         PIC X       VALUE 'Y'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005001                                                                          
005010*    --- PARAMETERS TO ABEND                                              
005030 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
005040 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
005050 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
005060                                                                          
005100 01  ERROR-TEXT.                                                          
005200     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
005300     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005400                                                                          
005500 77  W22471-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  END-OF-W22471                       VALUE 'Y'.                   
005700     EJECT                                                                
005800 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005900 01  FILLER REDEFINES TODAYS-DATE.                                        
006000     03  TODAYS-DATE-YEAR        PIC 9(2).                                
006100     03  TODAYS-DATE-MONTH       PIC 9(2).                                
006200     03  TODAYS-DATE-DAY         PIC 9(2).                                
006300     EJECT                                                                
006400 01  GENERAL-SUBPROGRAMS.                                                 
006500*                                                                         
006600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006810     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
006900     EJECT                                                                
007000*    --- PARAMETRAR TILL POSTSUM                                          
007100*                                                                         
007200*01  -COPY W0005   -PRE  POSTSUM-                                         
007300     EJECT                                                                
007400 01  IN-AREA-START               PIC X(24)   VALUE                        
007500                                             'IN-AREA-START'.             
007600     SKIP2                                                                
007700                                                                          
007800*01  AREA -COPY W2247301     -PRE IN-                                     
007900*                                                                         
008000     EJECT                                                                
008010*01 -COPY WWDCKONS                                                        
008020     EJECT                                                                
008100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008200     SKIP3                                                                
008300 01  KEYS-TILL-DLI.                                                       
008510     03  W-IDARTNR-X.                                                     
008520         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008600                                                                          
008610     03  W-IDLEVNR-SHIP-X.                                                
008620         05  W-IDLEVNR-SHIP      PIC X(5)    VALUE SPACE.                 
008630                                                                          
008631     03  W-WDG3KEY-2213-X.                                                
008632         05  W-IDHTYP-2213       PIC X(4)    VALUE '2213'.                
008633         05  W-IDDC-2213         PIC X(2)    VALUE SPACE.                 
008634         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
008635                                                                          
008640     03  W-WDGXKEY-4579-X.                                                
008650         05  W-IDHTYP-4579       PIC X(4)    VALUE '4579'.                
008660         05  W-IDPGM             PIC X(8)    VALUE 'W2247100'.            
008670         05  FILLER              PIC X(18)   VALUE LOW-VALUE.             
009100                                                                          
009200*    --- STATUS-KOD FRÅN IMS                                              
009300 01  STATUS-WS                   PIC XX.                                  
009400     88  SEGMENT-FOUND                       VALUE '  '.                  
009500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
009600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
009700     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
009800     88  IMS-NOT-OK                          VALUE 'XD'.                  
009900     SKIP2                                                                
010000 01  GOOD-STATUSCODES.                                                    
010100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
010200     SKIP3                                                                
010300 01  SSA1                        PIC X(64).                               
010400 01  SSA2                        PIC X(64).                               
010500     EJECT                                                                
010600*    --- IMS FUNCTION CODES                                               
010700*01  -COPY W0003                                                          
010800     EJECT                                                                
010900*    ---  DLI INPUT-OUTPUT AREA                                           
011000                                                                          
011500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
011600 01  DLI-IO-WDK611.                                                       
011700*    03  -COPY WDK611                                                     
011800     EJECT                                                                
011900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX2214'.                    
012000 01  DLI-IO-WDGX2214.                                                     
012100*    03  -COPY WDGX2214                                                   
012200     EJECT                                                                
012210*-ÅTERSTARTSREGISTER WDR4                                                 
012220 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4580'.                    
012230 01  DLI-IO-WDGX4580.                                                     
012240*    03  -COPY WDGX4580                                                   
012250     EJECT                                                                
012300 LINKAGE SECTION.                                                         
012400                                                                          
012500*01  -COPY W0009   -PRE MSG-                                              
012600                                                                          
012700*01  -COPY W0008  -PRE WDK6-                                              
012800     05  FILLER                  PIC X.                                   
012900     EJECT                                                                
013000*01  -COPY W0008  -PRE WDG3-                                              
013100     05  FILLER                  PIC X.                                   
013200     EJECT                                                                
013210*01  -COPY W0008  -PRE 4579-                                              
013220     05  FILLER                  PIC X.                                   
013230     EJECT                                                                
013300 PROCEDURE DIVISION  USING MSG-PCB WDK6-PCB WDG3-PCB 4579-PCB.            
013400 MAIN SECTION.                                                            
013500     ENTRY 'DLITCBL' USING MSG-PCB WDK6-PCB WDG3-PCB 4579-PCB.            
013600                                                                          
013700     PERFORM A-INIT                                                       
013701                                                                          
013703     PERFORM IMS-LAS-ATERSTART                                            
013704     IF 4580-KVPOST > +0                                                  
013707        PERFORM B-LAES-FRAM-TILL-CHKPOINT                                 
013708     ELSE                                                                 
013709       PERFORM S01-READ-W22471                                            
013710     END-IF                                                               
013900                                                                          
014200     PERFORM UNTIL END-OF-W22471                                          
014400        MOVE IN-IDARTNR         TO W-IDARTNR                              
014500        MOVE IN-IDLEVNR         TO W-IDLEVNR-SHIP                         
014600        PERFORM IMS-GHU-WDK611                                            
014610                                                                          
014620        IF SEGMENT-FOUND                                                  
014700           PERFORM H-UPDATE-WDK6                                          
014800        END-IF                                                            
014900                                                                          
014910        PERFORM S01-READ-W22471                                           
015100     END-PERFORM                                                          
015200                                                                          
015300     PERFORM Z-FINIT                                                      
015400                                                                          
015500     MOVE ZERO TO RETURN-CODE                                             
015600     GOBACK                                                               
015700     .                                                                    
015800     EJECT                                                                
015900 A-INIT SECTION.                                                          
016000     SKIP2                                                                
016100                                                                          
016200     OPEN  INPUT W22471                                                   
016300                                                                          
016400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
016500                                                                          
016600     PERFORM IMS-RESTART                                                  
016601                                                                          
016602     ACCEPT TODAYS-DATE  FROM DATE                                        
016603                                                                          
016610     MOVE +0                 TO W-KVPOST-IN                               
016620                                CHKP-ANT                                  
016700     .                                                                    
016800     EJECT                                                                
016810 B-LAES-FRAM-TILL-CHKPOINT SECTION.                                       
016820     MOVE 'B-LAES-FRAM-TILL-CHKPOINT' TO CURRENT-SECTION                  
016830                                                                          
016840     PERFORM S01-READ-W22471                                              
016850     PERFORM UNTIL END-OF-W22471 OR                                       
016860                   W-KVPOST-IN = 4580-KVPOST                              
016870        PERFORM S01-READ-W22471                                           
016880     END-PERFORM                                                          
016890                                                                          
016891     IF END-OF-W22471                                                     
016892        MOVE 'INPUTFIL EOF = YES, VID ÅTERSTART'                          
016893                      TO ERROR-TEXT                                       
016894        CALL ABEND USING RKOD-ABEND-NO-DUMP                               
016895     END-IF                                                               
016896     .                                                                    
016897     EJECT                                                                
016900 H-UPDATE-WDK6 SECTION.                                                   
017000                                                                          
017100     MOVE NOO                      TO W-UPDATE-WDK611                     
017200                                                                          
017400     IF IN-KVVECKOR-AT NOT = ALL '+'                                      
017410        IF CLAG-FLMANAT = NOO                                             
017500          MOVE IN-KVVECKOR-AT      TO W-KVVECKOR-AT                       
017510          MOVE W-KVVECKOR-AT-NUM   TO CLAG-KVVECKOR-AT                    
017520          MOVE YES                 TO W-UPDATE-WDK611                     
017600       END-IF                                                             
017601     END-IF                                                               
017610                                                                          
017700     IF IN-KVVECKOR-LT NOT = ALL '+'                                      
017710        IF CLAG-FLMANLT = NOO                                             
017800           MOVE IN-KVVECKOR-LT     TO W-KVVECKOR-LT                       
017810           MOVE W-KVVECKOR-LT-NUM  TO CLAG-KVVECKOR-LT                    
017900           COMPUTE CLAG-KVVECKOR-FT ROUNDED =                             
018000                  (CLAG-KVDAGAR-FFH / 5) + CLAG-KVVECKOR-LT               
018010          MOVE YES                 TO W-UPDATE-WDK611                     
018100        END-IF                                                            
018101     END-IF                                                               
018102                                                                          
018110     IF IN-KVDAGAR-TT NOT = ALL '+'                                       
018120        MOVE IN-KVDAGAR-TT         TO W-KVDAGAR-TT                        
018130        MOVE W-KVDAGAR-TT-NUM      TO CLAG-KVDAGAR-TT                     
018131        MOVE YES                   TO W-UPDATE-WDK611                     
018140     END-IF                                                               
018210                                                                          
018220     IF W-UPDATE-WDK611 = YES                                             
018230        PERFORM IMS-REPL-WDK611                                           
018240                                                                          
018300        MOVE WC-CDC-SE             TO W-IDDC-2213                         
018310        MOVE IN-IDARTNR            TO 2214-IDARTNR                        
018400        PERFORM IMS-ISRT-WDGX-2214                                        
018500                                                                          
018510        ADD  +2                    TO CHKP-ANT                            
018515                                                                          
018520        IF CHKP-ANT > CHKP-MAX                                            
018530           PERFORM X-TAKE-CHECKPOINT                                      
018540        END-IF                                                            
018550     END-IF                                                               
018600     .                                                                    
018700     EJECT                                                                
018800 Z-FINIT SECTION.                                                         
018900                                                                          
019000     CLOSE W22471                                                         
019100                                                                          
019200     MOVE 'S' TO POSTSUM-OPKOD                                            
019300     CALL POSTSUM USING POSTSUM-PARM                                      
019301                                                                          
019310     PERFORM IMS-LAS-ATERSTART                                            
019320                                                                          
019330     MOVE +0                           TO 4580-KVPOST                     
019340     MOVE TODAYS-DATE                  TO 4580-TIUPPDAT                   
019350     ACCEPT 4580-TIUPPTID FROM TIME                                       
019360                                                                          
019370     PERFORM IMS-REPL-ATERSTART                                           
019400     .                                                                    
019500     EJECT                                                                
019600 S01-READ-W22471               SECTION.                                   
019700     SKIP2                                                                
019800     READ W22471 INTO IN-AREA                                             
019900     AT END                                                               
019910        SET END-OF-W22471 TO TRUE                                         
019920                                                                          
019930     NOT AT END                                                           
020000                                                                          
020200        MOVE 'W22471'   TO POSTSUM-FDNAMN                                 
020300        MOVE 'W22471D1' TO POSTSUM-DDNAMN2                                
020400        MOVE 'PARM'     TO POSTSUM-TRANSTYP                               
020500        CALL POSTSUM USING POSTSUM-PARM                                   
020510        ADD +1          TO W-KVPOST-IN                                    
020600     END-READ                                                             
020700     .                                                                    
020800     EJECT                                                                
020900* --- IMS SECTIONS  ---                                                   
021000                                                                          
021010 X-TAKE-CHECKPOINT   SECTION.                                             
021020     MOVE 'X-TAKE-CHECKPOINT        ' TO CURRENT-SECTION                  
021030                                                                          
021040*    UPPDATERA ÅTERSTARTREGISTRET                                         
021050     PERFORM IMS-LAS-ATERSTART                                            
021060                                                                          
021070     MOVE W-KVPOST-IN       TO 4580-KVPOST                                
021080     ACCEPT 4580-TIUPPDAT FROM DATE                                       
021090     ACCEPT 4580-TIUPPTID FROM TIME                                       
021091                                                                          
021093     PERFORM IMS-REPL-ATERSTART                                           
021094                                                                          
021095*    TAG CHECKPOINT                                                       
021096     PERFORM IMS-CHECKPOINT                                               
021097                                                                          
021098     MOVE +0                TO CHKP-ANT                                   
021099     .                                                                    
021100     EJECT                                                                
022000* --- IMS SECTIONS  ---                                                   
022100                                                                          
022200     EJECT                                                                
022300 IMS-RESTART SECTION.                                                     
022400     SKIP2                                                                
022500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
022600     MOVE '  ' TO GOOD-STATUSCODES                                        
022700     CALL CBLTDLI USING XRST MSG-PCB                                      
022800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
022900                        CHKP-AREA-LENGTH CHKP-AREA                        
023000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
023100     PERFORM IMS-STATUSCHECK                                              
023200     .                                                                    
023300     SKIP3                                                                
023400 IMS-CHECKPOINT SECTION.                                                  
023500     SKIP2                                                                
023600     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
023700     MOVE '  XD' TO GOOD-STATUSCODES                                      
023800     CALL CBLTDLI USING CHKP MSG-PCB                                      
023900                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
024000                        CHKP-AREA-LENGTH CHKP-AREA                        
024100     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
024200     PERFORM IMS-STATUSCHECK                                              
024300                                                                          
024400     IF IMS-NOT-OK                                                        
024500       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
024510                                    TO ERROR-TEXT-STR                     
024600       DISPLAY ERROR-TEXT                                                 
024700       CALL FELLOG                                                        
024800     END-IF                                                               
024900     .                                                                    
025000     EJECT                                                                
025100 IMS-GHU-WDK611         SECTION.                                          
025200     SKIP3                                                                
025300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X  ')'                        
025400              DELIMITED BY SIZE INTO SSA1                                 
025430     MOVE 'WDK611 '               TO SSA2                                 
025500     MOVE '  GE'                  TO GOOD-STATUSCODES                     
025600     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611                        
025700                                     SSA1 SSA2                            
025800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
025900     PERFORM IMS-STATUSCHECK                                              
026000     .                                                                    
026100                                                                          
027500 IMS-REPL-WDK611 SECTION.                                                 
027600                                                                          
027700     MOVE '  ' TO GOOD-STATUSCODES                                        
027800     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
027900     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
028000     PERFORM IMS-STATUSCHECK                                              
028100     .                                                                    
028200     EJECT                                                                
028300 IMS-ISRT-WDGX-2214 SECTION.                                              
028400                                                                          
028500     STRING 'WDG301  (WDG3KEY  =' W-WDG3KEY-2213-X ')'                    
028600             DELIMITED BY SIZE INTO SSA1                                  
028700     MOVE 'WDG302 '              TO SSA2                                  
028800     MOVE '  II'                 TO GOOD-STATUSCODES                      
028900     CALL CBLTDLI USING ISRT WDG3-PCB DLI-IO-WDGX2214 SSA1 SSA2           
029000     MOVE WDG3-STATUS-CODE     TO STATUS-WS                               
029100     PERFORM IMS-STATUSCHECK                                              
029200     .                                                                    
029300     EJECT                                                                
029310 IMS-LAS-ATERSTART SECTION.                                               
029320     MOVE 'IMS-LAS-ATERSTART    ' TO DBS-SECTION                          
029330                                                                          
029331     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4579-X ')'                    
029350                    DELIMITED BY SIZE INTO SSA1                           
029360     MOVE 'WDR470 '        TO SSA2                                        
029370     MOVE '  '             TO GOOD-STATUSCODES                            
029380     CALL CBLTDLI USING GHU 4579-PCB DLI-IO-WDGX4580 SSA1 SSA2            
029390     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
029391     PERFORM IMS-STATUSCHECK                                              
029392     .                                                                    
029393                                                                          
029394 IMS-REPL-ATERSTART SECTION.                                              
029395     MOVE 'IMS-REPL-ATERSTART   ' TO DBS-SECTION                          
029396                                                                          
029397     MOVE '  '             TO GOOD-STATUSCODES                            
029398     CALL CBLTDLI USING REPL 4579-PCB DLI-IO-WDGX4580                     
029399     MOVE 4579-STATUS-CODE TO STATUS-WS                                   
029400     PERFORM IMS-STATUSCHECK                                              
029401     .                                                                    
029402                                                                          
029410 IMS-STATUSCHECK SECTION.                                                 
029500     SKIP2                                                                
029600     SET STATUS-IX TO 1                                                   
029700     SEARCH GOOD-STATUS                                                   
029800       AT END                                                             
029900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
030000           DELIMITED BY SIZE INTO ERROR-TEXT                              
030100         DISPLAY ERROR-TEXT                                               
030200         CALL FELLOG                                                      
030300       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
030400         CONTINUE                                                         
030500     END-SEARCH                                                           
030600     .                                                                    
