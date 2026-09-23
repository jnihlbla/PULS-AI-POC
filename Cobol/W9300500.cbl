001200 ID DIVISION.                                                             
001300 PROGRAM-ID.     W9300500.                                                
001400 AUTHOR.         BHAT ARCHANA.                                            
001500 DATE-WRITTEN.   20/10/05.                                                
001600 DATE-COMPILED.                                                           
001700                                                                          
001800                                                                          
001900*    FUNCTION:                                                            
002000*        THIS PROGRAM UPDATES THE YEARLY RATES IN CURRENCY DB             
002100*                                                                         
002210*        THE PROGRAM UPDATES   WDG2                                       
002300*                                                                         
002400                                                                          
002500     SKIP3                                                                
002600 ENVIRONMENT DIVISION.                                                    
002700     SKIP2                                                                
002800 INPUT-OUTPUT SECTION.                                                    
002900                                                                          
003000 FILE-CONTROL.                                                            
003101     SKIP2                                                                
003102*          --- YEARLY RATES                                               
003110     SELECT W93005                     ASSIGN TO W93005D1.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003701     SKIP3                                                                
003702 FD  W93005                                                               
003703     RECORDING       F                                                    
003704     BLOCK CONTAINS  0.                                                   
003705                                                                          
003710*01  -COPY W93005      -L.                                                
003800     EJECT                                                                
003900 WORKING-STORAGE SECTION.                                                 
004000                                                                          
004100 77  IDPGM                       PIC X(8)    VALUE 'W9300500'.            
004110 77  W-IDLEGSEL                  PIC X(4)    VALUE SPACES.                
004200 01  CHKP-VAR.                                                            
004300     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
004400     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
004500     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
004600     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
004700     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
004800     03 CHKP-MAX                 PIC S9(3)   VALUE +50  COMP-3.           
004900 77  YES                         PIC X       VALUE 'J'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005100     SKIP2                                                                
005200 01  ERROR-TEXT.                                                          
005300     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
005400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005601                                                                          
005602 77  W93005-EOF-SW               PIC X       VALUE 'N'.                   
005610     88  END-OF-W93005                       VALUE 'Y'.                   
005900     EJECT                                                                
006000 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
006500     EJECT                                                                
006510 01  WS-DATE-YYYYMMDD.                                                    
006520     03  FILLER                  PIC 9(2).                                
006530     03  WS-DATE-YYMMDD          PIC 9(6).                                
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700*                                                                         
006800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007010     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007101     EJECT                                                                
007102*    --- PARAMETRAR TILL POSTSUM                                          
007103*                                                                         
007110*01  -COPY W0005   -PRE  POSTSUM-                                         
007401     EJECT                                                                
007402*    -COPY WY2000W1                                                       
007403     EJECT                                                                
007404 01  IN-AREA-START               PIC X(24)   VALUE                        
007405                                             'IN-AREA-START'.             
007406     SKIP2                                                                
007407                                                                          
007410*01  AREA -COPY W93005     -PRE IN-                                       
007500*                                                                         
007600     EJECT                                                                
007700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007800     SKIP3                                                                
008110 01  KEYS-TILL-DLI.                                                       
008120     03  W-WDGXKEY-X.                                                     
008130         05  W-IDHTYP            PIC X(4)    VALUE '9305'.                
008140         05  W-KDVALISO-HUV      PIC X(3)    VALUE SPACE.                 
008150         05  W-KDVALTYP          PIC X(1)    VALUE 'A'.                   
008160         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
008170     03  W-KDVALISO-X.                                                    
008180         05  W-KDVALISO          PIC X(3)    VALUE SPACE.                 
008190     03  W-TISTADA9-X.                                                    
008191         05  W-TISTADAT-9KOMPL   PIC S9(7)   VALUE ZERO COMP-3.           
008192     SKIP2                                                                
008200*    --- STATUS-KOD FRÅN IMS                                              
008300 01  STATUS-WS                   PIC XX.                                  
008400     88  SEGMENT-FOUND                       VALUE '  '.                  
008500     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
008600     88  SEGMENT-MISSING                     VALUE 'GE'.                  
008700     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
008800     88  IMS-NOT-OK                          VALUE 'XD'.                  
008900     SKIP2                                                                
009000 01  GOOD-STATUSCODES.                                                    
009100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009200     SKIP3                                                                
009300 01  SSA1                        PIC X(64).                               
009400 01  SSA2                        PIC X(64).                               
009410 01  SSA3                        PIC X(64).                               
009500     EJECT                                                                
009600*    --- IMS FUNCTION CODES                                               
009700*01  -COPY W0003                                                          
009800     EJECT                                                                
010000*    ---  DLI INPUT-OUTPUT AREA                                           
010100                                                                          
010201 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9305'.                    
010202 01  DLI-IO-WDGX9305.                                                     
010203*    03  -COPY WDGX9305                                                   
010204     EJECT                                                                
010205 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9306'.                    
010206 01  DLI-IO-WDGX9306.                                                     
010207*    03  -COPY WDGX9306                                                   
010208     EJECT                                                                
010209 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
010210 01  DLI-IO-WDGX9308.                                                     
010220*    03  -COPY WDGX9308                                                   
010300                                                                          
010400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
010500 01  DLI-IO-WDB601.                                                       
010600*    03  -COPY WDB601                                                     
010700     EJECT                                                                
010800 LINKAGE SECTION.                                                         
010900                                                                          
011000*01  -COPY W0009  -PRE MSG-                                               
011101                                                                          
011102*01  -COPY W0008  -PRE 9305-                                              
011110     05  FILLER                  PIC X.                                   
011120*01  -COPY W0008  -PRE WDB6-                                              
011130     05  FILLER                  PIC X.                                   
011400     EJECT                                                                
011501 PROCEDURE DIVISION  USING MSG-PCB 9305-PCB WDB6-PCB.                     
011502 MAIN SECTION.                                                            
011510     ENTRY 'DLITCBL' USING MSG-PCB 9305-PCB WDB6-PCB.                     
011600                                                                          
011800     SKIP2                                                                
011900     PERFORM A-INIT                                                       
012010     PERFORM S01-READ-W93005                                              
012100     PERFORM UNTIL END-OF-W93005                                          
012200       IF CHKP-ANT > CHKP-MAX                                             
012300         PERFORM X-TAKE-CHECKPOINT                                        
012400       END-IF                                                             
012500       PERFORM B-PROCESS                                                  
013110       PERFORM S01-READ-W93005                                            
013200     END-PERFORM                                                          
013400                                                                          
013500     PERFORM Z-FINIT                                                      
013600                                                                          
013700     MOVE ZERO TO RETURN-CODE                                             
013800     GOBACK                                                               
013900     .                                                                    
014000     EJECT                                                                
014100 A-INIT SECTION.                                                          
014200     SKIP2                                                                
014300                                                                          
014400     PERFORM IMS-RESTART                                                  
014601                                                                          
014610     OPEN INPUT W93005                                                    
014900                                                                          
015000     ACCEPT TODAYS-DATE        FROM DATE                                  
015310     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015600     .                                                                    
015800     EJECT                                                                
015810 B-PROCESS SECTION.                                                       
015820                                                                          
015830     IF IN-IDLEGSEL = 'VCCS'                                              
015840       MOVE 'SEK'                TO W-KDVALISO-HUV                        
015850     ELSE                                                                 
015860       MOVE IN-IDLEGSEL          TO W-IDLEGSEL                            
015870       PERFORM IMS-GU-WDB601-LEGSEL                                       
015880       IF SEGMENT-FOUND                                                   
015890         MOVE DCS-KDVALISO       TO W-KDVALISO-HUV                        
015891       END-IF                                                             
015892     END-IF                                                               
015894     MOVE IN-KDVALISO            TO W-KDVALISO                            
015899     PERFORM IMS-GU-WDGX9306                                              
015900     IF SEGMENT-MISSING                                                   
015901       MOVE IN-KDVALISO          TO 9306-KDVALISO                         
015902       PERFORM IMS-ISRT-WDGX9306                                          
015903       ADD +1                    TO CHKP-ANT                              
015904       MOVE IN-DASTADAT          TO WS-DATE-YYYYMMDD                      
015905       MOVE WS-DATE-YYMMDD       TO 9308-TISTADAT                         
015906       COMPUTE 9308-TISTADAT-9KOMPL =                                     
015907                   9999999 - 9308-TISTADAT                                
015908       MOVE IN-REVALUTA-TO       TO 9308-REVALUTA-TO                      
015909       MOVE IN-REVALUTA-FROM     TO 9308-REVALUTA-FROM                    
015910       MOVE IN-PRKURS-NEW        TO 9308-PRKURS                           
015911       MOVE IN-DAREGDAT          TO WS-DATE-YYYYMMDD                      
015912       MOVE WS-DATE-YYMMDD       TO 9308-TIREGDAT                         
015913       PERFORM IMS-ISRT-WDGX9308                                          
015914       ADD +1                    TO CHKP-ANT                              
015915     ELSE                                                                 
015916       MOVE IN-DASTADAT          TO WS-DATE-YYYYMMDD                      
015917       COMPUTE W-TISTADAT-9KOMPL =                                        
015918                   9999999 - WS-DATE-YYMMDD                               
015919       PERFORM IMS-GHNP-WDGX9308                                          
015920       IF SEGMENT-MISSING                                                 
015921         MOVE IN-DASTADAT        TO WS-DATE-YYYYMMDD                      
015922         MOVE WS-DATE-YYMMDD     TO 9308-TISTADAT                         
015923         COMPUTE 9308-TISTADAT-9KOMPL =                                   
015924                     9999999 - 9308-TISTADAT                              
015925         MOVE IN-REVALUTA-TO     TO 9308-REVALUTA-TO                      
015926         MOVE IN-REVALUTA-FROM   TO 9308-REVALUTA-FROM                    
015927         MOVE IN-PRKURS-NEW      TO 9308-PRKURS                           
015928         MOVE IN-DAREGDAT        TO WS-DATE-YYYYMMDD                      
015929         MOVE WS-DATE-YYMMDD     TO 9308-TIREGDAT                         
015930         PERFORM IMS-ISRT-WDGX9308                                        
015931         ADD +1                  TO CHKP-ANT                              
015932       ELSE                                                               
015933         IF IN-DADELDAT = 0                                               
015934           MOVE TODAYS-DATE        TO TMP1-YYMMDD                         
015935           MOVE 9308-TISTADAT      TO TMP2-YYMMDD                         
015936           PERFORM WY2000P1                                               
015937           IF TMP1-YYMMDD < TMP2-YYMMDD                                   
015938             MOVE IN-DASTADAT      TO WS-DATE-YYYYMMDD                    
015939             MOVE WS-DATE-YYMMDD   TO 9308-TISTADAT                       
015940             COMPUTE 9308-TISTADAT-9KOMPL =                               
015941                         9999999 - 9308-TISTADAT                          
015942             MOVE IN-REVALUTA-TO   TO 9308-REVALUTA-TO                    
015943             MOVE IN-REVALUTA-FROM TO 9308-REVALUTA-FROM                  
015944             MOVE IN-PRKURS-NEW    TO 9308-PRKURS                         
015945             MOVE IN-DAREGDAT      TO WS-DATE-YYYYMMDD                    
015946             MOVE WS-DATE-YYMMDD   TO 9308-TIREGDAT                       
015947             PERFORM IMS-REPL-WDGX9308                                    
015948             ADD +1                TO CHKP-ANT                            
015949           END-IF                                                         
015950         ELSE                                                             
015951           PERFORM IMS-DLET-WDGX9308                                      
015952           ADD +1                  TO CHKP-ANT                            
015953         END-IF                                                           
015954       END-IF                                                             
015960     END-IF                                                               
015981     .                                                                    
015982     EJECT                                                                
015990 Z-FINIT SECTION.                                                         
016000                                                                          
016401                                                                          
016410     CLOSE W93005                                                         
016601     SKIP2                                                                
016602     MOVE 'S' TO POSTSUM-OPKOD                                            
016610     CALL POSTSUM USING POSTSUM-PARM                                      
016800     .                                                                    
016901     EJECT                                                                
016902 S01-READ-W93005  SECTION.                                                
016903     SKIP2                                                                
016904     READ W93005 INTO IN-AREA                                             
016905     AT END                                                               
016906        MOVE HIGH-VALUE TO IN-AREA                                        
016907        SET END-OF-W93005 TO TRUE                                         
016908                                                                          
016909     NOT AT END                                                           
016910        MOVE 'W93005' TO POSTSUM-FDNAMN                                   
016911        MOVE 'W93005D1' TO POSTSUM-DDNAMN2                                
016913        CALL POSTSUM USING POSTSUM-PARM                                   
016914                                                                          
016916     END-READ                                                             
016920     .                                                                    
017200     EJECT                                                                
017300 X-TAKE-CHECKPOINT   SECTION.                                             
017400                                                                          
018000     PERFORM IMS-CHECKPOINT                                               
018100     MOVE ZERO TO CHKP-ANT                                                
018300     .                                                                    
018400     EJECT                                                                
018500* --- IMS SECTIONS  ---                                                   
018600                                                                          
018700 IMS-GU-WDGX9306 SECTION.                                                 
018701                                                                          
018702     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
018703             DELIMITED BY SIZE INTO SSA1                                  
018704     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
018705             DELIMITED BY SIZE INTO SSA2                                  
018706     MOVE '  GE'                 TO GOOD-STATUSCODES                      
018707     CALL CBLTDLI             USING GU                                    
018708                                    9305-PCB                              
018709                                    DLI-IO-WDGX9306                       
018710                                    SSA1 SSA2                             
018711     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
018712     PERFORM IMS-STATUSCHECK                                              
018713     .                                                                    
018714     SKIP3                                                                
018715 IMS-ISRT-WDGX9306 SECTION.                                               
018716                                                                          
018717     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
018718             DELIMITED BY SIZE INTO SSA1                                  
018719     MOVE 'WDGX9306 ' TO SSA2                                             
018720     MOVE '  '                   TO GOOD-STATUSCODES                      
018721     CALL CBLTDLI             USING ISRT                                  
018722                                    9305-PCB                              
018723                                    DLI-IO-WDGX9306                       
018724                                    SSA1 SSA2                             
018725     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
018726     PERFORM IMS-STATUSCHECK                                              
018727     .                                                                    
018728     EJECT                                                                
018729 IMS-GHNP-WDGX9308 SECTION.                                               
018730                                                                          
018731     STRING 'WDGX9308(TISTADA9 =' W-TISTADA9-X ')'                        
018732             DELIMITED BY SIZE INTO SSA1                                  
018733     MOVE '  GE'                 TO GOOD-STATUSCODES                      
018734     CALL CBLTDLI             USING GHNP                                  
018735                                    9305-PCB                              
018736                                    DLI-IO-WDGX9308                       
018737                                    SSA1                                  
018738     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
018739     PERFORM IMS-STATUSCHECK                                              
018740     .                                                                    
018741     SKIP3                                                                
018742 IMS-REPL-WDGX9308 SECTION.                                               
018743                                                                          
018744     MOVE '  '                   TO GOOD-STATUSCODES                      
018745     CALL CBLTDLI             USING REPL                                  
018746                                    9305-PCB                              
018747                                    DLI-IO-WDGX9308                       
018748     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
018749     PERFORM IMS-STATUSCHECK                                              
018750     .                                                                    
018751     EJECT                                                                
018752 IMS-ISRT-WDGX9308 SECTION.                                               
018753                                                                          
018754     STRING 'WDG201  (WDGXKEY  =' W-WDGXKEY-X ')'                         
018755             DELIMITED BY SIZE INTO SSA1                                  
018756     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
018757             DELIMITED BY SIZE INTO SSA2                                  
018758     MOVE 'WDGX9308 '            TO SSA3                                  
018759     MOVE '  '                   TO GOOD-STATUSCODES                      
018760     CALL CBLTDLI             USING ISRT                                  
018761                                    9305-PCB                              
018762                                    DLI-IO-WDGX9308                       
018763                                    SSA1 SSA2 SSA3                        
018764     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
018765     PERFORM IMS-STATUSCHECK                                              
018766     .                                                                    
018767     SKIP3                                                                
018768 IMS-DLET-WDGX9308 SECTION.                                               
018769                                                                          
018770     MOVE '  '                   TO GOOD-STATUSCODES                      
018771     CALL CBLTDLI             USING DLET                                  
018772                                    9305-PCB                              
018773                                    DLI-IO-WDGX9308                       
018774     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
018775     PERFORM IMS-STATUSCHECK                                              
018776     .                                                                    
018777     EJECT                                                                
018778 IMS-GU-WDB601-LEGSEL SECTION.                                            
018779     STRING 'WDB601  (IDLEGSEL =' W-IDLEGSEL ')'                          
018780            DELIMITED BY SIZE INTO SSA1                                   
018781     MOVE '  GB'                 TO GOOD-STATUSCODES                      
018782     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
018783     MOVE WDB6-STATUS-CODE       TO STATUS-WS                             
018784     PERFORM IMS-STATUSCHECK                                              
018785     .                                                                    
018786     SKIP3                                                                
018787 IMS-RESTART SECTION.                                                     
018788     SKIP2                                                                
018789     MOVE SPACE                  TO CHKP-MSG-IO-AREA                      
018790     MOVE '  '                   TO GOOD-STATUSCODES                      
018791     CALL CBLTDLI             USING XRST                                  
018792                                    MSG-PCB                               
018793                                    CHKP-MSG-IO-AREA-LENGTH               
018794                                    CHKP-MSG-IO-AREA                      
018795                                    CHKP-AREA-LENGTH                      
018796                                    CHKP-AREA                             
018797     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
018798     PERFORM IMS-STATUSCHECK                                              
018799     .                                                                    
018800     SKIP3                                                                
018801 IMS-CHECKPOINT SECTION.                                                  
018802     SKIP2                                                                
018803     MOVE SPACE                  TO CHKP-MSG-IO-AREA                      
018804     MOVE '  XD'                 TO GOOD-STATUSCODES                      
018805     CALL CBLTDLI             USING CHKP                                  
018806                                    MSG-PCB                               
018807                                    CHKP-MSG-IO-AREA-LENGTH               
018808                                    CHKP-MSG-IO-AREA                      
018809                                    CHKP-AREA-LENGTH                      
018810                                    CHKP-AREA                             
018811     MOVE MSG-STATUS-CODE        TO STATUS-WS                             
018812     PERFORM IMS-STATUSCHECK                                              
018813                                                                          
018814     IF IMS-NOT-OK                                                        
018815       MOVE 'IMS CONTROL REGION IS NOT ACCESSIBLE'                        
018816                                 TO ERROR-TEXT-STR                        
018817       DISPLAY ERROR-TEXT                                                 
018818       CALL FELLOG                                                        
018819     END-IF                                                               
018820     .                                                                    
018821     EJECT                                                                
018822 IMS-STATUSCHECK SECTION.                                                 
018823     SKIP2                                                                
018824     SET STATUS-IX               TO 1                                     
018825     SEARCH GOOD-STATUS                                                   
018826       AT END                                                             
018827         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
018828             DELIMITED BY SIZE INTO ERROR-TEXT                            
018829         DISPLAY ERROR-TEXT                                               
018830         CALL FELLOG                                                      
018831       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
018832         CONTINUE                                                         
018833     END-SEARCH                                                           
018834     .                                                                    
018835*    -COPY WY2000P1                                                       
018840     EJECT                                                                
