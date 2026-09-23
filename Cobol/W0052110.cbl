000010 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W0052110.                                                
000300 AUTHOR.         SARASWATHY S.                                            
000400 DATE-WRITTEN.   16/11/28.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        WEB RESPONSE TIME LOG                                            
000900*                                                                         
001000*        THE PROGRAM READS   TABLE TP0WLOG                                
001100*                                                                         
001200*    INDATA.                                                              
001300*        TRANSACTION: W0T521                                              
001400*        REQ:         W00521I1                                            
001500*                                                                         
001600*    OUTDATA.                                                             
001700*        RESP:        W00521O1                                            
001800                                                                          
001900     SKIP3                                                                
002000 ENVIRONMENT DIVISION.                                                    
002100                                                                          
002200 DATA DIVISION.                                                           
002300     EJECT                                                                
002400 WORKING-STORAGE SECTION.                                                 
002500 77  IDPGM                       PIC X(08)   VALUE 'W0052110'.            
002600                                                                          
002700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
002800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
002900                                                                          
003000 77  YES                         PIC X      VALUE 'J'.                    
003100 77  NOO                         PIC X      VALUE 'N'.                    
003200                                                                          
003300*    --- INDEX FOR SCROLL LINES                                           
003400 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
003500 77  MAX-INDX                    PIC S9(4)  VALUE +14   COMP SYNC.        
003600 77  WS-MAX-LINES                PIC S9(4)  VALUE +500  COMP-3.           
003610 77  WS-IX                       PIC S9(4)  VALUE ZEROS COMP-3.           
003610 77  W-IX                        PIC S9(4)  VALUE ZEROS COMP-3.           
003620 77  CURRENT-SECTION             PIC X(32)  VALUE SPACE.                  
003640 77  WS-FLSCR                    PIC X      VALUE 'Y'.                    
003650     88  FL-OK                              VALUE 'Y'.                    
003660     88  FL-WRONG                           VALUE 'N'.                    
003700*    --- WORK FIELDS FOR ACTUAL KEYVALUES OF SCREEN                       
003800                                                                          
003900 77  KEYS-SW                     PIC X      VALUE SPACES.                 
004000     88  KEYS-OK                            VALUE 'Y'.                    
004100     88  KEYS-WRONG                         VALUE 'N'.                    
004200*                                                                         
004300 01  MESSAGE-CODES.                                                       
004400     03  ERROR-CODES.                                                     
004500         05  ERR-INVALID-KEY         PIC X(3)   VALUE '022'.              
004700         05  ERR-LINES-NOT-FOUND     PIC X(3)   VALUE '025'.              
004800         05  ERR-TOO-MANY-LINES      PIC X(3)   VALUE '028'.              
004900         05  ERR-SYSTEM-ERROR        PIC X(3)   VALUE '099'.              
005000         05  INF-FIRST-PAGE          PIC X(3)   VALUE '010'.              
005300         05  INF-LAST-PAGE           PIC X(3)   VALUE '012'.              
005310         05  INF-MORE-LINES          PIC X(3)   VALUE '011'.              
005400*                                                                         
005500*    --- WORK FIELDS FOR  CONSTUCTING DATE AND TIME                       
005700 01  WS-LENGTH-TIREGDAT          PIC 9(4)    COMP-3 VALUE ZERO.           
005710 01  WS-TIREGDAT-LENGTH          PIC 9(4)    COMP-3 VALUE ZERO.           
005720 01  WS-DATE-LENGTH              PIC 9(4)    COMP-3 VALUE ZERO.           
005800 01  WS-TIREGTID                 PIC S9(7)   COMP-3 VALUE ZERO.           
005900 01  WS-LENGTH-TIREGTID          PIC 9(4)    COMP-3 VALUE ZERO.           
005910 01  WS-TIREGTID-LENGTH          PIC 9(4)    COMP-3 VALUE ZERO.           
005920 01  WS-TIME-LENGTH              PIC 9(4)    COMP-3 VALUE ZERO.           
006100 01  WS-AVG-TP0WLOG              PIC S9(9)   COMP-3 VALUE ZERO.           
006100 01  W-AVG-TP0WLOG               PIC S9(9)   COMP-3 VALUE ZERO.           
006200 01  WS-TODAYS-DATE              PIC S9(7)   COMP-3 VALUE ZERO.           
006300                                                                          
006400*    --- WORK FIELDS COUNTING SPACES                                      
006500 01  NSPACE                      PIC 9(4).                                
006510 01  NSPACE1                     PIC 9(4).                                
006600 01  WORK-KEY-FIELDS.                                                     
006700     03 WS-FLSORT-KEY        PIC X(1)   VALUE SPACE.                      
006800*    --- SEARCHING KEY FIELDS                                             
006900 01  W-IDDC-KEY-X.                                                        
007000     03 W-IDDC-L-AVG-KY      PIC X(2) VALUE SPACES.                       
007000     03 W-IDDC-H-AVG-KY      PIC X(2) VALUE SPACES.                       
007010     03 W-IDDC-LOW-KY        PIC X(2) VALUE SPACES.                       
007010     03 W-IDDC-HIGH-KY       PIC X(2) VALUE SPACES.                       
007200 01  W-TIREGDAT-KY-X.                                                     
007300     03 W-TIREGDAT-L-AVG-KY  PIC S9(7)  COMP-3 VALUE ZERO.                
007301     03 W-TIREGDAT-H-AVG-KY  PIC S9(7)  COMP-3 VALUE ZERO.                
007310     03 W-TIREGDAT-LOW-KY    PIC S9(7)  COMP-3 VALUE ZERO.                
007400     03 W-TIREGDAT-HIGH-KY   PIC S9(7)  COMP-3 VALUE 999999.              
007500 01  W-TIREGTID-KY-X.                                                     
007600     03 W-TIREGTID-L-AVG-KY  PIC S9(7)  COMP-3 VALUE ZERO.                
007601     03 W-TIREGTID-H-AVG-KY  PIC S9(7)  COMP-3 VALUE ZERO.                
007610     03 W-TIREGTID-LOW-KY    PIC S9(7)  COMP-3 VALUE ZERO.                
007700     03 W-TIREGTID-HIGH-KY   PIC S9(7)  COMP-3 VALUE 999999.              
007800 01  W-IDUSER-KY             PIC X(8)   VALUE SPACES.                     
007801 01  W-IDUSER-AVG-KY         PIC X(8)   VALUE SPACES.                     
008100 01  W-KVMILSEC-KY-X.                                                     
008200     03 W-KVMILSEC-L-AVG-KY  PIC S9(7)  COMP-3 VALUE ZERO.                
008201     03 W-KVMILSEC-H-AVG-KY  PIC S9(7)  COMP-3 VALUE 999999.              
008210     03 W-KVMILSEC-LOW-KY    PIC S9(7)  COMP-3 VALUE ZERO.                
008300     03 W-KVMILSEC-HIGH-KY   PIC S9(7)  COMP-3 VALUE 999999.              
008400 01  W-BEWEBSCR-KY           PIC X(50)  VALUE SPACES.                     
008401 01  W-BEWEBSCR-AVG-KY       PIC X(50)  VALUE SPACES.                     
       01  W-IDDC                  PIC X(2)   VALUE SPACES.                     
       01  W-MAXMILSEC             PIC S9(7)  COMP-3 VALUE ZERO.                
       01  W-MAXDATE               PIC S9(7)  COMP-3 VALUE ZERO.                
       01  W-MAXTIME               PIC S9(7)  COMP-3 VALUE ZERO.                
       01  W-KVMILSEC.                                                          
           03 WS-KVMILSEC PIC S9(7)  COMP-3 VALUE ZERO OCCURS 500.              
008700*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008800 01  GENERAL-SUBPROGRAMS.                                                 
008900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008910     03  WL01MCNV                PIC X(8)    VALUE 'WL01MCNV'.            
009000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009200*    --- PARAMETRAR TILL SUBPROGRAM WL01MCNV                              
009300*01 -COPY WL01MCNV                                                        
009400     SKIP3                                                                
009500*                                                                         
009600*01 -COPY WWDC99                                                          
009700     SKIP3                                                                
009800*                                                                         
009900 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010000 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010100 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010200 77  RKOD-ABEND-DB2              PIC S9(4)   COMP VALUE +998.             
010300*                                                                         
010400 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
010500       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010600                                                                          
010700 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
010800 01  DB2-WS.                                                              
010900     03  SQLCODE-WS              PIC 9(3)    VALUE ZERO.                  
011000         88  CURSOR-OK                       VALUE 000.                   
011100         88  LINES-FOUND                     VALUE 000.                   
011200         88  LINES-MISSING                   VALUE 100.                   
011300         88  NULL-VALUE                      VALUE 305.                   
011400     03  GOOD-SQLCODECODES.                                               
011500         05  GOOD-SQLCODE OCCURS 5                                        
011600             INDEXED BY SQLCODE-IX PIC 9(3).                              
011700     EJECT                                                                
011800 01  FILLER                      PIC X(16)  VALUE 'TP0WLOG-AREA'.         
011900                                                                          
012000*01  -COPY TP0WLOG -PRE TP0WLOG-                                          
012100     EJECT                                                                
012200     EXEC SQL INCLUDE TP0WLOG END-EXEC.                                   
012300     EJECT                                                                
012400 LINKAGE SECTION.                                                         
012600 01  REQU-AREA.                                                           
012700*    03 -COPY WZ01REQU                                                    
012800*    03 -COPY W00521I1                                                    
012900*    --- RESP AREA                                                        
013100 01  RESP-AREA.                                                           
013200*    03 -COPY WZ01RESP                                                    
013300*    03 -COPY W00521O1                                                    
013510 01  MAX-KVRADER                 PIC S9(4)  COMP SYNC.                    
013700     EJECT                                                                
013800 PROCEDURE DIVISION  USING REQU-AREA RESP-AREA MAX-KVRADER.               
014000 MAIN SECTION.                                                            
014100     ENTRY 'DLITCBL' USING REQU-AREA RESP-AREA MAX-KVRADER.               
014300       PERFORM A-INIT                                                     
014400       PERFORM B-CHECK-KEYS                                               
014500       IF KEYS-OK                                                         
014600           IF REQU-FIRST                                                  
014700             PERFORM C-FIRST-PAGE                                         
014800           ELSE                                                           
014900             IF REQU-NEXT                                                 
015000               PERFORM D-NEXT-PAGE                                        
015100             ELSE                                                         
015200               IF REQU-PREVIOUS                                           
015300                 PERFORM I-PREV-PAGE                                      
015400               ELSE                                                       
015500                 IF REQU-PRINT                                            
015501                   PERFORM J-LATEST-PAGE                                  
015502                 ELSE                                                     
015503                   PERFORM E-SAME-PAGE                                    
015504                 END-IF                                                   
015600               END-IF                                                     
015700             END-IF                                                       
015800           END-IF                                                         
015900         PERFORM F-READ-SHOW-INFO                                         
016000       END-IF                                                             
016200     GOBACK                                                               
016300     .                                                                    
016400     EJECT                                                                
016500                                                                          
016600 A-INIT SECTION.                                                          
016700                                                                          
016710     MOVE 'A-INIT'               TO CURRENT-SECTION                       
016800     INITIALIZE GOOD-SQLCODECODES                                         
016900     MOVE ALL '+'                TO RESP-W00521O1                         
017000                                                                          
017100     MOVE 001                    TO RESP-IDMSGVER                         
017200     MOVE SPACE                  TO RESP-IDMSG-ERROR                      
017300                                    RESP-IDMSG-INFO                       
017400                                    RESP-IDELMT-ERROR                     
017410     MOVE SPACE                  TO RESP-IDDC-START                       
017420                                    RESP-IDUSER-START                     
017430                                    RESP-IDDC-NEXT                        
017440                                    RESP-IDUSER-NEXT                      
017450     MOVE ZERO                   TO RESP-TIREGDAT-START                   
017460                                    RESP-TIREGTID-START                   
017470                                    RESP-KVMILSEC-START                   
017480                                    RESP-TIREGDAT-NEXT                    
017490                                    RESP-TIREGTID-NEXT                    
017491                                    RESP-KVMILSEC-NEXT                    
017500     MOVE REQU-KVRADER           TO RESP-KVRADER                          
017510     MOVE FUNCTION CURRENT-DATE(3:6)  TO WS-TODAYS-DATE                   
           IF (REQU-PRINT OR REQU-PF4FLAG = 'Y') AND                            
              (REQU-IDMSGVER = '001')                                           
              MOVE SPACES     TO REQU-IDDC-START                                
                                 REQU-IDUSER-START                              
                                 REQU-IDDC-KEY                                  
                                 REQU-TIREGDAT-KEY                              
                                 REQU-TIREGTID-KEY                              
                                 REQU-IDUSER-KEY                                
              MOVE ZEROS      TO REQU-TIREGDAT-START                            
                                 REQU-TIREGTID-START                            
                                 REQU-KVMILSEC-START                            
           END-IF                                                               
017600     .                                                                    
017700     EJECT                                                                
017800*                                                                         
017900 B-CHECK-KEYS SECTION.                                                    
018000*                                                                         
018007     MOVE 'B-CHECK-KEYS'        TO CURRENT-SECTION                        
018100     MOVE 'Y' TO KEYS-SW                                                  
018200*IDDC VALIDATION                                                          
018210     IF REQU-IDDC-KEY = SPACES                                            
018220        MOVE LOW-VALUES        TO W-IDDC-LOW-KY                           
018230                                  W-IDDC-L-AVG-KY                         
018230        MOVE HIGH-VALUES       TO W-IDDC-HIGH-KY                          
018230                                  W-IDDC-H-AVG-KY                         
018240     ELSE                                                                 
018250      IF REQU-IDDC-KEY = ALL '+'                                          
018220        MOVE LOW-VALUES        TO W-IDDC-LOW-KY                           
018230                                  W-IDDC-L-AVG-KY                         
018230        MOVE HIGH-VALUES       TO W-IDDC-HIGH-KY                          
018230                                  W-IDDC-H-AVG-KY                         
018280      ELSE                                                                
018290        IF REQU-PRINT                                                     
018220          MOVE LOW-VALUES      TO W-IDDC-LOW-KY                           
018230                                  W-IDDC-L-AVG-KY                         
018230          MOVE HIGH-VALUES     TO W-IDDC-HIGH-KY                          
018230                                  W-IDDC-H-AVG-KY                         
018295        ELSE                                                              
018296          MOVE REQU-IDDC-KEY         TO WS-IDDC                           
018297          IF (GOOD-DC AND REQU-IDDC-KEY > SPACES)                         
018318           MOVE REQU-IDDC-KEY  TO W-IDDC-LOW-KY                           
018319                                  W-IDDC-HIGH-KY                          
018319                                  W-IDDC-L-AVG-KY                         
018319                                  W-IDDC-H-AVG-KY                         
018322          ELSE                                                            
018323           MOVE 'IDDC'         TO RESP-IDELMT-ERROR                       
018324           MOVE 'N'            TO KEYS-SW                                 
018325          END-IF                                                          
018326        END-IF                                                            
018327      END-IF                                                              
018330     END-IF                                                               
020700*TIREGDAT VALIDATION                                                      
020800     IF REQU-TIREGDAT-KEY = ALL '+' OR SPACES OR ZEROS                    
020911       MOVE ZEROS  TO W-TIREGDAT-LOW-KY                                   
020912                      W-TIREGDAT-L-AVG-KY                                 
020913       MOVE 991231 TO W-TIREGDAT-HIGH-KY                                  
020914                      W-TIREGDAT-H-AVG-KY                                 
021310     ELSE                                                                 
021410       IF REQU-TIREGDAT-KEY > SPACES                                      
021620         MOVE ZERO TO WS-LENGTH-TIREGDAT                                  
021630                      WS-DATE-LENGTH                                      
021640                      WS-TIREGDAT-LENGTH                                  
021801         INSPECT FUNCTION REVERSE (REQU-TIREGDAT-KEY)                     
021802           TALLYING WS-LENGTH-TIREGDAT FOR LEADING SPACES                 
021803         COMPUTE WS-DATE-LENGTH = LENGTH OF REQU-TIREGDAT-KEY             
021804                                    - WS-LENGTH-TIREGDAT                  
021807         MOVE ZEROS TO W-TIREGDAT-LOW-KY                                  
021808                       W-TIREGDAT-HIGH-KY                                 
021809*        CHECK IF DATE IS NUMERIC                                         
021810         IF REQU-TIREGDAT-KEY(1:WS-DATE-LENGTH) IS NUMERIC                
021820          MOVE REQU-TIREGDAT-KEY(1:WS-DATE-LENGTH)                        
021830                                   TO W-TIREGDAT-LOW-KY                   
021840                                      W-TIREGDAT-HIGH-KY                  
021850          COMPUTE WS-TIREGDAT-LENGTH = 6 - WS-DATE-LENGTH                 
021860          PERFORM WS-TIREGDAT-LENGTH TIMES                                
021870           MULTIPLY 10 BY W-TIREGDAT-LOW-KY                               
021880           MULTIPLY 10 BY W-TIREGDAT-HIGH-KY                              
021890           ADD 9     TO W-TIREGDAT-HIGH-KY                                
021893          END-PERFORM                                                     
021894         ELSE                                                             
021895          MOVE 'TIREGDAT'       TO RESP-IDELMT-ERROR                      
021896          MOVE 'N'              TO KEYS-SW                                
021897         END-IF                                                           
021910         MOVE W-TIREGDAT-LOW-KY    TO W-TIREGDAT-L-AVG-KY                 
022000         MOVE W-TIREGDAT-HIGH-KY   TO W-TIREGDAT-H-AVG-KY                 
023900       ELSE                                                               
024000        MOVE 'TIREGDAT'       TO RESP-IDELMT-ERROR                        
024110        MOVE 'N'              TO KEYS-SW                                  
024200       END-IF                                                             
024201     END-IF                                                               
024300*TIREGTID VALIDATION                                                      
024400     IF REQU-TIREGTID-KEY = SPACES OR ALL '+' OR ZEROS                    
024820       MOVE ZEROS  TO W-TIREGTID-LOW-KY                                   
024821                      W-TIREGTID-L-AVG-KY                                 
024830       MOVE 999999 TO W-TIREGTID-HIGH-KY                                  
024831                      W-TIREGTID-H-AVG-KY                                 
024909     ELSE                                                                 
025000       IF REQU-TIREGTID-KEY > SPACES                                      
025100         MOVE ZERO TO WS-LENGTH-TIREGTID                                  
025110                      WS-TIME-LENGTH                                      
025120                      WS-TIREGTID-LENGTH                                  
025301         INSPECT FUNCTION REVERSE (REQU-TIREGTID-KEY)                     
025302           TALLYING WS-LENGTH-TIREGTID FOR LEADING SPACE                  
025305         COMPUTE WS-TIME-LENGTH = LENGTH OF REQU-TIREGTID-KEY             
025306                                    - WS-LENGTH-TIREGTID                  
025309         MOVE ZEROS TO W-TIREGTID-LOW-KY                                  
025310                       W-TIREGTID-HIGH-KY                                 
025311*        CHECK IF TIME IS NUMERIC                                         
025312         IF REQU-TIREGTID-KEY(1:WS-TIME-LENGTH) IS NUMERIC                
025320          MOVE REQU-TIREGTID-KEY(1:WS-TIME-LENGTH)                        
025330                                   TO W-TIREGTID-LOW-KY                   
025340                                      W-TIREGTID-HIGH-KY                  
025350          COMPUTE WS-TIREGTID-LENGTH = 6 - WS-TIME-LENGTH                 
025360          PERFORM WS-TIREGTID-LENGTH TIMES                                
025370           MULTIPLY 10 BY W-TIREGTID-LOW-KY                               
025380           MULTIPLY 10 BY W-TIREGTID-HIGH-KY                              
025390           ADD 9     TO W-TIREGTID-HIGH-KY                                
025391          END-PERFORM                                                     
025392         ELSE                                                             
025393          MOVE 'TIREGTID'       TO RESP-IDELMT-ERROR                      
025394          MOVE 'N'              TO KEYS-SW                                
025395         END-IF                                                           
025399         MOVE W-TIREGTID-LOW-KY    TO W-TIREGTID-L-AVG-KY                 
025400         MOVE W-TIREGTID-HIGH-KY   TO W-TIREGTID-H-AVG-KY                 
027300       ELSE                                                               
027400         MOVE 'TIREGTID'       TO RESP-IDELMT-ERROR                       
027500         MOVE 'N'              TO KEYS-SW                                 
027600       END-IF                                                             
027601     END-IF                                                               
027700*IDUSER VALIDATION                                                        
027800     IF REQU-IDUSER-KEY = SPACES                                          
027900        MOVE ALL '%'               TO W-IDUSER-KY                         
027901                                      W-IDUSER-AVG-KY                     
027920     ELSE                                                                 
027930        IF REQU-IDUSER-KEY = ALL '+'                                      
027940          MOVE ALL '%'             TO W-IDUSER-KY                         
027941                                      W-IDUSER-AVG-KY                     
028000        ELSE                                                              
028100         IF REQU-IDUSER-KEY > SPACES                                      
028300          MOVE ZERO                TO NSPACE                              
028400          INSPECT FUNCTION REVERSE (REQU-IDUSER-KEY)                      
028500           TALLYING NSPACE FOR LEADING SPACE                              
028600          COMPUTE NSPACE = LENGTH OF REQU-IDUSER-KEY - NSPACE + 1         
028700                                                                          
028701           IF NSPACE < LENGTH OF REQU-IDUSER-KEY                          
028702             INSPECT REQU-IDUSER-KEY (NSPACE:)                            
028703              CONVERTING SPACE TO '%'                                     
028704             MOVE REQU-IDUSER-KEY TO W-IDUSER-KY                          
028761           ELSE                                                           
028762             MOVE REQU-IDUSER-KEY TO W-IDUSER-KY                          
028770           END-IF                                                         
028780          MOVE W-IDUSER-KY        TO W-IDUSER-AVG-KY                      
029500         ELSE                                                             
029600          MOVE 'IDUSER'           TO RESP-IDELMT-ERROR                    
029800          MOVE 'N'                TO KEYS-SW                              
029900         END-IF                                                           
029910        END-IF                                                            
030000     END-IF                                                               
030100*FLSORT - KVMILSEC VALIDATION                                             
030200      IF REQU-FLSORT-KEY = ALL '+'                                        
030300        CONTINUE                                                          
030400      ELSE                                                                
030500        IF REQU-FLSORT-KEY = 'Y' OR 'J' OR 'D' OR 'N' OR SPACES           
030600           MOVE REQU-FLSORT-KEY    TO WS-FLSORT-KEY                       
030800        ELSE                                                              
030900           MOVE 'RESPONSE TIME'    TO RESP-IDELMT-ERROR                   
031010           MOVE 'N'                TO KEYS-SW                             
031100        END-IF                                                            
031200      END-IF                                                              
031300*BEWEBSCR VALIDATION                                                      
031400      IF REQU-BEWEBSCR-KEY = SPACES                                       
031410        MOVE ALL '%'               TO REQU-BEWEBSCR-KEY                   
031411                                      W-BEWEBSCR-KY                       
031412                                      W-BEWEBSCR-AVG-KY                   
031414      ELSE                                                                
031420       IF REQU-BEWEBSCR-KEY = ALL '+'                                     
031440        MOVE ALL '%'               TO REQU-BEWEBSCR-KEY                   
031441                                      W-BEWEBSCR-KY                       
031442                                      W-BEWEBSCR-AVG-KY                   
031600       ELSE                                                               
031700        IF REQU-BEWEBSCR-KEY > SPACES                                     
031800           MOVE ZERO TO NSPACE                                            
031810                        NSPACE1                                           
031900           INSPECT FUNCTION REVERSE (REQU-BEWEBSCR-KEY)                   
032000            TALLYING NSPACE FOR LEADING SPACE                             
032010           MOVE ZERO TO NSPACE1                                           
032100           COMPUTE NSPACE1 = LENGTH OF REQU-BEWEBSCR-KEY                  
032200                                      - NSPACE + 1                        
032201           IF NSPACE1 <= LENGTH OF REQU-BEWEBSCR-KEY                      
032202             INSPECT REQU-BEWEBSCR-KEY (NSPACE1:)                         
032203              CONVERTING SPACE TO '%'                                     
032204           END-IF                                                         
032210          STRING '%' DELIMITED BY SIZE                                    
032211                 REQU-BEWEBSCR-KEY DELIMITED NSPACE                       
032230                 INTO W-BEWEBSCR-KY                                       
032240          END-STRING                                                      
032241          MOVE W-BEWEBSCR-KY TO W-BEWEBSCR-AVG-KY                         
033000        ELSE                                                              
033100           MOVE 'BEWEBSCR'         TO RESP-IDELMT-ERROR                   
033300           MOVE 'N'                TO KEYS-SW                             
033400        END-IF                                                            
033410       END-IF                                                             
033500      END-IF                                                              
033502     IF KEYS-SW = 'N'                                                     
033504        MOVE ERR-INVALID-KEY    TO RESP-IDMSG-ERROR                       
033505        MOVE ZEROS              TO RESP-KVMILSEC-AVG                      
033506        MOVE ZEROS              TO RESP-KVRADER                           
033507     END-IF                                                               
033600     .                                                                    
033700     EJECT                                                                
033800 C-FIRST-PAGE SECTION.                                                    
033900                                                                          
033910     MOVE 'C-FIRST-PAGE'         TO CURRENT-SECTION                       
033920     MOVE INF-FIRST-PAGE         TO RESP-IDMSG-ERROR                      
034000     CONTINUE                                                             
034100     .                                                                    
034200     EJECT                                                                
034300 D-NEXT-PAGE SECTION.                                                     
034400                                                                          
034500     MOVE 'D-NEXT-PAGE'             TO CURRENT-SECTION                    
034510     MOVE 'Y'                       TO WS-FLSCR                           
           IF REQU-PRINT OR REQU-PF4FLAG = 'Y'                                  
              MOVE REQU-IDDC-START        TO W-IDDC-LOW-KY                      
              MOVE HIGH-VALUES            TO W-IDDC-HIGH-KY                     
           END-IF                                                               
034601     IF REQU-FLSORT-KEY = 'Y' OR 'J' OR 'D'                               
034602        IF REQU-KVMILSEC-START = ALL '+'                                  
034603           MOVE 999999              TO W-KVMILSEC-HIGH-KY                 
034604        ELSE                                                              
034605           IF (REQU-KVMILSEC-START IS NUMERIC) AND                        
034606              (REQU-PF4FLAG = 'N')                                        
034607             MOVE REQU-KVMILSEC-START TO W-KVMILSEC-HIGH-KY               
034608           ELSE                                                           
034609             MOVE 999999              TO W-KVMILSEC-HIGH-KY               
034610           END-IF                                                         
034611        END-IF                                                            
034613     ELSE                                                                 
034614      IF REQU-FLSORT-KEY = 'N' OR SPACES                                  
034615        IF REQU-TIREGDAT-START = ALL '+'                                  
034620           CONTINUE                                                       
034630        ELSE                                                              
034640           IF (REQU-TIREGDAT-START IS NUMERIC) AND                        
034650              (REQU-PF4FLAG = 'N')                                        
034660             MOVE REQU-TIREGDAT-START TO W-TIREGDAT-HIGH-KY               
034670           ELSE                                                           
034680             MOVE 991231              TO W-TIREGDAT-HIGH-KY               
034690           END-IF                                                         
034710        END-IF                                                            
034720      END-IF                                                              
034730     END-IF                                                               
035100     .                                                                    
035200     EJECT                                                                
035300 E-SAME-PAGE SECTION.                                                     
035400     MOVE 'E-SAME-PAGE'             TO CURRENT-SECTION                    
035401     MOVE 'Y'                       TO WS-FLSCR                           
           IF REQU-PRINT OR REQU-PF4FLAG = 'Y'                                  
              MOVE REQU-IDDC-START        TO W-IDDC-LOW-KY                      
              MOVE HIGH-VALUES            TO W-IDDC-HIGH-KY                     
           END-IF                                                               
035404     IF (REQU-QUERY AND REQU-IDMSGVER = '101')                            
035405       IF REQU-FLSORT-KEY = 'Y' OR 'J' OR 'D'                             
035406        IF REQU-KVMILSEC-START = ALL '+'                                  
035413           MOVE 999999              TO W-KVMILSEC-HIGH-KY                 
035414        ELSE                                                              
035415           IF (REQU-KVMILSEC-START IS NUMERIC) AND                        
035416              (REQU-PF4FLAG = 'N')                                        
035417             MOVE REQU-KVMILSEC-START TO W-KVMILSEC-HIGH-KY               
035418           ELSE                                                           
035419             MOVE 999999              TO W-KVMILSEC-HIGH-KY               
035420           END-IF                                                         
035421        END-IF                                                            
035422       ELSE                                                               
035423        IF REQU-FLSORT-KEY = 'N' OR SPACES                                
035424         IF REQU-TIREGDAT-START = ALL '+'                                 
035425           CONTINUE                                                       
035426         ELSE                                                             
035427           IF (REQU-TIREGDAT-START IS NUMERIC) AND                        
035428              (REQU-PF4FLAG = 'N')                                        
035429              MOVE REQU-TIREGDAT-START TO W-TIREGDAT-HIGH-KY              
 35430           ELSE                                                           
035431              MOVE 991231              TO W-TIREGDAT-HIGH-KY              
035432           END-IF                                                         
035433         END-IF                                                           
035434        END-IF                                                            
035435       END-IF                                                             
035440     END-IF                                                               
035450     IF (REQU-QUERY AND REQU-IDMSGVER = '001') AND                        
035460          (REQU-FLSORT-KEY = 'Y' OR 'J' OR 'D')                           
035480           MOVE 999999              TO W-KVMILSEC-HIGH-KY                 
035990     END-IF                                                               
036000     .                                                                    
036100     EJECT                                                                
036200 I-PREV-PAGE SECTION.                                                     
036300     MOVE 'I-PREV-PAGE'             TO CURRENT-SECTION                    
036310     MOVE 'Y'                       TO WS-FLSCR                           
           IF REQU-PRINT OR REQU-PF4FLAG = 'Y'                                  
              MOVE REQU-IDDC-START        TO W-IDDC-LOW-KY                      
              MOVE HIGH-VALUES            TO W-IDDC-HIGH-KY                     
           END-IF                                                               
036320     IF REQU-FLSORT-KEY = 'Y' OR 'J' OR 'D'                               
036330        IF REQU-KVMILSEC-START = ALL '+'                                  
036340           MOVE 999999              TO W-KVMILSEC-HIGH-KY                 
036350        ELSE                                                              
036351           IF (REQU-KVMILSEC-START IS NUMERIC) AND                        
036352              (REQU-PF4FLAG = 'N')                                        
036353             MOVE REQU-KVMILSEC-START TO W-KVMILSEC-HIGH-KY               
036354           ELSE                                                           
036355             MOVE 999999              TO W-KVMILSEC-HIGH-KY               
036356           END-IF                                                         
036370        END-IF                                                            
036380     ELSE                                                                 
036390      IF REQU-FLSORT-KEY = 'N' OR SPACES                                  
036391        IF REQU-TIREGDAT-START = ALL '+'                                  
036393           CONTINUE                                                       
036394        ELSE                                                              
036395           IF REQU-PF4FLAG = 'Y'                                          
036396              MOVE 991231           TO W-TIREGDAT-HIGH-KY                 
036397           ELSE                                                           
036398              MOVE REQU-TIREGDAT-START TO W-TIREGDAT-HIGH-KY              
036399           END-IF                                                         
036400        END-IF                                                            
036410      END-IF                                                              
036500     END-IF                                                               
036900     .                                                                    
037000     EJECT                                                                
037100*                                                                         
037101 J-LATEST-PAGE SECTION.                                                   
037102     MOVE 'J-LATEST-PAGE'     TO CURRENT-SECTION                          
037104     CONTINUE                                                             
037105     .                                                                    
037106     EJECT                                                                
037200 F-READ-SHOW-INFO SECTION.                                                
037210     MOVE 'F-READ-SHOW-INFO'  TO   CURRENT-SECTION                        
037420     IF WS-FLSORT-KEY = 'Y' OR 'J' OR 'D'                                 
037500       IF REQU-PRINT OR REQU-PF4FLAG = 'Y'                                
037601         MOVE WS-TODAYS-DATE TO W-TIREGDAT-LOW-KY                         
037601                                W-TIREGDAT-L-AVG-KY                       
037602                                W-TIREGDAT-HIGH-KY                        
037602                                W-TIREGDAT-H-AVG-KY                       
037610         PERFORM DB2-DCL-OPN-TP0WLOG-DISTDC                               
037611         PERFORM DB2-FETCH-TP0WLOG-DISTDC                                 
037612         MOVE ZERO                   TO WS-IX                             
037613         PERFORM UNTIL LINES-MISSING OR WS-IX = MAX-KVRADER               
037614           PERFORM DB2-DCL-OPN-TP0WLOG-MAXMILSEC                          
037620           PERFORM DB2-FETCH-TP0WLOG-MAXMILSEC                            
037640           PERFORM DB2-DCL-OPN-TP0WLOG-MAXSEC                             
037620           PERFORM DB2-FETCH-TP0WLOG-MAXSEC                               
037660           PERFORM FA-CHECK-TO-MOVE                                       
037698           PERFORM DB2-CLOSE-TP0WLOG-MAXSEC                               
037661           PERFORM DB2-CLOSE-TP0WLOG-MAXMILSEC                            
037670           PERFORM DB2-FETCH-TP0WLOG-DISTDC                               
037680         END-PERFORM                                                      
037690         IF LINES-FOUND                                                   
037692           PERFORM FA-MOVE-TO-RESP-NEXT-DC                                
037693         ELSE                                                             
037694           IF WS-IX > 0 AND LINES-MISSING                                 
037695             PERFORM FA-START-TO-RESP-NEXT                                
037696           END-IF                                                         
037697         END-IF                                                           
037698         PERFORM DB2-CLOSE-TP0WLOG-DISTDC                                 
037699         IF WS-IX > 0 AND REQU-IDMSGVER = '101'                           
037700          PERFORM DB2-AVG-TP0WLOG-PF4                                     
037710          MOVE W-AVG-TP0WLOG        TO RESP-KVMILSEC-AVG                  
037720         END-IF                                                           
037730       ELSE                                                               
037740         PERFORM DB2-DCL-OPN-TP0WLOG-FCRS                                 
037750         PERFORM DB2-FETCH-TP0WLOG-FCRS                                   
037760         MOVE ZERO                    TO WS-IX                            
037770                                                                          
037780         PERFORM UNTIL LINES-MISSING OR WS-IX = MAX-KVRADER               
037790          PERFORM FA-CHECK-TO-MOVE                                        
037791          PERFORM DB2-FETCH-TP0WLOG-FCRS                                  
037792         END-PERFORM                                                      
037793         IF LINES-FOUND                                                   
037795          PERFORM FA-MOVE-TO-RESP-NEXT                                    
037796         ELSE                                                             
037797          IF WS-IX > 0 AND LINES-MISSING                                  
037798            PERFORM FA-START-TO-RESP-NEXT                                 
037799          END-IF                                                          
037800         END-IF                                                           
037810         PERFORM DB2-CLOSE-TP0WLOG-FCRS                                   
037699         IF WS-IX > 0 AND REQU-IDMSGVER = '101'                           
037830          PERFORM DB2-AVG-TP0WLOG                                         
037840          MOVE WS-AVG-TP0WLOG        TO RESP-KVMILSEC-AVG                 
037850         END-IF                                                           
037860       END-IF                                                             
037900     ELSE                                                                 
038000       IF (WS-FLSORT-KEY = SPACES OR ALL '+' OR 'N')                      
038010        IF REQU-PRINT OR REQU-PF4FLAG = 'Y'                               
037610         PERFORM DB2-DCL-OPN-TP0WLOG-DISTDC                               
037611         PERFORM DB2-FETCH-TP0WLOG-DISTDC                                 
037612         MOVE ZERO    TO WS-IX                                            
037613         PERFORM UNTIL LINES-MISSING OR WS-IX = MAX-KVRADER               
037614           PERFORM DB2-DCL-OPN-TP0WLOG-MAXDATTIM                          
037620           PERFORM DB2-FETCH-TP0WLOG-MAXDATTIM                            
037640           PERFORM DB2-DCL-OPN-TP0WLOG-LATEST                             
037620           PERFORM DB2-FETCH-TP0WLOG-LATEST                               
037660           PERFORM FA-CHECK-TO-MOVE                                       
037698           PERFORM DB2-CLOSE-TP0WLOG-LATEST                               
037661           PERFORM DB2-CLOSE-TP0WLOG-MAXDATTIM                            
037670           PERFORM DB2-FETCH-TP0WLOG-DISTDC                               
037680         END-PERFORM                                                      
037690         IF LINES-FOUND                                                   
037692           PERFORM FA-MOVE-TO-RESP-NEXT-DC                                
037693         ELSE                                                             
037694           IF WS-IX > 0 AND LINES-MISSING                                 
037695             PERFORM FA-START-TO-RESP-NEXT                                
037696           END-IF                                                         
037697         END-IF                                                           
037698         PERFORM DB2-CLOSE-TP0WLOG-DISTDC                                 
037699         IF WS-IX > 0 AND REQU-IDMSGVER = '101'                           
037700          PERFORM DB2-AVG-TP0WLOG-PF4                                     
039502          MOVE W-AVG-TP0WLOG TO RESP-KVMILSEC-AVG                         
039510         END-IF                                                           
039511        ELSE                                                              
039902           PERFORM DB2-DCL-OPN-TP0WLOG-CRS                                
040000           PERFORM DB2-FETCH-TP0WLOG-CRS                                  
040702           MOVE ZERO                   TO WS-IX                           
040703                                                                          
040704           PERFORM UNTIL LINES-MISSING OR WS-IX = MAX-KVRADER             
040705            PERFORM FA-CHECK-TO-MOVE                                      
040707            PERFORM DB2-FETCH-TP0WLOG-CRS                                 
040708           END-PERFORM                                                    
040709           IF LINES-FOUND                                                 
040711             PERFORM FA-MOVE-TO-RESP-NEXT                                 
040712           ELSE                                                           
040713            IF WS-IX > 0 AND LINES-MISSING                                
040714             PERFORM FA-START-TO-RESP-NEXT                                
040715            END-IF                                                        
040716           END-IF                                                         
040717           PERFORM DB2-CLOSE-TP0WLOG-CRS                                  
037699           IF WS-IX > 0 AND REQU-IDMSGVER = '101'                         
040800             PERFORM DB2-AVG-TP0WLOG                                      
040830             MOVE WS-AVG-TP0WLOG       TO RESP-KVMILSEC-AVG               
040840           END-IF                                                         
040850        END-IF                                                            
041000      END-IF                                                              
041100     END-IF                                                               
041200                                                                          
041300     MOVE WS-IX  TO RESP-KVRADER                                          
041310     IF WS-IX = ZERO                                                      
041320       MOVE ERR-LINES-NOT-FOUND        TO RESP-IDMSG-ERROR                
041330       MOVE 'KEY'                      TO RESP-IDELMT-ERROR               
041340     ELSE                                                                 
041350       IF WS-IX > WS-MAX-LINES                                            
041360         MOVE ERR-TOO-MANY-LINES       TO RESP-IDMSG-ERROR                
041380       END-IF                                                             
041390     END-IF                                                               
041400     .                                                                    
041500     EJECT                                                                
041600*                                                                         
041601 FA-MOVE-TO-RESP-NEXT SECTION.                                            
041602     MOVE 'FA-MOVE-TO-RESP-NEXT' TO CURRENT-SECTION                       
041603                                                                          
041604     MOVE IDDC                   TO RESP-IDDC-NEXT                        
041605     MOVE TIREGDAT               TO RESP-TIREGDAT-NEXT                    
041606     MOVE TIREGTID               TO RESP-TIREGTID-NEXT                    
041607     MOVE IDUSER                 TO RESP-IDUSER-NEXT                      
041608     MOVE KVMILSEC               TO RESP-KVMILSEC-NEXT                    
041609     MOVE INF-MORE-LINES         TO RESP-IDMSG-INFO                       
041617     .                                                                    
041620     EJECT                                                                
041630*                                                                         
041601 FA-MOVE-TO-RESP-NEXT-DC SECTION.                                         
041602     MOVE 'FA-MOVE-TO-RESP-NEXT-DC' TO CURRENT-SECTION                    
041603                                                                          
041604     MOVE IDDC                   TO RESP-IDDC-NEXT                        
041605     MOVE ZEROS                  TO RESP-TIREGDAT-NEXT                    
041606     MOVE ZEROS                  TO RESP-TIREGTID-NEXT                    
041607     MOVE SPACES                 TO RESP-IDUSER-NEXT                      
041608     MOVE ZEROS                  TO RESP-KVMILSEC-NEXT                    
041609     MOVE INF-MORE-LINES         TO RESP-IDMSG-INFO                       
041617     .                                                                    
041620     EJECT                                                                
041630*                                                                         
041631 FA-START-TO-RESP-NEXT SECTION.                                           
041632     MOVE 'FA-START-TO-RESP-NEXT' TO CURRENT-SECTION                      
041633                                                                          
041634     MOVE INF-LAST-PAGE          TO RESP-IDMSG-INFO                       
041635     MOVE RESP-IDDC-START        TO RESP-IDDC-NEXT                        
041636     MOVE RESP-TIREGDAT-START    TO RESP-TIREGDAT-NEXT                    
041637     MOVE RESP-TIREGTID-START    TO RESP-TIREGTID-NEXT                    
041638     MOVE RESP-IDUSER-START      TO RESP-IDUSER-NEXT                      
041639     MOVE RESP-KVMILSEC-START    TO RESP-KVMILSEC-NEXT                    
041640     .                                                                    
041650     EJECT                                                                
041651*                                                                         
041652*** - CHECK TO MOVE.                                                      
041653 FA-CHECK-TO-MOVE SECTION.                                                
041654                                                                          
041658     IF REQU-FIRST OR (REQU-QUERY AND REQU-IDMSGVER = '001')              
041659                   OR WS-FLSCR = 'N'                                      
041660                   OR REQU-PRINT                                          
041662        PERFORM FA-MOVE-TO-RESPOND                                        
041664     ELSE                                                                 
041665        IF IDDC = REQU-IDDC-START AND                                     
041666           TIREGDAT = REQU-TIREGDAT-START AND                             
041667           TIREGTID = REQU-TIREGTID-START AND                             
041668           IDUSER = REQU-IDUSER-START AND                                 
041669           WS-FLSCR = 'Y'                                                 
041670             MOVE 'N' TO WS-FLSCR                                         
041671             PERFORM FA-MOVE-TO-RESPOND                                   
              ELSE                                                              
                 IF IDDC = REQU-IDDC-START AND                                  
                    WS-FLSCR = 'Y' AND                                          
                    REQU-PF4FLAG = 'Y'                                          
                    MOVE 'N' TO WS-FLSCR                                        
                    PERFORM FA-MOVE-TO-RESPOND                                  
                 END-IF                                                         
041673        END-IF                                                            
041674     END-IF                                                               
041675     .                                                                    
041676*** - MOVE DATA TO RESPOND.                                               
041677 FA-MOVE-TO-RESPOND SECTION.                                              
041678                                                                          
041679     MOVE 'FA-MOVE-TO-RESPOND' TO CURRENT-SECTION                         
041680     ADD 1                     TO WS-IX                                   
041697                                                                          
042201                                                                          
042202     MOVE IDDC         TO RESP-IDDC-LINE(WS-IX)                           
042203     MOVE TIREGDAT     TO RESP-TIREGDAT-LINE(WS-IX)                       
042204     MOVE TIREGTID     TO RESP-TIREGTID-LINE(WS-IX)                       
042205     MOVE IDUSER       TO RESP-IDUSER-LINE(WS-IX)                         
042206     MOVE KVMILSEC     TO RESP-KVMILSEC-LINE(WS-IX)                       
042207     MOVE BEWEBSCR     TO RESP-BEWEBSCR-LINE(WS-IX)                       
042208     MOVE BEWEBURL     TO RESP-BEWEBURL-LINE(WS-IX)                       
042209     MOVE IDLOPNR      TO RESP-IDLOPNR-LINE(WS-IX)                        
042210                                                                          
042211     IF WS-IX = 1                                                         
042212       MOVE IDDC       TO RESP-IDDC-START                                 
042213       MOVE TIREGDAT   TO RESP-TIREGDAT-START                             
042214       MOVE TIREGTID   TO RESP-TIREGTID-START                             
042215       MOVE IDUSER     TO RESP-IDUSER-START                               
042216       MOVE KVMILSEC   TO RESP-KVMILSEC-START                             
042226     END-IF                                                               
042228     .                                                                    
042229                                                                          
042230*                                                                         
044400 DB2-AVG-TP0WLOG-PF4  SECTION.                                            
044520     MOVE 'DB2-AVG-TP0WLOG-PF4' TO CURRENT-SECTION                        
           MOVE ZEROS TO  W-AVG-TP0WLOG                                         
041680     MOVE 1     TO  W-IX                                                  
           PERFORM UNTIL W-IX > WS-IX                                           
             MOVE RESP-KVMILSEC-LINE(W-IX) TO WS-KVMILSEC(W-IX)                 
             COMPUTE W-AVG-TP0WLOG = W-AVG-TP0WLOG + WS-KVMILSEC(W-IX)          
041680     ADD  1     TO  W-IX                                                  
           END-PERFORM                                                          
           COMPUTE W-AVG-TP0WLOG = W-AVG-TP0WLOG / WS-IX                        
046900     .                                                                    
047000     EJECT                                                                
      *                                                                         
044400 DB2-AVG-TP0WLOG SECTION.                                                 
044500                                                                          
044520     MOVE 'DB2-AVG-TP0WLOG' TO CURRENT-SECTION                            
044600     EXEC SQL                                                             
044700       SELECT CEILING(AVG(KVMILSEC))                                      
044800                                                                          
044900       INTO  :WS-AVG-TP0WLOG                                              
045000                                                                          
045100       FROM   TP0WLOG                                                     
045200                                                                          
045300       WHERE  IDDC       BETWEEN :W-IDDC-L-AVG-KY                         
045300                             AND :W-IDDC-H-AVG-KY                         
045500        AND   TIREGDAT   BETWEEN :W-TIREGDAT-L-AVG-KY                     
045600                             AND :W-TIREGDAT-H-AVG-KY                     
045700        AND   TIREGTID   BETWEEN :W-TIREGTID-L-AVG-KY                     
045800                             AND :W-TIREGTID-H-AVG-KY                     
045900        AND   IDUSER     LIKE    :W-IDUSER-AVG-KY                         
046000        AND   KVMILSEC   BETWEEN :W-KVMILSEC-L-AVG-KY                     
046010                             AND :W-KVMILSEC-H-AVG-KY                     
046100        AND   BEWEBSCR   LIKE    :W-BEWEBSCR-AVG-KY                       
046300     END-EXEC                                                             
046400                                                                          
046500     MOVE 000100305  TO GOOD-SQLCODECODES                                 
046600                                                                          
046700     MOVE SQLCODE TO SQLCODE-WS                                           
046800     PERFORM DB2-STATUS-CHECK                                             
046900     .                                                                    
047000     EJECT                                                                
047100 DB2-DCL-OPN-TP0WLOG-FCRS SECTION.                                        
047200                                                                          
047210     MOVE 'DB2-DCL-OPN-TP0WLOG-FCRS' TO CURRENT-SECTION                   
047400                                                                          
047500     EXEC SQL                                                             
047600       DECLARE TP0WLOG-FCRS CURSOR FOR                                    
047700                                                                          
047800       SELECT  IDDC                                                       
047900             , TIREGDAT                                                   
048000             , TIREGTID                                                   
048100             , IDUSER                                                     
048200             , KVMILSEC                                                   
048300             , BEWEBSCR                                                   
048400             , BEWEBURL                                                   
048500             , IDLOPNR                                                    
048600                                                                          
048700       FROM    TP0WLOG                                                    
048800                                                                          
048900       WHERE  IDDC       BETWEEN :W-IDDC-LOW-KY                           
048900                             AND :W-IDDC-HIGH-KY                          
049100        AND   TIREGDAT   BETWEEN :W-TIREGDAT-LOW-KY                       
049200                             AND :W-TIREGDAT-HIGH-KY                      
049300        AND   TIREGTID   BETWEEN :W-TIREGTID-LOW-KY                       
049400                             AND :W-TIREGTID-HIGH-KY                      
049500        AND   IDUSER     LIKE    :W-IDUSER-KY                             
049600        AND   KVMILSEC   BETWEEN :W-KVMILSEC-LOW-KY                       
049610                             AND :W-KVMILSEC-HIGH-KY                      
049700        AND   BEWEBSCR   LIKE    :W-BEWEBSCR-KY                           
050000       ORDER BY KVMILSEC DESC                                             
050100              , TIREGDAT DESC                                             
050200              , TIREGTID DESC                                             
050300     END-EXEC                                                             
050400                                                                          
050500     MOVE 000100305  TO GOOD-SQLCODECODES                                 
050600                                                                          
050700     EXEC SQL                                                             
050800       OPEN TP0WLOG-FCRS                                                  
050900     END-EXEC                                                             
051000                                                                          
051100     MOVE SQLCODE TO SQLCODE-WS                                           
051200     PERFORM DB2-STATUS-CHECK                                             
051300     .                                                                    
051400                                                                          
051500 DB2-FETCH-TP0WLOG-FCRS SECTION.                                          
051600                                                                          
051700     MOVE 'DB2-FETCH-TP0WLOG-FCRS' TO CURRENT-SECTION                     
051800                                                                          
051900     EXEC SQL                                                             
052000       FETCH TP0WLOG-FCRS                                                 
052100                                                                          
052200       INTO :TP0WLOG.IDDC                                                 
052300          , :TP0WLOG.TIREGDAT                                             
052400          , :TP0WLOG.TIREGTID                                             
052500          , :TP0WLOG.IDUSER                                               
052600          , :TP0WLOG.KVMILSEC                                             
052700          , :TP0WLOG.BEWEBSCR                                             
052800          , :TP0WLOG.BEWEBURL                                             
052900          , :TP0WLOG.IDLOPNR                                              
053000     END-EXEC                                                             
053100                                                                          
053110     MOVE 000100305  TO GOOD-SQLCODECODES                                 
053200     MOVE SQLCODE TO SQLCODE-WS                                           
053300     PERFORM DB2-STATUS-CHECK                                             
053400     .                                                                    
053500                                                                          
053600 DB2-CLOSE-TP0WLOG-FCRS SECTION.                                          
053700                                                                          
053710     MOVE 'DB2-CLOSE-TP0WLOG-FCRS' TO CURRENT-SECTION                     
053800     EXEC SQL                                                             
053900        CLOSE TP0WLOG-FCRS                                                
054000     END-EXEC                                                             
054100     .                                                                    
054200                                                                          
056964 DB2-DCL-OPN-TP0WLOG-DISTDC SECTION.                                      
056966     MOVE 'DB2-DCL-OPN-TP0WLOG-DISTDC' TO CURRENT-SECTION                 
056971     EXEC SQL                                                             
056972       DECLARE TP0WLOG-DISTDC CURSOR FOR                                  
056973       SELECT  DISTINCT(IDDC)                                             
056981       FROM    TP0WLOG                                                    
056994        WHERE   IDDC     BETWEEN :W-IDDC-LOW-KY                           
056994                           AND   :W-IDDC-HIGH-KY                          
057074          AND   TIREGDAT BETWEEN :W-TIREGDAT-LOW-KY                       
057075                           AND   :W-TIREGDAT-HIGH-KY                      
057003       ORDER BY IDDC     ASC                                              
057007     END-EXEC                                                             
057008                                                                          
057009     MOVE 000100305  TO GOOD-SQLCODECODES                                 
057010                                                                          
057011     EXEC SQL                                                             
057012       OPEN TP0WLOG-DISTDC                                                
057013     END-EXEC                                                             
057014                                                                          
057015     MOVE SQLCODE TO SQLCODE-WS                                           
057016     PERFORM DB2-STATUS-CHECK                                             
057017     .                                                                    
057018                                                                          
057019 DB2-FETCH-TP0WLOG-DISTDC SECTION.                                        
057020                                                                          
057021     MOVE 'DB2-FETCH-TP0WLOG-DISTDC' TO CURRENT-SECTION                   
057022     EXEC SQL                                                             
057023       FETCH TP0WLOG-DISTDC                                               
057025       INTO :TP0WLOG.IDDC                                                 
057033     END-EXEC                                                             
057034                                                                          
           MOVE IDDC       TO W-IDDC-LOW-KY                                     
                              W-IDDC-HIGH-KY                                    
                              W-IDDC                                            
057035     MOVE 000100305  TO GOOD-SQLCODECODES                                 
057036     MOVE SQLCODE    TO SQLCODE-WS                                        
057037     PERFORM DB2-STATUS-CHECK                                             
057038     .                                                                    
057039                                                                          
057040 DB2-CLOSE-TP0WLOG-DISTDC SECTION.                                        
057041                                                                          
057042     MOVE 'DB2-CLOSE-TP0WLOG-DISTDC' TO CURRENT-SECTION                   
057043     EXEC SQL                                                             
057044        CLOSE TP0WLOG-DISTDC                                              
057045     END-EXEC                                                             
057046     .                                                                    
057047 DB2-DCL-OPN-TP0WLOG-MAXMILSEC SECTION.                                   
057049     MOVE 'DB2-DCL-OPN-TP0WLOG-MAXMILSEC' TO CURRENT-SECTION              
           MOVE IDDC TO W-IDDC                                                  
057053     EXEC SQL                                                             
057054       DECLARE TP0WLOG-MAXMILSEC CURSOR FOR                               
057055       SELECT  MAX(KVMILSEC)                                              
057063       FROM    TP0WLOG                                                    
057074        WHERE   TIREGDAT   BETWEEN :W-TIREGDAT-LOW-KY                     
057075                               AND :W-TIREGDAT-HIGH-KY                    
057073        AND     IDDC = :W-IDDC                                            
057084     END-EXEC                                                             
057009     MOVE 000100305  TO GOOD-SQLCODECODES                                 
057010                                                                          
057011     EXEC SQL                                                             
057012       OPEN TP0WLOG-MAXMILSEC                                             
057013     END-EXEC                                                             
057014                                                                          
057015     MOVE SQLCODE TO SQLCODE-WS                                           
057016     PERFORM DB2-STATUS-CHECK                                             
057017     .                                                                    
057019 DB2-FETCH-TP0WLOG-MAXMILSEC SECTION.                                     
057020                                                                          
057021     MOVE 'DB2-FETCH-TP0WLOG-MAXMILSEC' TO CURRENT-SECTION                
057022     EXEC SQL                                                             
057023       FETCH TP0WLOG-MAXMILSEC                                            
057025       INTO :W-MAXMILSEC                                                  
057033     END-EXEC                                                             
057034                                                                          
057035     MOVE 000100305  TO GOOD-SQLCODECODES                                 
057036     MOVE SQLCODE TO SQLCODE-WS                                           
057037     PERFORM DB2-STATUS-CHECK                                             
057038     .                                                                    
057039                                                                          
057040 DB2-CLOSE-TP0WLOG-MAXMILSEC SECTION.                                     
057041                                                                          
057042     MOVE 'DB2-CLOSE-TP0WLOG-MAXMILSEC' TO CURRENT-SECTION                
057043     EXEC SQL                                                             
057044        CLOSE TP0WLOG-MAXMILSEC                                           
057045     END-EXEC                                                             
057046     .                                                                    
057047 DB2-DCL-OPN-TP0WLOG-MAXDATTIM SECTION.                                   
057049     MOVE 'DB2-DCL-OPN-TP0WLOG-MAXDATTIM' TO CURRENT-SECTION              
           MOVE IDDC TO W-IDDC                                                  
057053     EXEC SQL                                                             
057054       DECLARE TP0WLOG-MAXDATTIM CURSOR FOR                               
057055       SELECT  TIREGDAT, TIREGTID                                         
057063       FROM    TP0WLOG                                                    
057073       WHERE   IDDC = :W-IDDC                                             
             ORDER BY TIREGDAT DESC                                             
                    , TIREGTID DESC                                             
057084     END-EXEC                                                             
057009     MOVE 000100305  TO GOOD-SQLCODECODES                                 
057010                                                                          
057011     EXEC SQL                                                             
057012       OPEN TP0WLOG-MAXDATTIM                                             
057013     END-EXEC                                                             
057014                                                                          
057015     MOVE SQLCODE TO SQLCODE-WS                                           
057016     PERFORM DB2-STATUS-CHECK                                             
057017     .                                                                    
057019 DB2-FETCH-TP0WLOG-MAXDATTIM SECTION.                                     
057020                                                                          
057021     MOVE 'DB2-FETCH-TP0WLOG-MAXDATTIM' TO CURRENT-SECTION                
057022     EXEC SQL                                                             
057023       FETCH TP0WLOG-MAXDATTIM                                            
057025       INTO :W-MAXDATE                                                    
                , :W-MAXTIME                                                    
057033     END-EXEC                                                             
057034                                                                          
057035     MOVE 000100305  TO GOOD-SQLCODECODES                                 
057036     MOVE SQLCODE TO SQLCODE-WS                                           
057037     PERFORM DB2-STATUS-CHECK                                             
057038     .                                                                    
057039                                                                          
057040 DB2-CLOSE-TP0WLOG-MAXDATTIM SECTION.                                     
057041                                                                          
057042     MOVE 'DB2-CLOSE-TP0WLOG-MAXDATTIM' TO CURRENT-SECTION                
057043     EXEC SQL                                                             
057044        CLOSE TP0WLOG-MAXDATTIM                                           
057045     END-EXEC                                                             
057046     .                                                                    
57047  DB2-DCL-OPN-TP0WLOG-MAXSEC SECTION.                                      
057049     MOVE 'DB2-DCL-OPN-TP0WLOG-MAXSEC' TO CURRENT-SECTION                 
057053     EXEC SQL                                                             
057054       DECLARE TP0WLOG-MAXSEC CURSOR FOR                                  
057055       SELECT  IDDC                                                       
057056             , TIREGDAT                                                   
057057             , TIREGTID                                                   
057058             , IDUSER                                                     
057059             , KVMILSEC                                                   
057060             , BEWEBSCR                                                   
057061             , BEWEBURL                                                   
057062             , IDLOPNR                                                    
057063       FROM    TP0WLOG                                                    
057073        WHERE   IDDC     = :W-IDDC                                        
057074        AND   TIREGDAT   BETWEEN :W-TIREGDAT-LOW-KY                       
057075                             AND :W-TIREGDAT-HIGH-KY                      
057079        AND   KVMILSEC   = :W-MAXMILSEC                                   
057082       ORDER BY IDDC ASC                                                  
057083              , TIREGDAT DESC                                             
                    , TIREGTID DESC                                             
057084     END-EXEC                                                             
057085                                                                          
057086     MOVE 000100305  TO GOOD-SQLCODECODES                                 
057087                                                                          
057088     EXEC SQL                                                             
057089       OPEN TP0WLOG-MAXSEC                                                
057090     END-EXEC                                                             
057091                                                                          
057092     MOVE SQLCODE TO SQLCODE-WS                                           
057093     PERFORM DB2-STATUS-CHECK                                             
057094     .                                                                    
057095                                                                          
057096 DB2-FETCH-TP0WLOG-MAXSEC SECTION.                                        
057097                                                                          
057098     MOVE 'DB2-FETCH-TP0WLOG-MAXSEC' TO CURRENT-SECTION                   
057099     EXEC SQL                                                             
057100       FETCH TP0WLOG-MAXSEC                                               
057101                                                                          
057102       INTO :TP0WLOG.IDDC                                                 
057103          , :TP0WLOG.TIREGDAT                                             
057104          , :TP0WLOG.TIREGTID                                             
057105          , :TP0WLOG.IDUSER                                               
057106          , :TP0WLOG.KVMILSEC                                             
057107          , :TP0WLOG.BEWEBSCR                                             
057108          , :TP0WLOG.BEWEBURL                                             
057109          , :TP0WLOG.IDLOPNR                                              
057110     END-EXEC                                                             
057111                                                                          
057112     MOVE 000100305  TO GOOD-SQLCODECODES                                 
057113     MOVE SQLCODE TO SQLCODE-WS                                           
057114     PERFORM DB2-STATUS-CHECK                                             
057115     .                                                                    
057116                                                                          
057117 DB2-CLOSE-TP0WLOG-MAXSEC SECTION.                                        
057118                                                                          
057119     MOVE 'DB2-CLOSE-TP0WLOG-MAXSEC' TO CURRENT-SECTION                   
057120     EXEC SQL                                                             
057121        CLOSE TP0WLOG-MAXSEC                                              
057122     END-EXEC                                                             
057123     .                                                                    
57047  DB2-DCL-OPN-TP0WLOG-LATEST SECTION.                                      
057049     MOVE 'DB2-DCL-OPN-TP0WLOG-LATEST' TO CURRENT-SECTION                 
           MOVE W-IDDC          TO W-IDDC-LOW-KY                                
           MOVE HIGH-VALUES     TO W-IDDC-HIGH-KY                               
057053     EXEC SQL                                                             
057054       DECLARE TP0WLOG-LATEST CURSOR FOR                                  
057055       SELECT  IDDC                                                       
057056             , TIREGDAT                                                   
057057             , TIREGTID                                                   
057058             , IDUSER                                                     
057059             , KVMILSEC                                                   
057060             , BEWEBSCR                                                   
057061             , BEWEBURL                                                   
057062             , IDLOPNR                                                    
057063       FROM    TP0WLOG                                                    
057073        WHERE   IDDC     BETWEEN :W-IDDC-LOW-KY                           
057073                             AND :W-IDDC-HIGH-KY                          
057074        AND   TIREGDAT   BETWEEN :W-MAXDATE                               
057075                             AND :W-MAXDATE                               
057074        AND   TIREGTID   BETWEEN :W-MAXTIME                               
057075                             AND :W-MAXTIME                               
057082       ORDER BY IDDC ASC                                                  
057083              , TIREGDAT DESC                                             
                    , TIREGTID DESC                                             
057084     END-EXEC                                                             
057085                                                                          
057086     MOVE 000100305  TO GOOD-SQLCODECODES                                 
057087                                                                          
057088     EXEC SQL                                                             
057089       OPEN TP0WLOG-LATEST                                                
057090     END-EXEC                                                             
057091                                                                          
057092     MOVE SQLCODE TO SQLCODE-WS                                           
057093     PERFORM DB2-STATUS-CHECK                                             
057094     .                                                                    
057095                                                                          
057096 DB2-FETCH-TP0WLOG-LATEST SECTION.                                        
057097                                                                          
057098     MOVE 'DB2-FETCH-TP0WLOG-LATEST' TO CURRENT-SECTION                   
057099     EXEC SQL                                                             
057100       FETCH TP0WLOG-LATEST                                               
057101                                                                          
057102       INTO :TP0WLOG.IDDC                                                 
057103          , :TP0WLOG.TIREGDAT                                             
057104          , :TP0WLOG.TIREGTID                                             
057105          , :TP0WLOG.IDUSER                                               
057106          , :TP0WLOG.KVMILSEC                                             
057107          , :TP0WLOG.BEWEBSCR                                             
057108          , :TP0WLOG.BEWEBURL                                             
057109          , :TP0WLOG.IDLOPNR                                              
057110     END-EXEC                                                             
057111                                                                          
057112     MOVE 000100305  TO GOOD-SQLCODECODES                                 
057113     MOVE SQLCODE TO SQLCODE-WS                                           
057114     PERFORM DB2-STATUS-CHECK                                             
057115     .                                                                    
057116                                                                          
057117 DB2-CLOSE-TP0WLOG-LATEST SECTION.                                        
057118                                                                          
057119     MOVE 'DB2-CLOSE-TP0WLOG-LATEST' TO CURRENT-SECTION                   
057120     EXEC SQL                                                             
057121        CLOSE TP0WLOG-LATEST                                              
057122     END-EXEC                                                             
057123     .                                                                    
057124 DB2-DCL-OPN-TP0WLOG-CRS SECTION.                                         
057125                                                                          
057126     MOVE 'DB2-DCL-OPN-TP0WLOG-CRS' TO CURRENT-SECTION                    
057127                                                                          
057128     EXEC SQL                                                             
057129       DECLARE TP0WLOG-CRS CURSOR FOR                                     
057130                                                                          
057131       SELECT  IDDC                                                       
057132             , TIREGDAT                                                   
057133             , TIREGTID                                                   
057134             , IDUSER                                                     
057135             , KVMILSEC                                                   
057136             , BEWEBSCR                                                   
057137             , BEWEBURL                                                   
057138             , IDLOPNR                                                    
057139                                                                          
057140       FROM    TP0WLOG                                                    
057141                                                                          
057142       WHERE  IDDC       BETWEEN :W-IDDC-LOW-KY                           
057142                             AND :W-IDDC-HIGH-KY                          
057143        AND   TIREGDAT   BETWEEN :W-TIREGDAT-LOW-KY                       
057144                             AND :W-TIREGDAT-HIGH-KY                      
057145        AND   TIREGTID   BETWEEN :W-TIREGTID-LOW-KY                       
057146                             AND :W-TIREGTID-HIGH-KY                      
057147        AND   IDUSER     LIKE    :W-IDUSER-KY                             
057148        AND   KVMILSEC   BETWEEN :W-KVMILSEC-LOW-KY                       
057149                             AND :W-KVMILSEC-HIGH-KY                      
057150        AND   BEWEBSCR   LIKE    :W-BEWEBSCR-KY                           
057160                                                                          
057200       ORDER BY TIREGDAT DESC                                             
057300              , TIREGTID DESC                                             
057400     END-EXEC                                                             
057500                                                                          
057600     MOVE 000100305  TO GOOD-SQLCODECODES                                 
057700                                                                          
057800     EXEC SQL                                                             
057900       OPEN TP0WLOG-CRS                                                   
058000     END-EXEC                                                             
058100                                                                          
058200     MOVE SQLCODE TO SQLCODE-WS                                           
058300     PERFORM DB2-STATUS-CHECK                                             
058400     .                                                                    
058500                                                                          
058600 DB2-FETCH-TP0WLOG-CRS SECTION.                                           
058700                                                                          
058720     MOVE 'DB2-FETCH-TP0WLOG-CRS' TO CURRENT-SECTION                      
058900                                                                          
059000     EXEC SQL                                                             
059100       FETCH TP0WLOG-CRS                                                  
059200                                                                          
059300       INTO :TP0WLOG.IDDC                                                 
059400          , :TP0WLOG.TIREGDAT                                             
059500          , :TP0WLOG.TIREGTID                                             
059600          , :TP0WLOG.IDUSER                                               
059700          , :TP0WLOG.KVMILSEC                                             
059800          , :TP0WLOG.BEWEBSCR                                             
059900          , :TP0WLOG.BEWEBURL                                             
060000          , :TP0WLOG.IDLOPNR                                              
060100     END-EXEC                                                             
060200                                                                          
060210     MOVE 000100305  TO GOOD-SQLCODECODES                                 
060300     MOVE SQLCODE TO SQLCODE-WS                                           
060400     PERFORM DB2-STATUS-CHECK                                             
060500     .                                                                    
060600                                                                          
060700 DB2-CLOSE-TP0WLOG-CRS SECTION.                                           
060800                                                                          
060810     MOVE 'DB2-CLOSE-TP0WLOG-CRS' TO CURRENT-SECTION                      
060900     EXEC SQL                                                             
061000        CLOSE TP0WLOG-CRS                                                 
061100     END-EXEC                                                             
061200     .                                                                    
062794*                                                                         
062800 DB2-STATUS-CHECK  SECTION.                                               
062900                                                                          
062910     MOVE 'DB2-STATUS-CHECK' TO CURRENT-SECTION                           
063000     SET SQLCODE-IX TO 1                                                  
063100     SEARCH GOOD-SQLCODE                                                  
063200       AT END                                                             
063300          STRING 'INVALID DB2 SQL STATUS CODE: ' SQLCODE-WS               
063400          DELIMITED BY SIZE INTO ERROR-TEXT                               
063500          CALL ABEND USING RKOD-ABEND-DB2                                 
063600       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
063700          CONTINUE                                                        
063800     END-SEARCH                                                           
063900     .                                                                    
