000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W1115100.                                                
000300 AUTHOR.         REDDY RAHUL.                                             
000400 DATE-WRITTEN.   16/07/08.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        VALIDATE THE DATA FROM MIC.                                      
000900*        SEND ERROR RECORDS TO D&P.                                       
001000*                                                                         
001100*        THE PROGRAM READS     WDK6                                       
001200*                              WDF1                                       
001300*                              WDR2/5109                                  
001400*                                                                         
001500*    ABENDCODES:                                                          
001600*        U0016 -  . . . .                                                 
001700*        U1000 -  . . . .                                                 
001800*                                                                         
001900                                                                          
002000     SKIP3                                                                
002100 ENVIRONMENT DIVISION.                                                    
002200     SKIP2                                                                
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600     SKIP2                                                                
002700*          --- PCOO DATA FROM MIC                                         
002800     SELECT W11151                     ASSIGN TO W11151D1.                
002900     SKIP2                                                                
003000*          --- PCOO DATA FROM MIC, POST VALIDATIONS                       
003100     SELECT W11153                     ASSIGN TO W11151D2.                
003200     SKIP2                                                                
003300*          --- ERROR DATA TO D&P                                          
003400     SELECT W11154                     ASSIGN TO W11151D3.                
003500     EJECT                                                                
003600 DATA DIVISION.                                                           
003700     SKIP2                                                                
003800 FILE SECTION.                                                            
003900     SKIP3                                                                
004000 FD  W11151                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  -COPY W11151      -L.                                                
004500     SKIP3                                                                
004600 FD  W11153                                                               
004700     RECORDING       F                                                    
004800     BLOCK CONTAINS  0.                                                   
004900                                                                          
005000*01  RECORD -COPY W11151 -PRE  UT-  -L.                                   
005100     SKIP3                                                                
005200 FD  W11154                                                               
005300     RECORDING       V                                                    
005400     BLOCK CONTAINS  0.                                                   
005500                                                                          
005600 01  UT2-RECORD                  PIC X(313).                              
005700     EJECT                                                                
005800 WORKING-STORAGE SECTION.                                                 
005900                                                                          
006000 77  IDPGM                       PIC X(8)    VALUE 'W1115100'.            
006100 77  YES                         PIC X       VALUE 'J'.                   
006200 77  NOO                         PIC X       VALUE 'N'.                   
       01  AGREEMENT-EXIST             PIC X       VALUE 'N'.                   
006300                                                                          
006400 77  WS-PREV-IDARTNR             PIC X(9)    VALUE SPACE.                 
006500 77  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
006600 01  WS-CURR-DATE                PIC 9(8).                                
006700 01  WS-CURR-DATE-MIN-5          PIC 9(8).                                
006800 01  WS-CURR-DATE-PLUS-5         PIC 9(8).                                
006900 01  WS-DATE                     PIC 9(8).                                
007000 01  FILLER REDEFINES WS-DATE.                                            
007100     03  WS-CC                   PIC 9(2).                                
007200     03  WS-AAMMDD               PIC 9(6).                                
007300                                                                          
007400 77  INDATA-SW                   PIC X       VALUE 'N'.                   
007500     88  INDATA-OK                           VALUE 'J'.                   
007600     88  INDATA-FEL                          VALUE 'N'.                   
007700                                                                          
007800 77  SW-ERR-HDR                  PIC X       VALUE 'N'.                   
007900     88  WS-ERR-HDR-YES                      VALUE 'J'.                   
008000     88  WS-ERR-HDR-NO                       VALUE 'N'.                   
008100                                                                          
008200 77  W11151-EOF-SW               PIC X       VALUE 'N'.                   
008300     88  END-OF-W11151                       VALUE 'J'.                   
008400                                                                          
008500 77  WS-POS                      PIC 9(3)    VALUE ZERO.                  
008600 77  WS-ERR-BEAVTAL              PIC X(220)  VALUE SPACE.                 
008700 77  MAX-BEAVTAL                 PIC 99      VALUE ZERO.                  
008800 77  MAX-AVTAL-IX                PIC 99      VALUE 20.                    
008900 01  WS-BEAVTAL-TABLE.                                                    
009000     03  WS-BEAVTAL-TAB OCCURS 20 DEPENDING ON MAX-BEAVTAL                
009100                                  INDEXED BY AVTAL-IX.                    
009200         05  WS-BEAVTAL          PIC X(10).                               
009300         05  SW-BEAVTAL          PIC X.                                   
009400                                                                          
009500                                                                          
009600 01  WS-ERROR-HEADER.                                                     
009700     03  FILLER                  PIC X(313)  VALUE                        
009800         'PART-NUMBER  SUPPLIER  PCOO  AGREEMENT  ERROR'.                 
009900 01  WS-ERROR-MESSAGES.                                                   
010000     03  ERR-BEAVTAL-MISSING     PIC X(53)   VALUE                        
010100         'MIC NOT SENDING FOLLOWING AGREEMENTS TO PULS        '.          
010200     03  ERR-INVALID-PART        PIC X(53)   VALUE                        
010300         'INVALID PART NUMBER - NOT NUMERIC.                  '.          
010400     03  ERR-NOT-PULS-PART       PIC X(53)   VALUE                        
010500         'INVALID PART NUMBER - NOT A PULS PART.              '.          
010600     03  ERR-SUPERSEDED-PART     PIC X(53)   VALUE                        
010700         'PART IS SUPERSEDED.                                '.           
010800     03  ERR-NOT-PULS-SUPPLIER   PIC X(53)   VALUE                        
010900         'INVALID SUPPLIER    - NOT A PULS SUPPLIER.          '.          
011000     03  ERR-NOT-ISO-LAND        PIC X(53)   VALUE                        
011100         'INVALID PCOO        - NOT AN ISO COUNTRY.           '.          
011200     03  ERR-INVALID-CERT-STAT   PIC X(53)   VALUE                        
011300         'INVALID CERT STATUS - NOT A STATUS PULS KNOW OF.    '.          
011400     03  ERR-INVALID-FOM-DATE    PIC X(53)   VALUE                        
011500         'INVALID EFFECTIVE DATE.                             '.          
011600     03  ERR-INVALID-TOM-DATE    PIC X(53)   VALUE                        
011700         'INVALID EXPIRATION DATE.                            '.          
011800     03  ERR-BEAVTAL-NOT-FOUND   PIC X(53)   VALUE                        
011900         'INVALID AGREEMENT   - NOT AN AGREEMENT PULS KNOW OF.'.          
           03  ERR-IDAVTAL-NOT-FOUND   PIC X(53)   VALUE                        
               'INVALID SUPPLIER - AGREEMENT IS MISSING.            '.          
010800     03  ERR-NOT-MAIN-SUPPLIER   PIC X(53)   VALUE                        
010900         'INVALID SUPPLIER    - NOT A MAIN SUPPLIER.          '.          
012000     EJECT                                                                
012100 01  GENERAL-SUBPROGRAMS.                                                 
012200*                                                                         
012300     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
012400     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
012700     03  WISOLAND                PIC X(8)    VALUE 'WISOLAND'.            
012800     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
012900     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
013000     03  W400ARTU                PIC X(8)    VALUE 'W400ARTU'.            
013100                                                                          
013200     SKIP2                                                                
013210* - - - - - - - - - - - - - - - - - - -  KEYS TO ARTU                     
013220 01  FILLER                      PIC X(16)  VALUE 'STARTARTU'.            
013230*    -COPY W400ARTU                                                       
013240     EJECT                                                                
013300*    --- PARAMETERS TO ABEND                                              
013400                                                                          
013500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
013600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
013700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
013800     SKIP2                                                                
013900 01  ERROR-TEXT.                                                          
014000     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
014100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
014200     EJECT                                                                
014300*    --- PARAMETRAR TILL POSTSUM                                          
014400*                                                                         
014500*01  -COPY W0005   -PRE  POSTSUM-                                         
014600     EJECT                                                                
014700*01  -COPY WISOLAND                                                       
014800     EJECT                                                                
014900*01  -COPY WDECAREA                                                       
015000     EJECT                                                                
015100*01  -COPY WDATAREA                                                       
015200     EJECT                                                                
015300*                                                                         
015400 01  IN-AREA-START               PIC X(24)   VALUE                        
015500                                 'IN-AREA-START  '.                       
015600     SKIP2                                                                
015700                                                                          
015800*01  AREA -COPY W11151     -PRE IN-                                       
015900     EJECT                                                                
016000 01  UT-AREA-START               PIC X(24)   VALUE                        
016100                                 'UT-AREA-START  '.                       
016200     SKIP2                                                                
016300                                                                          
016400*01  AREA -COPY W11151     -PRE UT-                                       
016500     EJECT                                                                
016600 01  UT2-AREA-START              PIC X(24)   VALUE                        
016700                                 'UT2-AREA-START  '.                      
016800     SKIP2                                                                
016900 01  UT2-AREA.                                                            
017000     03  UT2-DATA.                                                        
017100       05  UT2-IDARTNR           PIC X(9).                                
017200       05  FILLER                PIC X(4).                                
017300       05  UT2-IDLEVNR           PIC X(5).                                
017400       05  FILLER                PIC X(5).                                
017500       05  UT2-KDARTURS-PCOO     PIC X(2).                                
017600       05  FILLER                PIC X(4).                                
017700       05  UT2-BEAVTAL           PIC X(10).                               
017800       05  FILLER                PIC X(1).                                
017900       05  UT2-TEXT              PIC X(53).                               
018000       05  UT2-BEAVTAL-TEXT      PIC X(220).                              
018100     EJECT                                                                
018200*    --- AREAS FOR IMS-SECTIONS                                           
018300*                                                                         
018400     EJECT                                                                
018500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
018600     SKIP3                                                                
018700 01  KEYS-FOR-DLI.                                                        
018800     03  W-IDARTNR-X.                                                     
018900         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
019000                                                                          
019100     03  W-IDLEVNR-X.                                                     
019200         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
019300                                                                          
019400     03  W-WDGXKEY01-X.                                                   
019500         05  W-WDGXKEY01.                                                 
019600             07  W-IDHTYP        PIC X(4)    VALUE '5109'.                
019700             07  W-FILLER        PIC X(26)   VALUE LOW-VALUE.             
019800                                                                          
019900*    03  W-BEAVTAL-X.                                                     
020000*        05  W-BEAVTAL           PIC X(10)   VALUE SPACE.                 
020100*                                                                         
020200*    03  W-IDLANDX2-X.                                                    
020300*        05  W-IDLANDX2          PIC X(2)    VALUE SPACE.                 
020400                                                                          
020500                                                                          
020600     SKIP2                                                                
020700*    --- STATUS-KOD FRÅN IMS                                              
020800 01  STATUS-WS                   PIC XX.                                  
020900     88  SEGMENT-FOUND                       VALUE '  '.                  
021000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
021100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
021100     88  SEGMENT-NOMORE                      VALUE 'GB'.                  
021200     SKIP2                                                                
021300 01  GOOD-STATUSCODES.                                                    
021400     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
021500     SKIP3                                                                
021600 01  SSA1                        PIC X(64).                               
021700 01  SSA2                        PIC X(64).                               
021800 01  SSA3                        PIC X(64).                               
021900     EJECT                                                                
022000*    --- IMS FUNCTION CODES                                               
022100*01  -COPY W0003                                                          
022200     EJECT                                                                
022300*    ---  DLI INPUT-OUTPUT AREA                                           
022400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
022500 01  DLI-IO-WDK601.                                                       
022600*    03  -COPY WDK601                                                     
022700     EJECT                                                                
068800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
069000 01  DLI-IO-WDK611.                                                       
069100*    03  -COPY WDK611                                                     
069200     EJECT                                                                
       01  FILLER         PIC X(24) VALUE 'DLI-IO-WDK623'.                      
       01  DLI-IO-WDK623.                                                       
      *   03  -COPY WDK623                                                      
           EJECT                                                                
022800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
022900 01  DLI-IO-WDF101.                                                       
023000*    03  -COPY WDF101                                                     
023100     EJECT                                                                
023200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX01'.                      
023300 01  DLI-IO-WDGX01.                                                       
023400*    03  -COPY WDGX01                                                     
023500     EJECT                                                                
023600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5110'.                    
023700 01  DLI-IO-WDGX5110.                                                     
023800*    03  -COPY WDGX5110                                                   
023900     EJECT                                                                
024000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX5112'.                    
024100 01  DLI-IO-WDGX5112.                                                     
024200*    03  -COPY WDGX5112                                                   
024300     EJECT                                                                
024400 LINKAGE SECTION.                                                         
024500*01  -COPY W0008  -PRE WDK6-                                              
024600     05  FILLER                  PIC X.                                   
024700     EJECT                                                                
024800*01  -COPY W0008  -PRE WDF1-                                              
024900     05  FILLER                  PIC X.                                   
025000     EJECT                                                                
025100*01  -COPY W0008  -PRE WDR2-                                              
025200     05  FILLER                  PIC X.                                   
025300     EJECT                                                                
025400 PROCEDURE DIVISION  USING WDK6-PCB WDF1-PCB WDR2-PCB.                    
025500 MAIN SECTION.                                                            
025600     ENTRY 'DLITCBL' USING WDK6-PCB WDF1-PCB WDR2-PCB.                    
025700                                                                          
025800     PERFORM A-INIT                                                       
025900     PERFORM S01-READ-W11151                                              
026000                                                                          
026100     IF NOT END-OF-W11151                                                 
026200       MOVE IN-IDARTNR           TO WS-PREV-IDARTNR                       
026300       PERFORM B-LOAD-BEAVTAL-TABLE                                       
026400       PERFORM UNTIL END-OF-W11151                                        
026500         IF IN-IDARTNR = WS-PREV-IDARTNR                                  
026600           CONTINUE                                                       
026700         ELSE                                                             
026800           PERFORM C-VALIDATE-BEAVTAL                                     
026900           MOVE IN-IDARTNR       TO WS-PREV-IDARTNR                       
027000         END-IF                                                           
027100         PERFORM D-VALIDATE-INPUT                                         
027200         PERFORM S01-READ-W11151                                          
027300       END-PERFORM                                                        
027400       PERFORM C-VALIDATE-BEAVTAL                                         
027500     END-IF                                                               
027600                                                                          
027700     PERFORM Z-FINIT                                                      
027800                                                                          
027900     MOVE ZERO                   TO RETURN-CODE                           
028000     GOBACK                                                               
028100     .                                                                    
028200     EJECT                                                                
028300 A-INIT SECTION.                                                          
028400                                                                          
028500     OPEN INPUT  W11151                                                   
028600                                                                          
028700     OPEN OUTPUT W11153                                                   
028800                 W11154                                                   
028900                                                                          
029000     MOVE IDPGM                  TO POSTSUM-PROGNAMN                      
029100                                                                          
029200     MOVE SPACES                 TO UT2-AREA                              
029300                                                                          
029400     MOVE FUNCTION CURRENT-DATE (1:8)                                     
029500                                 TO WS-CURR-DATE                          
029600     COMPUTE WS-CURR-DATE-MIN-5 = WS-CURR-DATE - 50000                    
029700     COMPUTE WS-CURR-DATE-PLUS-5 = WS-CURR-DATE + 50000                   
029800                                                                          
029900     .                                                                    
030000     EJECT                                                                
030100 B-LOAD-BEAVTAL-TABLE SECTION.                                            
030200                                                                          
030300     MOVE ZERO                   TO MAX-BEAVTAL                           
030400     MOVE SPACE                  TO WS-BEAVTAL-TABLE                      
030500     SET AVTAL-IX                TO 1                                     
030600                                                                          
030700     PERFORM IMS-GN-WDGX5110                                              
030800     PERFORM                                                              
030900       UNTIL SEGMENT-MISSING OR                                           
031000             AVTAL-IX > MAX-AVTAL-IX                                      
031100       MOVE 5110-BEAVTAL         TO WS-BEAVTAL (AVTAL-IX)                 
031200       MOVE SPACE                TO SW-BEAVTAL (AVTAL-IX)                 
031300       ADD 1                     TO MAX-BEAVTAL                           
031400       SET AVTAL-IX           UP BY 1                                     
031500       PERFORM IMS-GN-WDGX5110                                            
031600     END-PERFORM                                                          
031700     IF AVTAL-IX > MAX-AVTAL-IX                                           
031800       MOVE 'INCREASE WS-BEAVTAL-TABLE LENGTH'                            
031900                                 TO ERROR-TEXT-STR                        
032000       CALL FELLOG                                                        
032100     END-IF                                                               
032200                                                                          
032300     .                                                                    
032400     EJECT                                                                
032500 C-VALIDATE-BEAVTAL SECTION.                                              
032600     MOVE 1                      TO WS-POS                                
032700     MOVE SPACES                 TO WS-ERR-BEAVTAL                        
032800     PERFORM                                                              
032900     VARYING AVTAL-IX FROM 1 BY 1                                         
033000       UNTIL AVTAL-IX > MAX-BEAVTAL                                       
033100       IF SW-BEAVTAL (AVTAL-IX) = YES                                     
033200         MOVE SPACE              TO SW-BEAVTAL (AVTAL-IX)                 
033300       ELSE                                                               
033400         MOVE WS-BEAVTAL (AVTAL-IX)                                       
033500                                 TO WS-ERR-BEAVTAL (WS-POS:10)            
033600         ADD 11                  TO WS-POS                                
033700       END-IF                                                             
033800     END-PERFORM                                                          
033900     IF WS-POS > 1                                                        
034000       MOVE WS-PREV-IDARTNR      TO UT2-IDARTNR                           
034100       MOVE ERR-BEAVTAL-MISSING  TO UT2-TEXT                              
034200       MOVE WS-ERR-BEAVTAL       TO UT2-BEAVTAL-TEXT                      
034300       PERFORM S12-WRITE-W11154                                           
034400     END-IF                                                               
034500     .                                                                    
034600     EJECT                                                                
034700 D-VALIDATE-INPUT SECTION.                                                
034800                                                                          
034900     MOVE YES                    TO INDATA-SW                             
035000                                                                          
035100     IF INDATA-OK                                                         
035200       SET AVTAL-IX              TO 1                                     
035300       SEARCH WS-BEAVTAL-TAB                                              
035400         AT END                                                           
035500           MOVE ERR-BEAVTAL-NOT-FOUND                                     
035600                                 TO UT2-TEXT                              
035700           PERFORM DA-HANDLE-ERROR                                        
035800         WHEN WS-BEAVTAL (AVTAL-IX) = IN-BEAVTAL                          
035900           MOVE YES              TO SW-BEAVTAL (AVTAL-IX)                 
036000       END-SEARCH                                                         
036100     END-IF                                                               
036200                                                                          
036300     IF INDATA-OK                                                         
036400       MOVE IN-IDARTNR           TO DEC-IDFRIDATA                         
036500       MOVE 9                    TO DEC-KVHELTAL                          
036600       MOVE 0                    TO DEC-KVDECIMAL                         
036700                                                                          
036800       CALL WDECEDIT          USING DEC-WDECAREA                          
036900                                                                          
037000       IF DEC-KDSVAR-OK                                                   
037100         MOVE DEC-IDEDITDATA     TO WS-IDARTNR                            
037200       ELSE                                                               
037300         MOVE ERR-INVALID-PART   TO UT2-TEXT                              
037400         PERFORM DA-HANDLE-ERROR                                          
037500       END-IF                                                             
037600     END-IF                                                               
037700                                                                          
037800     IF INDATA-OK                                                         
037900       MOVE WS-IDARTNR           TO W-IDARTNR                             
038000       PERFORM IMS-GU-WDK601                                              
038100       IF SEGMENT-MISSING                                                 
038200         MOVE ERR-NOT-PULS-PART  TO UT2-TEXT                              
038300         PERFORM DA-HANDLE-ERROR                                          
038400       ELSE                                                               
038500         IF ART-KDERS-UTG > 20                                            
038600           MOVE ERR-SUPERSEDED-PART TO UT2-TEXT                           
038700           PERFORM DA-HANDLE-ERROR                                        
038800         ELSE                                                             
                IF ART-IDLEVNR IS NOT = IN-IDLEVNR                              
                 MOVE ERR-NOT-MAIN-SUPPLIER TO UT2-TEXT                         
                 PERFORM DA-HANDLE-ERROR                                        
                END-IF                                                          
               END-IF                                                           
038900       END-IF                                                             
039000     END-IF                                                               
039100                                                                          
039200     IF INDATA-OK                                                         
039300       MOVE IN-IDLEVNR           TO W-IDLEVNR                             
039400       PERFORM IMS-GU-WDF101                                              
039500       IF SEGMENT-MISSING                                                 
039600         MOVE ERR-NOT-PULS-SUPPLIER                                       
039700                                 TO UT2-TEXT                              
039800         PERFORM DA-HANDLE-ERROR                                          
             ELSE                                                               
               MOVE NOO TO AGREEMENT-EXIST                                      
               PERFORM IMS-GNP-WDK611                                           
               IF SEGMENT-FOUND                                                 
                PERFORM IMS-GNP-WDK623                                          
                PERFORM UNTIL SEGMENT-MISSING OR (AGREEMENT-EXIST = 'J')        
                              OR SEGMENT-NOMORE                                 
                 IF IN-IDLEVNR = AVT-IDLEVNR-AVT                                
                  MOVE YES TO AGREEMENT-EXIST                                   
                 ELSE                                                           
                  PERFORM IMS-GNP-WDK623                                        
                 END-IF                                                         
                END-PERFORM                                                     
               END-IF                                                           
                IF AGREEMENT-EXIST = 'J'                                        
                 CONTINUE                                                       
                ELSE                                                            
                 MOVE ERR-IDAVTAL-NOT-FOUND TO UT2-TEXT                         
                 PERFORM DA-HANDLE-ERROR                                        
                END-IF                                                          
039900       END-IF                                                             
040000     END-IF                                                               
040100                                                                          
040200*    IF INDATA-OK                                                         
040300*      MOVE IN-KDARTURS-PCOO     TO LAND-IDLANDX2                         
040400*      MOVE SPACE                TO LAND-IDLANDX3                         
040500*      CALL WISOLAND          USING LAND-WISOLAND                         
040600*      IF LAND-KDSVAR NOT = SPACE                                         
040700*         MOVE ERR-NOT-ISO-LAND  TO UT2-TEXT                              
040800*        PERFORM DA-HANDLE-ERROR                                          
040900*      END-IF                                                             
041000*    END-IF                                                               
041100                                                                          
041110     IF INDATA-OK                                                         
041120       MOVE IN-KDARTURS-PCOO TO ARTU-KDARTURS                             
041130       MOVE SPACE        TO ARTU-IDDC                                     
041140       MOVE ZERO         TO ARTU-IDDISTR                                  
041150       CALL W400ARTU USING ARTU-W400ARTU                                  
041160       IF ARTU-KDARTURS = SPACE OR                                        
041170          ARTU-KDARTURS-NUM = ZERO                                        
041180           MOVE ERR-NOT-ISO-LAND  TO UT2-TEXT                             
041190           PERFORM DA-HANDLE-ERROR                                        
041191       END-IF                                                             
041194     END-IF                                                               
041195                                                                          
041200     IF INDATA-OK                                                         
041300       IF IN-IDSTAMIC = '01' OR '50'                                      
041400         CONTINUE                                                         
041500       ELSE                                                               
041600         MOVE ERR-INVALID-CERT-STAT                                       
041700                                 TO UT2-TEXT                              
041800         PERFORM DA-HANDLE-ERROR                                          
041900       END-IF                                                             
042000     END-IF                                                               
042100                                                                          
042200     IF INDATA-OK                                                         
042300       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
042400       MOVE IN-TIGILTIG-FOM      TO DAT-I-TIDATUM                         
042500       CALL WDATKONV          USING DAT-KDDATFORM                         
042600                                    DAT-I-TIDATUM                         
042700                                    DAT-O-TIDATUM                         
042800                                    DAT-KDSVAR                            
042900       IF DAT-KDSVAR-OK                                                   
043000         MOVE DAT-TISEKEL        TO WS-CC                                 
043100         MOVE DAT-TIAAMMDD       TO WS-AAMMDD                             
043200         IF WS-CURR-DATE-MIN-5 <= WS-DATE AND                             
043300            WS-CURR-DATE-PLUS-5 >= WS-DATE                                
043400           CONTINUE                                                       
043500         ELSE                                                             
043600           MOVE ERR-INVALID-FOM-DATE                                      
043700                                 TO UT2-TEXT                              
043800           PERFORM DA-HANDLE-ERROR                                        
043900         END-IF                                                           
044000       ELSE                                                               
044100         MOVE ERR-INVALID-FOM-DATE                                        
044200                                 TO UT2-TEXT                              
044300         PERFORM DA-HANDLE-ERROR                                          
044400       END-IF                                                             
044500     END-IF                                                               
044600                                                                          
044700     IF INDATA-OK                                                         
044800       MOVE 'AAMMDD'             TO DAT-KDDATFORM                         
044900       MOVE IN-TIGILTIG-TOM      TO DAT-I-TIDATUM                         
045000       CALL WDATKONV            USING DAT-KDDATFORM                       
045100                                      DAT-I-TIDATUM                       
045200                                      DAT-O-TIDATUM                       
045300                                      DAT-KDSVAR                          
045400       IF DAT-KDSVAR-OK                                                   
045500         MOVE DAT-TISEKEL        TO WS-CC                                 
045600         MOVE DAT-TIAAMMDD       TO WS-AAMMDD                             
045700         IF WS-CURR-DATE-MIN-5 <= WS-DATE AND                             
045800            WS-CURR-DATE-PLUS-5 >= WS-DATE                                
045900           CONTINUE                                                       
046000         ELSE                                                             
046100           MOVE ERR-INVALID-TOM-DATE                                      
046200                                 TO UT2-TEXT                              
046300           PERFORM DA-HANDLE-ERROR                                        
046400         END-IF                                                           
046500       ELSE                                                               
046600         MOVE ERR-INVALID-TOM-DATE                                        
046700                                 TO UT2-TEXT                              
046800         PERFORM DA-HANDLE-ERROR                                          
046900       END-IF                                                             
047000     END-IF                                                               
047100                                                                          
047200     IF INDATA-OK                                                         
047300       MOVE WS-IDARTNR           TO UT-IDARTNR                            
047400       MOVE IN-IDLEVNR           TO UT-IDLEVNR                            
047500       MOVE IN-KDARTURS-PCOO     TO UT-KDARTURS-PCOO                      
047600       MOVE IN-IDSTAMIC          TO UT-IDSTAMIC                           
047700       MOVE IN-TIGILTIG-FOM      TO UT-TIGILTIG-FOM                       
047800       MOVE IN-TIGILTIG-TOM      TO UT-TIGILTIG-TOM                       
047900       MOVE IN-BEAVTAL           TO UT-BEAVTAL                            
048000       PERFORM S11-WRITE-W11153                                           
048100     END-IF                                                               
048200     .                                                                    
048300     EJECT                                                                
048400 DA-HANDLE-ERROR SECTION.                                                 
048500     MOVE NOO                    TO INDATA-SW                             
048600     MOVE IN-IDARTNR             TO UT2-IDARTNR                           
048700     MOVE IN-IDLEVNR             TO UT2-IDLEVNR                           
048800     MOVE IN-KDARTURS-PCOO       TO UT2-KDARTURS-PCOO                     
048900     MOVE IN-BEAVTAL             TO UT2-BEAVTAL                           
049000     PERFORM S12-WRITE-W11154                                             
049100     .                                                                    
049200     EJECT                                                                
049300 Z-FINIT SECTION.                                                         
049400     CLOSE W11151                                                         
049500           W11153                                                         
049600           W11154                                                         
049700     SKIP2                                                                
049800     MOVE 'S'                    TO POSTSUM-OPKOD                         
049900     CALL POSTSUM             USING POSTSUM-PARM                          
050000     .                                                                    
050100     EJECT                                                                
050200 S01-READ-W11151  SECTION.                                                
050300     READ W11151               INTO IN-AREA                               
050400     AT END                                                               
050500        MOVE HIGH-VALUE          TO IN-AREA                               
050600        SET END-OF-W11151        TO TRUE                                  
050700                                                                          
050800     NOT AT END                                                           
050900        MOVE 'W11151'            TO POSTSUM-FDNAMN                        
051000        MOVE 'W11151D1'          TO POSTSUM-DDNAMN2                       
051100        MOVE SPACE               TO POSTSUM-TRANSTYP                      
051200        CALL POSTSUM          USING POSTSUM-PARM                          
051300     END-READ                                                             
051400     .                                                                    
051500     EJECT                                                                
051600 S11-WRITE-W11153 SECTION.                                                
051700                                                                          
051800     WRITE UT-RECORD           FROM UT-AREA                               
051900                                                                          
052000     MOVE SPACE                  TO POSTSUM-TRANSTYP                      
052100     MOVE 'W11153'               TO POSTSUM-FDNAMN                        
052200     MOVE 'W11151D2'             TO POSTSUM-DDNAMN2                       
052300     CALL POSTSUM             USING POSTSUM-PARM                          
052400                                                                          
052500     MOVE SPACE                  TO UT-AREA                               
052600     .                                                                    
052700     EJECT                                                                
052800 S12-WRITE-W11154 SECTION.                                                
052900                                                                          
053000     IF WS-ERR-HDR-NO                                                     
053100       WRITE UT2-RECORD        FROM WS-ERROR-HEADER                       
053200       SET WS-ERR-HDR-YES        TO TRUE                                  
053300     END-IF                                                               
053400                                                                          
053500     WRITE UT2-RECORD          FROM UT2-AREA                              
053600                                                                          
053700     MOVE SPACE                  TO POSTSUM-TRANSTYP                      
053800     MOVE 'W11154'               TO POSTSUM-FDNAMN                        
053900     MOVE 'W11151D3'             TO POSTSUM-DDNAMN2                       
054000     CALL POSTSUM             USING POSTSUM-PARM                          
054100                                                                          
054200     MOVE SPACE                  TO UT2-AREA                              
054300     .                                                                    
054400     EJECT                                                                
054500 S99-ABEND SECTION.                                                       
054600                                                                          
054700     SKIP2                                                                
054800     MOVE 'S'                    TO POSTSUM-OPKOD                         
054900     CALL POSTSUM             USING POSTSUM-PARM                          
055000     CALL ABEND               USING RKOD-ABEND                            
055100     .                                                                    
055200     EJECT                                                                
055300* --- IMS SECTIONS  ---                                                   
055400                                                                          
055500     EJECT                                                                
055600 IMS-GU-WDK601 SECTION.                                                   
055700                                                                          
055800     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
055900          DELIMITED BY SIZE INTO SSA1                                     
056000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
056100     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
056200     MOVE WDK6-STATUS-CODE       TO STATUS-WS                             
056300     PERFORM IMS-STATUSCHECK                                              
056400     .                                                                    
056500     EJECT                                                                
       IMS-GNP-WDK611 SECTION.                                                  
           MOVE 'WDK611 ' TO SSA1                                               
           MOVE '  GE' TO GOOD-STATUSCODES                                      
           CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
           MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
           EJECT                                                                
       IMS-GNP-WDK623 SECTION.                                                  
           MOVE 'WDK623 ' TO SSA1                                               
           MOVE '  GEGB' TO GOOD-STATUSCODES                                    
           CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK623 SSA1                   
           MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
           PERFORM IMS-STATUSCHECK                                              
           .                                                                    
           EJECT                                                                
056600 IMS-GU-WDF101 SECTION.                                                   
056700                                                                          
056800     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
056900          DELIMITED BY SIZE INTO SSA1                                     
057000     MOVE '  GE'                 TO GOOD-STATUSCODES                      
057100     CALL CBLTDLI USING GU WDF1-PCB DLI-IO-WDF101 SSA1                    
057200     MOVE WDF1-STATUS-CODE       TO STATUS-WS                             
057300     PERFORM IMS-STATUSCHECK                                              
057400     .                                                                    
057500     EJECT                                                                
057600 IMS-GN-WDGX5110 SECTION.                                                 
057700                                                                          
057800     STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY01-X ')'                       
057900          DELIMITED BY SIZE INTO SSA1                                     
058000     MOVE 'WDGX5110 '            TO SSA2                                  
058100     MOVE '  GE'                 TO GOOD-STATUSCODES                      
058200     CALL CBLTDLI USING GN WDR2-PCB DLI-IO-WDGX5110 SSA1 SSA2             
058300     MOVE WDR2-STATUS-CODE       TO STATUS-WS                             
058400     PERFORM IMS-STATUSCHECK                                              
058500     .                                                                    
058600     EJECT                                                                
058700*IMS-GU-WDGX5112 SECTION.                                                 
058800*                                                                         
058900*    STRING 'WDR201  (WDGXKEY  =' W-WDGXKEY01-X ')'                       
059000*         DELIMITED BY SIZE INTO SSA1                                     
059100*    STRING 'WDGX5110(BEAVTAL  =' W-BEAVTAL-X ')'                         
059200*         DELIMITED BY SIZE INTO SSA2                                     
059300*    STRING 'WDGX5112(IDLANDX2 =' W-IDLANDX2-X ')'                        
059400*         DELIMITED BY SIZE INTO SSA3                                     
059500*    MOVE '  GE'                 TO GOOD-STATUSCODES                      
059600*    CALL CBLTDLI USING GU WDR2-PCB DLI-IO-WDGX5112                       
059700*                       SSA1 SSA2 SSA3                                    
059800*    MOVE WDR2-STATUS-CODE       TO STATUS-WS                             
059900*    PERFORM IMS-STATUSCHECK                                              
060000*    .                                                                    
060100*    EJECT                                                                
060200 IMS-STATUSCHECK SECTION.                                                 
060300                                                                          
060400     SET STATUS-IX TO 1                                                   
060500     SEARCH GOOD-STATUS                                                   
060600       AT END                                                             
060700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
060800           DELIMITED BY SIZE INTO ERROR-TEXT                              
060900         DISPLAY ERROR-TEXT                                               
061000         CALL FELLOG                                                      
061100       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
061200         CONTINUE                                                         
061300     END-SEARCH                                                           
061400     .                                                                    
