000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W6018B00.                                                
000400 AUTHOR.         SANTHOSHKUMAR ANGAMUTHU.                                 
000500 DATE-WRITTEN.   20/03/04.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        RECEIVE VALUES FROM METRILUX HARDWARE                            
001000*                                                                         
001100*        PROGRAM UPDATES  WDK2                                            
001200*        PROGRAM UPDATES  WDK6                                            
001300*        PROGRAM UPDATE   WDK7                                            
001900*    INDATA.                                                              
002000*        TRANSAKTION: W6018BX                                             
002100*        MID:         W6I18B01                                            
002200*                                                                         
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 WORKING-STORAGE SECTION.                                                 
003300 77  IDPGM                       PIC X(08)     VALUE 'W6018B00'.          
003400                                                                          
003700*    --- WORK FIELDS FOR ERROR MESSAGES WHEN CALLING ABEND/FELLOG         
003800 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
003820 77  KDRC-DISPLAY                PIC Z(5).                                
003821 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
003830                                                                          
003900 77  CURRENT-SECTION             PIC X(16) VALUE SPACE.                   
004000 77  CURRENT-IMS-SECTION         PIC X(16) VALUE SPACE.                   
004100                                                                          
004200 77  CURRENT-DATE                PIC 9(6)  VALUE ZERO.                    
004300 77  CURRENT-TIME                PIC 9(8)  VALUE ZERO.                    
004400                                                                          
004500 77  YES                         PIC X     VALUE 'Y'.                     
004600 77  NOO                         PIC X     VALUE 'N'.                     
004700                                                                          
004800 77  LNG-P-TO-P-PREFIX           PIC S9(4) COMP SYNC VALUE +17.           
004900                                                                          
005000 77  WS-KDUVKNTO                 PIC X(1)  VALUE SPACE.                   
005100 01  WC-UPD-MAN                  PIC X(1)  VALUE '2'.                     
005200 01  WC-UPD-VKART                PIC X(1)  VALUE '4'.                     
005300                                                                          
005400 77  WS-VKART-NTO-CURR           PIC S9(9)       VALUE ZERO.              
005500 77  WS-VKART-BTO-CURR           PIC S9(9)       VALUE ZERO.              
005600 01  WS-VLARTNTO-CURR-X.                                                  
005700     03 WS-VLARTNTO-CURR         PIC S9(8)V9(1)  VALUE ZERO.              
005800                                                                          
005900 77  WS-VLARTNTO                 PIC S9(8)V9(1)  VALUE ZERO.              
006000                                                                          
006100 01  WS-VLARTNTO-DISP-NUM        PIC 9(8)V9(1).                           
006200 01  WS-VLARTNTO-DISP-XX  REDEFINES WS-VLARTNTO-DISP-NUM.                 
006300     05 WS-VLARTNTO-DISP-ALFA    PIC X(9).                                
006400*                                                                         
006500*01    -COPY WWDCKONS                                                     
006600                                                                          
006700*01    -COPY WWLNDKON                                                     
006800                                                                          
006810 77  INDATA-SW                   PIC X       VALUE 'Y'.                   
006820     88  INDATA-OK                           VALUE 'Y'.                   
006830     88  INDATA-WRONG                        VALUE 'N'.                   
006840                                                                          
006900 77  WS-MACHINE-FLAG             PIC X       VALUE 'Y'.                   
007000     88  MACHINE-YES                         VALUE 'Y'.                   
007100     88  MACHINE-NO                          VALUE 'N'.                   
007200                                                                          
007300 77  KEYS-SW                     PIC X       VALUE 'Y'.                   
007400     88  KEYS-OK                             VALUE 'Y'.                   
007500     88  KEYS-WRONG                          VALUE 'N'.                   
007600                                                                          
007700 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
007900     88  GOOD-MID                            VALUE '6161' '6162'          
008000                                                   '6163' '6164'          
008100                                                   '6165' '6166'          
008200                                                   '6167' '6168'          
008300                                                   '6169'.                
008400     88  HELP-MID                            VALUE '0551'.                
008500                                                                          
008600*    --- SUBPROGRAMS AND PARAMETER AREAS                                  
008700 01  GENERAL-SUBPROGRAMS.                                                 
008800     03  WMEDKONV                PIC X(8)    VALUE 'WMEDKONV'.            
008900     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009000     03  W006KOM                 PIC X(8)    VALUE 'W006KOM '.            
009100     03  WDECEDIT                PIC X(8)    VALUE 'WDECEDIT'.            
009200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
009300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009310     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
009311     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009400                                                                          
009500*    --- PARAMETERS FOR SUBPROGRAM WMEDKONV                               
009600*01 -COPY WMEDAREA                                                        
009700                                                                          
010600 01  FILLER                      PIC X(16)  VALUE 'WWOMVAND '.            
010700*   -COPY WWOMVAND                                                        
010800                                                                          
010900*01 -COPY WDECAREA                                                        
011000                                                                          
011100*    --- PARAMETERS FOR SUB PROGRAM W005INIT                              
011200*                                                                         
011300 01  FILLER                      PIC X(16)   VALUE 'WMSGINIT'.            
011400                                                                          
011500*01 -COPY WMSGINIT                                                        
011600                                                                          
011700*                                                                         
011800 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
011900     SKIP3                                                                
012000*01  -COPY WZ01SUB                                                        
012100     EJECT                                                                
012200*    --- AREAS FOR MFS AND SCREEN MANAGEMENT                              
012300*                                                                         
012400 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
012410 01  REQU-AREA.                                                           
012420*    03  -COPY WZ01REQU                                                   
012430*    03  -COPY W6I18B01                                                   
012440     EJECT                                                                
012500                                                                          
012800                                                                          
012900 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
012910                                                                          
012920*01  -COPY WMSGAREA                                                       
012930                                                                          
013600                                                                          
013700 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
013800                                                                          
013900*01  -COPY WMFSAREA                                                       
014000                                                                          
014100 01  FILLER                   PIC X(16)  VALUE 'KOM-MSG-IO-AREA '.        
014200 01  KOM-MFG-IO-AREA.                                                     
014300*03  -COPY WMSGKOM                                                        
014400                                                                          
014500 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW'.           
014600 01  P-TO-P-SW.                                                           
014700     02     P-TO-P-KVLL             PIC S9(4) COMP SYNC.                  
014800     02     P-TO-P-KDZ1             PIC X(1)  VALUE LOW-VALUE.            
014900     02     P-TO-P-KDZ2             PIC X(1)  VALUE LOW-VALUE.            
015000     02     P-TO-P-KDTRANS          PIC X(8).                             
015100     02     P-TO-P-IDTRANS          PIC X(4).                             
015200     02     P-TO-P-KDMFSFOR         PIC X(1).                             
015300     02     P-TO-P-DATA             PIC X(1000).                          
015400                                                                          
015500 01  FILLER                      PIC X(16)   VALUE 'P-TO-P-SW2'.          
015600 01  P-TO-P-SW2.                                                          
015700     02     P-TO-P2-KVLL             PIC S9(4) COMP SYNC.                 
015800     02     P-TO-P2-KDZ1             PIC X(1)  VALUE LOW-VALUE.           
015900     02     P-TO-P2-KDZ2             PIC X(1)  VALUE LOW-VALUE.           
016000     02     P-TO-P2-KDTRANS          PIC X(8).                            
016100     02     P-TO-P2-IDTRANS          PIC X(4).                            
016200     02     P-TO-P2-KDMFSFOR         PIC X(1).                            
016300     02     MID -COPY W4I28901   -PRE P-TO-P2-                            
016400*****************************************************************         
016500                                                                          
016600                                                                          
016700 01      FILLER                  PIC X(24)   VALUE                        
016800                                 '619B-MID-W6I19B01'.                     
016900     SKIP2                                                                
017000     -COPY W6I19B01 -PRE 619B-                                            
017100                                                                          
017200*    --- WORK-AREAS FOR IMS-SECTIONS                                      
017300*                                                                         
017400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
017500                                                                          
017600 01  KEYS-FOR-DLI.                                                        
017700     03  W-IDARTNR-X.                                                     
017800         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
017900     03  W-KDSEGKEY-X.                                                    
018000         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
018500     03  W-IDLAND-X.                                                      
018600         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
018700                                                                          
018800*    --- STATUS CODES FROM IMS                                            
018900 01  STATUS-WS                   PIC XX.                                  
019000     88  SEGMENT-FOUND                       VALUE '  '.                  
019100     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
019200     88  SEGMENT-MISSING                     VALUE 'GE'.                  
019300                                                                          
019400 01  GOOD-STATUSCODES.                                                    
019500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
019600                                                                          
019700 01  ALL-SSA.                                                             
019800     03 SSA1                     PIC X(64).                               
019900     03 SSA2                     PIC X(64).                               
020000                                                                          
020100*    --- IMS FUNCTION CODES                                               
020200*01  -COPY W0003                                                          
020300                                                                          
020400*    ---  DLI INPUT-OUTPUT AREA                                           
020500                                                                          
020600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK201'.                      
020700 01  DLI-IO-WDK201.                                                       
020800*    03  -COPY WDK201                                                     
020900                                                                          
021000                                                                          
021100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK211'.                      
021200 01  DLI-IO-WDK211.                                                       
021300*    03  -COPY WDK211                                                     
021400                                                                          
021900                                                                          
022800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
022900 01  DLI-IO-WDK601.                                                       
023000*    03  -COPY WDK601                                                     
023100                                                                          
023200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
023300 01  DLI-IO-WDK611.                                                       
023400*    03  -COPY WDK611                                                     
023500                                                                          
023600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
023700 01  DLI-IO-WDK712.                                                       
023800*    03  -COPY WDK712                                                     
023900                                                                          
024000                                                                          
054220 LINKAGE SECTION.                                                         
054230*01  -COPY W0009   -PRE MSG-                                              
054240*01  -COPY W0009   -PRE DISP-                                             
054250 01  KOM-WDP8-PCB               PIC X.                                    
054280                                                                          
054290*01  -COPY W0008  -PRE WDK2-                                              
054291     05  FILLER                  PIC X.                                   
054292                                                                          
054296*01  -COPY W0008  -PRE WDK6-                                              
054297     05  FILLER                  PIC X.                                   
054298                                                                          
054299*01  -COPY W0008  -PRE WDK7-                                              
054300     05  FILLER                  PIC X.                                   
054301                                                                          
054302                                                                          
054303 PROCEDURE DIVISION  USING MSG-PCB  DISP-PCB KOM-WDP8-PCB                 
054304                           WDK2-PCB                                       
054305                           WDK6-PCB WDK7-PCB.                             
054306 MAIN SECTION.                                                            
054307     ENTRY 'DLITCBL' USING MSG-PCB  DISP-PCB KOM-WDP8-PCB                 
054308                           WDK2-PCB                                       
054309                           WDK6-PCB WDK7-PCB.                             
054310                                                                          
054311     PERFORM S01-FETCH-REQUEST-ARGUMENT                                   
054312     IF SUB-KDRC = 0                                                      
056100         PERFORM A-INIT                                                   
056300         PERFORM B-UPPD-WDK201-WDK211                                     
056400         PERFORM C-CHECK-UPPD-WDK611                                      
056410         PERFORM D-CHECK-UPPD-WDK712                                      
057600     END-IF                                                               
057700                                                                          
057800     MOVE ZERO TO RETURN-CODE                                             
057900     GOBACK                                                               
058000     .                                                                    
058100     EJECT                                                                
058200 A-INIT SECTION.                                                          
058300                                                                          
058310     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
059100                                                                          
060800     ACCEPT CURRENT-DATE   FROM DATE                                      
060900     ACCEPT CURRENT-TIME   FROM TIME                                      
061000     .                                                                    
061100     EJECT                                                                
061200 B-UPPD-WDK201-WDK211 SECTION.                                            
061300                                                                          
061400                                                                          
061402     MOVE MID-IDARTNR                   TO W-IDARTNR                      
061403     PERFORM IMS-GU-WDK201                                                
061404                                                                          
061405                                                                          
061406     IF SEGMENT-MISSING                                                   
061409       MOVE W-IDARTNR                   TO ARTM-IDARTNR                   
061410       PERFORM IMS-ISRT-WDK201                                            
061411       PERFORM BA-INIT-WDK211                                             
061412       MOVE '1'                         TO MATD-KDSEGKEY                  
061413       PERFORM BB-CHECK-VALUE-METRILUX                                    
061415       PERFORM IMS-ISRT-WDK211                                            
061417     ELSE                                                                 
061420       PERFORM IMS-GHNP-WDK211                                            
061421       IF SEGMENT-MISSING                                                 
061422         PERFORM BA-INIT-WDK211                                           
061423         MOVE '1'                       TO MATD-KDSEGKEY                  
061424         PERFORM BB-CHECK-VALUE-METRILUX                                  
061426         PERFORM IMS-ISRT-WDK211                                          
061427       ELSE                                                               
061428         PERFORM BB-CHECK-VALUE-METRILUX                                  
061432         PERFORM IMS-REPL-WDK211                                          
061433       END-IF                                                             
061435     END-IF                                                               
061436                                                                          
061438     .                                                                    
061439     EJECT                                                                
061485 BA-INIT-WDK211 SECTION.                                                  
061486     SKIP2                                                                
061487                                                                          
061488     MOVE ZERO                         TO  MATD-KDVSOP                    
061489                                           MATD-KVHEIGHT-BTO              
061490                                           MATD-KVHEIGHT-NTO              
061491                                           MATD-KVLENGTH-BTO              
061492                                           MATD-KVLENGTH-NTO              
061493                                           MATD-KVWIDTH-BTO               
061494                                           MATD-KVWIDTH-NTO               
061495                                           MATD-TIUPPDAT                  
061496                                           MATD-VKART-BTO                 
061497                                           MATD-VKART-NTO                 
061498                                           MATD-VLARTNTO                  
061499     .                                                                    
061500     EJECT                                                                
061501 BB-CHECK-VALUE-METRILUX SECTION.                                         
061502     SKIP2                                                                
061503                                                                          
061504     IF MID-FLEMB = 'Y' OR 'J'                                            
061505** (GROSS VALUES)                                                         
061506        IF MID-KVHEIGHT > 0                                               
061507           MOVE MID-KVHEIGHT TO MATD-KVHEIGHT-BTO                         
061508        END-IF                                                            
061509                                                                          
061510        IF MID-KVLENGTH > 0                                               
061511           MOVE MID-KVLENGTH TO MATD-KVLENGTH-BTO                         
061512        END-IF                                                            
061513                                                                          
061514        IF MID-KVWIDTH > 0                                                
061515           MOVE MID-KVWIDTH TO MATD-KVWIDTH-BTO                           
061516        END-IF                                                            
061517                                                                          
061518        IF MID-VKART > 0                                                  
061519           MOVE MID-VKART TO MATD-VKART-BTO                               
061520        END-IF                                                            
061521                                                                          
061522        IF MID-KDVSOP > 0                                                 
061523           MOVE MID-KDVSOP TO MATD-KDVSOP                                 
061524        END-IF                                                            
061525                                                                          
061526        IF MID-KVHEIGHT > 0 AND                                           
061527           MID-KVLENGTH > 0 AND                                           
061528           MID-KVWIDTH > 0                                                
061529           COMPUTE MATD-VLARTNTO = MID-KVHEIGHT * MID-KVLENGTH *          
061530                                   MID-KVWIDTH                            
061531           MOVE MID-KDVSOP TO MATD-KDVSOP                                 
061532        END-IF                                                            
061533                                                                          
061540        MOVE CURRENT-DATE  TO MATD-TIUPPDAT                               
061600                                                                          
061700     ELSE                                                                 
061800** (NET VALUES)                                                           
061810                                                                          
061820        IF MID-KVHEIGHT > 0                                               
061830           MOVE MID-KVHEIGHT TO MATD-KVHEIGHT-NTO                         
061840        END-IF                                                            
061841                                                                          
061842        IF MID-KVLENGTH > 0                                               
061843           MOVE MID-KVLENGTH TO MATD-KVLENGTH-NTO                         
061844        END-IF                                                            
061845                                                                          
061846        IF MID-KVWIDTH  > 0                                               
061847           MOVE MID-KVWIDTH TO MATD-KVWIDTH-NTO                           
061848        END-IF                                                            
061849                                                                          
061850        IF MID-VKART > 0                                                  
061851           MOVE MID-VKART TO MATD-VKART-NTO                               
061852        END-IF                                                            
061860                                                                          
061870        MOVE CURRENT-DATE  TO MATD-TIUPPDAT                               
061880                                                                          
061900     END-IF                                                               
062000     .                                                                    
062100     EJECT                                                                
062200 C-CHECK-UPPD-WDK611 SECTION.                                             
062210     SKIP2                                                                
062300                                                                          
062500       MOVE NOO TO WS-MACHINE-FLAG                                        
062600                                                                          
062700       PERFORM IMS-GHU-WDK611                                             
062800       IF SEGMENT-FOUND                                                   
062801                                                                          
062810         IF MID-FLEMB = 'Y' OR 'J'                                        
062900** (GROSS VALUES)                                                         
063000           IF MID-VKART > 0                                               
063100              MOVE MATD-VKART-BTO TO CLAG-VKART                           
063200              IF CLAG-VKART-NTO = ZERO OR                                 
063300                 CLAG-VKART-NTO > CLAG-VKART                              
063400                 MOVE CLAG-VKART TO CLAG-VKART-NTO                        
063500                 MOVE '4' TO CLAG-KDUVKNTO                                
063600                 MOVE YES TO WS-MACHINE-FLAG                              
063700              END-IF                                                      
063800           END-IF                                                         
063900                                                                          
064000           IF MID-KDVSOP > 0                                              
064100              MOVE MATD-KDVSOP TO CLAG-KDVSOP                             
064200              MOVE YES TO WS-MACHINE-FLAG                                 
064300           END-IF                                                         
064400                                                                          
064500           IF MID-KVHEIGHT > 0 AND                                        
064600              MID-KVLENGTH > 0 AND                                        
064700              MID-KVWIDTH > 0                                             
064800              MOVE MATD-VLARTNTO TO CLAG-VLARTNTO                         
064900              MOVE YES TO WS-MACHINE-FLAG                                 
065000           END-IF                                                         
065100                                                                          
065200           IF MACHINE-YES                                                 
065300              MOVE 'MACHINE ' TO CLAG-IDUSER-VUPD                         
065400              MOVE CURRENT-DATE TO CLAG-TIUPPDAT-VUPD                     
065500           END-IF                                                         
065510         ELSE                                                             
065520                                                                          
065530** (NET VALUES)                                                           
065540            IF MID-VKART > 0                                              
065550               MOVE MID-VKART TO CLAG-VKART-NTO                           
065560               MOVE '1' TO CLAG-KDUVKNTO                                  
065570               MOVE 'MACHINE ' TO CLAG-IDUSER-VUPD                        
065580               MOVE CURRENT-DATE TO CLAG-TIUPPDAT-VUPD                    
065590                IF  CLAG-VKART = ZERO OR                                  
065591                    CLAG-VKART < CLAG-VKART-NTO                           
065592                    MOVE CLAG-VKART-NTO TO CLAG-VKART                     
065593                END-IF                                                    
065594            END-IF                                                        
065595                                                                          
065600         END-IF                                                           
065700         PERFORM IMS-REPL-WDK611                                          
065800                                                                          
065900       END-IF                                                             
066400                                                                          
066500     .                                                                    
066600     EJECT                                                                
066700 D-CHECK-UPPD-WDK712 SECTION.                                             
066800                                                                          
066810     IF MID-FLEMB = 'Y' OR 'J'                                            
066820** (GROSS VALUES)                                                         
066900                                                                          
066910       IF CLAG-IDDC-REF(1:1) = '7' OR '4'                                 
066920          IF CLAG-IDDC-REF(1:1) = '7'                                     
066930             MOVE WC-LAND-CN     TO W-IDLAND                              
066940          ELSE                                                            
066950             IF CLAG-IDDC-REF(1:1) = '4'                                  
066960                MOVE WC-LAND-US     TO W-IDLAND                           
066970             END-IF                                                       
066980          END-IF                                                          
066990          PERFORM IMS-GHU-WDK712                                          
066991          IF SEGMENT-FOUND                                                
066992             IF MID-VKART > 0                                             
066993                MOVE MATD-VKART-BTO       TO LART-VKART                   
066995             END-IF                                                       
066997             IF MID-KVHEIGHT > 0 AND                                      
066998                MID-KVLENGTH > 0 AND                                      
066999                MID-KVWIDTH  > 0                                          
067000                MOVE MATD-VLARTNTO        TO LART-VLARTNTO                
067001             END-IF                                                       
067002             IF MID-KVHEIGHT > 0 AND                                      
067003                MID-KVLENGTH > 0 AND                                      
067004                MID-KVWIDTH  > 0 AND                                      
067005                MID-VKART    > 0                                          
067006                MOVE 'J'                  TO LART-FLMSKUPD                
067007             END-IF                                                       
067008             PERFORM IMS-REPL-WDK712                                      
067009          END-IF                                                          
067010       END-IF                                                             
067011       PERFORM DA-STARTA-DISPATCHEN                                       
067012     END-IF                                                               
067020     .                                                                    
067100     EJECT                                                                
067200 DA-STARTA-DISPATCHEN SECTION.                                            
067300     MOVE 'DA-STARTA-DISPAT'   TO CURRENT-SECTION                         
067400                                                                          
067500     MOVE SPACE                TO MSG-KOM-WMSGKOM                         
067600     COMPUTE MSG-KOM-KVLL = LENGTH OF MSG-KOM-WMSGKOM                     
067700     MOVE LOW-VALUE            TO MSG-KOM-KDZ1                            
067800     MOVE LOW-VALUE            TO MSG-KOM-KDZ2                            
067900     MOVE SPACE                TO MSG-KOM-KDTRANS                         
068000     MOVE 'W6I19B01'           TO MSG-KOM-IDCPYTXT                        
068100     MOVE 'INLEV   '           TO MSG-KOM-IDSNDNOD                        
068200     MOVE 'W6018B00'           TO MSG-KOM-IDSNDJOB                        
068300     MOVE CURRENT-DATE         TO MSG-KOM-TIREGDAT                        
068400     MOVE CURRENT-TIME         TO MSG-KOM-TIKLOCK                         
068500     MOVE SPACE                TO MSG-KOM-IDMFSMED                        
068600                                                                          
068700     MOVE W-IDARTNR            TO 619B-MID-IDARTNR                        
068800     IF CLAG-IDDC-REF NOT = SPACE                                         
068900        MOVE CLAG-IDDC-REF     TO 619B-MID-IDDC                           
069000     ELSE                                                                 
069100        MOVE WC-CDC-SE         TO 619B-MID-IDDC                           
069200     END-IF                                                               
069300     COMPUTE P-TO-P-KVLL       =  LNG-P-TO-P-PREFIX + 87                  
069400     MOVE 'W6T19BX '           TO P-TO-P-KDTRANS                          
069500     MOVE '619B'               TO P-TO-P-IDTRANS                          
069600     MOVE MFS-KDMFSFOR         TO P-TO-P-KDMFSFOR                         
069700     MOVE 619B-MID-W6I19B01    TO P-TO-P-DATA                             
069800                                                                          
069900     CALL W006KOM USING MSG-PCB                                           
070000                        DISP-PCB                                          
070100                        KOM-WDP8-PCB                                      
070200                        MSG-KOM-WMSGKOM                                   
070300                        P-TO-P-SW                                         
070400     .                                                                    
070500*    --- DISPATCHER SECTIONS                                              
070600 S01-FETCH-REQUEST-ARGUMENT SECTION.                                      
070700                                                                          
070800     MOVE 'GETARG'               TO SUB-KDFUNC                            
070900     MOVE 'CARPARTS.PULS.PARTMETRICS'                                     
071000                                 TO SUB-ADDISPABS                         
071100     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
071200                                                                          
071300     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
071400                                                                          
071500     IF SUB-KDRC > 0                                                      
071600       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
071700       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
071800       DELIMITED BY SIZE INTO ERROR-TEXT                                  
071900       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
072000     END-IF                                                               
072100     .                                                                    
072200     SKIP3                                                                
267100* --- IMS SECTIONS ---                                                    
267200                                                                          
269100 IMS-GU-WDK201 SECTION.                                                   
269200     MOVE 'IMS-GU-WDK201   ' TO CURRENT-IMS-SECTION                       
269300                                                                          
269400     MOVE SPACE               TO ALL-SSA                                  
269500     STRING 'WDK201  (IDARTNR  =' W-IDARTNR-X ')'                         
269600          DELIMITED BY SIZE INTO SSA1                                     
269700     MOVE '  GE'              TO GOOD-STATUSCODES                         
269800     CALL CBLTDLI USING GU WDK2-PCB DLI-IO-WDK201 SSA1                    
269900     MOVE WDK2-STATUS-CODE    TO STATUS-WS                                
270000     PERFORM IMS-STATUSCHECK                                              
270100     .                                                                    
270200                                                                          
270210 IMS-ISRT-WDK201 SECTION.                                                 
270220     MOVE 'WDK201  '       TO SSA1                                        
270230     MOVE '  '             TO GOOD-STATUSCODES                            
270240     CALL CBLTDLI USING ISRT WDK2-PCB DLI-IO-WDK201 SSA1                  
270250     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
270260     PERFORM IMS-STATUSCHECK                                              
270270     .                                                                    
270280                                                                          
270290 IMS-GHNP-WDK211 SECTION.                                                 
270291     MOVE 'WDK211  '       TO SSA1                                        
270292     MOVE '  GE'           TO GOOD-STATUSCODES                            
270293     CALL CBLTDLI USING GHNP WDK2-PCB DLI-IO-WDK211 SSA1                  
270294     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
270295     PERFORM IMS-STATUSCHECK                                              
270296     .                                                                    
270297                                                                          
270298 IMS-ISRT-WDK211 SECTION.                                                 
270299     STRING 'WDK201  (IDARTNR  =' W-IDARTNR-X ')'                         
270300          DELIMITED BY SIZE INTO SSA1                                     
270301     MOVE 'WDK211  '       TO SSA2                                        
270302     MOVE '  '             TO GOOD-STATUSCODES                            
270303     CALL CBLTDLI USING ISRT WDK2-PCB DLI-IO-WDK211 SSA1 SSA2             
270304     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
270305     PERFORM IMS-STATUSCHECK                                              
270306     .                                                                    
270307                                                                          
270308 IMS-REPL-WDK211 SECTION.                                                 
270309     MOVE '  '             TO GOOD-STATUSCODES                            
270310     CALL CBLTDLI USING REPL WDK2-PCB DLI-IO-WDK211                       
270311     MOVE WDK2-STATUS-CODE TO STATUS-WS                                   
270312     PERFORM IMS-STATUSCHECK                                              
270313     .                                                                    
270314     EJECT                                                                
270315                                                                          
270320                                                                          
270400 IMS-GNP-WDK211 SECTION.                                                  
270500     MOVE 'IMS-GNP-WDK211  ' TO CURRENT-IMS-SECTION                       
270600                                                                          
270700     MOVE SPACE               TO ALL-SSA                                  
270800     STRING 'WDK211  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
270900          DELIMITED BY SIZE INTO SSA1                                     
271000     MOVE '  GE'              TO GOOD-STATUSCODES                         
271100     CALL CBLTDLI USING GNP WDK2-PCB DLI-IO-WDK211 SSA1                   
271200     MOVE WDK2-STATUS-CODE    TO STATUS-WS                                
271300     PERFORM IMS-STATUSCHECK                                              
271400     .                                                                    
271500                                                                          
271600                                                                          
271700 IMS-GNP-WDK211 SECTION.                                                  
271800     MOVE 'IMS-GNP-WDK211  ' TO CURRENT-IMS-SECTION                       
271900                                                                          
272000     MOVE SPACE               TO ALL-SSA                                  
272100     STRING 'WDK211  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
272200          DELIMITED BY SIZE INTO SSA1                                     
272300     MOVE '  GE'              TO GOOD-STATUSCODES                         
272400     CALL CBLTDLI USING GNP WDK2-PCB DLI-IO-WDK211 SSA1                   
272500     MOVE WDK2-STATUS-CODE    TO STATUS-WS                                
272600     PERFORM IMS-STATUSCHECK                                              
272700     .                                                                    
272800                                                                          
272900                                                                          
275600 IMS-GU-WDK601 SECTION.                                                   
275700     MOVE 'IMS-GU-WDK601   ' TO CURRENT-IMS-SECTION                       
275800                                                                          
275900     MOVE SPACE               TO ALL-SSA                                  
276000     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
276100          DELIMITED BY SIZE INTO SSA1                                     
276200     MOVE '  GE'              TO GOOD-STATUSCODES                         
276300     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1                    
276400     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
276500     PERFORM IMS-STATUSCHECK                                              
276600     .                                                                    
276700                                                                          
276800 IMS-GHNP-WDK611 SECTION.                                                 
276900     MOVE 'IMS-GHNP-WDK611 ' TO CURRENT-IMS-SECTION                       
277000                                                                          
277100     MOVE SPACE               TO ALL-SSA                                  
277200     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
277300          DELIMITED BY SIZE INTO SSA1                                     
277400     MOVE '  GE'              TO GOOD-STATUSCODES                         
277500     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
277600     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
277700     PERFORM IMS-STATUSCHECK                                              
277800     .                                                                    
277900                                                                          
278000 IMS-GHU-WDK611 SECTION.                                                  
278100     MOVE 'IMS-GHU-WDK611  ' TO CURRENT-IMS-SECTION                       
278200                                                                          
278300     MOVE SPACE               TO ALL-SSA                                  
278400     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
278500          DELIMITED BY SIZE INTO SSA1                                     
278600     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
278700          DELIMITED BY SIZE INTO SSA2                                     
278800     MOVE '  GE'              TO GOOD-STATUSCODES                         
278900     CALL CBLTDLI USING GHU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2              
279000     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
279100     PERFORM IMS-STATUSCHECK                                              
279200     .                                                                    
279300                                                                          
279400 IMS-REPL-WDK611 SECTION.                                                 
279500     MOVE 'IMS-REPL-WDK611 ' TO CURRENT-IMS-SECTION                       
279600                                                                          
279700     MOVE SPACE               TO ALL-SSA                                  
279800     MOVE '  '                TO GOOD-STATUSCODES                         
279900     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
280000     MOVE WDK6-STATUS-CODE    TO STATUS-WS                                
280100     PERFORM IMS-STATUSCHECK                                              
280200     .                                                                    
280300                                                                          
280400                                                                          
280500 IMS-GHU-WDK712 SECTION.                                                  
280600     MOVE 'IMS-GHU-WDK712  ' TO CURRENT-IMS-SECTION                       
280700                                                                          
280800     MOVE SPACE               TO ALL-SSA                                  
280900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
281000          DELIMITED BY SIZE INTO SSA1                                     
281100     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
281200          DELIMITED BY SIZE INTO SSA2                                     
281300     MOVE '  GE'              TO GOOD-STATUSCODES                         
281400     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2              
281500     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
281600     PERFORM IMS-STATUSCHECK                                              
281700     .                                                                    
281800                                                                          
281900                                                                          
282000 IMS-REPL-WDK712 SECTION.                                                 
282100     MOVE 'IMS-REPL-WDK712 ' TO CURRENT-IMS-SECTION                       
282200                                                                          
282300     MOVE SPACE              TO ALL-SSA                                   
282400     MOVE '  '               TO GOOD-STATUSCODES                          
282500     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK712                       
282600     MOVE WDK7-STATUS-CODE   TO STATUS-WS                                 
282700     PERFORM IMS-STATUSCHECK                                              
282800     .                                                                    
282900                                                                          
283000                                                                          
283100 IMS-STATUSCHECK SECTION.                                                 
283200                                                                          
283300     SET STATUS-IX TO 1                                                   
283400     SEARCH GOOD-STATUS                                                   
283500       AT END                                                             
283600         STRING ' INVALID STATUS CODE FROM IMS: ' STATUS-WS               
283700         DELIMITED BY SIZE INTO ERROR-TEXT                                
283800         CALL FELLOG                                                      
283900       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
284000         CONTINUE                                                         
284100     END-SEARCH                                                           
284200     .                                                                    
