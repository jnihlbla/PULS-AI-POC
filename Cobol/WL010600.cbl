000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     WL010600.                                                
000300 AUTHOR.         BODIL LINDAHL.                                           
000400 DATE-WRITTEN.   03/11/14.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    NAMN:CARPARTS.LDC.WL0106                                             
000800*    WEB-LDC: WL010600 PROGRAM IS A REPLICA OF W6030600 PROGRAM           
000900*             AND CUSTOMIZED FOR WEB-LDC REQUIREMENTS.                    
001000*                                                                         
001100*    FUNKTION:                                                            
001200*        LOCATION ENQUIRY LDC                                             
001300*        PROGRAM READS FOLLOWING DATABASE WLARTR = WDK7A                  
001400*                                         WLBENA = WDD311                 
001500*    INDATA.                                                              
001600*        TRANSAKTION: WL0106T                                             
001700*        REQUEST:     WZ01REQU                                            
001800*                     WL0106I1                                            
001900*                                                                         
002000*    UTDATA.                                                              
002100*        RESPONSE:    WZ01RESP                                            
002200*                     WL0106O1                                            
002300*                                                                         
002400 ENVIRONMENT DIVISION.                                                    
002500     SKIP2                                                                
002600 INPUT-OUTPUT SECTION.                                                    
002700                                                                          
002800 FILE-CONTROL.                                                            
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     EJECT                                                                
003400 WORKING-STORAGE SECTION.                                                 
003500*    -- CHECKED BY WY2000                                                 
003600 77  IDPGM                       PIC X(08) VALUE 'WL010600'.              
003700 77  WS-ADRESS                   PIC X(50) VALUE                          
003800          'CARPARTS.LDC.LOCATIONINQUIRY'.                                 
004000 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
004100 77  ERROR-TEXT                  PIC X(80) VALUE SPACE.                   
004200 77  KDRC-DISPLAY                PIC Z(5).                                
004300 77  JA                          PIC X      VALUE 'J'.                    
004400 77  NEJ                         PIC X      VALUE 'N'.                    
004500 77  INDX                        PIC S9(4)  VALUE +0    COMP SYNC.        
004600 77  MAX-INDX                    PIC S9(4)  VALUE +9999 COMP SYNC.        
004700 77  WS-KVANT                    PIC S9(4)  VALUE +0    COMP SYNC.        
004710 77  WS-LAENGD-RESP              PIC S9(9)  VALUE +0    COMP SYNC.        
004800 77  WS-ADLAGOMR                 PIC 9(2).                                
004900 77  WS-ADGANG                   PIC 9(2).                                
005000 77  WS-ADPLATS                  PIC 9(5).                                
005100 77  WS-IDSKYLT-CN               PIC X(3)  VALUE 'RCN'.                   
005200 77  WS-IDSKYLT-GB               PIC X(3)  VALUE 'GB '.                   
005300 77  WS-CP-UTF8                  PIC X(4)  VALUE 'UTF8'.                  
005400 77  WS-CP-278                   PIC X(3)  VALUE '278'.                   
005500                                                                          
005600 77  NYCKLAR-SW                  PIC X      VALUE 'J'.                    
005700     88  NYCKLAR-OK                         VALUE 'J'.                    
005800     88  NYCKLAR-FEL                        VALUE 'N'.                    
005900                                                                          
006000 77  RKOD-ABEND                  PIC S9(4)  COMP VALUE +0.                
006100 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)  COMP VALUE +16.               
006200 77  RKOD-ABEND-MED-DUMP         PIC S9(4)  COMP VALUE +1000.             
006300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)  COMP VALUE +1000.             
006400 01  WS-YYMMDDHHMM.                                                       
006500     03 WS-YYMMDD                PIC  9(6).                               
006600     03 WS-TIME                  PIC  9(4).                               
006700                                                                          
006800 01  WS-HHMMSSTH                 PIC  9(8).                               
006900 01  FILLER REDEFINES WS-HHMMSSTH.                                        
007000       03  WS-HHMM               PIC 9(4).                                
007100       03  WS-SSTH               PIC 9(4).                                
007200     EJECT                                                                
007300*    --- VALID DC CODES                                                   
007400*01  -COPY WWDC99                                                         
007500     EJECT                                                                
007600*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007700 01  GENERELLA-SUBPROGRAM.                                                
007800     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
007900     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
008000     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
008100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008300     03  WTRAUTF8                PIC X(8)    VALUE 'WTRAUTF8'.            
008400     EJECT                                                                
008500 01  MESSAGE-CODES.                                                       
008600     03  INVALID-KEY-FIELDS      PIC X(3)  VALUE '022'.                   
008700     03  TOO-MANY-LINES          PIC X(3)  VALUE '028'.                   
008800     03  SYSTEM-ERROR            PIC X(3)  VALUE '099'.                   
008900     03  KEYS-ARE-MISSING        PIC X(3)  VALUE '041'.                   
009000     03  LINES-NOT-FOUND         PIC X(3)  VALUE '027'.                   
009100     EJECT                                                                
009200 01  FILLER                      PIC X(16) VALUE 'WTRAUTF8-AREA'.         
009300*01  -COPY WTRAUTF8                                                       
009400     EJECT                                                                
009500                                                                          
009600 01  FILLER                      PIC X(16)   VALUE 'SUB-CONTROL'.         
009700*01  -COPY WZ01SUB                                                        
009800     EJECT                                                                
009900                                                                          
010000 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
010100*01  -COPY WZ01SEND                                                       
010200                                                                          
010300     EJECT                                                                
010400                                                                          
010500 01  FILLER                      PIC X(16)   VALUE 'REQU-AREA'.           
010600 01  REQU-AREA.                                                           
010700*    03  -COPY WZ01REQU                                                   
010800*    03  -COPY WL0106I1                                                   
010900     EJECT                                                                
011000 01  FILLER                      PIC X(16)   VALUE 'RESP-AREA'.           
011100 01  RESP-AREA.                                                           
011200*    03  -COPY WZ01RESP                                                   
011300*    03  -COPY WL0106O1                                                   
011400     EJECT                                                                
011500 01  FILLER                      PIC X(16)   VALUE 'HDR-AREA'.            
011600 01  HDR-AREA.                                                            
011700*    03  -COPY WZ01REQU  -PRE HDR-                                        
011800*    03  -COPY WZ04HDR                                                    
011900*                                                                         
012000 01  FILLER                      PIC X(16)   VALUE 'DOC-AREA'.            
012100 01  DOC-AREA.                                                            
012200*    03  -COPY WL01061                                                    
012300*                                                                         
012400     EJECT                                                                
012500*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
012600*                                                                         
012700 01  FILLER                  PIC X(16)   VALUE 'IMS-WS'.                  
012800 01  NYCKLAR-TILL-DLI.                                                    
012900   03   W-IDARTNR-X.                                                      
013000     05 W-IDARTNR            PIC S9(9)  COMP-3  VALUE ZERO.               
013100                                                                          
013200   03   W-IDDC-X.                                                         
013300     05 W-IDDC               PIC X(2) VALUE SPACE.                        
013400                                                                          
013500   03   W-IDSKYLT-X.                                                      
013600     05 W-IDSKYLT            PIC X(3).                                    
         03  W-IDDC-B6-X.                                                       
           05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                     
013700                                                                          
013800   03  WDK7A1KY-MIN-X.                                                    
013900     05  W-IDDC-MIN          PIC X(2)           VALUE SPACE.              
014000     05  W-ADART-MIN.                                                     
014100       07  W-ADLAGOMR-MIN    PIC S9(3)  COMP-3.                           
014200       07  W-ADGANG-MIN      PIC S9(3)  COMP-3.                           
014300       07  W-ADPLATS-MIN     PIC S9(5)  COMP-3.                           
014400     05 W-IDARTNR-MIN        PIC S9(9)  COMP-3.                           
014500                                                                          
014600   03  WDK7A1KY-MAX-X.                                                    
014700     05  W-IDDC-MAX          PIC X(2)           VALUE SPACE.              
014800     05  W-ADART-MAX.                                                     
014900       07  W-ADLAGOMR-MAX    PIC S9(3)  COMP-3.                           
015000       07  W-ADGANG-MAX      PIC S9(3)  COMP-3.                           
015100       07  W-ADPLATS-MAX     PIC S9(5)  COMP-3.                           
015200     05 W-IDARTNR-MAX        PIC S9(9)  COMP-3.                           
015300     SKIP2                                                                
015310** KEYS DECLARATION FOR WDK811                                            
015320     03  W-WDD8ASEQ-MIN-X.                                                
015330         05  W-IDDC-WDD8-MIN      PIC X(2)         VALUE SPACE.           
015340         05  W-ADBUFFOM-WDD8-MIN  PIC S9(3) COMP-3 VALUE ZERO.            
015360         05  W-ADBUFGAN-WDD8-MIN  PIC S9(3) COMP-3 VALUE ZERO.            
015370         05  W-ADBUFPL-WDD8-MIN   PIC S9(5) COMP-3 VALUE ZERO.            
015380                                                                          
015390     03  W-WDD8ASEQ-MAX-X.                                                
015391         05  W-IDDC-WDD8-MAX      PIC X(2)         VALUE SPACE.           
015392         05  W-ADBUFFOM-WDD8-MAX  PIC S9(3) COMP-3 VALUE +999.            
015394         05  W-ADBUFGAN-WDD8-MAX  PIC S9(3) COMP-3 VALUE +999.            
015395         05  W-ADBUFPL-WDD8-MAX   PIC S9(5) COMP-3 VALUE +99999.          
015396                                                                          
015400*    --- STATUS-KOD FRÅN IMS                                              
015500 01  STATUS-WS                   PIC XX.                                  
015600     88  STATUS-OK                           VALUE '  '.                  
015700     88  SEGMENT-FINNS                       VALUE '  '.                  
015800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015900     88  BASEN-SLUT                          VALUE 'GB'.                  
016000     88  TRANSKOD-FEL                        VALUE 'A1'.                  
016100     88  SECURITY-FEL                        VALUE 'A4'.                  
016200     SKIP2                                                                
016300 01  GODK-STATUSKODER.                                                    
016400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016500     SKIP3                                                                
016600 01  SSA1                        PIC X(128).                              
016700 01  SSA2                        PIC X(128).                              
016800     EJECT                                                                
016900*    --- IMS FUNKTIONSKODER                                               
017000*01  -COPY W0003                                                          
017100     EJECT                                                                
017200*    ---  DLI INPUT-OUTPUT AREA                                           
017300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
017400     SKIP3                                                                
017500 01  DLI-IO-AREA.                                                         
017600     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
017700     SKIP3                                                                
017800     03  WLARTR   REDEFINES IO-AREA.                                      
017900*        05  -COPY WDK7A1                                                 
018000     EJECT                                                                
018100     03  WLBENA     REDEFINES IO-AREA.                                    
018200*        05  -COPY WDD311                                                 
018210*                                                                         
018220 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
018300 01  DLI-IO-WDK711.                                                       
018400*    03  -COPY WDK711                                                     
018401*                                                                         
018410 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD801'.                      
018420 01  DLI-IO-WDD801.                                                       
018430*    03  -COPY WDD801                                                     
018440*                                                                         
018450 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD811'.                      
018460 01  DLI-IO-WDD811.                                                       
018470*    03  -COPY WDD811                                                     
       01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
       01   DLI-IO-AREA-B601.                                                   
      *     03  -COPY WDB601                                                    
018480*                                                                         
018500     EJECT                                                                
018600 LINKAGE SECTION.                                                         
018700*01  -COPY W0009  -PRE MSG-                                               
018800     EJECT                                                                
018900 01  DISTRDOC-PCB                PIC X.                                   
019000     EJECT                                                                
019100                                                                          
019200*01  -COPY W0008  -PRE ARTR-                                              
019300     05  FILLER                  PIC X.                                   
019400     EJECT                                                                
019500*01  -COPY W0008  -PRE WDK7-                                              
019600     05  FILLER                  PIC X.                                   
019700     EJECT                                                                
019800*01  -COPY W0008  -PRE BENA-                                              
019900     05  FILLER                  PIC X.                                   
020000     EJECT                                                                
020010*01  -COPY W0008  -PRE WDD8-                                              
020020     05  FILLER                  PIC X.                                   
020030     EJECT                                                                
      *01  -COPY W0008  -PRE WDB6-                                              
           05  FILLER                  PIC X.                                   
           EJECT                                                                
020100 PROCEDURE DIVISION  USING MSG-PCB DISTRDOC-PCB                           
020200                           ARTR-PCB WDK7-PCB BENA-PCB WDD8-PCB            
                                 WDB6-PCB.                                      
020300     ENTRY 'DLITCBL' USING MSG-PCB DISTRDOC-PCB                           
020400                           ARTR-PCB WDK7-PCB BENA-PCB WDD8-PCB            
                                 WDB6-PCB.                                      
020500 MAIN SECTION.                                                            
020600                                                                          
020700     PERFORM S01-HAEMTA-ANROPSDATA                                        
020800     IF SUB-KDRC = 0                                                      
020900        PERFORM A-INIT                                                    
021000        PERFORM B-KOLLA-NYCKLAR                                           
021100        IF NYCKLAR-OK                                                     
021200          IF REQU-KDPGMACT = 'S'                                          
021300            PERFORM F-LAES-VISA-INFO                                      
021400          ELSE                                                            
021500            IF REQU-KDPGMACT = 'P'                                        
021600              PERFORM G-PRINT-LIST                                        
021700            END-IF                                                        
021800          END-IF                                                          
021900        END-IF                                                            
022000        PERFORM S02-RETURNERA-SVAR                                        
022100     END-IF                                                               
022200                                                                          
022300     MOVE ZERO TO RETURN-CODE                                             
022400     GOBACK                                                               
022500     .                                                                    
022600     EJECT                                                                
022700 A-INIT SECTION.                                                          
022800                                                                          
022900     MOVE ALL '+'    TO RESP-AREA                                         
023000     MOVE ZERO       TO RESP-KVRADER                                      
023100     MOVE SPACE      TO RESP-IDMSG-ERROR                                  
023200                        RESP-IDMSG-INFO                                   
023300                        RESP-IDELMT-ERROR                                 
023400     MOVE '001'      TO RESP-IDMSGVER                                     
023500     MOVE ZERO       TO W-IDARTNR-MIN                                     
023600     MOVE LOW-VALUE  TO WDK7A1KY-MIN-X                                    
023700     MOVE HIGH-VALUE TO WDK7A1KY-MAX-X                                    
023800                                                                          
023900     ACCEPT WS-YYMMDD      FROM DATE                                      
024000     ACCEPT WS-HHMMSSTH    FROM TIME                                      
024100     MOVE WS-HHMM          TO WS-TIME                                     
           MOVE REQU-IDDC-KEY TO W-IDDC-B6                                      
           PERFORM IMS-GU-WDB601                                                
024200     .                                                                    
024300     EJECT                                                                
024400 B-KOLLA-NYCKLAR SECTION.                                                 
024500                                                                          
024600     MOVE JA                TO NYCKLAR-SW                                 
024700                                                                          
024800***  KONTROLL AV REQU-KDPGMACT                                            
024900     IF REQU-KDPGMACT = 'S' OR 'P'                                        
025000        CONTINUE                                                          
025100     ELSE                                                                 
025200        MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                             
025300        MOVE 'KDPGMACT'   TO RESP-IDELMT-ERROR                            
025400        MOVE NEJ TO NYCKLAR-SW                                            
025500     END-IF                                                               
025600                                                                          
025700***  KONTROLL AV ADLAGOMR                                                 
025800     IF REQU-ADLAGOMR-KEY = ALL '+'                                       
025810       IF REQU-KDCMDVAL = 'BUF'                                           
025820         MOVE ZERO TO REQU-ADLAGOMR-KEY                                   
025830       ELSE                                                               
025900         MOVE NEJ TO NYCKLAR-SW                                           
026000         MOVE 'ADLAGOMR' TO RESP-IDELMT-ERROR                             
026010       END-IF                                                             
026100     ELSE                                                                 
026200       INSPECT REQU-ADLAGOMR-KEY REPLACING LEADING SPACE BY ZERO          
026300       IF REQU-ADLAGOMR-KEY NUMERIC                                       
026400          MOVE REQU-ADLAGOMR-KEY TO WS-ADLAGOMR                           
026500       ELSE                                                               
026600          MOVE REQU-ADLAGOMR-KEY TO WS-ADLAGOMR                           
026700          MOVE 'ADLAGOMR' TO RESP-IDELMT-ERROR                            
026800       END-IF                                                             
026900     END-IF                                                               
027000                                                                          
027100***  KONTROLL AV ADGANG                                                   
027200     IF NYCKLAR-OK                                                        
027300       IF REQU-ADGANG-KEY = ALL '+'                                       
027310         IF REQU-KDCMDVAL = 'BUF'                                         
027320           MOVE ZERO TO REQU-ADGANG-KEY                                   
027330         ELSE                                                             
027400           MOVE 'ADGANG' TO RESP-IDELMT-ERROR                             
027500           MOVE NEJ TO NYCKLAR-SW                                         
027510         END-IF                                                           
027600       ELSE                                                               
027700         INSPECT REQU-ADGANG-KEY REPLACING LEADING SPACE BY ZERO          
027800         IF REQU-ADGANG-KEY NUMERIC                                       
027900            MOVE REQU-ADGANG-KEY TO WS-ADGANG                             
028000         ELSE                                                             
028100            MOVE REQU-ADGANG-KEY TO WS-ADGANG                             
028200            MOVE 'ADGANG' TO RESP-IDELMT-ERROR                            
028300            MOVE NEJ TO NYCKLAR-SW                                        
028400         END-IF                                                           
028500       END-IF                                                             
028600     END-IF                                                               
028700                                                                          
028800***  KONTROLL AV ADPLATS                                                  
028900     IF REQU-ADPLATS-KEY = ALL '+'                                        
029000        CONTINUE                                                          
029100**      MOVE ZERO TO WS-ADPLATS                                           
029200     ELSE                                                                 
029300       INSPECT REQU-ADPLATS-KEY REPLACING LEADING SPACE BY ZERO           
029400       IF REQU-ADPLATS-KEY NUMERIC                                        
029500          MOVE REQU-ADPLATS-KEY TO WS-ADPLATS                             
029600       ELSE                                                               
029700          MOVE REQU-ADPLATS-KEY TO WS-ADPLATS                             
029800          MOVE 'ADPLATS' TO RESP-IDELMT-ERROR                             
029900          MOVE NEJ TO NYCKLAR-SW                                          
030000       END-IF                                                             
030100     END-IF                                                               
030200                                                                          
030300***  KONTROLL AV IDDC                                                     
030400     MOVE REQU-IDDC-KEY TO RESP-IDDC-KEY                                  
030500                           WS-IDDC                                        
030510                           W-IDDC-WDD8-MIN                                
030520                           W-IDDC-WDD8-MAX                                
030600                                                                          
030700                                                                          
030800     IF NYCKLAR-OK                                                        
030900       IF WS-ADLAGOMR NUMERIC                                             
031000        MOVE WS-ADLAGOMR     TO W-ADLAGOMR-MIN                            
031100                                W-ADLAGOMR-MAX                            
031200                                                                          
031300       END-IF                                                             
031400                                                                          
031500       IF WS-ADGANG   NUMERIC                                             
031600        MOVE WS-ADGANG       TO W-ADGANG-MIN                              
031700                                W-ADGANG-MAX                              
031800       END-IF                                                             
031900                                                                          
032000       IF WS-ADPLATS  NUMERIC                                             
032100        MOVE WS-ADPLATS      TO W-ADPLATS-MIN                             
032200                                W-ADPLATS-MAX                             
032300       END-IF                                                             
032400       MOVE RESP-IDDC-KEY   TO W-IDDC-MIN                                 
032500                               W-IDDC-MAX                                 
032600                               W-IDDC                                     
032700*      MOVE 99999999        TO W-IDARTNR-MAX                              
032800     END-IF                                                               
032900                                                                          
033000     MOVE WS-ADLAGOMR TO RESP-ADLAGOMR-KEY                                
033100*    INSPECT RESP-ADLAGOMR-KEY REPLACING LEADING ZERO BY SPACE            
033200     MOVE WS-ADGANG   TO RESP-ADGANG-KEY                                  
033300*    INSPECT RESP-ADGANG-KEY REPLACING LEADING ZERO BY SPACE              
033400     MOVE WS-ADPLATS  TO RESP-ADPLATS-KEY                                 
033500*    INSPECT RESP-ADPLATS-KEY REPLACING LEADING ZERO BY SPACE             
033600                                                                          
033700     IF NYCKLAR-FEL                                                       
033800        IF RESP-IDELMT-ERROR = 'KDPGMACT'                                 
033900           MOVE SYSTEM-ERROR TO RESP-IDMSG-ERROR                          
034000        ELSE                                                              
034100           MOVE INVALID-KEY-FIELDS TO RESP-IDMSG-ERROR                    
034200        END-IF                                                            
034300     END-IF                                                               
034400     .                                                                    
034500     EJECT                                                                
034600 F-LAES-VISA-INFO      SECTION.                                           
034601     IF REQU-KDCMDVAL = 'LOC'                                             
034602       MOVE 'LOC' TO RESP-KDCMDVAL                                        
034604       PERFORM FA-LAES-VISA-INFO-LOC                                      
034605     ELSE                                                                 
034606       IF REQU-KDCMDVAL = 'BUF'                                           
034607         MOVE 'BUF' TO RESP-KDCMDVAL                                      
034608         PERFORM FB-LAES-VISA-INFO-BUF                                    
034609       END-IF                                                             
034610     END-IF                                                               
034611     .                                                                    
034620     EJECT                                                                
034700                                                                          
034710 FA-LAES-VISA-INFO-LOC SECTION.                                           
034720                                                                          
034800     MOVE ZERO TO WS-KVANT                                                
034900     PERFORM IMS-GN-ARTR01                                                
035000                                                                          
035100     IF SEGMENT-SAKNAS                                                    
035200        MOVE LINES-NOT-FOUND TO RESP-IDMSG-ERROR                          
035300        MOVE ZERO             TO RESP-KVRADER                             
035400     ELSE                                                                 
035500        MOVE +1 TO INDX                                                   
035600        PERFORM UNTIL SEGMENT-SAKNAS  OR                                  
035700                      BASEN-SLUT      OR                                  
035800                      WS-KVANT = MAX-INDX                                 
035900            IF INDX <= MAX-INDX                                           
036000               PERFORM FAA-BUILD-LINES-LOC                                
036100               ADD 1 TO INDX                                              
036200            END-IF                                                        
036300            ADD +1 TO WS-KVANT                                            
036400            PERFORM IMS-GN-ARTR01                                         
036500        END-PERFORM                                                       
036700        MOVE WS-KVANT TO RESP-KVRADER                                     
037100                                                                          
037200        IF WS-KVANT >= MAX-INDX AND                                       
037300           SEGMENT-FINNS                                                  
037400           MOVE TOO-MANY-LINES TO RESP-IDMSG-ERROR                        
037500        END-IF                                                            
037600     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900 FAA-BUILD-LINES-LOC SECTION.                                             
038000                                                                          
038100     MOVE SEQA-IDARTNR  TO RESP-IDARTNR(INDX)                             
038200                           W-IDARTNR                                      
038300     MOVE SEQA-ADLAGOMR TO RESP-ADLAGOMR(INDX)                            
038400     MOVE SEQA-ADGANG   TO RESP-ADGANG(INDX)                              
038500     MOVE SEQA-ADPLATS  TO RESP-ADPLATS (INDX)                            
038600                                                                          
038700     PERFORM IMS-GU-WDK711                                                
038800     MOVE SLAG-KVLS     TO RESP-KVLS(INDX)                                
038900     MOVE SLAG-KVPB-REF TO RESP-KVPB-REF(INDX)                            
039000*    MOVE 'GB '         TO W-IDSKYLT                                      
           MOVE DCS-IDSKYLT-DB        TO W-IDSKYLT                              
           IF DCS-UNICODE-IDSKYLT                                               
              MOVE 'UTF8'             TO TRAUTF8-KDCP                           
           ELSE                                                                 
              MOVE '278 '             TO TRAUTF8-KDCP                           
           END-IF                                                               
039900                                                                          
040000     PERFORM IMS-GET-BENA-TEXT                                            
040100     IF SEGMENT-FINNS                                                     
040200        MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                    
040300     ELSE                                                                 
040400        MOVE SPACES             TO TRAUTF8-TECONV-FROM                    
040600     END-IF                                                               
           IF TRAUTF8-TECONV-FROM = SPACES                                      
            MOVE 'GB'  TO W-IDSKYLT                                             
            MOVE '278' TO TRAUTF8-KDCP                                          
            PERFORM IMS-GET-BENA-TEXT                                           
            MOVE TEXT-BEART    TO TRAUTF8-TECONV-FROM                           
           END-IF                                                               
040700*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
040800     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
040900                                                                          
041000*    -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                        
041100     MOVE TRAUTF8-TECONV-TO     TO RESP-BEART-ENG(INDX)                   
041200     .                                                                    
041300     EJECT                                                                
041310 FB-LAES-VISA-INFO-BUF SECTION.                                           
041320                                                                          
041330     MOVE ZERO TO WS-KVANT                                                
041331     INSPECT REQU-ADLAGOMR-KEY REPLACING LEADING SPACE BY ZERO            
041332     MOVE REQU-ADLAGOMR-KEY   TO W-ADBUFFOM-WDD8-MIN                      
041333     IF REQU-ADLAGOMR-KEY > 0                                             
041334       MOVE REQU-ADLAGOMR-KEY TO W-ADBUFFOM-WDD8-MAX                      
041335     END-IF                                                               
041336                                                                          
041337     INSPECT REQU-ADGANG-KEY REPLACING LEADING SPACE BY ZERO              
041338     MOVE REQU-ADGANG-KEY     TO W-ADBUFGAN-WDD8-MIN                      
041339     IF REQU-ADGANG-KEY > 0                                               
041340       MOVE REQU-ADGANG-KEY   TO W-ADBUFGAN-WDD8-MAX                      
041341     END-IF                                                               
041342                                                                          
041343     IF REQU-ADPLATS-KEY = ALL '+'                                        
041344       MOVE ZEROS TO REQU-ADPLATS-KEY                                     
041345     END-IF                                                               
041346     INSPECT REQU-ADPLATS-KEY REPLACING LEADING SPACE BY ZERO             
041347     MOVE REQU-ADPLATS-KEY    TO  W-ADBUFPL-WDD8-MIN                      
041348     IF REQU-ADPLATS-KEY > 0                                              
041349       MOVE REQU-ADPLATS-KEY  TO W-ADBUFPL-WDD8-MAX                       
041350     END-IF                                                               
041351     PERFORM IMS-GU-WDD8-ASEQ                                             
041353                                                                          
041360     IF SEGMENT-SAKNAS                                                    
041370        MOVE LINES-NOT-FOUND TO RESP-IDMSG-ERROR                          
041380        MOVE ZERO             TO RESP-KVRADER                             
041390     ELSE                                                                 
041391        MOVE +1 TO INDX                                                   
041392        PERFORM UNTIL SEGMENT-SAKNAS  OR                                  
041393                      BASEN-SLUT      OR                                  
041394                      WS-KVANT = MAX-INDX                                 
041395            IF INDX <= MAX-INDX                                           
041396               PERFORM FBA-BUILD-LINES-BUF                                
041397               ADD 1 TO INDX                                              
041398            END-IF                                                        
041399            ADD +1 TO WS-KVANT                                            
041400            PERFORM IMS-GN-WDD8-ASEQ                                      
041401        END-PERFORM                                                       
041403        MOVE WS-KVANT TO RESP-KVRADER                                     
041407                                                                          
041408        IF WS-KVANT >= MAX-INDX AND                                       
041409           SEGMENT-FINNS                                                  
041410           MOVE TOO-MANY-LINES TO RESP-IDMSG-ERROR                        
041411        END-IF                                                            
041412     END-IF                                                               
041413     .                                                                    
041414     EJECT                                                                
041415 FBA-BUILD-LINES-BUF SECTION.                                             
041422     MOVE SALDO-ADBUFFOMR  TO RESP-ADLAGOMR(INDX)                         
041423     MOVE SALDO-ADBUFFGANG TO RESP-ADGANG(INDX)                           
041424     MOVE SALDO-ADBUFFPL   TO RESP-ADPLATS (INDX)                         
041425     COMPUTE RESP-KVLS(INDX) = SALDO-KVBUFF-F +                           
041426                               SALDO-KVBUFF-OF                            
041427                                                                          
041428     PERFORM IMS-GNP-WDD801                                               
041429     MOVE ART-IDARTNR      TO RESP-IDARTNR(INDX)                          
041430                              W-IDARTNR                                   
041431     PERFORM IMS-GU-WDK711                                                
041432     IF SEGMENT-FINNS                                                     
041433       MOVE SLAG-KVPB-REF  TO RESP-KVPB-REF(INDX)                         
041434     END-IF                                                               
041435                                                                          
041436     IF NDC-CN OR LDC-CN                                                  
041437        MOVE WS-IDSKYLT-CN      TO W-IDSKYLT                              
041438        MOVE WS-CP-UTF8         TO TRAUTF8-KDCP                           
041439     ELSE                                                                 
041440        MOVE WS-IDSKYLT-GB      TO W-IDSKYLT                              
041441        MOVE WS-CP-278          TO TRAUTF8-KDCP                           
041442     END-IF                                                               
041443                                                                          
041444     PERFORM IMS-GET-BENA-TEXT                                            
041445     IF SEGMENT-FINNS                                                     
041446        MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                    
041447     ELSE                                                                 
041448        MOVE SPACES             TO TRAUTF8-TECONV-FROM                    
041449        MOVE WS-CP-278          TO TRAUTF8-KDCP                           
041450     END-IF                                                               
041451*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
041452     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
041453                                                                          
041454*    -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                        
041455     MOVE TRAUTF8-TECONV-TO     TO RESP-BEART-ENG(INDX)                   
041456     .                                                                    
041457     EJECT                                                                
041460 G-PRINT-LIST SECTION.                                                    
041461     IF REQU-KDCMDVAL = 'LOC'                                             
041462       MOVE 'LOC' TO RESP-KDCMDVAL                                        
041463       PERFORM GA-PRINT-LIST-LOC                                          
041464     ELSE                                                                 
041465       IF REQU-KDCMDVAL = 'BUF'                                           
041466         MOVE 'BUF' TO RESP-KDCMDVAL                                      
041467         PERFORM GB-PRINT-LIST-BUF                                        
041468       END-IF                                                             
041469     END-IF                                                               
041470     .                                                                    
041471     EJECT                                                                
041480 GA-PRINT-LIST-LOC SECTION.                                               
041500     MOVE ZERO TO WS-KVANT                                                
041600     PERFORM IMS-GN-ARTR01                                                
041700                                                                          
041800     IF SEGMENT-SAKNAS                                                    
041900        MOVE LINES-NOT-FOUND TO RESP-IDMSG-ERROR                          
042000        MOVE ZERO             TO RESP-KVRADER                             
042100     ELSE                                                                 
042200        PERFORM S90-OPEN-DAP-SEND                                         
042300        MOVE 001             TO HDR-REQU-IDMSGVER                         
042400        MOVE SPACE           TO HDR-REQU-KDPGMACT                         
042500        MOVE REQU-IDUSER     TO HDR-REQU-IDUSER                           
042600                                                                          
042700        MOVE 'LOCATION-QUERY'    TO HDR-IDOUTTYPE                         
042800        MOVE SPACE               TO HDR-IDOUTREC                          
042900        MOVE REQU-IDDC-KEY       TO HDR-IDOUTREC(1:2)                     
043000        MOVE REQU-IDUSER         TO HDR-IDOUTREC(3:8)                     
043100*****   MOVE 'DR'                TO HDR-IDOUTREC(11:2)                    
043200        MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                            
043300        PERFORM S90-PUT-DAP-HEADER                                        
043400                                                                          
043500        MOVE +1 TO INDX                                                   
043600        PERFORM UNTIL SEGMENT-SAKNAS  OR                                  
043700                      BASEN-SLUT      OR                                  
043800                      WS-KVANT = MAX-INDX                                 
043900            IF INDX <= MAX-INDX                                           
044000               PERFORM GAA-BUILD-PRINT-LINES-LOC                          
044100               ADD 1 TO INDX                                              
044200            END-IF                                                        
044300            ADD +1 TO WS-KVANT                                            
044400            PERFORM IMS-GN-ARTR01                                         
044500        END-PERFORM                                                       
044600        MOVE WS-KVANT TO RESP-KVRADER                                     
044700        PERFORM S90-CLOSE-DAP-SEND                                        
044800        MOVE '015'   TO RESP-IDMSG-INFO                                   
044900     END-IF                                                               
045000     .                                                                    
045100     EJECT                                                                
045200 GAA-BUILD-PRINT-LINES-LOC  SECTION.                                      
045300** KOPIA PÅ FA SECTIONEN                                                  
045400     MOVE 'LINE'        TO DOC-IDAFPRCD                                   
045500     MOVE W-IDDC        TO DOC-IDDC                                       
045600     MOVE SEQA-IDARTNR  TO DOC-IDARTNR                                    
045700     MOVE SEQA-IDARTNR  TO RESP-IDARTNR(INDX)                             
045800                           W-IDARTNR                                      
045900     MOVE SEQA-ADLAGOMR TO DOC-ADLAGOMR                                   
046000                           RESP-ADLAGOMR(INDX)                            
046100     MOVE SEQA-ADGANG   TO DOC-ADGANG                                     
046200                           RESP-ADGANG(INDX)                              
046300     MOVE SEQA-ADPLATS  TO DOC-ADPLATS                                    
046400                           RESP-ADPLATS(INDX)                             
046500                                                                          
046600     PERFORM IMS-GU-WDK711                                                
046700     MOVE SLAG-KVLS     TO DOC-KVLS                                       
046800                           RESP-KVLS(INDX)                                
046900     MOVE SLAG-KVPB-REF TO DOC-KVPB-REF                                   
047000                           RESP-KVPB-REF(INDX)                            
047100*    MOVE 'GB '         TO W-IDSKYLT                                      
047200     IF NDC-CN OR LDC-CN                                                  
047300        MOVE WS-IDSKYLT-CN      TO W-IDSKYLT                              
047400        MOVE WS-CP-UTF8         TO TRAUTF8-KDCP                           
047500     ELSE                                                                 
047600        MOVE WS-IDSKYLT-GB      TO W-IDSKYLT                              
047700        MOVE WS-CP-278          TO TRAUTF8-KDCP                           
047800     END-IF                                                               
047900                                                                          
048000     PERFORM IMS-GET-BENA-TEXT                                            
048100     IF SEGMENT-FINNS                                                     
048200        MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                    
048300     ELSE                                                                 
048400        MOVE SPACES             TO TRAUTF8-TECONV-FROM                    
048600     END-IF                                                               
048700*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
048800     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
048900                                                                          
049000*    -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                        
049100     MOVE TRAUTF8-TECONV-TO   TO DOC-BEART                                
049200                                 RESP-BEART-ENG(INDX)                     
049300     PERFORM S90-PUT-DOC                                                  
049400     .                                                                    
049500     EJECT                                                                
049510 GB-PRINT-LIST-BUF SECTION.                                               
049520     MOVE ZERO TO WS-KVANT                                                
049521     INSPECT REQU-ADLAGOMR-KEY REPLACING LEADING SPACE BY ZERO            
049522     MOVE REQU-ADLAGOMR-KEY   TO W-ADBUFFOM-WDD8-MIN                      
049523     IF REQU-ADLAGOMR-KEY > 0                                             
049524       MOVE REQU-ADLAGOMR-KEY TO W-ADBUFFOM-WDD8-MAX                      
049525     END-IF                                                               
049526                                                                          
049527     INSPECT REQU-ADGANG-KEY REPLACING LEADING SPACE BY ZERO              
049528     MOVE REQU-ADGANG-KEY     TO W-ADBUFGAN-WDD8-MIN                      
049529     IF REQU-ADGANG-KEY > 0                                               
049530       MOVE REQU-ADGANG-KEY   TO W-ADBUFGAN-WDD8-MAX                      
049531     END-IF                                                               
049532                                                                          
049533     IF REQU-ADPLATS-KEY = ALL '+'                                        
049534       MOVE ZEROS TO REQU-ADPLATS-KEY                                     
049535     END-IF                                                               
049536     INSPECT REQU-ADPLATS-KEY REPLACING LEADING SPACE BY ZERO             
049537     MOVE REQU-ADPLATS-KEY    TO  W-ADBUFPL-WDD8-MIN                      
049538     IF REQU-ADPLATS-KEY > 0                                              
049539       MOVE REQU-ADPLATS-KEY  TO W-ADBUFPL-WDD8-MAX                       
049540     END-IF                                                               
049541                                                                          
049553     PERFORM IMS-GU-WDD8-ASEQ                                             
049554                                                                          
049555     IF SEGMENT-SAKNAS                                                    
049560        MOVE LINES-NOT-FOUND TO RESP-IDMSG-ERROR                          
049570        MOVE ZERO             TO RESP-KVRADER                             
049580     ELSE                                                                 
049590        PERFORM S90-OPEN-DAP-SEND                                         
049591        MOVE 001             TO HDR-REQU-IDMSGVER                         
049592        MOVE SPACE           TO HDR-REQU-KDPGMACT                         
049593        MOVE REQU-IDUSER     TO HDR-REQU-IDUSER                           
049594                                                                          
049595        MOVE 'LOCATION-QUERY'    TO HDR-IDOUTTYPE                         
049596        MOVE SPACE               TO HDR-IDOUTREC                          
049597        MOVE REQU-IDDC-KEY       TO HDR-IDOUTREC(1:2)                     
049598        MOVE REQU-IDUSER         TO HDR-IDOUTREC(3:8)                     
049599*****   MOVE 'DR'                TO HDR-IDOUTREC(11:2)                    
049600        MOVE WS-YYMMDDHHMM       TO HDR-IDLIST                            
049601        PERFORM S90-PUT-DAP-HEADER                                        
049602                                                                          
049603        MOVE +1 TO INDX                                                   
049604        PERFORM UNTIL SEGMENT-SAKNAS  OR                                  
049605                      BASEN-SLUT      OR                                  
049606                      WS-KVANT = MAX-INDX                                 
049607            IF INDX <= MAX-INDX                                           
049608               PERFORM GBA-BUILD-PRINT-LINES-BUF                          
049609               ADD 1 TO INDX                                              
049610            END-IF                                                        
049611            ADD +1 TO WS-KVANT                                            
049612            PERFORM IMS-GN-WDD8-ASEQ                                      
049613        END-PERFORM                                                       
049614        MOVE WS-KVANT TO RESP-KVRADER                                     
049615        PERFORM S90-CLOSE-DAP-SEND                                        
049616        MOVE '015'   TO RESP-IDMSG-INFO                                   
049617     END-IF                                                               
049618     .                                                                    
049619     EJECT                                                                
049620 GBA-BUILD-PRINT-LINES-BUF  SECTION.                                      
049621** KOPIA PÅ FA SECTIONEN                                                  
049622     MOVE 'LINE'        TO DOC-IDAFPRCD                                   
049623     MOVE W-IDDC        TO DOC-IDDC                                       
049627     MOVE SALDO-ADBUFFOMR  TO DOC-ADLAGOMR                                
049628                              RESP-ADLAGOMR(INDX)                         
049629     MOVE SALDO-ADBUFFGANG TO DOC-ADGANG                                  
049630                              RESP-ADGANG(INDX)                           
049631     MOVE SALDO-ADBUFFPL   TO DOC-ADPLATS                                 
049632                              RESP-ADPLATS(INDX)                          
049633     COMPUTE RESP-KVLS(INDX) = SALDO-KVBUFF-F +                           
049634                               SALDO-KVBUFF-OF                            
049635     MOVE RESP-KVLS(INDX)  TO  DOC-KVLS                                   
049636                                                                          
049637     PERFORM IMS-GNP-WDD801                                               
049638     MOVE ART-IDARTNR      TO RESP-IDARTNR(INDX)                          
049639                              DOC-IDARTNR                                 
049640                              W-IDARTNR                                   
049641                                                                          
049642     PERFORM IMS-GU-WDK711                                                
049645     MOVE SLAG-KVPB-REF TO DOC-KVPB-REF                                   
049646                           RESP-KVPB-REF(INDX)                            
049647*    MOVE 'GB '         TO W-IDSKYLT                                      
049648     IF NDC-CN OR LDC-CN                                                  
049649        MOVE WS-IDSKYLT-CN      TO W-IDSKYLT                              
049650        MOVE WS-CP-UTF8         TO TRAUTF8-KDCP                           
049651     ELSE                                                                 
049652        MOVE WS-IDSKYLT-GB      TO W-IDSKYLT                              
049653        MOVE WS-CP-278          TO TRAUTF8-KDCP                           
049654     END-IF                                                               
049655                                                                          
049656     PERFORM IMS-GET-BENA-TEXT                                            
049657     IF SEGMENT-FINNS                                                     
049658        MOVE TEXT-BEART         TO TRAUTF8-TECONV-FROM                    
049659     ELSE                                                                 
049660        MOVE SPACES             TO TRAUTF8-TECONV-FROM                    
049661     END-IF                                                               
049662*    -- STRIP SPACE OR CONVERT TO UNICODE                                 
049663     CALL WTRAUTF8 USING TRAUTF8-AREA                                     
049664                                                                          
049665*    -- MOVE CONVERTED DESCRIPTION TO THE RESPONSE                        
049666     MOVE TRAUTF8-TECONV-TO   TO DOC-BEART                                
049667                                 RESP-BEART-ENG(INDX)                     
049668     PERFORM S90-PUT-DOC                                                  
049669     .                                                                    
049670     EJECT                                                                
049680 S01-HAEMTA-ANROPSDATA SECTION.                                           
049700                                                                          
049800     MOVE 'GETARG'               TO SUB-KDFUNC                            
049900     MOVE WS-ADRESS              TO SUB-ADDISPABS                         
050000     MOVE LENGTH OF REQU-AREA    TO SUB-KVDLEN                            
050100                                                                          
050200     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN REQU-AREA             
050300                                                                          
050400     IF SUB-KDRC > 0                                                      
050500       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
050600       STRING 'WZ01SUB GETARG ERROR RC=' KDRC-DISPLAY                     
050700       DELIMITED BY SIZE INTO FELTEXT                                     
050800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
050900     END-IF                                                               
051000     .                                                                    
051100     SKIP3                                                                
051200 S02-RETURNERA-SVAR SECTION.                                              
051300                                                                          
051400     COMPUTE WS-LAENGD-RESP = LENGTH OF RESP-AREA                         
051500        - LENGTH OF RESP-TABELLRAD * (MAX-INDX - WS-KVANT)                
051510                                                                          
051600     MOVE 'RETURN'                   TO SUB-KDFUNC                        
051700     MOVE WS-LAENGD-RESP             TO SUB-KVDLEN                        
051800                                                                          
051900     CALL WZ01SUB USING SUB-CONTROL-AREA SUB-KVDLEN RESP-AREA             
052000                                                                          
052100     IF SUB-KDRC > 0                                                      
052200       MOVE SUB-KDRC TO KDRC-DISPLAY                                      
052300       STRING 'WZ01SUB RETURN ERROR RC=' KDRC-DISPLAY                     
052400       DELIMITED BY SIZE INTO FELTEXT                                     
052500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900 S90-OPEN-DAP-SEND SECTION.                                               
053000*    MOVE 'S90-OPEN-DAP-S' TO CURR-SECTION                                
053100                                                                          
053200     MOVE 'CARPARTS.DAP.DISTRDOCWEB' TO SEND-ADDISPABS                    
053300     MOVE 'OPEN'                     TO SEND-KDFUNC                       
053400     CALL WZ01SEND USING SEND-CONTROL-AREA                                
053500                         SEND-OPEN-AREA                                   
053600     IF SEND-KDRC > ZERO                                                  
053700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
053800       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
053900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
054000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
054100     END-IF                                                               
054200     .                                                                    
054300                                                                          
054400 S90-CLOSE-DAP-SEND SECTION.                                              
054500*    MOVE 'S90-CLOSE-DAP-' TO CURR-SECTION                                
054600                                                                          
054700     MOVE 'CLOSE'                    TO SEND-KDFUNC                       
054800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
054900                                                                          
055000     IF SEND-KDRC > 0                                                     
055100       MOVE SEND-KDRC TO KDRC-DISPLAY                                     
055200       STRING 'WZ01SEND CLOSE ERROR RC=' KDRC-DISPLAY                     
055300       DELIMITED BY SIZE INTO ERROR-TEXT                                  
055400       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
055500     END-IF                                                               
055600     .                                                                    
055700                                                                          
055800 S90-PUT-DAP-HEADER SECTION.                                              
055900*    MOVE 'S90-PUT-DAP-HE' TO CURR-SECTION                                
056000                                                                          
056100     MOVE 'PUT'                           TO SEND-KDFUNC                  
056200     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
056300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
056400                         SEND-KVDLEN                                      
056500                         HDR-AREA                                         
056600     IF SEND-KDRC > ZERO                                                  
056700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
056800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
056900       DELIMITED BY SIZE INTO ERROR-TEXT                                  
057000       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
057100     END-IF                                                               
057200     .                                                                    
057300 S90-PUT-DOC      SECTION.                                                
057400*    MOVE 'S90-PUT-DOC ' TO CURR-SECTION                                  
057500                                                                          
057600     MOVE 'PUT'                           TO SEND-KDFUNC                  
057700     MOVE LENGTH OF DOC-AREA              TO SEND-KVDLEN                  
057800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
057900                         SEND-KVDLEN                                      
058000                         DOC-AREA                                         
058100     IF SEND-KDRC > ZERO                                                  
058200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
058300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
058400       DELIMITED BY SIZE INTO ERROR-TEXT                                  
058500       CALL ABEND USING RKOD-ABEND-WITH-DUMP                              
058600     END-IF                                                               
058700     .                                                                    
058800***               ---- IMS SEKTIONER ----                                 
058900 IMS-GU-ARTR01 SECTION.                                                   
059000     STRING 'WLARTR01(WDK7A1KY=>' WDK7A1KY-MIN-X                          
059100                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
059200          DELIMITED BY SIZE INTO SSA1                                     
059300     MOVE '  GE' TO GODK-STATUSKODER                                      
059400     CALL CBLTDLI USING GU ARTR-PCB DLI-IO-AREA SSA1                      
059500     MOVE ARTR-STATUS-CODE TO STATUS-WS                                   
059600     PERFORM IMS-STATUS-KONTROLL                                          
059700     .                                                                    
059800     SKIP3                                                                
059900 IMS-GN-ARTR01 SECTION.                                                   
060000     STRING 'WLARTR01(WDK7A1KY=>' WDK7A1KY-MIN-X                          
060100                    '&WDK7A1KY=<' WDK7A1KY-MAX-X ')'                      
060200          DELIMITED BY SIZE INTO SSA1                                     
060300     MOVE '  GE' TO GODK-STATUSKODER                                      
060400     CALL CBLTDLI USING GN ARTR-PCB DLI-IO-AREA SSA1                      
060500     MOVE ARTR-STATUS-CODE TO STATUS-WS                                   
060600     PERFORM IMS-STATUS-KONTROLL                                          
060700     .                                                                    
060800     SKIP3                                                                
060900 IMS-GU-WDK711 SECTION.                                                   
061000                                                                          
061100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
061200     DELIMITED BY SIZE INTO SSA1                                          
061300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
061400     DELIMITED BY SIZE INTO SSA2                                          
061500     MOVE '  GE' TO GODK-STATUSKODER                                      
061600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711   SSA1 SSA2             
061700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
061800     PERFORM IMS-STATUS-KONTROLL                                          
061900     .                                                                    
062000     SKIP2                                                                
062010 IMS-GU-WDD8-ASEQ SECTION.                                                
062020     STRING 'WDD811  (WDD8ASEQ=>' W-WDD8ASEQ-MIN-X                        
062030                    '&WDD8ASEQ=<' W-WDD8ASEQ-MAX-X ')'                    
062050            DELIMITED BY SIZE INTO SSA1                                   
062060     MOVE '  GE' TO GODK-STATUSKODER                                      
062070     CALL CBLTDLI USING GU WDD8-PCB DLI-IO-WDD811 SSA1                    
062080     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
062090     PERFORM IMS-STATUS-KONTROLL                                          
062091     .                                                                    
062092     SKIP3                                                                
062093 IMS-GNP-WDD801 SECTION.                                                  
062098     MOVE 'WDD801  '  TO SSA1                                             
062099     MOVE '  ' TO GODK-STATUSKODER                                        
062100     CALL CBLTDLI USING GNP WDD8-PCB DLI-IO-WDD801 SSA1                   
062101     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
062102     PERFORM IMS-STATUS-KONTROLL                                          
062103     .                                                                    
062104     SKIP3                                                                
062105 IMS-GN-WDD8-ASEQ SECTION.                                                
062106     STRING 'WDD811  (WDD8ASEQ=>' W-WDD8ASEQ-MIN-X                        
062107                    '&WDD8ASEQ=<' W-WDD8ASEQ-MAX-X ')'                    
062108            DELIMITED BY SIZE INTO SSA1                                   
062109     MOVE '  GE' TO GODK-STATUSKODER                                      
062110     CALL CBLTDLI USING GN WDD8-PCB DLI-IO-WDD811 SSA1                    
062111     MOVE WDD8-STATUS-CODE TO STATUS-WS                                   
062112     PERFORM IMS-STATUS-KONTROLL                                          
062113     .                                                                    
062114     SKIP3                                                                
062120 IMS-GET-BENA-TEXT SECTION.                                               
062200     STRING 'WLBENA01(WDD3BSEQ =' W-IDARTNR-X ')'                         
062300            DELIMITED BY SIZE INTO SSA1                                   
062400     STRING 'WLBENA11(IDSKYLT  =' W-IDSKYLT-X ')'                         
062500            DELIMITED BY SIZE INTO SSA2                                   
062600     MOVE '  GE' TO GODK-STATUSKODER                                      
062700     CALL CBLTDLI USING GU BENA-PCB DLI-IO-AREA SSA1 SSA2                 
062800     MOVE BENA-STATUS-CODE TO STATUS-WS                                   
062900     PERFORM IMS-STATUS-KONTROLL                                          
063000     .                                                                    
063100     EJECT                                                                
       IMS-GU-WDB601 SECTION.                                                   
                                                                                
           STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
                DELIMITED BY SIZE INTO SSA1                                     
           MOVE '  ' TO GODK-STATUSKODER                                        
           CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
           MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
           PERFORM IMS-STATUS-KONTROLL                                          
           .                                                                    
           EJECT                                                                
063200 IMS-STATUS-KONTROLL SECTION.                                             
063300     SET STATUS-IX TO 1                                                   
063400     SEARCH GODK-STATUS                                                   
063500       AT END                                                             
063600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
063700         DELIMITED BY SIZE INTO FELTEXT                                   
063800         CALL FELLOG                                                      
063900       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
064000         CONTINUE                                                         
064100     END-SEARCH                                                           
065000     .                                                                    
