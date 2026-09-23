000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4121800.                                                
000300 AUTHOR.         JAIN UMESH.                                              
000400 DATE-WRITTEN.   20/10/02.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700* FUNCTION:                                                               
000800*     PROGRAM TO AUTOMATICALLY RELEASE VOR ORDERS FROM                    
000900*     VOR QUEUE(WDA6) AS FÖRBI ORDERS(422X) EVERY 10 MINS.                
001000*     THE SCHEDULE CAN BE SEEN IN W00507 PGM.                             
001100*                                                                         
001200*     WDA6M INDEX IS USED SO THAT WE START SOLVING THE                    
001300*     OLDEST BACKORDER IN THE VOR QUEUE FIRST.                            
001400*                                                                         
001500*     THIS PGM SENDS IDTRANS AS 'V412' TO BE IDENTIFIED IN                
001600*     FOLLWING PGMS - 4221, 4222, 4223.                                   
001700*                                                                         
001800*     THE PROGRAM READS     WDA6M, WDK6, WDK7, WDB2, WDL2                 
001900*                                                                         
002000* NOTE: WE DON'T HAVE CHECKPOINT LOGIC IN TIS BMP BECOZ                   
002100*       IF THE PGM ABENDS, WE CAN RESTART SINCE THERE WILL BE             
002200*       NOT SO MANY ORDERS TO RE-PROCESS WHENEVER THE RTN IS RUN.         
002300*                                                                         
002400*    ABENDCODES:                                                          
002500*        U0016 -  . . . .                                                 
002600*        U1000 -  . . . .                                                 
002700*                                                                         
002710* 08/SEP/21 STORY 1935252:WEIGHT AND VOLUME CONTROL ON DC 11 TO           
002720*                DETERMINE IF WE SHOULD HAVE FC 17 OR FC DEFAULT          
002730*                                                                         
002800                                                                          
002900     SKIP3                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100     SKIP2                                                                
003200 INPUT-OUTPUT SECTION.                                                    
003300                                                                          
003400 FILE-CONTROL.                                                            
003500     SELECT W41218           ASSIGN TO      W41218D1.                     
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800     SKIP2                                                                
003900 FILE SECTION.                                                            
004000 FD  W41218                                                               
004100     RECORDING V                                                          
004200     BLOCK CONTAINS 0.                                                    
004300 01  W41218-POST                 PIC X(80).                               
004400     EJECT                                                                
004500 WORKING-STORAGE SECTION.                                                 
004600                                                                          
004700 77  IDPGM                       PIC X(8)    VALUE 'W4121800'.            
004800 77  JA                          PIC X       VALUE 'J'.                   
004900 77  NEJ                         PIC X       VALUE 'N'.                   
005000 77  IX                          PIC 99      VALUE ZERO.                  
005100 77  TAB-IX                      PIC 9(5)    VALUE ZERO.                  
005200 77  PART-IX                     PIC 9(5)    VALUE ZERO.                  
005300 77  MAX-TVS-IX                  PIC 9(2)    VALUE 16.                    
005400 77  MAX-SALDO-IX                PIC 9(5)    VALUE 6000.                  
005500 77  IX-DCCLEAR-MAX              PIC 9(2)    VALUE 99.                    
005600 77  WS-ALLOC-DC                 PIC X(2)    VALUE SPACE.                 
005700 77  WS-IDDISTR                  PIC 9(4)    VALUE ZERO.                  
005800 77  WS-IDKUNDNR                 PIC 9(6)    VALUE ZERO.                  
005900 77  WS-IDDISTR-PREV             PIC 9(4)    VALUE ZERO.                  
006000 77  WS-IDKUNDNR-PREV            PIC 9(6)    VALUE ZERO.                  
006100 77  WS-IDORDNR7-PREV            PIC 9(7)    VALUE ZERO.                  
006200 77  WS-IDARTNR-PREV             PIC 9(9)    VALUE ZERO.                  
006300 77  W-KVAKS-SDC                 PIC 9(7)    VALUE ZERO.                  
006400 77  W-KVOKS-DAG                 PIC 9(7)    VALUE ZERO.                  
006500 77  W-KVOKS-BULK                PIC 9(7)    VALUE ZERO.                  
006500 77  W-KVPREAVB-VOR              PIC 9(7)    VALUE ZERO.                  
006600 77  W-DISP                      PIC S9(8)   VALUE ZERO.                  
006700 77  W-DISP-CDC                  PIC S9(8)   VALUE ZERO.                  
006800 77  W-DISP-AK                   PIC S9(8)   VALUE ZERO.                  
006900 77  W-IDTYP-R31                 PIC X(3)    VALUE 'R31'.                 
007000 77  W-DISP-XDC                  PIC S9(8)   VALUE ZERO.                  
007100 77  WS-TOT-VKART                PIC S9(9)   VALUE ZERO.                  
007200*                                                                         
007300 01  WS-DISTR-DC.                                                         
007400     03  WS-DISTR-DC       PIC X(4)   VALUE SPACE.                        
007500     03  WS-IDDC-DC        PIC X(2)   VALUE SPACE.                        
007600                                                                          
007700 01  SALDO-TABLE.                                                         
007800    03  SALDO-PART-DC-TAB   OCCURS 6000 TIMES.                            
007900     05  TAB-IDARTNR          PIC S9(9)   COMP-3 VALUE ZERO.              
008000     05  TAB-IDDC             PIC X(2)    VALUE SPACE.                    
008100     05  TAB-SALDO            PIC S9(8)   VALUE ZERO.                     
008200                                                                          
008300 01  UT-AREA.                                                             
008400     03  FILLER              PIC X(02)    VALUE SPACE.                    
008500     03  UT-IDDISTR          PIC Z(3)9    VALUE ZERO.                     
008600     03  FILLER              PIC X(01)    VALUE ';'.                      
008700     03  UT-IDKUNDNR         PIC Z(5)9    VALUE ZERO.                     
008800     03  FILLER              PIC X(01)    VALUE ';'.                      
008900     03  UT-IDORDNR7         PIC Z(6)9    VALUE ZERO.                     
009000     03  FILLER              PIC X(01)    VALUE ';'.                      
009100     03  UT-IDARTNR          PIC Z(8)9    VALUE ZERO.                     
009200     03  FILLER              PIC X(01)    VALUE ';'.                      
009300     03  UT-VOR-KVBEART-Q    PIC Z(6)9    VALUE ZERO.                     
009400     03  FILLER              PIC X(01)    VALUE ';'.                      
009500     03  UT-IDDC             PIC X(02)    VALUE SPACE.                    
009600     03  FILLER              PIC X(01)    VALUE ';'.                      
009700                                                                          
009800 01  GENERAL-SUBPROGRAMS.                                                 
009900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
010000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010200     SKIP2                                                                
010300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
010400                                                                          
010500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010800     SKIP2                                                                
010900 01  ERROR-TEXT.                                                          
011000     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
011100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
011200     EJECT                                                                
011300*    --- AREAS FOR IMS-SECTIONS                                           
011400*                                                                         
011500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011600                                                                          
011700 01  VK-VL-SW                    PIC X.                                   
011800     88  VK-VL-OK                            VALUE 'J'.                   
011900     88  VK-VL-NOT-OK                        VALUE 'N'.                   
012000                                                                          
012100 01  PART-DC-SW                  PIC X.                                   
012200     88  PART-DC-FOUND                       VALUE 'J'.                   
012300     88  PART-DC-MISSING                     VALUE 'N'.                   
012400                                                                          
012500 01  SALDO-SW                    PIC X.                                   
012600     88  SALDO-FOUND                         VALUE 'J'.                   
012700     88  NO-SALDO                            VALUE 'N'.                   
012800                                                                          
012810 01  SW-DEFAULT-FC               PIC X       VALUE 'J'.                   
012820                                                                          
012900 01  KEYS-FOR-DLI.                                                        
013000     03  W-IDARTNR-X.                                                     
013100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013200                                                                          
013300     03  W-IDDC-X.                                                        
013400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
013500                                                                          
013600     03  W-WDA6MSEQ-MIN-X.                                                
013700         05  W-IDARTNR-MSEQ-MIN      PIC S9(9) VALUE ZERO COMP-3.         
013800         05  FILLER                  PIC X(9)  VALUE LOW-VALUE.           
013900                                                                          
014000     03  W-WDA6MSEQ-MAX-X.                                                
014100         05  W-IDARTNR-MSEQ-MAX      PIC S9(9) VALUE ZERO COMP-3.         
014200         05  FILLER                  PIC X(9)  VALUE HIGH-VALUE.          
014300                                                                          
014400     03  W-IDGMT-X.                                                       
014500         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
014600         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
014700                                                                          
014800*01  -COPY WWDC99                                                         
014900                                                                          
015000*01  -COPY WWDISVOR                                                       
015001                                                                          
015002*01  -COPY WWDIST17                                                       
015100                                                                          
015200*01  -COPY WMSGAREA                                                       
015300*    --- FÖR HOPP TILL 4221-FÖRBI/VOR-ORDER                               
015400       05  4221-MID REDEFINES MSG-MID-OUT.                                
015500*          07  -COPY W4I22101 -PRE 4221-MID-                              
015600                                                                          
015700*    --- STATUS-KOD FRÅN IMS                                              
015800 01  STATUS-WS                   PIC XX.                                  
015900     88  SEGMENT-FOUND                       VALUE '  '.                  
016000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
016200     88  SEGMENT-SLUT                        VALUE 'GB'.                  
016300     SKIP2                                                                
016400 01  GOOD-STATUSCODES.                                                    
016500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016600     SKIP3                                                                
016700 01  SSA1                        PIC X(64).                               
016800 01  SSA2                        PIC X(64).                               
016900     EJECT                                                                
017000*    --- IMS FUNCTION CODES                                               
017100*01  -COPY W0003                                                          
017200     EJECT                                                                
017300*    ---  DLI INPUT-OUTPUT AREA                                           
017400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA601'.                      
017500 01  DLI-IO-WDA601.                                                       
017600*    03  -COPY WDA601                                                     
017700                                                                          
017800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
017900 01  DLI-IO-WDB201.                                                       
018000*    03  -COPY WDB201                                                     
018100                                                                          
018200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
018300 01  DLI-IO-WDK611.                                                       
018400*    03  -COPY WDK611                                                     
018500                                                                          
018600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
018700 01  DLI-IO-WDK711.                                                       
018800*    03  -COPY WDK711                                                     
018900     EJECT                                                                
019000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL201'.                      
019100 01  DLI-IO-WDL201.                                                       
019200*    03  -COPY WDL201                                                     
019300     EJECT                                                                
019000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK901'.                      
019100 01  DLI-IO-WDK901.                                                       
019200*    03  -COPY WDK901                                                     
019300     EJECT                                                                
019400 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL221'.                      
019500 01  DLI-IO-WDL221.                                                       
019600*    03  -COPY WDL221                                                     
019700     EJECT                                                                
019800 LINKAGE SECTION.                                                         
019900                                                                          
020000*01  -COPY W0009  -PRE MSG-                                               
020100                                                                          
020200*01  -COPY W0009  -PRE ALT-4221-                                          
020300                                                                          
020400*01  -COPY W0008  -PRE WDA6M-                                             
020500     05  FILLER                  PIC X.                                   
020600                                                                          
020700*01  -COPY W0008  -PRE WDK6-                                              
020800     05  FILLER                  PIC X.                                   
020900                                                                          
021000*01  -COPY W0008  -PRE WDK7-                                              
021100     05  FILLER                  PIC X.                                   
021200                                                                          
021000*01  -COPY W0008  -PRE WDK9-                                              
021100     05  FILLER                  PIC X.                                   
021200                                                                          
021300*01  -COPY W0008  -PRE WDB2-                                              
021400     05  FILLER                  PIC X.                                   
021500                                                                          
021600*01  -COPY W0008  -PRE WDL2-                                              
021700     05  FILLER                  PIC X.                                   
021800                                                                          
021900                                                                          
022000 PROCEDURE DIVISION  USING MSG-PCB ALT-4221-PCB WDA6M-PCB                 
022100                           WDK6-PCB WDK7-PCB WDK9-PCB                     
022100                           WDB2-PCB WDL2-PCB.                             
022200 MAIN SECTION.                                                            
022300     ENTRY 'DLITCBL' USING MSG-PCB ALT-4221-PCB WDA6M-PCB                 
022400                           WDK6-PCB WDK7-PCB WDK9-PCB                     
022400                           WDB2-PCB WDL2-PCB.                             
022500                                                                          
022600     PERFORM A-INIT                                                       
022700                                                                          
022800     PERFORM IMS-GHN-WDA601-MSEQ                                          
022900                                                                          
023000     PERFORM UNTIL SEGMENT-SLUT                                           
023100                                                                          
023200** READ A6 ONLY FOR CODE 92, 93 AND ALSO SKIP RECORDS WITH                
023300** TEXT MARKED WITH BEWLOW CODES ARE HANDLED MANUALLY                     
023400       IF (VOR-KDORDBEK = 92 OR 93) AND                                   
023500          (VOR-TEVORMRK NOT = 'GE' AND 'PR' AND 'TR' AND                  
023600                              'QC' AND '00' AND 'IN' AND 'SP')            
023700** IF WE HAVE AN ORDER WITH MORE THAN ONE PART, THEN WE                   
023800** PICK ONLY THE FIRST LINE IN CURRENT RUN OF BMP, REST OF THEM           
023900** ARE PROCESSED IN LATER RUNS                                            
024000         IF ((VOR-IDDISTR   = WS-IDDISTR-PREV)  AND                       
024100             (VOR-IDKUNDNR  = WS-IDKUNDNR-PREV) AND                       
024200             (VOR-IDORDNR7  = WS-IDORDNR7-PREV))                          
024300*           (VOR-IDARTNR   NOT = WS-IDARTNR-PREV))                        
024400           CONTINUE                                                       
024500         ELSE                                                             
024600           MOVE NEJ   TO SALDO-SW                                         
024700           MOVE SPACE TO WS-ALLOC-DC                                      
024800           MOVE JA    TO VK-VL-SW                                         
024810                         SW-DEFAULT-FC                                    
024900*                                                                         
025000           MOVE VOR-IDARTNR    TO W-IDARTNR                               
025100           PERFORM IMS-GU-WDK611                                          
025200*                                                                         
025300           IF CLAG-FLAUTREL = NEJ                                         
025400*            IF CLAG-KDFARLIG NOT = 4 AND 6                               
025500               COMPUTE WS-TOT-VKART = VOR-KVBEART-Q * CLAG-VKART          
025600*                                                                         
025700               MOVE VOR-IDDISTR   TO W-IDDISTR                            
025710                                     DIST17-IDDISTR                       
025800               MOVE VOR-IDKUNDNR  TO W-IDKUNDNR                           
025900               PERFORM IMS-GU-WDB201                                      
026000                                                                          
026100***** START TO CHECK FOR QTY ON CLEARING DC'S ON 4414 SCREEN              
026200               MOVE +1 TO IX                                              
026300              PERFORM UNTIL IX > IX-DCCLEAR-MAX OR                        
026310                                 SALDO-FOUND    OR                        
026400                                 GMT-IDDC-VOR (IX) = SPACE                
026500                 MOVE GMT-IDDC-VOR (IX)    TO W-IDDC                      
026600                                              WS-IDDC                     
026601                 IF ((CDC-SE AND                                          
026602                     ((CLAG-KDFARLIG = +4 OR +6) OR                       
026603                      (CLAG-KDFARLIG NOT = +4 AND +6))) OR                
026604                     (CLAG-KDFARLIG NOT = +4 AND +6))                     
026700                   PERFORM C-CHECK-SALDO                                  
026800                   IF SALDO-FOUND                                         
026900                     MOVE GMT-IDDC-VOR (IX)  TO WS-ALLOC-DC               
026901                     IF CDC-SE                                            
026910                        PERFORM E-SET-FC-VALUE                            
026920                     END-IF                                               
027000                     PERFORM D-RELEASE-VOR-ORDER                          
027100                   END-IF                                                 
027200                 END-IF                                                   
027210                 ADD +1 TO IX                                             
027300               END-PERFORM                                                
027400                                                                          
027500***** IF QTY NOT AVAILABLE IN CLEARING DC'S ON 4414 SCREEN                
027600***** START TO CHECK ON TVS DC'S ON 4412 SCREEN                           
027700               IF CLAG-KDFARLIG NOT = 4 AND 6                             
027710                 IF NO-SALDO                                              
027800                   MOVE +1 TO IX                                          
027900                   PERFORM UNTIL IX > MAX-TVS-IX OR SALDO-FOUND OR        
028000                                 GMT-IDDC-TVSVOR (IX) = SPACE             
028100                     MOVE GMT-IDDC-TVSVOR (IX)   TO W-IDDC                
028200                                                    WS-IDDC               
028300                                                    WS-ALLOC-DC           
028400                     PERFORM B-CHECK-VKART-VLART-COPYBOOK                 
028500                     IF VK-VL-OK                                          
028600                       PERFORM C-CHECK-SALDO                              
028700                       IF SALDO-FOUND                                     
028800                         PERFORM D-RELEASE-VOR-ORDER                      
028900                       END-IF                                             
029000                     END-IF                                               
029100                     ADD +1 TO IX                                         
029200                   END-PERFORM                                            
029210                 END-IF                                                   
029300               END-IF                                                     
029400*            END-IF                                                       
029500           END-IF                                                         
029600         END-IF                                                           
029700       END-IF                                                             
029800*                                                                         
029900       PERFORM IMS-GHN-WDA601-MSEQ                                        
030000*                                                                         
030100       MOVE VOR-IDARTNR  TO WS-IDARTNR-PREV                               
030200     END-PERFORM                                                          
030300                                                                          
030400     PERFORM Z-FINIT                                                      
030500                                                                          
030600     MOVE ZERO TO RETURN-CODE                                             
030700     GOBACK                                                               
030800     .                                                                    
030900     EJECT                                                                
031000 A-INIT SECTION.                                                          
031100     OPEN OUTPUT W41218                                                   
031200     ACCEPT VOR-TIUPPDAT FROM DATE                                        
031300     ACCEPT VOR-TIUPPTID FROM TIME                                        
031400     INITIALIZE SALDO-TABLE                                               
031500     DISPLAY  'DIST   IDKUNDNR IDORDNR IDARTNR    DC  FC'                 
031600     .                                                                    
031700                                                                          
031800 B-CHECK-VKART-VLART-COPYBOOK SECTION.                                    
031900** CHECK OF PART IS GT 20KG OR VOLUME GT 0.1M3 OR VSOP > 220              
032000** CHECK THE COPYBOOK WWXXXXX IF DC IS PRESENT FOR CORR DIST.,            
032100** IF SO, LEAVE THE LINE IN VOR QUEUE AND DON'T PROCESS IT.               
032200     COMPUTE WS-IDDISTR = VOR-IDDISTR                                     
032300     MOVE WS-IDDISTR     TO WS-DISTR-DC                                   
032400     MOVE WS-ALLOC-DC    TO WS-IDDC-DC                                    
032500     MOVE JA             TO VK-VL-SW                                      
032600                                                                          
032700     IF WS-TOT-VKART > 20000 OR CLAG-VLARTNTO > 100000                    
032800        OR CLAG-KDVSOP > 220                                              
032900       SEARCH ALL WWDC00-DIST-DC                                          
033000          AT END                                                          
033100             CONTINUE                                                     
033200          WHEN WWDC00-SOK-DIST-DC(WWDC00-IX)                              
033300                                 = WS-DISTR-DC                            
033400             MOVE NEJ    TO VK-VL-SW                                      
033500       END-SEARCH                                                         
033600     END-IF                                                               
033700     .                                                                    
033800                                                                          
033900 C-CHECK-SALDO SECTION.                                                   
034000* CHECK SALDO FIRST IN THE INTERNAL TABLE(UPDATED) AND IF PART/DC         
034100* COMBINATION NOT FOUND THE CHECK SALDO DIRECTLY IN DB                    
034200     PERFORM CA-CHECK-SALDO-TABLE                                         
034300     IF PART-DC-MISSING                                                   
034400       MOVE ZERO TO W-DISP-AK                                             
034500       PERFORM CB-CHECK-SALDO-K6-K7                                       
034600     END-IF                                                               
034700     .                                                                    
034800                                                                          
034900 CA-CHECK-SALDO-TABLE SECTION.                                            
035000** IF PART/DC ALREADY EXIST IN TABLE, CHECK SALDO AND UPDATE IT           
035100** ACCORDINGLY                                                            
035200     MOVE NEJ  TO PART-DC-SW                                              
035300     MOVE +1   TO TAB-IX                                                  
035400*                                                                         
035500     PERFORM UNTIL TAB-IX > MAX-SALDO-IX OR SALDO-FOUND OR                
035600                   PART-DC-FOUND OR TAB-IDARTNR (TAB-IX) = ZERO           
035700       IF VOR-IDARTNR = TAB-IDARTNR(TAB-IX) AND                           
035800          WS-IDDC     = TAB-IDDC(TAB-IX)                                  
035900          MOVE JA TO PART-DC-SW                                           
036000          IF TAB-SALDO(TAB-IX) >= VOR-KVBEART-Q                           
036100            MOVE JA   TO SALDO-SW                                         
036200            COMPUTE TAB-SALDO(TAB-IX) = TAB-SALDO(TAB-IX) -               
036300                                        VOR-KVBEART-Q                     
036400          END-IF                                                          
036500       END-IF                                                             
036600       ADD +1 TO TAB-IX                                                   
036700     END-PERFORM                                                          
036800     .                                                                    
036900                                                                          
037000 CB-CHECK-SALDO-K6-K7 SECTION.                                            
037100** IF PART/DC DOESN'T EXIST IN INTERNAL TABLE, READ THE CORR. DB          
037200** AND CHECK IF SALDO IS AVAILABLE. ALSO INSERT IN INTERNAL TABLE.        
037300     MOVE VOR-IDARTNR    TO W-IDARTNR                                     
037400*                                                                         
037500     IF CDC                                                               
037600       PERFORM IMS-GU-WDK611                                              
037600       PERFORM IMS-GU-WDK901                                              
037600       IF SEGMENT-MISSING                                                 
037600         MOVE +0                   TO W-KVPREAVB-VOR                      
037600       ELSE                                                               
037600         IF SEGMENT-FOUND                                                 
038800           IF ART-KVPREAVB-VOR < +0                                       
038800             MOVE +0               TO W-KVPREAVB-VOR                      
038800           ELSE                                                           
038800             MOVE ART-KVPREAVB-VOR TO W-KVPREAVB-VOR                      
038800           END-IF                                                         
037600         END-IF                                                           
037600       END-IF                                                             
037700       PERFORM IMS-GU-WDL201                                              
037800       IF SEGMENT-FOUND                                                   
037900         PERFORM IMS-GNP-WDL221                                           
038000         PERFORM UNTIL SEGMENT-MISSING                                    
038100           IF MOT-KDRT = 0 AND MOT-IDDC = 11                              
038200             COMPUTE W-DISP-AK = MOT-KVAVIS - MOT-KDAVVANT +              
038300                                  W-DISP-AK                               
038400           END-IF                                                         
038500           PERFORM IMS-GNP-WDL221                                         
038600         END-PERFORM                                                      
038700       END-IF                                                             
038800*                                                                         
038900       COMPUTE W-DISP-CDC = CLAG-KVLS + W-DISP-AK - W-KVPREAVB-VOR        
039000       IF W-DISP-CDC >= VOR-KVBEART-Q                                     
039100         MOVE JA TO SALDO-SW                                              
039200       END-IF                                                             
039300     ELSE                                                                 
039400       PERFORM IMS-GU-WDK711                                              
039500       IF SEGMENT-FOUND                                                   
039600         IF SLAG-KVAKS-SDC < +0                                           
039700           MOVE +0             TO W-KVAKS-SDC                             
039800         ELSE                                                             
039900           MOVE SLAG-KVAKS-SDC TO W-KVAKS-SDC                             
040000         END-IF                                                           
040100         IF SLAG-KVOKS-DAG < +0                                           
040200           MOVE +0             TO W-KVOKS-DAG                             
040300         ELSE                                                             
040400           MOVE SLAG-KVOKS-DAG TO W-KVOKS-DAG                             
040500         END-IF                                                           
040600         IF SLAG-KVOKS-BULK < +0                                          
040700           MOVE +0              TO W-KVOKS-BULK                           
040800         ELSE                                                             
040900           MOVE SLAG-KVOKS-BULK TO W-KVOKS-BULK                           
041000         END-IF                                                           
041100         COMPUTE W-DISP-XDC = SLAG-KVLS    +                              
041200                              W-KVAKS-SDC  -                              
041300                              W-KVOKS-DAG  -                              
041400                              W-KVOKS-BULK                                
041500         IF W-DISP-XDC > ZERO                                             
041600           COMPUTE W-DISP-XDC = W-DISP-XDC       -                        
041700                                SLAG-KVUTRS -                             
041800                                SLAG-KVSPARR-KVAL                         
041900         END-IF                                                           
042000         IF W-DISP-XDC >= VOR-KVBEART-Q                                   
042100           MOVE JA TO SALDO-SW                                            
042200         END-IF                                                           
042300       END-IF                                                             
042400     END-IF                                                               
042500                                                                          
042600*IF THE PART/DC MISSING IN INTERNAL TABLE, INSERT IT.                     
042700     IF PART-DC-MISSING                                                   
042800       PERFORM CBA-INSERT-IDARTNR-TABLE                                   
042900     END-IF                                                               
043000     .                                                                    
043100                                                                          
043200 CBA-INSERT-IDARTNR-TABLE SECTION.                                        
043300* FOR EVERY NEW COMBINATION OF PART/DC INSERT INTO TABLE                  
043400     IF CDC                                                               
043500       COMPUTE W-DISP = W-DISP-CDC                                        
043600     ELSE                                                                 
043700       COMPUTE W-DISP = W-DISP-XDC                                        
043800     END-IF                                                               
043900*                                                                         
044000     ADD +1 TO PART-IX                                                    
044100     MOVE VOR-IDARTNR TO TAB-IDARTNR(PART-IX)                             
044200     MOVE WS-IDDC     TO TAB-IDDC(PART-IX)                                
044300     IF SALDO-FOUND                                                       
044400       COMPUTE TAB-SALDO(TAB-IX) = W-DISP - VOR-KVBEART-Q                 
043500     ELSE                                                                 
044400       IF W-DISP > ZERO                                                   
044400         MOVE W-DISP    TO TAB-SALDO(TAB-IX)                              
044400       ELSE                                                               
044400         MOVE ZERO      TO TAB-SALDO(TAB-IX)                              
044400       END-IF                                                             
044400     END-IF                                                               
045200     .                                                                    
045300                                                                          
045400 D-RELEASE-VOR-ORDER SECTION.                                             
045500     MOVE VOR-IDDISTR   TO WS-IDDISTR-PREV                                
045600     MOVE VOR-IDKUNDNR  TO WS-IDKUNDNR-PREV                               
045700     MOVE VOR-IDORDNR7  TO WS-IDORDNR7-PREV                               
045800*                                                                         
045900     MOVE '1'               TO VOR-KDVORATG                               
046000     MOVE '**'              TO VOR-TEVORMRK                               
046100     MOVE MSG-SIGNON-USERID TO VOR-IDUSER                                 
046200*                                                                         
046300     PERFORM IMS-REPL-WDA601                                              
046400                                                                          
046500*  --- SKAPA MID FÖR FÖRBI (FLFORBI=J)                                    
046600     COMPUTE MSG-KVLL = LENGTH OF 4221-MID-W4I22101 + 17                  
046700     MOVE LOW-VALUE    TO MSG-KDZ1                                        
046800                          MSG-KDZ2                                        
046900     MOVE 'W4T221'     TO MSG-KDTRANS-1                                   
047000     MOVE 'V412'       TO MSG-IDTRANS-1                                   
047100     MOVE ' '          TO MSG-KDMFSFOR-1                                  
047200                                                                          
047300     MOVE ALL '+'      TO 4221-MID-W4I22101                               
047301                                                                          
047310     IF SW-DEFAULT-FC  = NEJ                                              
047320         MOVE '17'     TO 4221-MID-KDFRAKT                                
047330     END-IF                                                               
047340                                                                          
047400     MOVE JA           TO 4221-MID-FLVORKO                                
047500     MOVE WS-ALLOC-DC  TO 4221-MID-IDDC-TVS                               
047600     MOVE VOR-IDORDNR7 (3:5)                                              
047700                       TO 4221-MID-BEKUNDRF                               
047800     MOVE '0'          TO 4221-MID-KDORDKL                                
047900                                                                          
048000     MOVE VOR-IDDISTR  TO WS-IDDISTR                                      
048100     MOVE WS-IDDISTR   TO 4221-MID-IDDISTR                                
048200                                                                          
048300     MOVE VOR-IDKUNDNR TO WS-IDKUNDNR                                     
048400     MOVE WS-IDKUNDNR  TO 4221-MID-IDKUNDNR                               
048500     MOVE JA           TO 4221-MID-FLFORBI                                
048600     DISPLAY   VOR-IDDISTR '/' VOR-IDKUNDNR                               
048700                           '/' VOR-IDORDNR7                               
048800                           '/' VOR-IDARTNR                                
048900                           '/' WS-ALLOC-DC                                
048910                           '/' 4221-MID-KDFRAKT                           
049000                                                                          
049100     MOVE VOR-IDDISTR  TO UT-IDDISTR                                      
049200     MOVE VOR-IDKUNDNR TO UT-IDKUNDNR                                     
049300     MOVE VOR-IDORDNR7 TO UT-IDORDNR7                                     
049400     MOVE VOR-IDARTNR  TO UT-IDARTNR                                      
049500     MOVE VOR-KVBEART-Q                                                   
049600                       TO UT-VOR-KVBEART-Q                                
049700     MOVE WS-ALLOC-DC  TO UT-IDDC                                         
049800     WRITE W41218-POST FROM UT-AREA AFTER 1                               
049900                                                                          
050000     PERFORM IMS-PURGE-4221-MSG                                           
050100     .                                                                    
050200                                                                          
050300 E-SET-FC-VALUE SECTION.                                                  
050400                                                                          
050410     IF DIST17-CDC-VOR-FC                                                 
050420        IF WS-TOT-VKART > 20000 OR CLAG-VLARTNTO > 100000                 
050430           OR CLAG-KDVSOP > 220 OR (CLAG-KDFARLIG = 4 AND 6)              
050440             MOVE JA                 TO SW-DEFAULT-FC                     
050450        ELSE                                                              
050460             MOVE NEJ                TO SW-DEFAULT-FC                     
050470        END-IF                                                            
050480     ELSE                                                                 
050490        MOVE JA                      TO SW-DEFAULT-FC                     
050491     END-IF                                                               
050500     .                                                                    
050510                                                                          
050520 Z-FINIT SECTION.                                                         
050530     CLOSE W41218                                                         
050540     .                                                                    
050600     EJECT                                                                
050700 S99-ABEND SECTION.                                                       
050800                                                                          
050900     CALL ABEND USING RKOD-ABEND                                          
051000     .                                                                    
051100     EJECT                                                                
051200* --- IMS SECTIONS  ---                                                   
051300                                                                          
051400     EJECT                                                                
051500 IMS-PURGE-4221-MSG SECTION.                                              
051600                                                                          
051700     MOVE SPACE TO GOOD-STATUSCODES                                       
051800     CALL CBLTDLI USING PURG ALT-4221-PCB MSG-IO-AREA                     
051900     MOVE ALT-4221-STATUS-CODE TO STATUS-WS                               
052000     PERFORM IMS-STATUSCHECK                                              
052100     .                                                                    
052200     EJECT                                                                
052300 IMS-GU-WDB201 SECTION.                                                   
052400                                                                          
052500     STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
052600          DELIMITED BY SIZE INTO SSA1                                     
052700     MOVE '  ' TO GOOD-STATUSCODES                                        
052800     CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
052900     MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
053000     PERFORM IMS-STATUSCHECK                                              
053100     .                                                                    
053200 IMS-GHN-WDA601-MSEQ SECTION.                                             
053300                                                                          
053400     MOVE 'WDA601' TO SSA1                                                
053500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
053600     CALL CBLTDLI USING GHN WDA6M-PCB DLI-IO-WDA601 SSA1                  
053700     MOVE WDA6M-STATUS-CODE TO STATUS-WS                                  
053800     PERFORM IMS-STATUSCHECK                                              
053900     .                                                                    
054000     EJECT                                                                
054100 IMS-REPL-WDA601 SECTION.                                                 
054200                                                                          
054300     MOVE '  ' TO GOOD-STATUSCODES                                        
054400     CALL CBLTDLI USING REPL WDA6M-PCB DLI-IO-WDA601                      
054500     MOVE WDA6M-STATUS-CODE TO STATUS-WS                                  
054600     PERFORM IMS-STATUSCHECK                                              
054700     .                                                                    
054800     EJECT                                                                
054900 IMS-GU-WDK611 SECTION.                                                   
055000                                                                          
055100     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
055200          DELIMITED BY SIZE INTO SSA1                                     
055300     MOVE 'WDK611' TO SSA2                                                
055400     MOVE '  ' TO GOOD-STATUSCODES                                        
055500     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
055600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
055700     PERFORM IMS-STATUSCHECK                                              
055800     .                                                                    
055900     EJECT                                                                
056000 IMS-GU-WDK711 SECTION.                                                   
056100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
056200            DELIMITED BY SIZE INTO SSA1                                   
056300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
056400            DELIMITED BY SIZE INTO SSA2                                   
056500     MOVE '  GE' TO GOOD-STATUSCODES                                      
056600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
056700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
056800     PERFORM IMS-STATUSCHECK                                              
056900     .                                                                    
056000 IMS-GU-WDK901 SECTION.                                                   
056100     STRING 'WDK901  (IDARTNR  =' W-IDARTNR-X ')'                         
056200            DELIMITED BY SIZE INTO SSA1                                   
056500     MOVE '  GE' TO GOOD-STATUSCODES                                      
056600     CALL CBLTDLI USING GU WDK9-PCB DLI-IO-WDK901 SSA1                    
056700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
056800     PERFORM IMS-STATUSCHECK                                              
056900     .                                                                    
057000 IMS-GU-WDL201 SECTION.                                                   
057100     STRING 'WDL201  (IDARTNR  =' W-IDARTNR-X ')'                         
057200          DELIMITED BY SIZE INTO SSA1                                     
057300     MOVE '  GE' TO GOOD-STATUSCODES                                      
057400     CALL CBLTDLI USING GU WDL2-PCB DLI-IO-WDL201 SSA1                    
057500     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
057600     PERFORM IMS-STATUSCHECK                                              
057700     .                                                                    
057800 IMS-GNP-WDL221 SECTION.                                                  
057900     STRING 'WDL221  (IDPTYP   =' W-IDTYP-R31 ')'                         
058000          DELIMITED BY SIZE INTO SSA1                                     
058100     MOVE '  GE' TO GOOD-STATUSCODES                                      
058200     CALL CBLTDLI USING GNP WDL2-PCB DLI-IO-WDL221 SSA1                   
058300     MOVE WDL2-STATUS-CODE TO STATUS-WS                                   
058400     PERFORM IMS-STATUSCHECK                                              
058500     .                                                                    
058600 IMS-STATUSCHECK SECTION.                                                 
058700                                                                          
058800     SET STATUS-IX TO 1                                                   
058900     SEARCH GOOD-STATUS                                                   
059000       AT END                                                             
059100         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
059200           DELIMITED BY SIZE INTO ERROR-TEXT                              
059300         DISPLAY ERROR-TEXT                                               
059400         CALL FELLOG                                                      
059500       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
059600         CONTINUE                                                         
059700     END-SEARCH                                                           
059800     .                                                                    
