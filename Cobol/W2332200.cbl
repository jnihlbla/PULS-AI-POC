000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.      W2332200.                                               
000400 AUTHOR.          URBAN ZACKRISSON.                                       
000500 DATE-WRITTEN.    NOV 1984.                                               
000600                                                                          
000700                                                                          
000800*    FUNKTION:                                                            
000900*                                                                         
001000*        PROGRAMMMET LÄSER WDK6 MED SB OCH WDK9 WDK7 MED                  
001100*        DLI.  SKRIVER TVÅ FILER                                          
001101*                                                                         
001110*    ÄNDRAD FUNKTION:                                                     
001120*        ETRACKER 4820410. DO NOT INCLUDE OVERSTOCK AT MICRO-LDC          
001130*        TILLKOMMER WDB601-LÄSNING.                                       
001140*                                                                         
001141*                                                                         
001150*    2012-01-04  E-TRACKER 10143271 CHINA WAREHOUSE PROJECT-1             
001160*                                                                         
001200                                                                          
001300                                                                          
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP3                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100     SELECT W23324                       ASSIGN TO UT-S-W23322D1.         
002200     SKIP2                                                                
002300     SELECT W23319                       ASSIGN TO UT-S-W23322D2.         
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP2                                                                
002700 FILE SECTION.                                                            
002800 FD  W23324                                                               
002900     LABEL RECORD   STANDARD                                              
003000     RECORDING      F                                                     
003100     BLOCK CONTAINS 0.                                                    
003200     SKIP2                                                                
003300*01  POST  -COPY W23301     -PRE W23324- -L.                              
003400     EJECT                                                                
003500 FD  W23319                                                               
003600     LABEL RECORD   STANDARD                                              
003700     RECORDING      F                                                     
003800     BLOCK CONTAINS 0.                                                    
003900     SKIP2                                                                
004000*01  POST  -COPY W23319     -PRE W23319- -L.                              
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300                                                                          
004400*    -COPY WY2000W9                                                       
004500     SKIP3                                                                
004600 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W2332200'.            
004610 01  FELTEXT.                                                             
004620     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004630     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
004700 77  JA                          PIC X       VALUE 'J'.                   
004800 77  NEJ                         PIC X       VALUE 'N'.                   
004900 77  SW-SKRIV-19                 PIC X       VALUE 'N'.                   
005000 77  SW-SKRIV-24                 PIC X       VALUE 'N'.                   
005100 77  SW-BEHANDLA-24              PIC X       VALUE 'N'.                   
005200 77  WS-IDINK                    PIC X(4)    VALUE SPACE.                 
005300 77  WS-KVAKS                    PIC S9(7)   VALUE ZERO COMP-3.           
005301                                                                          
005302 01  RKOD                        PIC S9(4)   VALUE +0  COMP SYNC.         
005303                                                                          
005310 01  ARB-SDC.                                                             
005400     03 W-TILLG-SDC              PIC S9(7)   VALUE ZERO COMP-3.           
005500     03 W-MAX-SDC                PIC S9(7)   VALUE ZERO COMP-3.           
005600     03 W-OVERLAGER-SDC          PIC S9(7)   VALUE ZERO COMP-3.           
005610     03 WS-FLOVRLAGBER           PIC X     VALUE SPACE.                   
005620     03 WS-DCS-KDDC              PIC XX    VALUE SPACE.                   
005700                                                                          
005800 01  ARBETSAREOR.                                                         
005900     03  DAGENS-AAVV         PIC 9(4)     VALUE ZERO.                     
006000     03  FILLER REDEFINES DAGENS-AAVV.                                    
006100         05  DAGENS-AA       PIC 9(2).                                    
006200         05  DAGENS-VV       PIC 9(2).                                    
006300     03  TIFINLV-AAVVD       PIC 9(5)     VALUE ZERO.                     
006400     03  FILLER REDEFINES TIFINLV-AAVVD.                                  
006500         05  TIFINLV-AA      PIC 9(2).                                    
006600         05  TIFINLV-VV      PIC 9(2).                                    
006700         05  FILLER          PIC 9(1).                                    
006800     03  DAGENS-ABS-VV       PIC 9(5)     VALUE ZERO.                     
006900     03  TIFINLV-ABS-VV      PIC 9(5)     VALUE ZERO.                     
007000     EJECT                                                                
007100*                                                                         
007200 01  NYCKLAR-TILL-DLI.                                                    
007300                                                                          
007400     03 W-IDARTNR-X.                                                      
007500       05 W-IDARTNR             PIC S9(9)  VALUE ZERO  COMP-3.            
007600*      --- VALID IDDC CODES                                               
007700*                                                                         
007800*01    -COPY WWDC99                                                       
007900       EJECT                                                              
008000                                                                          
008100 01  DYNAMISKA-SUBPROGRAM.                                                
008200     03  POSTSUM     PIC X(8)    VALUE 'POSTSUM '.                        
008300     03  CBLTDLI     PIC X(8)    VALUE 'CBLTDLI '.                        
008500     03  FELLOG      PIC X(8)    VALUE 'FELLOG'.                          
008510     03  ABEND       PIC X(8)    VALUE 'ABEND   '.                        
008600     03  WDATKONV    PIC X(8)    VALUE 'WDATKONV'.                        
008610     03  WINTSOR     PIC X(8)    VALUE 'WINTSOR'.                         
008700     EJECT                                                                
008800     SKIP3                                                                
008900*- - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                      
009000                                                                          
009100*    -COPY W0005       -PRE POSTSUM-                                      
009200     EJECT                                                                
009300*                            *** PARAMETRAR TILL WDATKONV '               
009400*01  -COPY WDATAREA.                                                      
009500     EJECT                                                                
009600 01  FILLER                      PIC X(24)  VALUE                         
009700                                            'UT24-AREA-START'.            
009800     SKIP2                                                                
009900*01  AREA  -COPY W23301 -PRE UT24-                                        
010000     EJECT                                                                
010100 01  FILLER                      PIC X(24)  VALUE                         
010200                                            'UT19-AREA-START'.            
010300     SKIP2                                                                
010400*01  AREA  -COPY W23319 -PRE UT19-                                        
010500     EJECT                                                                
010501                                                                          
010502 01  FILLER              PIC X(16)   VALUE 'IDDC-TABELL'.                 
010503*- - - - - - - - - - - - - TABELL MED ALLA IDDC PÅ WDB601                 
010504*- - - - - - - - - - - - - OCCURS-MAX SÄTTS TILL VERKLIGT ANTAL           
010505*- - - - - - - - - - - - - I T-SEKTIONEN.                                 
010506 01  IDDC-INDEX-WS.                                                       
010507     03 DC-MAX           PIC S9(3)   VALUE +100 COMP SYNC.                
010508                                                                          
010509     03 WDCIX            PIC S9(3)   VALUE +0   COMP SYNC.                
010510     03 DCS-TRAEFF       PIC X       VALUE 'J'.                           
010520                                                                          
010521 01  IDDC-TABELL.                                                         
010522     03 DC-TAB  OCCURS 1 TO 100 DEPENDING ON DC-MAX                       
010523                INDEXED BY DCIX.                                          
010524        05 T-DCS.                                                         
010525          07 T-DCS-IDDC           PIC X(2).                               
010526          07 T-DCS-KDDC           PIC X(2).                               
010527          07 T-DCS-FLOVRLAGBER    PIC X.                                  
010528                                                                          
010529                                                                          
010530 01  FILLER              PIC X(16)   VALUE 'DC-TABELL-SORT'.              
010531*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
010532 01  TABENTRY-PARM.                                                       
010533     03  STEGLANGD               PIC S9(9) COMP.                          
010534     03  ANTAL                   PIC S9(9) COMP.                          
010535     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
010536                                                                          
010537                                                                          
010540     EJECT                                                                
010600 01  IMS-WS.                                                              
010700     03   FILLER     PIC X(8)  VALUE 'IMS-WS'.                            
010800*----------------------------------STATUSKODER FRÅN IMS                   
010900     03  STATUS-WS   PIC XX.                                              
011000         88  SEGMENT-FINNS       VALUE '  '.                              
011100         88  SEGMENT-SLUT        VALUE 'GB'.                              
011200         88  SEGMENT-SAKNAS      VALUE 'GE'.                              
011300                                                                          
011400     03  GODK-STATUSKODER.                                                
011500      05  GODK-STATUS  OCCURS 5 INDEXED BY STATUS-IX PIC XX.              
011600                                                                          
011700 01   SSA1                 PIC X(64).                                     
011800 01   SSA2                 PIC X(64).                                     
011900 01   SSA3                 PIC X(64).                                     
012000     EJECT                                                                
012100*----------------------------------IMS-CALL FUNKTIONER                    
012200*01              -COPY W0003                                              
012300     EJECT                                                                
012310 01  FILLER               PIC X(16)   VALUE 'IO-AREA1'.                   
012400 01  IO-AREA.                                                             
012500   03 IO-AREA1                   PIC X(900).                              
012600                                                                          
012700*  03 WLARTC01  -COPY WDK601             -RED IO-AREA1.                   
012800     EJECT                                                                
012900*  03 WLARTC11  -COPY WDK611             -RED IO-AREA1.                   
013000     EJECT                                                                
013010 01  FILLER               PIC X(16)   VALUE 'IO-AREA2'.                   
013100 01  DLI-IO-AREA2.                                                        
013200   03 IO-AREA2                   PIC X(150).                              
013300                                                                          
013400*  03  AREA   -COPY WDK901     -PRE ARTM-   -RED IO-AREA2.                
013500     EJECT                                                                
013510 01  FILLER               PIC X(16)   VALUE 'IO-AREA3'.                   
013600 01  DLI-IO-AREA3.                                                        
013700   03 IO-AREA3                   PIC X(600).                              
013800                                                                          
013900*  03 WLARTS11  -COPY WDK711             -RED IO-AREA3.                   
014000     EJECT                                                                
014010 01  FILLER               PIC X(16)   VALUE 'IO-AREA-B601'.               
014020 01  DLI-IO-AREA-B601.                                                    
014030*     03  -COPY WDB601                                                    
014040     EJECT                                                                
014050                                                                          
014100 LINKAGE SECTION.                                                         
014200*01    -COPY W0008         -PRE WDK6-                                     
014300          05  FILLER      PIC   XX.                                       
014400     EJECT                                                                
014500*01    -COPY W0008         -PRE ARTM-                                     
014600          05  FILLER      PIC   XX.                                       
014700     EJECT                                                                
014800*01    -COPY W0008         -PRE ARTS-                                     
014900          05  FILLER      PIC   XX.                                       
015000     EJECT                                                                
015010*01    -COPY W0008         -PRE WDB6-                                     
015020          05  FILLER      PIC   XX.                                       
015030     EJECT                                                                
015040                                                                          
015050                                                                          
015060                                                                          
015100 PROCEDURE DIVISION USING WDK6-PCB ARTM-PCB ARTS-PCB WDB6-PCB.            
015200     ENTRY 'CBLTDLI'  USING WDK6-PCB.                                     
015300     ENTRY 'DLITCBL'  USING ARTM-PCB ARTS-PCB WDB6-PCB.                   
015400     PERFORM A-INIT                                                       
015410                                                                          
015411     PERFORM T-LAES-WDB6-DC-INFO                                          
015420                                                                          
015500     PERFORM IMS-GET-WDK6                                                 
015600     PERFORM UNTIL SEGMENT-SLUT                                           
015700         EVALUATE WDK6-SEG-NAME-FB                                        
015800            WHEN 'WDK601  '                                               
015900             PERFORM B-FLYTTA-ART-INFO                                    
016000            WHEN  'WDK611  '                                              
016100             PERFORM C-FLYTTA-MAT-INFO                                    
016200             PERFORM D-FLYTTA-MTRLF-INFO                                  
016300             PERFORM E-FLYTTA-GEMCL-INFO                                  
016400         END-EVALUATE                                                     
016500         PERFORM IMS-GET-WDK6                                             
016600     END-PERFORM                                                          
016700     PERFORM Z-FINIT                                                      
016800     MOVE ZERO TO RETURN-CODE                                             
016900     GOBACK                                                               
017000     .                                                                    
017100     EJECT                                                                
017200 A-INIT SECTION.                                                          
017300                                                                          
017400     OPEN OUTPUT W23324                                                   
017500                 W23319                                                   
017600                                                                          
017700     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
017800                                                                          
017900     MOVE 'IDAG  '               TO DAT-KDDATFORM                         
018000                                                                          
018100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
018200                         DAT-O-TIDATUM DAT-KDSVAR                         
018300                                                                          
018400     MOVE DAT-TIAA               TO DAGENS-AA                             
018500     MOVE DAT-TIVV               TO DAGENS-VV                             
018600     .                                                                    
018700     EJECT                                                                
018800 B-FLYTTA-ART-INFO SECTION.                                               
018900                                                                          
019000     PERFORM S01-SKRIV-FOREG-ART-W23324                                   
019100     PERFORM S02-SKRIV-FOREG-ART-W23319                                   
019200                                                                          
019300     MOVE ART-IDARTNR  TO UT19-IDARTNR                                    
019400     MOVE ART-KDPRODSL TO UT19-KDPRODSL                                   
019500     MOVE JA TO SW-SKRIV-19                                               
019600                                                                          
019700     IF ART-KDERS-UTG > 0 OR                                              
019800        ART-IDLEVNR = SPACE                                               
019900       MOVE NEJ TO SW-BEHANDLA-24                                         
020000     ELSE                                                                 
020100       MOVE JA TO SW-BEHANDLA-24                                          
020200       MOVE ART-IDARTNR  TO UT24-IDARTNR                                  
020300       MOVE ART-KDPRODSL TO UT24-KDPRODSL                                 
020400       MOVE ART-TIFINLV  TO UT24-TIFINLV   TIFINLV-AAVVD                  
020500       MOVE ART-IDLEVNR  TO UT24-IDLEVNR                                  
020600       MOVE ART-IDARTNR  TO W-IDARTNR                                     
020700       PERFORM IMS-GET-ARTM01                                             
020800       IF SEGMENT-FINNS                                                   
020900          MOVE ARTM-ART-KVOKS-BULK     TO UT24-KVOKS-BULK (1)             
021000          MOVE ARTM-ART-KVOKS-DAG      TO UT24-KVOKS-DAG (1)              
021100          MOVE ARTM-ART-KVOKS-VOR      TO UT24-KVOKS-VOR (1)              
021200          MOVE ZERO                    TO UT24-KVOKS-BULK (2)             
021300          MOVE ZERO                    TO UT24-KVOKS-DAG (2)              
021400          MOVE ZERO                    TO UT24-KVOKS-VOR (2)              
021500       ELSE                                                               
021600          MOVE ZERO                    TO UT24-KVOKS-BULK (1)             
021700                                          UT24-KVOKS-DAG (1)              
021800                                          UT24-KVOKS-VOR (1)              
021900                                          UT24-KVOKS-BULK (2)             
022000                                          UT24-KVOKS-DAG (2)              
022100                                          UT24-KVOKS-VOR (2)              
022200       END-IF                                                             
022300                                                                          
022400       MOVE ZERO TO W-TILLG-SDC                                           
022500                    W-OVERLAGER-SDC                                       
022600                                                                          
022700       MOVE ART-IDARTNR  TO W-IDARTNR                                     
022800       PERFORM IMS-GET-ARTS01                                             
022900       IF SEGMENT-FINNS                                                   
023000          PERFORM IMS-GET-ARTS11                                          
023100          PERFORM UNTIL SEGMENT-SAKNAS                                    
023200             MOVE ZERO           TO W-TILLG-SDC                           
023300             ADD SLAG-KVLS       TO W-TILLG-SDC                           
023400             ADD SLAG-KVBEART    TO W-TILLG-SDC                           
023500             ADD SLAG-KVAKS-SDC  TO W-TILLG-SDC                           
023600             ADD SLAG-KVAKS-PAV  TO W-TILLG-SDC                           
023700             SUBTRACT SLAG-KVOKS-DAG  FROM W-TILLG-SDC                    
023800             SUBTRACT SLAG-KVOKS-BULK FROM W-TILLG-SDC                    
023900                                                                          
024000             MOVE SLAG-IDDC        TO WS-IDDC                             
024013             SET DCIX TO +1                                               
024014             SEARCH DC-TAB                                                
024015                AT END                                                    
024016                   MOVE NEJ TO DCS-TRAEFF                                 
024017                WHEN T-DCS-IDDC (DCIX) = SLAG-IDDC                        
024018                   MOVE JA  TO DCS-TRAEFF                                 
024019                   CONTINUE                                               
024020             END-SEARCH                                                   
024021                                                                          
024022             IF T-DCS-KDDC(DCIX)(1:1) = 'N'                               
024024*               --- HOPPA ÖVER NDC-LAGER (POS 1 = N)                      
024025                CONTINUE                                                  
024026             ELSE                                                         
024027               IF DCS-TRAEFF = JA                                         
024028*                -- KOLLA SDC-LAGER (POS 1 = S)                           
024029                 IF (T-DCS-KDDC(DCIX)(1:1) = 'S') AND NOT LDC-CN          
024030*                  -- KOLLA OM ÖVERLAGERBERÄKNING PÅ SDC SKA GÖRAS        
024031                   IF T-DCS-FLOVRLAGBER(DCIX) = NEJ                       
024032                     CONTINUE                                             
024033                   ELSE                                                   
024034                     IF SLAG-KVREFOVL < W-TILLG-SDC                       
024035                       COMPUTE W-OVERLAGER-SDC = W-OVERLAGER-SDC          
024036                                           + W-TILLG-SDC                  
024037                                           - SLAG-KVREFOVL                
024038                     END-IF                                               
024039                   END-IF                                                 
024040                 END-IF                                                   
024041               ELSE                                                       
024042*                -- SDC ÄR EJ REGISTRERAT PÅ WDB6. HOPPA !                
024043*                -- Detta borde egentligen aldrig inträffa'               
024044                 DISPLAY 'IDDC ' SLAG-IDDC ' EJ REG PÅ WDB6'              
024045                 CONTINUE                                                 
024046               END-IF                                                     
024047             END-IF                                                       
024050*            --                                                           
024600             MOVE DAGENS-AA  TO TMP1-YY                                   
024700             MOVE TIFINLV-AA TO TMP2-YY                                   
024800             PERFORM WY2000P9                                             
024900             COMPUTE DAGENS-ABS-VV  = TMP1-YY * 52 + DAGENS-VV            
025000             COMPUTE TIFINLV-ABS-VV = TMP2-YY * 52 + TIFINLV-VV           
025100             IF (DAGENS-ABS-VV - TIFINLV-ABS-VV) < 52                     
025200                MOVE ZERO TO W-OVERLAGER-SDC                              
025300             END-IF                                                       
025400             PERFORM IMS-GET-ARTS11                                       
025500          END-PERFORM                                                     
025600          MOVE W-OVERLAGER-SDC TO UT24-KVLS-SDC-OVER                      
025700       ELSE                                                               
025800           MOVE ZERO           TO UT24-KVLS-SDC-OVER                      
025900       END-IF                                                             
026000     END-IF                                                               
026100     EJECT                                                                
026200     .                                                                    
026300 C-FLYTTA-MAT-INFO SECTION.                                               
026400                                                                          
026500     IF SW-BEHANDLA-24 = JA                                               
026600       MOVE CLAG-IDINK        TO UT24-IDINK                               
026700                                 WS-IDINK                                 
026800       MOVE CLAG-KDAVT        TO UT24-KDAVT                               
026900       MOVE CLAG-KDHF         TO UT24-KDHF                                
027000       MOVE CLAG-KVDAGAR-INLEV TO UT24-KVDAGAR-INLEV                      
027100       MOVE CLAG-KVDAGAR-TT   TO UT24-KVDAGAR-TT                          
027200       MOVE CLAG-KVLAAN       TO UT24-KVLAAN                              
027300       MOVE CLAG-KVQ          TO UT24-KVQ                                 
027400       MOVE CLAG-KVQ-JUST     TO UT24-KVQ-JUST                            
027500       MOVE CLAG-KVVECKOR-FT  TO UT24-KVVECKOR-FT                         
027600       MOVE CLAG-TIQJUST      TO UT24-TIQJUST                             
027700     END-IF                                                               
027800     .                                                                    
027900     EJECT                                                                
028000 D-FLYTTA-MTRLF-INFO SECTION.                                             
028100                                                                          
028200     IF SW-BEHANDLA-24 = JA                                               
028300        MOVE CLAG-KVPB-SEP TO UT24-KVPB-SEP (1)                           
028400        MOVE CLAG-REDIRLEV TO UT24-REDIRLEV (1)                           
028500        MOVE ZERO          TO UT24-KVPB-SEP (2)                           
028600                              UT24-REDIRLEV (2)                           
028700     END-IF                                                               
028800     .                                                                    
028900     EJECT                                                                
029000 E-FLYTTA-GEMCL-INFO SECTION.                                             
029100                                                                          
029200     IF SW-BEHANDLA-24 = JA                                               
029300       MOVE JA TO SW-SKRIV-24                                             
029400       MOVE CLAG-KDERS     TO UT24-KDERS                                  
029500       COMPUTE WS-KVAKS = CLAG-KVAKS-CDC + CLAG-KVAKS-PAV                 
029600                          + CLAG-KVAKS-T                                  
029700       MOVE WS-KVAKS       TO UT24-KVAKS (1)                              
029800       MOVE CLAG-KVLS      TO UT24-KVLS (1)                               
029900       MOVE CLAG-KVRESS    TO UT24-KVRESS (1)                             
030000       MOVE CLAG-KVROS     TO UT24-KVROS (1)                              
030100       MOVE CLAG-KVSLAGER TO UT24-KVSLAGER (1)                            
030200       MOVE ZERO           TO UT24-KVAKS (2)                              
030300                              UT24-KVLS (2)                               
030400                              UT24-KVRESS (2)                             
030500                              UT24-KVROS (2)                              
030600                              UT24-KVSLAGER (2)                           
030700     END-IF                                                               
030800     .                                                                    
030900     EJECT                                                                
031000 S01-SKRIV-FOREG-ART-W23324 SECTION.                                      
031100                                                                          
031200     IF SW-SKRIV-24 = JA                                                  
031300       WRITE W23324-POST FROM UT24-AREA                                   
031400       MOVE SPACE    TO POSTSUM-TRANSTYP                                  
031500       MOVE 'W23324' TO POSTSUM-FDNAMN                                    
031600       MOVE 'W23322D1' TO POSTSUM-DDNAMN2                                 
031700       CALL POSTSUM USING POSTSUM-PARM                                    
031800       MOVE NEJ TO SW-SKRIV-24                                            
031900     END-IF                                                               
032000     SKIP3                                                                
032100     .                                                                    
032200 S02-SKRIV-FOREG-ART-W23319 SECTION.                                      
032300                                                                          
032400     IF SW-SKRIV-19 = JA                                                  
032500       WRITE W23319-POST FROM UT19-AREA                                   
032600       MOVE SPACE    TO POSTSUM-TRANSTYP                                  
032700       MOVE 'W23319' TO POSTSUM-FDNAMN                                    
032800       MOVE 'W23322D1' TO POSTSUM-DDNAMN2                                 
032900       CALL POSTSUM USING POSTSUM-PARM                                    
033000       MOVE NEJ TO SW-SKRIV-19                                            
033100     END-IF                                                               
033200     SKIP3                                                                
033300     .                                                                    
033310 T-LAES-WDB6-DC-INFO SECTION.                                             
033320******************************************************************        
033330*                                                                *        
033340*    LÄS IDDC-BASEN WDB6 OCH SKAPA EN TABELL MED DATA            *        
033350*    FÖR ATT SLIPPA LÄSA WDB6 FÖR VARJE ARTIKEL PÅ WDK6          *        
033370*                                                                *        
033380******************************************************************        
033390     SET DCIX TO +1                                                       
033391     PERFORM IMS-GN-WDB601                                                
033392                                                                          
033393     PERFORM UNTIL SEGMENT-SLUT                                           
033394       IF DCIX <= DC-MAX                                                  
033395         Move DCS-IDDC To T-DCS-IDDC(DCIX)                                
033396         Move DCS-KDDC To T-DCS-KDDC(DCIX)                                
033397         Move DCS-FLOVRLAGBER To T-DCS-FLOVRLAGBER(DCIX)                  
033398         SET DCIX UP BY +1                                                
033399         PERFORM IMS-GN-WDB601                                            
033400       ELSE                                                               
033401         MOVE 35 TO RKOD                                                  
033402         MOVE 'DC-TABELL SLUT. ÖKA DC-MAX' TO FELTEXT-STR                 
033404*        -- INGEN ARTIKEL ÄR ÄNNU PROCESSAD                               
033405         DISPLAY FELTEXT                                                  
033406         CALL ABEND USING RKOD                                            
033407       END-IF                                                             
033408     END-PERFORM                                                          
033409                                                                          
033412*    --- SÄTTER TAKET PÅ TABELLEN                                         
033413     SET DCIX   DOWN BY +1                                                
033414     SET DC-MAX TO DCIX                                                   
033417                                                                          
033418*    --- SORTERA TABELLEN PÅ IDDC, FÖR ATT SEARCH SKA FUNKA               
033419     MOVE DC-MAX                  TO ANTAL                                
033420     MOVE LENGTH OF T-DCS(1)      TO STEGLANGD                            
033421     MOVE LENGTH OF T-DCS-IDDC(1) TO NYCKELLANGD                          
033422                                                                          
033423     CALL WINTSOR USING IDDC-TABELL  STEGLANGD  ANTAL                     
033424                  T-DCS-IDDC(1) NYCKELLANGD                               
033425     .                                                                    
033426     EJECT                                                                
033427******************************************************************        
033428*                                                                *        
033429*    I M S   S E K T I O N E R                                   *        
033430*                                                                *        
033431******************************************************************        
033432                                                                          
033440 IMS-GET-WDK6 SECTION.                                                    
033500     SKIP3                                                                
033600     MOVE '  GAGB' TO GODK-STATUSKODER                                    
033700     CALL CBLTDLI USING GN WDK6-PCB IO-AREA1                              
033800     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
033900     PERFORM IMS-STATUSKONTROLL                                           
034000     .                                                                    
034100     SKIP3                                                                
034200 IMS-GET-ARTM01 SECTION.                                                  
034300     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
034400            DELIMITED BY SIZE INTO SSA1                                   
034500     MOVE '  GE'                 TO GODK-STATUSKODER                      
034600     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA2 SSA1                     
034700     MOVE ARTM-STATUS-CODE       TO STATUS-WS                             
034800     PERFORM IMS-STATUSKONTROLL                                           
034900     .                                                                    
035000     EJECT                                                                
035100 IMS-GET-ARTS01 SECTION.                                                  
035200     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
035300            DELIMITED BY SIZE INTO SSA1                                   
035400     MOVE '  GE' TO GODK-STATUSKODER                                      
035500     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA3 SSA1                     
035600     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
035700     PERFORM IMS-STATUSKONTROLL                                           
035800     .                                                                    
035900     SKIP3                                                                
036000 IMS-GET-ARTS11 SECTION.                                                  
036100     MOVE 'WLARTS11 ' TO SSA1                                             
036200     MOVE '  GE' TO GODK-STATUSKODER                                      
036300     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA3 SSA1                    
036400     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
036500     PERFORM IMS-STATUSKONTROLL                                           
036600     .                                                                    
036700     SKIP3                                                                
036710 IMS-GN-WDB601    SECTION.                                                
036720     STRING 'WDB601   '                                                   
036730          DELIMITED BY SIZE INTO SSA1                                     
036740     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
036750     CALL CBLTDLI USING GN WDB6-PCB  DLI-IO-AREA-B601 SSA1                
036760     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
036770     PERFORM IMS-STATUSKONTROLL                                           
036780     .                                                                    
036790     EJECT                                                                
036800 IMS-STATUSKONTROLL SECTION.                                              
036900     SKIP3                                                                
037000     SET STATUS-IX TO 1                                                   
037100     SEARCH GODK-STATUS AT END CALL FELLOG                                
037200       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS                            
037300       CONTINUE                                                           
037400     END-SEARCH                                                           
037500     .                                                                    
037600 Z-FINIT SECTION.                                                         
037700                                                                          
037800     PERFORM S01-SKRIV-FOREG-ART-W23324                                   
037900     PERFORM S02-SKRIV-FOREG-ART-W23319                                   
038000     CLOSE W23324                                                         
038100           W23319                                                         
038200     MOVE 'S' TO POSTSUM-OPKOD                                            
038300     CALL POSTSUM USING POSTSUM-PARM                                      
038400     .                                                                    
038500     EJECT                                                                
038600*    -COPY WY2000P9                                                       
