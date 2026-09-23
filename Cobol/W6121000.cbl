000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.      W6121000.                                               
000400 AUTHOR.          KARL JOHAN HANSSON                                      
000500 DATE-WRITTEN.    OKTOBER  1985.                                          
000600                                                                          
000700*    REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*       PROGRAMET LÄSER IGENOM WDL6 (HISTORIKBAS FÖR S-LAGER) OCH         
001100*       SKRIVER EN FIL MED HISTORIK FÖR AKTUELL VECKA.                    
001110*       SKRIVER EN FIL MED HISTORIK FÖR PV INKÖP USA O KINA W233PV        
001200     EJECT                                                                
001300 ENVIRONMENT DIVISION.                                                    
001400     SKIP3                                                                
001500 INPUT-OUTPUT SECTION.                                                    
001600                                                                          
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*- - - - - - - - - - - - - - UTFIL:                                       
002000     SELECT W61210                       ASSIGN TO UT-S-W61210D1.         
002100     SELECT W61212                       ASSIGN TO UT-S-W61210D2.         
002200     SELECT W61214                       ASSIGN TO UT-S-W61210D3.         
002300     SELECT W61216                       ASSIGN TO UT-S-W61210D4.         
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600     SKIP2                                                                
002700 FILE SECTION.                                                            
002800 FD  W61210                                                               
002900     RECORDING F                                                          
003000     BLOCK CONTAINS 0.                                                    
003100     SKIP2                                                                
003200*01  POST  -COPY W61210   -PRE UT-  -L.                                   
003300     EJECT                                                                
003400 FD  W61212                                                               
003500     RECORDING F                                                          
003600     BLOCK CONTAINS 0.                                                    
003700     SKIP2                                                                
003800*01  POST  -COPY W61212   -PRE UT2- -L.                                   
003900     EJECT                                                                
004000 FD  W61214                                                               
004100     RECORDING F                                                          
004200     BLOCK CONTAINS 0.                                                    
004300     SKIP2                                                                
004400*01  POST  -COPY W61210   -PRE EXT-  -L.                                  
004500     EJECT                                                                
004600 FD  W61216                                                               
004700     RECORDING F                                                          
004800     BLOCK CONTAINS 0.                                                    
004900     SKIP2                                                                
005000*01  POST  -COPY W61216   -PRE UT4-  -L.                                  
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300                                                                          
005400                                                                          
005500*    -- CHECKED BY WY2000                                                 
005600 77   IDPGM                      PIC X(8)    VALUE 'W6121000'.            
005700                                                                          
005800 01  JA                          PIC X       VALUE 'J'.                   
005900 01  NEJ                         PIC X       VALUE 'N'.                   
006000 01  SPAR-IDARTNR                PIC S9(9) COMP-3 VALUE +0.               
006100 01  WDATUM                      PIC X(6)    VALUE 'WDATUM'.              
006101 01  CURRENT-SECTION             PIC X(20)   VALUE 'SPACE'.               
006102 01  RKOD                        PIC S9(4)  VALUE +0  COMP SYNC.          
006103 01  FELTEXT.                                                             
006104     03  FILLER              PIC X(8)   VALUE 'FELTEXT'.                  
006105     03  FELTEXT-STR         PIC X(72)  VALUE SPACE.                      
006106                                                                          
006107 01  WORK-FIELDS.                                                         
006200                                                                          
006201     03 WS-IDLEVNR-DC            PIC X(5)  VALUE SPACE.                   
006202                                                                          
006203     03 DAGENS-TIAAMMDD          PIC 9(6)       VALUE ZERO.               
006204     03 DAGENS-TIAAMMDD-GRP      REDEFINES DAGENS-TIAAMMDD.               
006205        05 DAGENS-TIAA1          PIC 9(2).                                
006206        05 DAGENS-TIMM           PIC 9(2).                                
006207        05 DAGENS-TIDD           PIC 9(2).                                
006208                                                                          
006210     03 DAGENS-TIAAPP            PIC 9(4)       VALUE ZERO.               
006220     03 DAGENS-TIAAPP-GRP       REDEFINES DAGENS-TIAAPP.                  
006230        05 DAGENS-TIAA2          PIC 9(2).                                
006240        05 DAGENS-TIPP           PIC 9(2).                                
006241                                                                          
006242     03 DAGENS-TIAAPP-STARTDAY   PIC 9(6)       VALUE ZERO.               
006250                                                                          
006300*01    -COPY WWDC99                                                       
006400                                                                          
006500 01  FILLER              PIC X(16)   VALUE 'IDDC-TABELL'.                 
006600*- - - - - - - - - - - - - TABELL MED ALLA IDDC PÅ WDB601                 
006700*- - - - - - - - - - - - - DC-MAX occurs SÄTTS TILL VERKLIGT ANTAL        
006800*- - - - - - - - - - - - - I M- SEKTIONEN.                                
006900 01  IDDC-INDEX-WS.                                                       
007000     03 DC-MAX           PIC S9(3)   VALUE +100 COMP SYNC.                
007100                                                                          
007200     03 WDCIX            PIC S9(3)   VALUE +0  COMP SYNC.                 
007300     03 DCS-TRAEFF       PIC X       VALUE 'J'.                           
007400                                                                          
007500 01  IDDC-TABELL.                                                         
007600     03 DC-TAB  OCCURS 1 TO 100 DEPENDING ON DC-MAX                       
007700                INDEXED BY DCIX.                                          
007800        05 T-DCS.                                                         
007900          07 T-DCS-IDDC           PIC X(2).                               
008000          07 T-DCS-KDDC           PIC X(2).                               
008100          07 T-DCS-IDLEVNR-DC     PIC X(5).                               
008110          07 T-DCS-IDLEVNR-EMB    PIC X(5).                               
008200                                                                          
008300                                                                          
008400 01  FILLER              PIC X(16)   VALUE 'DC-TABELL-SORT'.              
008500*    --- PARAMETRAR TILL SUBPROGRAM WINTSOR                               
008600 01  TABENTRY-PARM.                                                       
008700     03  STEGLANGD               PIC S9(9) COMP.                          
008800     03  ANTAL                   PIC S9(9) COMP.                          
008900     03  NYCKELLANGD             PIC S9(9) COMP  VALUE 9.                 
009000                                                                          
009100                                                                          
009200       EJECT                                                              
011125 01  DYNAMISKA-SUBPROGRAM.                                                
011126   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
011127   03  DATKORT                   PIC X(8)    VALUE 'DATKORT '.            
011128   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
011129   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
011130   03  ABEND                     PIC X(8)    VALUE 'ABEND  '.             
011131   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
011133   03  WINTSOR                   PIC X(8)    VALUE 'WINTSOR '.            
011134     EJECT                                                                
011135 01  FILLER                      PIC X(8)    VALUE 'IMS-WS  '.            
011136                                                                          
011137 01  IMS-WS.                                                              
011138                                                                          
011139   03  STATUS-WS                 PIC X(2).                                
011140      88  SEGMENT-FINNS                      VALUE '  '.                  
011141      88  SEGMENT-SAKNAS                     VALUE 'GE'.                  
011142      88  SEGMENT-SLUT                       VALUE 'GB'.                  
011143      88  SEGMENT-OK                         VALUE 'GA'.                  
011144                                                                          
011145   03 GODK-STATUSKODER.                                                   
011146      05 GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC X(2).              
011147                                                                          
011148   03 SSA1                       PIC X(64)  VALUE SPACE.                  
011149     EJECT                                                                
011150*01        -COPY W0003                                                    
011151     EJECT                                                                
011152 01  FILLER                      PIC X(16) VALUE 'POSTSUM'.               
011153                                                                          
011154*    -COPY W0005       -PRE POSTSUM-                                      
011155     EJECT                                                                
011156 01  FILLER                      PIC X(16) VALUE 'DATKONV'.               
011157                                                                          
011158*        -COPY WDATAREA.                                                  
011159     EJECT                                                                
011160 01  FILLER                      PIC X(16) VALUE 'DATUMKORT'.             
011161                                                                          
011162*01  DATUM   -COPY WDATKORT.                                              
011163     EJECT                                                                
011164 01  FILLER                      PIC X(16)  VALUE 'UT-AREA-START'.        
011165                                                                          
011166*01  AREA  -COPY W61210   -PRE UT-                                        
011167     EJECT                                                                
011168*01  AREA  -COPY W61212   -PRE UT2-                                       
011169     EJECT                                                                
011170*01  AREA  -COPY W61216   -PRE UT4-                                       
011171     EJECT                                                                
011172 01  FILLER                      PIC X(16)  VALUE 'IO-AREA'.              
011173 01  IO-AREA.                                                             
011174   03  IO-AREA1                  PIC X(200).                              
011180                                                                          
011200*  03  ART-AREA   -COPY WDL601     -RED IO-AREA1                          
011300     EJECT                                                                
011400*  03  INL-AREA   -COPY WDL611     -RED IO-AREA1                          
011500     EJECT                                                                
011600                                                                          
011610   03  IO-AREA2                  PIC X(800).                              
011620                                                                          
011621*  03  WDB601-AREA   -COPY WDB601     -RED IO-AREA2                       
011622     EJECT                                                                
011630                                                                          
011650     EJECT                                                                
011700 LINKAGE SECTION.                                                         
011800                                                                          
011900*01  -COPY W0008       -PRE WDL6-                                         
012000       05 FILLER                 PIC X(1).                                
012010*01  -COPY   W0008     -PRE WDB6-                                         
012020     05  FILLER                  PIC X.                                   
012030     EJECT                                                                
012100     EJECT                                                                
012130     EJECT                                                                
012200 PROCEDURE DIVISION USING WDL6-PCB WDB6-PCB.                              
012300     ENTRY 'DLITCBL' USING WDL6-PCB WDB6-PCB.                             
012400                                                                          
012500     PERFORM A-INIT                                                       
012510     PERFORM AA-SKAPA-DCTABELL                                            
012600     PERFORM IMS-GET-WDL6                                                 
012700                                                                          
012800     PERFORM UNTIL SEGMENT-SLUT                                           
012900                                                                          
013000      EVALUATE WDL6-SEG-NAME-FB                                           
013100        WHEN  'WDL601'                                                    
013200          MOVE ART-IDARTNR      TO SPAR-IDARTNR                           
013300        WHEN  'WDL611'                                                    
013400          PERFORM B-BEARBETA                                              
013500       END-EVALUATE                                                       
013600                                                                          
013700       PERFORM IMS-GET-WDL6                                               
013800     END-PERFORM                                                          
013900                                                                          
014000     PERFORM Z-FINIT                                                      
014100     MOVE ZERO TO RETURN-CODE                                             
014200     GOBACK                                                               
014300     .                                                                    
014400     SKIP3                                                                
014500 A-INIT SECTION.                                                          
014510                                                                          
014520     MOVE 'A-INIT'           TO CURRENT-SECTION.                          
014600                                                                          
014700     OPEN OUTPUT W61210                                                   
014800                 W61212                                                   
014900                 W61214                                                   
015000                 W61216                                                   
015100                                                                          
015200     MOVE IDPGM              TO POSTSUM-PROGNAMN                          
015300                                                                          
015400     CALL DATKORT USING IDPGM WDATUM DATUMKORT                            
015410                                                                          
015420     MOVE D-AAR     TO DAGENS-TIAA1                                       
015430     MOVE D-MAANAD  TO DAGENS-TIMM                                        
015440     MOVE D-DAG     TO DAGENS-TIDD                                        
015441                                                                          
015450     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
015460     MOVE DAGENS-TIAAMMDD    TO DAT-I-TIDATUM                             
015470     CALL WDATKONV USING        DAT-KDDATFORM                             
015480                                DAT-I-TIDATUM                             
015490                                DAT-O-TIDATUM                             
015491                                DAT-KDSVAR                                
015492                                                                          
015493     IF DAT-KDSVAR-OK                                                     
015494       MOVE DAT-TIAAPP       TO DAGENS-TIAAPP                             
015495     END-IF                                                               
015496                                                                          
015497     MOVE 'AAPP'           TO DAT-KDDATFORM                               
015498     MOVE DAGENS-TIAAPP      TO DAT-I-TIDATUM                             
015499     CALL WDATKONV USING        DAT-KDDATFORM                             
015500                                DAT-I-TIDATUM                             
015501                                DAT-O-TIDATUM                             
015502                                DAT-KDSVAR                                
015503                                                                          
015504     IF DAT-KDSVAR-OK                                                     
015505       MOVE DAT-TIAAMMDD     TO DAGENS-TIAAPP-STARTDAY                    
015506     END-IF                                                               
015507                                                                          
015510     .                                                                    
015600     EJECT                                                                
015601                                                                          
015610 AA-SKAPA-DCTABELL SECTION.                                               
015611     MOVE 'AA-SKAPA-DCTABELL'  TO CURRENT-SECTION.                        
015612                                                                          
015613     SET DCIX TO +1                                                       
015614     PERFORM IMS-GN-WDB601                                                
015615                                                                          
015616     PERFORM UNTIL SEGMENT-SLUT                                           
015617       IF DCIX <= DC-MAX                                                  
015618         MOVE DCS-IDDC TO T-DCS-IDDC(DCIX)                                
015619         MOVE DCS-KDDC TO T-DCS-KDDC(DCIX)                                
015620         MOVE DCS-IDLEVNR-DC  TO T-DCS-IDLEVNR-DC(DCIX)                   
015621         MOVE DCS-IDLEVNR-EMB TO T-DCS-IDLEVNR-EMB(DCIX)                  
015622*                                                                         
015623         SET DCIX UP BY +1                                                
015624                                                                          
015625         PERFORM IMS-GN-WDB601                                            
015626       ELSE                                                               
015627                                                                          
015628           MOVE 'DC-TABELL SLUT. ÖKA DC-MAX' TO FELTEXT-STR               
015631           DISPLAY FELTEXT                                                
015632           CALL ABEND USING RKOD                                          
015633       END-IF                                                             
015634     END-PERFORM                                                          
015635                                                                          
015636*    --- SÄTTER TAKET PÅ TABELLEN                                         
015637     Set DCIX   Down BY +1                                                
015638     Set DC-MAX TO DCIX                                                   
015639                                                                          
015640*    --- SORTERA TABELLEN PÅ IDDC, FÖR ATT SEARCH SKA FUNKA               
015641     MOVE DC-MAX                  TO ANTAL                                
015642     MOVE LENGTH OF T-DCS(1)      TO STEGLANGD                            
015643     MOVE LENGTH OF T-DCS-IDDC(1) TO NYCKELLANGD                          
015644                                                                          
015645     CALL WINTSOR USING IDDC-TABELL  STEGLANGD  ANTAL                     
015646                  T-DCS-IDDC(1) NYCKELLANGD                               
015647     .                                                                    
015648     EJECT                                                                
015650                                                                          
015700 B-BEARBETA SECTION.                                                      
015810     MOVE 'B-BEARBETA'    TO CURRENT-SECTION.                             
015820                                                                          
015900     PERFORM BA-URVAL-TIINLMOT                                            
016000     PERFORM BB-URVAL-TIINLINL                                            
016100     PERFORM BC-URVAL-TIINLINL                                            
016110     PERFORM BD-URVAL-SIPLUS-CN-US                                        
016200                                                                          
016300     .                                                                    
016400     EJECT                                                                
016500 BA-URVAL-TIINLMOT SECTION.                                               
016700     SKIP2                                                                
016710     MOVE 'BA-URVAL-TIINLMOT' TO CURRENT-SECTION.                         
016720                                                                          
016800     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
016900     MOVE INL-TIINLMOT       TO DAT-I-TIDATUM                             
017000     CALL WDATKONV USING        DAT-KDDATFORM                             
017100                                DAT-I-TIDATUM                             
017200                                DAT-O-TIDATUM                             
017300                                DAT-KDSVAR                                
017400                                                                          
017410     IF DAT-KDSVAR-OK                                                     
017500       IF DAT-TIAA = D-AAR AND DAT-TIVV = D-VECKA                         
017600         MOVE SPAR-IDARTNR                   TO UT-IDARTNR                
017700         MOVE INL-DAINLEV                    TO UT-DAINLEV                
017800         MOVE INL-ADLAGOMR                   TO UT-ADLAGOMR               
017900         MOVE INL-ADGANG                     TO UT-ADGANG                 
018000         MOVE INL-ADPLATS                    TO UT-ADPLATS                
018100         MOVE INL-IDDC                       TO UT-IDDC                   
018200         MOVE INL-IDFAKT                     TO UT-IDFAKT                 
018300         MOVE INL-IDGMTREF                   TO UT-IDGMTREF               
018400         MOVE INL-IDKOLLI                    TO UT-IDKOLLI                
018500         MOVE INL-IDPTYP                     TO UT-IDPTYP                 
018600         MOVE INL-KDKOLLI                    TO UT-KDKOLLI                
018700         MOVE INL-KVANTMOT                   TO UT-KVANTMOT               
018800         MOVE INL-KVAVIS                     TO UT-KVAVIS                 
018900         MOVE INL-TIINLMOT                   TO UT-TIINLMOT               
019000         MOVE INL-TIINLINL                   TO UT-TIINLINL               
019100                                                                          
019200         PERFORM S01-SKRIV-W61210-FILEN                                   
019300       END-IF                                                             
019400     END-IF                                                               
019500     .                                                                    
019600     EJECT                                                                
019700 BB-URVAL-TIINLINL SECTION.                                               
019710                                                                          
019720     MOVE 'BB-URVAL-TIINLINL' TO CURRENT-SECTION.                         
019730                                                                          
019800     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
019900     MOVE INL-TIINLINL       TO DAT-I-TIDATUM                             
020000     CALL WDATKONV USING        DAT-KDDATFORM                             
020100                                DAT-I-TIDATUM                             
020200                                DAT-O-TIDATUM                             
020300                                DAT-KDSVAR                                
020400                                                                          
020500     IF DAT-KDSVAR-OK                                                     
020600       IF DAT-TIAA = D-AAR AND DAT-TIVV = D-VECKA                         
020700         IF INL-KVAVIS NOT = INL-KVANTMOT                                 
020800           IF INL-IDFAKT > 0 AND INL-IDPTYP = 'R32'                       
020900             MOVE INL-IDDC                       TO UT2-IDDC              
021000             MOVE SPAR-IDARTNR                   TO UT2-IDARTNR           
021100             MOVE INL-KVANTMOT                   TO UT2-KVANTMOT          
021200             MOVE INL-KVAVIS                     TO UT2-KVAVIS            
021300             MOVE INL-TIINLINL                   TO UT2-TIINLINL          
021400             MOVE INL-IDFAKT                     TO UT2-IDFAKT            
021500             MOVE INL-IDGMTREF                   TO UT2-IDGMTREF          
021600             MOVE INL-IDKOLLI                    TO UT2-IDKOLLI           
021700             MOVE INL-KDFRAKT                    TO UT2-KDFRAKT           
021800                                                                          
021900             PERFORM S02-SKRIV-W61212-FILEN                               
022000           END-IF                                                         
022100         END-IF                                                           
022200       END-IF                                                             
022300     END-IF                                                               
022400     .                                                                    
022500     EJECT                                                                
022600 BC-URVAL-TIINLINL SECTION.                                               
022700     SKIP2                                                                
022710     MOVE 'BC-URVAL-TIINLINL' TO CURRENT-SECTION.                         
022800     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
022900     MOVE INL-TIINLINL       TO DAT-I-TIDATUM                             
023000     CALL WDATKONV USING        DAT-KDDATFORM                             
023100                                DAT-I-TIDATUM                             
023200                                DAT-O-TIDATUM                             
023300                                DAT-KDSVAR                                
023400                                                                          
023500     IF DAT-KDSVAR-OK                                                     
023600       IF DAT-TIAA = D-AAR AND DAT-TIVV = D-VECKA                         
023700         MOVE SPAR-IDARTNR                   TO UT-IDARTNR                
023800         MOVE INL-DAINLEV                    TO UT-DAINLEV                
023900         MOVE INL-ADLAGOMR                   TO UT-ADLAGOMR               
024000         MOVE INL-ADGANG                     TO UT-ADGANG                 
024100         MOVE INL-ADPLATS                    TO UT-ADPLATS                
024200         MOVE INL-IDDC                       TO UT-IDDC                   
024300         MOVE INL-IDFAKT                     TO UT-IDFAKT                 
024400         MOVE INL-IDGMTREF                   TO UT-IDGMTREF               
024500         MOVE INL-IDKOLLI                    TO UT-IDKOLLI                
024600         MOVE INL-IDPTYP                     TO UT-IDPTYP                 
024700         MOVE INL-KDKOLLI                    TO UT-KDKOLLI                
024800         MOVE INL-KVANTMOT                   TO UT-KVANTMOT               
024900         MOVE INL-KVAVIS                     TO UT-KVAVIS                 
025000         MOVE INL-TIINLMOT                   TO UT-TIINLMOT               
025100         MOVE INL-TIINLINL                   TO UT-TIINLINL               
025200                                                                          
025300         PERFORM S03-SKRIV-W61214-FILEN                                   
025400       END-IF                                                             
025500     END-IF                                                               
025600     .                                                                    
025700     EJECT                                                                
025701                                                                          
025710 BD-URVAL-SIPLUS-CN-US SECTION.                                           
025711                                                                          
025712     MOVE 'BD-URVAL-SIPLUS-CN-US' TO CURRENT-SECTION.                     
025713                                                                          
025720     MOVE INL-IDDC             TO WS-IDDC                                 
025721     IF NDC-CN OR NDC-US                                                  
025722       IF INL-IDPTYP = 'R31' OR 'R32'                                     
025724         IF INL-TIINLMOT >= DAGENS-TIAAPP-STARTDAY OR                     
025725            INL-TIINLINL >= DAGENS-TIAAPP-STARTDAY                        
025727           IF INL-KDRT = ZERO                                             
025728             PERFORM BDA-KOLLA-OM-INTERN-LEV                              
025729             IF DCS-TRAEFF = JA                                           
025730               CONTINUE                                                   
025731             ELSE                                                         
025732               PERFORM BDB-HAMTA-IDLEVNR-DC                               
025733               PERFORM BDC-SKAPA-W61216-FIL                               
025734               PERFORM S04-SKRIV-W61216-FILEN                             
025735             END-IF                                                       
025736           END-IF                                                         
025737         END-IF                                                           
025738       END-IF                                                             
025739     END-IF                                                               
025740     .                                                                    
025741     EJECT                                                                
025742                                                                          
025801 BDA-KOLLA-OM-INTERN-LEV SECTION.                                         
025802                                                                          
025803     MOVE 'BDA-KOLLA-OM-INTERN-LEV' TO CURRENT-SECTION.                   
025804                                                                          
025805*    MOVE  INL-IDLEVNR TO WS-IDLEVNR                                      
025809     SET DCIX TO +1                                                       
025810     SEARCH DC-TAB                                                        
025811        AT END                                                            
025812          MOVE NEJ TO DCS-TRAEFF                                          
025813        WHEN T-DCS-IDLEVNR-DC(DCIX) = INL-IDLEVNR                         
025814          MOVE JA  TO DCS-TRAEFF                                          
025815          CONTINUE                                                        
025816     END-SEARCH                                                           
025817                                                                          
025839     .                                                                    
025840     EJECT                                                                
025841                                                                          
025842 BDB-HAMTA-IDLEVNR-DC SECTION.                                            
025843                                                                          
025844     MOVE 'BDB-HAMTA-IDLEVNR-DC' TO CURRENT-SECTION.                      
025845*    MOVE  INL-IDDC TO WS-IDDC                                            
025849     SET DCIX TO +1                                                       
025850     SEARCH DC-TAB                                                        
025851        AT END                                                            
025852          MOVE NEJ TO DCS-TRAEFF                                          
025853        WHEN T-DCS-IDDC(DCIX) = INL-IDDC                                  
025854          MOVE JA  TO DCS-TRAEFF                                          
025855          CONTINUE                                                        
025856     END-SEARCH                                                           
025857                                                                          
025859     IF DCS-TRAEFF = JA                                                   
025860        IF T-DCS-KDDC(DCIX) = 'NA'                                        
025861          MOVE T-DCS-IDLEVNR-EMB(DCIX) TO WS-IDLEVNR-DC                   
025862        ELSE                                                              
025870          MOVE T-DCS-IDLEVNR-DC(DCIX)  TO WS-IDLEVNR-DC                   
025871        END-IF                                                            
025873     ELSE                                                                 
025876       DISPLAY 'IDDC ' INL-IDDC ' EJ REG PÅ WDB6'                         
025879     END-IF                                                               
025880     .                                                                    
025881     EJECT                                                                
025882                                                                          
025883 BDC-SKAPA-W61216-FIL SECTION.                                            
025884                                                                          
025885     MOVE 'BDC-SKAPA-W61216-FIL' TO CURRENT-SECTION.                      
025886                                                                          
025887     MOVE DAGENS-TIAAPP          TO UT4-TIAAPP                            
025888     MOVE DAGENS-TIAAPP-STARTDAY TO UT4-TIAAMMDD                          
025889     MOVE INL-IDDC               TO UT4-IDDC                              
025890     MOVE INL-IDLEVNR            TO UT4-IDLEVNR                           
025891     MOVE SPAR-IDARTNR           TO UT4-IDARTNR                           
025892     MOVE INL-IDPTYP             TO UT4-IDPTYP                            
025893     MOVE INL-KVAVIS             TO UT4-KVAVIS                            
025894     MOVE INL-KVANTMOT           TO UT4-KVANTMOT                          
025895     MOVE INL-TIINLMOT           TO UT4-TIINLMOT                          
025896     MOVE INL-TIINLINL           TO UT4-TIINLINL                          
025897     MOVE INL-KDRT               TO UT4-KDRT                              
025898     MOVE WS-IDLEVNR-DC          TO UT4-IDLEVNR-DC                        
025900     .                                                                    
025901     EJECT                                                                
025902                                                                          
025903                                                                          
025910 Z-FINIT SECTION.                                                         
026000                                                                          
026100     CLOSE W61210                                                         
026200           W61212                                                         
026300           W61214                                                         
026310           W61216                                                         
026400                                                                          
026500     MOVE 'S' TO POSTSUM-OPKOD                                            
026600     CALL POSTSUM USING POSTSUM-PARM                                      
026700     .                                                                    
026800     SKIP2                                                                
026900 S01-SKRIV-W61210-FILEN SECTION.                                          
027000                                                                          
027100     WRITE UT-POST FROM UT-AREA                                           
027200                                                                          
027300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
027400     MOVE 'UT-INLEV' TO POSTSUM-FDNAMN                                    
027500     MOVE 'W61210D1' TO POSTSUM-DDNAMN2                                   
027600     CALL POSTSUM USING POSTSUM-PARM                                      
027700     .                                                                    
027800     SKIP2                                                                
027900 S02-SKRIV-W61212-FILEN SECTION.                                          
028000                                                                          
028100     WRITE UT2-POST FROM UT2-AREA                                         
028200                                                                          
028300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
028400     MOVE 'UT-INLEV' TO POSTSUM-FDNAMN                                    
028500     MOVE 'W61210D2' TO POSTSUM-DDNAMN2                                   
028600     CALL POSTSUM USING POSTSUM-PARM                                      
028700     .                                                                    
028800     SKIP2                                                                
028900 S03-SKRIV-W61214-FILEN SECTION.                                          
029000                                                                          
029100     WRITE EXT-POST FROM UT-AREA                                          
029200                                                                          
029300     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
029400     MOVE 'SI+-CN'   TO POSTSUM-FDNAMN                                    
029500     MOVE 'W61210D3' TO POSTSUM-DDNAMN2                                   
029600     CALL POSTSUM USING POSTSUM-PARM                                      
029700     .                                                                    
029800     SKIP2                                                                
029810 S04-SKRIV-W61216-FILEN SECTION.                                          
029820                                                                          
029830     WRITE UT4-POST FROM UT4-AREA                                         
029840                                                                          
029850     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
029860     MOVE 'VOLUME ' TO POSTSUM-FDNAMN                                     
029870     MOVE 'W61210D4' TO POSTSUM-DDNAMN2                                   
029880     CALL POSTSUM USING POSTSUM-PARM                                      
029890     .                                                                    
029891     SKIP2                                                                
029900 IMS-GET-WDL6 SECTION.                                                    
030000                                                                          
030100     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
030200     CALL CBLTDLI USING GN WDL6-PCB IO-AREA                               
030300     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
030400     PERFORM IMS-STATUSKONTROLL                                           
030500     .                                                                    
030600     SKIP2                                                                
030610 IMS-GN-WDB601    SECTION.                                                
030620     STRING 'WDB601   '                                                   
030630          DELIMITED BY SIZE INTO SSA1                                     
030640     MOVE '  GB' TO GODK-STATUSKODER                                      
030650     CALL CBLTDLI USING GN WDB6-PCB  WDB601-AREA SSA1                     
030660     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
030670     PERFORM IMS-STATUSKONTROLL                                           
030680     .                                                                    
030690     EJECT                                                                
030700 IMS-STATUSKONTROLL SECTION.                                              
030800                                                                          
030900     SET STATUS-IX TO 1                                                   
031000     SEARCH GODK-STATUS AT END CALL FELLOG                                
031100     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                    
031200     END-SEARCH                                                           
031300     .                                                                    
