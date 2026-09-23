000400 ID  DIVISION.                                                            
000500     SKIP2                                                                
000600 PROGRAM-ID.    W4610100.                                                 
001000*AUTHOR.        N.N.    /MARGARETA GABRIELSSON                            
001100*DATE-WRITTEN.  NOV 1984/SEPTEMBER 1995.                                  
001200                                                                          
001300*REMARKS.                                                                 
001400                                                                          
001500*    FUNKTION:                                                            
001600                                                                          
001700*        PROGRAMMET VÄLJER UT RADPOSTER VILKA LÄMNAS TILL SORT,           
001800*         (UNDANTAG                                                       
001900*            BIPACKAD RAD (RONR NOT = 0)  SERVICEGRAD = 999.99)           
002000*        ÖVRIGA POSTER GÅR DIREKT TILL UTFIL W46102.                      
002100*        - RADPOSTERNAS SORTERAS I ORDNING: PRODNR                        
002200*                                           RONR                          
002300*                                           ARTNR                         
002400*                                           BESTANT                       
002500*        - DÄREFTER BERÄKNAS SERVICEGRAD I RADPOSTERNA:                   
002600*            SERVICEGRAD = LEVANT / BESTANT * 100                         
002700*            FÖR RAD SOM ÄR SPLITTAD I FLERA KOLLI                        
002800*            SERVICEGRAD = SUMMA(LEVANT) / BESTANT * 100                  
002900*        - SKRIV ALLA RADPOSTER PÅ FIL W46102.                            
003000*          SKRIV ALLA RADPOSTER DÄR MAN HAR BERÄKNAT                      
003100*          SERVICEGRAD PÅ FIL W46101  OCH                                 
003200*        KOLLIPOSTER VÄLJS INTE UT, MEN KOMPLETTERAS MED                  
003210*        BOLLANUMMER INNAN DE SKRIVS UT.                                  
003220*        BOLLANUMMER HÄMTAS FRÅN HÄNDELSETYP 4491 (WDR4)                  
003300                                                                          
003400*    ABENDKODER:                                                          
003500                                                                          
003600*        U0016    - OM RETURKOD FRÅN SORT                                 
003700     EJECT                                                                
003800 ENVIRONMENT DIVISION.                                                    
003900     SKIP2                                                                
004000 INPUT-OUTPUT SECTION.                                                    
004100                                                                          
004200 FILE-CONTROL.                                                            
004300     SKIP2                                                                
004400*- - - - - - - - - - - - INFIL:                                           
004500*                        - -  FIL FRÅN FAKTURERINGEN                      
004600     SELECT W46100-FAKT                  ASSIGN TO UT-S-W46101D1.         
004700     SKIP2                                                                
004800*- - - - - - - - - - - - UTFIL1:                                          
004900*                        - -  W46101 SERVICEGRAD                          
005000     SELECT W46101-UT1                   ASSIGN TO UT-S-W46101D2.         
005100     SKIP2                                                                
005200*- - - - - - - - - - - - UTFIL2:                                          
005300*                        - -  W46102 SAMMA SOM INFIL                      
005400     SELECT W46102-UT2                   ASSIGN TO UT-S-W46101D3.         
005500     SKIP2                                                                
005600*- - - - - - - - - - - - SORTFIL:                                         
005700     SELECT SORTFIL                      ASSIGN TO UT-S-W46101DS.         
005800     EJECT                                                                
005900 DATA DIVISION.                                                           
006000     SKIP2                                                                
006100 FILE SECTION.                                                            
006200     SKIP3                                                                
006300*                                 FAKTURERINGSPOSTER                      
006400 FD  W46100-FAKT                                                          
006500     RECORDING      V                                                     
006600     BLOCK CONTAINS 0.                                                    
006700     SKIP2                                                                
006800*01  IN-POST -COPY W461010    -L.                                         
006900*01  IN-POST1 -COPY W461011   -L.                                         
007000*01  POST2 -COPY W461012    -PRE IN-.                                     
007100*01  POST3 -COPY W461013    -PRE IN-.                                     
007200     EJECT                                                                
007300*                          RADPOSTER MED                                  
007400*                          SERVICEGRADEN BERÄKNAD                         
007500*                          FIL TILL VCAS CENTRALA SYSTEM                  
007600 FD  W46102-UT2                                                           
007700     RECORDING      V                                                     
007800     BLOCK CONTAINS 0.                                                    
007900     SKIP2                                                                
008000*01  POST  -COPY W461010    -L.                                           
008100*01  POST1 -COPY W461011    -L.                                           
008200*01  POST2 -COPY W461012    -L.                                           
008300*01  POST3 -COPY W461013    -L.                                           
008400     EJECT                                                                
008500*                          FAKTURERINGSPOSTER MED                         
008600*                          SERVICEGRADEN BERÄKNAD                         
008700*                          VIPS FIL                                       
008800 FD  W46101-UT1                                                           
008900     RECORDING      V                                                     
009000     BLOCK CONTAINS 0.                                                    
009100     SKIP2                                                                
009200*01  UT1-POST -COPY W461013      -L.                                      
009300     EJECT                                                                
009400 SD  SORTFIL                                                              
009500                .                                                         
009600*01  SORTPOST1 -COPY W461013     -PRE SORTIN-.                            
009700     EJECT                                                                
009800 WORKING-STORAGE SECTION.                                                 
009810                                                                          
009900*    -- CHECKED BY WY2000                                                 
010400*- - - - - - - - - - - - - -  PROGRAM-NAMN                                
010500 77  IDPGM                       PIC X(8)    VALUE 'W4610100'.            
010700     SKIP2                                                                
010701*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
010710 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
010800*- - - - - - - - - - - - - -  GENERELLA KONSTANTER                        
010900                                                                          
011000 77  JA                          PIC X(1)    VALUE 'J'.                   
011100 77  NEJ                         PIC X(1)    VALUE 'N'.                   
011200     SKIP2                                                                
011300*- - - - - - - - - - - - - -  END-OF-FILE-SWITCHAR                        
011400                                                                          
011500 77  SORTFIL-EOF                 PIC X(1)    VALUE 'N'.                   
011600 77  W46100-EOF                  PIC X(1)    VALUE 'N'.                   
011700*                                                                         
011800     EJECT                                                                
011810 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
011820 01  FILLER REDEFINES DAGENS-DATUM.                                       
011830     03  DAGENS-DATUM-AAR        PIC 9(2).                                
011840     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
011850     03  DAGENS-DATUM-DAG        PIC 9(2).                                
011860     EJECT                                                                
011900*- - - - - - - - - - - - - -  SPAR AREAOR                                 
011910 01  SPAR-TIFAKT                 PIC S9(7)   COMP-3 VALUE ZERO.           
012000*                                                                         
012100*- - - - - - - - - - - - - -                                              
012200                                                                          
012300 01  W-SUMMOR.                                                            
012400     03  W-KVLEVART              PIC S9(7)   COMP-3  VALUE ZERO.          
012500*                                                                         
012600*- - - - - - - - - - - - - -                                              
012700                                                                          
012800     EJECT                                                                
012900 01  DYNAMISKA-SUBPROGRAM.                                                
012910   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
013000   03  ABEND                     PIC X(8)    VALUE 'ABEND'.               
013100   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM'.             
013110   03  DATKORT                   PIC X(8)    VALUE 'DATKORT'.             
013120   03  FELLOG                    PIC X(8)    VALUE 'FELLOG '.             
013200     SKIP3                                                                
013210*    --- PARAMETRAR TILL DATKORT                                          
013220*                                                                         
013230 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W46101'.              
013240     SKIP2                                                                
013250 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
013260     SKIP2                                                                
013270*01  -COPY WDATKORT                                                       
013280     EJECT                                                                
013300*- - - - - - - - - - - - - -  PARAMETRAR TILL ABEND                       
013400                                                                          
013500 01  RETURKODER.                                                          
013600   03  RKOD                      PIC S9(4)  COMP SYNC VALUE ZERO.         
013700   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4)  COMP SYNC VALUE +16.          
013800   03  RKOD-ABEND-MED-DUMP       PIC S9(4)  COMP SYNC VALUE +1000.        
013900     EJECT                                                                
014000*- - - - - - - - - - - - - -  PARAMETRAR TILL POSTSUM                     
014100                                                                          
014200*01  -COPY W0005       -PRE  POSTSUM-.                                    
014300     EJECT                                                                
014400 01  FILLER                      PIC X(8)    VALUE 'ARBETS  '.            
014500 01  W-ARBAREA-X.                                                         
014600     SKIP3                                                                
014700*    03  W-ARBAREA -COPY W461013     -L.                                  
014800     SKIP3                                                                
014900*    03 HUVAREA   -COPY W461010     -PRE W- -RED W-ARBAREA.               
015000     EJECT                                                                
015100*    03 REFAREA   -COPY W461011     -PRE W- -RED W-ARBAREA.               
015200     EJECT                                                                
015300*    03 KOLLIAREA -COPY W461012     -PRE W- -RED W-ARBAREA.               
015400     EJECT                                                                
015500*    03 RADAREA   -COPY W461013     -PRE W- -RED W-ARBAREA.               
015600     EJECT                                                                
015700 01  FILLER                      PIC X(8)    VALUE 'SORT    '.            
015800*01  AREA -COPY W461013     -PRE SORT-.                                   
015900     EJECT                                                                
016000 01  FILLER                      PIC X(8)    VALUE 'WSORT   '.            
016100*01  AREA  -COPY W461013     -PRE  WSORT-.                                
016200     EJECT                                                                
016201 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
016202     SKIP3                                                                
016203 01  NYCKLAR-TILL-DLI.                                                    
016204     03  W-WDGXKEY-4491-X.                                                
016205         05  W-IDHTYP-4491       PIC X(4)    VALUE '4491'.                
016206         05  W-IDDC-4491         PIC X(2)    VALUE SPACE.                 
016207         05  FILLER              PIC X(24)   VALUE LOW-VALUE.             
016211     SKIP2                                                                
016212     03  W-DALASTN-X.                                                     
016213         05  W-DALASTN           PIC  9(8)   VALUE ZERO.                  
016214     03  W-IDKOLLI-X.                                                     
016215         05  W-IDKOLLI           PIC S9(5)   VALUE ZERO COMP-3.           
016216     03  W-IDGMTREF-X.                                                    
016217         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
016218         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
016219         05  W-IDKUNDRF.                                                  
016220             07  W-IDORDNR7      PIC 9(7)    VALUE ZERO.                  
016221             07  FILLER          PIC X(3)    VALUE SPACE.                 
016222     03  W-IDLBBET-X.                                                     
016223         05  W-IDLBBET           PIC X(12)   VALUE SPACE.                 
016224     EJECT                                                                
016225*    --- STATUS-KOD FRÅN IMS                                              
016226 01  STATUS-WS                   PIC XX.                                  
016227     88  SEGMENT-FINNS                       VALUE '  '.                  
016228     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
016229     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
016230     SKIP2                                                                
016231 01  GODK-STATUSKODER.                                                    
016232     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016233     SKIP3                                                                
016234 01  SSA1                        PIC X(96).                               
016235     EJECT                                                                
016236*    --- IMS FUNKTIONSKODER                                               
016237*01  -COPY W0003                                                          
016238     EJECT                                                                
016239*    ---  DLI INPUT-OUTPUT AREA                                           
016240 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
016241     SKIP3                                                                
016242 01  DLI-IO-AREA.                                                         
016243     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
016244     SKIP3                                                                
016245     03  WL449112 REDEFINES IO-AREA.                                      
016246*        05  -COPY WDGX4494  -PRE BOLLA-                                  
016247     EJECT                                                                
016248 LINKAGE SECTION.                                                         
016250                                                                          
016291*01  -COPY W0008  -PRE 4494-                                              
016292     05  FILLER                  PIC X.                                   
016293     EJECT                                                                
016294 PROCEDURE DIVISION  USING 4494-PCB.                                      
016295     ENTRY 'DLITCBL' USING 4494-PCB.                                      
016296                                                                          
016500     PERFORM A-INIT                                                       
016600                                                                          
016700     SORT SORTFIL ASCENDING                                               
016800                  SORTIN-FRAD-IDPRODNR                                    
016900                  SORTIN-FRAD-IDARTNR                                     
017000                  SORTIN-FRAD-FLSPLIT                                     
017100                  SORTIN-FRAD-KVLEVART                                    
017200          INPUT  PROCEDURE B-INPUT-PROCEDURE                              
017300          OUTPUT PROCEDURE C-OUTPUT-PROCEDURE                             
017400                                                                          
017500     IF SORT-RETURN > ZERO                                                
017600       DISPLAY '***  W46101    - FEL VID SORTERING'                       
017700       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017800     ELSE                                                                 
017900       PERFORM Z-FINIT                                                    
018000       MOVE ZERO TO RETURN-CODE                                           
018100       GOBACK                                                             
018200                                                                          
018300     END-IF                                                               
018400     .                                                                    
018500     EJECT                                                                
018600 A-INIT SECTION.                                                          
018700                                                                          
018800     OPEN OUTPUT W46102-UT2 W46101-UT1                                    
018900     OPEN  INPUT W46100-FAKT                                              
019000                                                                          
019100     MOVE IDPGM        TO POSTSUM-PROGNAMN                                
019200     MOVE ZERO         TO WSORT-FRAD-IDPRODNR                             
019300     MOVE ZERO         TO WSORT-FRAD-IDRONR                               
019400     MOVE ZERO         TO WSORT-FRAD-IDARTNR                              
019500     MOVE ZERO         TO WSORT-FRAD-KVBEART                              
019501                                                                          
019502     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
019503     MOVE D-AAR     TO  DAGENS-DATUM-AAR                                  
019504     MOVE D-MAANAD  TO  DAGENS-DATUM-MAANAD                               
019505     MOVE D-DAG     TO  DAGENS-DATUM-DAG                                  
019600     .                                                                    
019700     EJECT                                                                
019800 B-INPUT-PROCEDURE SECTION.                                               
019900                                                                          
020000     PERFORM S01-LAS-W46100                                               
020100     PERFORM UNTIL W46100-EOF = JA                                        
020300       IF   IN-FKOLLI-IDPTYP = '013'                                      
020400         IF   IN-FRAD-IDRONR = ZERO                                       
020500           RELEASE SORTIN-SORTPOST1 FROM W-FRAD-W461013                   
020600         ELSE                                                             
020700           MOVE    999.99      TO W-FRAD-RESERVG                          
020800           PERFORM S02-SKRIV-W46102                                       
020900         END-IF                                                           
021000       ELSE                                                               
021010         IF IN-FKOLLI-IDPTYP = '010'                                      
021020           MOVE W-FHUV-TIFAKT TO SPAR-TIFAKT                              
021030         ELSE                                                             
021040           IF IN-FKOLLI-IDPTYP = '012'                                    
021050             PERFORM BA-KOMPLETTERA-BOLLA                                 
021060           END-IF                                                         
021070         END-IF                                                           
021100         PERFORM S02-SKRIV-W46102                                         
021200       END-IF                                                             
021300       PERFORM S01-LAS-W46100                                             
021400     END-PERFORM                                                          
021500     .                                                                    
021600     SKIP3                                                                
021610 BA-KOMPLETTERA-BOLLA SECTION.                                            
021620                                                                          
021630     MOVE SPAR-TIFAKT                 TO W-DALASTN                        
021631     IF SPAR-TIFAKT NOT = ZERO                                            
021632       IF SPAR-TIFAKT < 500000                                            
021633         MOVE 20                      TO W-DALASTN (1:2)                  
021634       ELSE                                                               
021635         IF SPAR-TIFAKT < 999999                                          
021636           MOVE 19                    TO W-DALASTN (1:2)                  
021637         ELSE                                                             
021638           MOVE 99999999              TO W-DALASTN                        
021639         END-IF                                                           
021640       END-IF                                                             
021641     END-IF                                                               
021642                                                                          
021643     MOVE W-FKOLLI-IDDISTR            TO W-IDDISTR                        
021644     MOVE W-FKOLLI-IDKUNDNR           TO W-IDKUNDNR                       
021645     MOVE W-FKOLLI-IDORDNR            TO W-IDORDNR7                       
021646     MOVE W-FKOLLI-IDLBBET            TO W-IDLBBET                        
021647     MOVE W-FKOLLI-IDKOLLI            TO W-IDKOLLI                        
021648     MOVE W-FRAD-IDDC                 TO W-IDDC-4491                      
021650     PERFORM IMS-GU-WL4491                                                
021680     PERFORM IMS-GNP-WL4494                                               
021681     IF SEGMENT-FINNS                                                     
021690       MOVE BOLLA-4494-IDTRPBO      TO W-FKOLLI-IDTRPBO                   
021691     ELSE                                                                 
021692       MOVE SPACE                   TO W-FKOLLI-IDTRPBOT                  
021693       MOVE ZERO                    TO W-FKOLLI-IDTRPBON                  
021694     END-IF                                                               
021701     .                                                                    
021702     EJECT                                                                
021710 C-OUTPUT-PROCEDURE SECTION.                                              
021800                                                                          
021900     PERFORM S03-RETURN-SORTFIL                                           
022100                                                                          
022200     PERFORM UNTIL SORTFIL-EOF = JA                                       
022400       IF      SORT-FRAD-FLSPLIT = NEJ                                    
022500         PERFORM CA-BER-SERV-GRAD-NORMAL-RAD                              
022600         PERFORM S03-RETURN-SORTFIL                                       
022700       ELSE                                                               
022800         PERFORM CB-SERV-GRAD-SPLITTAD-RAD                                
022900       END-IF                                                             
023000     END-PERFORM                                                          
023100     .                                                                    
023200     SKIP3                                                                
023300 CA-BER-SERV-GRAD-NORMAL-RAD SECTION.                                     
023400                                                                          
023500     IF      SORT-FRAD-KVBEART = ZERO                                     
023600       MOVE    ZERO           TO SORT-FRAD-RESERVG                        
023700     ELSE                                                                 
023800       COMPUTE SORT-FRAD-RESERVG ROUNDED =                                
023900       SORT-FRAD-KVLEVART / SORT-FRAD-KVBEART * 100                       
024000     END-IF                                                               
024100     PERFORM S06-SKRIV-W46101-SORT                                        
024200     PERFORM S07-SKRIV-W46102-SORT                                        
024300     .                                                                    
024400     EJECT                                                                
024500 CB-SERV-GRAD-SPLITTAD-RAD SECTION.                                       
024510                                                                          
024600     MOVE    SORT-AREA           TO WSORT-AREA                            
024700     MOVE    WSORT-FRAD-KVLEVART TO W-KVLEVART                            
024800     PERFORM S03-RETURN-SORTFIL                                           
024900     PERFORM UNTIL                                                        
025000      NOT ( ((SORTFIL-EOF = NEJ AND SORT-FRAD-IDPRODNR =                  
025100        WSORT-FRAD-IDPRODNR AND SORT-FRAD-FLSPLIT = JA AND                
025200        SORT-FRAD-IDARTNR = WSORT-FRAD-IDARTNR AND                        
025300        SORT-FRAD-KVBEART = WSORT-FRAD-KVBEART) AND                       
025400        (W-KVLEVART NOT > WSORT-FRAD-KVBEART)) )                          
025500       MOVE    999.99                  TO WSORT-FRAD-RESERVG              
025600       PERFORM S04-SKRIV-W46102-W                                         
025700       MOVE  SORT-AREA TO WSORT-AREA                                      
025800       ADD     WSORT-FRAD-KVLEVART     TO W-KVLEVART                      
025900                                                                          
026000       PERFORM S03-RETURN-SORTFIL                                         
026100     END-PERFORM                                                          
026200     PERFORM CBA-BERAKNA-SERVICEGRAD                                      
026300                                                                          
026400     PERFORM S04-SKRIV-W46102-W                                           
026500     PERFORM S05-SKRIV-W46101-W                                           
026600     .                                                                    
026800     SKIP3                                                                
026900 CBA-BERAKNA-SERVICEGRAD SECTION.                                         
027000                                                                          
027100     IF      W-KVLEVART = ZERO                                            
027200       MOVE    ZERO   TO WSORT-FRAD-RESERVG                               
027300     ELSE                                                                 
027400       COMPUTE WSORT-FRAD-RESERVG ROUNDED = W-KVLEVART                    
027500                                          /   WSORT-FRAD-KVBEART          
027600               * 100                                                      
027700     END-IF                                                               
027800     .                                                                    
027900     EJECT                                                                
028000 S01-LAS-W46100 SECTION.                                                  
028100                                                                          
028200     READ  W46100-FAKT INTO W-ARBAREA                                     
028300                      AT END MOVE JA TO W46100-EOF                        
028400     END-READ                                                             
028500                                                                          
028600     IF W46100-EOF = NEJ                                                  
028700                                                                          
028800       MOVE 'W46100'            TO POSTSUM-FDNAMN                         
028900       MOVE 'W46101D1'          TO POSTSUM-DDNAMN2                        
029000       MOVE IN-FKOLLI-IDPTYP    TO POSTSUM-TRANSTYP                       
029100       CALL POSTSUM   USING POSTSUM-PARM                                  
029200                                                                          
029300     END-IF                                                               
029400     .                                                                    
029500     EJECT                                                                
029600 S02-SKRIV-W46102  SECTION.                                               
029700                                                                          
029800     EVALUATE IN-FKOLLI-IDPTYP                                            
029900     WHEN '010'                                                           
030000       WRITE     POST         FROM W-ARBAREA                              
030100     WHEN '011'                                                           
030200       WRITE     POST1        FROM W-ARBAREA                              
030300     WHEN '012'                                                           
030400       WRITE     POST2        FROM W-ARBAREA                              
030500     WHEN '013'                                                           
030600       WRITE     POST3        FROM W-ARBAREA                              
030700     END-EVALUATE                                                         
030710                                                                          
030800     MOVE 'W46102'            TO POSTSUM-FDNAMN                           
030900     MOVE 'W46101D3'          TO POSTSUM-DDNAMN2                          
031000     MOVE IN-FKOLLI-IDPTYP    TO POSTSUM-TRANSTYP                         
031100     CALL POSTSUM   USING POSTSUM-PARM                                    
031200     .                                                                    
031400     SKIP3                                                                
031500 S03-RETURN-SORTFIL SECTION.                                              
031600                                                                          
031700     RETURN SORTFIL              INTO  SORT-AREA                          
031800                      AT END MOVE JA TO SORTFIL-EOF                       
031900     END-RETURN                                                           
032000     .                                                                    
032200     SKIP3                                                                
032300 S04-SKRIV-W46102-W SECTION.                                              
032400                                                                          
032500     WRITE     POST3    FROM WSORT-AREA                                   
032600                                                                          
032800     MOVE 'W46102'            TO POSTSUM-FDNAMN                           
032900     MOVE 'W46101D3'          TO POSTSUM-DDNAMN2                          
033000     MOVE  SORT-FRAD-IDPTYP   TO POSTSUM-TRANSTYP                         
033100     CALL POSTSUM   USING POSTSUM-PARM                                    
033200     .                                                                    
033500     EJECT                                                                
033600 S05-SKRIV-W46101-W SECTION.                                              
033700                                                                          
033800     WRITE UT1-POST         FROM WSORT-AREA                               
033900                                                                          
034400     MOVE 'W46101'            TO POSTSUM-FDNAMN                           
034500     MOVE 'W46101D2'          TO POSTSUM-DDNAMN2                          
034600     MOVE WSORT-FRAD-IDPTYP   TO POSTSUM-TRANSTYP                         
034700     CALL POSTSUM   USING POSTSUM-PARM                                    
034800     .                                                                    
034900     SKIP3                                                                
035000 S06-SKRIV-W46101-SORT SECTION.                                           
035100                                                                          
035200     WRITE UT1-POST         FROM  SORT-AREA                               
035300                                                                          
035800     MOVE 'W46101'            TO POSTSUM-FDNAMN                           
035900     MOVE 'W46101D2'          TO POSTSUM-DDNAMN2                          
036000     MOVE  SORT-FRAD-IDPTYP   TO POSTSUM-TRANSTYP                         
036100     CALL POSTSUM   USING POSTSUM-PARM                                    
036200     .                                                                    
036300                                                                          
036400     EJECT                                                                
036500 S07-SKRIV-W46102-SORT SECTION.                                           
036600                                                                          
036700     WRITE     POST3    FROM  SORT-AREA                                   
036900                                                                          
037000     MOVE 'W46102'            TO POSTSUM-FDNAMN                           
037100     MOVE 'W46101D3'          TO POSTSUM-DDNAMN2                          
037200     MOVE  SORT-FRAD-IDPTYP   TO POSTSUM-TRANSTYP                         
037300     CALL POSTSUM   USING POSTSUM-PARM                                    
037400     .                                                                    
037500                                                                          
037600     SKIP3                                                                
037700 Z-FINIT SECTION.                                                         
037800     SKIP2                                                                
037900     CLOSE W46102-UT2 W46101-UT1                                          
038000     SKIP2                                                                
038100*- - - - - - - - - - - - - - - - - - SKRIV UT ANTAL LÄSTA OCH             
038200*                                    SKRIVNA POSTER                       
038300                                                                          
038400     MOVE 'S' TO POSTSUM-OPKOD                                            
038500     CALL POSTSUM USING POSTSUM-PARM                                      
038600     .                                                                    
038601     EJECT                                                                
038610* --- IMS SEKTIONER ---                                                   
038620     SKIP3                                                                
038700 IMS-GU-WL4491  SECTION.                                                  
038800                                                                          
038900     STRING 'WL449101(WDGXKEY  =' W-WDGXKEY-4491-X ')'                    
039000          DELIMITED BY SIZE INTO SSA1                                     
039100     MOVE '  GE' TO GODK-STATUSKODER                                      
039200     CALL CBLTDLI USING GU 4494-PCB DLI-IO-AREA SSA1                      
039300     MOVE 4494-STATUS-CODE TO STATUS-WS                                   
039400     PERFORM IMS-STATUSKONTROLL                                           
039500     .                                                                    
039600     SKIP3                                                                
039700 IMS-GNP-WL4494 SECTION.                                                  
039800                                                                          
039810     STRING 'WL449112*F(DALASTN  =' W-DALASTN-X                           
039820                      '&IDGMTREF =' W-IDGMTREF-X                          
039830                      '&IDLBBET  =' W-IDLBBET-X                           
039840                      '&IDKOLLI  =' W-IDKOLLI-X ')'                       
040000          DELIMITED BY SIZE INTO SSA1                                     
040100     MOVE '  GE' TO GODK-STATUSKODER                                      
040200     CALL CBLTDLI USING GNP 4494-PCB DLI-IO-AREA SSA1                     
040300     MOVE 4494-STATUS-CODE TO STATUS-WS                                   
040400     PERFORM IMS-STATUSKONTROLL                                           
040500     .                                                                    
040600     SKIP3                                                                
040700 IMS-STATUSKONTROLL SECTION.                                              
040800                                                                          
040900     SET STATUS-IX TO 1                                                   
041000     SEARCH GODK-STATUS                                                   
041100       AT END                                                             
041200         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
041300         DELIMITED BY SIZE INTO FELTEXT                                   
041400         CALL FELLOG                                                      
041500       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
041600         CONTINUE                                                         
041700     END-SEARCH                                                           
041800     .                                                                    
041900     EJECT                                                                
