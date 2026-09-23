000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4639500.                                                
000300 AUTHOR.         KJELLSON GÖRAN.                                          
000400 DATE-WRITTEN.   13/06/19.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*        REDIGERA FIL FÖR DIREKTLEVERANSLARM                              
000900*                                                                         
001000*        PROGRAMMET LÄSER      WDF4 (DIREKTLEVERANSER)                    
001100*                                                                         
001200* UTFIL W4639J - UPPFÖLJNINGSFIL SENA OCH FREKVENT                        
001300*                OMPLANERADE DIREKTLEVERANSRADER                          
001400*       W4639K - DIREKTLEVERANSRADER ÄLDRE ÄN TVÅ (2) ÅR                  
001410*       W4639R - DIREKTLEVERANSRADER SOM SAKNAR MOTSVARIGHET              
001500*                I WDE4B1, KUNDORDERREGISTER RADSEGMENT,                  
001510*                SKA UT PÅ BORTTAGSFIL                                    
001520*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
002000                                                                          
002100                                                                          
002200 ENVIRONMENT DIVISION.                                                    
002300 INPUT-OUTPUT SECTION.                                                    
002400                                                                          
002500 FILE-CONTROL.                                                            
002600                                                                          
002700*          --- DIREKTLEVERANSLARM                                         
002800     SELECT W4639J                     ASSIGN TO W46395D1.                
002900     SELECT W4639K                     ASSIGN TO W46395D2.                
002910     SELECT W4639R                     ASSIGN TO W46395D3.                
002920     SELECT W4639S                     ASSIGN TO W46395D4.                
003000                                                                          
003100 DATA DIVISION.                                                           
003200 FILE SECTION.                                                            
003300                                                                          
003400 FD  W4639J                                                               
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700*01  POST      -COPY W4639J -PRE  LARM-  -L.                              
003800                                                                          
003900                                                                          
004000 FD  W4639K                                                               
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300*01  POST      -COPY W4639J -PRE   OLD-  -L.                              
004400                                                                          
004410                                                                          
004420 FD  W4639R                                                               
004430     RECORDING       F                                                    
004440     BLOCK CONTAINS  0.                                                   
004450*01  POST      -COPY W4639R -PRE  BORT-  -L.                              
004460                                                                          
004470 FD  W4639S                                                               
004480     RECORDING       F                                                    
004490     BLOCK CONTAINS  0.                                                   
004491*01  POST      -COPY W4639S -PRE  LIST-  -L.                              
004492                                                                          
004500                                                                          
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W4639500'.            
004900 77  JA                          PIC X       VALUE 'J'.                   
005000 77  NEJ                         PIC X       VALUE 'N'.                   
005100 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
005200 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
005300                                                                          
005414                                                                          
005415 01  BORTTAG-WS                  PIC X       VALUE 'N'.                   
005416     88  BORTTAGES                           VALUE 'J'.                   
005417     88  BORTTAGES-EJ                        VALUE 'N'.                   
005418                                                                          
005419 01  LISTA-SW                    PIC X       VALUE 'N'.                   
005420     88  SKRIV-LISTA                         VALUE 'J'.                   
005421     88  SKRIV-INTE-LISTA                    VALUE 'N'.                   
005422                                                                          
005430 01  WS-LARM-DAGAR               PIC 9(3)    VALUE 3.                     
005500 01  WS-LARM-MANADER             PIC 9(3)    VALUE 1.                     
005510 01  WS-LARM-AAR                 PIC 9(2)    VALUE 2.                     
005600 01  WS-LARM-ANTAL               PIC 9(3)    VALUE 2.                     
005700                                                                          
005800 01  WS-MAX-DATUM                PIC 9(6)    VALUE ZERO.                  
005900 01  FILLER REDEFINES WS-MAX-DATUM.                                       
006000     03  WS-MAX-DATUM-AAR        PIC 9(2).                                
006100     03  FILLER                  PIC 9(4).                                
006200                                                                          
006310 01  WS-MAX-DATUM-DAGAR          PIC 9(6)    VALUE ZERO.                  
006400 01  WS-MAX-DATUM-PLUS-7         PIC 9(6)    VALUE ZERO.                  
006500 01  WS-MAX-DATUM-MANAD          PIC 9(6)    VALUE ZERO.                  
006600 01  FILLER REDEFINES WS-MAX-DATUM-MANAD.                                 
006700     03  WS-MAX-DATUM-MANAD-AA   PIC 9(2).                                
006800     03  WS-MAX-DATUM-MANAD-MM   PIC 9(2).                                
006900     03  WS-MAX-DATUM-MANAD-DD   PIC 9(2).                                
007000                                                                          
007001 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
007002     SKIP3                                                                
007003 01  KEYS-TILL-DLI.                                                       
007010*                                                                         
007012   03    W-WDE4B1KY-X.                                                    
007013     05    W-IDPRODNR            PIC S9(7)   VALUE ZERO COMP-3.           
007014     05    W-IDPURAD             PIC S9(5)   VALUE ZERO COMP-3.           
007104*                                                                         
007110 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007200 01  FILLER REDEFINES DAGENS-DATUM.                                       
007300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007600                                                                          
007700 01  DYNAMISKA-SUBPROGRAM.                                                
007800                                                                          
007900     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008100     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
008400                                                                          
008500*    --- PARAMETRAR TILL ABEND                                            
008600                                                                          
008700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009000                                                                          
009100 01  FELTEXT.                                                             
009200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009400                                                                          
009500*    --- PARAMETRAR TILL POSTSUM                                          
009600*                                                                         
009700*01  -COPY W0005   -PRE  POSTSUM-                                         
009800                                                                          
009900*    --- PARAMETRAR TILL WZ20DAYS                                         
010100*01  -COPY WZ20DAYS                                                       
010200                                                                          
010210*    --- VALID DDGS                                                       
010220*01  -COPY WWLEV06                                                        
010230                                                                          
010300                                                                          
010400 01  LARM-AREA-START             PIC X(24)   VALUE                        
010500                                 'LARM-AREA-START  '.                     
010600                                                                          
010700*01  AREA -COPY W4639J     -PRE LARM-                                     
010800                                                                          
010900                                                                          
011000 01  OLD-AREA-START              PIC X(24)   VALUE                        
011100                                 'OLD-AREA-START   '.                     
011200                                                                          
011300*01  AREA -COPY W4639J     -PRE OLD-                                      
011400                                                                          
011410                                                                          
011420 01  BORT-AREA-START             PIC X(24)   VALUE                        
011430                                 'BORT-AREA-START  '.                     
011440                                                                          
011450*01  AREA -COPY W4639R     -PRE BORT-                                     
011460                                                                          
011500                                                                          
011510 01  LIST-AREA-START             PIC X(24)   VALUE                        
011520                                 'LIST-AREA-START  '.                     
011530                                                                          
011540*01  AREA -COPY W4639S     -PRE LIST-                                     
011550                                                                          
011560                                                                          
011600*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
011700                                                                          
011800 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011900                                                                          
012000*    --- STATUS-KOD FRÅN IMS                                              
012100 01  STATUS-WS                   PIC XX.                                  
012200     88  SEGMENT-FINNS                       VALUE '  '.                  
012300     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012400     88  SEGMENT-SLUT                        VALUE 'GB'.                  
012500                                                                          
012600 01  GODK-STATUSKODER.                                                    
012700     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012800                                                                          
012900                                                                          
013000 01  SSA1                        PIC X(100).                              
013010 01  SSA2                        PIC X(100).                              
013100                                                                          
013200*    --- IMS FUNKTIONSKODER                                               
013300*01  -COPY W0003                                                          
013400                                                                          
013500*    ---  DLI INPUT-OUTPUT AREA                                           
013600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF401'.                      
013700 01  DLI-IO-WDF401.                                                       
013800*    03  -COPY WDF401                                                     
013900                                                                          
014000 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF411'.                      
014100 01  DLI-IO-WDF411.                                                       
014200*    03  -COPY WDF411                                                     
014300                                                                          
014341                                                                          
014342 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE4B1'.                      
014343 01  DLI-IO-WDE4B1.                                                       
014344*    03  -COPY WDE4B1                                                     
014350                                                                          
014400                                                                          
014500 LINKAGE SECTION.                                                         
014600                                                                          
014700                                                                          
014800*01  -COPY W0008  -PRE WDF4-                                              
014900     05  FILLER                  PIC X.                                   
015000                                                                          
015010                                                                          
015020*01  -COPY W0008  -PRE WDE4B-                                             
015030     05  FILLER                  PIC X.                                   
015040                                                                          
015100                                                                          
015200 PROCEDURE DIVISION  USING WDF4-PCB                                       
015210                           WDE4B-PCB.                                     
015300 MAIN SECTION.                                                            
015400     ENTRY 'DLITCBL' USING WDF4-PCB                                       
015410                           WDE4B-PCB.                                     
015500                                                                          
015600                                                                          
015700     PERFORM A-INIT                                                       
015800                                                                          
015900     PERFORM IMS-GN-WDF401                                                
016000     PERFORM UNTIL SEGMENT-SLUT                                           
016010*    DISPLAY '****** DLEV-IDLEVNR ' DLEV-IDLEVNR                          
016100        PERFORM IMS-GNP-WDF411                                            
016200        PERFORM UNTIL SEGMENT-SAKNAS                                      
016210*    DISPLAY '****** DLOR-IDARTNR ' DLOR-IDARTNR ' ' DLOR-TIUTSKR         
016300                                                                          
016310           MOVE DLEV-IDLEVNR TO LEV06-IDLEVNR                             
016314           PERFORM C-KOLLA-SKRIV-BORTTAG                                  
016315           IF LEV06-DDGS AND SKRIV-LISTA                                  
016316              PERFORM B-KOLLA-SKRIV-LISTA                                 
016317           END-IF                                                         
016318           IF BORTTAGES-EJ                                                
016320             IF LEV06-DDGS                                                
016400                PERFORM D-KOLLA-SKRIV-LARM                                
016410             END-IF                                                       
016500             PERFORM E-KOLLA-SKRIV-GAMLA                                  
016600           END-IF                                                         
016700           PERFORM IMS-GNP-WDF411                                         
016800        END-PERFORM                                                       
016900        PERFORM IMS-GN-WDF401                                             
017000     END-PERFORM                                                          
017100                                                                          
017200     PERFORM Z-FINIT                                                      
017300                                                                          
017400     MOVE ZERO TO RETURN-CODE                                             
017500     GOBACK                                                               
017600     .                                                                    
017700                                                                          
017800                                                                          
017900 A-INIT SECTION.                                                          
018000     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
018100                                                                          
018200     OPEN OUTPUT W4639J W4639K W4639R W4639S                              
018300                                                                          
018400     ACCEPT DAGENS-DATUM  FROM DATE                                       
018500     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
018600                                                                          
018700     PERFORM AA-BERAKNA-GRANSDATUM                                        
018800     .                                                                    
018900                                                                          
019000                                                                          
019100 AA-BERAKNA-GRANSDATUM SECTION.                                           
019200     MOVE 'AA-GRANSDATUM   ' TO CURRENT-SECTION                           
019300                                                                          
019310     DISPLAY '*** DAGENS-DATUM ' DAGENS-DATUM                             
019400     MOVE DAGENS-DATUM         TO WS-MAX-DATUM                            
019500     SUBTRACT WS-LARM-AAR    FROM WS-MAX-DATUM-AAR                        
019510     DISPLAY '*** WS-MAX-DATUM ' WS-MAX-DATUM                             
019600                                                                          
019700     MOVE 'YYMMDD'             TO DAYS-KDDATFMT1                          
019800     MOVE 'YYMMDD'             TO DAYS-KDDATFMT2                          
019900     MOVE SPACE                TO DAYS-TIDATE2                            
020000                                  DAYS-IDCALEND                           
020110     COMPUTE DAYS-KVDAYS = WS-LARM-DAGAR * -1                             
020200                                                                          
020300     MOVE DAGENS-DATUM         TO DAYS-TIDATE1                            
020400     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
020500     IF DAYS-KDRC = ZERO                                                  
020600        MOVE DAYS-TIDATE2(1:6) TO WS-MAX-DATUM-DAGAR                      
020610     DISPLAY '*** WS-MAX-DATUM-DAGAR ' WS-MAX-DATUM-DAGAR                 
020700     ELSE                                                                 
020800        MOVE 'WZ20DAYS WS-MAX-DATUM-DAGAR FEL' TO FELTEXT-STR             
020900        DISPLAY FELTEXT                                                   
021000        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
021100     END-IF                                                               
021200                                                                          
021300     MOVE SPACE                TO DAYS-TIDATE2                            
021400     MOVE '7'                  TO DAYS-KVDAYS                             
021410     COMPUTE DAYS-KVDAYS = DAYS-KVDAYS * -1                               
021500                                                                          
021600     MOVE DAGENS-DATUM         TO DAYS-TIDATE1                            
021700     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
021800     IF DAYS-KDRC = ZERO                                                  
021900        MOVE DAYS-TIDATE2(1:6) TO WS-MAX-DATUM-PLUS-7                     
021910     DISPLAY '*** WS-MAX-DATUM-PLUS-7 ' WS-MAX-DATUM-PLUS-7               
022000     ELSE                                                                 
022100        MOVE 'WZ20DAYS WS-MAX-DATUM-PLUS-7 FEL' TO FELTEXT-STR            
022200        DISPLAY FELTEXT                                                   
022300        CALL ABEND USING RKOD-ABEND-MED-DUMP                              
022400     END-IF                                                               
022500                                                                          
022600     MOVE DAGENS-DATUM         TO WS-MAX-DATUM-MANAD                      
022700     PERFORM UNTIL WS-LARM-MANADER < 13                                   
022800        SUBTRACT 1           FROM WS-MAX-DATUM-MANAD-AA                   
022900        SUBTRACT 12          FROM WS-LARM-MANADER                         
023000     END-PERFORM                                                          
023100                                                                          
023110     IF WS-LARM-MANADER = WS-MAX-DATUM-MANAD-MM                           
023120        SUBTRACT 1           FROM WS-MAX-DATUM-MANAD-MM                   
023121        MOVE     12            TO WS-MAX-DATUM-MANAD-AA                   
023130     ELSE                                                                 
023140        IF WS-LARM-MANADER < WS-MAX-DATUM-MANAD-MM                        
023150           SUBTRACT WS-LARM-MANADER FROM WS-MAX-DATUM-MANAD-MM            
023160        ELSE                                                              
023170           SUBTRACT 1        FROM WS-MAX-DATUM-MANAD-AA                   
023180           ADD      12         TO WS-MAX-DATUM-MANAD-MM                   
023190           SUBTRACT WS-LARM-MANADER                                       
023191                             FROM WS-MAX-DATUM-MANAD-MM                   
023192        END-IF                                                            
023600     END-IF                                                               
023610     DISPLAY '*** WS-MAX-DATUM-MANAD ' WS-MAX-DATUM-MANAD                 
023700                                                                          
023800     INITIALIZE WS-LARM-MANADER                                           
023900     .                                                                    
024000                                                                          
024100                                                                          
024101 B-KOLLA-SKRIV-LISTA SECTION.                                             
024102     MOVE 'B-KOLLA-SKRIV   ' TO CURRENT-SECTION                           
024103                                                                          
024114     IF DLOR-TIPACKN = 0                                                  
024115        IF DLOR-TISKEPPN < DAGENS-DATUM                                   
024118           MOVE '001'        TO LIST-IDPTYP                               
024119           PERFORM BA-SKRIV-LIST                                          
024120        END-IF                                                            
024121     END-IF                                                               
024122                                                                          
024128     IF DLOR-TISKEPPN NOT < WS-MAX-DATUM-PLUS-7 AND                       
024129        DLOR-TISKEPPN NOT > DAGENS-DATUM                                  
024130        MOVE '002'        TO LIST-IDPTYP                                  
024131        PERFORM BA-SKRIV-LIST                                             
024132     END-IF                                                               
024140                                                                          
024141     IF DLOR-TIPACKN = 0                                                  
024142        IF DLOR-TISKEPPN < DAGENS-DATUM                                   
024143          IF DLOR-TISKEPPN NOT < WS-MAX-DATUM-PLUS-7 AND                  
024144             DLOR-TISKEPPN NOT > DAGENS-DATUM                             
024145             MOVE '003'        TO LIST-IDPTYP                             
024146             PERFORM BA-SKRIV-LIST                                        
024147          END-IF                                                          
024150        END-IF                                                            
024151     END-IF                                                               
024152                                                                          
024153     IF DLOR-TIPACKN = DLOR-TISKEPPN                                      
024154        IF DLOR-TIPACKN NOT < WS-MAX-DATUM-PLUS-7 AND                     
024155           DLOR-TIPACKN NOT > DAGENS-DATUM                                
024156           MOVE '004'        TO LIST-IDPTYP                               
024157           PERFORM BA-SKRIV-LIST                                          
024158        END-IF                                                            
024159     END-IF                                                               
024160                                                                          
024161     IF DLOR-TIPACKN > 0 AND DLOR-TISLULEV = 0                            
024162        IF DLOR-TIPACKN > DLOR-TISKEPPN                                   
024163          IF DLOR-TISKEPPN NOT < WS-MAX-DATUM-PLUS-7 AND                  
024164             DLOR-TISKEPPN NOT > DAGENS-DATUM                             
024165             MOVE '005'        TO LIST-IDPTYP                             
024166             PERFORM BA-SKRIV-LIST                                        
024167          END-IF                                                          
024168        END-IF                                                            
024169     END-IF                                                               
024170                                                                          
024171     IF DLOR-TIPACKN > 0 AND DLOR-TISLULEV = 0                            
024172        IF DLOR-TIPACKN < DLOR-TISKEPPN                                   
024173          IF DLOR-TISKEPPN NOT < WS-MAX-DATUM-PLUS-7 AND                  
024174             DLOR-TISKEPPN NOT > DAGENS-DATUM                             
024175             MOVE '006'        TO LIST-IDPTYP                             
024176             PERFORM BA-SKRIV-LIST                                        
024177          END-IF                                                          
024178        END-IF                                                            
024179     END-IF                                                               
024180                                                                          
024181     IF DLOR-TIPACKN > 0                                                  
024182        IF DLOR-TIPACKN = DLOR-TISLULEV                                   
024183          IF DLOR-TISLULEV NOT < WS-MAX-DATUM-PLUS-7 AND                  
024184             DLOR-TISLULEV NOT > DAGENS-DATUM                             
024185             MOVE '007'        TO LIST-IDPTYP                             
024186             PERFORM BA-SKRIV-LIST                                        
024187          END-IF                                                          
024188        END-IF                                                            
024189     END-IF                                                               
024190                                                                          
024191     IF DLOR-TIPACKN > 0 AND DLOR-TISLULEV > 0                            
024192        IF DLOR-TIPACKN > DLOR-TISLULEV                                   
024193          IF DLOR-TISLULEV NOT < WS-MAX-DATUM-PLUS-7 AND                  
024194             DLOR-TISLULEV NOT > DAGENS-DATUM                             
024195             MOVE '008'        TO LIST-IDPTYP                             
024196             PERFORM BA-SKRIV-LIST                                        
024197          END-IF                                                          
024198        END-IF                                                            
024199     END-IF                                                               
024200                                                                          
024201     IF DLOR-TIPACKN > 0                                                  
024202        IF DLOR-TIPACKN < DLOR-TISLULEV AND                               
024203           DLOR-TIPACKN > DLOR-TISKEPPN                                   
024204          IF DLOR-TISLULEV NOT < WS-MAX-DATUM-PLUS-7 AND                  
024205             DLOR-TISLULEV NOT > DAGENS-DATUM                             
024206             MOVE '009'        TO LIST-IDPTYP                             
024207             PERFORM BA-SKRIV-LIST                                        
024208          END-IF                                                          
024209        END-IF                                                            
024210     END-IF                                                               
024211                                                                          
024212     .                                                                    
024213 BA-SKRIV-LIST      SECTION.                                              
024214     MOVE 'BA-SKRIV-LIST    ' TO CURRENT-SECTION                          
024215                                                                          
024216     MOVE DLEV-IDLEVNR       TO LIST-IDLEVNR                              
024217     MOVE DLOR-IDPRODNR      TO LIST-IDPRODNR                             
024218     MOVE DLOR-IDARTNR       TO LIST-IDARTNR                              
024219     MOVE DLOR-IDDISTR       TO LIST-IDDISTR                              
024220     MOVE DLOR-IDKUNDNR      TO LIST-IDKUNDNR                             
024221     MOVE DLOR-IDORDNR7      TO LIST-IDORDNR7                             
024222     MOVE DLOR-TIPACKN       TO LIST-TIPACKN                              
024223     MOVE DLOR-TISLULEV      TO LIST-TISLULEV                             
024224     MOVE DLOR-TISKEPPN      TO LIST-TISKEPPN                             
024225                                                                          
024226     PERFORM S14-SKRIV-W4639S                                             
024227     .                                                                    
024228                                                                          
024229 C-KOLLA-SKRIV-BORTTAG SECTION.                                           
024230     MOVE 'C-KOLLA-BORTTAG ' TO CURRENT-SECTION                           
024231                                                                          
024235     MOVE NEJ             TO BORTTAG-WS                                   
024236     MOVE JA              TO LISTA-SW                                     
024237                                                                          
024238     IF DLOR-TIPACKN > 0                                                  
024239        PERFORM CA-SKRIV-BORT-POST                                        
024240        MOVE JA           TO BORTTAG-WS                                   
024242     ELSE                                                                 
024243       MOVE DLOR-IDPRODNR TO W-IDPRODNR                                   
024244       MOVE DLOR-IDPURAD  TO W-IDPURAD                                    
024245       PERFORM IMS-GU-WDE4B1                                              
024246       IF SEGMENT-SAKNAS                                                  
024247         PERFORM CA-SKRIV-BORT-POST                                       
024248         MOVE JA          TO BORTTAG-WS                                   
024249         MOVE NEJ         TO LISTA-SW                                     
024250       END-IF                                                             
024251     END-IF                                                               
024252     .                                                                    
024253                                                                          
024254                                                                          
024255 CA-SKRIV-BORT-POST SECTION.                                              
024256     MOVE 'CA-SKRIV-BORT-PO' TO CURRENT-SECTION                           
024257                                                                          
024258     MOVE DLEV-IDLEVNR       TO BORT-IDLEVNR                              
024259     MOVE DLOR-IDPRODNR      TO BORT-IDPRODNR                             
024260     MOVE DLOR-IDPURAD       TO BORT-IDPURAD                              
024261     MOVE DLOR-TIUTSKR       TO BORT-TIUTSKR                              
024262                                                                          
024263     PERFORM S13-SKRIV-W4639R                                             
024264     .                                                                    
024265                                                                          
024270 D-KOLLA-SKRIV-LARM SECTION.                                              
024300     MOVE 'D-KOLLA-SKRIV   ' TO CURRENT-SECTION                           
024400                                                                          
024500     IF (DLOR-KDANNULL = '1'  AND                                         
024510         DLOR-TIANNULL > ZERO AND                                         
024600         DLOR-TIANNULL < WS-MAX-DATUM-PLUS-7)                             
024700                                                                          
024800         PERFORM DA-SKRIV-LARM-001                                        
024900     ELSE                                                                 
025000        IF (DLOR-TISLULEV > ZERO                                          
025010        AND DLOR-TISLULEV < WS-MAX-DATUM-PLUS-7)                          
025100                                                                          
025200           PERFORM DB-SKRIV-LARM-002                                      
025300        ELSE                                                              
025400           IF (DLOR-TISLULEV = ZERO                                       
025500           AND DLOR-TISKEPPN < WS-MAX-DATUM-DAGAR)                        
025600                                                                          
025700              PERFORM DC-SKRIV-LARM-003                                   
025800           ELSE                                                           
025900              IF (DLOR-TIUTSKR  < WS-MAX-DATUM-MANAD  AND                 
026000                  DLOR-KVSLULEV > WS-LARM-ANTAL)                          
026100                                                                          
026200                 PERFORM DD-SKRIV-LARM-004                                
026300              END-IF                                                      
026400           END-IF                                                         
026500        END-IF                                                            
026600     END-IF                                                               
026700     .                                                                    
026800                                                                          
026900                                                                          
027000 DA-SKRIV-LARM-001 SECTION.                                               
027100     MOVE 'DA-SKRIV-LARM-1 ' TO CURRENT-SECTION                           
027200                                                                          
027300     MOVE '001'              TO LARM-IDPTYP                               
027400     MOVE DLEV-IDLEVNR       TO LARM-IDLEVNR                              
027500     MOVE DLOR-IDDISTR       TO LARM-IDDISTR                              
027600     MOVE DLOR-IDARTNR       TO LARM-IDARTNR                              
027700     MOVE DLOR-IDKUNDNR      TO LARM-IDKUNDNR                             
027800     MOVE DLOR-IDORDNR7      TO LARM-IDORDNR5                             
027900     MOVE DLOR-IDPRODNR      TO LARM-IDPRODNR                             
028000     MOVE DLOR-IDPURAD       TO LARM-IDPURAD                              
028100     MOVE DLOR-TIUTSKR       TO LARM-TIUTSKR                              
028200     MOVE DLOR-TISKEPPN      TO LARM-TISKEPPN                             
028300     MOVE DLOR-TISLULEV      TO LARM-TISLULEV                             
028400     MOVE DLOR-KVSLULEV      TO LARM-KVSLULEV                             
028500     MOVE DLOR-KDANNULL      TO LARM-KDANNULL                             
028510     MOVE DLOR-TIANNULL      TO LARM-TIANNULL                             
028600     IF DLOR-TIUTSKR  < WS-MAX-DATUM                                      
028700        MOVE '*'             TO LARM-KDOLD                                
028800     ELSE                                                                 
028900        MOVE SPACE           TO LARM-KDOLD                                
029000     END-IF                                                               
029100                                                                          
029200     PERFORM S11-SKRIV-W4639J                                             
029300     .                                                                    
029400                                                                          
029500                                                                          
029600 DB-SKRIV-LARM-002 SECTION.                                               
029700     MOVE 'DB-SKRIV-LARM-2 ' TO CURRENT-SECTION                           
029800                                                                          
029900     MOVE '002'              TO LARM-IDPTYP                               
030000     MOVE DLEV-IDLEVNR       TO LARM-IDLEVNR                              
030100     MOVE DLOR-IDDISTR       TO LARM-IDDISTR                              
030200     MOVE DLOR-IDARTNR       TO LARM-IDARTNR                              
030300     MOVE DLOR-IDKUNDNR      TO LARM-IDKUNDNR                             
030400     MOVE DLOR-IDORDNR7      TO LARM-IDORDNR5                             
030500     MOVE DLOR-IDPRODNR      TO LARM-IDPRODNR                             
030600     MOVE DLOR-IDPURAD       TO LARM-IDPURAD                              
030700     MOVE DLOR-TIUTSKR       TO LARM-TIUTSKR                              
030800     MOVE DLOR-TISKEPPN      TO LARM-TISKEPPN                             
030900     MOVE DLOR-TISLULEV      TO LARM-TISLULEV                             
031000     MOVE DLOR-KVSLULEV      TO LARM-KVSLULEV                             
031100     MOVE ZERO               TO LARM-KDANNULL                             
031110     MOVE ZERO               TO LARM-TIANNULL                             
031200     IF DLOR-TIUTSKR  < WS-MAX-DATUM                                      
031300        MOVE '*'             TO LARM-KDOLD                                
031400     ELSE                                                                 
031500        MOVE SPACE           TO LARM-KDOLD                                
031600     END-IF                                                               
031700                                                                          
031800     PERFORM S11-SKRIV-W4639J                                             
031900     .                                                                    
032000                                                                          
032100                                                                          
032200 DC-SKRIV-LARM-003 SECTION.                                               
032300     MOVE 'DC-SKRIV-LARM-3 ' TO CURRENT-SECTION                           
032400                                                                          
032500     MOVE '003'              TO LARM-IDPTYP                               
032600     MOVE DLEV-IDLEVNR       TO LARM-IDLEVNR                              
032700     MOVE DLOR-IDDISTR       TO LARM-IDDISTR                              
032800     MOVE DLOR-IDARTNR       TO LARM-IDARTNR                              
032900     MOVE DLOR-IDKUNDNR      TO LARM-IDKUNDNR                             
033000     MOVE DLOR-IDORDNR7      TO LARM-IDORDNR5                             
033100     MOVE DLOR-IDPRODNR      TO LARM-IDPRODNR                             
033200     MOVE DLOR-IDPURAD       TO LARM-IDPURAD                              
033300     MOVE DLOR-TIUTSKR       TO LARM-TIUTSKR                              
033400     MOVE DLOR-TISKEPPN      TO LARM-TISKEPPN                             
033500     MOVE ZERO               TO LARM-TISLULEV                             
033600     MOVE ZERO               TO LARM-KVSLULEV                             
033700     MOVE DLOR-KDANNULL      TO LARM-KDANNULL                             
033710     MOVE DLOR-TIANNULL      TO LARM-TIANNULL                             
033800     IF DLOR-TIUTSKR  < WS-MAX-DATUM                                      
033900        MOVE '*'             TO LARM-KDOLD                                
034000     ELSE                                                                 
034100        MOVE SPACE           TO LARM-KDOLD                                
034200     END-IF                                                               
034300                                                                          
034400     PERFORM S11-SKRIV-W4639J                                             
034500     .                                                                    
034600                                                                          
034700                                                                          
034800 DD-SKRIV-LARM-004 SECTION.                                               
034900     MOVE 'DD-SKRIV-LARM-4 ' TO CURRENT-SECTION                           
035000                                                                          
035100     MOVE '004'              TO LARM-IDPTYP                               
035200     MOVE DLEV-IDLEVNR       TO LARM-IDLEVNR                              
035300     MOVE DLOR-IDDISTR       TO LARM-IDDISTR                              
035400     MOVE DLOR-IDARTNR       TO LARM-IDARTNR                              
035500     MOVE DLOR-IDKUNDNR      TO LARM-IDKUNDNR                             
035600     MOVE DLOR-IDORDNR7      TO LARM-IDORDNR5                             
035700     MOVE DLOR-IDPRODNR      TO LARM-IDPRODNR                             
035800     MOVE DLOR-IDPURAD       TO LARM-IDPURAD                              
035900     MOVE DLOR-TIUTSKR       TO LARM-TIUTSKR                              
036000     MOVE DLOR-TISKEPPN      TO LARM-TISKEPPN                             
036100     MOVE DLOR-TISLULEV      TO LARM-TISLULEV                             
036200     MOVE DLOR-KVSLULEV      TO LARM-KVSLULEV                             
036300     MOVE ZERO               TO LARM-KDANNULL                             
036310     MOVE ZERO               TO LARM-TIANNULL                             
036400     IF DLOR-TIUTSKR  < WS-MAX-DATUM                                      
036500        MOVE '*'             TO LARM-KDOLD                                
036600     ELSE                                                                 
036700        MOVE SPACE           TO LARM-KDOLD                                
036800     END-IF                                                               
036900                                                                          
037000     PERFORM S11-SKRIV-W4639J                                             
037100     .                                                                    
037200                                                                          
037300                                                                          
037400 E-KOLLA-SKRIV-GAMLA SECTION.                                             
037500     MOVE 'E-KOLLA-GAMLA   ' TO CURRENT-SECTION                           
037600                                                                          
037700     IF DLOR-TIUTSKR  < WS-MAX-DATUM                                      
037800        MOVE 'OLD '          TO OLD-IDPTYP                                
037900        MOVE DLEV-IDLEVNR    TO OLD-IDLEVNR                               
038000        MOVE DLOR-IDDISTR    TO OLD-IDDISTR                               
038100        MOVE DLOR-IDARTNR    TO OLD-IDARTNR                               
038200        MOVE DLOR-IDKUNDNR   TO OLD-IDKUNDNR                              
038300        MOVE DLOR-IDORDNR7   TO OLD-IDORDNR5                              
038400        MOVE DLOR-IDPRODNR   TO OLD-IDPRODNR                              
038500        MOVE DLOR-IDPURAD    TO OLD-IDPURAD                               
038600        MOVE DLOR-TIUTSKR    TO OLD-TIUTSKR                               
038700        MOVE DLOR-TISKEPPN   TO OLD-TISKEPPN                              
038800        MOVE DLOR-TISLULEV   TO OLD-TISLULEV                              
038900        MOVE DLOR-KVSLULEV   TO OLD-KVSLULEV                              
039000        MOVE DLOR-KDANNULL   TO OLD-KDANNULL                              
039010        MOVE DLOR-TIANNULL   TO OLD-TIANNULL                              
039100        MOVE '*'             TO OLD-KDOLD                                 
039200                                                                          
039300        PERFORM S12-SKRIV-W4639K                                          
039400     END-IF                                                               
039500     .                                                                    
039600                                                                          
039610                                                                          
039810 Z-FINIT SECTION.                                                         
039900     CLOSE W4639J W4639K W4639R W4639S                                    
040000                                                                          
040100     MOVE 'S'        TO POSTSUM-OPKOD                                     
040200     CALL POSTSUM USING POSTSUM-PARM                                      
040300     .                                                                    
040400                                                                          
040500                                                                          
040600 S11-SKRIV-W4639J SECTION.                                                
040700                                                                          
040800     WRITE LARM-POST FROM LARM-AREA                                       
040900                                                                          
041000     MOVE 'LARM'      TO POSTSUM-TRANSTYP                                 
041100     MOVE 'W4639J'    TO POSTSUM-FDNAMN                                   
041200     MOVE 'W46395D1'  TO POSTSUM-DDNAMN2                                  
041300     CALL POSTSUM  USING POSTSUM-PARM                                     
041400     .                                                                    
041500                                                                          
041600                                                                          
041700 S12-SKRIV-W4639K SECTION.                                                
041800                                                                          
041900     WRITE OLD-POST FROM OLD-AREA                                         
042000                                                                          
042100     MOVE 'OLD '      TO POSTSUM-TRANSTYP                                 
042200     MOVE 'W4639K'    TO POSTSUM-FDNAMN                                   
042300     MOVE 'W46395D2'  TO POSTSUM-DDNAMN2                                  
042400     CALL POSTSUM  USING POSTSUM-PARM                                     
042500     .                                                                    
042600                                                                          
042610                                                                          
042620 S13-SKRIV-W4639R SECTION.                                                
042630                                                                          
042640     WRITE BORT-POST FROM BORT-AREA                                       
042650                                                                          
042660     MOVE 'BORT'      TO POSTSUM-TRANSTYP                                 
042670     MOVE 'W4639R'    TO POSTSUM-FDNAMN                                   
042680     MOVE 'W46395D3'  TO POSTSUM-DDNAMN2                                  
042690     CALL POSTSUM  USING POSTSUM-PARM                                     
042691     .                                                                    
042692                                                                          
042700                                                                          
042710 S14-SKRIV-W4639S SECTION.                                                
042720                                                                          
042730     WRITE LIST-POST FROM LIST-AREA                                       
042740                                                                          
042750     MOVE 'LIST'      TO POSTSUM-TRANSTYP                                 
042760     MOVE 'W4639S'    TO POSTSUM-FDNAMN                                   
042770     MOVE 'W46395D4'  TO POSTSUM-DDNAMN2                                  
042780     CALL POSTSUM  USING POSTSUM-PARM                                     
042790     .                                                                    
042791                                                                          
042800                                                                          
042900* --- IMS SEKTIONER ---                                                   
043000                                                                          
043100                                                                          
043200 IMS-GN-WDF401 SECTION.                                                   
043300     MOVE 'IMS-GN-WDF401   ' TO CURRENT-SECTION                           
043400                                                                          
043500                                                                          
043600     MOVE 'WDF401 '           TO SSA1                                     
043700     MOVE '  GB'              TO GODK-STATUSKODER                         
043800     CALL CBLTDLI USING GN WDF4-PCB DLI-IO-WDF401 SSA1                    
043900     MOVE WDF4-STATUS-CODE    TO STATUS-WS                                
044000     PERFORM IMS-STATUSKONTROLL                                           
044100     .                                                                    
044200                                                                          
044300 IMS-GNP-WDF411 SECTION.                                                  
044400     MOVE 'IMS-GNP-WDF411  ' TO CURRENT-SECTION                           
044500                                                                          
044600                                                                          
044700     MOVE 'WDF411 '           TO SSA1                                     
044800     MOVE '  GE'              TO GODK-STATUSKODER                         
044900     CALL CBLTDLI USING GNP WDF4-PCB DLI-IO-WDF411 SSA1                   
045000     MOVE WDF4-STATUS-CODE    TO STATUS-WS                                
045100     PERFORM IMS-STATUSKONTROLL                                           
045200     .                                                                    
045300                                                                          
045310 IMS-GU-WDE4B1      SECTION.                                              
045311     MOVE 'IMS-GU-WDE4B1  ' TO CURRENT-SECTION                            
045320     STRING 'WDE4B1  (WDE4B1KY =' W-WDE4B1KY-X ')'                        
045330            DELIMITED BY SIZE INTO SSA1                                   
045360     MOVE '  GE' TO GODK-STATUSKODER                                      
045370     CALL CBLTDLI USING GU     WDE4B-PCB DLI-IO-WDE4B1 SSA1               
045380     MOVE WDE4B-STATUS-CODE TO STATUS-WS                                  
045390     PERFORM IMS-STATUSKONTROLL                                           
045391     .                                                                    
045393                                                                          
045410 IMS-STATUSKONTROLL SECTION.                                              
045500                                                                          
045600     SET STATUS-IX TO 1                                                   
045700     SEARCH GODK-STATUS                                                   
045800       AT END                                                             
045900         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
046000           DELIMITED BY SIZE INTO FELTEXT                                 
046100         DISPLAY FELTEXT                                                  
046200         CALL FELLOG                                                      
046300       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
046400         CONTINUE                                                         
046500     END-SEARCH                                                           
046600     .                                                                    
